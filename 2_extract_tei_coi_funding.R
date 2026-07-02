# =============================================================================
# 2_extract_tei_coi_funding.R  -- FUNCTION LIBRARY (single source of truth)
# -----------------------------------------------------------------------------
# TEI funder-name / funding-statement / COI-statement regex extraction from
# GROBID TEI XML. Sourcing this file ONLY defines functions and regex constants
# -- no top-level execution, no file I/O, no API calls.
#
# Sourced by run_pipeline.R (the pipeline) and by validation/pilot_extract_tei.R
# (the pilot-1 + pilot-2 batch reproduction + metacheck comparison).
#
# Public functions:
#   heading_matches(), clean_extracted_text(), extract_regex(), extract_metacheck()
# =============================================================================
suppressMessages({
  library(dplyr)
  library(stringr)
  library(xml2)
})

# NA-aware null-coalesce (see run_pipeline.R for the rationale).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

heading_matches <- function(heading, pattern, compact_pattern) {
  if (is.na(heading)) return(FALSE)
  str_detect(heading, pattern) ||
    str_detect(gsub("\\s+", "", heading), compact_pattern)
}

funding_heading_re <- regex("fund|grant|sponsor|acknowledg|financial.{0,5}support|author.{0,3}note", ignore_case = TRUE)
funding_compact_re <- regex("FUND|GRANT|SPONSOR|ACKNOWLEDG|FINANCIALSUPPORT|AUTHORNOTE", ignore_case = TRUE)
coi_heading_re <- regex("conflict.{0,10}interest|competing.{0,5}interest|disclosure|declaration.{0,15}interest", ignore_case = TRUE)
coi_compact_re <- regex("CONFLICTO?F?INTEREST|COMPETINGINTEREST|DISCLOSURE|DECLARATIONO?F?INTEREST", ignore_case = TRUE)
coi_body_re <- regex("conflict.{0,10}interest|competing.{0,5}interest|no.{0,5}conflict|no known conflict", ignore_case = TRUE)

no_funding_re <- regex(paste0(
  "received no (financial support|specific grant|external funding|funding)|",
  "no (external|specific) funding|",
  "no specific grant from|",
  "(did not|didn't|do not|does not) receive (any )?(specific |external )?(funding|grant|financial support)|",
  "no funding (was )?received|",
  "(study|research|work|project|paper|article|comment\\w*|commentary) (was|were) not funded|",
  "(was|were) not funded by any|",
  "there (was|were|is|are) no (external )?funding"
), ignore_case = TRUE)
positive_funding_re <- regex(paste0(
  "funded by|grant (no|number|#|:)|under grant|fellowship from|",
  "financial support (from|by)|support(ed)? (from|by) the|",
  "awarded (by|to|from)|received (funding|a grant|support) (from|by)"
), ignore_case = TRUE)

trailing_section_re <- regex(
  paste0(
    "(Informed Consent|",
    "Ethics (Approval|Statement)|Ethical (Approval|Statement)|Ethical standard[.]|",
    "Data Availability|",
    "ORCID iD|About the author|Notes on contributor|",
    "Author Contribution|",
    "Appendix|SUPPORTING INFORMATION|Supplemental Material|",
    "This article is licensed under a Creative Commons)"
  ),
  ignore_case = TRUE
)

clean_extracted_text <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(text)
  loc <- str_locate(text, trailing_section_re)
  if (!is.na(loc[1, "start"]) && loc[1, "start"] > 20) {
    prefix <- substr(text, max(1, loc[1, "start"] - 2), loc[1, "start"] - 1)
    if (grepl("[.!?]\\s?$|\\n$", prefix) || loc[1, "start"] <= 2) {
      text <- substr(text, 1, loc[1, "start"] - 1) |> trimws()
    }
  }
  text
}

extract_regex <- function(tei_file, doi) {
  doc <- tryCatch({
    d <- read_xml(tei_file)
    xml_ns_strip(d)
    d
  }, error = function(e) {
    message("  XML parse failed: ", conditionMessage(e))
    NULL
  })

  rx_funder_names <- NA_character_
  rx_funding_text <- NA_character_
  rx_coi_text <- NA_character_

  if (!is.null(doc)) {
    back_leaf_divs <- xml_find_all(doc, "//back//div[not(div)]")
    body_leaf_divs <- xml_find_all(doc, "//body//div[not(div)]")
    back_all_divs <- xml_find_all(doc, "//back//div")
    body_all_divs <- xml_find_all(doc, "//body//div")
    all_divs <- c(back_leaf_divs, body_leaf_divs)
    all_divs_incl_parents <- c(back_all_divs, body_all_divs)

    node_text <- function(xpath) {
      nodes <- xml_find_all(doc, xpath)
      if (length(nodes) == 0) return(character(0))
      str_squish(xml_text(nodes))
    }
    funder_names <- c(
      node_text("//titleStmt/funder"),
      node_text("//rs[@type='funder']"),
      node_text("//listOrg[@type='funding']//orgName[not(@type='grant-name') and not(@type='grant-number')]")
    )
    funder_names <- unique(funder_names[nchar(funder_names) > 0])

    funding_parts <- character(0)
    funding_content_re <- regex("fund|grant|support|financ|award|fellowship|scholarship|sponsor", ignore_case = TRUE)

    funding_parent_divs <- xml_find_all(doc, "//back//div[@type='funding']")
    for (fp_node in funding_parent_divs) {
      children <- xml_find_all(fp_node, "./div")
      if (length(children) > 0) {
        for (child in children) {
          child_text <- xml_text(child) |> trimws()
          if (nchar(child_text) > 0 && str_detect(child_text, funding_content_re))
            funding_parts <- c(funding_parts, clean_extracted_text(child_text))
        }
      } else {
        funding_parts <- c(funding_parts, clean_extracted_text(xml_text(fp_node) |> trimws()))
      }
    }

    grant_nodes <- xml_find_all(doc, "//listOrg[@type='funding']//idno[@type='grant-number']")
    if (length(grant_nodes) > 0)
      funding_parts <- c(funding_parts, paste("Grant:", xml_text(grant_nodes) |> trimws()))

    for (node in all_divs) {
      heading_raw <- xml_text(xml_find_first(node, ".//head"))
      body_text <- xml_text(node) |> trimws()
      cleaned_body <- clean_extracted_text(body_text)
      if (heading_matches(heading_raw, funding_heading_re, funding_compact_re) &&
          !cleaned_body %in% funding_parts) {
        is_ack_heading <- !is.na(heading_raw) &&
          (str_detect(heading_raw, regex("acknowledg|author.{0,3}note", ignore_case = TRUE)) ||
           str_detect(gsub("\\s+", "", heading_raw), regex("ACKNOWLEDG|AUTHORNOTE", ignore_case = TRUE)))
        if (!is_ack_heading || str_detect(body_text, funding_content_re))
          funding_parts <- c(funding_parts, cleaned_body)
      }
    }

    if (length(funding_parts) == 0 && length(funder_names) == 0) {
      strong_funding_re <- regex("(this (work|study|research|project) was (funded|supported)|funded by|financial support .{0,20}(from|by)|supported by .{0,20}(grant|award|fellowship))", ignore_case = TRUE)
      for (node in all_divs) {
        body_text <- xml_text(node) |> trimws()
        if (str_detect(body_text, strong_funding_re))
          funding_parts <- c(funding_parts, clean_extracted_text(body_text))
      }
    }

    funding_parts <- unique(funding_parts[nchar(funding_parts) > 0])
    if (length(funding_parts) > 0)
      rx_funding_text <- paste(funding_parts, collapse = " || ")

    if (length(funder_names) > 0) {
      rx_funder_names <- paste(funder_names, collapse = " || ")
    } else if (!is.na(rx_funding_text) &&
               str_detect(rx_funding_text, no_funding_re) &&
               !str_detect(rx_funding_text, positive_funding_re)) {
      rx_funder_names <- "no funding"
    }

    coi_parts <- character(0)
    extract_coi_sentences <- function(text) {
      if (nchar(text) <= 500) return(text)
      sentences <- unlist(str_split(text, "(?<=\\.)\\s+"))
      coi_sentences <- sentences[str_detect(sentences, coi_body_re)]
      if (length(coi_sentences) > 0) paste(coi_sentences, collapse = " ") else text
    }

    for (node in all_divs) {
      heading_raw <- xml_text(xml_find_first(node, ".//head"))
      body_text <- xml_text(node) |> trimws()
      if (heading_matches(heading_raw, coi_heading_re, coi_compact_re)) {
        heading_stripped <- gsub("\\s+", "", heading_raw %||% "")
        body_stripped <- gsub("\\s+", "", body_text)
        if (nchar(body_stripped) > nchar(heading_stripped)) {
          coi_parts <- c(coi_parts, extract_coi_sentences(body_text))
        } else {
          coi_parts <- c(coi_parts, "[COI heading found, statement text missing in TEI]")
        }
      }
    }

    if (length(coi_parts) == 0) {
      for (node in all_divs_incl_parents) {
        heading_raw <- xml_text(xml_find_first(node, ".//head"))
        body_text <- xml_text(node) |> trimws()
        if (heading_matches(heading_raw, coi_heading_re, coi_compact_re)) {
          extracted <- extract_coi_sentences(body_text)
          if (nchar(extracted) > 0 && nchar(extracted) <= nchar(body_text))
            coi_parts <- c(coi_parts, extracted)
        }
      }
    }

    if (length(coi_parts) == 0) {
      for (node in xml_find_all(doc, "//back//div[@type='funding']")) {
        body_text <- xml_text(node) |> trimws()
        if (str_detect(body_text, coi_body_re)) {
          sentences <- unlist(str_split(body_text, "(?<=\\.)\\s+"))
          coi_sentences <- sentences[str_detect(sentences, coi_body_re)]
          coi_parts <- c(coi_parts, coi_sentences)
        }
      }
    }

    if (length(coi_parts) == 0) {
      annex_divs <- xml_find_all(doc, "//back//div[@type='annex']//div | //back//div[@type='author-notes']//div")
      for (node in annex_divs) {
        body_text <- xml_text(node) |> trimws()
        if (str_detect(body_text, coi_body_re))
          coi_parts <- c(coi_parts, extract_coi_sentences(body_text))
      }
    }

    if (length(coi_parts) == 0) {
      strong_coi_re <- regex("(the authors? (have|has|declare|report) no (known )?conflict|no (potential )?conflict.{0,5}interest|we have no conflict)", ignore_case = TRUE)
      for (node in c(all_divs, all_divs_incl_parents)) {
        body_text <- xml_text(node) |> trimws()
        if (nchar(body_text) < 500 && str_detect(body_text, strong_coi_re))
          coi_parts <- c(coi_parts, body_text)
      }
    }

    coi_parts <- unique(coi_parts[nchar(coi_parts) > 0])
    coi_parts <- sapply(coi_parts, clean_extracted_text, USE.NAMES = FALSE)
    coi_parts <- coi_parts[nchar(coi_parts) > 0]
    if (length(coi_parts) > 0)
      rx_coi_text <- paste(coi_parts, collapse = " || ")
  }

  tibble(doi = doi,
         rx_funder_names = rx_funder_names,
         rx_funding_text = rx_funding_text,
         rx_coi_text = rx_coi_text)
}

# ---- 2. Metacheck extraction (optional) ----
# NOTE: the installed metacheck (>= 0.0.1.9001) replaced read_grobid() with read()
# and changed the module output schema (summary_table$<x>_found, table$text).
extract_metacheck <- function(tei_file, doi) {
  paper <- tryCatch(metacheck::read(tei_file), error = function(e) {
    message("  metacheck read failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(paper)) {
    return(tibble(doi = doi,
                  mc_funding_found = NA, mc_funding_text = NA_character_,
                  mc_coi_found = NA, mc_coi_text = NA_character_))
  }

  # Pull a boolean "<x>_found" out of a module's summary_table, robust to col name
  found_from_summary <- function(mod) {
    if (is.null(mod) || is.null(mod$summary_table)) return(NA)
    st <- mod$summary_table
    found_col <- grep("_found$", names(st), value = TRUE)
    if (length(found_col) > 0) return(isTRUE(any(as.logical(st[[found_col[1]]]), na.rm = TRUE)))
    if (!is.null(mod$traffic_light)) return(identical(mod$traffic_light, "green"))
    NA
  }
  # Collapse the module's text matches (the `text` column of $table) into one string
  text_from_table <- function(mod) {
    if (is.null(mod) || is.null(mod$table) || nrow(mod$table) == 0) return(NA_character_)
    tbl <- mod$table
    txt_col <- if ("text" %in% names(tbl)) "text" else
      setdiff(names(tbl)[sapply(tbl, is.character)], "paper_id")[1]
    if (is.na(txt_col) || is.null(txt_col)) return(NA_character_)
    vals <- unique(trimws(as.character(tbl[[txt_col]])))
    vals <- vals[!is.na(vals) & nchar(vals) > 0]
    if (length(vals) == 0) return(NA_character_)
    paste(vals, collapse = " || ")
  }

  funding_mod <- tryCatch(metacheck::module_run(paper, "funding_check"), error = function(e) NULL)
  coi_mod     <- tryCatch(metacheck::module_run(paper, "coi_check"),     error = function(e) NULL)

  tibble(doi = doi,
         mc_funding_found = found_from_summary(funding_mod),
         mc_funding_text  = text_from_table(funding_mod),
         mc_coi_found     = found_from_summary(coi_mod),
         mc_coi_text      = text_from_table(coi_mod))
}
