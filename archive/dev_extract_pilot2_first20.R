# Dev script: run TEI extraction on first 20 pilot2 DOIs, compare to manual coding
library(dplyr)
library(stringr)
library(xml2)

if (!"metacheck" %in% rownames(installed.packages())) {
  remotes::install_github("scienceverse/metacheck", dependencies = TRUE)
}
library(metacheck)

# ---- Settings ----
tei_dir <- "../pilot2_pdf_conversion/tei"
pilot2_csv <- "data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"

# ---- Load pilot2 DOIs + manual coding ----
manual <- read.csv(pilot2_csv)
manual <- manual |>
  mutate(
    doi = X1.2..Article.DOI |>
      sub("^https?://doi\\.org/", "", x = _) |>
      sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
      trimws()
  ) |>
  filter(doi != "" & doi != "test") |>
  # Keep first entry per DOI (some have multiple coders)
  filter(!duplicated(doi))

# Manual ground truth columns
manual_gt <- manual |>
  transmute(
    doi,
    manual_funding_yn = X8.1..Does.the.article.include.a.funding.statement.,
    manual_funding_text = X8.1.1.If.the.article.include.a.funding.statement..what.is.the.statement.,
    manual_coi_yn = X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI..,
    manual_coi_text = X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement.
  )

# ---- Match DOIs to TEI files ----
all_tei <- list.files(tei_dir, pattern = "\\.tei\\.xml$", full.names = TRUE)

extract_doi_from_filename <- function(fname) {
  base <- basename(fname)
  doi_part <- sub("_-_.*", "", base)
  sub("^(10\\.[0-9]{4,5})-", "\\1/", doi_part)
}

tei_dois <- sapply(all_tei, extract_doi_from_filename, USE.NAMES = FALSE)
names(all_tei) <- tei_dois

pilot2_dois <- manual_gt$doi
matched_idx <- match(pilot2_dois, tei_dois)
matched_dois <- pilot2_dois[!is.na(matched_idx)]
matched_files <- all_tei[matched_idx[!is.na(matched_idx)]]

message(length(matched_dois), " DOIs matched to TEI files out of ", length(pilot2_dois))

# ---- Select DOIs to process ----
# Set to "all", "dev20", or "problem5"
RUN_MODE <- Sys.getenv("RUN_MODE", "all")
if (RUN_MODE == "problem5") {
  problem <- c("10.1002/cpp.2863", "10.1037/fam0001090", "10.1111/bjhp.12677",
               "10.3390/jintelligence11030044", "10.1002/ajcp.12671")
  keep <- matched_dois %in% problem
  dev_dois <- matched_dois[keep]
  dev_files <- matched_files[keep]
} else if (RUN_MODE == "dev20") {
  dev_dois <- matched_dois[1:min(20, length(matched_dois))]
  dev_files <- matched_files[1:min(20, length(matched_files))]
} else {
  dev_dois <- matched_dois
  dev_files <- matched_files
}

# ---- Metacheck extraction ----
extract_metacheck <- function(tei_file, doi) {
  paper <- tryCatch(metacheck::read_grobid(tei_file), error = function(e) {
    message("  metacheck read failed: ", conditionMessage(e)); NULL
  })
  if (is.null(paper)) {
    return(tibble(doi = doi, mc_funding_found = NA, mc_funding_text = NA_character_,
                  mc_coi_found = NA, mc_coi_text = NA_character_))
  }

  funding_mod <- tryCatch(metacheck::module_run(paper, "funding_check"), error = function(e) NULL)
  mc_funding_found <- FALSE
  mc_funding_text <- NA_character_
  if (!is.null(funding_mod)) {
    if (!is.null(funding_mod$summary_table)) {
      mc_funding_found <- any(funding_mod$summary_table$value %in% c(TRUE, "TRUE"), na.rm = TRUE)
    }
    if (!is.null(funding_mod$table) && nrow(funding_mod$table) > 0) {
      text_cols <- sapply(funding_mod$table, is.character)
      texts <- funding_mod$table[, text_cols, drop = FALSE]
      mc_funding_text <- paste(unique(unlist(texts)), collapse = " || ")
      if (mc_funding_text == "") mc_funding_text <- NA_character_
      mc_funding_found <- TRUE
    }
  }

  coi_mod <- tryCatch(metacheck::module_run(paper, "coi_check"), error = function(e) NULL)
  mc_coi_found <- FALSE
  mc_coi_text <- NA_character_
  if (!is.null(coi_mod)) {
    if (!is.null(coi_mod$summary_table)) {
      mc_coi_found <- any(coi_mod$summary_table$value %in% c(TRUE, "TRUE"), na.rm = TRUE)
    }
    if (!is.null(coi_mod$table) && nrow(coi_mod$table) > 0) {
      text_cols <- sapply(coi_mod$table, is.character)
      texts <- coi_mod$table[, text_cols, drop = FALSE]
      mc_coi_text <- paste(unique(unlist(texts)), collapse = " || ")
      if (mc_coi_text == "") mc_coi_text <- NA_character_
      mc_coi_found <- TRUE
    }
  }

  tibble(doi = doi, mc_funding_found = mc_funding_found, mc_funding_text = mc_funding_text,
         mc_coi_found = mc_coi_found, mc_coi_text = mc_coi_text)
}

# ---- Regex extraction ----

# Match heading against pattern, handling GROBID's spaced-out headings
# e.g. "CON F L IC T OF I N T ER E S T" or "AC K NOW L E DGM E N T S"
heading_matches <- function(heading, pattern, compact_pattern) {
  if (is.na(heading)) return(FALSE)
  str_detect(heading, pattern) ||
    str_detect(gsub("\\s+", "", heading), compact_pattern)
}

# Regex patterns — both spaced and compact versions
funding_heading_re <- regex("fund|grant|sponsor|acknowledg|financial.{0,5}support|author.{0,3}note", ignore_case = TRUE)
funding_compact_re <- regex("FUND|GRANT|SPONSOR|ACKNOWLEDG|FINANCIALSUPPORT|AUTHORNOTE", ignore_case = TRUE)
coi_heading_re <- regex("conflict.{0,10}interest|competing.{0,5}interest|disclosure|declaration.{0,15}interest", ignore_case = TRUE)
coi_compact_re <- regex("CONFLICTO?F?INTEREST|COMPETINGINTEREST|DISCLOSURE|DECLARATIONO?F?INTEREST", ignore_case = TRUE)
coi_body_re <- regex("conflict.{0,10}interest|competing.{0,5}interest|no.{0,5}conflict|no known conflict", ignore_case = TRUE)

# Trim trailing non-funding/non-COI boilerplate that GROBID often concatenates
# (e.g. ethics approvals, IRB statements, ORCID blocks, author bios, informed consent)
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
  # Find the first trailing section boundary and truncate
  loc <- str_locate(text, trailing_section_re)
  if (!is.na(loc[1, "start"]) && loc[1, "start"] > 20) {
    text <- substr(text, 1, loc[1, "start"] - 1) |> trimws()
  }
  text
}

extract_regex <- function(tei_file, doi) {
  raw <- readLines(tei_file, warn = FALSE)
  xml_str <- paste(raw, collapse = "\n")
  xml_clean <- gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "", xml_str, fixed = TRUE)
  doc <- tryCatch(read_xml(xml_clean), error = function(e) { message("  XML parse failed"); NULL })

  rx_funding_text <- NA_character_
  rx_coi_text <- NA_character_

  if (!is.null(doc)) {
    # Collect leaf-level divs (no child divs) to avoid parent containers
    # that concatenate all children's text. Also collect parent divs separately
    # for fallback when leaf search fails.
    back_leaf_divs <- xml_find_all(doc, "//back//div[not(div)]")
    body_leaf_divs <- xml_find_all(doc, "//body//div[not(div)]")
    back_all_divs <- xml_find_all(doc, "//back//div")
    body_all_divs <- xml_find_all(doc, "//body//div")
    all_divs <- c(back_leaf_divs, body_leaf_divs)
    all_divs_incl_parents <- c(back_all_divs, body_all_divs)

    # --- Funding ---
    funding_parts <- character(0)
    funding_content_re <- regex("fund|grant|support|financ|award|fellowship|scholarship|sponsor", ignore_case = TRUE)

    # Structured funder elements in header
    funder_nodes <- xml_find_all(doc, "//titleStmt/funder")
    if (length(funder_nodes) > 0)
      funding_parts <- c(funding_parts, xml_text(funder_nodes) |> trimws())

    # Typed funding divs in back — prefer leaf children over parent container
    funding_parent_divs <- xml_find_all(doc, "//back//div[@type='funding']")
    for (fp_node in funding_parent_divs) {
      children <- xml_find_all(fp_node, "./div")
      if (length(children) > 0) {
        # Use leaf children that contain funding-related content
        for (child in children) {
          child_text <- xml_text(child) |> trimws()
          if (nchar(child_text) > 0 && str_detect(child_text, funding_content_re))
            funding_parts <- c(funding_parts, clean_extracted_text(child_text))
        }
      } else {
        funding_parts <- c(funding_parts, clean_extracted_text(xml_text(fp_node) |> trimws()))
      }
    }

    # Grant numbers from listOrg
    grant_nodes <- xml_find_all(doc, "//listOrg[@type='funding']//idno[@type='grant-number']")
    if (length(grant_nodes) > 0)
      funding_parts <- c(funding_parts, paste("Grant:", xml_text(grant_nodes) |> trimws()))

    # Heading-based search across leaf divs (back + body)
    # For acknowledgment sections, verify text actually mentions funding
    for (node in all_divs) {
      heading_raw <- xml_text(xml_find_first(node, ".//head"))
      body_text <- xml_text(node) |> trimws()
      if (heading_matches(heading_raw, funding_heading_re, funding_compact_re) &&
          !body_text %in% funding_parts) {
        # If matched via "acknowledg" heading, verify body has funding content
        is_ack_heading <- !is.na(heading_raw) &&
          (str_detect(heading_raw, regex("acknowledg|author.{0,3}note", ignore_case = TRUE)) ||
           str_detect(gsub("\\s+", "", heading_raw), regex("ACKNOWLEDG|AUTHORNOTE", ignore_case = TRUE)))
        if (!is_ack_heading || str_detect(body_text, funding_content_re))
          funding_parts <- c(funding_parts, clean_extracted_text(body_text))
      }
    }

    # 4. Fallback: scan ALL divs for explicit funding phrases (catches misplaced sections)
    if (length(funding_parts) == 0) {
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

    # --- COI ---
    coi_parts <- character(0)

    # Helper: for long text blocks, extract only COI-relevant sentences
    extract_coi_sentences <- function(text) {
      if (nchar(text) <= 500) return(text)
      sentences <- unlist(str_split(text, "(?<=\\.)\\s+"))
      coi_sentences <- sentences[str_detect(sentences, coi_body_re)]
      if (length(coi_sentences) > 0) paste(coi_sentences, collapse = " ") else text
    }

    # 1. Heading-based search on LEAF divs first (avoids parent containers)
    for (node in all_divs) {
      heading_raw <- xml_text(xml_find_first(node, ".//head"))
      body_text <- xml_text(node) |> trimws()
      if (heading_matches(heading_raw, coi_heading_re, coi_compact_re)) {
        heading_stripped <- gsub("\\s+", "", heading_raw %||% "")
        body_stripped <- gsub("\\s+", "", body_text)
        if (nchar(body_stripped) > nchar(heading_stripped)) {
          coi_parts <- c(coi_parts, extract_coi_sentences(body_text))
        } else {
          # Heading-only div (GROBID lost the statement text)
          coi_parts <- c(coi_parts, "[COI heading found, statement text missing in TEI]")
        }
      }
    }

    # 1b. If leaf search failed entirely, try parent divs with sentence extraction
    if (length(coi_parts) == 0) {
      for (node in all_divs_incl_parents) {
        heading_raw <- xml_text(xml_find_first(node, ".//head"))
        body_text <- xml_text(node) |> trimws()
        if (heading_matches(heading_raw, coi_heading_re, coi_compact_re)) {
          extracted <- extract_coi_sentences(body_text)
          # Only use parent text if sentence extraction found COI-specific content
          # (i.e., it's shorter than the full text, meaning it filtered successfully)
          if (nchar(extracted) > 0 && nchar(extracted) < nchar(body_text))
            coi_parts <- c(coi_parts, extracted)
        }
      }
    }

    # 2. Check inside funding divs for embedded COI text
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

    # 3. Fallback: body-text scan in annex/author-notes child divs
    if (length(coi_parts) == 0) {
      annex_divs <- xml_find_all(doc, "//back//div[@type='annex']//div | //back//div[@type='author-notes']//div")
      for (node in annex_divs) {
        body_text <- xml_text(node) |> trimws()
        if (str_detect(body_text, coi_body_re))
          coi_parts <- c(coi_parts, extract_coi_sentences(body_text))
      }
    }

    # 4. Last resort: scan ALL divs for strong COI phrases
    if (length(coi_parts) == 0) {
      strong_coi_re <- regex("(the authors? (have|has|declare|report) no (known )?conflict|no (potential )?conflict.{0,5}interest.{0,30}(report|declar)|we have no conflict)", ignore_case = TRUE)
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

  tibble(doi = doi, rx_funding_text = rx_funding_text, rx_coi_text = rx_coi_text)
}

# ---- Run on dev set ----
message("\nProcessing ", length(dev_files), " TEI files...")
mc_results <- vector("list", length(dev_files))
rx_results <- vector("list", length(dev_files))

for (i in seq_along(dev_files)) {
  message(sprintf("[%d/%d] %s", i, length(dev_files), dev_dois[i]))
  mc_results[[i]] <- extract_metacheck(dev_files[i], dev_dois[i])
  rx_results[[i]] <- extract_regex(dev_files[i], dev_dois[i])
}

mc_df <- bind_rows(mc_results)
rx_df <- bind_rows(rx_results)

# ---- Combine with manual ground truth ----
results <- full_join(mc_df, rx_df, by = "doi") |>
  mutate(
    rx_funding_found = !is.na(rx_funding_text),
    rx_coi_found = !is.na(rx_coi_text)
  ) |>
  left_join(manual_gt, by = "doi")

# Derive manual boolean flags
results <- results |>
  mutate(
    manual_funding_found = !is.na(manual_funding_yn) &
      str_detect(manual_funding_yn, regex("yes", ignore_case = TRUE)),
    manual_coi_found = !is.na(manual_coi_yn) &
      str_detect(manual_coi_yn, regex("yes", ignore_case = TRUE))
  )

# ---- Print comparison ----
cat("\n========== FUNDING COMPARISON ==========\n")
cat(sprintf("%-40s  Manual  MC  Regex\n", "DOI"))
cat(paste(rep("-", 75), collapse = ""), "\n")
for (i in 1:nrow(results)) {
  r <- results[i, ]
  cat(sprintf("%-40s  %-6s  %-3s %-5s\n",
              substr(r$doi, 1, 40),
              ifelse(r$manual_funding_found, "YES", "no"),
              ifelse(r$mc_funding_found, "YES", "no"),
              ifelse(r$rx_funding_found, "YES", "no")))
}

cat("\n========== COI COMPARISON ==========\n")
cat(sprintf("%-40s  Manual  MC  Regex\n", "DOI"))
cat(paste(rep("-", 75), collapse = ""), "\n")
for (i in 1:nrow(results)) {
  r <- results[i, ]
  cat(sprintf("%-40s  %-6s  %-3s %-5s\n",
              substr(r$doi, 1, 40),
              ifelse(r$manual_coi_found, "YES", "no"),
              ifelse(r$mc_coi_found, "YES", "no"),
              ifelse(r$rx_coi_found, "YES", "no")))
}

# ---- Accuracy metrics ----
calc_metrics <- function(predicted, actual) {
  tp <- sum(predicted & actual, na.rm = TRUE)
  fp <- sum(predicted & !actual, na.rm = TRUE)
  fn <- sum(!predicted & actual, na.rm = TRUE)
  tn <- sum(!predicted & !actual, na.rm = TRUE)
  accuracy <- (tp + tn) / (tp + fp + fn + tn)
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0)
    2 * precision * recall / (precision + recall) else NA
  list(tp = tp, fp = fp, fn = fn, tn = tn,
       accuracy = accuracy, precision = precision, recall = recall, f1 = f1)
}

cat("\n========== METRICS ==========\n")
for (what in c("funding", "coi")) {
  actual <- results[[paste0("manual_", what, "_found")]]
  mc_pred <- results[[paste0("mc_", what, "_found")]]
  rx_pred <- results[[paste0("rx_", what, "_found")]]

  mc_m <- calc_metrics(mc_pred, actual)
  rx_m <- calc_metrics(rx_pred, actual)

  cat(sprintf("\n--- %s ---\n", toupper(what)))
  cat(sprintf("  %-12s  Accuracy  Precision  Recall  F1\n", "Method"))
  cat(sprintf("  %-12s  %.3f     %.3f      %.3f   %.3f\n", "Metacheck",
              mc_m$accuracy, mc_m$precision, mc_m$recall, mc_m$f1))
  cat(sprintf("  %-12s  %.3f     %.3f      %.3f   %.3f\n", "Regex",
              rx_m$accuracy, rx_m$precision, rx_m$recall, rx_m$f1))
  cat(sprintf("  TP/FP/FN/TN  MC: %d/%d/%d/%d  Regex: %d/%d/%d/%d\n",
              mc_m$tp, mc_m$fp, mc_m$fn, mc_m$tn,
              rx_m$tp, rx_m$fp, rx_m$fn, rx_m$tn))
}

# ---- Show misses for debugging ----
cat("\n========== REGEX MISSES (found by manual but not regex) ==========\n")
for (what in c("funding", "coi")) {
  actual <- results[[paste0("manual_", what, "_found")]]
  rx_pred <- results[[paste0("rx_", what, "_found")]]
  misses <- results |> filter(actual & !rx_pred)
  if (nrow(misses) > 0) {
    cat(sprintf("\n--- %s misses (%d) ---\n", what, nrow(misses)))
    for (i in 1:nrow(misses)) {
      cat(sprintf("  DOI: %s\n", misses$doi[i]))
      cat(sprintf("  Manual text: %s\n\n", substr(misses[[paste0("manual_", what, "_text")]][i], 1, 200)))
    }
  }
}

cat("\n========== REGEX FALSE POSITIVES ==========\n")
for (what in c("funding", "coi")) {
  actual <- results[[paste0("manual_", what, "_found")]]
  rx_pred <- results[[paste0("rx_", what, "_found")]]
  fps <- results |> filter(!actual & rx_pred)
  if (nrow(fps) > 0) {
    cat(sprintf("\n--- %s false positives (%d) ---\n", what, nrow(fps)))
    for (i in 1:nrow(fps)) {
      cat(sprintf("  DOI: %s\n", fps$doi[i]))
      cat(sprintf("  Extracted: %s\n\n", substr(fps[[paste0("rx_", what, "_text")]][i], 1, 200)))
    }
  }
}

# Save results for inspection
write.csv(results, "data/scratch_dev_results.csv", row.names = FALSE)
message("\nDev results saved to data/scratch_dev_results.csv")
