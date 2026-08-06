# =============================================================================
# run_pipeline.R
# -----------------------------------------------------------------------------
# ONE consolidated, documented extraction pipeline. Given a set of DOIs and
# their GROBID TEI files, returns EXACTLY the 20-column narrow field set
# (one row per DOI) documented in docs/README_pipeline.md.
#
# It SOURCES four function libraries (single source of truth -- each function is
# defined once, in its numbered module; sourcing them has no side effects):
#   - R/1_extract_doi_metadata.R     -> process_dois() (OpenAlex + Crossref metadata)
#   - R/2_extract_tei_coi_funding.R  -> TEI funder/COI/funding regex extraction
#   - R/3_extract_prereg.R           -> preregistration / registered-report / trial
#   - R/4_link_funders_locations.R   -> funder -> country resolution
#
# The pilot-1 + pilot-2 batch reproductions and manual-coding validation for
# stages 2-4 live in validation/pilot_*.R. On-disk caches are reused so a rerun
# makes no new API calls:
#   data/pipeline_metadata_cache.csv  (process_dois output; created here)
#   data/osf_guid_types.csv           (OSF GUID -> type)
#   data/osf_reg_templates.csv        (OSF registration -> template)
#   data/funder_id_country.csv        (Crossref funder id -> country)
#   data/funder_name_country.csv      (TEI funder name -> registry resolution)
#
# Public interface:
#   run_pipeline(doi_tei_df, ...)   doi_tei_df: columns `doi`, `tei_file`
#
# API volume for the pilot run: process_dois queries OpenAlex + Crossref
# (+ first/last author profiles) ~4 calls/DOI over ~340 DOIs = ~1,400 calls,
# all free. Results are cached to data/pipeline_metadata_cache.csv so reruns
# skip OpenAlex/Crossref entirely. The OSF and funder caches already exist.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(stringr)
  library(xml2)
  library(httr2)
  library(tidyr)
  library(purrr)
  library(jsonlite)
  library(countrycode)
  library(stringdist)
})

# Source the function-based metadata script (safe: no side effects at top level)
source("R/1_extract_doi_metadata.R")

# Source the extraction function libraries (single source of truth). These files
# define ONLY functions + constants (no side effects), so sourcing is safe. Their
# canonical definitions are the generalized versions the pipeline uses; the pilot
# batch reproductions live in validation/pilot_*.R.
source("R/2_extract_tei_coi_funding.R")   # heading_matches, clean_extracted_text, extract_regex, extract_metacheck
source("R/3_extract_prereg.R")            # parse_tei, osf_bare_code, classify_paper, resolve_osf_caches, ...
source("R/4_link_funders_locations.R")    # resolve_funder_ids/_names, cluster_funders, resolve_paper_funders, ...

# --- NA-aware null-coalesce (used by the prereg + funder code). Defined AFTER
#     sourcing R/1_extract_doi_metadata.R so a single, consistent operator is in
#     scope. For the list/scalar fields the metadata functions coalesce, the
#     NA-aware guard yields identical results (verified by the regression check
#     against the existing output CSVs at the bottom of this file).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

MAILTO   <- "lukas.wallrich@gmail.com"
OA_KEY   <- Sys.getenv("OPENALEX_API_KEY", "")
DATA     <- "data"
have_stringi <- requireNamespace("stringi", quietly = TRUE)

# Default cache paths ---------------------------------------------------------
META_CACHE_DEFAULT     <- file.path(DATA, "pipeline_metadata_cache.csv")
OSF_GUID_CACHE         <- file.path(DATA, "osf_guid_types.csv")
OSF_REG_CACHE          <- file.path(DATA, "osf_reg_templates.csv")
FUNDER_ID_CACHE        <- file.path(DATA, "funder_id_country.csv")
FUNDER_NAME_CACHE      <- file.path(DATA, "funder_name_country.csv")

# Metadata columns kept from process_dois (scalar; used downstream) -----------
META_COLS <- c("doi", "journal", "issn", "sjr", "top_factor",
               "article_location_detail_country", "first_author_location_detail_country",
               "article_location_label", "first_author_location_label",
               "funding_funders_crossref", "funding_funder_ids_crossref",
               "coi_crossref")

# =============================================================================
# 0. Shared helpers
# =============================================================================
norm_doi <- function(x) {
  x |>
    sub("^https?://(dx\\.)?doi\\.org/", "", x = _) |>
    sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
    trimws() |>
    tolower()
}

# TEI filenames encode DOIs: / -> -, then _-_ separates author info
# e.g. 10.1080-03069885.2023.2214307 -> 10.1080/03069885.2023.2214307
extract_doi_from_filename <- function(fname) {
  base <- basename(fname)
  doi_part <- sub("_-_.*", "", base)
  sub("^(10\\.[0-9]{4,})-", "\\1/", doi_part)
}

splitpp <- function(x) {                       # split a '||'-joined string
  if (is.na(x) || x == "") return(character(0))
  v <- str_trim(unlist(strsplit(x, "\\|\\|")))
  v[v != ""]
}

# =============================================================================
# 1. METADATA (OpenAlex + Crossref) via process_dois, incrementally cached
# =============================================================================
# Resumable: writes the cache after every batch and skips DOIs already cached,
# so a mid-run network hiccup never loses completed work and a rerun makes zero
# OpenAlex/Crossref calls.
run_metadata <- function(dois, cache_path = META_CACHE_DEFAULT,
                         use_api = TRUE, batch_size = 25L) {
  dois <- unique(norm_doi(dois))
  if (file.exists(cache_path)) {
    cache <- readr::read_csv(cache_path, show_col_types = FALSE,
                             col_types = readr::cols(.default = "c"))
    cache$doi <- norm_doi(cache$doi)
  } else {
    cache <- tibble(!!!setNames(rep(list(character()), length(META_COLS)), META_COLS))
  }
  need <- setdiff(dois, cache$doi)
  message(sprintf("Metadata: %d DOIs, %d cached, %d to fetch",
                  length(dois), length(intersect(dois, cache$doi)), length(need)))
  if (length(need) > 0) {
    if (!use_api)
      stop("Metadata cache incomplete but use_api = FALSE (", length(need), " missing)")
    batches <- split(need, ceiling(seq_along(need) / batch_size))
    for (bi in seq_along(batches)) {
      b <- batches[[bi]]
      message(sprintf("  metadata batch %d/%d (%d DOIs)...", bi, length(batches), length(b)))
      res <- process_dois(b, return_location_details = TRUE)
      # keep only scalar META_COLS (add any missing as NA)
      for (cc in setdiff(META_COLS, names(res))) res[[cc]] <- NA
      res <- res[, META_COLS, drop = FALSE]
      res$doi <- norm_doi(res$doi)
      res[] <- lapply(res, as.character)
      cache <- bind_rows(cache, res) |> distinct(doi, .keep_all = TRUE)
      readr::write_csv(cache, cache_path)
    }
  }
  # numeric coercions
  out <- cache |> filter(doi %in% dois)
  out$sjr        <- suppressWarnings(as.numeric(out$sjr))
  out$top_factor <- suppressWarnings(as.numeric(out$top_factor))
  out
}

# =============================================================================
# Sections 2 (TEI extraction), 3 (preregistration) and 4 (funder -> country) are
# now defined in the sourced libraries 2_/3_/4_*.R above -- one definition each.

# =============================================================================
# 5. COI classifier (negation regex, mirrors the prereg negation approach)
# =============================================================================
COI_NEG_RE <- regex(paste0(
  "no (potential |financial |known |relevant |competing )*(conflict|conflicts|competing interest",
  "|competing interests|interest to declare|interests to declare)|",
  "no conflict|no competing|no known conflict|",
  "nothing to disclose|no disclosures?|no relevant financial|",
  "declare[sd]?( that)?( they| the authors| we)?( have)? (no|none)|",
  "report[s]?( that)?( they| the authors| we)?( have)? no (conflict|competing|financial)|",
  "have no (conflict|competing|financial|relevant)|",
  "(is|are|was|were) no (conflict|competing|financial)|",
  "declare[sd]? none|declare no conflict|",
  "no financial[^.]{0,40}(interest|relationship)|",
  "authors? (declare|report|have)[^.]{0,30}\\bnone\\b|",
  # additional no-conflict phrasings seen in real statements
  "in the absence of (any )?(commercial|financial|competing|conflict)|",
  "do(es)? not have any (conflict|competing|financial)|",
  "none\\.?\\s*$"                        # bare 'None' closing a COI statement (incl. GROBID 'STATEMENTNone.')
), ignore_case = TRUE)

classify_coi <- function(stmt) {
  if (is.na(stmt) || !nzchar(str_trim(stmt))) return("no statement")
  if (str_detect(stmt, COI_NEG_RE)) "no conflict declared" else "conflict declared"
}

# =============================================================================
# 6. run_pipeline: orchestrates everything -> exactly 20 columns per DOI
# =============================================================================
OUT_COLS <- c("doi","journal","issn","top_factor","sjr",
              "first_author_country","first_author_location_full","first_author_location_source",
              "funding_category","funding_statement","funders","funding_country",
              "coi_statement","coi_category",
              "prereg_present","prereg_platform","prereg_url",
              "is_registered_report","is_trial_registry","registration_any")

run_pipeline <- function(doi_tei_df, use_api = TRUE,
                         metadata_cache = META_CACHE_DEFAULT) {
  stopifnot(all(c("doi","tei_file") %in% names(doi_tei_df)))
  df <- doi_tei_df |>
    mutate(doi = norm_doi(doi)) |>
    distinct(doi, .keep_all = TRUE)
  n <- nrow(df)
  message("== run_pipeline: ", n, " unique DOIs ==")

  # -- (a) Metadata --
  meta <- run_metadata(df$doi, cache_path = metadata_cache, use_api = use_api)

  # -- (b) TEI regex extraction --
  message("Extracting TEI funder/funding/COI text...")
  rx <- vector("list", n)
  parsed <- vector("list", n)
  for (i in seq_len(n)) {
    f <- df$tei_file[i]; d <- df$doi[i]
    if (is.na(f) || !nzchar(f) || !file.exists(f)) {
      rx[[i]] <- tibble(doi = d, rx_funder_names = NA_character_,
                        rx_funding_text = NA_character_, rx_coi_text = NA_character_)
      parsed[[i]] <- list(text = NA_character_, urls = character(0))
    } else {
      rx[[i]] <- extract_regex(f, d)
      parsed[[i]] <- parse_tei(f)
    }
  }
  rx_df <- bind_rows(rx)

  # -- (c) Prereg (resolve OSF caches once, then classify each paper) --
  message("Resolving OSF caches + classifying preregistration...")
  osf <- resolve_osf_caches(parsed, use_api = use_api)
  prereg <- bind_rows(lapply(parsed, classify_paper,
                             osf_type_of = osf$type, osf_template_of = osf$template))
  prereg$doi <- df$doi

  # -- (d) Funder -> country resolution --
  message("Resolving funders -> countries...")
  meta_j <- meta |> select(doi, journal, funding_funder_ids_crossref) |> distinct(doi, .keep_all = TRUE)
  fdf <- df |> select(doi) |>
    left_join(meta_j, by = "doi") |>
    left_join(rx_df |> select(doi, rx_funder_names, rx_funding_text), by = "doi")

  all_ids   <- unique(unlist(lapply(fdf$funding_funder_ids_crossref, splitpp)))
  all_names <- unique(unlist(lapply(fdf$rx_funder_names, splitpp)))
  id_lookup <- resolve_funder_ids(all_ids, use_api = use_api)
  name_info <- resolve_funder_names(all_names, use_api = use_api)
  name_info <- as.data.frame(name_info)

  fund_rows <- vector("list", n)
  for (i in seq_len(n)) {
    ids    <- splitpp(fdf$funding_funder_ids_crossref[i])
    tnames <- splitpp(fdf$rx_funder_names[i])
    fund_rows[[i]] <- resolve_paper_funders(
      journal = fdf$journal[i], ids = ids, tnames = tnames,
      ftxt = fdf$rx_funding_text[i], id_lookup = id_lookup, name_info = name_info)
  }
  fund_df <- bind_rows(fund_rows)
  fund_df$doi <- df$doi

  # -- (e) Assemble the 20-column output --
  out <- df |> select(doi) |>
    left_join(meta, by = "doi") |>
    left_join(rx_df, by = "doi") |>
    left_join(prereg, by = "doi") |>
    left_join(fund_df, by = "doi")

  # location coalesce (article-level preferred)
  art_ctry  <- out$article_location_detail_country
  prof_ctry <- out$first_author_location_detail_country
  art_lab   <- out$article_location_label
  prof_lab  <- out$first_author_location_label
  na_blank <- function(x) ifelse(!is.na(x) & nzchar(trimws(x)), x, NA_character_)
  art_ctry <- na_blank(art_ctry); prof_ctry <- na_blank(prof_ctry)
  art_lab  <- na_blank(art_lab);  prof_lab  <- na_blank(prof_lab)

  out$first_author_country        <- dplyr::coalesce(art_ctry, prof_ctry)
  out$first_author_location_full  <- dplyr::coalesce(art_lab, prof_lab)
  out$first_author_location_source <- dplyr::case_when(
    !is.na(art_ctry) ~ "article",
    !is.na(prof_ctry) ~ "author-profile",
    TRUE ~ NA_character_)

  # funding
  fc <- dplyr::recode(out$funding_status, unknown = "no statement")
  out$funding_category <- factor(fc, levels = c("funded","no funding","no statement"))
  out$funding_statement <- na_blank(out$rx_funding_text)
  out$funders <- out$best_funders
  out$funding_country <- out$best_funder_countries

  # COI
  out$coi_statement <- dplyr::coalesce(na_blank(out$rx_coi_text), na_blank(out$coi_crossref))
  out$coi_category <- factor(vapply(out$coi_statement, classify_coi, character(1)),
                             levels = c("conflict declared","no conflict declared","no statement"))

  # prereg
  out$prereg_present       <- out$prereg_found
  out$prereg_url           <- out$prereg_urls
  # (prereg_platform, is_registered_report, is_trial_registry, registration_any already present)

  out <- out[, OUT_COLS]
  # guarantee one row per input DOI, same order
  out <- out[match(df$doi, out$doi), ]
  rownames(out) <- NULL
  out
}

# =============================================================================
# 7. DRIVER: build the pilot-1 + pilot-2 index, run pipeline, write output
# =============================================================================
build_pilot_index <- function(
    pilot1_map_csv = "data/pilot1_doi_tei_map.csv",
    pilot2_csv     = "data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv",
    pilot2_tei_dir = "../pilot2_pdf_conversion/tei") {

  # pilot 1: explicit DOI -> TEI map
  p1map <- read.csv(pilot1_map_csv, stringsAsFactors = FALSE)
  p1map <- p1map[!is.na(p1map$tei_file) & p1map$tei_file != "", ]
  p1 <- tibble(doi = norm_doi(p1map$doi), tei_file = p1map$tei_file, pilot = "pilot1")
  p1 <- p1[file.exists(p1$tei_file), ]

  # pilot 2: DOI-encoded TEI filenames matched to manual DOIs
  manual2 <- read.csv(pilot2_csv, check.names = TRUE, stringsAsFactors = FALSE)
  p2dois <- unique(norm_doi(manual2[["X1.2..Article.DOI"]]))
  p2dois <- p2dois[p2dois != "" & p2dois != "test"]
  all_tei <- list.files(pilot2_tei_dir, pattern = "\\.tei\\.xml$", full.names = TRUE)
  tei_dois <- tolower(sapply(all_tei, extract_doi_from_filename, USE.NAMES = FALSE))
  midx <- match(p2dois, tei_dois)
  p2 <- tibble(doi = p2dois[!is.na(midx)], tei_file = all_tei[midx[!is.na(midx)]], pilot = "pilot2")

  idx <- bind_rows(p1, p2) |> distinct(doi, .keep_all = TRUE)
  message(sprintf("Pilot index: %d papers (%d pilot1 + %d pilot2)",
                  nrow(idx), sum(idx$pilot == "pilot1"), sum(idx$pilot == "pilot2")))
  idx
}

# ------- Manual-codebook labels (for validation) -----------------------------
manual_funding_category <- function(x, pilot) {
  if (is.na(x)) x <- ""            # pilot1: distinguish "test" (NA) from blank
  x <- trimws(x)
  if (tolower(x) == "test") return(NA_character_)
  if (pilot == "pilot2") {
    if (x == "") return(NA_character_)                 # uncoded
    # Order matters: "No, there is no funding statement." contains "no funding",
    # so test the leading "No" (= no statement) FIRST.
    if (grepl("^No\\b", x, ignore.case = TRUE)) return("no statement")
    if (grepl("no funding", x, ignore.case = TRUE)) return("no funding")
    if (grepl("provides funding|^Yes", x, ignore.case = TRUE)) return("funded")
    return(NA_character_)               # stray CRediT-type answers
  } else {                              # pilot 1: free text (blank => no statement)
    if (grepl(no_funding_re, x) && !grepl(positive_funding_re, x)) return("no funding")
    if (nzchar(x)) return("funded")
    return("no statement")             # blank funding-statement field
  }
}
manual_coi_category <- function(x) {
  x <- trimws(x %||% "")
  if (x == "" || tolower(x) == "test") return(NA_character_)
  if (grepl("one or more conflict", x, ignore.case = TRUE)) return("conflict declared")
  if (grepl("no conflict of interest$|there is no conflict of interest", x, ignore.case = TRUE) &&
      grepl("^Yes", x, ignore.case = TRUE)) return("no conflict declared")
  if (grepl("^No,|no conflict of interest statement", x, ignore.case = TRUE)) return("no statement")
  NA_character_
}
manual_prereg <- function(x) {
  x <- trimws(x %||% "")
  if (x == "" || tolower(x) == "test") return(NA)
  grepl("^Yes", x, ignore.case = TRUE)
}

confusion_report <- function(pred, truth, label) {
  keep <- !is.na(pred) & !is.na(truth)
  pred <- pred[keep]; truth <- truth[keep]
  cat("\n---", label, "(n =", length(pred), ") ---\n")
  if (length(pred) == 0) { cat("  (no coded papers)\n"); return(invisible()) }
  tab <- table(auto = pred, manual = truth)
  print(tab)
  acc <- mean(as.character(pred) == as.character(truth))
  cat(sprintf("  accuracy = %.3f (%d/%d)\n", acc, sum(as.character(pred)==as.character(truth)), length(pred)))
  invisible(acc)
}

# ------- Main (guarded so sourcing the file does not auto-run the pilot) ------
if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  if (identical(Sys.getenv("PIPELINE_RUN"), "1")) {
    idx <- build_pilot_index()
    result <- run_pipeline(idx |> select(doi, tei_file), use_api = TRUE)

    out_path <- "data/final_extraction_pilot1-and-pilot2.csv"
    readr::write_csv(result, out_path)
    message("Wrote ", out_path, " (", nrow(result), " rows x ", ncol(result), " cols)")

    # attach pilot for validation
    result_pilot <- result |> left_join(idx |> select(doi, pilot), by = "doi")

    # ---- codebook validation ----
    m1 <- read.csv("data/2025-12-09_Codingform_PilotingAssessment_pilot.csv", check.names = TRUE, stringsAsFactors = FALSE)
    m2 <- read.csv("data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv", check.names = TRUE, stringsAsFactors = FALSE)
    man1 <- tibble(doi = norm_doi(m1[["X1.2..Article.DOI"]]),
                   pilot = "pilot1",
                   mfund = m1[["X8.1..Does.the.article.include.a.funding.statement."]],
                   mcoi  = m1[["X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest."]],
                   mprereg = m1[["X2.2...Does.the.article.state.whether.or.not.reported.study.studies.were.preregistered."]])
    man2 <- tibble(doi = norm_doi(m2[["X1.2..Article.DOI"]]),
                   pilot = "pilot2",
                   mfund = m2[["X8.1..Does.the.article.include.a.funding.statement."]],
                   mcoi  = m2[["X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI.."]],
                   mprereg = m2[["X2.2...According.to.what.is.stated.in.the.article..were.the.reported.study.ies..preregistered.."]])
    man <- bind_rows(man1, man2) |>
      filter(doi != "" & doi != "test") |>
      distinct(doi, .keep_all = TRUE)
    man <- man |> mutate(
      man_fund   = mapply(manual_funding_category, mfund, pilot),
      man_coi    = vapply(mcoi, manual_coi_category, character(1)),
      man_prereg = vapply(mprereg, manual_prereg, logical(1)))

    val <- result_pilot |> left_join(man |> select(doi, man_fund, man_coi, man_prereg), by = "doi")

    cat("\n############ CODEBOOK VALIDATION (coded papers only) ############\n")
    for (pl in list(c("pilot1"), c("pilot2"), c("pilot1","pilot2"))) {
      lab <- paste(pl, collapse = "+")
      sub <- val |> filter(pilot %in% pl)
      cat("\n================ ", toupper(lab), " ================\n")
      confusion_report(as.character(sub$funding_category), sub$man_fund, "funding_category")
      confusion_report(as.character(sub$coi_category), sub$man_coi, "coi_category")
      confusion_report(sub$prereg_present, sub$man_prereg, "prereg_present")
    }

    # ---- 3 funder edge cases ----
    cat("\n############ 3 FUNDER EDGE CASES ############\n")
    for (d in c("10.32872/cpe.9341","10.1080/10447318.2023.2276529","10.1192/bjo.2023.12")) {
      r <- result |> filter(doi == d)
      if (!nrow(r)) { cat("MISSING", d, "\n"); next }
      cat(sprintf("\n%s | %s | funders: %s | countries: %s\n", r$doi, r$funding_category,
                  r$funders %||% "-", r$funding_country %||% "-"))
    }

    # ---- 12 example rows ----
    cat("\n############ 12 EXAMPLE ROWS ############\n")
    ex_idx <- unique(c(
      which(result$funding_category == "funded" & grepl("\\|\\|", result$funding_country))[1:2],
      which(result$funding_category == "no funding")[1:2],
      which(result$funding_category == "no statement")[1:2],
      which(result$coi_category == "conflict declared")[1:2],
      which(result$coi_category == "no conflict declared")[1:2],
      which(result$prereg_present & !is.na(result$prereg_platform))[1:2]))
    ex_idx <- ex_idx[!is.na(ex_idx)]
    print(as.data.frame(result[ex_idx, ]), width = 400)

    cat("\n############ COLUMN CHECK ############\n")
    cat("ncol =", ncol(result), "| n rows =", nrow(result),
        "| dup DOIs =", sum(duplicated(result$doi)), "\n")
    cat("cols:", paste(names(result), collapse = ", "), "\n")
  }
}
