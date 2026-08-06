#!/usr/bin/env Rscript
# =============================================================================
# run_full_sample.R
# -----------------------------------------------------------------------------
# DRIVER: run the extraction pipeline over the FULL pre-keyword empirical
# sample, i.e. all articles in full_sample.csv that the LLM classifier marked
# as empirical, BEFORE the pilot-keyword screening. This is what the analysis
# needs in order to compare articles that do and do not report pilot studies
# (team decision, meeting of 2026-07).
#
# Inputs
#   full_sample.csv                                   Zotero export: DOI + PDF path
#   exploration/classification/classified_articles.csv  LLM: empirical yes/no + research_area
#   ../pilot_pilot_teis/tei/                          GROBID TEI for the full sample
#   ../pilot_pilot_teis/pilot_keyword_screening_filenames.csv  keyword-screen hits
#
# Outputs
#   data/full_sample_index.csv               one row per full_sample article:
#                                            doi, tei_file, empirical, research_area,
#                                            keyword_hit, article_status
#                                            (join key for the analysis)
#   data/final_extraction_full-empirical-sample.csv
#                                            the 20-column pipeline output, one row
#                                            per empirical article with a TEI file
#
# DOI -> TEI matching is deterministic: full_sample.csv carries the Zotero PDF
# filename for every record, and GROBID kept that filename (punctuation -> "_",
# truncated). We match on the normalised first 90 characters of the stem.
#
# Usage
#   set -a; source ~/.claude/api_keys.env; set +a     # OpenAlex polite pool
#   Rscript run_full_sample.R              # full run
#   N_MAX=50 Rscript run_full_sample.R     # smoke test on the first 50 articles
#
# API volume: process_dois makes ~4 free OpenAlex/Crossref calls per uncached
# DOI (~3,850 DOIs on a cold cache), plus OSF GUID lookups for OSF links found
# in the TEIs. Everything is cached to data/ and the metadata cache is written
# after every batch of 25, so the run is resumable -- rerunning after an
# interruption picks up where it stopped.
# =============================================================================

source("run_pipeline.R")   # sourcing only defines functions (driver is env-guarded)

FULL_SAMPLE_CSV <- "full_sample.csv"
CLASSIFIED_CSV  <- "exploration/classification/classified_articles.csv"
TEI_DIR         <- "../pilot_pilot_teis/tei"
KEYWORD_CSV     <- "../pilot_pilot_teis/pilot_keyword_screening_filenames.csv"
PILOT1_CODING   <- "data/inputs/2025-12-09_Codingform_PilotingAssessment_pilot.csv"
PILOT2_CODING   <- "data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"
PILOT2_TEI_DIR  <- "../pilot2_pdf_conversion/tei"
INDEX_OUT       <- "data/full_sample_index.csv"
EXTRACTION_OUT  <- "data/final_extraction_full-empirical-sample.csv"

# Normalise a filename stem the way GROBID did (non-alphanumerics -> "_").
norm_stem <- function(x) tolower(gsub("[^A-Za-z0-9]+", "_", x))
STEM_KEY_LEN <- 90L   # TEI filenames are truncated; compare a safe common prefix

# ---------------------------------------------------------------------------
# Manual-coding status
# ---------------------------------------------------------------------------
# Read the DOIs out of a coding form (the column is named differently in the
# two rounds, so match on the name rather than a fixed position).
coding_form_dois <- function(path) {
  if (!file.exists(path)) return(character(0))
  d <- read.csv(path, check.names = TRUE, stringsAsFactors = FALSE)
  col <- grep("DOI", names(d), ignore.case = TRUE)[1]
  if (is.na(col)) return(character(0))
  v <- norm_doi(d[[col]])               # norm_doi also strips a "doi:" prefix
  unique(v[!is.na(v) & nzchar(v) & grepl("^10\\.", v)])   # drops "test" rows
}

# Label each article by the manual coding round it was drawn into.
#   pilot1            manually coded in pilot round 1
#   pilot2            manually coded in pilot round 2
#   pilot1-unsampled  in the frame, never drawn into a coding round
pilot_status <- function(dois,
                         pilot1_csv = PILOT1_CODING,
                         pilot2_csv = PILOT2_CODING) {
  d1 <- coding_form_dois(pilot1_csv)
  d2 <- coding_form_dois(pilot2_csv)
  out <- rep("pilot1-unsampled", length(dois))
  out[dois %in% d2] <- "pilot2"
  out[dois %in% d1] <- "pilot1"     # pilot 1 wins if a DOI somehow appears in both
  out
}

# Index rows for the pilot-2 articles, whose TEI filenames encode the DOI.
# Reuses build_pilot_index() from run_pipeline.R (the single source of truth for
# pilot-2 DOI -> TEI matching) and keeps only its pilot-2 half.
build_pilot2_rows <- function(pilot2_csv = PILOT2_CODING,
                              pilot2_tei_dir = PILOT2_TEI_DIR) {
  if (!dir.exists(pilot2_tei_dir)) {
    warning("Pilot-2 TEI directory not found: ", pilot2_tei_dir,
            " -- pilot-2 articles are omitted")
    return(tibble())
  }
  pilot_idx <- build_pilot_index(pilot2_csv = pilot2_csv,
                                 pilot2_tei_dir = pilot2_tei_dir)
  pilot_idx |>
    filter(pilot == "pilot2") |>
    transmute(doi,
              pdf_stem       = basename(tei_file),
              tei_file,
              empirical      = NA_character_,
              research_area  = NA_character_,
              keyword_hit    = NA,
              article_status = "pilot2")
}

# ---------------------------------------------------------------------------
# Build the index: one row per full_sample article
# ---------------------------------------------------------------------------
build_full_sample_index <- function(full_sample_csv = FULL_SAMPLE_CSV,
                                    classified_csv  = CLASSIFIED_CSV,
                                    tei_dir         = TEI_DIR,
                                    keyword_csv     = KEYWORD_CSV) {
  fs <- readr::read_csv(full_sample_csv, show_col_types = FALSE)
  stopifnot(all(c("DOI", "File Attachments") %in% names(fs)))

  pdf_stem <- basename(gsub("\\\\", "/", fs$`File Attachments`))
  pdf_stem <- sub("\\.pdf$", "", pdf_stem)

  tei_files <- list.files(tei_dir, pattern = "\\.tei\\.xml$", full.names = TRUE)
  tei_stem  <- sub("\\.tei\\.xml$", "", basename(tei_files))

  key_pdf <- substr(norm_stem(pdf_stem), 1, STEM_KEY_LEN)
  key_tei <- substr(norm_stem(tei_stem), 1, STEM_KEY_LEN)
  if (any(duplicated(key_tei)))
    warning("Ambiguous TEI filename prefixes: ", sum(duplicated(key_tei)))

  m <- match(key_pdf, key_tei)

  idx <- tibble(
    doi       = norm_doi(fs$DOI),
    pdf_stem  = pdf_stem,
    tei_file  = ifelse(is.na(m), NA_character_, tei_files[m])
  )

  # LLM classification: empirical yes/no + research area
  cls <- readr::read_csv(classified_csv, show_col_types = FALSE) |>
    transmute(doi = norm_doi(doi), empirical, research_area) |>
    distinct(doi, .keep_all = TRUE)
  idx <- idx |> left_join(cls, by = "doi")

  # Pilot-keyword screening hits (recorded for the analysis; NOT a filter here)
  if (file.exists(keyword_csv)) {
    kw <- readr::read_csv(keyword_csv, show_col_types = FALSE)
    kw_key <- substr(norm_stem(sub("\\.tei\\.xml$", "", kw$tei_file)), 1, STEM_KEY_LEN)
    idx$keyword_hit <- substr(norm_stem(idx$pdf_stem), 1, STEM_KEY_LEN) %in% kw_key
  } else {
    idx$keyword_hit <- NA
  }

  idx <- idx |> filter(!is.na(doi), doi != "") |> distinct(doi, .keep_all = TRUE)

  # Which manual coding round (if any) each article belongs to.
  idx$article_status <- pilot_status(idx$doi)

  # Pilot 2 was drawn from a different, broader frame: none of its articles are
  # in full_sample.csv. Append them so the manually coded set is complete. They
  # carry no LLM classification (that ran over full_sample.csv only) and no
  # keyword-screen result, so both stay NA -- `article_status` is what selects
  # them, not `empirical`.
  p2 <- build_pilot2_rows()
  p2 <- p2 |> filter(!doi %in% idx$doi)
  if (nrow(p2) > 0) {
    idx <- bind_rows(idx, p2)
    message(sprintf("  appended %d pilot-2 articles from a separate frame (%s)",
                    nrow(p2), PILOT2_TEI_DIR))
  }

  message(sprintf(
    "Full-sample index: %d articles | %d with TEI | %d empirical | %d empirical+TEI | %d keyword hits",
    nrow(idx), sum(!is.na(idx$tei_file)),
    sum(idx$empirical == "yes", na.rm = TRUE),
    sum(idx$empirical == "yes" & !is.na(idx$tei_file), na.rm = TRUE),
    sum(idx$keyword_hit, na.rm = TRUE)))

  unmatched <- idx |> filter(is.na(tei_file))
  if (nrow(unmatched) > 0) {
    message("  ", nrow(unmatched), " articles without a TEI file, e.g.:")
    message(paste0("    - ", head(unmatched$pdf_stem, 5), collapse = "\n"))
  }
  missing_cls <- sum(is.na(idx$empirical) & idx$article_status != "pilot2")
  if (missing_cls > 0)
    message("  ", missing_cls, " articles without an LLM classification (treated as non-empirical)")

  # Manual-coding coverage: how many coded articles the index actually contains.
  for (st in c("pilot1", "pilot2")) {
    in_idx  <- idx |> filter(article_status == st)
    coded_n <- length(coding_form_dois(if (st == "pilot1") PILOT1_CODING else PILOT2_CODING))
    message(sprintf("  %s: %d of %d coded articles in the index, %d with a TEI file",
                    st, nrow(in_idx), coded_n, sum(!is.na(in_idx$tei_file))))
  }

  idx
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
if (sys.nframe() == 0L || identical(Sys.getenv("FULL_SAMPLE_RUN"), "1")) {
  idx <- build_full_sample_index()
  readr::write_csv(idx, INDEX_OUT)
  message("Wrote ", INDEX_OUT)

  # Extract for the empirical articles, plus EVERY manually coded article
  # regardless of the empirical classification. The coded articles are the
  # validation set for the other metadata fields, so dropping the ones the
  # classifier called non-empirical would shrink that set for no good reason.
  # The analysis filters on `empirical` and `article_status` in the index.
  emp <- idx |> filter(!is.na(tei_file),
                       empirical == "yes" | article_status %in% c("pilot1", "pilot2"))
  message(sprintf("Extracting for %d articles (%d empirical + %d coded but non-empirical)",
                  nrow(emp), sum(emp$empirical == "yes", na.rm = TRUE),
                  sum(!(emp$empirical %in% "yes"))))

  n_max <- suppressWarnings(as.integer(Sys.getenv("N_MAX", "")))
  if (!is.na(n_max) && n_max > 0) {
    message("N_MAX set: restricting to the first ", n_max, " articles (smoke test)")
    emp <- head(emp, n_max)
  }

  result <- run_pipeline(emp |> select(doi, tei_file), use_api = TRUE)

  readr::write_csv(result, EXTRACTION_OUT)
  message("Wrote ", EXTRACTION_OUT, " (", nrow(result), " rows x ", ncol(result), " cols)")

  cat("\n############ COVERAGE ############\n")
  cat("rows:", nrow(result), "| dup DOIs:", sum(duplicated(result$doi)), "\n")
  print(table(idx$article_status[match(result$doi, idx$doi)], useNA = "ifany"))
  cat("first_author_country present:", sum(!is.na(result$first_author_country)), "\n")
  cat("funding_category:\n"); print(table(result$funding_category, useNA = "ifany"))
  cat("coi_category:\n");     print(table(result$coi_category, useNA = "ifany"))
  cat("prereg_present:\n");   print(table(result$prereg_present, useNA = "ifany"))
}
