#!/usr/bin/env Rscript
# =============================================================================
# run_pilot2_sample.R
# -----------------------------------------------------------------------------
# DRIVER: run the extraction pipeline over the FULL pilot-2 sampling frame --
# every 2023 article in pilot2_sample.csv, not only the 184 that were manually
# coded. Pilot 2 was drawn from a different frame than pilot 1, so this is the
# pilot-2 counterpart of run_full_sample.R and the two outputs stack.
#
# Inputs
#   pilot2_sample.csv                          Zotero export of the frame: DOI +
#                                              bibliographic metadata (4,635 rows)
#   ../pilot2_pdf_conversion/tei/              GROBID TEI, filenames encode the DOI
#   ../pilot2_pdf_conversion/pilot_keyword_screening.csv
#                                              pilot-keyword screen, by DOI
#   data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv
#                                              which articles were manually coded
#
# Outputs
#   data/pilot2_sample_index.csv               one row per frame article:
#                                              doi, tei_file, keyword_hit,
#                                              article_status (join key)
#   data/final_extraction_pilot2-sample.csv    the 20-column pipeline output, one
#                                              row per frame article with a TEI
#
# No empirical / research-area classification is attached: the LLM classifier
# has run over the pilot-1 frame only. Those columns are absent here rather than
# NA-filled, and the frame is extracted unfiltered.
#
# Usage
#   set -a; source ~/.claude/api_keys.env; set +a     # OpenAlex polite pool
#   Rscript run_pilot2_sample.R            # full run
#   N_MAX=50 Rscript run_pilot2_sample.R   # smoke test on the first 50 articles
#
# Resumable: metadata, OSF and funder lookups are cached under data/cache/, so a
# rerun after an interruption makes no repeat network calls.
# =============================================================================

source("run_pipeline.R")   # sourcing only defines functions (driver is env-guarded)

FRAME_CSV      <- "pilot2_sample.csv"
TEI_DIR        <- "../pilot2_pdf_conversion/tei"
KEYWORD_CSV    <- "../pilot2_pdf_conversion/pilot_keyword_screening.csv"
PILOT2_CODING  <- "data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"
INDEX_OUT      <- "data/pilot2_sample_index.csv"
EXTRACTION_OUT <- "data/final_extraction_pilot2-sample.csv"

# ---------------------------------------------------------------------------
# Build the index: one row per frame article
# ---------------------------------------------------------------------------
build_pilot2_sample_index <- function(frame_csv   = FRAME_CSV,
                                      tei_dir     = TEI_DIR,
                                      keyword_csv = KEYWORD_CSV,
                                      coding_csv  = PILOT2_CODING) {
  fr <- readr::read_csv(frame_csv, show_col_types = FALSE)
  stopifnot("DOI" %in% names(fr))

  idx <- tibble(doi = norm_doi(fr$DOI)) |>
    filter(!is.na(doi), doi != "") |>
    distinct(doi, .keep_all = TRUE)

  # DOI -> TEI. Filenames carry the DOI with every "/" flattened to "-", so
  # match on doi_key() (defined in run_pipeline.R), which treats the two alike.
  all_tei <- list.files(tei_dir, pattern = "\\.tei\\.xml$", full.names = TRUE)
  m <- match(doi_key(idx$doi), tei_doi_key(all_tei))
  idx$tei_file <- ifelse(is.na(m), NA_character_, all_tei[m])

  # Pilot-keyword screening hits (recorded for the analysis; NOT a filter here)
  if (file.exists(keyword_csv)) {
    kw <- readr::read_csv(keyword_csv, show_col_types = FALSE)
    hit <- doi_key(kw$doi[kw$include])
    screened <- doi_key(kw$doi)
    idx$keyword_hit <- ifelse(doi_key(idx$doi) %in% screened,
                              doi_key(idx$doi) %in% hit, NA)
  } else {
    idx$keyword_hit <- NA
  }

  # Which articles the manual coding round drew.
  coded <- read.csv(coding_csv, check.names = TRUE, stringsAsFactors = FALSE)
  coded_dois <- unique(norm_doi(coded[["X1.2..Article.DOI"]]))
  coded_dois <- coded_dois[grepl("^10\\.", coded_dois)]
  idx$article_status <- ifelse(doi_key(idx$doi) %in% doi_key(coded_dois),
                               "pilot2-coded", "pilot2-unsampled")

  message(sprintf(
    "Pilot-2 index: %d articles | %d with TEI | %d keyword-screened (%d hits) | %d manually coded",
    nrow(idx), sum(!is.na(idx$tei_file)), sum(!is.na(idx$keyword_hit)),
    sum(idx$keyword_hit, na.rm = TRUE), sum(idx$article_status == "pilot2-coded")))

  missed <- setdiff(doi_key(coded_dois), doi_key(idx$doi))
  if (length(missed) > 0)
    message("  ", length(missed), " manually coded DOIs are not in the frame: ",
            paste(missed, collapse = ", "))

  n_no_tei <- sum(is.na(idx$tei_file))
  if (n_no_tei > 0)
    message("  ", n_no_tei, " articles have no TEI yet -- their PDFs are still ",
            "queued for GROBID conversion; rerun once the conversion finishes.")

  idx
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
if (sys.nframe() == 0L || identical(Sys.getenv("PILOT2_SAMPLE_RUN"), "1")) {
  idx <- build_pilot2_sample_index()
  readr::write_csv(idx, INDEX_OUT)
  message("Wrote ", INDEX_OUT)

  todo <- idx |> filter(!is.na(tei_file))

  n_max <- suppressWarnings(as.integer(Sys.getenv("N_MAX", "")))
  if (!is.na(n_max) && n_max > 0) {
    message("N_MAX set: restricting to the first ", n_max, " articles (smoke test)")
    todo <- head(todo, n_max)
  }

  message("Extracting for ", nrow(todo), " articles with a TEI file")
  result <- run_pipeline(todo |> select(doi, tei_file), use_api = TRUE)

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
