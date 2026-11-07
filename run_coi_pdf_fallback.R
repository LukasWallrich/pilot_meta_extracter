# =============================================================================
# run_coi_pdf_fallback.R -- optional step: PDF front-page COI fallback
# -----------------------------------------------------------------------------
# Applies R/5_pdf_coi_fallback.R to the existing final extraction CSVs in place,
# adding/refreshing `coi_source` (tei / crossref / pdf_front). Use this when the
# TEI-based run is already done (the pilot-1 TEIs need not be on disk); a fresh
# run can instead call run_pipeline(..., pdf_coi_fallback = TRUE) with a
# `pdf_file` column. Needs the synced PDF folder (PDF_ROOT_DEFAULT) and pdftotext.
# Safe to rerun: rows that already have a statement are left alone, and the
# per-DOI text cache (data/cache/pdf_front_text/) avoids re-reading PDFs.
#
#   Rscript run_coi_pdf_fallback.R
# =============================================================================
suppressMessages({ library(dplyr); library(readr); library(stringr) })
source("run_pipeline.R")   # classify_coi, norm_doi, module 5

meta <- read_csv(META_CACHE_DEFAULT, show_col_types = FALSE, col_types = cols(.default = "c"))
meta$doi <- norm_doi(meta$doi)

# TEI file per DOI (only its name is used, to find the PDF)
p1map <- read_csv("data/inputs/pilot1_doi_tei_map.csv", show_col_types = FALSE)
tei_of <- bind_rows(
  read_csv("data/full_sample_index.csv", show_col_types = FALSE) |> select(doi, tei_file),
  read_csv("data/pilot2_sample_index.csv", show_col_types = FALSE) |> select(doi, tei_file),
  p1map |> select(doi, tei_file)) |>
  mutate(doi = norm_doi(doi)) |> filter(!is.na(tei_file)) |> distinct(doi, .keep_all = TRUE)

add_pdf_fallback <- function(path) {
  out <- read_csv(path, show_col_types = FALSE, guess_max = 10000)
  message("\n== ", path, " (", nrow(out), " rows)")
  if (!"coi_source" %in% names(out)) {
    # Reconstruct provenance of existing statements: the pipeline coalesces TEI
    # text before Crossref, so a statement equal to the Crossref text came from Crossref.
    cr <- meta$coi_crossref[match(out$doi, meta$doi)]
    out$coi_source <- case_when(is.na(out$coi_statement) ~ NA_character_,
                                !is.na(cr) & out$coi_statement == cr ~ "crossref",
                                TRUE ~ "tei")
  }
  pdfs <- map_pdf_files(tei_of$tei_file[match(out$doi, tei_of$doi)])
  out <- apply_pdf_coi_fallback(out, pdfs)
  out <- out |> relocate(coi_source, .after = coi_category)
  stopifnot(identical(names(out), OUT_COLS))
  write_csv(out, path)
  print(table(out$coi_source, useNA = "ifany"))
}

for (f in c("data/final_extraction_pilot1-and-pilot2.csv",
            "data/final_extraction_full-empirical-sample.csv",
            "data/final_extraction_pilot2-sample.csv")) add_pdf_fallback(f)
