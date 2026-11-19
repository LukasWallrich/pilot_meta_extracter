# =============================================================================
# run_coi_pdf_fallback.R -- optional step: PDF front-page COI fallback
# -----------------------------------------------------------------------------
# Applies R/5_pdf_coi_fallback.R to data/final_extraction_pilot1-and-pilot2.csv
# in place, adding/refreshing `coi_source` (tei / crossref / pdf_front). Use this
# when the TEI-based run is already done; a fresh run can instead call
# run_pipeline(..., pdf_coi_fallback = TRUE) with a `pdf_file` column.
# Needs the source PDF folder (set PILOT_PDF_ROOT), pdftotext, and the Pilot 2
# TEI folder (only its file names are read, to find each article's PDF).
# Safe to rerun: rows that already have a statement are left alone, and the
# per-DOI text cache (data/cache/pdf_front_text/) avoids re-reading PDFs.
#
#   Rscript run_coi_pdf_fallback.R
# =============================================================================
suppressMessages({ library(dplyr); library(readr); library(stringr) })
source("run_pipeline.R")   # classify_coi, norm_doi, module 5

OUT_PATH       <- "data/final_extraction_pilot1-and-pilot2.csv"
PILOT2_CODING  <- "data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"
PILOT2_TEI_DIR <- "../pilot2_pdf_conversion/tei"

meta <- read_csv(META_CACHE_DEFAULT, show_col_types = FALSE, col_types = cols(.default = "c"))
meta$doi <- norm_doi(meta$doi)

# TEI file per DOI (only its name is used, to find the PDF): the Pilot 1 map,
# then the Pilot 2 TEIs matched by DOI as in build_pilot_index().
p1map <- read_csv("data/inputs/pilot1_doi_tei_map.csv", show_col_types = FALSE) |>
  filter(!is.na(tei_file), tei_file != "")
m2 <- read.csv(PILOT2_CODING, check.names = TRUE, stringsAsFactors = FALSE)
p2dois <- unique(norm_doi(m2[["X1.2..Article.DOI"]]))
p2dois <- p2dois[p2dois != "" & p2dois != "test"]
p2tei <- list.files(PILOT2_TEI_DIR, pattern = "\\.tei\\.xml$", full.names = TRUE)
if (!length(p2tei)) stop("No Pilot 2 TEI files found in ", PILOT2_TEI_DIR)
midx <- match(doi_key(p2dois), tei_doi_key(p2tei))
tei_of <- bind_rows(
  p1map |> transmute(doi = norm_doi(doi), tei_file),
  tibble(doi = p2dois[!is.na(midx)], tei_file = p2tei[midx[!is.na(midx)]])) |>
  distinct(doi, .keep_all = TRUE)

out <- read_csv(OUT_PATH, show_col_types = FALSE, guess_max = 10000)
message("\n== ", OUT_PATH, " (", nrow(out), " rows)")
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
write_csv(out, OUT_PATH)
print(table(out$coi_source, useNA = "ifany"))
