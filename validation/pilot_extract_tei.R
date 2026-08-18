# =============================================================================
# validation/pilot_extract_tei.R
# -----------------------------------------------------------------------------
# PILOT-1 + PILOT-2 batch reproduction of the TEI funder/funding/COI extraction.
# Rebuilds the pilot DOI->TEI index exactly as the original stage script, runs
# the library extractor over every paper, and writes the stage CSVs:
#   data/stages/tei_coi_funding_extracted_pilot1-and-pilot2.csv
# (both pilots in one file; the `coding` column marks the round)
# Set use_metacheck <- TRUE before sourcing to also run metacheck for comparison.
# Run from the project root:  Rscript validation/pilot_extract_tei.R
# =============================================================================
source("R/2_extract_tei_coi_funding.R")   # heading_matches, clean_extracted_text, extract_regex, extract_metacheck

# ---- Settings ----
pilot1_map_csv <- "data/inputs/pilot1_doi_tei_map.csv"
pilot2_tei_dir <- "../pilot2_pdf_conversion/tei"
pilot2_csv     <- "data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"
# The oracle stage CSV includes metacheck columns, so reproduce with metacheck on
# by default (set use_metacheck <- FALSE before sourcing to skip the comparison).
if (!exists("use_metacheck")) use_metacheck <- TRUE

if (use_metacheck) {
  if (!"metacheck" %in% rownames(installed.packages())) {
    remotes::install_github("scienceverse/metacheck", dependencies = TRUE)
  }
  library(metacheck)
}

# ---- 1. Build a unified (doi, coding, tei_file) index ----

## pilot1: from the explicit DOI->TEI map
p1map <- read.csv(pilot1_map_csv, stringsAsFactors = FALSE)
p1map <- p1map[!is.na(p1map$tei_file) & p1map$tei_file != "", ]
p1_idx <- tibble(
  doi      = trimws(p1map$doi),
  coding   = "pilot1",
  tei_file = p1map$tei_file
)
p1_exists <- file.exists(p1_idx$tei_file)
if (any(!p1_exists)) {
  message(sum(!p1_exists), " pilot1 TEI files missing on disk, dropping: ",
          paste(head(p1_idx$doi[!p1_exists], 5), collapse = ", "))
  p1_idx <- p1_idx[p1_exists, ]
}
message(nrow(p1_idx), " pilot1 papers from map")

## pilot2: DOIs from manual CSV, matched to DOI-encoded TEI filenames
manual <- read.csv(pilot2_csv)
pilot2_dois <- manual[["X1.2..Article.DOI"]] |>
  sub("^https?://doi\\.org/", "", x = _) |>
  sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
  trimws() |>
  unique()
pilot2_dois <- pilot2_dois[pilot2_dois != "" & pilot2_dois != "test"]
message(length(pilot2_dois), " unique pilot2 DOIs loaded")

all_tei <- list.files(pilot2_tei_dir, pattern = "\\.tei\\.xml$", full.names = TRUE)

# TEI filenames encode DOIs with every "/" flattened to "-", then "_-_" before
# the author info. A DOI suffix can contain "-" as well, so match on a key that
# treats the two alike: 10.1093/abm/kaac079 -> 10.1093-abm-kaac079
doi_key <- function(x) tolower(gsub("/", "-", x))
tei_doi_key <- function(fname) doi_key(sub("_-_.*", "", basename(fname)))

tei_dois <- tei_doi_key(all_tei)
names(all_tei) <- tei_dois

matched_idx    <- match(doi_key(pilot2_dois), tei_dois)
matched_dois   <- pilot2_dois[!is.na(matched_idx)]
matched_files  <- all_tei[matched_idx[!is.na(matched_idx)]]
unmatched_dois <- pilot2_dois[is.na(matched_idx)]
message(length(matched_dois), " pilot2 DOIs matched to TEI files, ",
        length(unmatched_dois), " unmatched")
if (length(unmatched_dois) > 0) {
  message("Unmatched pilot2 DOIs: ", paste(head(unmatched_dois, 10), collapse = ", "),
          if (length(unmatched_dois) > 10) "..." else "")
}

p2_idx <- tibble(
  doi      = matched_dois,
  coding   = "pilot2",
  tei_file = unname(matched_files)
)

idx <- bind_rows(p1_idx, p2_idx)
message("\nUnified index: ", nrow(idx), " papers (",
        sum(idx$coding == "pilot1"), " pilot1 + ",
        sum(idx$coding == "pilot2"), " pilot2)")

# ---- 4. Run extraction over the unified index ----
message("\nExtracting from ", nrow(idx), " TEI files...",
        if (use_metacheck) " (with metacheck)" else "")

rx_results <- vector("list", nrow(idx))
if (use_metacheck) mc_results <- vector("list", nrow(idx))

for (i in seq_len(nrow(idx))) {
  f <- idx$tei_file[i]
  d <- idx$doi[i]
  message(sprintf("[%d/%d] %s (%s)", i, nrow(idx), d, idx$coding[i]))

  rx_results[[i]] <- extract_regex(f, d)
  if (use_metacheck) mc_results[[i]] <- extract_metacheck(f, d)
}

rx_df <- bind_rows(rx_results)
rx_df$coding <- idx$coding

# ---- 5. Combine and save ----
if (use_metacheck) {
  mc_df <- bind_rows(mc_results)
  tei_extracted <- left_join(rx_df, mc_df, by = "doi")
} else {
  tei_extracted <- rx_df
  tei_extracted$mc_funding_found <- NA
  tei_extracted$mc_funding_text  <- NA_character_
  tei_extracted$mc_coi_found     <- NA
  tei_extracted$mc_coi_text      <- NA_character_
}

# Funding evidence exists if a funder is named OR statement prose was found
tei_extracted <- tei_extracted |>
  mutate(
    rx_funding_found = !is.na(rx_funder_names) | !is.na(rx_funding_text),
    rx_coi_found     = !is.na(rx_coi_text)
  ) |>
  select(doi, coding, rx_funder_names, rx_funding_text, rx_coi_text,
         rx_funding_found, rx_coi_found,
         mc_funding_found, mc_funding_text, mc_coi_found, mc_coi_text)

combined_file <- "data/stages/tei_coi_funding_extracted_pilot1-and-pilot2.csv"
write.csv(tei_extracted, combined_file, row.names = FALSE)
message("\nCombined results saved to ", combined_file)

# The pilot2-only file is no longer written: it was an exact subset of the
# combined file above. Filter on `coding == "pilot2"` instead.

# ---- 6. Quick summary ----
message("\n--- Summary ---")
message("Total papers processed: ", nrow(tei_extracted))
for (pl in c("pilot1", "pilot2")) {
  sub <- tei_extracted |> filter(coding == pl)
  message("\n[", pl, "] n = ", nrow(sub))
  message("  Regex funding found: ", sum(sub$rx_funding_found, na.rm = TRUE),
          " (", round(100 * mean(sub$rx_funding_found, na.rm = TRUE), 1), "%)")
  message("  Regex COI found:     ", sum(sub$rx_coi_found, na.rm = TRUE),
          " (", round(100 * mean(sub$rx_coi_found, na.rm = TRUE), 1), "%)")
  message("  Funder names: real = ",
          sum(!is.na(sub$rx_funder_names) & sub$rx_funder_names != "no funding"),
          ", 'no funding' = ", sum(sub$rx_funder_names == "no funding", na.rm = TRUE),
          ", NA = ", sum(is.na(sub$rx_funder_names)))
  if (use_metacheck) {
    message("  Metacheck funding found: ", sum(sub$mc_funding_found, na.rm = TRUE))
    message("  Metacheck COI found:     ", sum(sub$mc_coi_found, na.rm = TRUE))
  }
}
