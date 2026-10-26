# =============================================================================
# validation/pilot2_auto_vs_manual.R
# -----------------------------------------------------------------------------
# Pilot-2 coverage of funding and COI statements, as reported in the Stage 1
# manuscript: for the 84 articles coded as reporting a pilot, cross-tabulates
# whether coders recorded a statement against whether automated extraction
# found one. "Automated" = any Crossref funding/COI field (from the combined
# file built in docs/write-up-for-reg-rep.Rmd) OR the TEI extractor
# (data/stages/tei_coi_funding_extracted_pilot1-and-pilot2.csv, current run).
# Same definitions as the write-up; only the TEI results are refreshed.
# Run from the project root:  Rscript validation/pilot2_auto_vs_manual.R
# Writes data/pilot2_auto_vs_manual_counts.csv
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

combined_csv <- "data/inputs/2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv"
tei_csv      <- "data/stages/tei_coi_funding_extracted_pilot1-and-pilot2.csv"
out_csv      <- "data/pilot2_auto_vs_manual_counts.csv"

nonblank <- function(x) {
  x <- as.character(x)
  !is.na(x) & str_trim(x) != "" & !str_to_lower(str_trim(x)) %in% c("na", "n/a")
}

tei <- read.csv(tei_csv, stringsAsFactors = FALSE) |>
  filter(coding == "pilot2") |>
  select(doi, rx_funding_found, rx_coi_found)

d <- read_csv(combined_csv, show_col_types = FALSE) |>
  filter(doi != "test", coding == "pilot2") |>
  filter(str_detect(X1.3..Is.there.a.statement.in.the.article.that.there.was.a.pilot.study.conducted., "Yes")) |>
  select(-any_of(c("rx_funding_found", "rx_coi_found"))) |>
  left_join(tei, by = "doi") |>
  mutate(
    funding_auto   = if_any(starts_with("funding"), nonblank) | rx_funding_found %in% TRUE,
    funding_manual = nonblank(X8.1.1.If.the.article.include.a.funding.statement..what.is.the.statement.),
    coi_auto       = if_any(starts_with("coi"), nonblank) | rx_coi_found %in% TRUE,
    coi_manual     = str_starts(X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI.., "Yes") %in% TRUE
  )

counts <- bind_rows(
  count(d, manual = funding_manual, auto = funding_auto) |> mutate(outcome = "funding"),
  count(d, manual = coi_manual, auto = coi_auto) |> mutate(outcome = "coi")
) |>
  select(outcome, manual, auto, n)

write.csv(counts, out_csv, row.names = FALSE)
message("Articles: ", nrow(d))
print(counts)
