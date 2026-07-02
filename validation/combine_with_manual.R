# Combine the manual coding (both pilots) with the NEW automated extraction
# (run_pipeline.R -> data/final_extraction_pilot1-and-pilot2.csv).
# Run from the project root:  Rscript validation/combine_with_manual.R
suppressMessages({library(dplyr); library(stringr); library(readr)})

clean_doi <- function(x) {
  x |>
    sub("^https?://doi\\.org/", "", x = _) |>
    sub("^doi\\.org/", "", x = _) |>
    sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
    str_trim()
}

p1 <- read.csv("data/2025-12-09_Codingform_PilotingAssessment_pilot.csv",
               stringsAsFactors = FALSE, check.names = TRUE) |> mutate(coding = "pilot1")
p2 <- read.csv("data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv",
               stringsAsFactors = FALSE, check.names = TRUE) |> mutate(coding = "pilot2")

manual <- bind_rows(p1, p2) |>
  mutate(doi = clean_doi(X1.2..Article.DOI)) |>
  filter(doi != "" & doi != "test")

auto <- read_csv("data/final_extraction_pilot1-and-pilot2.csv", show_col_types = FALSE) |>
  mutate(doi = clean_doi(doi))

combined <- manual |> left_join(auto, by = "doi")

out <- "data/manual-and-auto-combined_2026-07-01.csv"
write_csv(combined, out)
message(sprintf("Wrote %s: %d rows (%d matched to auto), %d columns",
                out, nrow(combined), sum(combined$doi %in% auto$doi), ncol(combined)))
