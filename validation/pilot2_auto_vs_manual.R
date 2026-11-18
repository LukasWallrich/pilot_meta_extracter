# Pilot 2 statement presence: final metadata/XML/PDF pipeline vs original codes.
# Sourcing defines the read-only comparison; running writes its counts and audit.
# Run from the repo root: Rscript validation/pilot2_auto_vs_manual.R

pilot2_final_statement_comparison <- function() {
  # Reuse original-code mappings without executing the guarded extraction batch.
  source("run_pipeline.R", local = TRUE)
  m <- read.csv("data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv",
                check.names = TRUE, stringsAsFactors = FALSE)
  manual <- tibble::tibble(
    doi = norm_doi(m[["X1.2..Article.DOI"]]),
    funding_manual = vapply(m[["X8.1..Does.the.article.include.a.funding.statement."]],
                            manual_funding_category, character(1), pilot = "pilot2"),
    coi_manual = vapply(m[["X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI.."]],
                        manual_coi_category, character(1))
  ) |>
    dplyr::filter(!is.na(doi), doi != "", doi != "test") |>
    dplyr::distinct(doi, .keep_all = TRUE)
  final <- readr::read_csv("data/final_extraction_pilot1-and-pilot2.csv",
                           show_col_types = FALSE) |>
    dplyr::mutate(doi = norm_doi(doi))
  stopifnot(!anyDuplicated(final$doi))
  d <- dplyr::inner_join(manual, final, by = "doi")
  dplyr::bind_rows(lapply(c("funding", "coi"), function(outcome) {
    manual_category <- d[[paste0(outcome, "_manual")]]
    automated_category <- d[[paste0(outcome, "_category")]]
    tibble::tibble(doi = d$doi, outcome = outcome,
                   manual_category = manual_category,
                   automated_category = automated_category,
                   manual = manual_category != "no statement",
                   auto = automated_category != "no statement")
  })) |>
    dplyr::filter(!is.na(manual), !is.na(auto))
}

if (sys.nframe() == 0) {
  comparison <- pilot2_final_statement_comparison()
  counts <- comparison |>
    dplyr::count(outcome, manual, auto) |>
    tidyr::complete(outcome, manual = c(FALSE, TRUE), auto = c(FALSE, TRUE),
                    fill = list(n = 0))
  readr::write_csv(counts, "data/pilot2_auto_vs_manual_counts.csv")
  readr::write_csv(comparison, "data/pilot2_final_statement_comparison.csv")
  print(counts)
}
