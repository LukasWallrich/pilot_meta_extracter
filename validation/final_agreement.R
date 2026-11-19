# Agreement of the final pipeline output with the original manual codes,
# computed offline from data/final_extraction_pilot1-and-pilot2.csv and the two
# coding forms. It reuses run_pipeline.R's codebook mappings and reproduces the
# codebook-validation block of `PIPELINE_RUN=1 Rscript run_pipeline.R` without
# re-running extraction (that run needs the TEI corpora).
# Read-only: prints the agreement table and writes nothing.
# Run from the repository root: Rscript validation/final_agreement.R
source("run_pipeline.R")

final <- readr::read_csv("data/final_extraction_pilot1-and-pilot2.csv",
                         show_col_types = FALSE) |>
  mutate(doi = norm_doi(doi))
stopifnot(!anyDuplicated(final$doi))

# Pilot membership as build_pilot_index() assigns it: Pilot 1 map first.
p1map <- read.csv("data/inputs/pilot1_doi_tei_map.csv", stringsAsFactors = FALSE)
p1dois <- norm_doi(p1map$doi[!is.na(p1map$tei_file) & p1map$tei_file != ""])
final$pilot <- ifelse(final$doi %in% p1dois, "pilot1", "pilot2")

m1 <- read.csv("data/inputs/2025-12-09_Codingform_PilotingAssessment_pilot.csv",
               check.names = TRUE, stringsAsFactors = FALSE)
m2 <- read.csv("data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv",
               check.names = TRUE, stringsAsFactors = FALSE)
man <- bind_rows(
  tibble(doi = norm_doi(m1[["X1.2..Article.DOI"]]), pilot = "pilot1",
         mfund = m1[["X8.1..Does.the.article.include.a.funding.statement."]],
         mcoi  = m1[["X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest."]],
         mprereg = m1[["X2.2...Does.the.article.state.whether.or.not.reported.study.studies.were.preregistered."]]),
  tibble(doi = norm_doi(m2[["X1.2..Article.DOI"]]), pilot = "pilot2",
         mfund = m2[["X8.1..Does.the.article.include.a.funding.statement."]],
         mcoi  = m2[["X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI.."]],
         mprereg = m2[["X2.2...According.to.what.is.stated.in.the.article..were.the.reported.study.ies..preregistered.."]])) |>
  filter(doi != "" & doi != "test") |>
  distinct(doi, .keep_all = TRUE) |>
  mutate(man_fund   = mapply(manual_funding_category, mfund, pilot),
         man_coi    = vapply(mcoi, manual_coi_category, character(1)),
         man_prereg = vapply(mprereg, manual_prereg, logical(1)))

val <- final |> left_join(man |> select(doi, man_fund, man_coi, man_prereg), by = "doi")

agree <- function(pred, truth) {
  keep <- !is.na(pred) & !is.na(truth)
  c(agree = sum(as.character(pred[keep]) == as.character(truth[keep])), n = sum(keep))
}
rows <- list()
for (pl in list("pilot1", "pilot2", c("pilot1", "pilot2"))) {
  d <- val |> filter(pilot %in% pl)
  lab <- if (length(pl) == 2) "pooled" else pl
  coi_present_auto <- d$coi_category != "no statement"
  coi_present_man  <- ifelse(is.na(d$man_coi), NA, d$man_coi != "no statement")
  rows <- c(rows, list(
    c(sample = lab, measure = "funding_category", agree(d$funding_category, d$man_fund)),
    c(sample = lab, measure = "coi_category", agree(d$coi_category, d$man_coi)),
    c(sample = lab, measure = "coi_presence", agree(coi_present_auto, coi_present_man)),
    c(sample = lab, measure = "prereg_present", agree(d$prereg_present, d$man_prereg))))
}
res <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
res$agree <- as.integer(res$agree); res$n <- as.integer(res$n)
res$percent <- round(100 * res$agree / res$n, 1)
print(res, row.names = FALSE)
invisible(res)
