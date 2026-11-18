# Three-category alternative: Pilot 1 original Crossref metadata, Pilot 2 final
# pipeline. Original manual codes only; no adjudicated reference labels.
# Run from the repository root: Rscript validation/plot_pilot_categories.R
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

pilot1_metadata_categories <- function() {
  # Classify retained metadata only: do not run extraction or read full texts.
  source("run_pipeline.R", local = TRUE)
  original <- read.csv(
    "data/inputs/2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv",
    stringsAsFactors = FALSE, check.names = TRUE
  ) |>
    filter(coding == "pilot1", doi != "test",
           startsWith(X1.3..Is.there.a.statement.in.the.article.that.there.was.a.pilot.study.conducted., "Yes")) |>
    mutate(doi = norm_doi(doi))
  stopifnot(!anyDuplicated(original$doi))
  present <- function(x) !is.na(x) & nzchar(trimws(x)) &
    !tolower(trimws(x)) %in% c("na", "n/a")
  # Structured funding fields record funders, not explicit no-funding denials.
  funding_found <- original |>
    transmute(found = if_any(c(funding_funders_crossref,
                               funding_funder_ids_crossref,
                               funding_award_ids_crossref), present)) |>
    pull(found)
  coi_text <- original$coi_crossref
  coi_text[!present(coi_text)] <- NA_character_
  bind_rows(
    tibble(doi = original$doi, outcome = "funding",
           manual_category = vapply(original$X8.1..Does.the.article.include.a.funding.statement.,
                                    manual_funding_category, character(1), pilot = "pilot1"),
           automated_category = if_else(funding_found, "funded", "no statement")),
    tibble(doi = original$doi, outcome = "coi",
           manual_category = vapply(original$X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest.,
                                    manual_coi_category, character(1)),
           automated_category = vapply(coi_text, classify_coi, character(1)))
  ) |>
    filter(!is.na(manual_category), !is.na(automated_category)) |>
    mutate(pilot = "Pilot 1", source = "Crossref only")
}

source("validation/pilot2_auto_vs_manual.R")
comparison <- bind_rows(
  pilot1_metadata_categories(),
  pilot2_final_statement_comparison() |>
    select(doi, outcome, manual_category, automated_category) |>
    mutate(pilot = "Pilot 2", source = "Final pipeline")
)

category_panel <- function(pilot_label, variable) {
  d <- comparison |> filter(pilot == pilot_label, outcome == variable)
  levels <- if (variable == "funding") c("funded", "no funding", "no statement") else
    c("conflict declared", "no conflict declared", "no statement")
  labels <- if (variable == "funding") c("Funded", "No\nfunding", "No\nstatement") else
    c("Conflict\ndeclared", "No conflict\ndeclared", "No\nstatement")
  counts <- d |>
    transmute(x = match(automated_category, levels),
              y = 4L - match(manual_category, levels)) |>
    count(x, y) |>
    complete(x = 1:3, y = 1:3, fill = list(n = 0)) |>
    mutate(agreement = x + y == 4L)
  stopifnot(!anyNA(counts), sum(counts$n) == nrow(d))
  agreement <- mean(d$manual_category == d$automated_category)
  ggplot(counts, aes(x, y, fill = agreement)) +
    geom_tile(width = 0.96, height = 0.96) +
    geom_text(aes(label = n, colour = agreement), size = 5) +
    scale_fill_manual(values = c(`TRUE` = "#2D6DB5", `FALSE` = "#E8EDF2"), guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#333333"), guide = "none") +
    scale_x_continuous(breaks = 1:3, labels = labels, expand = c(0, 0), limits = c(0.5, 3.5)) +
    scale_y_continuous(breaks = 1:3, labels = rev(labels), expand = c(0, 0), limits = c(0.5, 3.5)) +
    coord_fixed() +
    labs(title = paste(pilot_label, if (variable == "funding") "funding" else "COI"),
         subtitle = sprintf("%s; n = %d; %.1f%% agreement", unique(d$source), nrow(d), 100 * agreement),
         x = "Automated classification", y = "Original manual classification") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text = element_text(size = 9),
          axis.title = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10),
          plot.margin = margin(8, 8, 8, 8))
}

p <- wrap_plots(list(category_panel("Pilot 1", "funding"),
                     category_panel("Pilot 2", "funding"),
                     category_panel("Pilot 1", "coi"),
                     category_panel("Pilot 2", "coi")), ncol = 2) +
  plot_annotation(caption = paste(
    "Original manual codes; Pilot 1 categories reconstructed from original Crossref metadata.",
    "Blank Pilot 1 funding answers are uncoded. Pilot 2 uses final metadata/XML/PDF outputs.",
    sep = "\n"
  ), theme = theme(plot.caption = element_text(size = 9, hjust = 0)))

dir.create("docs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("docs/figures/pilot_extraction_categories.png", p, width = 8, height = 7.3, dpi = 300)
ggsave("docs/figures/pilot_extraction_categories.pdf", p, width = 8, height = 7.3)
write_csv(comparison, "data/pilot_category_comparison.csv")
comparison |>
  group_by(pilot, outcome, source) |>
  summarise(n = n(), agreements = sum(manual_category == automated_category),
            agreement = agreements / n, .groups = "drop") |>
  print()
