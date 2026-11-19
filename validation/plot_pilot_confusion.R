# Coder vs automated agreement figures for both pilots.
# Pilot 1 retains its original Crossref-only comparison (2026-06-11 combined input);
# Pilot 2 uses the final pipeline outputs. Original manual codes only.
# Writes docs/figures/pilot_extraction_{confusion,categories,mixed}.{png,pdf} and
# data/pilot_category_comparison.csv. No extraction or API calls.
# The code is taken unchanged from the figure chunks of docs/write-up-for-reg-rep.Rmd,
# which now sources this file.
# Run from the repository root: Rscript validation/plot_pilot_confusion.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(readr)
})
dir.create("docs/figures", recursive = TRUE, showWarnings = FALSE)

# load combined data
comb = read_csv("data/inputs/2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv")


# add information to combined file about extracted info on funding and coi
comb = 
  comb %>%
  # remove "test" data
  filter(doi != "test") %>%
  # add rough indicator of whether pilot study was conducted
  mutate(includes_pilot = ifelse(str_detect(X1.3..Is.there.a.statement.in.the.article.that.there.was.a.pilot.study.conducted., "Yes"), "yes", "other")) %>%
  # indicator of whether funding info was extracted
  mutate(
    funding_auto_extracted = if_any(starts_with("funding"), ~ 
                              !is.na(.) & str_trim(.) != "" & !str_to_lower(.) %in% c("na", "n/a")|
                              !(rx_funding_found == FALSE | is.na(rx_funding_found))
    ),
    # different coding for pilot 1 and pilot 2
    funding_manual_extracted = 
      ifelse(coding == "pilot1", 
      !is.na(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) &
      str_trim(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) != "" &
      !str_to_lower(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) %in% c("na", "n/a"),
    # for pilot 2 split into two questions
      !is.na(.data[["X8.1.1.If.the.article.include.a.funding.statement..what.is.the.statement."]]) &
      str_trim(.data[["X8.1.1.If.the.article.include.a.funding.statement..what.is.the.statement."]]) != "" &
      !str_to_lower(.data[["X8.1.1.If.the.article.include.a.funding.statement..what.is.the.statement."]]) %in% c("na", "n/a"))
  ) %>%
  # indicator of whether COI info was extracted
  mutate(
    coi_auto_extracted = if_any(starts_with("coi"), ~ 
                              !is.na(.) & str_trim(.) != "" & !str_to_lower(.) %in% c("na", "n/a")|
                              !(rx_coi_found == FALSE | is.na(rx_coi_found))
    ),
    coi_manual_extracted_content_available = !is.na(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) &
      str_trim(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) != "" &
      !str_to_lower(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) %in% c("na", "n/a")
  ) %>%
  mutate(manual_does_include_coi = 
           ifelse(coding == "pilot1", 
                  X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest.,
                  X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI..)) 


auto_pilot1 = 
  comb %>%
  filter(coding == "pilot1") %>%
  select(where(~ !all(is.na(.))))  %>%
  # remove "test" data
  filter(doi != "test") %>%
  filter(includes_pilot == "yes")

source("validation/pilot2_auto_vs_manual.R")
pilot2_comparison <- pilot2_final_statement_comparison()


# Prepare data for plotting
make_plot_input <- function(dat, pilot, source) {

  pilot_label <- paste0(
    pilot, " (n = ", nrow(dat), ")\n", source
  )

  bind_rows(

    # Funding statement
    dat %>%
      transmute(
        pilot_lab = pilot_label,
        statement = "Funding statement",
        coder_found = funding_manual_extracted,
        automated_found = funding_auto_extracted
      ),

    # COI statement
    dat %>%
      transmute(
        pilot_lab = pilot_label,
        statement = "COI statement",
        coder_found = str_starts(manual_does_include_coi, "Yes"),
        automated_found = coi_auto_extracted
      )
  )
}


# Combine both pilots
plot_input <- bind_rows(
  make_plot_input(auto_pilot1, "Pilot 1", "Crossref only"),
  pilot2_comparison %>%
    transmute(
      pilot_lab = "Pilot 2\nFinal pipeline",
      statement = if_else(outcome == "funding", "Funding statement", "COI statement"),
      coder_found = manual,
      automated_found = auto
    )
) %>%
  filter(!is.na(coder_found), !is.na(automated_found)) %>%
  mutate(
    statement = factor(
      statement,
      levels = c("COI statement", "Funding statement")
    ),
    pilot_lab = factor(pilot_lab, levels = unique(pilot_lab)),
    agreement_cell = coder_found == automated_found,

    # Coordinates: Yes on top/left, No on bottom/right
    x = if_else(automated_found, 1, 2),
    y = if_else(coder_found, 2, 1)
  )


# Count observations in each cell
plot_counts <- plot_input %>%
  count(pilot_lab, statement, x, y, name = "n") %>%
  complete(
    pilot_lab,
    statement,
    x = 1:2,
    y = 1:2,
    fill = list(n = 0)
  ) %>%
  mutate(
    agreement_cell = (x == 1 & y == 2) | (x == 2 & y == 1)
  ) %>%
  mutate(
    statement = factor(
      statement,
      levels = c("Funding statement", "COI statement"))
  )


# Calculate percentage agreement for each panel
plot_agreement <- plot_input %>%
  group_by(pilot_lab, statement) %>%
  summarise(
    agreement = mean(agreement_cell) * 100,
    N = n(),
    .groups = "drop"
  )


# Create figure
agreement_plot = 
  ggplot(plot_counts, aes(x = x, y = y, fill = agreement_cell)) +

  # Heatmap cells
  geom_tile(
  width = 0.96,
  height = 0.96,
  color = NA
) +

  # Numbers inside cells
  geom_text(
    aes(label = n, color = agreement_cell),
    size = 5
  ) +

  # Agreement percentages above each panel
  geom_text(
    data = plot_agreement,
    aes(
      x = 1.5,
      y = 2.7,
      label = paste0(round(agreement), "% agreement\n(n = ", N, ")")
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.5
  ) +

  # Arrange panels
  facet_grid(
    rows = vars(statement),
    cols = vars(pilot_lab),
    switch = "y",
    labeller = labeller(
    statement = c(
      "Funding statement" = "Funding\nstatement",
      "COI statement" = "COI\nstatement"
    ))
  ) +

  # Axis labels
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Yes", "No"),
    limits = c(0.5, 2.5),
    expand = c(0, 0)
  ) +

  scale_y_continuous(
    breaks = c(1, 2),
    labels = c("No", "Yes"),
    limits = c(0.5, 2.95),
    expand = c(0, 0)
  ) +

  # Colours
  scale_fill_manual(
    values = c(
      "TRUE" = "#2D6DB5",
      "FALSE" = "#E8EDF2"
    ),
    guide = "none"
  ) +

  scale_color_manual(
    values = c(
      "TRUE" = "white",
      "FALSE" = "#333333"
    ),
    guide = "none"
  ) +

  labs(
    x = "Automated: statement found",
    y = "Coders: statement found"
  ) +

  coord_fixed() +

  theme_minimal(base_size = 11) +

  theme(
    panel.grid = element_blank(),

    strip.background = element_blank(),
    strip.placement = "outside",

    strip.text.x = element_text(
      face = "bold",
      size = 10
    ),

    strip.text.y.left = element_text(
      angle = 0,
      face = "bold",
      hjust = 0.5,
      vjust = 0.4
    ),

    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),

    plot.margin = margin(5, 5, 5, 5),
    panel.spacing.x = grid::unit(0.6, "lines")
  )


ggsave(
  "docs/figures/pilot_extraction_confusion.png",
  plot = agreement_plot,
  width = 7,
  height = 6.2,
  dpi = 300
)
ggsave(
  "docs/figures/pilot_extraction_confusion.pdf",
  plot = agreement_plot,
  width = 7,
  height = 6.2
)


# Three-category alternative (also writes data/pilot_category_comparison.csv)
source("validation/plot_pilot_categories.R", local = TRUE)

# Mixed binary/categorical alternative
pilot1_binary_panel <- function(variable) {
  statement_label <- if (variable == "funding") "Funding statement" else "COI statement"
  d <- plot_counts |>
    filter(startsWith(as.character(pilot_lab), "Pilot 1"),
           as.character(statement) == statement_label)
  stopifnot(sum(d$n) == 65)
  agreement <- sum(d$n[d$agreement_cell]) / sum(d$n)
  ggplot(d, aes(x, y, fill = agreement_cell)) +
    geom_tile(width = 0.96, height = 0.96) +
    geom_text(aes(label = n, colour = agreement_cell), size = 5) +
    scale_fill_manual(values = c(`TRUE` = "#2D6DB5", `FALSE` = "#E8EDF2"), guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#333333"), guide = "none") +
    scale_x_continuous(breaks = 1:2, labels = c("Yes", "No"),
                       limits = c(0.5, 2.5), expand = c(0, 0)) +
    scale_y_continuous(breaks = 1:2, labels = c("No", "Yes"),
                       limits = c(0.5, 2.5), expand = c(0, 0)) +
    coord_fixed() +
    labs(title = paste("Pilot 1", if (variable == "funding") "funding" else "COI", "(presence)"),
         subtitle = sprintf("Crossref only; n = 65; %.1f%% agreement", 100 * agreement),
         x = "Automated: statement found", y = "Original manual: statement found") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text = element_text(size = 9),
          axis.title = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10),
          plot.margin = margin(8, 8, 8, 8))
}

mixed_plot <- patchwork::wrap_plots(list(
  pilot1_binary_panel("funding"),
  category_panel("Pilot 2", "funding") + labs(title = "Pilot 2 funding (categories)"),
  pilot1_binary_panel("coi"),
  category_panel("Pilot 2", "coi") + labs(title = "Pilot 2 COI (categories)")
), ncol = 2) + patchwork::plot_annotation(
  caption = paste(
    "Pilot 1: original binary comparison. Pilot 2: final three-category pipeline. Original manual codes.",
    "Agreement percentages describe different tasks and are not directly comparable across pilots.",
    sep = "\n"
  ), theme = theme(plot.caption = element_text(size = 9, hjust = 0))
)

ggsave("docs/figures/pilot_extraction_mixed.png", mixed_plot, width = 8, height = 7.3, dpi = 300)
ggsave("docs/figures/pilot_extraction_mixed.pdf", mixed_plot, width = 8, height = 7.3)
