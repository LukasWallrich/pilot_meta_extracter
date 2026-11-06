# Confusion-matrix figure: coders vs automated extraction of funding and COI
# statements in the two pilot rounds (manuscript figure).
#
# Pilot 2 counts are read from data/pilot2_auto_vs_manual_counts.csv
# (written by validation/pilot2_auto_vs_manual.R: Crossref metadata or TEI
# extraction vs coders, 84 articles reporting a pilot).
# Pilot 1 counts are the Crossref-only comparison reported in the manuscript
# (65 articles reporting a pilot); they predate the TEI pipeline and have no
# output file, so they are entered here.
#
# Run from the repo root:  Rscript validation/plot_pilot_confusion.R
# Writes docs/figures/pilot_extraction_confusion.{png,pdf}

suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
})

yn <- function(x) ifelse(x, "Yes", "No")

pilot1 <- tribble(
  ~outcome,  ~manual, ~auto, ~n,
  "funding", TRUE,  TRUE,  38,
  "funding", TRUE,  FALSE, 15,
  "funding", FALSE, TRUE,   2,
  "funding", FALSE, FALSE, 10,
  "coi",     TRUE,  TRUE,   6,
  "coi",     TRUE,  FALSE, 36,
  "coi",     FALSE, TRUE,   0,
  "coi",     FALSE, FALSE, 23
) |> mutate(pilot = "Pilot 1")

pilot2 <- read_csv("data/pilot2_auto_vs_manual_counts.csv", show_col_types = FALSE) |>
  mutate(pilot = "Pilot 2")

d <- bind_rows(pilot1, pilot2) |>
  group_by(pilot, outcome) |>
  mutate(N = sum(n), agree = sum(n[manual == auto]) / N) |>
  ungroup()

n_lab <- d |> distinct(pilot, N)
pilot_labels <- c(
  `Pilot 1` = sprintf("Pilot 1 (n = %d)\nCrossref only", n_lab$N[n_lab$pilot == "Pilot 1"][1]),
  `Pilot 2` = sprintf("Pilot 2 (n = %d)\nCrossref + full text", n_lab$N[n_lab$pilot == "Pilot 2"][1])
)

d <- d |>
  mutate(
    match  = manual == auto,
    pilot  = factor(pilot_labels[pilot], levels = pilot_labels),
    outcome = factor(outcome, levels = c("funding", "coi"),
                     labels = c("Funding\nstatement", "COI\nstatement")),
    manual = factor(yn(manual), levels = c("No", "Yes")),
    auto   = factor(yn(auto), levels = c("Yes", "No"))
  )

ag <- d |> distinct(pilot, outcome, agree)

p <- ggplot(d, aes(auto, manual)) +
  geom_tile(aes(fill = match), width = 0.96, height = 0.94) +
  geom_text(aes(label = n, colour = match), size = 5) +
  geom_text(data = ag, aes(x = 1.5, y = 2.62, label = sprintf("%.0f%% agreement", 100 * agree)),
            inherit.aes = FALSE, size = 3.6, colour = "#222222", fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = "#256abf", `FALSE` = "#e8ecf1"), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1a1a1a"), guide = "none") +
  facet_grid(outcome ~ pilot, switch = "y") +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.7))) +
  labs(x = "Automated: statement found", y = "Coders: statement found") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.placement = "outside",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.spacing = unit(1.2, "lines"),
    axis.title = element_text(colour = "#444444")
  )

dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("docs/figures/pilot_extraction_confusion.png", p, width = 7, height = 5, dpi = 300)
ggsave("docs/figures/pilot_extraction_confusion.pdf", p, width = 7, height = 5)
