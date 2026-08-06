# =====================================================================
# Validate the custom regex/XPath TEI extractor ("rx") against the
# metacheck baseline for FUNDING and COI detection, across pilot 1 and
# pilot 2 (Phase C1).
#
# Does rx still outperform metacheck? Previously only tested on pilot 2.
#
# Predictions come from:
#   data/stages/tei_coi_funding_extracted_pilot1-and-pilot2.csv
#   (rx_funding_found, rx_coi_found, mc_funding_found, mc_coi_found)
# Manual ground truth:
#   pilot 1: data/inputs/2025-12-09_Codingform_PilotingAssessment_pilot.csv
#   pilot 2: data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv
#
# Output:
#   data/rx_vs_metacheck_metrics.csv  (tidy metrics)
#   console summary + verdict + pilot-1 FP/FN listings
#
# NOTE: this script does NOT modify the extractor scripts; it only
# consumes their output.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# ---- Settings -------------------------------------------------------
pred_csv   <- "data/stages/tei_coi_funding_extracted_pilot1-and-pilot2.csv"
pilot1_csv <- "data/inputs/2025-12-09_Codingform_PilotingAssessment_pilot.csv"
pilot2_csv <- "data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"

# ---- Helpers --------------------------------------------------------
clean_doi <- function(x) {
  x |>
    sub("^https?://doi\\.org/", "", x = _) |>
    sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
    trimws()
}

is_nonblank <- function(x) {
  !is.na(x) & str_trim(x) != "" & !str_to_lower(str_trim(x)) %in% c("na", "n/a")
}

# Manual positive = answer contains "Yes" (case-insensitive)
is_yes <- function(x) is_nonblank(x) & str_detect(x, regex("yes", ignore_case = TRUE))

# ---- Load predictions ----------------------------------------------
pred <- read.csv(pred_csv, stringsAsFactors = FALSE)

# Coerce found columns to logical; a failed/missing extraction = "found nothing" = FALSE.
# (rx never returns NA; metacheck returns NA when read_grobid failed on that TEI.)
to_logical_falseNA <- function(x) {
  v <- as.logical(x)
  v[is.na(v)] <- FALSE
  v
}
pred <- pred |>
  mutate(
    rx_funding_found = to_logical_falseNA(rx_funding_found),
    rx_coi_found     = to_logical_falseNA(rx_coi_found),
    mc_funding_found = to_logical_falseNA(mc_funding_found),
    mc_coi_found     = to_logical_falseNA(mc_coi_found)
  )

# Report how many metacheck predictions were NA (parse failures) before coercion.
pred_raw <- read.csv(pred_csv, stringsAsFactors = FALSE)
na_report <- pred_raw |>
  group_by(coding) |>
  summarise(
    mc_funding_na = sum(is.na(as.logical(mc_funding_found))),
    mc_coi_na     = sum(is.na(as.logical(mc_coi_found))),
    rx_funding_na = sum(is.na(as.logical(rx_funding_found))),
    rx_coi_na     = sum(is.na(as.logical(rx_coi_found))),
    .groups = "drop"
  )

# ---- Build ground truth --------------------------------------------
# Returns a long tibble: doi, pilot, outcome, manual_pos (logical), manual_text
build_gt <- function(csv_path, pilot_label) {
  m <- read.csv(csv_path, stringsAsFactors = FALSE)
  m <- m |>
    mutate(doi = clean_doi(.data[["X1.2..Article.DOI"]])) |>
    filter(doi != "" & doi != "test") |>
    filter(!duplicated(doi))

  funding_col <- "X8.1..Does.the.article.include.a.funding.statement."
  coi_col <- if (pilot_label == "pilot1") {
    "X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest."
  } else {
    "X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI.."
  }
  includes_pilot_col <- "X1.3..Is.there.a.statement.in.the.article.that.there.was.a.pilot.study.conducted."

  # ---- FUNDING ground truth ----
  # pilot2: explicit Yes/No answers -> denominator = non-blank answer;
  #         positive = contains "Yes".
  # pilot1: the funding question was answered with the free-text funding
  #         statement (no Yes/No), so "contains Yes" is impossible (0 cases).
  #         Denominator = papers where coders reported a pilot study
  #         (X1.3 == "Yes"), matching the existing write-up Rmd, which filters
  #         to includes_pilot == "yes" and defines manual funding by non-blank
  #         text.  positive = non-blank funding text; negative = blank.
  if (pilot_label == "pilot1") {
    in_pilot <- str_detect(m[[includes_pilot_col]], regex("yes", ignore_case = TRUE))
    in_pilot[is.na(in_pilot)] <- FALSE
    fund_gt <- tibble(
      doi         = m$doi[in_pilot],
      pilot       = pilot_label,
      outcome     = "funding",
      manual_pos  = is_nonblank(m[[funding_col]][in_pilot]),
      manual_text = m[[funding_col]][in_pilot]
    )
  } else {
    keep <- is_nonblank(m[[funding_col]])
    fund_gt <- tibble(
      doi         = m$doi[keep],
      pilot       = pilot_label,
      outcome     = "funding",
      manual_pos  = is_yes(m[[funding_col]][keep]),
      manual_text = m[[funding_col]][keep]
    )
  }

  # ---- COI ground truth (uniform: explicit Yes/No in both pilots) ----
  keep_coi <- is_nonblank(m[[coi_col]])
  coi_gt <- tibble(
    doi         = m$doi[keep_coi],
    pilot       = pilot_label,
    outcome     = "coi",
    manual_pos  = is_yes(m[[coi_col]][keep_coi]),
    manual_text = m[[coi_col]][keep_coi]
  )

  bind_rows(fund_gt, coi_gt)
}

gt <- bind_rows(
  build_gt(pilot1_csv, "pilot1"),
  build_gt(pilot2_csv, "pilot2")
)

# ---- Join GT to predictions ----------------------------------------
# Only papers that have BOTH a manual answer AND a TEI-based prediction
# are scorable. Use the extracted file's `coding` to confirm pilot.
pred_long <- pred |>
  transmute(
    doi, coding,
    funding_rx = rx_funding_found, funding_mc = mc_funding_found,
    coi_rx = rx_coi_found,         coi_mc = mc_coi_found,
    rx_funding_text, mc_funding_text, rx_coi_text, mc_coi_text
  )

scored <- gt |>
  inner_join(pred, by = "doi") |>
  mutate(
    rx_pred = case_when(outcome == "funding" ~ rx_funding_found,
                        outcome == "coi"     ~ rx_coi_found),
    mc_pred = case_when(outcome == "funding" ~ mc_funding_found,
                        outcome == "coi"     ~ mc_coi_found),
    rx_text = case_when(outcome == "funding" ~ rx_funding_text,
                        outcome == "coi"     ~ rx_coi_text),
    mc_text = case_when(outcome == "funding" ~ mc_funding_text,
                        outcome == "coi"     ~ mc_coi_text)
  )

# Sanity: confirm coding in predictions matches the GT pilot
mismatch <- scored |> filter(coding != pilot)
if (nrow(mismatch) > 0) {
  warning(nrow(mismatch), " rows where extracted `coding` != GT pilot; check DOI overlap between pilots.")
}

# ---- Metrics --------------------------------------------------------
calc_metrics <- function(predicted, actual) {
  tp <- sum(predicted & actual)
  fp <- sum(predicted & !actual)
  fn <- sum(!predicted & actual)
  tn <- sum(!predicted & !actual)
  n  <- tp + fp + fn + tn
  accuracy  <- (tp + tn) / n
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall    <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0)
    2 * precision * recall / (precision + recall) else NA_real_
  tibble(n = n, tp = tp, fp = fp, fn = fn, tn = tn,
         accuracy = accuracy, precision = precision, recall = recall, f1 = f1)
}

build_row <- function(df, pilot_lab, outcome_lab, method_lab, pred_col) {
  m <- calc_metrics(df[[pred_col]], df$manual_pos)
  bind_cols(tibble(pilot = pilot_lab, outcome = outcome_lab, method = method_lab), m)
}

metrics <- list()
for (oc in c("funding", "coi")) {
  for (pl in c("pilot1", "pilot2", "pooled")) {
    df <- scored |> filter(outcome == oc)
    if (pl != "pooled") df <- df |> filter(pilot == pl)
    if (nrow(df) == 0) next
    metrics[[paste(oc, pl, "rx")]] <- build_row(df, pl, oc, "rx", "rx_pred")
    metrics[[paste(oc, pl, "metacheck")]] <- build_row(df, pl, oc, "metacheck", "mc_pred")
  }
}
metrics_df <- bind_rows(metrics) |>
  arrange(outcome, factor(pilot, levels = c("pilot1", "pilot2", "pooled")), method)

write.csv(metrics_df, "data/rx_vs_metacheck_metrics.csv", row.names = FALSE)

# =====================================================================
# Reporting
# =====================================================================
fmt <- function(x) ifelse(is.na(x), "  NA ", sprintf("%.3f", x))

cat("\n=====================================================================\n")
cat("VALIDATION: rx (regex/XPath TEI) vs metacheck — FUNDING & COI\n")
cat("=====================================================================\n")

cat("\n--- Scored set sizes (papers with manual GT AND a TEI prediction) ---\n")
set_sizes <- scored |>
  group_by(outcome, pilot) |>
  summarise(n = n(), positives = sum(manual_pos), negatives = sum(!manual_pos), .groups = "drop")
print(as.data.frame(set_sizes), row.names = FALSE)

cat("\n--- Metacheck NA (parse-failure) predictions, by pilot (coerced to FALSE) ---\n")
print(as.data.frame(na_report), row.names = FALSE)

for (oc in c("funding", "coi")) {
  cat(sprintf("\n=================  %s  =================\n", toupper(oc)))
  cat(sprintf("%-7s %-10s %4s %4s %4s %4s %4s   %-6s %-6s %-6s %-6s\n",
              "pilot", "method", "n", "TP", "FP", "FN", "TN",
              "acc", "prec", "rec", "F1"))
  cat(paste(rep("-", 78), collapse = ""), "\n")
  sub <- metrics_df |> filter(outcome == oc)
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    cat(sprintf("%-7s %-10s %4d %4d %4d %4d %4d   %-6s %-6s %-6s %-6s\n",
                r$pilot, r$method, r$n, r$tp, r$fp, r$fn, r$tn,
                fmt(r$accuracy), fmt(r$precision), fmt(r$recall), fmt(r$f1)))
  }
}

# ---- Side-by-side rx vs metacheck + verdict ------------------------
cat("\n=====================================================================\n")
cat("SIDE-BY-SIDE (rx vs metacheck) + VERDICT\n")
cat("=====================================================================\n")
wide <- metrics_df |>
  select(pilot, outcome, method, accuracy, precision, recall, f1) |>
  pivot_wider(names_from = method, values_from = c(accuracy, precision, recall, f1))

for (oc in c("funding", "coi")) {
  cat(sprintf("\n--- %s ---\n", toupper(oc)))
  for (pl in c("pilot1", "pilot2", "pooled")) {
    w <- wide |> filter(outcome == oc, pilot == pl)
    if (nrow(w) == 0) next
    cat(sprintf("  [%s]  F1: rx=%s vs mc=%s   (rx-mc = %+.3f)\n",
                pl, fmt(w$f1_rx), fmt(w$f1_metacheck),
                (w$f1_rx %||% NA) - (w$f1_metacheck %||% NA)))
    cat(sprintf("        acc: rx=%s mc=%s | prec: rx=%s mc=%s | rec: rx=%s mc=%s\n",
                fmt(w$accuracy_rx), fmt(w$accuracy_metacheck),
                fmt(w$precision_rx), fmt(w$precision_metacheck),
                fmt(w$recall_rx), fmt(w$recall_metacheck)))
    verdict <- if (is.na(w$f1_rx) || is.na(w$f1_metacheck)) "indeterminate (NA F1)" else
      if (w$f1_rx > w$f1_metacheck) "rx WINS" else
        if (w$f1_rx < w$f1_metacheck) "metacheck wins" else "tie"
    cat(sprintf("        VERDICT (by F1): %s\n", verdict))
  }
}

# ---- Pilot-1 rx false positives / false negatives ------------------
cat("\n=====================================================================\n")
cat("PILOT 1 — rx FALSE POSITIVES / FALSE NEGATIVES (new data, for eyeballing)\n")
cat("=====================================================================\n")
slice_txt <- function(x, n = 180) {
  x <- ifelse(is.na(x), "<none>", x)
  substr(x, 1, n)
}
for (oc in c("funding", "coi")) {
  d <- scored |> filter(pilot == "pilot1", outcome == oc)
  fps <- d |> filter(rx_pred & !manual_pos)
  fns <- d |> filter(!rx_pred & manual_pos)
  cat(sprintf("\n----- %s : rx FALSE POSITIVES (%d) -----\n", toupper(oc), nrow(fps)))
  for (i in seq_len(nrow(fps))) {
    cat(sprintf("  DOI: %s\n", fps$doi[i]))
    cat(sprintf("    manual: %s\n", slice_txt(fps$manual_text[i], 120)))
    cat(sprintf("    rx extracted: %s\n\n", slice_txt(fps$rx_text[i])))
  }
  cat(sprintf("----- %s : rx FALSE NEGATIVES (%d) -----\n", toupper(oc), nrow(fns)))
  for (i in seq_len(nrow(fns))) {
    cat(sprintf("  DOI: %s\n", fns$doi[i]))
    cat(sprintf("    manual: %s\n", slice_txt(fns$manual_text[i], 200)))
    cat(sprintf("    mc found this? %s\n\n", fns$mc_pred[i]))
  }
}

# ---- Integrity checks ----------------------------------------------
cat("\n=====================================================================\n")
cat("INTEGRITY CHECKS\n")
cat("=====================================================================\n")
for (oc in c("funding", "coi")) {
  n1 <- nrow(scored |> filter(outcome == oc, pilot == "pilot1"))
  n2 <- nrow(scored |> filter(outcome == oc, pilot == "pilot2"))
  npool <- nrow(scored |> filter(outcome == oc))
  cat(sprintf("  %s: pilot1 n=%d + pilot2 n=%d = %d ; pooled n=%d  -> %s\n",
              oc, n1, n2, n1 + n2, npool,
              ifelse(n1 + n2 == npool, "OK", "MISMATCH")))
}
dup_doi <- scored |> count(outcome, doi) |> filter(n > 1)
cat(sprintf("  DOIs double-counted within an outcome: %d  -> %s\n",
            nrow(dup_doi), ifelse(nrow(dup_doi) == 0, "OK (none)", "PROBLEM")))

cat("\nMetrics written to data/rx_vs_metacheck_metrics.csv\n")
