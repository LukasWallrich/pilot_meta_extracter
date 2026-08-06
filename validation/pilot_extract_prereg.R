# =============================================================================
# validation/pilot_extract_prereg.R
# -----------------------------------------------------------------------------
# PILOT-1 + PILOT-2 batch reproduction of the preregistration extraction, plus
# manual-coding validation. Uses the CANONICAL classify_paper() (which adds
# is_registered_report and folds it into registration_any) and the shared
# resolve_osf_caches() from the library. Writes:
#   data/prereg_extracted_pilot1-and-pilot2.csv
# Run from the project root:  Rscript validation/pilot_extract_prereg.R
# =============================================================================
source("R/3_extract_prereg.R")   # parse_tei, classify_paper, resolve_osf_caches, keyword_scan, ...

# ---- Settings ----------------------------------------------------------------
pilot1_map_csv <- "data/pilot1_doi_tei_map.csv"
pilot1_manual_csv <- "data/2025-12-09_Codingform_PilotingAssessment_pilot.csv"
pilot2_manual_csv <- "data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"
tei_dir2 <- "../pilot2_pdf_conversion/tei"
output_csv <- "data/prereg_extracted_pilot1-and-pilot2.csv"
PILOT1_DROP <- "10.1016/j.cognition.2022.105212"  # NA TEI row

norm_doi <- function(x) {
  x |>
    sub("^https?://doi\\.org/", "", x = _) |>
    sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
    trimws() |>
    tolower()
}

# =============================================================================
# 1. Build file lists (doi, tei_file, pilot) for both pilots
# =============================================================================

# ---- Pilot 1: from the prebuilt DOI->TEI map --------------------------------
p1map <- read.csv(pilot1_map_csv, stringsAsFactors = FALSE)
p1map <- p1map[!is.na(p1map$tei_file) & p1map$tei_file != "" &
                 p1map$doi != PILOT1_DROP, ]
p1_files <- data.frame(doi = p1map$doi, tei_file = p1map$tei_file,
                       pilot = "pilot1", stringsAsFactors = FALSE)
message(nrow(p1_files), " pilot1 TEIs")

# ---- Pilot 2: match DOI-encoded TEI filenames to manual DOIs -----------------
manual2 <- read.csv(pilot2_manual_csv, stringsAsFactors = FALSE, check.names = TRUE)
pilot2_dois <- manual2[["X1.2..Article.DOI"]] |> norm_doi() |> unique()
pilot2_dois <- pilot2_dois[pilot2_dois != "" & pilot2_dois != "test"]

all_tei <- list.files(tei_dir2, pattern = "\\.tei\\.xml$", full.names = TRUE)
extract_doi_from_filename <- function(fname) {
  base <- basename(fname)
  doi_part <- sub("_-_.*", "", base)
  sub("^(10\\.[0-9]{4,})-", "\\1/", doi_part)
}
tei_dois <- tolower(sapply(all_tei, extract_doi_from_filename, USE.NAMES = FALSE))
midx <- match(pilot2_dois, tei_dois)
p2_files <- data.frame(doi = pilot2_dois[!is.na(midx)],
                       tei_file = all_tei[midx[!is.na(midx)]],
                       pilot = "pilot2", stringsAsFactors = FALSE)
message(nrow(p2_files), " pilot2 TEIs matched (",
        sum(is.na(midx)), " pilot2 DOIs unmatched)")

files <- bind_rows(p1_files, p2_files)
files$doi_norm <- norm_doi(files$doi)

message("\nParsing ", nrow(files), " TEI files...")
parsed <- lapply(files$tei_file, parse_tei)

# =============================================================================
# 3. OSF GUID-type + registration-template resolution (shared library function)
# =============================================================================
osf_bare_all <- function(p) vapply(p$urls, osf_bare_code, character(1))
all_codes <- unique(na.omit(unlist(lapply(parsed, osf_bare_all))))
all_codes <- all_codes[all_codes != ""]
message("\nUnique bare osf.io codes needing API resolution: ", length(all_codes))

osf <- resolve_osf_caches(parsed, use_api = TRUE)
osf_type_of     <- osf$type
osf_template_of <- osf$template

message("\nOSF type distribution (resolved bare codes):")
print(table(osf_type_of[all_codes], useNA = "ifany"))

reg_codes <- names(osf_type_of)[osf_type_of == "registrations"]
reg_codes <- intersect(reg_codes, all_codes)
message("\nOSF registration TEMPLATE distribution:")
print(table(osf_template_of[reg_codes], useNA = "ifany"))

# =============================================================================
# 4. Per-paper classification (canonical classify_paper; osf caches passed in)
# =============================================================================
res <- bind_rows(lapply(parsed, classify_paper,
                        osf_type_of = osf_type_of, osf_template_of = osf_template_of))
out <- bind_cols(files[, c("doi", "pilot", "doi_norm")], res)

# =============================================================================
# 7. Join manual ground-truth coding
# =============================================================================
p1m <- read.csv(pilot1_manual_csv, stringsAsFactors = FALSE, check.names = TRUE)
p2m <- read.csv(pilot2_manual_csv, stringsAsFactors = FALSE, check.names = TRUE)

p1_coding <- data.frame(
  doi_norm = norm_doi(p1m[["X1.2..Article.DOI"]]),
  x21_rr   = trimws(p1m[["X2.1...Is.the.article.a.Registered.Report."]]),
  x22      = trimws(p1m[["X2.2...Does.the.article.state.whether.or.not.reported.study.studies.were.preregistered."]]),
  stringsAsFactors = FALSE)
p2_coding <- data.frame(
  doi_norm = norm_doi(p2m[["X1.2..Article.DOI"]]),
  x21_rr   = trimws(p2m[["X2.1...Is.the.article.a.Registered.Report."]]),
  x22      = trimws(p2m[["X2.2...According.to.what.is.stated.in.the.article..were.the.reported.study.ies..preregistered.."]]),
  stringsAsFactors = FALSE)
coding <- bind_rows(p1_coding, p2_coding)
coding <- coding[!duplicated(coding$doi_norm), ]

out <- left_join(out, coding, by = "doi_norm")

# Manual binary label from X2.2 free text
manual_label <- function(x22) {
  t <- trimws(x22 %||% "")
  if (t == "" || tolower(t) == "test") return(NA_character_)
  if (grepl("clinical trial|clinicaltrials|trials? registr|\\bACTRN", t, ignore.case = TRUE) &&
      !grepl("^Yes", t)) return("trial")
  if (grepl("^Yes", t)) return("positive")
  if (grepl("\\bno\\b|not\\s+pre-?regist|no preregistration", t, ignore.case = TRUE)) return("negative")
  NA_character_   # e.g. free-text RR notes
}
out$manual_x22_label <- vapply(out$x22, manual_label, character(1))
out$manual_rr <- !is.na(out$x21_rr) & out$x21_rr == "Yes"
out$manual_prereg <- dplyr::case_when(
  out$manual_x22_label == "positive" ~ TRUE,
  out$manual_x22_label == "negative" ~ FALSE,
  TRUE ~ NA)
out$manual_reg_any <- dplyr::case_when(
  out$manual_x22_label %in% c("positive", "trial") ~ TRUE,
  out$manual_rr ~ TRUE,
  out$manual_x22_label == "negative" ~ FALSE,
  TRUE ~ NA)

out$coding <- out$manual_x22_label   # `coding` = manual ground-truth label

# =============================================================================
# 5. Write output (is_registered_report added vs. the pre-refactor stage file)
# =============================================================================
out_cols <- c("doi", "pilot", "coding", "prereg_found", "registration_any",
              "is_registered_report",
              "prereg_platform", "osf_reg_template", "prereg_urls", "prereg_statement",
              "is_trial_registry", "has_negated_prereg",
              "x21_rr", "x22", "manual_prereg", "manual_reg_any", "manual_rr")
write.csv(out[, out_cols], output_csv, row.names = FALSE)
message("\nResults written to ", output_csv)

# =============================================================================
# 9. Summaries + validation
# =============================================================================
cat("\n================ PLATFORM DISTRIBUTION ================\n")
plat <- unlist(str_split(out$prereg_platform[!is.na(out$prereg_platform)], " \\|\\| "))
print(sort(table(plat), decreasing = TRUE))

cat("\n================ prereg_found counts per pilot ================\n")
print(out |> group_by(pilot) |>
        summarise(n = n(),
                  prereg_found = sum(prereg_found),
                  registration_any = sum(registration_any),
                  trial_registry = sum(is_trial_registry)))

metrics <- function(pred, truth) {
  keep <- !is.na(truth) & !is.na(pred)
  pred <- pred[keep]; truth <- truth[keep]
  tp <- sum(pred & truth); fp <- sum(pred & !truth)
  fn <- sum(!pred & truth); tn <- sum(!pred & !truth)
  prec <- if ((tp+fp) > 0) tp/(tp+fp) else NA
  rec  <- if ((tp+fn) > 0) tp/(tp+fn) else NA
  f1   <- if (!is.na(prec) && !is.na(rec) && (prec+rec) > 0) 2*prec*rec/(prec+rec) else NA
  acc  <- (tp+tn)/(tp+fp+fn+tn)
  data.frame(n = tp+fp+fn+tn, TP=tp, FP=fp, FN=fn, TN=tn,
             accuracy=round(acc,3), precision=round(prec,3),
             recall=round(rec,3), F1=round(f1,3))
}

cat("\n================ VALIDATION: prereg_found vs manual X2.2 binary ================\n")
cat("\n-- Pooled --\n");  print(metrics(out$prereg_found, out$manual_prereg))
cat("\n-- Pilot 1 --\n"); print(metrics(out$prereg_found[out$pilot=="pilot1"], out$manual_prereg[out$pilot=="pilot1"]))
cat("\n-- Pilot 2 --\n"); print(metrics(out$prereg_found[out$pilot=="pilot2"], out$manual_prereg[out$pilot=="pilot2"]))

cat("\nConfusion matrix (pooled, prereg_found rows x manual cols):\n")
vv <- out[!is.na(out$manual_prereg), ]
print(table(prereg_found = vv$prereg_found, manual = vv$manual_prereg))

cat("\n================ VALIDATION: registration_any vs manual (incl. RR + trial) ================\n")
cat("\n-- Pooled --\n"); print(metrics(out$registration_any, out$manual_reg_any))
cat("\n-- Pilot 1 --\n"); print(metrics(out$registration_any[out$pilot=="pilot1"], out$manual_reg_any[out$pilot=="pilot1"]))
cat("\n-- Pilot 2 --\n"); print(metrics(out$registration_any[out$pilot=="pilot2"], out$manual_reg_any[out$pilot=="pilot2"]))

cat("\n================ FALSE POSITIVES (prereg_found=TRUE, manual=FALSE) ================\n")
fp <- out[!is.na(out$manual_prereg) & out$prereg_found & !out$manual_prereg, ]
for (i in seq_len(nrow(fp))) {
  cat(sprintf("\n[%s] %s\n  platform: %s\n  osf_reg_template: %s\n  urls: %s\n  stmt: %s\n  manual X2.2: %s\n",
              fp$pilot[i], fp$doi[i], fp$prereg_platform[i], fp$osf_reg_template[i] %||% "NA",
              substr(fp$prereg_urls[i] %||% "", 1, 200),
              substr(fp$prereg_statement[i] %||% "", 1, 200), fp$x22[i]))
}

cat("\n================ FALSE NEGATIVES (prereg_found=FALSE, manual=TRUE) ================\n")
fn <- out[!is.na(out$manual_prereg) & !out$prereg_found & out$manual_prereg, ]
for (i in seq_len(nrow(fn))) {
  cat(sprintf("\n[%s] %s\n  platform: %s\n  osf_reg_template: %s\n  urls: %s\n  stmt: %s\n  has_negated: %s\n  manual X2.2: %s\n",
              fn$pilot[i], fn$doi[i], fn$prereg_platform[i] %||% "NA", fn$osf_reg_template[i] %||% "NA",
              substr(fn$prereg_urls[i] %||% "", 1, 200),
              substr(fn$prereg_statement[i] %||% "", 1, 200),
              fn$has_negated_prereg[i], fn$x22[i]))
}

cat("\nManual join coverage:\n")
cat("  rows with non-NA manual_prereg binary:", sum(!is.na(out$manual_prereg)),
    "(pilot1:", sum(!is.na(out$manual_prereg) & out$pilot=="pilot1"),
    "pilot2:", sum(!is.na(out$manual_prereg) & out$pilot=="pilot2"), ")\n")
cat("  rows with no manual coding joined (NA x22):", sum(is.na(out$x22)), "\n")
