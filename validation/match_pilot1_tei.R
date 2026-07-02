#!/usr/bin/env Rscript
# match_pilot1_tei.R
# Build a mapping from the manually-coded pilot-1 DOIs to their GROBID TEI files.
#
# Pilot-1 TEI filenames do NOT contain DOIs; they are
#   "FirstAuthorSurname et al. - YEAR - truncated title".
# We therefore fetch bibliographic metadata (first-author family name, year,
# title) for each DOI from OpenAlex (with a Crossref fallback for DOIs whose
# OpenAlex record is wrong / a collision), then match against the canonical
# filename<->tei_file list in pilot_keyword_screening_filenames.csv.
#
# Outputs: data/pilot1_doi_tei_map.csv  and prints a full match-rate report.
#
# Run with the OpenAlex polite-pool key available:
#   set -a; source ~/.claude/api_keys.env; set +a; Rscript match_pilot1_tei.R

suppressMessages({
  library(jsonlite)
  library(httr)
  library(stringdist)
})

# ---------------------------------------------------------------------------
# Config / paths
# ---------------------------------------------------------------------------
coding_csv <- "data/2025-12-09_Codingform_PilotingAssessment_pilot.csv"
helper_csv <- "../pilot_pilot_teis/pilot_keyword_screening_filenames.csv"
tei_dir    <- "../pilot_pilot_teis/tei"
oa_cache   <- "data/pilot1_oa_metadata.csv"
cr_cache   <- "data/pilot1_crossref_metadata.csv"
out_csv    <- "data/pilot1_doi_tei_map.csv"
MAILTO     <- "lukas.wallrich@gmail.com"
SCORE_MIN  <- 0.85   # minimum title-prefix similarity to accept a match

# ---------------------------------------------------------------------------
# Text helpers
# ---------------------------------------------------------------------------
clean_doi <- function(x) {
  x <- trimws(tolower(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  trimws(x)
}

# Normalise a string for comparison.
norm_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("horizontal ellipsis", " ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Normalise a surname for comparison.
norm_surname <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(ifelse(is.na(x), "", x))
  gsub("[^a-z]", "", x)
}

# Surname of an OpenAlex/Crossref "Given Family" display name.
surname_of <- function(display_name) {
  if (is.na(display_name) || !nzchar(display_name)) return(NA_character_)
  parts <- strsplit(trimws(display_name), "\\s+")[[1]]
  norm_surname(parts[length(parts)])
}

# ---------------------------------------------------------------------------
# 1. Read & clean DOIs
# ---------------------------------------------------------------------------
d <- read.csv(coding_csv, check.names = TRUE, stringsAsFactors = FALSE)
dois <- clean_doi(d[["X1.2..Article.DOI"]])
dois <- dois[!is.na(dois) & dois != "" & dois != "test"]
dois <- unique(dois)
message(sprintf("Distinct pilot-1 DOIs: %d", length(dois)))

# ---------------------------------------------------------------------------
# 2. OpenAlex metadata (cached)
# ---------------------------------------------------------------------------
api_key <- Sys.getenv("OPENALEX_API_KEY")

fetch_oa <- function(doi) {
  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
  q <- list(mailto = MAILTO)
  if (nzchar(api_key)) q$api_key <- api_key
  resp <- tryCatch(GET(url, query = q, timeout(30)), error = function(e) NULL)
  bad <- function(st) data.frame(doi = doi, title = NA_character_, year = NA_integer_,
                                 first_author = NA_character_, status = st,
                                 stringsAsFactors = FALSE)
  if (is.null(resp) || status_code(resp) != 200)
    return(bad(if (is.null(resp)) "error" else as.character(status_code(resp))))
  j <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j)) return(bad("parse_error"))
  title <- j$title; if (is.null(title)) title <- j$display_name
  title <- if (is.null(title)) NA_character_ else title
  year  <- if (is.null(j$publication_year)) NA_integer_ else as.integer(j$publication_year)
  fa <- NA_character_
  if (!is.null(j$authorships) && length(j$authorships) > 0) {
    for (a in j$authorships) {
      if (!is.null(a$author_position) && a$author_position == "first") {
        if (!is.null(a$author$display_name)) fa <- a$author$display_name
        break
      }
    }
    if (is.na(fa) && !is.null(j$authorships[[1]]$author$display_name))
      fa <- j$authorships[[1]]$author$display_name
  }
  data.frame(doi = doi, title = title, year = year, first_author = fa,
             status = "ok", stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Crossref metadata (cached) -- fallback for wrong/collided OpenAlex records
# ---------------------------------------------------------------------------
fetch_cr <- function(doi) {
  url <- paste0("https://api.crossref.org/works/", doi)
  resp <- tryCatch(GET(url, query = list(mailto = MAILTO), timeout(30)),
                   error = function(e) NULL)
  bad <- function(st) data.frame(doi = doi, title = NA_character_, year = NA_integer_,
                                 first_author = NA_character_, status = st,
                                 stringsAsFactors = FALSE)
  if (is.null(resp) || status_code(resp) != 200)
    return(bad(if (is.null(resp)) "error" else as.character(status_code(resp))))
  j <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j) || is.null(j$message)) return(bad("parse_error"))
  m <- j$message
  title <- if (!is.null(m$title) && length(m$title) > 0) m$title[[1]] else NA_character_
  year <- NA_integer_
  dp <- m$issued$`date-parts`
  if (!is.null(dp) && length(dp) > 0 && length(dp[[1]]) > 0)
    year <- as.integer(dp[[1]][[1]])
  fa <- NA_character_
  if (!is.null(m$author) && length(m$author) > 0) {
    a <- m$author[[1]]
    fam <- a$family; giv <- a$given
    if (!is.null(fam)) fa <- paste(c(giv, fam), collapse = " ")
  }
  data.frame(doi = doi, title = title, year = year, first_author = fa,
             status = "ok", stringsAsFactors = FALSE)
}

load_or_fetch <- function(cache, dois, fetch_fn, label) {
  if (file.exists(cache)) {
    m <- read.csv(cache, stringsAsFactors = FALSE)
  } else {
    m <- data.frame(doi = character(), title = character(), year = integer(),
                    first_author = character(), status = character(),
                    stringsAsFactors = FALSE)
  }
  todo <- setdiff(dois, m$doi)
  if (length(todo) > 0) {
    message(sprintf("Querying %s for %d DOIs ...", label, length(todo)))
    rows <- vector("list", length(todo))
    for (i in seq_along(todo)) {
      rows[[i]] <- fetch_fn(todo[i]); Sys.sleep(0.12)
      if (i %% 25 == 0) message(sprintf("  ... %d/%d", i, length(todo)))
    }
    m <- rbind(m, do.call(rbind, rows))
    write.csv(m, cache, row.names = FALSE)
  }
  m
}

oa <- load_or_fetch(oa_cache, dois, fetch_oa, "OpenAlex")
oa <- oa[match(dois, oa$doi), ]
message(sprintf("OpenAlex: %d ok, %d failed",
                sum(oa$status == "ok", na.rm = TRUE), sum(oa$status != "ok", na.rm = TRUE)))

# ---------------------------------------------------------------------------
# 3. Parse helper file into (surname, year, title) candidates
# ---------------------------------------------------------------------------
h <- read.csv(helper_csv, stringsAsFactors = FALSE)
h <- h[grepl(" - [0-9]{4} - ", h$filename), ]   # drop the stray "main" row

parse_filename <- function(fn) {
  m <- regmatches(fn, regexec("^(.*?) - ([0-9]{4}) - (.*)$", fn))[[1]]
  author <- m[2]; year <- as.integer(m[3]); title <- m[4]
  fa <- author
  fa <- sub("\\s+et al\\..*$", "", fa)
  fa <- sub("\\s+and\\s+.*$", "", fa)
  fa <- sub("\\s*&.*$", "", fa)
  fa <- sub(",.*$", "", fa)
  list(surname = trimws(fa), year = year, title = title)
}
ph <- lapply(h$filename, parse_filename)
h$f_surname <- norm_surname(vapply(ph, `[[`, "", "surname"))
h$f_year    <- vapply(ph, `[[`, integer(1), "year")
h$f_title_n <- norm_txt(vapply(ph, `[[`, "", "title"))

# ---------------------------------------------------------------------------
# 4. Matching
# ---------------------------------------------------------------------------
# Title-prefix similarity: the filename title is truncated, so compare the
# metadata title's leading characters (to filename-title length) by Jaro-Winkler.
title_sim <- function(meta_title, f_title_n) {
  if (is.na(meta_title) || nchar(f_title_n) == 0) return(NA_real_)
  mn <- norm_txt(meta_title)
  L <- nchar(f_title_n)
  stringsim(substr(mn, 1, L), f_title_n, method = "jw", p = 0.1)
}

# Surname-compatible: equal, OR one is a prefix of the other with the shorter
# string >= 4 chars (so "kim" only matches "kim", but "miller" matches
# "millergoldwater"). Used ONLY to add candidates; title sim disambiguates.
surname_compat_vec <- function(fsur_vec, fasur) {
  if (is.na(fasur) || !nzchar(fasur)) return(rep(FALSE, length(fsur_vec)))
  eq <- fsur_vec == fasur
  pref <- vapply(fsur_vec, function(fs) {
    if (is.na(fs) || !nzchar(fs)) return(FALSE)
    short <- if (nchar(fs) <= nchar(fasur)) fs else fasur
    long  <- if (nchar(fs) <= nchar(fasur)) fasur else fs
    nchar(short) >= 4 && startsWith(long, short)
  }, logical(1))
  eq | pref
}

# Match one set of metadata. Tries candidate pools from most- to least-
# anchored; accepts the first pool whose best title sim >= SCORE_MIN.
# Returns the accepted result, or (if none reaches threshold) the best
# near-miss found for diagnostic reporting.
match_meta <- function(meta_title, meta_year, meta_fa) {
  fa_sur <- surname_of(meta_fa)
  pick_best <- function(idx, method) {
    if (length(idx) == 0) return(NULL)
    sims <- vapply(idx, function(k) title_sim(meta_title, h$f_title_n[k]), numeric(1))
    if (all(is.na(sims))) return(NULL)
    b <- which.max(sims)
    list(idx = idx[b], method = method, score = sims[b])
  }
  pools <- list()
  compat <- surname_compat_vec(h$f_surname, fa_sur)
  if (!is.na(fa_sur)) {
    if (!is.na(meta_year))
      pools[["surname+year"]] <- which(compat & abs(h$f_year - meta_year) <= 1)
    pools[["surname"]] <- which(compat)
  }
  if (!is.na(meta_year))
    pools[["year+title"]] <- which(abs(h$f_year - meta_year) <= 1)
  pools[["title_only"]] <- seq_len(nrow(h))

  best_overall <- NULL
  for (nm in names(pools)) {
    r <- pick_best(pools[[nm]], nm)
    if (is.null(r)) next
    if (is.null(best_overall) || r$score > best_overall$score) best_overall <- r
    if (r$score >= SCORE_MIN) return(r)   # first anchored pool that clears bar
  }
  if (is.null(best_overall))
    return(list(idx = NA_integer_, method = "none", score = NA_real_))
  best_overall
}

# Pass 1: OpenAlex metadata
src      <- rep("openalex", nrow(oa))
m_title  <- oa$title; m_year <- oa$year; m_fa <- oa$first_author
res <- lapply(seq_len(nrow(oa)),
              function(i) match_meta(m_title[i], m_year[i], m_fa[i]))
score <- vapply(res, function(r) as.numeric(r$score), numeric(1))

# Pass 2: Crossref fallback. Re-verify (a) DOIs that did not clear the bar, and
# (b) matches that are NOT surname-anchored (title_only / year+title) -- these
# lack an author anchor and a borderline title sim can be a false positive
# driven by a wrong/collided OpenAlex record (e.g. pas0001107 -> wrong author).
method_p1 <- vapply(res, function(r) r$method, character(1))
need_cr <- which(is.na(score) | score < SCORE_MIN |
                 method_p1 %in% c("title_only", "year+title"))
if (length(need_cr) > 0) {
  cr <- load_or_fetch(cr_cache, dois[need_cr], fetch_cr, "Crossref")
  cr <- cr[match(dois[need_cr], cr$doi), ]
  for (j in seq_along(need_cr)) {
    i <- need_cr[j]
    if (is.na(cr$status[j]) || cr$status[j] != "ok") next
    r2 <- match_meta(cr$title[j], cr$year[j], cr$first_author[j])
    s2 <- as.numeric(r2$score)
    # adopt Crossref result if it clears the bar (and OpenAlex did not),
    # or if it is a strict improvement
    if (!is.na(s2) && (is.na(score[i]) || s2 > score[i])) {
      res[[i]] <- r2; score[i] <- s2
      src[i]   <- "crossref"
      m_title[i] <- cr$title[j]; m_year[i] <- cr$year[j]; m_fa[i] <- cr$first_author[j]
    }
  }
}

# ---------------------------------------------------------------------------
# 5. Assemble output
# ---------------------------------------------------------------------------
idx     <- vapply(res, function(r) as.integer(r$idx), integer(1))
method  <- vapply(res, function(r) r$method, character(1))
accepted <- !is.na(score) & score >= SCORE_MIN

tei_basename <- ifelse(is.na(idx) | !accepted, NA_character_, h$tei_file[idx])
tei_file <- ifelse(is.na(tei_basename), NA_character_, file.path(tei_dir, tei_basename))

final <- data.frame(
  doi = dois,
  tei_file = tei_file,
  tei_basename = tei_basename,
  oa_title = m_title,
  oa_year = m_year,
  oa_first_author = m_fa,
  match_method = ifelse(src == "crossref" & accepted, paste0("crossref:", method), method),
  match_score = round(score, 4),
  stringsAsFactors = FALSE
)
write.csv(final, out_csv, row.names = FALSE)

# ---------------------------------------------------------------------------
# 6. Report
# ---------------------------------------------------------------------------
n_total <- nrow(final); n_match <- sum(accepted)
cat("\n==================== MATCH REPORT ====================\n")
cat(sprintf("Matched: %d / %d  (%.1f%%)\n", n_match, n_total, 100 * n_match / n_total))
cat(sprintf("Metadata source for matches: openalex=%d, crossref=%d\n",
            sum(accepted & src == "openalex"), sum(accepted & src == "crossref")))
cat("\nMethod breakdown (accepted):\n")
print(table(final$match_method[accepted]))

dup_tab <- table(final$tei_basename[accepted])
dups <- names(dup_tab[dup_tab > 1])
cat(sprintf("\nDuplicate tei_file assignments: %d\n", length(dups)))
for (db in dups) {
  cat("  DUPLICATE:", db, "\n")
  print(final[which(final$tei_basename == db), c("doi","oa_first_author","oa_year","match_score")])
}

border <- accepted & score < 0.92
cat(sprintf("\nBorderline accepted (%.2f <= score < 0.92): %d\n", SCORE_MIN, sum(border)))
if (any(border))
  print(final[border, c("doi","oa_first_author","oa_year","match_score","tei_basename")])

unm <- which(!accepted)
cat(sprintf("\nUNMATCHED DOIs: %d\n", length(unm)))
for (i in unm)
  cat(sprintf("  - %s | %s (%s) | %s | bestscore=%.3f via %s [src=%s]\n",
              final$doi[i],
              ifelse(is.na(final$oa_first_author[i]), "?", final$oa_first_author[i]),
              ifelse(is.na(final$oa_year[i]), "?", final$oa_year[i]),
              ifelse(is.na(final$oa_title[i]), "?", substr(final$oa_title[i], 1, 70)),
              ifelse(is.na(score[i]), -1, score[i]), method[i], src[i]))

cat("\nMap written to:", out_csv, "\n")
