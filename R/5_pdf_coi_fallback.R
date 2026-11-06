# =============================================================================
# R/5_pdf_coi_fallback.R  -- FUNCTION LIBRARY (no side effects when sourced)
# -----------------------------------------------------------------------------
# Optional COI fallback from the PDF's front pages. GROBID drops the page-1
# author note that APA (10.1037), Cambridge (10.1017) and 10.1192 use for COI
# disclosures, and repository copies with a cover sheet push it to page 2-3.
# When neither the TEI nor Crossref yields a COI statement (or the TEI only has
# the "[COI heading found, statement text missing in TEI]" placeholder), the
# raw text of pages 1-3 is extracted with `pdftotext` (poppler) and searched for
# disclosure-style COI sentences. The result is classified downstream by the
# same COI_NEG_RE negation regex as TEI/Crossref statements.
#
# Requires the `pdftotext` binary on PATH. Text is cached per DOI in
# data/cache/pdf_front_text/<doi-key>.txt, so reruns do not re-read the PDFs
# (useful when they sit on an on-demand Google Drive mount).
# =============================================================================

PDF_TEXT_CACHE   <- file.path("data", "cache", "pdf_front_text")
PDF_FRONT_PAGES  <- 3L
COI_PLACEHOLDER  <- "[COI heading found, statement text missing in TEI]"

# The COI term must name an *interest* (or a financial disclosure), so that
# "there was no conflict between conditions" in a Stroop paper cannot fire.
COI_TERM_RE <- stringr::regex(
  "conflicts?.{0,10}interests?|competing.{0,5}interests?|declarations?.{0,15}interests?|financial disclosures?",
  ignore_case = TRUE)
# ... and the sentence must read as a disclosure, not a body-text mention.
COI_DISCLOSE_RE <- stringr::regex(paste0(
  "\\b(declare[sd]?|disclose[sd]?|disclosures?|report(s|ed)?|none|nil)\\b|",
  "\\b(ha(ve|s)|there (is|are|were|was)) no\\b|",
  "^\\W*(conflicts?.{0,10}interests?|competing.{0,5}interests?|declarations?.{0,15}interests?)\\W*[:.]"),
  ignore_case = TRUE)
# A capitalised COI heading ("Conflict of interest:", "Declarations of interest.")
COI_HEAD <- "[CD](?i:onflicts? of interests?|ompeting interests?|eclarations? of (?:conflicting |competing )?interests?)(?i:\\s*statement)?"
COI_HEAD_SPLIT_RE <- stringr::regex(paste0("(?=\\b", COI_HEAD, "\\s*[:.])"))
COI_HEAD_ONLY_RE  <- stringr::regex(paste0("^\\W*", COI_HEAD, "\\W*$"))
COI_HEAD_NONE_RE  <- stringr::regex(paste0("^\\W*(", COI_HEAD, ")\\W*(?i:none|nil)\\b(?i:\\s+to\\s+(declare|disclose|report))?.*$"))

pdf_cache_file <- function(doi, cache_dir = PDF_TEXT_CACHE)
  file.path(cache_dir, paste0(gsub("[^A-Za-z0-9.-]", "_", gsub("/", "-", tolower(doi))), ".txt"))

# Raw text of the first `pages` pages, NFKC-normalised (ligatures such as "ﬂ"),
# de-hyphenated and whitespace-collapsed. Cached per DOI; NA if no PDF.
pdf_front_text <- function(pdf, doi, pages = PDF_FRONT_PAGES, cache_dir = PDF_TEXT_CACHE) {
  cf <- pdf_cache_file(doi, cache_dir)
  if (!file.exists(cf) || file.size(cf) == 0) {
    if (is.na(pdf) || !file.exists(pdf)) return(NA_character_)
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    ok <- system2("pdftotext", c("-f", "1", "-l", pages, "-enc", "UTF-8",
                                 shQuote(path.expand(pdf)), shQuote(cf)),
                  stdout = FALSE, stderr = FALSE)
    if (ok != 0 || !file.exists(cf)) return(NA_character_)
  }
  txt <- paste(readLines(cf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  txt <- stringi::stri_trans_nfkc(txt)
  txt <- gsub("(\\w)-\\n(\\w)", "\\1\\2", txt)
  gsub("\\s+", " ", txt)
}

# Disclosure-style COI sentence(s) from front-page text, '||'-joined; NA if none.
pdf_coi_text <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(NA_character_)
  txt <- stringr::str_replace_all(txt, COI_HEAD_SPLIT_RE, ". ")
  s <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
  out <- character(0)
  for (i in which(stringr::str_detect(s, COI_TERM_RE))) {
    cand <- s[i]
    # a bare heading ("Conflict of interest.") takes the next sentence as its body
    if (stringr::str_detect(cand, COI_HEAD_ONLY_RE) && i < length(s)) cand <- paste(cand, s[i + 1])
    # front-page headers run on ("Conflicts of Interest: None Abstract ..."): cut after "None"
    cand <- stringr::str_replace(cand, COI_HEAD_NONE_RE, "\\1: None.")
    if (stringr::str_detect(cand, COI_DISCLOSE_RE)) out <- c(out, stringr::str_trim(cand))
  }
  out <- unique(out[nchar(out) < 600])
  if (length(out)) paste(out, collapse = " || ") else NA_character_
}

# Fill coi_statement from the PDF where it is missing (or the TEI placeholder),
# re-classify, and set coi_source to "pdf_front". `out` needs doi, coi_statement,
# coi_source; `pdf_files` is aligned with out$doi (NA where no PDF).
apply_pdf_coi_fallback <- function(out, pdf_files, cache_dir = PDF_TEXT_CACHE) {
  need <- which(is.na(out$coi_statement) | out$coi_statement == COI_PLACEHOLDER)
  message(sprintf("PDF COI fallback: %d rows without a usable statement, %d with a PDF",
                  length(need), sum(!is.na(pdf_files[need]))))
  for (i in need) {
    stmt <- pdf_coi_text(pdf_front_text(pdf_files[i], out$doi[i], cache_dir = cache_dir))
    if (!is.na(stmt)) {
      out$coi_statement[i] <- stmt
      out$coi_source[i]    <- "pdf_front"
    }
  }
  out$coi_category <- factor(vapply(out$coi_statement, classify_coi, character(1)),
                             levels = c("conflict declared", "no conflict declared", "no statement"))
  message(sprintf("PDF COI fallback: statements recovered for %d rows",
                  sum(out$coi_source == "pdf_front", na.rm = TRUE)))
  out
}

# Source PDFs: the synced Google Drive folder the GROBID action converts from
# (pilot2 = Drive folder 18vChmJn4Q2biMmTbRWWgwkQd8KE002JR; pilot 1 = Zotero_PDFs).
# Override with the PILOT_PDF_ROOT environment variable.
PDF_ROOT_DEFAULT <- Sys.getenv("PILOT_PDF_ROOT",
  "~/Library/CloudStorage/GoogleDrive-lukas.wallrich@gmail.com/My Drive/Pilot-coding pilot PDFs")

# Map TEI file names to their source PDFs. GROBID names a TEI after the PDF with
# every character outside [alnum . -] replaced by "_"; macOS returns file names
# in decomposed Unicode, so both sides are NFC-normalised before matching.
map_pdf_files <- function(tei_files, pdf_root = PDF_ROOT_DEFAULT,
                          subdirs = c("Zotero_PDFs", "pilot2")) {
  nfc  <- stringi::stri_trans_nfc
  pdfs <- unlist(lapply(file.path(path.expand(pdf_root), subdirs), list.files,
                        pattern = "\\.pdf$", full.names = TRUE))
  stem <- nfc(gsub("[^[:alnum:]\\.-]", "_", nfc(tools::file_path_sans_ext(basename(pdfs)))))
  pdfs[match(nfc(sub("\\.tei\\.xml$", "", basename(tei_files))), stem)]
}
