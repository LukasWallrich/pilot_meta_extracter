# =============================================================================
# R/3_extract_prereg.R  -- FUNCTION LIBRARY (single source of truth)
# -----------------------------------------------------------------------------
# Preregistration / registered-report / trial-registry detection from GROBID
# TEI full text, with OSF GUID-type and registration-template resolution
# (cached). Sourcing this file ONLY defines functions and constants -- no
# top-level execution, no file I/O, no API calls.
#
# Sourced by run_pipeline.R (the pipeline) and validation/pilot_extract_prereg.R
# (pilot-1 + pilot-2 batch reproduction + manual validation).
#
# classify_paper() takes osf_type_of + osf_template_of as ARGUMENTS and emits
# is_registered_report; resolve_osf_caches() wraps the OSF API caching so both
# the pipeline and the pilot driver share one implementation.
# =============================================================================
suppressMessages({
  library(dplyr)
  library(stringr)
  library(xml2)
  library(httr2)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# OSF cache paths (defaults for resolve_osf_caches()).
OSF_GUID_CACHE <- "data/cache/osf_guid_types.csv"
OSF_REG_CACHE  <- "data/cache/osf_reg_templates.csv"

URL_RE <- regex(
  "(https?://[^\\s)\\]\"'>,]+|www\\.[^\\s)\\]\"'>,]+|10\\.\\d{4,}/[^\\s)\\]\"'>,]+|osf\\.io/[^\\s)\\]\"'>,]+|aspredicted\\.org/[^\\s)\\]\"'>,]+)",
  ignore_case = TRUE)

parse_tei <- function(tei_file) {
  doc <- tryCatch({ d <- read_xml(tei_file); xml_ns_strip(d); d },
                  error = function(e) NULL)
  if (is.null(doc)) return(list(text = NA_character_, urls = character(0)))
  xml_remove(xml_find_all(doc, "//listBibl | //div[@type='references']"))
  txt <- xml_text(doc)
  attrs <- c(xml_attr(xml_find_all(doc, "//ptr"), "target"),
             xml_attr(xml_find_all(doc, "//ref"), "target"))
  attrs <- attrs[!is.na(attrs) & grepl("^https?://|osf\\.io|aspredicted|^10\\.", attrs)]
  from_text <- unlist(str_extract_all(txt, URL_RE))
  urls <- unique(c(attrs, from_text))
  urls <- str_replace(urls, "[.,;:)\\]]+$", "")
  list(text = txt, urls = unique(urls))
}

PREPRINT_DOI <- c("10.31234", "10.31219", "10.31222", "10.31235", "10.31730",
                  "10.31231", "10.26434", "10.20944", "10.21203", "10.35542",
                  "10.1101")
PREPRINT_HOST <- regex(
  "psyarxiv|arxiv\\.org|biorxiv|medrxiv|preprints\\.org|ssrn|researchsquare|research-square|osf\\.io/preprints",
  ignore_case = TRUE)

TRIAL_REG <- list(
  c("clinicaltrials\\.gov",                    "ClinicalTrials.gov"),
  c("isrctn",                                  "ISRCTN"),
  c("drks\\.de|drks",                          "DRKS"),
  c("anzctr|actrn\\d",                          "ANZCTR"),
  c("researchregistry",                        "Research Registry"),
  c("prospero|crd\\.york\\.ac\\.uk/prospero",  "PROSPERO"),
  c("chictr",                                  "ChiCTR"),
  c("umin\\.ac\\.jp/ctr|umin\\.ac\\.jp",       "UMIN-CTR")
)
TRIAL_ID <- list(
  c("NCT\\d{8}",            "ClinicalTrials.gov"),
  c("ISRCTN\\d{6,}",        "ISRCTN"),
  c("DRKS\\d{6,}",          "DRKS"),
  c("ACTRN\\d{6,}",         "ANZCTR"),
  c("ChiCTR[-A-Za-z0-9]{4,}", "ChiCTR"),
  c("CRD4?2?\\d{6,}",       "PROSPERO")
)

osf_bare_code <- function(u) {
  ul <- tolower(u)
  if (grepl("osf\\.io/preprints", ul)) return(NA_character_)
  if (grepl("osf\\.io/registries", ul)) return(NA_character_)
  m <- str_match(ul, "osf\\.io/(?:project/)?([a-z0-9]{5,6})(?![a-z0-9])")
  if (is.na(m[1, 1])) return(NA_character_)
  m[1, 2]
}

PREREG_KW <- regex(
  "pre[-\\s]*regist|aspredicted|registered on the osf|osf\\s+registration|time-?stamped (?:hypothes|preregist)",
  ignore_case = TRUE)
RR_KW <- regex("registered report", ignore_case = TRUE)
NEG_PREREG <- regex(
  "(\\bnot\\b|\\bno\\b|\\bnone\\b|\\bnon-?\\b|\\bnever\\b|\\bwithout\\b|\\bneither\\b|wasn'?t|weren'?t|did not|was not|were not|not been)[^.;]{0,40}pre[-\\s]*regist|non-?pre[-\\s]*regist|pre[-\\s]*regist[^.;]{0,25}\\b(was|were)\\b[^.;]{0,15}\\b(not|none)\\b",
  ignore_case = TRUE)

split_sentences <- function(text) {
  if (is.na(text)) return(character(0))
  s <- unlist(str_split(text, "(?<=[.!?])\\s+"))
  s[nchar(s) > 0]
}
keyword_scan <- function(text) {
  sents <- split_sentences(text)
  prereg_sents <- sents[str_detect(sents, PREREG_KW)]
  is_neg <- str_detect(prereg_sents, NEG_PREREG)
  rr_sents <- sents[str_detect(sents, RR_KW)]
  list(pos = prereg_sents[!is_neg], neg = prereg_sents[is_neg], rr = rr_sents)
}
snip <- function(x, n = 300) {
  if (length(x) == 0) return(NA_character_)
  x <- str_squish(x[1])
  if (nchar(x) > n) paste0(substr(x, 1, n), "...") else x
}

DEDICATED_TEMPLATE_RE <- regex(
  "Preregistration|Prereg|Pre-?Registration|Replication Recipe|Pre-?Data Collection",
  ignore_case = TRUE)
is_dedicated_template <- function(tmpl) !is.na(tmpl) & str_detect(tmpl, DEDICATED_TEMPLATE_RE)
# A "Registered Report Protocol" template signals a registered report but is NOT
# caught by DEDICATED_TEMPLATE_RE, so test it separately.
RR_TEMPLATE_RE <- regex("registered report", ignore_case = TRUE)

# classify_paper needs osf_type_of + osf_template_of in scope (passed via args).
classify_paper <- function(p, osf_type_of, osf_template_of) {
  urls <- p$urls
  text <- p$text

  has_aspredicted  <- any(grepl("aspredicted\\.org", urls, ignore.case = TRUE))
  has_osf_reg_route <- any(grepl("osf\\.io/registries", urls, ignore.case = TRUE))
  is_pp_doi <- function(u) any(startsWith(tolower(sub("^https?://(dx\\.)?doi\\.org/", "",
                          tolower(u))), PREPRINT_DOI)) ||
                          any(startsWith(tolower(u), PREPRINT_DOI))
  pp_urls  <- urls[vapply(urls, function(u) is_pp_doi(u) ||
                            grepl(PREPRINT_HOST, u, ignore.case = TRUE), logical(1))]
  has_preprint <- length(pp_urls) > 0

  kw <- keyword_scan(text)
  has_pos_kw <- length(kw$pos) > 0
  has_rr_kw  <- length(kw$rr) > 0

  trial_hits <- character(0); trial_urls <- character(0)
  for (tr in TRIAL_REG) {
    hit <- grepl(tr[1], urls, ignore.case = TRUE)
    if (any(hit)) { trial_hits <- c(trial_hits, tr[2]); trial_urls <- c(trial_urls, urls[hit]) }
  }
  trial_ids <- character(0)
  if (!is.na(text)) for (ti in TRIAL_ID) {
    m <- unlist(str_extract_all(text, regex(ti[1], ignore_case = TRUE)))
    if (length(m) > 0) { trial_hits <- c(trial_hits, ti[2]); trial_ids <- c(trial_ids, unique(m)) }
  }
  is_trial_registry <- length(trial_hits) > 0

  codes <- na.omit(vapply(urls, osf_bare_code, character(1)))
  codes <- codes[codes != ""]
  osf_types <- if (length(codes)) unname(osf_type_of[codes]) else character(0)
  osf_urls  <- urls[!is.na(vapply(urls, osf_bare_code, character(1)))]
  has_osf_node <- any(osf_types %in% c("nodes", "files"), na.rm = TRUE)
  has_osf_preprint <- any(osf_types == "preprints", na.rm = TRUE)
  if (has_osf_preprint) has_preprint <- TRUE

  this_reg_codes <- codes[osf_types == "registrations"]
  reg_templates  <- if (length(this_reg_codes)) unname(osf_template_of[this_reg_codes]) else character(0)
  has_reg_dedicated <- has_osf_reg_route || any(is_dedicated_template(reg_templates))
  has_reg_ambiguous <- any(!is_dedicated_template(reg_templates))
  reg_counts <- has_reg_dedicated || (has_reg_ambiguous && has_pos_kw)

  prereg_found <- has_aspredicted || reg_counts || has_pos_kw

  # Registered report: keyword in text OR an OSF "Registered Report Protocol"
  # registration template.
  has_rr_template <- length(reg_templates) > 0 &&
    any(!is.na(reg_templates) & str_detect(reg_templates, RR_TEMPLATE_RE))
  is_registered_report <- has_rr_kw || has_rr_template

  platforms <- character(0)
  if (has_aspredicted)        platforms <- c(platforms, "AsPredicted")
  if (reg_counts)             platforms <- c(platforms, "OSF registration")
  if (has_reg_ambiguous && !reg_counts) platforms <- c(platforms, "OSF registration (unconfirmed)")
  if (has_osf_node && !reg_counts) platforms <- c(platforms, "OSF project")
  if (has_preprint)           platforms <- c(platforms, "preprint")
  if (is_trial_registry)      platforms <- c(platforms, unique(trial_hits))
  if (has_pos_kw && !has_aspredicted && !reg_counts && !has_osf_node && length(this_reg_codes) == 0)
    platforms <- c(platforms, "statement (no URL)")
  platforms <- unique(platforms)

  # registration_any per task spec: prereg OR registered report OR trial registry
  registration_any <- prereg_found || is_registered_report || is_trial_registry

  rel_urls <- unique(c(urls[grepl("aspredicted\\.org", urls, ignore.case = TRUE)],
                       osf_urls, pp_urls, trial_urls, trial_ids))

  stmt <- if (has_pos_kw) snip(kw$pos)
          else if (length(kw$neg)) snip(kw$neg)
          else NA_character_

  data.frame(
    prereg_found = prereg_found,
    registration_any = registration_any,
    is_registered_report = is_registered_report,
    prereg_platform = if (length(platforms)) paste(platforms, collapse = " || ") else NA_character_,
    osf_reg_template = if (length(reg_templates)) paste(unique(reg_templates), collapse = " || ") else NA_character_,
    prereg_urls = if (length(rel_urls)) paste(rel_urls, collapse = " || ") else NA_character_,
    prereg_statement = stmt,
    is_trial_registry = is_trial_registry,
    has_negated_prereg = length(kw$neg) > 0,
    stringsAsFactors = FALSE
  )
}

# --- OSF cache resolution (copied from R/3_extract_prereg.R) ---------------------
osf_query_one <- function(code) {
  url <- sprintf("https://api.osf.io/v2/guids/%s/", code)
  res <- tryCatch({
    resp <- request(url) |>
      req_user_agent("prereg-extractor (research; lukas.wallrich@gmail.com)") |>
      req_timeout(30) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    sc <- resp_status(resp)
    if (sc == 200) {
      j <- resp_body_json(resp, simplifyVector = TRUE)
      ty <- tryCatch(j$data$type, error = function(e) NA_character_)
      if (is.null(ty) || length(ty) == 0) ty <- "unknown"
      ty[1]
    } else if (sc %in% c(401, 403)) "private" else "unknown"
  }, error = function(e) "error")
  res
}
osf_query_template <- function(code) {
  url <- sprintf("https://api.osf.io/v2/registrations/%s/", code)
  tryCatch({
    resp <- request(url) |>
      req_user_agent("prereg-extractor (research; lukas.wallrich@gmail.com)") |>
      req_timeout(30) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    if (resp_status(resp) == 200) {
      j <- resp_body_json(resp, simplifyVector = TRUE)
      a <- j$data$attributes
      data.frame(code = code,
                 template  = (a$registration_supplement %||% NA_character_)[1],
                 category  = (a$category %||% NA_character_)[1],
                 title     = (a$title %||% NA_character_)[1],
                 stringsAsFactors = FALSE)
    } else {
      data.frame(code = code, template = NA_character_, category = NA_character_,
                 title = NA_character_, stringsAsFactors = FALSE)
    }
  }, error = function(e)
    data.frame(code = code, template = NA_character_, category = NA_character_,
               title = NA_character_, stringsAsFactors = FALSE))
}

resolve_osf_caches <- function(parsed_list, use_api = TRUE,
                               guid_cache_path = OSF_GUID_CACHE,
                               reg_cache_path = OSF_REG_CACHE) {
  all_codes <- unique(na.omit(unlist(lapply(parsed_list, function(p)
    vapply(p$urls, osf_bare_code, character(1))))))
  all_codes <- all_codes[all_codes != ""]

  # GUID type cache
  if (file.exists(guid_cache_path)) {
    osf_cache <- read.csv(guid_cache_path, stringsAsFactors = FALSE)
  } else {
    osf_cache <- data.frame(code = character(0), osf_type = character(0),
                            queried_at = character(0), stringsAsFactors = FALSE)
  }
  to_query <- setdiff(all_codes, osf_cache$code)
  if (length(to_query) > 0) {
    if (!use_api) stop("OSF guid cache incomplete but use_api = FALSE (",
                       length(to_query), " codes missing)")
    message("Resolving ", length(to_query), " new OSF codes via API...")
    new_rows <- vector("list", length(to_query))
    for (i in seq_along(to_query)) {
      new_rows[[i]] <- data.frame(code = to_query[i], osf_type = osf_query_one(to_query[i]),
                                  queried_at = as.character(Sys.time()), stringsAsFactors = FALSE)
      Sys.sleep(0.35)
    }
    osf_cache <- bind_rows(osf_cache, bind_rows(new_rows))
    write.csv(osf_cache, guid_cache_path, row.names = FALSE)
  }
  osf_type_of <- setNames(osf_cache$osf_type, osf_cache$code)

  # Registration template cache (only for registration-typed codes)
  reg_codes <- intersect(names(osf_type_of)[osf_type_of == "registrations"], all_codes)
  if (file.exists(reg_cache_path)) {
    osf_reg_cache <- read.csv(reg_cache_path, stringsAsFactors = FALSE)
  } else {
    osf_reg_cache <- data.frame(code = character(0), template = character(0),
                                category = character(0), title = character(0),
                                stringsAsFactors = FALSE)
  }
  reg_to_query <- setdiff(reg_codes, osf_reg_cache$code)
  if (length(reg_to_query) > 0) {
    if (!use_api) stop("OSF reg-template cache incomplete but use_api = FALSE (",
                       length(reg_to_query), " codes missing)")
    message("Resolving ", length(reg_to_query), " new OSF registration templates via API...")
    rows <- vector("list", length(reg_to_query))
    for (i in seq_along(reg_to_query)) {
      rows[[i]] <- osf_query_template(reg_to_query[i]); Sys.sleep(0.8)
    }
    osf_reg_cache <- bind_rows(osf_reg_cache, bind_rows(rows))
    write.csv(osf_reg_cache, reg_cache_path, row.names = FALSE)
  }
  osf_template_of <- setNames(osf_reg_cache$template, osf_reg_cache$code)

  list(type = osf_type_of, template = osf_template_of)
}
