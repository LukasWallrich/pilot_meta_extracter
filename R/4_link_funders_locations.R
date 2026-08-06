# =============================================================================
# R/4_link_funders_locations.R  -- FUNCTION LIBRARY (single source of truth)
# -----------------------------------------------------------------------------
# Funder -> country resolution. Crossref funder IDs and TEI funder names are
# passed in as ARGUMENTS (resolve_funder_ids / resolve_funder_names /
# resolve_paper_funders); this library NEVER reads the combined CSV. Sourcing it
# ONLY defines functions and constants -- no top-level execution, no file I/O
# beyond the on-disk resolution caches (touched only inside the resolve_*
# functions when there are new IDs/names to look up), no unconditional API calls.
#
# Sourced by run_pipeline.R (the pipeline) and validation/pilot_link_funders.R
# (pilot-1 + pilot-2 batch reproduction; the ONLY file allowed to read the
# 2026-06-11 auto-and-manual-combined CSV for the Crossref funder columns).
# =============================================================================
suppressMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
  library(purrr); library(httr2); library(jsonlite); library(countrycode)
  library(stringdist)
})

MAILTO   <- "lukas.wallrich@gmail.com"
# OpenAlex keys are read from OPENALEX_API_KEYS below (see OA_KEY_POOL).
DATA     <- "data"
have_stringi <- requireNamespace("stringi", quietly = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# Registry-resolution cache paths (defaults for the resolve_* functions).
FUNDER_ID_CACHE   <- file.path(DATA, "cache", "funder_id_country.csv")
FUNDER_NAME_CACHE <- file.path(DATA, "cache", "funder_name_country.csv")

strip_ascii <- function(x) {
  if (have_stringi) stringi::stri_trans_general(x, "Latin-ASCII")
  else iconv(x, to = "ASCII//TRANSLIT")
}
STOP <- c("the","of","for","and","a","an","de","la","le","les","du","des","der",
          "die","das","und","fur","von","el","los","den")
norm_name_one <- function(x) {
  if (is.na(x)) return("")
  x <- strip_ascii(as.character(x)); x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 ]", " ", x)
  toks <- unlist(strsplit(x, "\\s+"))
  toks <- toks[toks != "" & !toks %in% STOP]
  paste(toks, collapse = " ")
}
norm_name <- function(x) vapply(x, norm_name_one, character(1), USE.NAMES = FALSE)
name_sim <- function(a, b) {
  if (a == "" || b == "") return(0)
  jw  <- 1 - stringdist(a, b, method = "jw", p = 0.1)
  ta  <- unique(unlist(strsplit(a, " "))); tb <- unique(unlist(strsplit(b, " ")))
  jac <- length(intersect(ta, tb)) / length(union(ta, tb))
  max(jw, jac)
}
# --- OpenAlex key rotation ---------------------------------------------------
# Every OpenAlex request carries a daily credit budget (a name search costs 10
# credits; a key allows 10,000/day, the keyless polite pool only 1,000). A
# full-sample funder-name pass needs ~40,000 credits, i.e. several keys, so we
# hold the whole key list and step to the next key when one reports 429.
OA_KEY_POOL <- local({
  keys <- Sys.getenv("OPENALEX_API_KEYS", "")
  keys <- trimws(unlist(strsplit(keys, ",")))
  keys <- keys[nzchar(keys)]
  if (!length(keys)) {
    k1 <- Sys.getenv("OPENALEX_API_KEY", "")
    if (nzchar(k1)) keys <- k1
  }
  keys
})
oa_key_state <- new.env(parent = emptyenv())
oa_key_state$i <- 1L        # index of the key currently in use

current_oa_key <- function() {
  if (!length(OA_KEY_POOL) || oa_key_state$i > length(OA_KEY_POOL)) return("")
  OA_KEY_POOL[oa_key_state$i]
}
# Retire the exhausted key and report what is left.
rotate_oa_key <- function() {
  oa_key_state$i <- oa_key_state$i + 1L
  if (oa_key_state$i > length(OA_KEY_POOL))
    message("  OpenAlex: all ", length(OA_KEY_POOL), " keys exhausted")
  else
    message("  OpenAlex: key ", oa_key_state$i - 1L, " out of quota, switching to key ",
            oa_key_state$i, " of ", length(OA_KEY_POOL))
  invisible(current_oa_key())
}

get_json <- function(url) {
  perform_once <- function(key) {
    req <- request(url) |>
      req_url_query(mailto = MAILTO) |>
      req_user_agent(paste0("pilot_meta_extracter (", MAILTO, ")")) |>
      # Cap the wait between tries. Without `after`, httr2 obeys a server
      # Retry-After header verbatim, and an out-of-quota OpenAlex key returns
      # one of ~13 hours, which stalls a large run indefinitely.
      req_retry(max_tries = 3, backoff = ~ 2, after = ~ 5,
                is_transient = function(resp) resp_status(resp) %in% c(500, 502, 503, 504)) |>
      req_throttle(capacity = 8, fill_time_s = 1) |>
      req_timeout(40) |>
      req_error(is_error = function(resp) FALSE)
    if (nzchar(key) && grepl("openalex", url)) req <- req_url_query(req, api_key = key)
    tryCatch(req_perform(req), error = function(e) NULL)
  }

  is_openalex <- grepl("openalex", url)
  repeat {
    key  <- if (is_openalex) current_oa_key() else ""
    resp <- perform_once(key)
    # 429 on a keyed OpenAlex request means that key's daily budget is spent:
    # rotate and retry immediately rather than waiting out its Retry-After.
    if (is_openalex && !is.null(resp) && resp_status(resp) == 429 && nzchar(key)) {
      rotate_oa_key()
      next
    }
    # Out of budget on every key. Abort rather than return NULL: a NULL here is
    # indistinguishable from "the registry has no match", so it would be cached
    # as a permanent `unresolved` and never retried. Completed chunks are
    # already on disk, so rerunning after the daily reset resumes cleanly.
    if (is_openalex && !is.null(resp) && resp_status(resp) == 429 && !nzchar(key))
      stop("OpenAlex daily quota exhausted on all ", length(OA_KEY_POOL),
           " keys and on the keyless pool. Cached progress is kept -- ",
           "rerun after the quota resets (see the Retry-After header, ~13 h).",
           call. = FALSE)
    if (is.null(resp) || resp_status(resp) >= 400) return(NULL)
    return(tryCatch(fromJSON(resp_body_string(resp), simplifyVector = FALSE),
                    error = function(e) NULL))
  }
}
cc_to_country <- function(cc) suppressWarnings(countrycode(cc, "iso2c", "country.name", warn = FALSE))

FUNDER_TOKENS <- paste0(
  "council|conseil|consejo|conselho|foundation|fondation|fondazione|fundac|",
  "stiftung|fonds|\\bfund\\b|\\bfunds\\b|funding|minist|universi|college|",
  "institut|research|forschung|\\bscien|academ|\\bagenc|societ|\\bassoci|",
  "\\btrust\\b|cent(er|re)|department|\\boffice\\b|programme|\\bprogram\\b|",
  "\\bscheme\\b|fellowship|\\bgrant|commission|authority|organi|\\bboard\\b|",
  "charity|hospital|government|gobierno|gesellschaft|wellcome|chair\\b|",
  "endowment|laboratory|nationale|nazionale|initiative")
has_funder_token <- function(raw) grepl(FUNDER_TOKENS, tolower(strip_ascii(raw)))
contained_cc <- function(raw) {
  cc <- suppressWarnings(countrycode(raw, "country.name", "iso2c", warn = FALSE))
  if (!is.na(cc)) return(cc)
  r <- tolower(strip_ascii(raw))
  if (grepl("\\b(u\\.?k\\.?|britain|british)\\b", r)) return("GB")
  if (grepl("\\b(u\\.?s\\.?a?\\.?|american)\\b", r))  return("US")
  NA_character_
}
is_country_only <- function(raw) !is.na(contained_cc(raw)) && !has_funder_token(raw)
PUBLISHER_PAT <- paste0(
  "^bmc\\b|^plos\\b|^frontiers\\b|^springer|^elsevier|^wiley|^sage\\b|^hindawi|",
  "^mdpi|^taylor\\b|nature publishing|\\bjournal of\\b|\\bproceedings of\\b|",
  "annual review|\\btransactions on\\b|\\bbmj\\b")
is_journal_like <- function(raw, journal) {
  r <- tolower(strip_ascii(raw))
  if (grepl(PUBLISHER_PAT, r)) return(TRUE)
  if (!is.na(journal) && nzchar(journal)) {
    nj <- norm_name_one(journal); nr <- norm_name_one(raw)
    if (nj != "" && nr != "" && name_sim(nr, nj) >= 0.90) return(TRUE)
  }
  FALSE
}
EU_NAME_PAT <- paste0(
  "european commission|european research council|horizon 2020|horizon europe|",
  "\\bh2020\\b|marie sk|marie curie|\\berc\\b|european union|european regional")
is_eu_name <- function(raw) grepl(EU_NAME_PAT, tolower(strip_ascii(raw)))
junk_terms <- c("unknown","na","n/a","none","null","not applicable","")
is_acronym <- function(raw) grepl("^[A-Z][A-Z0-9]{1,5}$", str_trim(raw)) && is.na(contained_cc(raw))
initials   <- function(nm) {
  toks <- unlist(strsplit(norm_name_one(nm), " "))
  paste(toupper(substr(toks, 1, 1)), collapse = "")
}
merge_key_match <- function(a, b) {
  na <- norm_name_one(a); nb <- norm_name_one(b)
  if (na == "" || nb == "") return(FALSE)
  if (na == nb) return(TRUE)
  ta <- unlist(strsplit(na, " ")); tb <- unlist(strsplit(nb, " "))
  if (all(ta %in% tb) || all(tb %in% ta)) return(TRUE)
  for (pair in list(c(a, b), c(b, a))) {
    s <- str_trim(pair[1]); l <- pair[2]
    if (grepl("^[A-Za-z]{2,6}$", s) && toupper(s) == initials(l)) return(TRUE)
  }
  if (name_sim(na, nb) >= 0.90) return(TRUE)
  FALSE
}
CONF_RANK <- c(high = 1, medium = 2, low = 3)
cluster_funders <- function(df) {
  if (!nrow(df)) return(df[0, ])
  used <- rep(FALSE, nrow(df)); out <- list()
  for (i in seq_len(nrow(df))) {
    if (used[i]) next
    grp <- i; used[i] <- TRUE
    for (j in seq_len(nrow(df))) {
      if (used[j]) next
      same_id <- !is.na(df$crossref_funder_id[i]) && !is.na(df$crossref_funder_id[j]) &&
                 df$crossref_funder_id[i] == df$crossref_funder_id[j]
      if (same_id || merge_key_match(df$name[i], df$name[j])) { grp <- c(grp, j); used[j] <- TRUE }
    }
    g <- df[grp, , drop = FALSE]
    canon <- g$name[which.max(nchar(g$name))]
    gc <- g |> filter(!is.na(country)) |> arrange(pref)
    ctry <- if (nrow(gc)) gc$country[1] else NA_character_
    csrc <- if (nrow(gc)) gc$country_source[1] else NA_character_
    cid  <- { v <- g$crossref_funder_id[!is.na(g$crossref_funder_id)]; if (length(v)) v[1] else NA_character_ }
    src  <- if (length(unique(g$source)) > 1) "both" else g$source[1]
    conf <- names(CONF_RANK)[min(CONF_RANK[g$conf])]
    out[[length(out)+1]] <- tibble(name = canon, crossref_funder_id = cid,
                                   country = ctry, country_source = csrc, source = src, conf = conf)
  }
  bind_rows(out)
}

# ---- Registry resolution of Crossref funder IDs (cached) --------------------
eu_ids <- c("10.13039/100010663","10.13039/100010665","10.13039/501100000780",
            "10.13039/501100000781","10.13039/501100007601","10.13039/100011102",
            "10.13039/501100008530")
resolve_funder_ids <- function(all_ids, use_api = TRUE, cache_path = FUNDER_ID_CACHE) {
  all_ids <- all_ids[grepl("^10\\.13039/", all_ids)]
  if (file.exists(cache_path)) {
    id_cache <- readr::read_csv(cache_path, show_col_types = FALSE)
  } else {
    id_cache <- tibble(crossref_funder_id = character(), funder_name = character(),
                       country_code = character(), country = character(), source = character())
  }
  need_ids <- setdiff(all_ids, id_cache$crossref_funder_id)
  if (length(need_ids) > 0) {
    if (!use_api) stop("Funder-id cache incomplete but use_api = FALSE (",
                       length(need_ids), " ids missing)")
    message("Resolving ", length(need_ids), " new Crossref funder IDs via API...")
    oa_rows <- list()
    num_ids <- sub("^10\\.13039/", "", need_ids)
    batches <- split(num_ids, ceiling(seq_along(num_ids) / 50))
    for (b in batches) {
      url <- paste0("https://api.openalex.org/funders?per-page=50&filter=ids.crossref:",
                    paste(b, collapse = "|"))
      js <- get_json(url)
      if (!is.null(js) && length(js$results) > 0) {
        for (f in js$results) {
          cr <- f$ids$crossref; if (is.null(cr)) next
          oa_rows[[length(oa_rows) + 1]] <- tibble(
            crossref_funder_id = paste0("10.13039/", cr),
            funder_name = f$display_name %||% NA_character_,
            country_code = f$country_code %||% NA_character_, source = "openalex")
        }
      }
      Sys.sleep(0.15)
    }
    oa_df <- if (length(oa_rows)) bind_rows(oa_rows) else
      tibble(crossref_funder_id = character(), funder_name = character(),
             country_code = character(), source = character())
    resolved_ids <- oa_df$crossref_funder_id[!is.na(oa_df$country_code)]
    fallback_ids <- setdiff(need_ids, resolved_ids)
    cr_rows <- list()
    for (id in fallback_ids) {
      num <- sub("^10\\.13039/", "", id)
      js  <- get_json(paste0("https://api.crossref.org/funders/", num))
      nm <- cc <- NA_character_
      if (!is.null(js) && !is.null(js$message)) {
        m <- js$message; nm <- m$name %||% NA_character_
        loc <- m$location %||% NA_character_
        if (!is.na(loc)) cc <- suppressWarnings(countrycode(loc, "country.name", "iso2c", warn = FALSE))
      }
      cr_rows[[length(cr_rows) + 1]] <- tibble(
        crossref_funder_id = id, funder_name = nm, country_code = cc,
        source = if (!is.na(cc)) "crossref_registry" else "unresolved")
      Sys.sleep(0.05)
    }
    cr_df <- if (length(cr_rows)) bind_rows(cr_rows) else
      tibble(crossref_funder_id = character(), funder_name = character(),
             country_code = character(), source = character())
    new_id <- bind_rows(oa_df, cr_df) |>
      group_by(crossref_funder_id) |>
      arrange(is.na(country_code), match(source, c("openalex","crossref_registry","unresolved"))) |>
      slice(1) |> ungroup()
    miss <- setdiff(need_ids, new_id$crossref_funder_id)
    if (length(miss)) new_id <- bind_rows(new_id, tibble(
      crossref_funder_id = miss, funder_name = NA_character_,
      country_code = NA_character_, source = "unresolved"))
    new_id$country <- cc_to_country(new_id$country_code)
    id_cache <- bind_rows(id_cache, new_id |>
                            select(crossref_funder_id, funder_name, country_code, country, source)) |>
      distinct(crossref_funder_id, .keep_all = TRUE)
    id_cache <- id_cache |>
      mutate(country_code = ifelse(crossref_funder_id %in% eu_ids & is.na(country_code), "EU", country_code),
             country      = ifelse(crossref_funder_id %in% eu_ids & is.na(country), "European Union", country),
             source       = ifelse(crossref_funder_id %in% eu_ids & !grepl("eu_override", source),
                                    paste0(source, "+eu_override"), source))
    readr::write_csv(id_cache, cache_path)
  }
  id_cache |> filter(crossref_funder_id %in% all_ids)
}

# ---- Registry resolution of TEI funder NAMES (cached) -----------------------
NAME_SCHEMA <- c("query_name","matched_name","crossref_funder_id","reg_country_code",
                 "reg_country","relevance","similarity","reg_confidence","source")
resolve_name_registry <- function(raw) {
  empty <- tibble(query_name = raw, matched_name = NA_character_, crossref_funder_id = NA_character_,
                  reg_country_code = NA_character_, reg_country = NA_character_,
                  relevance = NA_real_, similarity = 0, reg_confidence = "unresolved",
                  source = "tei_name")
  rl <- tolower(str_trim(raw))
  if (rl %in% junk_terms || is_country_only(raw)) { empty$reg_confidence <- "skipped"; return(empty) }
  url <- paste0("https://api.openalex.org/funders?per-page=5&search=", URLencode(raw, reserved = TRUE))
  js <- get_json(url); Sys.sleep(0.12)
  if (is.null(js) || length(js$results) == 0) return(empty)
  acro <- is_acronym(raw)
  qn   <- norm_name(raw)
  cand <- map_dfr(js$results, function(f) {
    alt <- if (!is.null(f$alternate_titles)) toupper(unlist(f$alternate_titles)) else character(0)
    tibble(matched_name = f$display_name %||% NA_character_,
           crossref_funder_id = if (!is.null(f$ids$crossref)) paste0("10.13039/", f$ids$crossref) else NA_character_,
           country_code = f$country_code %||% NA_character_,
           relevance = f$relevance_score %||% NA_real_,
           acro_hit = acro && (toupper(str_trim(raw)) %in% alt ||
                                 initials(f$display_name %||% "") == toupper(str_trim(raw))))
  })
  cand$similarity <- vapply(cand$matched_name, function(m) name_sim(qn, norm_name(m)), numeric(1))
  if (acro) cand <- cand |> arrange(desc(acro_hit), desc(relevance))
  else      cand <- cand |> arrange(desc(similarity), desc(relevance))
  best <- cand[1, ]
  conf <- if (acro && best$acro_hit) "high"
          else if (!acro && best$similarity >= 0.92) "high"
          else if (!acro && best$similarity >= 0.80) "medium"
          else "unresolved"
  resolved <- conf %in% c("high","medium")
  tibble(query_name = raw, matched_name = if (resolved) best$matched_name else NA_character_,
         crossref_funder_id = if (resolved) best$crossref_funder_id else NA_character_,
         reg_country_code = if (resolved) best$country_code else NA_character_,
         reg_country = if (resolved) cc_to_country(best$country_code) else NA_character_,
         relevance = best$relevance, similarity = best$similarity,
         reg_confidence = conf, source = "tei_name")
}
resolve_funder_names <- function(uniq_names, use_api = TRUE, cache_path = FUNDER_NAME_CACHE) {
  uniq_names <- uniq_names[tolower(uniq_names) != "no funding"]
  uniq_names <- unique(uniq_names)
  if (file.exists(cache_path)) {
    name_cache <- readr::read_csv(cache_path, show_col_types = FALSE)
    if (!all(NAME_SCHEMA %in% names(name_cache))) name_cache <- NULL
  } else name_cache <- NULL
  if (is.null(name_cache))
    name_cache <- tibble(query_name = character(), matched_name = character(),
                         crossref_funder_id = character(), reg_country_code = character(),
                         reg_country = character(), relevance = double(),
                         similarity = double(), reg_confidence = character(), source = character())
  need_names <- setdiff(uniq_names, name_cache$query_name)
  if (length(need_names) > 0) {
    if (!use_api) stop("Funder-name cache incomplete but use_api = FALSE (",
                       length(need_names), " names missing)")
    message("Resolving ", length(need_names), " new TEI funder names via API...")
    # Resolve in chunks and write the cache after each one, so an interrupted
    # run keeps the names it already resolved (a full-sample run resolves
    # thousands of names and takes hours).
    chunks <- split(need_names, ceiling(seq_along(need_names) / 100))
    for (ci in seq_along(chunks)) {
      message(sprintf("  funder-name chunk %d/%d (%d names)...",
                      ci, length(chunks), length(chunks[[ci]])))
      new_name   <- map_dfr(chunks[[ci]], resolve_name_registry)
      name_cache <- bind_rows(name_cache, new_name) |> distinct(query_name, .keep_all = TRUE)
      readr::write_csv(name_cache, cache_path)
    }
  }
  # annotate cheap local signals (recomputed, not baked into cache)
  name_cache |>
    filter(query_name %in% uniq_names) |>
    mutate(has_token   = vapply(query_name, has_funder_token, logical(1)),
           cont_cc     = vapply(query_name, contained_cc, character(1)),
           cont_country= cc_to_country(cont_cc),
           bare_country= vapply(query_name, is_country_only, logical(1)),
           resolved_reg= reg_confidence %in% c("high","medium"))
}

# ---- Per-paper funder resolution (the R/4_link_funders_locations.R loop body) ---
# Returns one row: best_funders, best_funder_countries, funding_status.
resolve_paper_funders <- function(journal, ids, tnames, ftxt, id_lookup, name_info) {
  ids <- ids[grepl("^10\\.13039/", ids)]
  raw_tnames <- tnames
  is_no_funding <- any(tolower(raw_tnames) == "no funding")
  tnames <- raw_tnames[tolower(raw_tnames) != "no funding"]

  name_get <- function(nm) {
    r <- name_info[name_info$query_name == nm, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    r[1, ]
  }

  cr_set <- id_lookup |> filter(crossref_funder_id %in% ids)

  # TEI country hints (from standalone bare-country fragments)
  hint_ccs <- character(0)
  for (nm in tnames) {
    r <- name_get(nm); if (is.null(r)) next
    if (isTRUE(r$bare_country)) hint_ccs <- c(hint_ccs, r$cont_cc)
  }
  hint_ccs <- unique(na.omit(hint_ccs))
  tei_names_norm <- norm_name(tnames)

  cr_set <- cr_set |>
    mutate(country_source = ifelse(is.na(country), NA_character_, "crossref_id"))
  if (length(hint_ccs) == 1 && nrow(cr_set)) {
    hint_country <- cc_to_country(hint_ccs)
    for (k in seq_len(nrow(cr_set))) {
      reg_cc   <- cr_set$country_code[k]
      in_tei   <- any(vapply(tei_names_norm,
                             function(t) name_sim(t, norm_name_one(cr_set$funder_name[k])) >= 0.85, logical(1)))
      if (in_tei && (is.na(reg_cc) || !(reg_cc %in% hint_ccs))) {
        cr_set$country_code[k]    <- hint_ccs
        cr_set$country[k]         <- hint_country
        cr_set$country_source[k]  <- "tei_override_crossref"
      }
    }
  }

  te_rows <- list()
  for (nm in tnames) {
    r <- name_get(nm); if (is.null(r)) next
    rl <- tolower(str_trim(nm))
    if (rl %in% junk_terms)                                  next
    if (isTRUE(r$bare_country))                              next
    if (is_journal_like(nm, journal))                       next
    if (!r$resolved_reg && !r$has_token && is.na(r$cont_cc)) next
    if (r$resolved_reg && !is.na(r$reg_country)) {
      ctry <- r$reg_country; csrc <- "tei_registry"; cid <- r$crossref_funder_id
      cn <- r$matched_name; conf <- r$reg_confidence
    } else if (!is.na(r$cont_cc)) {
      ctry <- r$cont_country; csrc <- "tei_contained"
      cid  <- if (r$resolved_reg) r$crossref_funder_id else NA_character_
      cn   <- nm; conf <- "medium"
    } else if (is_eu_name(nm)) {
      ctry <- "European Union"; csrc <- "tei_contained"; cid <- r$crossref_funder_id
      cn <- nm; conf <- "medium"
    } else {
      ctry <- NA_character_; csrc <- NA_character_
      cid  <- if (r$resolved_reg) r$crossref_funder_id else NA_character_
      cn   <- if (r$resolved_reg && !is.na(r$matched_name)) r$matched_name else nm
      conf <- "low"
    }
    te_rows[[length(te_rows)+1]] <- tibble(name = cn, crossref_funder_id = cid,
                                           country = ctry, country_source = csrc,
                                           source = "tei_name", conf = conf)
  }
  te_set <- if (length(te_rows)) bind_rows(te_rows) else
    tibble(name=character(), crossref_funder_id=character(), country=character(),
           country_source=character(), source=character(), conf=character())

  cr_part <- if (nrow(cr_set)) tibble(name = cr_set$funder_name, crossref_funder_id = cr_set$crossref_funder_id,
                                      country = cr_set$country, country_source = cr_set$country_source,
                                      source = "crossref_id", conf = "high") else te_set[0, ]
  combined <- bind_rows(cr_part, te_set)
  pref_map <- c(crossref_id = 1, tei_override_crossref = 1, tei_registry = 2, tei_contained = 3)
  combined$pref <- ifelse(is.na(combined$country_source), 9, pref_map[combined$country_source])
  best <- cluster_funders(combined)

  has_cr <- nrow(cr_set) > 0; has_te <- nrow(te_set) > 0
  matched <- FALSE
  cr_ids <- na.omit(cr_set$crossref_funder_id)
  te_ids <- na.omit(te_set$crossref_funder_id)
  if (length(intersect(cr_ids, te_ids)) > 0) matched <- TRUE
  if (!matched && has_cr && has_te) {
    for (a in cr_set$funder_name) for (b in te_set$name)
      if (name_sim(norm_name_one(a), norm_name_one(b)) >= 0.80) { matched <- TRUE; break }
  }
  recon <- if (is_no_funding && !has_cr && !has_te) "no_funding"
           else if (!has_cr && !has_te) "none"
           else if (matched) "both"
           else if (has_cr && !has_te) "crossref_only"
           else if (!has_cr && has_te) "tei_only"
           else "conflict"

  has_funding_text <- !is.na(ftxt) && nchar(str_trim(ftxt)) > 15 &&
                      tolower(str_trim(ftxt)) != "no funding"
  fstatus <- if (recon == "no_funding") "no funding"
             else if (nrow(best) > 0) "funded"
             else if (has_funding_text && !is_no_funding) "funded"
             else "unknown"

  if (fstatus == "no funding") {
    best_funders <- "no funding"; best_countries <- NA_character_
  } else if (nrow(best) > 0) {
    best_funders   <- paste(best$name, collapse = " || ")
    ucs <- unique(na.omit(best$country))
    best_countries <- if (length(ucs)) paste(ucs, collapse = " || ") else NA_character_
  } else {
    best_funders <- NA_character_; best_countries <- NA_character_
  }
  tibble(best_funders = best_funders, best_funder_countries = best_countries,
         funding_status = fstatus, reconciliation = recon)
}
