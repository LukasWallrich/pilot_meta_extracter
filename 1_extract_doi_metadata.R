# DOI Metadata Extraction ------------------------------------------------------
# Focuses on OpenAlex for all metadata, journal metrics, and location data.

if (!require("pacman")) install.packages("pacman")
if (!require("pak", quietly = TRUE)) install.packages("pak")
if (!require("sjrdata", quietly = TRUE)) pak::pak("ikashnitsky/sjrdata")
pacman::p_load(
  httr,
  countrycode,
  dplyr,
  sjrdata
)

# Optional TOP Factor data -----------------------------------------------------
if (file.exists("top_factor_data.RData")) {
  load("top_factor_data.RData")
} else {
  message("TOP Factor data not found. Run download_top_factor.R first")
  top_factor_data <- NULL
}

# Constants -------------------------------------------------------------------
OA_USER_AGENT <- "mailto:lukas.wallrich@gmail.com"
data("sjr_journals", package = "sjrdata")

# Utility helpers -------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_country_name <- function(iso2) {
  if (is.null(iso2) || length(iso2) == 0) return(NA_character_)
  tryCatch(
    countrycode::countrycode(toupper(iso2), origin = "iso2c", destination = "country.name"),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
}

as_numeric_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

is_generic_institution <- function(name) {
  if (is.null(name) || length(name) == 0) return(TRUE)
  normalized <- tolower(trimws(name))
  if (normalized == "") return(TRUE)
  if (grepl("(university|universit|college|institute|academy|hospital|centre for|center for)", normalized)) {
    return(FALSE)
  }
  grepl(
    "^(department|dept\\.|school|faculty|centre|center|division|unit|program|programme|clinic|laboratory|lab|research group|office)\\b",
    normalized
  )
}

format_location_label <- function(location) {
  if (is.null(location)) return(NA_character_)
  pieces <- Filter(
    function(x) !is.null(x) && length(x) > 0 && !is.na(x) && trimws(x) != "",
    c(
      location$display_name,
      location$city,
      location$region,
      location$country
    )
  )
  pieces <- unique(pieces)
  if (length(pieces) == 0) return(NA_character_)
  paste(pieces, collapse = ", ")
}

location_country <- function(location) {
  if (is.null(location)) return(NA_character_)
  value <- location$country %||% NA_character_
  if (is.na(value) || !nzchar(value)) return(NA_character_)
  value
}

# OpenAlex requests -----------------------------------------------------------
fetch_openalex_work <- function(doi) {
  url <- paste0("https://api.openalex.org/works/doi:", doi)
  response <- tryCatch(
    httr::GET(url, httr::add_headers(`User-Agent` = OA_USER_AGENT)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) {
    warning(sprintf("OpenAlex work fetch failed for %s (status %s)", doi, httr::status_code(response %||% NA)))
    return(NULL)
  }
  tryCatch(httr::content(response, as = "parsed"), error = function(e) NULL)
}

normalize_author_id <- function(author_id) {
  if (is.null(author_id) || length(author_id) == 0 || is.na(author_id)) return(NA_character_)
  if (startsWith(author_id, "https://openalex.org/")) {
    sub("https://openalex.org/", "", author_id, fixed = TRUE)
  } else {
    author_id
  }
}

fetch_author_profile <- function(author_id) {
  author_id <- normalize_author_id(author_id)
  if (is.na(author_id)) return(NULL)
  url <- paste0("https://api.openalex.org/authors/", author_id)
  response <- tryCatch(
    httr::GET(url, httr::add_headers(`User-Agent` = OA_USER_AGENT)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) return(NULL)
  tryCatch(httr::content(response, as = "parsed"), error = function(e) NULL)
}

# Extract funding (grants) info from OpenAlex work object
get_openalex_funding <- function(work) {
  if (is.null(work)) return(list(funders = character(), funder_ids = character(), award_ids = character(), grants = NULL))
  
  grants <- work$grants %||% list()
  if (length(grants) == 0) return(list(funders = character(), funder_ids = character(), award_ids = character(), grants = NULL))
  
  # unclear if this is deprecated, no documentation on it, but has ror id for funders
  # currently not included
  # funders_work <- work$funders %||% list()

  funders <- c()
  funder_ids <- c()
  award_ids <- c()
  for (g in grants) {
    fid <- g$funder %||% NA_character_
    fname <- g$funder_display_name %||% NA_character_
    aid <- g$award_id %||% NA_character_
    
    if (!is.na(fname) && nzchar(fname)) funders <- c(funders, fname)
    if (!is.na(fname) && nzchar(fname)) funder_ids <- c(funder_ids, fid)
    if (!is.na(aid) && nzchar(aid)) award_ids <- c(award_ids, aid)
  }
  
  list(
    funders = if (length(funders) > 0) unique(funders) else character(),
    funder_ids = if (length(funder_ids) > 0) unique(funder_ids) else character(),
    award_ids = if (length(award_ids) > 0) unique(award_ids) else character()#,
    # grants = grants # keep raw
  )
}

# Location handling -----------------------------------------------------------
build_location_details <- function(inst, source) {
  if (is.null(inst)) return(NULL)
  geo <- inst$geo %||% list()
  iso2 <- inst$country_code %||% NA_character_
  list(
    id = inst$id %||% NA_character_,
    display_name = inst$display_name %||% NA_character_,
    type = inst$type %||% NA_character_,
    country_code = if (!is.null(iso2)) toupper(iso2) else NA_character_,
    country = safe_country_name(iso2),
    city = geo$city %||% NA_character_,
    region = geo$region %||% NA_character_,
    latitude = as_numeric_or_na(geo$latitude),
    longitude = as_numeric_or_na(geo$longitude),
    source = source
  )
}

select_institution_from_list <- function(institutions, source) {
  if (is.null(institutions) || length(institutions) == 0) return(NULL)
  for (inst in institutions) {
    name <- inst$display_name %||% ""
    if (is_generic_institution(name)) next
    location <- build_location_details(inst, source)
    if (!is.null(location)) return(location)
  }
  NULL
}

get_first_authorship <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return(NULL)
  for (authorship in authorships) {
    if ((authorship$author_position %||% NA_character_) == "first") return(authorship)
  }
  authorships[[1]]
}

get_last_authorship <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return(NULL)
  for (i in seq_along(authorships)) {
    idx <- length(authorships) - i + 1
    authorship <- authorships[[idx]]
    if ((authorship$author_position %||% NA_character_) == "last") return(authorship)
  }
  authorships[[length(authorships)]]
}

profile_institution_candidates <- function(profile) {
  candidates <- list()
  if (!is.null(profile$last_known_institution)) {
    candidates <- c(candidates, list(profile$last_known_institution))
  }
  if (!is.null(profile$last_known_institutions) && length(profile$last_known_institutions) > 0) {
    candidates <- c(candidates, profile$last_known_institutions)
  }
  if (!is.null(profile$affiliations) && length(profile$affiliations) > 0) {
    for (aff in profile$affiliations) {
      inst <- aff$institution %||% aff
      candidates <- c(candidates, list(inst))
    }
  }
  Filter(function(x) !is.null(x), candidates)
}

get_article_location_details <- function(work) {
  authorships <- work$authorships %||% list()
  first_authorship <- get_first_authorship(authorships)
  if (is.null(first_authorship)) return(NULL)
  select_institution_from_list(first_authorship$institutions, "article")
}

get_author_profile_location <- function(authorship) {
  if (is.null(authorship)) return(NULL)
  profile <- fetch_author_profile(authorship$author$id)
  if (is.null(profile)) return(NULL)
  candidates <- profile_institution_candidates(profile)
  for (inst in candidates) {
    name <- inst$display_name %||% ""
    if (is_generic_institution(name)) next
    location <- build_location_details(inst, "author-profile")
    if (!is.null(location)) return(location)
  }
  NULL
}

get_first_author_location_details <- function(work) {
  authorships <- work$authorships %||% list()
  authorship <- get_first_authorship(authorships)
  get_author_profile_location(authorship)
}

get_last_author_location_details <- function(work) {
  authorships <- work$authorships %||% list()
  authorship <- get_last_authorship(authorships)
  if (is.null(authorship)) return(NULL)
  get_author_profile_location(authorship)
}

# ----------------- Crossref helpers: fetch + extract funding & COI --------------
# polite Crossref fetch for DOI -> returns Crossref 'message' or NULL
fetch_crossref_work <- function(doi, email = OA_USER_AGENT) {
  if (is.null(doi) || !nzchar(doi)) return(NULL)
  url <- paste0("https://api.crossref.org/works/", URLencode(doi, reserved = TRUE))
  headers <- httr::add_headers(`User-Agent` = paste0("doi-metadata-script (", email, ")"))
  resp <- tryCatch(
    httr::GET(url, headers, httr::accept_json()),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  content <- tryCatch(httr::content(resp, as = "parsed", simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(content) || is.null(content$message)) return(NULL)
  content$message
}

get_crossref_funding = function(msg) {
  if (is.null(msg)) return(list(funders = character(), funder_ids = character(), award_ids = character(), raw = NULL))
  funder_nodes <- msg$funder %||% msg$funding %||% list()
  if (length(funder_nodes) == 0) return(list(funders = character(), funder_ids = character(), award_ids = character(), raw = NULL))
  
  funders <- character()
  funder_ids <- character()
  award_ids <- character()
  
  for (f in funder_nodes) {
    name <- f$name %||% f$funder_name %||% NA_character_
    fid <- f$DOI %||% f$funder_id %||% f$`@id` %||% NA_character_
    aw <- f$award %||% f$awards %||% character()
    if (!is.na(name) && nzchar(name)) funders <- c(funders, name)
    if (!is.na(fid) && nzchar(fid)) funder_ids <- c(funder_ids, fid)
    if (!is.null(aw) && length(aw) > 0) award_ids <- c(award_ids, unlist(aw, use.names = FALSE))
  }
  
  list(
    funders = if (length(funders) > 0) unique(funders) else character(),
    funder_ids = if (length(funder_ids) > 0) unique(funder_ids) else character(),
    award_ids = if (length(award_ids) > 0) unique(award_ids) else character(),
    raw = funder_nodes
  )
}

# Heuristic: find COI/competing interests in Crossref message fields
get_crossref_coi <- function(msg) {
  if (is.null(msg)) return(NA_character_)
  # shallow recursive scan for COI phrases
  found <- character()
  limit <- 2000L
  counter <- 0L
  recurse <- function(v) {
    counter <<- counter + 1L
    if (counter > limit) return(NULL)
    if (is.null(v)) return(NULL)
    if (is.atomic(v) && is.character(v)) {
      for (el in v) {
        if (!is.na(el) && nzchar(el)) {
          lower <- tolower(el)
          # based on search keys in coding form V4 and some trial and error -- FIXME
          if (grepl("(conflict(s)? of interest(s)?|conflicting interests?|no conflicts?|competing interests?|disclos|declar)", lower, perl = TRUE)) {
            found <<- c(found, trimws(el))
          }
        }
      }
    } else if (is.list(v)) {
      for (sub in v) {
        recurse(sub)
        if (counter > limit) break
      }
    }
    invisible(NULL)
  }
  
  recurse(msg)
  if (length(found) == 0) return(NA_character_)
  paste(unique(found), collapse = " || ")
}



# Metadata helpers ------------------------------------------------------------
get_basic_metadata <- function(work) {
  source <- work$primary_location$source %||% list()
  if (length(source) == 0) {
    source <- work$primary_location %||% list()
  }
  if (length(source) == 0) {
    source <- work$host_venue %||% list()
  }

  issn_candidates <- source$issn %||% character()
  if (length(issn_candidates) > 0 && is.list(issn_candidates)) {
    issn_candidates <- unlist(issn_candidates, use.names = FALSE)
  }
  issn_candidates <- issn_candidates[!is.na(issn_candidates) & nzchar(issn_candidates)]
  if (length(issn_candidates) == 0) {
    issn_candidates <- source$issn_l %||% character()
  }
  issn_candidates <- unique(issn_candidates)

  list(
    year = work$publication_year %||% NA_integer_,
    journal = source$display_name %||% NA_character_,
    issn_primary = if (length(issn_candidates) > 0) issn_candidates[1] else NA_character_,
    issn_all = issn_candidates
  )
}

get_journal_sjr <- function(issn_all, year) {
  if (is.null(issn_all) || length(issn_all) == 0 || is.na(year)) return(NA_real_)
  tryCatch({
    for (issn in issn_all) {
      if (is.na(issn)) next
      issn_clean <- gsub("[^0-9]", "", issn)
      if (issn_clean == "") next

      journal_data <- sjr_journals %>%
        filter(year == !!year) %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn)))
      if (nrow(journal_data) > 0) return(journal_data$sjr[1])

      journal_data <- sjr_journals %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn))) %>%
        mutate(year_diff = abs(year - !!year)) %>%
        arrange(year_diff)
      if (nrow(journal_data) > 0) return(journal_data$sjr[1])
    }
    NA_real_
  }, error = function(e) NA_real_)
}

get_top_factor <- function(journal_name, issn) {
  if (is.null(top_factor_data)) return(NA_real_)
  tryCatch({
    if (!is.na(issn) && length(issn) > 0) {
      issn_clean <- gsub("-", "", issn)
      match <- top_factor_data %>%
        filter(gsub("-", "", Issn) == issn_clean | gsub("-", "", Eissn) == issn_clean)
      if (nrow(match) > 0) return(match$Total[1])
    }
    if (!is.na(journal_name) && length(journal_name) > 0) {
      match <- top_factor_data %>%
        filter(tolower(Journal) == tolower(journal_name))
      if (nrow(match) > 0) return(match$Total[1])
    }
    NA_real_
  }, error = function(e) NA_real_)
}

append_location_columns <- function(record, prefix, location) {
  record[[paste0(prefix, "_id")]] <- location$id
  record[[paste0(prefix, "_display_name")]] <- location$display_name
  record[[paste0(prefix, "_type")]] <- location$type
  record[[paste0(prefix, "_country_code")]] <- location$country_code
  record[[paste0(prefix, "_country")]] <- location$country
  record[[paste0(prefix, "_city")]] <- location$city
  record[[paste0(prefix, "_region")]] <- location$region
  record[[paste0(prefix, "_latitude")]] <- location$latitude
  record[[paste0(prefix, "_longitude")]] <- location$longitude
  record[[paste0(prefix, "_source")]] <- location$source
  record
}

# Helper to create empty location object
empty_location <- function() {
  list(
    id = NA_character_,
    display_name = NA_character_,
    type = NA_character_,
    country_code = NA_character_,
    country = NA_character_,
    city = NA_character_,
    region = NA_character_,
    latitude = NA_real_,
    longitude = NA_real_,
    source = NA_character_
  )
}

# Public API ------------------------------------------------------------------
extract_doi_metadata <- function(doi, return_location_details = TRUE) {
  message("Processing ", doi)
  
  # Try to fetch OpenAlex work
  work <- fetch_openalex_work(doi)
  
  # If OpenAlex fetch failed, return an empty row with all columns present
  if (is.null(work)) {
    empty <- data.frame(
      doi = doi,
      year = NA_integer_,
      journal = NA_character_,
      issn = NA_character_,
      sjr = NA_real_,
      top_factor = NA_real_,
      article_location = NA_character_,
      first_author_location = NA_character_,
      last_author_location = NA_character_,
      primary_topic_display_name = NA_character_,
      primary_topic_id = NA_character_,
      funding_funders_openalex = NA_character_,
      funding_funder_ids_openalex = NA_character_,
      funding_award_ids_openalex = NA_character_,
      funding_funders_crossref = NA_character_,
      funding_funder_ids_crossref = NA_character_,
      funding_award_ids_crossref = NA_character_,
      coi_crossref = NA_character_,
      stringsAsFactors = FALSE
    )
    empty$primary_topic <- I(list(NULL))
    return(empty)
  }
  
  # --- basic/journal metadata via OpenAlex work object ---
  basic <- get_basic_metadata(work)
  sjr <- get_journal_sjr(basic$issn_all, basic$year)
  top_factor <- get_top_factor(basic$journal, basic$issn_primary)
  
  # --- location details ---
  article_location <- get_article_location_details(work)
  first_author_location <- get_first_author_location_details(work)
  last_author_location <- get_last_author_location_details(work)
  
  article_country <- location_country(article_location)
  first_author_country <- location_country(first_author_location)
  last_author_country <- location_country(last_author_location)
  
  article_label <- format_location_label(article_location)
  first_author_label <- format_location_label(first_author_location)
  last_author_label <- format_location_label(last_author_location)
  
  # -------------------- Extract OpenAlex funding -----------------------
  # REDUNDANT > openalex uses crossref, crossref output seems more complete
  # only relevant if we want to use the open alex ids for funders
  oa_f <- get_openalex_funding(work)
  funding_funders_openalex <- if (length(oa_f$funders) > 0) paste(oa_f$funders, collapse = " || ") else NA_character_
  funding_funder_ids_openalex <- if (length(oa_f$funder_ids) > 0) paste(oa_f$funder_ids, collapse = " || ") else NA_character_
  funding_award_ids_openalex <- if (length(oa_f$award_ids) > 0) paste(oa_f$award_ids, collapse = " || ") else NA_character_
  # funding_grants_openalex <- oa_f$grants %||% NULL # raw info
  
  # -------------------- Attempt Crossref and extract funding & COI & abstract--------------
  cr_msg <- fetch_crossref_work(doi, email = OA_USER_AGENT)
  if (!is.null(cr_msg)) {
    # existing crossref extractor (keeps previous behavior)
    cr_f <- get_crossref_funding(cr_msg)
    funding_funders_crossref <- if (length(cr_f$funders) > 0) paste(cr_f$funders, collapse = " || ") else NA_character_
    funding_funder_ids_crossref <- if (length(cr_f$funder_ids) > 0) paste(cr_f$funder_ids, collapse = " || ") else NA_character_
    funding_award_ids_crossref <- if (length(cr_f$award_ids) > 0) paste(cr_f$award_ids, collapse = " || ") else NA_character_
    coi_crossref <- get_crossref_coi(cr_msg)
    # funding_grants_crossref <- cr_f$raw %||% NULL
    # FIXME abstract needed or already in data?
    cr_abstract = if (!is.null(cr_msg$abstract)) cr_msg$abstract else NA_character_
  } else {
    funding_funders_crossref <- NA_character_
    funding_funder_ids_crossref <- NA_character_
    funding_award_ids_crossref <- NA_character_
    coi_crossref <- NA_character_
    # funding_grants_crossref <- NULL
    cr_abstract = NA_character_
  }
  # -------------------------------------------------------------------------
  
  # --- assemble record (text columns) ---
  record <- data.frame(
    doi = doi,
    year = basic$year,
    journal = basic$journal,
    issn = basic$issn_primary,
    sjr = sjr,
    top_factor = top_factor,
    article_location = article_country,
    first_author_location = first_author_country,
    last_author_location = last_author_country,
    primary_topic_display_name = work$primary_topic$display_name %||% NA_character_,
    primary_topic_id = work$primary_topic$id %||% NA_character_,
    # OpenAlex columns
    funding_funders_openalex = funding_funders_openalex,
    funding_funder_ids_openalex = funding_funder_ids_openalex,
    funding_award_ids_openalex = funding_award_ids_openalex,
    # Crossref columns
    funding_funders_crossref = funding_funders_crossref,
    funding_funder_ids_crossref = funding_funder_ids_crossref,
    funding_award_ids_crossref = funding_award_ids_crossref,
    coi_crossref = coi_crossref,
    cr_abstract = cr_abstract,
    stringsAsFactors = FALSE
  )
  
  # keep structured objects in list-columns
  record$primary_topic <- I(list(work$primary_topic %||% NULL))
  record$article_location_label <- article_label
  record$first_author_location_label <- first_author_label
  record$last_author_location_label <- last_author_label
  
  # add raw grants as list-columns (preserve structure via R list-column)
  # record$funding_grants_openalex <- I(list(funding_grants_openalex))
  # record$funding_grants_crossref <- I(list(funding_grants_crossref))
  
  # append full location detail columns if requested
  if (return_location_details) {
    if (is.null(article_location)) article_location <- empty_location()
    if (is.null(first_author_location)) first_author_location <- empty_location()
    if (is.null(last_author_location)) last_author_location <- empty_location()
    
    record <- append_location_columns(record, "article_location_detail", article_location)
    record <- append_location_columns(record, "first_author_location_detail", first_author_location)
    record <- append_location_columns(record, "last_author_location_detail", last_author_location)
  }
  
  record
}

process_dois <- function(dois, return_location_details = TRUE, output_file = NULL) {
  results <- lapply(dois, function(doi) {
    tryCatch(
      extract_doi_metadata(doi, return_location_details = return_location_details),
      error = function(e) {
        warning(sprintf("Extraction failed for %s: %s", doi, e$message))
        fallback <- data.frame(
          doi = doi,
          year = NA_integer_,
          journal = NA_character_,
          issn = NA_character_,
          sjr = NA_real_,
          top_factor = NA_real_,
          article_location = NA_character_,
          first_author_location = NA_character_,
          last_author_location = NA_character_,
          primary_topic_display_name = NA_character_,
          primary_topic_id = NA_character_,
          funding_funders_openalex = NA_character_,
          funding_funder_ids_openalex = NA_character_,
          funding_award_ids_openalex = NA_character_,
          funding_funders_crossref = NA_character_,
          funding_funder_ids_crossref = NA_character_,
          funding_award_ids_crossref = NA_character_,
          coi_crossref = NA_character_,
          stringsAsFactors = FALSE
        )
        fallback$primary_topic <- I(list(NULL))
        fallback
      }
    )
  })
  
  df <- bind_rows(results)
  
  # optionally write to disk: CSV (text columns) + RDS (preserve list-columns)
  if (!is.null(output_file) && nzchar(output_file)) {
    outdir <- dirname(output_file)
    if (!dir.exists(outdir) && outdir != ".") dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      # write CSV (list-columns will be coerced to string; RDS preserves them)
      write.csv(df, file = output_file, row.names = FALSE, na = "")
      rds_path <- sub("\\.csv$", ".rds", output_file)
      saveRDS(df, file = rds_path)
      message("Wrote results to: ", output_file, " and ", rds_path)
    }, error = function(e) {
      warning("Failed to write output file: ", e$message)
    })
  }
  
  df
}
