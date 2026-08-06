escape_html <- function(x) {
  if (is.null(x)) {
    return("")
  }
  out <- ifelse(is.na(x), "", as.character(x))
  out <- gsub("&", "&amp;", out, fixed = TRUE)
  out <- gsub("<", "&lt;", out, fixed = TRUE)
  out <- gsub(">", "&gt;", out, fixed = TRUE)
  out <- gsub('"', "&quot;", out, fixed = TRUE)
  out <- gsub("'", "&#39;", out, fixed = TRUE)
  out
}

create_datatable_html <- function(data, table_id, allow_html = character()) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }
  if (!nzchar(table_id)) {
    stop("`table_id` must be a non-empty string")
  }
  headers <- names(data)
  header_html <- paste0(
    "<thead><tr>",
    paste(sprintf("<th>%s</th>", escape_html(headers)), collapse = ""),
    "</tr></thead>"
  )
  if (nrow(data) == 0) {
    body_html <- "<tbody></tbody>"
  } else {
    formatted <- data
    for (col in headers) {
      values <- formatted[[col]]
      values[is.na(values)] <- ""
      if (col %in% allow_html) {
        formatted[[col]] <- values
      } else {
        formatted[[col]] <- escape_html(values)
      }
    }
    row_html <- apply(
      formatted,
      1,
      function(row) paste0(
        "<tr>",
        paste(sprintf("<td>%s</td>", row), collapse = ""),
        "</tr>"
      )
    )
    body_html <- paste0("<tbody>", paste(row_html, collapse = "\n"), "</tbody>")
  }
  sprintf(
    '<table id="%s" class="display nowrap stripe" style="width:100%%">%s%s</table>',
    table_id,
    header_html,
    body_html
  )
}

render_datatable_script <- function(table_id, options_js = "{}") {
  if (!nzchar(options_js)) {
    options_js <- "{}"
  }
  sprintf(
    "<script>$(document).ready(function(){ if (!$.fn.DataTable.isDataTable('#%s')) { $('#%s').DataTable(%s); }});</script>",
    table_id,
    table_id,
    options_js
  )
}

fetch_openalex_articles <- function(journal, source_id, year = 2024) {
  if (missing(source_id) || is.na(source_id) || !nzchar(source_id)) {
    stop("`source_id` must be provided for fetch_openalex_articles().")
  }
  base_url <- "https://api.openalex.org/works"
  cursor <- "*"
  results <- list()
  mailto <- Sys.getenv("OPENALEX_EMAIL")

  repeat {
    query <- list(
      filter = sprintf(
        'primary_location.source.id:%s,from_publication_date:%d-01-01,to_publication_date:%d-12-31,type:journal-article',
        source_id,
        year,
        year
      ),
      per_page = 200,
      cursor = cursor,
      select = "id,doi,display_name,publication_year,publication_date,primary_topic"
    )
    if (nzchar(mailto)) {
      query$mailto <- mailto
    }

    response <- httr::RETRY(
      verb = "GET",
      url = base_url,
      query = query,
      pause_base = 2,
      pause_cap = 16,
      times = 6
    )
    httr::stop_for_status(response)
    payload <- httr::content(response, as = "parsed", type = "application/json")
    if (!is.null(payload$results) && length(payload$results) > 0) {
      results <- c(results, payload$results)
    }
    cursor <- payload$meta$next_cursor
    if (is.null(cursor)) {
      break
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      journal = character(0),
      openalex_id = character(0),
      title = character(0),
      publication_year = integer(0),
      publication_date = character(0),
      doi = character(0),
      primary_field = character(0),
      primary_subfield = character(0),
      primary_topic = character(0),
      primary_topic_score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  safe_chr <- function(value) {
    if (is.null(value) || length(value) == 0) return(NA_character_)
    as.character(value)
  }
  safe_int <- function(value) {
    if (is.null(value) || length(value) == 0) return(NA_integer_)
    as.integer(value)
  }
  safe_num <- function(value) {
    if (is.null(value) || length(value) == 0) return(NA_real_)
    as.numeric(value)
  }
  extract_nested <- function(item, ...) {
    keys <- list(...)
    value <- item
    for (key in keys) {
      if (is.null(value[[key]])) {
        return(NULL)
      }
      value <- value[[key]]
    }
    value
  }

  n <- length(results)
  data.frame(
    journal = rep(journal, n),
    openalex_id = vapply(results, function(x) safe_chr(x$id), character(1)),
    title = vapply(results, function(x) safe_chr(x$display_name), character(1)),
    publication_year = vapply(results, function(x) safe_int(x$publication_year), integer(1)),
    publication_date = vapply(results, function(x) safe_chr(x$publication_date), character(1)),
    doi = vapply(results, function(x) safe_chr(x$doi), character(1)),
    primary_field = vapply(results, function(x) safe_chr(extract_nested(x, "primary_topic", "field", "display_name")), character(1)),
    primary_subfield = vapply(results, function(x) safe_chr(extract_nested(x, "primary_topic", "subfield", "display_name")), character(1)),
    primary_topic = vapply(results, function(x) safe_chr(extract_nested(x, "primary_topic", "display_name")), character(1)),
    primary_topic_score = vapply(results, function(x) safe_num(extract_nested(x, "primary_topic", "score")), numeric(1)),
    stringsAsFactors = FALSE
  )
}

fetch_all_journals <- function(journal_table, year = 2024) {
  required_columns <- c("journal", "source_id")
  if (!all(required_columns %in% names(journal_table))) {
    stop("`journal_table` must contain `journal` and `source_id` columns.")
  }
  pieces <- lapply(
    seq_len(nrow(journal_table)),
    function(i) {
      fetch_openalex_articles(
        journal = journal_table$journal[i],
        source_id = journal_table$source_id[i],
        year = year
      )
    }
  )
  if (length(pieces) == 0) {
    return(data.frame(
      journal = character(0),
      openalex_id = character(0),
      title = character(0),
      publication_year = integer(0),
      publication_date = character(0),
      doi = character(0),
      primary_field = character(0),
      primary_subfield = character(0),
      primary_topic = character(0),
      primary_topic_score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, pieces)
}

prepare_article_tables <- function(article_data, journal_focus) {
  if (!is.data.frame(article_data)) {
    stop("`article_data` must be a data frame")
  }
  if (!is.data.frame(journal_focus)) {
    stop("`journal_focus` must be a data frame")
  }
  if (nrow(article_data) == 0) {
    return(list(
      articles_enriched = article_data,
      subfield_counts = data.frame(journal = character(0), primary_subfield = character(0), article_count = integer(0), stringsAsFactors = FALSE),
      article_table = data.frame(
        Journal = character(0),
        Title = character(0),
        `Publication date` = character(0),
        DOI = character(0),
        `Primary field` = character(0),
        `Primary subfield` = character(0),
        `Primary topic` = character(0),
        `Topic score` = character(0),
        `Journal focus` = character(0),
        `Focus check` = character(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    ))
  }

  match_idx <- match(article_data$journal, journal_focus$journal)
  focus_subfields <- journal_focus$focus_subfields[match_idx]
  focus_lower <- journal_focus$focus_subfields_lower[match_idx]
  focus_summary <- journal_focus$focus_summary[match_idx]

  subfield_lower <- tolower(article_data$primary_subfield)
  focus_match <- mapply(
    function(focus_list, subfield) {
      if (is.null(focus_list) || length(focus_list) == 0 || all(is.na(focus_list)) || is.na(subfield)) {
        return(NA)
      }
      subfield %in% focus_list
    },
    focus_lower,
    subfield_lower,
    SIMPLIFY = TRUE
  )
  flag_diff <- ifelse(is.na(focus_match), NA, !focus_match)
  flag_label <- ifelse(is.na(flag_diff), "Unknown", ifelse(flag_diff, "Different", "Matches"))

  doi_values <- ifelse(is.na(article_data$doi), "", article_data$doi)
  doi_href <- ifelse(
    nzchar(doi_values) & !grepl("^https?://", doi_values, ignore.case = TRUE),
    paste0("https://doi.org/", doi_values),
    doi_values
  )
  doi_link <- ifelse(
    nzchar(doi_values),
    sprintf('<a href="%s" target="_blank">%s</a>', escape_html(doi_href), escape_html(doi_values)),
    ""
  )

  openalex_values <- ifelse(is.na(article_data$openalex_id), "", article_data$openalex_id)
  title_values <- ifelse(is.na(article_data$title), "", article_data$title)
  title_link <- ifelse(
    nzchar(openalex_values),
    sprintf('<a href="%s" target="_blank">%s</a>', escape_html(openalex_values), escape_html(title_values)),
    escape_html(title_values)
  )

  topic_score_fmt <- ifelse(
    is.na(article_data$primary_topic_score),
    "",
    sprintf("%.3f", article_data$primary_topic_score)
  )

  enriched <- article_data
  enriched$focus_subfields <- focus_subfields
  enriched$focus_summary <- focus_summary
  enriched$focus_match <- focus_match
  enriched$flag_diff <- flag_diff
  enriched$flag_label <- flag_label

  primary_subfield_display <- ifelse(
    is.na(article_data$primary_subfield) | !nzchar(article_data$primary_subfield),
    "Missing subfield",
    article_data$primary_subfield
  )
  subfield_counts <- aggregate(
    list(article_count = rep(1L, nrow(article_data))),
    by = list(journal = article_data$journal, primary_subfield = primary_subfield_display),
    FUN = sum
  )
  subfield_counts <- subfield_counts[order(subfield_counts$journal, -subfield_counts$article_count, subfield_counts$primary_subfield), , drop = FALSE]

  article_table <- data.frame(
    Journal = article_data$journal,
    Title = title_link,
    `Publication date` = ifelse(is.na(article_data$publication_date), "", article_data$publication_date),
    DOI = doi_link,
    `Primary field` = article_data$primary_field,
    `Primary subfield` = article_data$primary_subfield,
    `Primary topic` = article_data$primary_topic,
    `Topic score` = topic_score_fmt,
    `Journal focus` = focus_summary,
    `Focus check` = flag_label,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  list(
    articles_enriched = enriched,
    subfield_counts = subfield_counts,
    article_table = article_table
  )
}
