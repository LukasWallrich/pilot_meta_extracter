#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(metacheck)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

parse_args <- function(args) {
  defaults <- list(
    tei_dir = "tei",
    out_csv = "pilot_keyword_screening.csv",
    out_md = "pilot_keyword_screening_report.md",
    max_files = NA_integer_
  )

  if (length(args) == 0) return(defaults)

  for (arg in args) {
    if (!str_detect(arg, "^--")) next
    kv <- str_split_fixed(sub("^--", "", arg), "=", 2)
    key <- kv[, 1]
    val <- kv[, 2]
    if (val == "") next

    if (key == "tei-dir") defaults$tei_dir <- val
    if (key == "out-csv") defaults$out_csv <- val
    if (key == "out-md") defaults$out_md <- val
    if (key == "max-files") defaults$max_files <- as.integer(val)
  }

  defaults
}

list_tei_files <- function(dir = "tei") {
  list.files(dir, pattern = "\\.tei\\.xml$", full.names = TRUE)
}

extract_doi_from_filename <- function(path) {
  stem <- basename(path) |>
    str_remove("\\.tei\\.xml$") |>
    str_remove("\\.xml$") |>
    utils::URLdecode()

  lower <- str_to_lower(stem)
  prefix <- str_split_fixed(lower, "_-_", 2)[, 1]

  # Expected filename DOI encoding uses '-' or '_' where DOI has '/'.
  prefix <- str_replace(prefix, "^(10\\.[0-9]{4,9})_", "\\1/")
  prefix <- str_replace(prefix, "^(10\\.[0-9]{4,9})-", "\\1/")

  doi <- str_extract(prefix, "10\\.[0-9]{4,9}/[-._;()/:a-z0-9]+")
  doi <- str_replace(doi %||% "", "[\\.,;:]+$", "")

  if (nchar(doi) == 0) {
    return(NA_character_)
  }

  doi
}

collect_full_text <- function(papers) {
  paper_list <- if (inherits(papers, "scivrs_paperlist")) {
    unname(papers)
  } else {
    list(papers)
  }

  map_dfr(paper_list, function(p) {
    txt <- p$text
    # A TEI GROBID could not parse yields no text table at all.
    if (is.null(txt) || !is.data.frame(txt) || nrow(txt) == 0) {
      return(tibble())
    }
    sec <- p$section
    if (is.data.frame(sec) && nrow(sec) > 0) {
      txt <- left_join(txt, sec[, c("section_id", "section_type")], by = "section_id")
    } else {
      txt$section_type <- NA_character_
    }
    tibble(
      text    = txt$text,
      section = txt$section_type,
      file    = normalizePath(p$info$file_name %||% p$paper_id,
                              winslash = "/", mustWork = FALSE)
    )
  })
}

build_markdown_report <- function(
  out_md,
  tei_dir,
  result,
  keyword_summary,
  section_summary,
  keyword_section_summary,
  unresolved_doi
) {
  total <- nrow(result)
  included <- sum(result$include)
  excluded <- total - included

  fmt_table <- function(df) {
    if (nrow(df) == 0) {
      return(c("_No hits found._", ""))
    }

    header <- paste0("| ", paste(names(df), collapse = " | "), " |")
    sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
    rows <- apply(df, 1, function(x) paste0("| ", paste(as.character(x), collapse = " | "), " |"))
    c(header, sep, rows, "")
  }

  lines <- c(
    "# Pilot Keyword Screening Report",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Input directory: `", tei_dir, "`"),
    "",
    "## Summary",
    "",
    paste0("- Total documents: ", total),
    paste0("- Included (>=1 hit): ", included),
    paste0("- Excluded (0 hits): ", excluded),
    paste0("- Missing DOI parsed from filename: ", unresolved_doi),
    "",
    "## Hits Per Keyword",
    ""
  )

  lines <- c(lines, fmt_table(keyword_summary), "## Hits Per Section", "")
  lines <- c(lines, fmt_table(section_summary), "## Hits Per Keyword And Section", "")
  lines <- c(lines, fmt_table(keyword_section_summary))

  writeLines(lines, con = out_md)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  tei_files <- list_tei_files(args$tei_dir)
  if (!is.na(args$max_files) && args$max_files > 0) {
    tei_files <- head(sort(tei_files), args$max_files)
  }
  if (length(tei_files) == 0) {
    stop("No .tei.xml files found in: ", args$tei_dir)
  }

  message("Reading TEI files with metacheck: ", length(tei_files), " files")
  papers <- metacheck::read(tei_files)
  full_text <- collect_full_text(papers)

  file_map <- tibble(
    file = normalizePath(tei_files, winslash = "/", mustWork = FALSE),
    doi = map_chr(tei_files, extract_doi_from_filename)
  ) |>
    mutate(doc_key = coalesce(doi, basename(file)))

  full_text <- full_text |>
    left_join(file_map |> select(file, doi, doc_key), by = "file")

  keywords <- c("pilot", "feasibility study", "preliminary study")
  keyword_re <- regex(str_c(keywords, collapse = "|"), ignore_case = TRUE)

  hits <- full_text |>
    filter(section != "references" | is.na(section)) |>
    filter(str_detect(text, keyword_re)) |>
    transmute(
      doc_key,
      doi,
      section = coalesce(section, "unknown"),
      keyword = str_extract_all(str_to_lower(text), keyword_re)
    ) |>
    unnest_longer(keyword, keep_empty = FALSE)

  per_doc_hits <- hits |>
    group_by(doc_key, doi) |>
    summarise(
      keywords_found = str_c(sort(unique(keyword)), collapse = "; "),
      sections_found = str_c(sort(unique(section)), collapse = "; "),
      include = TRUE,
      .groups = "drop"
    )

  result <- file_map |>
    select(doc_key, doi) |>
    distinct() |>
    left_join(per_doc_hits, by = c("doc_key", "doi")) |>
    mutate(
      keywords_found = coalesce(keywords_found, ""),
      sections_found = coalesce(sections_found, ""),
      include = coalesce(include, FALSE)
    ) |>
    select(doi, keywords_found, sections_found, include) |>
    arrange(desc(include), doi)

  keyword_summary <- hits |>
    group_by(keyword) |>
    summarise(
      mentions = n(),
      documents = n_distinct(doc_key),
      .groups = "drop"
    ) |>
    arrange(desc(documents), desc(mentions), keyword)

  section_summary <- hits |>
    group_by(section) |>
    summarise(
      mentions = n(),
      documents = n_distinct(doc_key),
      .groups = "drop"
    ) |>
    arrange(desc(documents), desc(mentions), section)

  keyword_section_summary <- hits |>
    distinct(doc_key, keyword, section) |>
    count(keyword, section, name = "documents") |>
    arrange(desc(documents), keyword, section)

  unresolved_doi <- sum(is.na(file_map$doi))

  utils::write.csv(result, args$out_csv, row.names = FALSE, na = "")
  build_markdown_report(
    out_md = args$out_md,
    tei_dir = args$tei_dir,
    result = result,
    keyword_summary = keyword_summary,
    section_summary = section_summary,
    keyword_section_summary = keyword_section_summary,
    unresolved_doi = unresolved_doi
  )

  message("Wrote CSV: ", args$out_csv)
  message("Wrote report: ", args$out_md)
}

main()
