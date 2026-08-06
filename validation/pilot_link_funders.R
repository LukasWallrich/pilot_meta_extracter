# =============================================================================
# validation/pilot_link_funders.R
# -----------------------------------------------------------------------------
# PILOT-1 + PILOT-2 batch reproduction of funder -> country resolution.
# This is the ONLY file allowed to read the 2026-06-11 auto-and-manual-combined
# CSV, from which it pulls the Crossref funder columns and passes them as
# ARGUMENTS to the shared library resolvers. Writes:
#   data/stages/funders_with_locations_pilot1-and-pilot2.csv  (one row per paper)
#   data/stages/funder_locations_long.csv                     (one row per paper x funder)
# Run from the project root:  Rscript validation/pilot_link_funders.R
# =============================================================================
source("R/4_link_funders_locations.R")   # resolve_funder_ids/_names, cluster_funders, name_sim, ...

norm_doi <- function(d) str_trim(str_to_lower(str_replace(d, "https?://(dx\\.)?doi\\.org/", "")))

splitpp <- function(x) {                       # split a '||'-joined string
  if (is.na(x) || x == "") return(character(0))
  v <- str_trim(unlist(strsplit(x, "\\|\\|")))
  v[v != ""]
}

# ---------------------------------------------------------------- load data --
comb <- read_csv(file.path(DATA, "inputs", "2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv"),
                 show_col_types = FALSE)
tei  <- read_csv(file.path(DATA, "stages", "tei_coi_funding_extracted_pilot1-and-pilot2.csv"),
                 show_col_types = FALSE)

comb$doi_n <- norm_doi(comb$doi)
tei$doi_n  <- norm_doi(tei$doi)

master <- comb |>
  filter(!is.na(doi_n), doi_n != "test") |>
  distinct(doi_n, .keep_all = TRUE) |>
  transmute(doi = doi_n, coding, journal,
            funders_cr_raw  = funding_funders_crossref,
            funder_ids_cr   = funding_funder_ids_crossref,
            manual_funding_stmt = `X8.1..Does.the.article.include.a.funding.statement.`)

tei_small <- tei |>
  distinct(doi_n, .keep_all = TRUE) |>
  transmute(doi = doi_n, rx_funder_names, rx_funding_text)

master <- master |> left_join(tei_small, by = "doi")

cat(sprintf("Master papers: %d  (with TEI funder names: %d)\n",
            nrow(master), sum(!is.na(master$rx_funder_names))))

# ================================================================ SOURCE A ===
# Structured Crossref funder IDs -> country (library resolver; cached).
all_ids <- unique(unlist(lapply(master$funder_ids_cr, splitpp)))
cat(sprintf("Unique Crossref funder IDs to resolve: %d\n",
            length(all_ids[grepl("^10\\.13039/", all_ids)])))
id_lookup <- resolve_funder_ids(all_ids, use_api = TRUE)
cat(sprintf("ID resolution rate (country obtained): %d / %d (%.0f%%)\n",
            sum(!is.na(id_lookup$country)), nrow(id_lookup),
            if (nrow(id_lookup)) 100 * mean(!is.na(id_lookup$country)) else 0))

# ================================================================ SOURCE B ===
# TEI funder names -> registry resolution (library resolver; cached). Cheap
# local signals (token / contained-country / bare-country) are recomputed.
all_names <- unlist(lapply(master$rx_funder_names, splitpp))
name_info <- resolve_funder_names(all_names, use_api = TRUE)
cat(sprintf("\nTEI names: bare-country %d | registry-resolved %d | has-token(unresolved) %d\n",
            sum(name_info$bare_country),
            sum(name_info$resolved_reg),
            sum(!name_info$resolved_reg & !name_info$bare_country & name_info$has_token)))

# fast lookup by name
ni <- name_info; rownames(ni) <- NULL
name_get <- function(nm) {
  r <- ni[ni$query_name == nm, , drop = FALSE]
  if (!nrow(r)) return(NULL)
  r[1, ]
}

# ================================================== BUILD PER-PAPER STRUCTURES
long_rows <- list(); paper_rows <- list(); override_log <- list(); junk_rows <- list()

for (i in seq_len(nrow(master))) {
  doi <- master$doi[i]; coding <- master$coding[i]; journal <- master$journal[i]
  ids <- splitpp(master$funder_ids_cr[i]); ids <- ids[grepl("^10\\.13039/", ids)]
  raw_tnames <- splitpp(master$rx_funder_names[i])
  is_no_funding <- any(tolower(raw_tnames) == "no funding")
  tnames <- raw_tnames[tolower(raw_tnames) != "no funding"]

  # ---- (1) Crossref-ID funders (authoritative identity + country) ----------
  cr_set <- id_lookup |> filter(crossref_funder_id %in% ids)

  # ---- (2) TEI country hints (author-stated, registry-INDEPENDENT) ---------
  # Used ONLY to correct a known-wrong registry country. Built strictly from
  # STANDALONE bare-country fragments (e.g. a lone "Taiwan" GROBID-split out next
  # to its funder) -- the strongest author-stated location signal. We do NOT use
  # the country *contained inside another funder's* name, because in a multi-
  # funder paper that would wrongly relocate a correctly-registered funder.
  hint_ccs <- character(0)
  for (nm in tnames) {
    r <- name_get(nm); if (is.null(r)) next
    if (isTRUE(r$bare_country)) hint_ccs <- c(hint_ccs, r$cont_cc)
  }
  hint_ccs <- unique(na.omit(hint_ccs))
  tei_names_norm <- norm_name(tnames)

  # ---- OVERRIDE: narrow correction of a wrong Crossref registry country ----
  # Fire only when (i) the paper's TEI country hints resolve to exactly ONE
  # country, (ii) that funder's name actually appears in the TEI (so the hint
  # refers to it), and (iii) the registry country is not among the hints.
  cr_set <- cr_set |>
    mutate(country_source = ifelse(is.na(country), NA_character_, "crossref_id"))
  if (length(hint_ccs) == 1 && nrow(cr_set)) {
    hint_country <- cc_to_country(hint_ccs)
    for (k in seq_len(nrow(cr_set))) {
      reg_cc   <- cr_set$country_code[k]
      in_tei   <- any(vapply(tei_names_norm,
                             function(t) name_sim(t, norm_name_one(cr_set$funder_name[k])) >= 0.85, logical(1)))
      if (in_tei && (is.na(reg_cc) || !(reg_cc %in% hint_ccs))) {
        override_log[[length(override_log)+1]] <- tibble(
          doi = doi, funder = cr_set$funder_name[k],
          from = ifelse(is.na(cr_set$country[k]), "NA", cr_set$country[k]), to = hint_country)
        cr_set$country_code[k]    <- hint_ccs
        cr_set$country[k]         <- hint_country
        cr_set$country_source[k]  <- "tei_override_crossref"
      }
    }
  }

  # ---- (3) Clean TEI funders (recording screened-out names for the audit) --
  te_rows <- list()
  add_junk <- function(name, reason) junk_rows[[length(junk_rows)+1]] <<- tibble(
    doi = doi, coding = coding, funder_name = name, crossref_funder_id = NA_character_,
    country_code = NA_character_, country = NA_character_, source = "tei_name",
    country_source = reason, confidence = "junk", junk = TRUE)
  for (nm in tnames) {
    r <- name_get(nm); if (is.null(r)) next
    rl <- tolower(str_trim(nm))
    # --- junk screens (emit an audit row, then skip) ---
    if (rl %in% junk_terms)                                  { add_junk(nm, "junk_term");    next }
    if (isTRUE(r$bare_country))                              { add_junk(nm, "bare_country"); next }
    if (is_journal_like(nm, journal))                       { add_junk(nm, "journal");      next }
    if (!r$resolved_reg && !r$has_token && is.na(r$cont_cc)){ add_junk(nm, "misparse");     next }
    # --- country: b (registry) > c (contained) > eu-name > NA; confidence with it
    if (r$resolved_reg && !is.na(r$reg_country)) {
      ctry <- r$reg_country; csrc <- "tei_registry"; cid <- r$crossref_funder_id
      cn <- r$matched_name; conf <- r$reg_confidence            # high or medium (fuzzy)
    } else if (!is.na(r$cont_cc)) {
      ctry <- r$cont_country; csrc <- "tei_contained"
      cid  <- if (r$resolved_reg) r$crossref_funder_id else NA_character_
      cn   <- nm; conf <- "medium"
    } else if (is_eu_name(nm)) {
      ctry <- "European Union"; csrc <- "tei_contained"; cid <- r$crossref_funder_id
      cn <- nm; conf <- "medium"
    } else {                                                   # kept (funder token) but no country
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

  # ---- (4) Merge CR + TEI funders into the deduped "best" set ---------------
  cr_part <- if (nrow(cr_set)) tibble(name = cr_set$funder_name, crossref_funder_id = cr_set$crossref_funder_id,
                                      country = cr_set$country, country_source = cr_set$country_source,
                                      source = "crossref_id", conf = "high") else te_set[0, ]
  combined <- bind_rows(cr_part, te_set)
  # priority for country selection within a cluster
  pref_map <- c(crossref_id = 1, tei_override_crossref = 1, tei_registry = 2, tei_contained = 3)
  combined$pref <- ifelse(is.na(combined$country_source), 9, pref_map[combined$country_source])
  best <- cluster_funders(combined)

  has_cr <- nrow(cr_set) > 0; has_te <- nrow(te_set) > 0

  # ---- long table rows: kept funders (junk=FALSE) then screened-out (junk=TRUE)
  for (j in seq_len(nrow(best))) {
    cc <- suppressWarnings(countrycode(best$country[j], "country.name", "iso2c", warn = FALSE))
    if (!is.na(best$country[j]) && best$country[j] == "European Union") cc <- "EU"
    long_rows[[length(long_rows)+1]] <- tibble(
      doi = doi, coding = coding, funder_name = best$name[j],
      crossref_funder_id = best$crossref_funder_id[j], country_code = cc, country = best$country[j],
      source = best$source[j], country_source = best$country_source[j] %||% NA_character_,
      confidence = best$conf[j], junk = FALSE)
  }

  # ---- reconciliation (kept for transparency) ------------------------------
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

  # ---- funding status ------------------------------------------------------
  ftxt <- master$rx_funding_text[i]
  has_funding_text <- !is.na(ftxt) && nchar(str_trim(ftxt)) > 15 &&
                      tolower(str_trim(ftxt)) != "no funding"
  fstatus <- if (recon == "no_funding") "no funding"
             else if (nrow(best) > 0) "funded"
             else if (has_funding_text && !is_no_funding) "funded"
             else "unknown"

  # ---- best output strings -------------------------------------------------
  if (fstatus == "no funding") {
    best_funders <- "no funding"; best_countries <- NA_character_
  } else if (nrow(best) > 0) {
    best_funders   <- paste(best$name, collapse = " || ")
    ucs <- unique(na.omit(best$country))
    best_countries <- if (length(ucs)) paste(ucs, collapse = " || ") else NA_character_
  } else {
    best_funders <- NA_character_; best_countries <- NA_character_
  }

  cr_disp <- if (has_cr) paste(sprintf("%s (%s)", cr_set$funder_name,
                ifelse(is.na(cr_set$country), "?", cr_set$country)), collapse = " || ") else NA
  te_disp <- if (has_te) paste(sprintf("%s (%s)", te_set$name,
                ifelse(is.na(te_set$country), "?", te_set$country)), collapse = " || ") else NA

  paper_rows[[length(paper_rows)+1]] <- tibble(
    doi = doi, coding = coding,
    best_funders = best_funders, best_funder_countries = best_countries,
    funding_status = fstatus, reconciliation = recon,
    n_funders = nrow(best), n_funders_crossref = nrow(cr_set), n_funders_tei = nrow(te_set),
    funders_crossref = cr_disp, funders_tei = te_disp)
}

# long table = kept funders (junk=FALSE, the audit trail's "what we used") followed
# by screened-out TEI names (junk=TRUE, "what we dropped and why" in country_source).
long_df  <- bind_rows(bind_rows(long_rows), bind_rows(junk_rows))
paper_df <- bind_rows(paper_rows)
ovr_df   <- if (length(override_log)) bind_rows(override_log) else
  tibble(doi=character(), funder=character(), from=character(), to=character())

write_csv(long_df,  file.path(DATA, "stages", "funder_locations_long.csv"))
write_csv(paper_df, file.path(DATA, "stages", "funders_with_locations_pilot1-and-pilot2.csv"))

# ================================================================== REPORT ===
cat("\n========== SUMMARY ==========\n")
cat("Funding status:\n");      print(table(paper_df$funding_status, useNA = "ifany"))
cat("\nReconciliation flags:\n"); print(table(paper_df$reconciliation, useNA = "ifany"))
cat(sprintf("\nPapers with >=1 best funder: %d   with >=1 best country: %d\n",
            sum(paper_df$n_funders > 0),
            sum(!is.na(paper_df$best_funder_countries))))
kept <- long_df |> filter(!junk)
cat(sprintf("Long-table KEPT funder rows: %d   with a country: %d (%.0f%%)   | junk audit rows: %d\n",
            nrow(kept), sum(!is.na(kept$country)), 100*mean(!is.na(kept$country)), sum(long_df$junk)))
cat("country_source distribution (kept rows):\n"); print(table(kept$country_source, useNA="ifany"))
cat("confidence distribution (kept rows):\n");     print(table(kept$confidence, useNA="ifany"))
cat("junk reasons (country_source on junk rows):\n"); print(table(long_df$country_source[long_df$junk]))

cat(sprintf("\nCrossref registry-country OVERRIDES applied: %d\n", nrow(ovr_df)))
if (nrow(ovr_df)) print(as.data.frame(ovr_df))

cat("\n========== 3 REQUIRED VERIFICATION CASES ==========\n")
show_paper <- function(d) {
  r <- paper_df |> filter(doi == d); if (!nrow(r)) { cat("MISSING", d, "\n"); return(invisible()) }
  cat(sprintf("\nDOI %s [%s]  status=%s  recon=%s\n", r$doi, r$coding, r$funding_status, r$reconciliation))
  cat("  best_funders:  ", r$best_funders, "\n")
  cat("  best_countries:", ifelse(is.na(r$best_funder_countries),"-",r$best_funder_countries), "\n")
}
for (d in c("10.32872/cpe.9341","10.1080/10447318.2023.2276529","10.1192/bjo.2023.12")) show_paper(d)

cat("\n========== ~12 WORKED EXAMPLES ==========\n")
pick <- function(flag, n) head(paper_df$doi[paper_df$reconciliation == flag], n)
ex <- unique(c("10.32872/cpe.9341","10.1080/10447318.2023.2276529","10.1192/bjo.2023.12",
               pick("both",3), pick("crossref_only",2), pick("tei_only",2),
               pick("no_funding",2), pick("none",1),
               head(paper_df$doi[paper_df$n_funders >= 3],1)))
for (d in head(ex,12)) show_paper(d)

cat("\nDONE. Wrote:\n  data/stages/funders_with_locations_pilot1-and-pilot2.csv\n",
    " data/stages/funder_locations_long.csv\n  data/cache/funder_id_country.csv\n  data/cache/funder_name_country.csv\n")
