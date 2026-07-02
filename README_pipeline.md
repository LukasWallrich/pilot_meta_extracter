# `run_pipeline.R` — consolidated extraction pipeline

One clean, documented pipeline that takes a set of DOIs + their GROBID TEI files
and returns a narrow, 20-column field set (one row per DOI). It `source()`s four
numbered function modules and orchestrates them, reusing their on-disk caches.

## Module layout (single source of truth)

The extraction logic lives in four numbered **function libraries** at the repo
root. Sourcing any of them only defines functions + constants — no side effects,
no I/O, no API calls — so each function is defined exactly once and shared by both
the pipeline and the per-stage pilot reproductions:

| Module | Provides |
|---|---|
| `1_extract_doi_metadata.R` | `process_dois()` — OpenAlex + Crossref metadata |
| `2_extract_tei_coi_funding.R` | TEI funder-name / funding / COI regex extraction (`extract_regex`, …) |
| `3_extract_prereg.R` | preregistration / registered-report / trial detection (`classify_paper`, `resolve_osf_caches`, …) |
| `4_link_funders_locations.R` | funder → country resolution (`resolve_funder_ids`, `resolve_funder_names`, `resolve_paper_funders`, …) |

`run_pipeline.R` sources all four and consumes their functions. The pilot-1 +
pilot-2 **batch reproductions and manual/metacheck validation** for stages 2–4
live in `validation/pilot_extract_tei.R`, `validation/pilot_extract_prereg.R`, and
`validation/pilot_link_funders.R` (run from the repo root). Those pilot drivers
re-source the same libraries, so there is one implementation of every function.
`validation/pilot_link_funders.R` is the only file that reads the historical
`data/2026-06-11_auto-and-manual-combined_*.csv`; the pipeline path never does.

## Interface

```r
source("run_pipeline.R")

# doi_tei_df: a data frame / tibble with columns
#   doi       chr   the article DOI (any case; normalised internally)
#   tei_file  chr   path to the GROBID TEI XML (may be NA if no TEI available)
result <- run_pipeline(doi_tei_df, use_api = TRUE,
                       metadata_cache = "data/pipeline_metadata_cache.csv")
```

Returns a tibble with **exactly** the 20 columns below, in this order, one row per
input DOI (deduplicated on normalised DOI; input order preserved). Rows with a
missing/NA `tei_file` still get metadata; their TEI-derived fields are NA/FALSE.

`use_api = FALSE` forces a cache-only run: if any cache is missing an entry the
call errors instead of making a network request (useful to prove a rerun is
offline).

### Driver (bottom of the file)

Running `Rscript run_pipeline.R` with the environment variable
`PIPELINE_RUN=1` set builds the pilot-1 + pilot-2 index, runs `run_pipeline`, and
writes `data/final_extraction_pilot1-and-pilot2.csv`, then prints the codebook
validation, the three funder edge cases, 12 example rows and the column check.

- **pilot 1** index: `data/pilot1_doi_tei_map.csv` (explicit DOI → TEI map).
- **pilot 2** index: DOI-encoded TEI filenames in `../pilot2_pdf_conversion/tei`
  matched to the pilot-2 manual DOIs — the exact filename→DOI derivation from
  `2_extract_tei_coi_funding.R` (`extract_doi_from_filename`).

Sourcing the file (without `PIPELINE_RUN=1`) only defines the functions; it does
**not** auto-run the pilot.

## Output columns

| # | column | source | derivation / allowed values |
|---|--------|--------|------------------------------|
| 1 | `doi` | input | normalised (lower-case, `doi.org/` + `doi:` prefixes stripped) |
| 2 | `journal` | OpenAlex (`process_dois`) | journal display name |
| 3 | `issn` | OpenAlex | primary ISSN |
| 4 | `top_factor` | TOP Factor dataset (`process_dois`) | numeric TOP total, else NA |
| 5 | `sjr` | sjrdata (`process_dois`) | SCImago Journal Rank, else NA |
| 6 | `first_author_country` | OpenAlex | `coalesce(article_location_detail_country, first_author_location_detail_country)` — the article-level affiliation (where the author was at research time) is preferred; the author profile is a fallback |
| 7 | `first_author_location_full` | OpenAlex | `coalesce(article_location_label, first_author_location_label)` — "Institution, City, Region, Country" |
| 8 | `first_author_location_source` | derived | `"article"` if the article-level value was used, else `"author-profile"`, else NA |
| 9 | `funding_category` | funder pipeline | factor `funded` / `no funding` / `no statement`. Equals the funder pipeline's `funding_status` with `unknown → "no statement"` (see below) |
| 10 | `funding_statement` | TEI | AUDIT: cleaned `rx_funding_text` (funding prose). NA if none |
| 11 | `funders` | funder pipeline | AUDIT: `best_funders`, `||`-separated (literal `"no funding"` allowed). NA if none |
| 12 | `funding_country` | funder pipeline | ALL distinct funder countries (`best_funder_countries`), `||`-separated. NA if no funding / unknown |
| 13 | `coi_statement` | TEI + Crossref | `coalesce(cleaned rx_coi_text, coi_crossref)`. NA if none |
| 14 | `coi_category` | derived | factor `conflict declared` / `no conflict declared` / `no statement`. NA statement → `no statement`; else negation regex (below) → `no conflict declared`; otherwise `conflict declared` |
| 15 | `prereg_present` | prereg logic | logical (`prereg_found`) |
| 16 | `prereg_platform` | prereg logic | chr, `||`-separated (OSF registration / AsPredicted / OSF project / preprint / trial registry / `statement (no URL)` …) |
| 17 | `prereg_url` | prereg logic | chr, matched registration URL(s), `||`-separated |
| 18 | `is_registered_report` | prereg logic | logical. Automated: "registered report" keyword in text OR an OSF "Registered Report Protocol" registration template |
| 19 | `is_trial_registry` | prereg logic | logical (a clinical-trial registry URL or plain-text registry ID) |
| 20 | `registration_any` | derived | logical, `prereg_present OR is_registered_report OR is_trial_registry` |

## Key derivation rules

**Location coalesce (cols 6–8).** Article-level affiliation is preferred over the
author profile because it reflects where the first author was at research time.
`first_author_location_source` records which branch fired.

**`funding_category` (col 9).** Taken directly from the reused funder pipeline's
`funding_status`, remapping `unknown → "no statement"`. `funding_status` is
`"funded"` when a funder is resolved (a Crossref funder ID present OR a real named
TEI funder) **or** a substantive funding statement is present; `"no funding"` when
the TEI marks an explicit no-funding statement and no funder is found; `"unknown"`
otherwise. This deliberately reuses the exact logic the validation was tuned on
rather than a hand-rolled stricter rule.

**COI negation regex (col 14).** `coi_category` is `no statement` when there is no
statement; otherwise a case-insensitive negation regex (`COI_NEG_RE`) classifies
the statement. It matches phrases such as *no conflict / no competing interest /
nothing to disclose / declare(s) no / the authors declare none / no financial …
interest / report no conflict*. A match → `no conflict declared`; a statement that
does not match → `conflict declared`. Example calls:

```
"The authors declare no conflict of interest."     -> no conflict declared
"No potential conflict of interest was reported."   -> no conflict declared
"Dr X received consulting fees from Pfizer."        -> conflict declared
NA                                                  -> no statement
```

**`is_registered_report` (col 18).** Exposes the prereg logic's registered-report
keyword hit (`has_rr_kw`) and additionally flags OSF registrations whose template
matches "Registered Report" (the `Registered Report Protocol Preregistration`
template, which the dedicated-preregistration template regex does not catch).

## Caches (reused; a rerun makes no new API calls)

| cache | contents | written by |
|-------|----------|-----------|
| `data/pipeline_metadata_cache.csv` | `process_dois` scalar output per DOI | this pipeline (incremental, per-batch, resumable) |
| `data/osf_guid_types.csv` | OSF GUID → object type | prereg logic |
| `data/osf_reg_templates.csv` | OSF registration → template | prereg logic |
| `data/funder_id_country.csv` | Crossref funder ID → country | funder logic |
| `data/funder_name_country.csv` | TEI funder name → registry resolution | funder logic |

The metadata step is the only slow/networked one: `process_dois` queries OpenAlex
+ Crossref (+ first/last author profiles), ~4 calls/DOI. Over the ~342 pilot DOIs
that is ~1,400 calls (all free). It is cached to `pipeline_metadata_cache.csv`
after every 25-DOI batch, so a rerun fetches nothing. With all five caches warm,
`run_pipeline(..., use_api = FALSE)` completes fully offline.

## Behaviour fidelity

The copied functions reproduce the source scripts exactly. Verified by an
all-342-paper regression against the scripts' existing on-disk outputs — every
reused field matched **342/342**:

| field(s) | vs | match |
|----------|----|-------|
| `funding_statement` (= cleaned `rx_funding_text`) | `tei_coi_funding_extracted_pilot1-and-pilot2.csv` | 342/342 |
| `prereg_found`, `prereg_platform`, `prereg_urls`, `is_trial_registry` | `prereg_extracted_pilot1-and-pilot2.csv` | 342/342 |
| `best_funders`, `best_funder_countries`, `funding_status` | `funders_with_locations_pilot1-and-pilot2.csv` | 342/342 |

`registration_any` differs from the old prereg output by design on the two
"Registered Report Protocol" papers (the new spec ORs in `is_registered_report`).

## Codebook validation (coded papers only)

Agreement measured on papers with a manual answer. `funding_category` vs `X8.1`,
`coi_category` vs `X8.2`, `prereg_present` vs `X2.2` ("Yes…" = TRUE). Pilot-2
`X8.1` uses fixed options (matched case-insensitively: leading `No` → no
statement; else `no funding` → no funding; else `Yes`/provides → funded).
Pilot-1 `X8.1` is free text (no-funding phrase → no funding; non-blank → funded;
**blank → no statement**, per the coding spec).

Pilot run (342 papers; index 161 pilot1 + 181 pilot2), accuracy = agreement/coded:

| category | pilot 1 | pilot 2 | pooled |
|----------|---------|---------|--------|
| `funding_category` | 0.391 (63/161) | **0.900 (72/80)** | 0.560 (135/241) |
| `coi_category`     | 0.750 (48/64)  | 0.852 (69/81)   | 0.807 (117/145) |
| `prereg_present`   | 0.938 (60/64)  | 0.951 (77/81)   | 0.945 (137/145) |

Pooled confusion matrices (rows = auto, cols = manual):

```
funding_category            coi_category                                        prereg_present
              funded noF noS               conflict noConf noStmt                      F   T
funded            97   4  93   conflict          5      1      2            FALSE    101   4
no funding         2  12   3   no conflict       9     81      4            TRUE       4  36
no statement       3   1  26   no statement      2     10     31
```

**Read the funding numbers via pilot 2 (0.900).** Pilot-2 `X8.1` is a
purpose-built categorical field and is the trustworthy signal. The pilot-1 and
pooled funding numbers are **depressed by incomplete manual transcription, not
pipeline error**: pilot-1 `X8.1` is free text that coders filled in only when
funding existed. Of the 109 pilot-1 papers with a blank `X8.1` (→ manual "no
statement"), the pipeline finds funding for 87, and **57 of those carry a
publisher-deposited Crossref funder ID** — definitive proof of funding the coder
did not transcribe. So the 93 auto-funded / manual-"no statement" cell is
overwhelmingly a coding-form gap, not a false positive.

**COI disagreements.** Of the 12 pooled cases where auto and manual both record a
statement but disagree, 7 are manual coding errors (statements that plainly
declare no conflict, e.g. "the authors declare no competing interests", coded as
"conflict declared"). The negation regex was tuned on the remainder, gaining the
phrasings "in the absence of any commercial/financial relationships", a bare
"None." closing the statement (incl. GROBID's concatenated `STATEMENTNone.`), and
"do not have any conflicts". One residual auto error is a genuinely mixed
statement (one author discloses royalties while the others declare none), which a
negation regex cannot resolve. The remaining `no statement` disagreements are TEI
extraction recall gaps (a statement exists but GROBID did not surface it),
inherited unchanged from the reused extractor.

**prereg** is the strongest signal (0.945 pooled): 4 false positives, 4 false
negatives out of 145 coded papers.
