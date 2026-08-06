# Pilot Meta-Extractor

Automated extraction of study-level metadata for a meta-analysis / registered report on the reporting of **pilot studies** in psychology. Given a set of article DOIs and their GROBID **TEI** files, the pipeline returns one clean row per article covering bibliographic metadata, first-author location, funding, conflict-of-interest, and preregistration — validated against manual coding across two pilot rounds.

## Quick start

```r
# 1. (optional, one-time) download the TOP Factor reference data
Rscript setup/download_top_factor.R

# 2a. run the pipeline on the two pilot samples (validation against manual coding)
PIPELINE_RUN=1 Rscript run_pipeline.R
# -> writes data/final_extraction_pilot1-and-pilot2.csv

# 2b. run the pipeline on the FULL pre-keyword empirical sample (the analysis dataset)
Rscript run_full_sample.R           # ~1.5 h on a cold cache; resumable
# -> writes data/full_sample_index.csv and
#           data/final_extraction_full-empirical-sample.csv
```

To run on your own DOIs, source the pipeline and call `run_pipeline()` with a data frame of `doi` + `tei_file` (path):

```r
source("run_pipeline.R")
result <- run_pipeline(data.frame(doi = my_dois, tei_file = my_tei_paths))
```

## Repository structure

Only the two entry points sit at the root; everything else is filed by role.

```
README.md
run_pipeline.R          main entry point — DOIs + TEIs -> the 20 columns
run_full_sample.R       driver for the analysis dataset
R/                      the extraction modules (function libraries)
setup/                  one-time reference-data setup
docs/                   method + validation write-ups
data/                   inputs, outputs, API caches
validation/             reproduction + scoring against the manual coding
exploration/            side experiments, not part of the pipeline
archive/                superseded outputs and dev scripts, kept for provenance
```

**Entry points**

| File | Role |
|---|---|
| `run_pipeline.R` | **Main entry point.** Sources the four modules, runs them per DOI, writes `data/final_extraction_*.csv`. Single source of truth for the extraction logic. |
| `run_full_sample.R` | **Driver for the analysis dataset.** Builds the DOI → TEI index over `full_sample.csv` plus the pilot-2 frame, and runs `run_pipeline()` over the empirical and manually coded articles — metadata for the sample *before* pilot-keyword screening, so pilot-reporting and non-reporting articles can be compared. |

**`R/` — the modules, numbered in run order**

| File | Role |
|---|---|
| `R/1_extract_doi_metadata.R` | OpenAlex + Crossref metadata — journal, ISSN, SJR, TOP Factor, author location, structured funders, COI, abstract. |
| `R/2_extract_tei_coi_funding.R` | Funding + COI extraction from GROBID TEI (regex/XPath over document structure). |
| `R/3_extract_prereg.R` | Preregistration detection — binary + platform (OSF template–gated, AsPredicted, trial registries). |
| `R/4_link_funders_locations.R` | Funder → country resolution (Crossref Funder Registry → OpenAlex/ROR, with a TEI-name fallback). |
| `R/openalex_utils.R` | Shared OpenAlex helpers. |

These are **function libraries**: sourcing one only defines functions. `run_pipeline.R` works on any DOIs + TEIs, querying OpenAlex / Crossref / OSF only for inputs not already in the on-disk caches under `data/` (and storing new results there), so repeat runs make no network calls.

**Everything else**

| Path | Contents |
|---|---|
| `setup/` | One-time reference-data setup: `download_top_factor.R`, `top_factor_manual_setup.R`. |
| `docs/` | `README_pipeline.md` (full column spec + codebook validation), the three per-module method docs, and `write-up-for-reg-rep.Rmd`. |
| `data/` | The current outputs at the top level; `inputs/` (coding forms, the pilot-1 DOI→TEI map), `cache/` (API caches — reruns make no network calls) and `stages/` (per-stage outputs from the validation drivers) below it. |
| `validation/` | Reproduction + validation: rx-vs-metacheck comparison, manual↔auto comparison, the pilot-1 DOI→TEI matcher, and the pilot **driver** scripts (`pilot_*.R`) that regenerate the per-stage outputs and score them against the manual coding. |
| `exploration/` | Side experiments not part of the pipeline: article classification (empirical / subfield), OpenAlex-field probes, and an open-ended-registration labelling experiment (`oer/`). |
| `archive/` | Superseded combined datasets, older outputs, and retired dev scripts, kept for provenance. |

## Resulting data

`run_pipeline.R` writes **`data/final_extraction_pilot1-and-pilot2.csv`** — one row per DOI, 20 columns:

| Column | Meaning |
|---|---|
| `doi` | Article DOI (join key). |
| `journal`, `issn` | Venue name and primary ISSN (OpenAlex). |
| `top_factor`, `sjr` | Journal transparency (TOP Factor) and Scimago (SJR) scores. |
| `first_author_country` | First author's country, preferring the **article-level affiliation** (where they were at research time), falling back to their OpenAlex profile. |
| `first_author_location_full` | `"Institution, City, Region, Country"` for the same. |
| `first_author_location_source` | `article` or `author-profile` (which supplied the location). |
| `funding_category` | `funded` / `no funding` / `no statement`. |
| `funding_statement` | Extracted funding statement text (audit). |
| `funders` | Named funders, `\|\|`-separated (audit); the literal `no funding` where stated. |
| `funding_country` | All distinct funder countries, `\|\|`-separated. |
| `coi_statement` | Extracted conflict-of-interest statement text. |
| `coi_category` | `conflict declared` / `no conflict declared` / `no statement`. |
| `prereg_present` | Whether the article reports a study preregistration. |
| `prereg_platform` | e.g. `OSF registration`, `AsPredicted`, `ClinicalTrials.gov`. |
| `prereg_url` | Registration link(s). |
| `is_registered_report` | Registered Report. |
| `is_trial_registry` | Trial/health-registry entry. |
| `registration_any` | `prereg_present` OR Registered Report OR trial registry. |

Column-by-column derivation rules and the codebook-validation numbers are in **`docs/README_pipeline.md`**.

`run_full_sample.R` writes the same 20 columns for the analysis sample:

- **`data/final_extraction_full-empirical-sample.csv`** — one row per article with a TEI file that is either classified empirical or manually coded in a pilot round. That is the empirical sample (3,851 of 4,076 `full_sample.csv` articles) **plus** every coded article, including the 16 pilot-1 articles the classifier called non-empirical — they are kept so the whole manually coded set stays available for validating the other metadata fields. Filter on `empirical == "yes"` in the index for the strict empirical sample.
- **`data/full_sample_index.csv`** — one row per `full_sample.csv` article with `doi`, `tei_file`, `empirical` (LLM classification), `research_area` (LLM classification), `keyword_hit` (whether the article was picked up by the pilot-keyword screen) and `article_status` (`pilot1` / `pilot2` / `pilot1-unsampled` — which manual coding round the article was drawn into). Join on `doi` to split the extraction output by pilot reporting or by coding round.

Other data products:

- `data/manual-and-auto-combined_2026-07-01.csv` — the manual coding (both pilots) joined to the pipeline output (via `validation/combine_with_manual.R`).
- `data/rx_vs_metacheck_metrics.csv` — validation of the custom extractor against the `metacheck` package.
- `data/stages/` — per-stage outputs (`tei_coi_funding_extracted_*`, `prereg_extracted_*`, `funders_with_locations_*`, `funder_locations_long`), regenerated by the `validation/pilot_*.R` drivers. Each covers both pilots, with a `coding` column marking the round.
- `data/cache/` — API caches (OpenAlex/Crossref metadata, OSF GUIDs and templates, funder ID and name resolution). Delete a file here to force those lookups to run again.

## Validation summary

Against the manual coding (papers with a usable code):

- **Funding / COI extraction** beats `metacheck` on both pilots (funding F1 0.95 vs 0.90; COI 0.91 vs 0.88 pooled) — see `docs/README_tei_extraction.md`.
- **Preregistration** detection: F1 0.91 (`prereg_found`), 0.93 (`registration_any`) — see `docs/README_prereg_extraction.md`.
- **Funder → country**: 100% country resolution for structured Crossref funder IDs — see `docs/README_funders.md`.

## Data sources

OpenAlex · Crossref · Scimago Journal Rank (`sjrdata`) · TOP Factor (COS) · OSF & AsPredicted APIs · GROBID (TEI). Please cite the sources you rely on.
