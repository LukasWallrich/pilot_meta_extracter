# Pilot Meta-Extractor

Automated extraction of study-level metadata for a meta-analysis / registered report on the reporting of **pilot studies** in psychology. Given a set of article DOIs and their GROBID **TEI** files, the pipeline returns one clean row per article covering bibliographic metadata, first-author location, funding, conflict-of-interest, and preregistration — validated against manual coding across two pilot rounds.

## Quick start

```r
# 1. (optional, one-time) download the TOP Factor reference data
Rscript download_top_factor.R

# 2. run the pipeline on the two pilot samples
PIPELINE_RUN=1 Rscript run_pipeline.R
# -> writes data/final_extraction_pilot1-and-pilot2.csv
```

To run on your own DOIs, source the pipeline and call `run_pipeline()` with a data frame of `doi` + `tei_file` (path):

```r
source("run_pipeline.R")
result <- run_pipeline(data.frame(doi = my_dois, tei_file = my_tei_paths))
```

## Repository structure

**Root — the pipeline (numbered in run order) + docs**

| File | Role |
|---|---|
| `run_pipeline.R` | **Main entry point.** Sources the four modules, runs them per DOI, writes `data/final_extraction_*.csv`. Single source of truth for the extraction logic. |
| `1_extract_doi_metadata.R` | Module: OpenAlex + Crossref metadata — journal, ISSN, SJR, TOP Factor, author location, structured funders, COI, abstract. |
| `2_extract_tei_coi_funding.R` | Module: funding + COI extraction from GROBID TEI (regex/XPath over document structure). |
| `3_extract_prereg.R` | Module: preregistration detection — binary + platform (OSF template–gated, AsPredicted, trial registries). |
| `4_link_funders_locations.R` | Module: funder → country resolution (Crossref Funder Registry → OpenAlex/ROR, with a TEI-name fallback). |
| `openalex_utils.R`, `download_top_factor.R`, `top_factor_manual_setup.R` | Shared helpers / one-time setup. |
| `write-up-for-reg-rep.Rmd` | Descriptive write-up for the registered report. |
| `README_pipeline.md` | Full column spec + codebook validation for `run_pipeline.R`. |
| `README_tei_extraction.md`, `README_prereg_extraction.md`, `README_funders.md` | Per-module method + validation docs. |

The numbered `N_*.R` files are **function libraries** (sourcing them only defines functions). `run_pipeline.R` derives everything fresh from the API for any input, so it has **no dependency on the pilot data**.

**Folders**

| Folder | Contents |
|---|---|
| `data/` | Coding forms, pipeline outputs (see below), and API caches (reruns make no network calls). |
| `validation/` | Reproduction + validation: rx-vs-metacheck comparison, manual↔auto comparison, the pilot-1 DOI→TEI matcher, and the pilot **driver** scripts (`pilot_*.R`) that regenerate the per-stage outputs and score them against the manual coding. |
| `exploration/` | Side experiments not part of the pipeline: OpenAlex topic classification, OpenAlex-field probes, and an open-ended-registration labelling experiment (`oer/`). |
| `archive/` | Superseded combined datasets and older outputs, kept for provenance. |

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

Column-by-column derivation rules and the codebook-validation numbers are in **`README_pipeline.md`**.

Other data products:

- `data/manual-and-auto-combined_*.csv` — the manual coding (both pilots) joined to the pipeline output (via `validation/combine_with_manual.R`).
- Per-stage outputs — `data/tei_coi_funding_extracted_*.csv`, `data/prereg_extracted_*.csv`, `data/funders_with_locations_*.csv`, `data/funder_locations_long.csv` — regenerated by the `validation/pilot_*.R` drivers.
- `data/rx_vs_metacheck_metrics.csv` — validation of the custom extractor against the `metacheck` package.

## Validation summary

Against the manual coding (papers with a usable code):

- **Funding / COI extraction** beats `metacheck` on both pilots (funding F1 0.95 vs 0.90; COI 0.91 vs 0.88 pooled) — see `README_tei_extraction.md`.
- **Preregistration** detection: F1 0.91 (`prereg_found`), 0.93 (`registration_any`) — see `README_prereg_extraction.md`.
- **Funder → country**: 100% country resolution for structured Crossref funder IDs — see `README_funders.md`.

## Data sources

OpenAlex · Crossref · Scimago Journal Rank (`sjrdata`) · TOP Factor (COS) · OSF & AsPredicted APIs · GROBID (TEI). Please cite the sources you rely on.
