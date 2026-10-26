# Pilot Meta-Extractor

Automated extraction of article-level metadata for a meta-scientific assessment (Registered Report) of how **pilot studies** are reported in psychology. Given a set of article DOIs and their GROBID **TEI** files, the pipeline returns one clean row per article covering bibliographic metadata, first-author location, funding, conflict-of-interest, and preregistration — validated against manual coding across two pilot rounds.

## Quick start

```r
# 1. (optional, one-time) download the TOP Factor reference data
Rscript setup/download_top_factor.R

# 2a. run the pipeline on the two pilot samples (validation against manual coding)
PIPELINE_RUN=1 Rscript run_pipeline.R
# -> writes data/final_extraction_pilot1-and-pilot2.csv

# 2b. run the pipeline on the FULL pre-keyword empirical sample (pilot-1 frame)
Rscript run_full_sample.R           # ~1.5 h on a cold cache; resumable
# -> writes data/full_sample_index.csv and
#           data/final_extraction_full-empirical-sample.csv

# 2c. run the pipeline on the FULL pilot-2 frame
Rscript run_pilot2_sample.R         # resumable
# -> writes data/pilot2_sample_index.csv and
#           data/final_extraction_pilot2-sample.csv
```

To run on your own DOIs, source the pipeline and call `run_pipeline()` with a data frame of `doi` + `tei_file` (path):

```r
source("run_pipeline.R")
result <- run_pipeline(data.frame(doi = my_dois, tei_file = my_tei_paths))
```

## Repository structure

Only the entry points sit at the root; everything else is filed by role.

```
README.md
run_pipeline.R          main entry point — DOIs + TEIs -> the 20 columns
run_full_sample.R       driver for the pilot-1 frame
run_pilot2_sample.R     driver for the pilot-2 frame
R/                      the extraction modules (function libraries)
setup/                  one-time reference-data setup
docs/                   method + validation write-ups
data/                   inputs, outputs, API caches
validation/             reproduction + scoring against the manual coding
screening/              PDF→TEI conversion workflow and pilot-keyword screen (copies)
exploration/            side experiments, not part of the pipeline
archive/                superseded outputs and dev scripts, kept for provenance
```

**Entry points**

| File | Role |
|---|---|
| `run_pipeline.R` | **Main entry point.** Sources the four modules, runs them per DOI, writes `data/final_extraction_*.csv`. Single source of truth for the extraction logic. |
| `run_full_sample.R` | **Driver for the pilot-1 frame.** Builds the DOI → TEI index over `full_sample.csv` plus the manually coded pilot-2 articles, and runs `run_pipeline()` over the empirical and manually coded articles — metadata for the sample *before* pilot-keyword screening, so pilot-reporting and non-reporting articles can be compared. |
| `run_pilot2_sample.R` | **Driver for the pilot-2 frame.** Builds the DOI → TEI index over `pilot2_sample.csv` (the 4,635-article 2023 frame that pilot 2 was drawn from) and runs `run_pipeline()` over every article with a TEI, coded or not. |

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

`run_pilot2_sample.R` writes the same 20 columns for the pilot-2 frame:

- **`data/final_extraction_pilot2-sample.csv`** — one row per pilot-2 frame article with a TEI file. The frame is extracted unfiltered: the empirical / research-area classifier has run over the pilot-1 frame only, so no `empirical` column exists here.
- **`data/pilot2_sample_index.csv`** — one row per `pilot2_sample.csv` article with `doi`, `tei_file`, `keyword_hit` (pilot-keyword screen; `NA` where the article has no TEI to screen) and `article_status` (`pilot2-coded` for the 184 manually coded articles, `pilot2-unsampled` otherwise).

The pilot-1 and pilot-2 frames are disjoint (they overlap in a single article), so the two extraction outputs stack into one dataset. Both frames' PDFs are converted by the GROBID GitHub Action in the private `pilot2_pdf_conversion` repository, which reads them from a Google Drive folder (the workflow and the keyword-screening script are copied into `screening/`; the converted full texts are not redistributed); `pilot2_sample.csv` and `full_sample.csv` are large Zotero exports and are gitignored.

Other data products:

- `data/manual-and-auto-combined_2026-07-01.csv` — the manual coding (both pilots) joined to the pipeline output (via `validation/combine_with_manual.R`).
- `data/rx_vs_metacheck_metrics.csv` — validation of the custom extractor against the `metacheck` package.
- `data/stages/` — per-stage outputs (`tei_coi_funding_extracted_*`, `prereg_extracted_*`, `funders_with_locations_*`, `funder_locations_long`), regenerated by the `validation/pilot_*.R` drivers. Each covers both pilots, with a `coding` column marking the round.
- `data/cache/` — API caches (OpenAlex/Crossref metadata, OSF GUIDs and templates, funder ID and name resolution). Delete a file here to force those lookups to run again.

## Validation summary

Against the manual coding of the two pilot rounds (papers with a usable manual code; pilot 1 2022, pilot 2 2023):

| Variable | n | Accuracy | F1 | Comparison |
|---|---|---|---|---|
| Funding statement present | 147 | 0.925 | 0.955 | `metacheck`: accuracy 0.837, F1 0.897 |
| COI statement present | 147 | 0.878 | 0.913 | `metacheck`: accuracy 0.830, F1 0.877 |
| Preregistration (`prereg_found`) | 142 | 0.951 | 0.911 | — |
| Any registration (`registration_any`) | 147 | 0.952 | 0.925 | — |

- Funding and COI: `data/rx_vs_metacheck_metrics.csv` (per pilot and pooled), method in `docs/README_tei_extraction.md`. The extractor was tuned on pilot-2 misses, so pilot 1 is the cleaner out-of-sample test (funding F1 0.93 vs 0.89 for `metacheck`; COI 0.85 vs 0.81).
- Preregistration: `docs/README_prereg_extraction.md`.
- Pilot-2 coverage as reported in the manuscript (84 articles reporting a pilot; automated = Crossref metadata or TEI extraction): a funding statement was found automatically for 66 of the 67 articles where coders recorded one, and for 7 of the 17 where they did not; a COI statement for 63 of 68 and 3 of 16. `validation/pilot2_auto_vs_manual.R` → `data/pilot2_auto_vs_manual_counts.csv`.
- Funder → country: every structured Crossref funder ID resolves to a country; of the 227 papers with at least one named funder, 200 (88%) get at least one funder country (`docs/README_funders.md`).

Validation outputs were regenerated on 26 October 2026 with R 4.6.1 and `metacheck` 0.1.0 (`validation/pilot_extract_tei.R`, `validation/validate_rx_vs_metacheck.R`, `validation/pilot_extract_prereg.R`, `validation/pilot_link_funders.R`, `validation/pilot2_auto_vs_manual.R`). Other packages: dplyr 1.2.1, stringr 1.6.0, tidyr 1.3.2, purrr 1.2.2, readr 2.2.0, xml2 1.6.0, httr 1.4.8, httr2 1.2.3, jsonlite 2.0.0, stringdist 0.9.17, countrycode 1.8.0, sjrdata 1.0.25. PDFs were converted with GROBID 0.8.2 (pilot 1) and 0.8.2.1 (pilot 2; see `screening/`). The validation drivers expect the pilot-1 TEI files in `../pilot_pilot_teis/tei` and the pilot-2 TEI files in `../pilot2_pdf_conversion/tei`.

## Main study (2025 sample)

The main study uses the same pipeline. Its input is the 2025 article database built by the project's database workflow: one row per sampled empirical article with its DOI, plus the GROBID TEI file for each retrieved PDF. Call `run_pipeline()` on that `doi` + `tei_file` data frame (see Quick start); metadata are extracted for every article before the pilot-keyword screen, so pilot-reporting and other articles can be compared. Journal-to-field grouping is done by the analysis team and is not part of this pipeline.

## Data sources

OpenAlex · Crossref · Scimago Journal Rank (`sjrdata`) · TOP Factor (COS) · OSF & AsPredicted APIs · GROBID (TEI). Please cite the sources you rely on.

## Licence

CC BY 4.0 — see `LICENSE`.
