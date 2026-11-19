# Release area

This folder defines the current-only release bundle (for the Zenodo deposit) and
records which tracked files are current and which are historical.

- `MANIFEST.txt` is the explicit list of files in the bundle. Nothing else enters
  the ZIP.
- `build_release.py` builds the bundle from committed content. It refuses
  untracked paths, anything under `archive/` or `exploration/`, TEI files, source
  PDFs, and secret-like files.
- `DEPOSIT_README.md` is the release README. It is copied into the ZIP as
  `RELEASE_README.md` and uploaded beside the ZIP.

## Build

From the repository root, on a clean working tree:

```sh
python3 release/build_release.py --out release/dist
```

This writes `release/dist/pilot-meta-extractor-<version>.zip`,
`release/dist/SHA256SUMS.txt` (every file inside the ZIP, then the ZIP itself) and
`release/dist/DEPOSIT_README.md`. `release/dist/` is gitignored. The ZIP is
deterministic: the same commit gives the same ZIP hash.

The builder pseudonymises the `Coder Initials` column in the three coding-derived
input tables (`C01`, `C02`, …, in order of first appearance across the files).
Every other byte of those files is unchanged. The mapping is never written; the
SHA256 of each original Git file is recorded in the release README.

## Inventory (19 November 2026)

### Current reusable pipeline (in the bundle)

| Path | Role |
|---|---|
| `run_pipeline.R` | Generic entry point: `run_pipeline()` on any `doi` + `tei_file` (+ optional `pdf_file`) table. |
| `R/1_extract_doi_metadata.R` … `R/5_pdf_coi_fallback.R` | The five extraction modules (function libraries). |
| `setup/` | One-time TOP Factor reference-data setup (`top_factor_data.RData`, gitignored). |
| `data/cache/` | API caches (OpenAlex/Crossref metadata, OSF, funder registry). With them, `run_pipeline(use_api = FALSE)` runs offline for the recorded DOIs. |

### Current study drivers, validation and reproducibility (in the bundle)

| Path | Role | Needs external full texts? |
|---|---|---|
| `PIPELINE_RUN=1 Rscript run_pipeline.R` | Pilot 1 + Pilot 2 coded batch → `data/final_extraction_pilot1-and-pilot2.csv` | Yes (TEI) |
| `run_full_sample.R`, `run_pilot2_sample.R` | Frame-level drivers → `data/final_extraction_{full-empirical-sample,pilot2-sample}.csv`, `data/{full_sample,pilot2_sample}_index.csv` | Yes (TEI and gitignored Zotero frame exports) |
| `run_coi_pdf_fallback.R` | PDF front-page COI fallback over the three final tables | Yes (PDF) |
| `screening/` | GROBID conversion workflow, pilot-keyword screen and its Pilot 2 output; `screening/classification/` holds the LLM empirical/subfield classifier that feeds `run_full_sample.R` | Yes |
| `validation/final_agreement.R` | Headline agreement of final outputs with original manual codes | No |
| `validation/pilot2_auto_vs_manual.R` | Pilot 2 statement presence | No |
| `validation/plot_pilot_confusion.R` + `docs/write-up-for-reg-rep.Rmd` | Binary and mixed manuscript figures (renders the Rmd's figure chunks) | No |
| `validation/plot_pilot_categories.R` | Three-category figure and table | No |
| `validation/validate_rx_vs_metacheck.R` | XML-only `rx` vs `metacheck` benchmark | No (reads the TEI stage table) |
| `validation/pilot_link_funders.R` | Funder → country stage tables | No (uses caches) |
| `validation/pilot_extract_tei.R`, `validation/pilot_extract_prereg.R` | TEI and preregistration stage tables | Yes (TEI) |
| `validation/match_pilot1_tei.R` + `data/cache/pilot1_*` | Provenance of `data/inputs/pilot1_doi_tei_map.csv` | Yes (Pilot 1 TEI) |
| `validation/recover_missing_pilot2.R` | Reproduces the recovered Pilot 2 article from its external XML and PDF | Yes (one XML, one PDF) |
| `validation/combine_with_manual.R` | Optional join of manual coding and final output (not run for this release) | No |
| `data/inputs/` | Coding forms (both pilots), the historical 11 June 2026 combined table, the Pilot 1 DOI→TEI map and the recovery provenance record | — |
| `data/*.csv`, `data/stages/`, `docs/figures/` | Current outputs | — |

The historical input `data/inputs/2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv`
stays current: it holds the original Pilot 1 Crossref metadata behind the Pilot 1
Crossref-only comparison (figures, `plot_pilot_categories.R`) and the Crossref funder
columns read by `pilot_link_funders.R`.

### Kept in Git but excluded from the bundle

| Path | Reason |
|---|---|
| `screening/classification/classified_articles.csv` | 8.7 MB with article titles and abstracts. Its outputs (`empirical`, `research_area`) are carried by `data/full_sample_index.csv`. |
| `screening/classification/classification_summary.png` | Not needed to run or interpret the pipeline. |
| `release/` | Build tooling; the release README is added separately. |

### Historical, experimental or superseded (moved to `archive/`)

See `archive/README.md` for the full index. Summary:

- Earlier combined datasets, a superseded copy of the Pilot 1 coding form, and retired
  development scripts (already in `archive/`).
- `exploration/OA fields/`, `exploration/oer/` → `archive/exploration/`.
- `validation/report.{Rmd,html}` and `R/openalex_utils.R` (OpenAlex subfield review;
  not used by the pipeline) → `archive/exploration/openalex_subfield_review/`.
- `docs/extraction_report.{qmd,html}` → `archive/docs/` (7 November 2026 snapshot,
  n = 147, before the recovered Pilot 2 article).
- `data/manual-and-auto-combined_2026-07-01.csv` → `archive/data/` (stale output of
  `combine_with_manual.R` from 2 July 2026).
