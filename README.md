# Pilot Meta-Extractor

Automated extraction of article-level metadata for a meta-scientific Registered
Report on how **pilot studies** are reported in psychology. Given article DOIs and
their GROBID **TEI** files (and optionally the PDFs), the pipeline returns one row
per article with bibliographic metadata, first-author location, funding,
conflict-of-interest (COI) and preregistration fields. It was validated against
manual coding in two pilot rounds (Pilot 1: 2022 articles; Pilot 2: 2023 articles).

## Quick start

```r
source("run_pipeline.R")          # defines functions only
result <- run_pipeline(data.frame(doi = my_dois, tei_file = my_tei_paths))
# optional PDF front-page COI fallback:
# run_pipeline(df_with_pdf_file_column, pdf_coi_fallback = TRUE)
```

`run_pipeline()` queries OpenAlex, Crossref and OSF only for DOIs that are not in
the caches under `data/cache/`, and adds new results there. With
`use_api = FALSE` it runs offline and stops if a cache entry is missing. The 21
output columns and their derivation are documented in `docs/README_pipeline.md`.

Optional one-time setup: `Rscript setup/download_top_factor.R` downloads the TOP
Factor journal data (`top_factor_data.RData`, gitignored). Without it,
`top_factor` is only available for DOIs already in the metadata cache.

## Repository layout

| Path | Contents |
|---|---|
| `run_pipeline.R` | Generic entry point and the Pilot 1 + Pilot 2 driver (`PIPELINE_RUN=1`). |
| `R/1_…` to `R/5_…` | Extraction modules: OpenAlex/Crossref metadata; TEI funding/COI; preregistration; funder → country; PDF COI fallback. Sourcing them only defines functions. |
| `run_full_sample.R`, `run_pilot2_sample.R`, `run_coi_pdf_fallback.R` | Drivers for the Pilot 1 and Pilot 2 sampling frames, and the PDF fallback over finished outputs. |
| `setup/` | TOP Factor reference-data setup. |
| `screening/` | Upstream steps: GROBID conversion workflow, pilot-keyword screen, LLM empirical/subfield classifier. |
| `validation/` | Stage reproductions, scoring against manual coding, figure scripts. |
| `data/` | Final outputs (top level), `inputs/` (coding forms, historical inputs, provenance), `cache/` (API caches), `stages/` (per-stage outputs). |
| `docs/` | Method documentation, the write-up Rmd that holds the figure code, and `figures/`. |
| `release/` | Release manifest and builder for the Zenodo bundle. |
| `archive/` | Historical, experimental and superseded material; see `archive/README.md`. Not used by current code. |

## Reproducing the reported results

These run offline from the committed files, from the repository root:

| Command | Reproduces |
|---|---|
| `Rscript validation/final_agreement.R` | Agreement of the final output with the original manual codes (table below). Read-only. |
| `Rscript validation/pilot2_auto_vs_manual.R` | `data/pilot2_auto_vs_manual_counts.csv`, `data/pilot2_final_statement_comparison.csv` |
| `Rscript validation/plot_pilot_confusion.R` | `docs/figures/pilot_extraction_{mixed,confusion,categories}.{png,pdf}` and `data/pilot_category_comparison.csv` (renders the figure chunks of `docs/write-up-for-reg-rep.Rmd`) |
| `Rscript validation/plot_pilot_categories.R` | The three-category figure and table alone |
| `Rscript validation/validate_rx_vs_metacheck.R` | `data/rx_vs_metacheck_metrics.csv` |
| `Rscript validation/pilot_link_funders.R` | `data/stages/funder*` (from the TEI stage table and caches). A rerun now differs in one cell: the recovered Pilot 2 article's `funding_status` becomes `funded`; 200/227 is unchanged. |

Re-running extraction itself needs the source full texts, which are not
redistributed:

| Command | Needs |
|---|---|
| `PIPELINE_RUN=1 Rscript run_pipeline.R` | Pilot 1 TEIs at the paths in `data/inputs/pilot1_doi_tei_map.csv` (`../pilot_pilot_teis/tei/`) and Pilot 2 TEIs in `../pilot2_pdf_conversion/tei/` |
| `validation/pilot_extract_tei.R`, `validation/pilot_extract_prereg.R` | The same TEI folders; `metacheck` for the TEI stage |
| `run_full_sample.R`, `run_pilot2_sample.R` | The gitignored Zotero exports `full_sample.csv` / `pilot2_sample.csv` and the frame TEIs |
| `run_coi_pdf_fallback.R` | Source PDFs: set `PILOT_PDF_ROOT` to the folder with `Zotero_PDFs/` and `pilot2/`; Poppler's `pdftotext` |
| `Rscript validation/recover_missing_pilot2.R XML PDF OUT.csv` | The original XML and PDF of `10.1177/09567976231165271` (hashes in `data/inputs/recovered_tei/`) |

TEIs were produced with GROBID 0.8.2 (Pilot 1 and the recovered Pilot 2 article)
and 0.8.2.1 (Pilot 2 frame; `screening/grobid_conversion_workflow.yml`).

## Validation summary

All figures compare automated output with the **original manual codes** of the two
pilot rounds. They are agreement on the development sample: the rules were refined
after inspecting these articles, so they are optimistic for unseen articles. Pilot 1
is the cleaner out-of-sample test for the TEI extractor.

**Final output vs original codes** (`validation/final_agreement.R`; with the PDF COI
fallback):

| Measure | Pilot 1 | Pilot 2 | Pooled |
|---|---|---|---|
| Funding category | 47/52 (90.4%) | 78/84 (92.9%) | 125/136 (91.9%) |
| COI category (3 categories) | 53/64 (82.8%) | 76/84 (90.5%) | 129/148 (87.2%) |
| COI statement presence | 59/64 (92.2%) | 80/84 (95.2%) | 139/148 (93.9%) |
| Preregistration | 60/64 (93.8%) | 79/84 (94.0%) | 139/148 (93.9%) |

Blank Pilot 1 funding answers are treated as uncoded (see `docs/README_pipeline.md`).
The recorded Pilot 2 response for `10.1037/xge0001439` that lists only the CRediT
"Funding acquisition" role is retained as a positive manual funding code. Pilot 2
funding statement presence is also 78/84 (92.9%).

**Figures.** `docs/figures/pilot_extraction_mixed.*` keeps Pilot 1's original
Crossref-only binary comparison (n = 65 per outcome) and shows Pilot 2's final
three-category output. The percentages describe different tasks and are not directly
comparable. Alternatives: all-binary (`pilot_extraction_confusion.*`) and
all-three-category (`pilot_extraction_categories.*`; Pilot 1 categories reconstructed
from original Crossref metadata, funding n = 53, COI n = 65).

**XML-only extractor vs `metacheck` 0.1.0** (statement presence from TEI only,
`data/rx_vs_metacheck_metrics.csv`, 148 pooled):

| Outcome | Extractor accuracy / F1 | `metacheck` accuracy / F1 |
|---|---|---|
| Funding | 0.926 / 0.956 | 0.838 / 0.898 |
| COI | 0.878 / 0.914 | 0.831 / 0.878 |

On Pilot 1 alone, F1 is 0.92 vs 0.89 (funding) and 0.85 vs 0.81 (COI).

**Funder → country.** Of the 227 papers with at least one named funder, 200 (88%)
get at least one funder country (`docs/README_funders.md`; stage run of 26 October
2026, not re-audited since).

Module-level preregistration figures in `docs/README_prereg_extraction.md` come from
an earlier stage run (147 coded papers) and use a stricter filter; they are not the
end-to-end figures above.

## Recovered Pilot 2 article

`10.1177/09567976231165271` was missing from the coded batch because its manual DOI
had a bare `doi.org/` prefix; `norm_doi()` now strips it. The output was regenerated
from the original GROBID 0.8.2 XML, whose source PDF matches the assigned Zotero/Drive
PDF by hash, with cached metadata and unchanged extraction rules. Provenance:
`data/inputs/recovered_tei/10.1177-09567976231165271.provenance.json`. The frame
index `data/pilot2_sample_index.csv` predates this fix and still lists the article
as `pilot2-unsampled`.

## Software

Validated with R 4.6.1, dplyr 1.2.1, stringr 1.6.0, tidyr 1.3.2, purrr 1.2.2,
readr 2.2.0, xml2 1.6.0, httr 1.4.8, httr2 1.2.3, jsonlite 2.0.0, stringdist 0.9.17,
stringi 1.8.9, countrycode 1.8.0, sjrdata 1.0.25 (GitHub `ikashnitsky/sjrdata`),
ggplot2 4.0.3, patchwork 1.3.2, knitr 1.51, tidyverse 2.0.0 and `metacheck` 0.1.0
(GitHub `scienceverse/metacheck`; TEI stage and keyword screen only). The PDF
fallback needs Poppler's `pdftotext`. Sourcing `R/1_extract_doi_metadata.R` installs
`pacman`, `pak` and `sjrdata` if they are missing.

API politeness: the code sends a contact address with OpenAlex, Crossref and OSF
requests (in `run_pipeline.R`, `R/1_…`, `R/3_…`, `R/4_…` and
`validation/match_pilot1_tei.R`). Replace it with
your own before making new API calls. `OPENALEX_API_KEY` is optional and read from the
environment.

## Data sources

OpenAlex · Crossref (including the Funder Registry) · Scimago Journal Rank
(`sjrdata`) · TOP Factor (Center for Open Science) · OSF and AsPredicted · GROBID.
Please cite the sources you rely on.

## Licence and citation

CC BY 4.0; see `LICENSE`. Citation metadata: `CITATION.cff`.
