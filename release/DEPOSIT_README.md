# Pilot Meta-Extractor — current pipeline release

Version `@@VERSION@@`, built from commit `@@COMMIT@@` (@@DATE@@) of
https://github.com/LukasWallrich/pilot_meta_extracter.
Reserved DOI: 10.5281/zenodo.24262974 (resolves after publication).

This ZIP holds the **current** extraction pipeline for the Piloting Assessment
Registered Report, with the inputs, caches and outputs needed to check its reported
validation. It is a curated subset of the repository, listed in
`release/MANIFEST.txt` of that commit. It is not a full copy of the repository.
Historical and experimental material stays in the repository's `archive/` folder.

## Contents (@@N_FILES@@ files)

- **Pipeline:** `run_pipeline.R` (entry point), `R/1_…` to `R/5_…` (modules),
  `setup/` (TOP Factor data).
- **Drivers and upstream steps:** `run_full_sample.R`, `run_pilot2_sample.R`,
  `run_coi_pdf_fallback.R`, `screening/`.
- **Validation:** `validation/`, plus `docs/write-up-for-reg-rep.Rmd`, whose figure
  chunks `validation/plot_pilot_confusion.R` renders. The Rmd is a working draft of
  the pilot write-up, included only as the figure source.
- **Data:** final outputs (`data/*.csv`), per-stage outputs (`data/stages/`), API
  caches (`data/cache/`), coding forms and historical inputs (`data/inputs/`),
  figures (`docs/figures/`).
- **Documentation:** `README.md`, `docs/README_*.md`, `CITATION.cff`, `LICENSE`.
- `SHA256SUMS.txt`: hash of every file in the ZIP.

**Not included:** article PDFs and GROBID TEI full texts (copyrighted; obtain them
separately), the gitignored Zotero frame exports, the classifier's article-level
table with titles and abstracts (its `empirical` and `research_area` results are in
`data/full_sample_index.csv`), credentials, and everything under the repository's
`archive/`.

**Coder initials are pseudonymised.** In the three tables below, the `Coder Initials`
column is replaced by stable codes (`C01`, `C02`, …; same coder, same code across
files). All other bytes are unchanged, so every script gives the same results. SHA256
of the original files in the Git commit:

@@ORIGINAL_SHA256@@

## Reproduce the reported results (offline)

Unzip, then from the top-level folder (R 4.6.1 was used; packages in `README.md`):

```sh
Rscript validation/final_agreement.R          # agreement table below (read-only)
Rscript validation/pilot2_auto_vs_manual.R    # Pilot 2 statement presence
Rscript validation/plot_pilot_confusion.R     # all three figure versions
Rscript validation/validate_rx_vs_metacheck.R # XML-only benchmark
```

Run from a fresh copy, these reproduce the committed tables and PNG figures byte
for byte without network access (the PDF figures differ only in embedded dates). Re-running extraction (`PIPELINE_RUN=1 Rscript run_pipeline.R`, the
frame drivers, the TEI/preregistration stages and the PDF fallback) needs the TEI
and PDF full texts; `README.md` lists the expected locations. With those files and
the included caches, `run_pipeline(..., use_api = FALSE)` makes no network calls.

## Validation summary

All agreement figures compare automated output with the original manual codes.
They are development-sample agreement: the rules were refined after inspecting
these articles. No adjudicated or corrected labels are used.

| Final output vs original codes | Pilot 1 | Pilot 2 | Pooled |
|---|---|---|---|
| Funding category | 47/52 (90.4%) | 78/84 (92.9%) | 125/136 (91.9%) |
| COI category | 53/64 (82.8%) | 76/84 (90.5%) | 129/148 (87.2%) |
| COI statement presence | 59/64 (92.2%) | 80/84 (95.2%) | 139/148 (93.9%) |
| Preregistration | 60/64 (93.8%) | 79/84 (94.0%) | 139/148 (93.9%) |

XML-only statement detection, 148 pooled articles (`data/rx_vs_metacheck_metrics.csv`):
funding F1 0.956 (`metacheck` 0.1.0: 0.898); COI F1 0.914 (`metacheck`: 0.878).
Funder → country: 200 of 227 papers with a named funder (88%) get at least one
funder country.

## Known limits

- Pilot 2 article `10.1177/09567976231165271` was recovered on 19 November 2026
  (a bare `doi.org/` DOI prefix had excluded it). It is in the final tables, the TEI
  stage table and all comparisons above. The preregistration stage table
  (`data/stages/prereg_extracted_pilot1-and-pilot2.csv`) and the frame indexes
  (`data/pilot2_sample_index.csv` lists it as `pilot2-unsampled`;
  `data/full_sample_index.csv` counts 184 Pilot 2 articles) predate the recovery.
  Regenerating them needs the full texts and frame exports.
- The funder stage tables come from the 26 October 2026 run and have not been
  re-audited. Re-running `validation/pilot_link_funders.R` now changes one cell: the
  recovered article's `funding_status` becomes `funded` (as in the final table). The
  200/227 result is unchanged.
- With the source TEIs and PDFs, a cache-only `run_pipeline(..., use_api = FALSE,
  pdf_coi_fallback = TRUE)` reproduces all 185 coded Pilot 2 rows of the final table.
  Automatic PDF lookup by file name (`map_pdf_files()`) misses the PDF of
  `10.1037/pspi0000433`, whose TEI is named `….pdf.tei.xml`; pass its `pdf_file`
  explicitly. The Pilot 1 TEIs were not available for this check.
- `docs/README_prereg_extraction.md` reports module-level preregistration figures
  from that earlier stage run (147 papers, stricter filter).
- Final tables include short funding and COI statements quoted from the articles.

## Creators, licence and citation

Creators (provisional, to be confirmed before publication): Lukas Wallrich; Ilse L.
Pit. Licence: CC BY 4.0, as in the repository's `LICENSE`. Cite via `CITATION.cff` or
the DOI above, and cite the data sources you rely on (OpenAlex, Crossref, Scimago
SJR, TOP Factor, OSF, GROBID).
