# Pilot Meta-Extractor — current pipeline release

Version `@@VERSION@@`, built from commit `@@COMMIT@@` (@@DATE@@) of
https://github.com/LukasWallrich/pilot_meta_extracter.
DOI: 10.5281/zenodo.24262974.

This ZIP holds the current metadata-extraction pipeline for the Piloting Assessment
Registered Report, with the inputs, caches and outputs needed to check its reported
validation against manual coding in two pilot rounds. It is a curated subset of the
repository, listed in `release/MANIFEST.txt` of that commit. Historical and
exploratory material, including earlier runs over the full sampling frames, stays in
the repository's `archive/` folder.

## Contents (@@N_FILES@@ files)

- **Pipeline:** `run_pipeline.R` (entry point), `R/0_config.R` to `R/5_…` (modules),
  `pipeline.conf` (API contact address), `setup/` (TOP Factor data),
  `run_coi_pdf_fallback.R` (PDF front-page COI fallback),
  `screening/grobid_conversion_workflow.yml` (PDF → TEI conversion).
- **Validation:** `validation/` (agreement checks, stage reproductions, figures).
- **Data:** the final output for the 346 coded articles
  (`data/final_extraction_pilot1-and-pilot2.csv`), validation tables (`data/*.csv`),
  per-stage outputs (`data/stages/`), API caches (`data/cache/`), coding forms and
  historical inputs (`data/inputs/`), figures (`docs/figures/`).
- **Documentation:** `README.md`, `docs/README_*.md`, `CITATION.cff`, `LICENSE`.
- `SHA256SUMS.txt`: hash of every file in the ZIP.

**Not included:** article PDFs and GROBID TEI full texts (copyrighted; obtain them
separately), credentials, the working draft of the pilot write-up, and everything
under the repository's `archive/`.

**Coding tables.** The two coding forms and the 11 June 2026 combined table omit four
coding-process columns: form timestamp, coder initials, feedback on the coding form,
and time taken to code. No script uses them. All other fields are unchanged. SHA256
of the full files in the Git commit:

@@ORIGINAL_SHA256@@

## Reproduce the reported results (offline)

Unzip, then from the top-level folder (R 4.6.1 was used; packages in `README.md`):

```sh
Rscript validation/final_agreement.R          # agreement table below (read-only)
Rscript validation/pilot2_auto_vs_manual.R    # Pilot 2 statement presence
Rscript validation/plot_pilot_confusion.R     # all three figure versions
Rscript validation/validate_rx_vs_metacheck.R # XML-only benchmark
Rscript validation/pilot_link_funders.R       # funder -> country stage tables
```

Run from a fresh copy, these reproduce the committed tables and PNG figures byte for
byte without network access (the PDF figures differ only in embedded dates).

Re-running extraction (`PIPELINE_RUN=1 Rscript run_pipeline.R`, the TEI and
preregistration stages, and the PDF fallback) needs the TEI and PDF full texts at the
locations given in `README.md`. With those files and the included caches, a
cache-only run (`use_api = FALSE`) followed by `run_coi_pdf_fallback.R` reproduces
every cell of `data/final_extraction_pilot1-and-pilot2.csv` for 345 of the 346
articles (the recovered article was appended last, so row order differs). The
remaining article is `10.1037/pspi0000433` (see below).

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

## Notes and limits

- Pilot 2 article `10.1177/09567976231165271` was recovered on 19 November 2026 (a
  bare `doi.org/` DOI prefix had excluded it). It is in the final table, all stage
  tables and all comparisons above. Provenance:
  `data/inputs/recovered_tei/10.1177-09567976231165271.provenance.json`.
- The funder → country resolutions have not been re-audited since October 2026.
- The current Pilot 2 TEI folder holds two TEI files for `10.1037/pspi0000433`.
  Automatic PDF lookup by file name (`map_pdf_files()`) then misses its PDF, so the
  PDF fallback does not recover its COI statement ("no conflict declared" in the
  recorded output). Passing its `pdf_file` explicitly to `run_pipeline()` reproduces
  the recorded row exactly.
- The final table includes short funding and COI statements quoted from the articles.
- Before making new API calls, set your own contact address in `pipeline.conf`.

## Creators, licence and citation

Lukas Wallrich (Birkbeck, University of London; ORCID 0000-0003-2121-5177) and
Ilse L. Pit (Max Planck Institute for Human Development; ORCID 0000-0002-4066-8086).
Licence: CC BY 4.0, as in `LICENSE`. Cite via `CITATION.cff` or the DOI above, and
cite the data sources you rely on (OpenAlex, Crossref, Scimago SJR, TOP Factor, OSF,
GROBID).
