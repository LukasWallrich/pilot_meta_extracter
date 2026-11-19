# Release area

This folder defines the current-only release bundle (for the Zenodo deposit) and
records which tracked files are current and which are historical.

- `MANIFEST.txt` is the explicit list of files in the bundle. Nothing else enters
  the ZIP.
- `build_release.py` builds the bundle from committed content. It refuses
  untracked paths, anything under `archive/` or `exploration/`, TEI files, source
  PDFs, absolute home paths and token-like strings.
- `check_bundle.sh` unzips a built bundle, reruns the offline entry points and
  compares every file with the unzipped original.
- `DEPOSIT_README.md` is the release README. It is copied into the ZIP as
  `RELEASE_README.md` and uploaded beside the ZIP.

## Build and check

From the repository root, on a clean working tree:

```sh
python3 release/build_release.py --out release/dist
release/check_bundle.sh release/dist/pilot-meta-extractor-<version>.zip
```

The builder writes `release/dist/pilot-meta-extractor-<version>.zip`,
`release/dist/SHA256SUMS.txt` (the ZIP and every file inside it) and
`release/dist/DEPOSIT_README.md`. `release/dist/` is gitignored. The ZIP is
deterministic: the same commit gives the same ZIP hash.

In the three coding tables, the builder drops four coding-process columns: the form
timestamp, coder initials, feedback on the coding form, and coding time. No bundled
script uses them. Every other field is copied unchanged, and the SHA256 of each
original Git file is recorded in the release README.

## Inventory (19 November 2026)

### Current and in the bundle

| Path | Role | Needs external full texts? |
|---|---|---|
| `run_pipeline.R`, `R/0_config.R` … `R/5_pdf_coi_fallback.R`, `pipeline.conf` | Generic pipeline: `run_pipeline()` on any `doi` + `tei_file` (+ optional `pdf_file`) table; `PIPELINE_RUN=1` runs the coded-pilot batch | Yes, for new runs |
| `setup/` | One-time TOP Factor reference-data setup | No |
| `data/cache/` | API caches; with them, `run_pipeline(use_api = FALSE)` runs offline for the recorded DOIs | — |
| `run_coi_pdf_fallback.R` | PDF front-page COI fallback over the coded-pilot table | Yes (PDFs) |
| `screening/grobid_conversion_workflow.yml` | How the TEIs were produced | Yes |
| `validation/final_agreement.R` | Headline agreement of the final output with the original manual codes | No |
| `validation/pilot2_auto_vs_manual.R` | Pilot 2 statement presence | No |
| `validation/plot_pilot_confusion.R` | Binary, three-category and mixed figures (sources `plot_pilot_categories.R`) | No |
| `validation/validate_rx_vs_metacheck.R` | XML-only extractor vs `metacheck` | No |
| `validation/pilot_link_funders.R` | Funder → country stage tables | No (uses caches) |
| `validation/pilot_extract_tei.R`, `validation/pilot_extract_prereg.R` | TEI and preregistration stage tables | Yes (TEI) |
| `validation/match_pilot1_tei.R` + `data/cache/pilot1_*` | Provenance of `data/inputs/pilot1_doi_tei_map.csv` | Yes (Pilot 1 TEI) |
| `validation/recover_missing_pilot2.R` | Reproduces the recovered Pilot 2 article | Yes (one XML, one PDF) |
| `validation/combine_with_manual.R` | Optional join of manual coding and final output (not run for this release) | No |
| `data/inputs/` | Coding forms, the historical 11 June 2026 combined table, the Pilot 1 DOI→TEI map, the recovery provenance record | — |
| `data/*.csv`, `data/stages/`, `docs/figures/` | Current outputs | — |

The historical input `data/inputs/2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv`
stays current: it holds the original Pilot 1 Crossref metadata behind the Pilot 1
Crossref-only comparison and the Crossref funder fields read by
`pilot_link_funders.R`.

### Current but not in the bundle

| Path | Reason |
|---|---|
| `docs/write-up-for-reg-rep.Rmd` | Working draft of the pilot write-up. Its figure code now lives in `validation/plot_pilot_confusion.R`, which the Rmd sources. |
| `release/`, `.gitignore` | Tooling and housekeeping |

### Archived

See `archive/README.md`. Summary: the sampling-frame runs (drivers, indexes,
extractions, keyword screen) and the LLM classifier, which the paper does not use;
the OpenAlex and OER experiments; the 7 November extraction report (n = 147); the
stale 2 July combined CSV; earlier combined datasets and retired dev scripts.

## Replacing the unpublished Zenodo draft

The reserved draft (DOI 10.5281/zenodo.24262974) held the 15 November 2026 package
(`pilot-meta-extractor-45fb68d.zip`, all 89 tracked files of commit `45fb68d`;
`DEPOSIT_README.md`; `manifest.json`). To replace it, rebuild from the merged commit,
run `check_bundle.sh`, then delete the three old files, upload the new ZIP,
`DEPOSIT_README.md` and `SHA256SUMS.txt`, and update the version, date, description,
creators and related identifier (`isDerivedFrom` the GitHub tree of the built
commit). Keep the same draft and DOI.
