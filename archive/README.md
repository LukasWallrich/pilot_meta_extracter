# Archive

Historical, experimental and superseded material, kept in Git for provenance. Nothing
here is used by the current pipeline or validation scripts, and none of it is in the
release bundle (`release/MANIFEST.txt`). Files are kept as they were last used:
scripts may refer to paths from before they were moved here, and their outputs may
not match the current pipeline.

Earlier states of every file are in the Git history.

## Sampling-frame runs (`frame_runs/`)

Extractions over the full Pilot 1 and Pilot 2 sampling frames, built to compare
pilot-reporting and other articles. The paper does not report them; the main study
will use the classified article list supplied by the database team.

| Path | What it is |
|---|---|
| `frame_runs/run_full_sample.R` | Driver for the Pilot 1 frame (`full_sample.csv`, 4,076 articles): extracted articles the LLM classifier called empirical, plus every coded article. Reads `screening/classification/classified_articles.csv`, a path from before the move (now `archive/exploration/classification/`). |
| `frame_runs/run_pilot2_sample.R` | Driver for the Pilot 2 frame (`pilot2_sample.csv`, 4,635 articles), unfiltered. |
| `frame_runs/data/full_sample_index.csv`, `frame_runs/data/final_extraction_full-empirical-sample.csv` | Pilot 1 frame index (with `empirical`, `research_area`, `keyword_hit`, `article_status`) and extraction (4,051 rows). |
| `frame_runs/data/pilot2_sample_index.csv`, `frame_runs/data/final_extraction_pilot2-sample.csv` | Pilot 2 frame index and extraction (4,635 rows). |
| `frame_runs/screening/search_pilot_keywords.R`, `frame_runs/screening/pilot2_keyword_screening.csv` | Pilot-keyword screen of the Pilot 2 frame TEIs (19 August 2026; 718 of 4,635 with a hit). |

Known state when archived: both frame indexes predate the recovery of Pilot 2
article `10.1177/09567976231165271` (the Pilot 2 index lists it as
`pilot2-unsampled`; the Pilot 1 index appends 184 rather than 185 coded Pilot 2
articles), and the Pilot 1 frame extraction lacks that article.

## Superseded data

| Path | What it is | Superseded by |
|---|---|---|
| `2025-12-09_Codingform_ PilotingAssessment_pilot.csv` | Earlier export of the Pilot 1 coding form (differs from the current copy). | `data/inputs/2025-12-09_Codingform_PilotingAssessment_pilot.csv` |
| `2026-04-03_auto-and-manual-combined_pilot1-and-pilot2.csv`, `2026-04-03_auto-and-manual-combined_pilot2.csv` | April 2026 joins of manual coding and API metadata, written by `compare_manual_auto.R`. | `data/inputs/2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv` (historical input still in use) and the final extraction table |
| `auto-and-manual-combined.csv` | Earliest combined auto/manual table. | As above |
| `extraction_mismatches.csv` | Development-phase list of 7 Pilot 2 funding/COI mismatches. | `data/rx_vs_metacheck_metrics.csv`, `data/pilot2_final_statement_comparison.csv` |
| `data/manual-and-auto-combined_2026-07-01.csv` | Output of `validation/combine_with_manual.R` from 2 July 2026. Predates later rule changes, the PDF COI fallback and the recovered Pilot 2 article. | Rerun `Rscript validation/combine_with_manual.R` (now writes `data/manual-and-auto-combined.csv`) |

## Retired scripts

| Path | What it is |
|---|---|
| `compare_manual_auto.R` | Early join of manual coding with automated metadata. |
| `dev_extract_pilot2_first20.R` | Development script for the TEI extractor (first 20 Pilot 2 DOIs). |

## Superseded reports

| Path | What it is |
|---|---|
| `docs/extraction_report.qmd`, `docs/extraction_report.html` | Method, validation and coverage report rendered on 7 November 2026, covering both sampling frames. Its pooled counts use 147 coded papers, before the recovery of Pilot 2 article `10.1177/09567976231165271`. Current figures: `README.md` and `docs/README_pipeline.md`. |

## Exploration (side experiments, not part of the pipeline)

| Path | What it is |
|---|---|
| `exploration/classification/` | LLM classifier (`gpt-5-mini` via `ellmer`) of `full_sample.csv` articles into empirical/non-empirical and a psychology subfield; `classified_articles.csv` holds titles, abstracts and the labels. Used only by the archived Pilot 1 frame run. |
| `exploration/OA fields/` | OpenAlex field/subfield probe for journals (`report.Rmd`, `ASJC_fields.csv`). |
| `exploration/openalex_subfield_review/` | OpenAlex subfield review of 2024 journal articles (`report.Rmd`, `report.html`) and its helper `openalex_utils.R` (formerly `validation/report.*` and `R/openalex_utils.R`). `report.Rmd` still sources `R/openalex_utils.R`. |
| `exploration/oer/` | Open-ended registration labelling experiment (LLM predictions and scaffolds). Scripts contain absolute paths from the original machine. |
