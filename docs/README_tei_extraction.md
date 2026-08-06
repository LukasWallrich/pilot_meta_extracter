# TEI-based COI and Funding Extraction

Extracts conflict of interest (COI) and funding statements from GROBID TEI XML files. By default uses direct XML/regex parsing, which outperforms metacheck on all metrics (see below). Optionally also runs [scienceverse/metacheck](https://github.com/scienceverse/metacheck) for comparison.

> **Module vs. driver.** `R/2_extract_tei_coi_funding.R` is now a **function library** (sourcing it only defines `heading_matches`, `clean_extracted_text`, `extract_regex`, `extract_metacheck` and the regex constants — no side effects). It is the single source of truth used by `run_pipeline.R`. The pilot-1 + pilot-2 batch run that writes `data/tei_coi_funding_extracted_pilot1-and-pilot2.csv` (and the pilot2-only `data/tei_coi_funding_extracted.csv`) plus the metacheck comparison lives in `validation/pilot_extract_tei.R` (run from the repo root; `use_metacheck` defaults to `TRUE` there so the stage CSV keeps its `mc_*` columns).

## Approach

### Metacheck

Uses `metacheck::read_grobid()` to parse each TEI file, then runs `module_run(paper, "funding_check")` and `module_run(paper, "coi_check")` to extract structured information. Returns binary flags and text for both funding and COI.

### Regex/XPath extraction

Directly parses the TEI XML with targeted XPath queries and regex heading/body matching. Handles several GROBID quirks discovered during validation:

**Funding extraction** (cascading strategy):
1. Structured `<funder>` elements in `<teiHeader>`
2. `<div type="funding">` in back matter — prefers leaf child divs over parent container to avoid concatenation with IRB/ethics/COI text
3. Grant numbers from `<listOrg type="funding">`
4. Heading-based search across all leaf divs (back + body) for headings matching funding/acknowledgment/author note patterns, with content validation for acknowledgment sections
5. Fallback: full-text scan for strong funding phrases ("funded by", "supported by a grant", etc.)

**COI extraction** (cascading strategy):
1. Heading-based search on leaf divs (back + body), with normalisation for GROBID's spaced-out headings (e.g., "CON F L IC T OF I N T ER E S T")
2. Parent div fallback with sentence-level extraction (only COI-matching sentences kept from long text blocks)
3. Embedded COI detection inside `<div type="funding">` paragraphs
4. Body-text scan in annex/author-notes child divs
5. Last resort: strong COI phrase scan across all divs

**Text cleaning**: Trailing boilerplate (ethics approvals, informed consent, ORCID blocks, author bios, Creative Commons license text) is trimmed from extracted statements. Open access funding mentions are preserved.

## Validation

Validated against manual coding from `data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv` (181 DOIs matched to TEI files out of 185). Of the 185 coded articles, only 84 had manual funding/COI ground truth — the remaining 101 were determined by coders to not contain a relevant pilot study, so the funding/COI questions were skipped.

### Detection performance (against 81 articles with manual ground truth)

|  | Accuracy | Precision | Recall | F1 |
|---|---|---|---|---|
| **Funding — Regex** | 0.963 | 0.957 | 1.000 | 0.978 |
| **COI — Regex** | 0.926 | 0.984 | 0.924 | 0.953 |

### Error breakdown

| | True FPs | TEI misses (GROBID failures) |
|---|---|---|
| **Funding** | 3 (3.7%) | 0 |
| **COI** | 1 (1.2%) | 5 |

The 3 funding FPs are: OA-fee-only acknowledgments (2 cases) and a CRediT author contributions section containing the word "funding" (1 case). The 1 COI FP is a paper where GROBID extracted the COI heading but lost the statement text. The 5 COI TEI misses are papers where GROBID failed to extract the COI statement from the PDF entirely.

### Development process

- First 20 DOIs used as dev set, remaining 159 as validation set
- Iterative improvement of regex strategy based on error analysis
- Key fixes discovered during development:
  - GROBID sometimes places funding/COI sections in `<body>` instead of `<back>`
  - Spaced-out headings from OCR artifacts require normalisation
  - COI text can be embedded inside funding paragraphs
  - Parent `<div type="annex/funding">` containers concatenate all children's text — leaf-div-first strategy avoids this

## Files

| File | Description |
|---|---|
| `R/2_extract_tei_coi_funding.R` | Production extraction script (metacheck + regex) |
| `scratch_dev_extract.R` | Development/validation script with comparison metrics |
| `data/tei_coi_funding_extracted.csv` | Extraction results (output of production script) |
| `data/extraction_mismatches.csv` | 7 DOIs with mismatches (among articles with manual ground truth), assessed as true FPs or TEI misses |
| `data/scratch_dev_results.csv` | Full results with manual ground truth (output of dev script) |

## Requirements

```r
dplyr
stringr
xml2
metacheck  # remotes::install_github("scienceverse/metacheck")
```

## Usage

```r
source("R/2_extract_tei_coi_funding.R")
# Reads pilot2 DOIs, matches to TEI files in ../pilot2_pdf_conversion/tei/,
# extracts using regex/XPath, saves to data/tei_coi_funding_extracted.csv

# To also run metacheck for comparison:
# Set use_metacheck <- TRUE before sourcing
```

To re-run validation against manual coding:
```r
# Set RUN_MODE env var: "all", "dev20", or "problem5"
# Rscript scratch_dev_extract.R
```
