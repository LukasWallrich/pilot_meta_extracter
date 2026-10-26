# PDF conversion and pilot-keyword screening

Copies of the two steps that run before the extraction pipeline. They live in the
private `pilot2_pdf_conversion` repository, which also holds the converted full
texts; those are not redistributed here.

| File | Role |
|---|---|
| `grobid_conversion_workflow.yml` | GitHub Actions workflow that downloads the article PDFs from a project Google Drive folder and converts them to TEI XML with GROBID 0.8.2.1 (`grobid/grobid:0.8.2.1-full`). Kept for documentation; it needs the Drive service-account secret to run. |
| `search_pilot_keywords.R` | Reads the TEI files with `metacheck::read()`, drops reference-list sections and flags articles whose remaining text matches `pilot`, `feasibility study` or `preliminary study` (case-insensitive substring match). Run from the conversion repository: `Rscript search_pilot_keywords.R --tei-dir=tei`. |
| `pilot2_keyword_screening.csv` | Output of that script for the 4,635 pilot-2 TEI files (run 19 August 2026): 718 articles with at least one in-text hit. `run_pilot2_sample.R` reads it to fill `keyword_hit`. |

This screen was run after pilot-2 coding, to compare pilot-reporting and other
articles. The pilot-2 coding sample itself (714 articles) was drawn by the
database workflow, so the two counts differ slightly.

Because the match is a substring match, `pilot` also catches `piloted`,
`piloting` and unrelated words such as `autopilot`, while `feasibility study`
and `preliminary study` miss the plurals `studies`.
