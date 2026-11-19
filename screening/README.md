# PDF conversion

Copy of the step that turns article PDFs into the GROBID TEI files the pipeline
reads. It lives in the private `pilot2_pdf_conversion` repository, which also
holds the converted full texts; those are not redistributed here.

| File | Role |
|---|---|
| `grobid_conversion_workflow.yml` | GitHub Actions workflow that downloads the article PDFs from a project Google Drive folder and converts them to TEI XML with GROBID 0.8.2.1 (`grobid/grobid:0.8.2.1-full`). Kept for documentation; it needs the Drive service-account secret to run. |

The pilot-keyword screen and the sampling-frame runs that used it are in
`archive/frame_runs/`.
