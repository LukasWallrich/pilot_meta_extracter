#!/usr/bin/env bash
# Check a built release ZIP: unzip it into a fresh folder, run the offline entry
# points there, and compare every file with the pristine unzipped copy.
#   release/check_bundle.sh release/dist/pilot-meta-extractor-<version>.zip [WORKDIR]
# Optional: PILOT2_TEI_DIR=../pilot2_pdf_conversion/tei also runs a cache-only
# run_pipeline() on the coded Pilot 2 articles and compares it with the final table
# (rows whose COI came from the PDF fallback are excluded, since no PDFs are passed).
set -euo pipefail
zip=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
work=${2:-$(mktemp -d)}
mkdir -p "$work"
rm -rf "$work/pristine" "$work/run"
mkdir "$work/pristine" "$work/run"
unzip -q "$zip" -d "$work/pristine"
unzip -q "$zip" -d "$work/run"
top=$(ls "$work/run")
cd "$work/run/$top"
(cd "$work/pristine/$top" && shasum -a 256 -c SHA256SUMS.txt --quiet) && echo "SHA256SUMS: all files verified"

run() { echo "== Rscript $*"; Rscript "$@" > "$work/$(basename "$1" .R).log" 2>&1 || { tail -30 "$work/$(basename "$1" .R).log"; exit 1; }; }
run validation/final_agreement.R
cat "$work/final_agreement.log" | grep -A13 "sample"
run validation/pilot2_auto_vs_manual.R
run validation/plot_pilot_confusion.R
run validation/plot_pilot_categories.R
run validation/validate_rx_vs_metacheck.R
run validation/pilot_link_funders.R

if [ -n "${PILOT2_TEI_DIR:-}" ]; then
  echo "== cache-only run_pipeline() on coded Pilot 2 articles"
  Rscript - "$PILOT2_TEI_DIR" > "$work/smoke.log" 2>&1 <<'EOF' || { tail -30 "$work/smoke.log"; exit 1; }
args <- commandArgs(trailingOnly = TRUE)
source("run_pipeline.R")
m <- read.csv("data/inputs/2026-04-03_Codingform_PilotingAssessment_pilot2.csv", check.names = TRUE)
dois <- unique(norm_doi(m[["X1.2..Article.DOI"]])); dois <- dois[dois != "" & dois != "test"]
tei <- list.files(args[1], pattern = "\\.tei\\.xml$", full.names = TRUE)
idx <- match(doi_key(dois), tei_doi_key(tei))
res <- run_pipeline(data.frame(doi = dois[!is.na(idx)], tei_file = tei[idx[!is.na(idx)]]), use_api = FALSE)
fin <- readr::read_csv("data/final_extraction_pilot1-and-pilot2.csv", show_col_types = FALSE, col_types = readr::cols(.default = "c"))
fin <- fin[match(res$doi, fin$doi), ]
keep <- is.na(fin$coi_source) | fin$coi_source != "pdf_front"
res[] <- lapply(res, as.character)
cols <- setdiff(names(res), character(0))
diffs <- sapply(cols, function(cc) sum(!mapply(identical, res[[cc]][keep], fin[[cc]][keep])))
cat(sprintf("compared %d of %d articles (excluded %d PDF-fallback rows)\n", sum(keep), nrow(res), sum(!keep)))
print(diffs[diffs > 0])
if (any(diffs > 0)) quit(status = 1)
cat("all columns identical\n")
EOF
  tail -3 "$work/smoke.log"
fi

echo "== differences after running (empty = byte-identical)"
cd "$work"
changed=$(diff -rq "pristine/$top" "run/$top" || true)
echo "$changed" | grep -v '^$' | grep -v '\.pdf differ$' || echo "(none besides PDFs)"
echo "$changed" | grep '\.pdf differ$' | sed 's/^/PDF (embedded timestamps): /' || true
for f in $(cd "pristine/$top" && ls docs/figures/*.png); do
  cmp -s "pristine/$top/$f" "run/$top/$f" && echo "PNG identical: $f" || echo "PNG DIFFERS: $f"
done
echo "workdir: $work"
