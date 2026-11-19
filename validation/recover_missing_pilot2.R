# Reproduce the recovered single-paper output using the assigned source XML/PDF.
# Run from repo root: Rscript validation/recover_missing_pilot2.R XML_PATH PDF_PATH OUTPUT_CSV
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L, file.exists(args[1]), file.exists(args[2]))
source("run_pipeline.R")
stopifnot(identical(norm_doi(c("doi.org/10.1177/09567976231165271",
                             "https://doi.org/10.1177/09567976231165271",
                             "DOI:10.1177/09567976231165271")),
                    rep("10.1177/09567976231165271", 3)))
result <- run_pipeline(data.frame(doi = "10.1177/09567976231165271",
                                  tei_file = args[1], pdf_file = args[2]),
                       use_api = FALSE, pdf_coi_fallback = TRUE)
stopifnot(nrow(result) == 1L)
readr::write_csv(result, args[3])
