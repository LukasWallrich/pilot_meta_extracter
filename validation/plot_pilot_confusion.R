# Render Ilse's manuscript figure from the write-up's own chunks.
# Pilot 1 retains the historical Crossref comparison; Pilot 2 uses final outputs.
# Run from the repo root: Rscript validation/plot_pilot_confusion.R
# No extraction/API calls: the extract-data-auto chunk is explicitly eval=FALSE.
stopifnot(!identical(Sys.getenv("PIPELINE_RUN"), "1"))
dir.create("docs/figures", recursive = TRUE, showWarnings = FALSE)
scratch <- tempfile("pilot-figure-")
dir.create(scratch)
knitr::opts_knit$set(root.dir = normalizePath("."))
knitr::opts_chunk$set(fig.path = paste0(scratch, "/"))
knitr::knit("docs/write-up-for-reg-rep.Rmd",
             output = file.path(scratch, "write-up.md"), quiet = TRUE,
             envir = new.env(parent = globalenv()))
message("Wrote docs/figures/pilot_extraction_confusion.{png,pdf}")
