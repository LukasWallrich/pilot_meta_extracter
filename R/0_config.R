# Pipeline configuration -------------------------------------------------------
# Reads pipeline.conf (repository root; "key: value" lines, # comments) and
# defines CONTACT_EMAIL. Sourced by the modules that call external APIs, so the
# contact address is set in one place. Sourcing only defines constants.

read_pipeline_conf <- function(path = "pipeline.conf") {
  if (!file.exists(path))
    stop("pipeline.conf not found; run from the repository root.", call. = FALSE)
  x <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  x <- x[nzchar(x) & !startsWith(x, "#")]
  kv <- regmatches(x, regexpr(":", x), invert = TRUE)
  setNames(vapply(kv, function(p) trimws(p[2]), character(1)),
           vapply(kv, function(p) trimws(p[1]), character(1)))
}

CONTACT_EMAIL <- read_pipeline_conf()[["contact_email"]]
if (is.null(CONTACT_EMAIL) || !grepl("^[^@[:space:]]+@[^@[:space:]]+$", CONTACT_EMAIL))
  stop("pipeline.conf: set contact_email to a valid address.", call. = FALSE)
