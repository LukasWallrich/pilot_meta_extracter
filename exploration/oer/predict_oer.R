# Run the considered text-gate and LLM classifier on the 71 OER validation cases.
# Method (fixed on mechanism before unblinding):
#   manuscript_ev = link context (sentences around THIS registration's link),
#                   falling back to the paper's prereg-mention sentences if absent.
#   text-gate = a non-negated preregistration statement in manuscript_ev.
#   LLM       = OER description + manuscript_ev, classified prereg vs archive.
# Output: data/oer_predictions.csv  (joined to manual labels later for scoring)

suppressMessages({
  library(dplyr)
  devtools::load_all("../metacheck", quiet = TRUE)
})

scaffold <- read.csv("data/oer_labeling_scaffold.csv", stringsAsFactors = FALSE)
message(nrow(scaffold), " OER cases")

# manuscript evidence: link context, else paper prereg sentences ----
manuscript_ev <- ifelse(
  nzchar(trimws(scaffold$link_context)),
  scaffold$link_context,
  scaffold$prereg_sentences
)
ev_source <- ifelse(nzchar(trimws(scaffold$link_context)), "link", "paper_fallback")

# ---- text-gate (ported pilot regexes; refs already excluded upstream) --------
PREREG_KW <- "pre[-\\s]*regist|aspredicted|registered on the osf|osf\\s+registration|time-?stamped (?:hypothes|preregist)"
NEG_PREREG <- "(\\bnot\\b|\\bno\\b|\\bnone\\b|\\bnon-?\\b|\\bnever\\b|\\bwithout\\b|\\bneither\\b|wasn'?t|weren'?t|did not|was not|were not|not been)[^.;]{0,40}pre[-\\s]*regist|non-?pre[-\\s]*regist|pre[-\\s]*regist[^.;]{0,25}\\b(was|were)\\b[^.;]{0,15}\\b(not|none)\\b"

textgate_pred <- vapply(manuscript_ev, function(txt) {
  if (is.na(txt) || !nzchar(trimws(txt))) return(FALSE)
  sents <- unlist(strsplit(txt, "(?<=[.!?])\\s+|\n|---", perl = TRUE))
  sents <- sents[grepl(PREREG_KW, sents, perl = TRUE, ignore.case = TRUE)]
  if (!length(sents)) return(FALSE)
  any(!grepl(NEG_PREREG, sents, perl = TRUE, ignore.case = TRUE))
}, logical(1), USE.NAMES = FALSE)

# ---- LLM: OER description + manuscript context -------------------------------
llm_use(TRUE)
LLM_MODEL <- "openai/gpt-5-mini"
llm_model(LLM_MODEL)
llm_max_calls(200)

system_prompt <- paste(
  "You decide whether an OSF Open-Ended Registration is a genuine study PREREGISTRATION",
  "(hypotheses and/or an analysis plan registered before the data were analysed) or merely an",
  "ARCHIVE of data or materials. Use both the registration's own description and what the",
  "manuscript says about it; if the manuscript states the study was not preregistered, answer FALSE.",
  "Answer with exactly one word: TRUE for a preregistration, FALSE for an archive."
)

inputs <- sprintf(
  "OSF Open-Ended Registration\nRegistration description: %s\n\nManuscript context around the registration link: %s",
  scaffold$oer_summary, manuscript_ev
)

res <- llm(inputs, system_prompt, params = list(seed = 1))
llm_raw <- trimws(res$answer)
llm_pred <- grepl("TRUE", toupper(llm_raw))

out <- data.frame(
  tei_file = scaffold$tei_file, doi = scaffold$doi, guid = scaffold$guid,
  ev_source = ev_source, textgate_pred = textgate_pred,
  llm_pred = llm_pred, llm_raw = llm_raw, stringsAsFactors = FALSE
)
write.csv(out, "data/oer_predictions.csv", row.names = FALSE)

cat("\n==== prediction summary (no ground truth) ====\n")
cat("LLM model:", LLM_MODEL, "\n")
cat("evidence from link context:", sum(ev_source == "link"),
    " / paper fallback:", sum(ev_source == "paper_fallback"), "\n")
cat("text-gate -> preregistration:", sum(textgate_pred), "/", nrow(out), "\n")
cat("LLM       -> preregistration:", sum(llm_pred), "/", nrow(out), "\n")
cat("agree:", sum(textgate_pred == llm_pred), " disagree:", sum(textgate_pred != llm_pred), "\n\n")
disagree <- out[out$textgate_pred != out$llm_pred, c("doi", "guid", "ev_source", "textgate_pred", "llm_pred")]
cat("Disagreements:\n"); print(disagree, row.names = FALSE)
