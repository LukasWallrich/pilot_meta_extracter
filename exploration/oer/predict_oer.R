# FROZEN validation run (development set, n=71). See codex review + decisions:
#  - link-level, clause-bounded URL attribution (prereg at a different link excluded)
#  - keywords add only `pre-analysis plan` and scoped `(study|registered|..) protocol`
#  - LLM: trust explicit author label, judge THIS link, drop before-data test,
#    warn that the OSF description may describe the broader project
#  - binary (no unclear); provenance logged
# Generates oer_predictions.csv + oer_llm_provenance.csv. Scoring is separate.

suppressMessages({
  library(dplyr)
  devtools::load_all("/Users/lukaswallrich/Documents/Coding/metacheck", quiet = TRUE)
})
OER <- "/Users/lukaswallrich/Documents/Coding/pilot_meta_extracter/exploration/oer/"
scaf <- read.csv(paste0(OER, "oer_labeling_scaffold.csv"), stringsAsFactors = FALSE)
message(nrow(scaf), " OER cases")

# ---- frozen regexes -----------------------------------------------------------
URL_RE_ATTR <- "https?://[^\\s)\\]]+|osf\\s*\\.\\s*io\\s*/\\s*[a-z0-9]{5}|aspredicted\\.org/[^\\s)\\]]+"
PREREG_KW <- "pre[-\\s]*regist|aspredicted|registered on the osf|osf\\s+registration|time-?stamped (?:hypothes|preregist)|pre[-\\s]?analysis plan|(?:study|registered|trial)\\s+protocol"
NEG_PREREG <- "(\\bno(?:ne|t)?\\b|\\bnon-?\\b|\\bnever\\b|\\bwithout\\b|\\bneither\\b|wasn'?t|weren'?t|did not|was not|were not|not been)[^.;]{0,40}pre[-\\s]*regist|non-?pre[-\\s]*regist|pre[-\\s]*regist[^.;]{0,25}\\b(was|were)\\b[^.;]{0,15}\\b(not|none)\\b"

# clause containing THIS link, bounded by neighbouring URLs and punctuation
oer_attributed_span <- function(text, guid) {
  if (is.na(text) || !nzchar(text)) return(character(0))
  gpos <- gregexpr(tolower(guid), tolower(text), fixed = TRUE)[[1]]  # OSF.IO/2F6QS etc.
  if (gpos[1] == -1) return(character(0))
  um <- gregexpr(URL_RE_ATTR, text, perl = TRUE, ignore.case = TRUE)[[1]]
  uml <- attr(um, "match.length")
  urls <- if (um[1] == -1) list() else Map(function(s, l) c(s, s + l - 1), um, uml)
  if (length(urls)) urls <- urls[order(vapply(urls, `[`, numeric(1), 1))]
  masked <- text
  for (u in urls) substr(masked, u[1], u[2]) <- strrep(" ", u[2] - u[1] + 1)
  pun <- gregexpr("[.;!?]", masked)[[1]]; pun <- if (pun[1] == -1) integer(0) else pun  # ":" introduces links
  n <- nchar(text); spans <- character(0)
  is_conn <- function(ae, bs) {  # gap holds no words other than and/or/&/respectively
    if (bs <= ae + 1) return(TRUE)
    g <- tolower(substr(text, ae + 1, bs - 1))
    g <- gsub("&|\\b(and|or|respectively)\\b", "", g)
    nchar(gsub("[^a-z0-9]", "", g)) == 0
  }
  for (gp in gpos) {
    xi <- which(vapply(urls, function(u) u[1] <= gp && gp <= u[2], logical(1)))
    if (length(xi) == 0) {
      grp_l <- gp; grp_r <- gp + nchar(guid) - 1; left_url <- 0; right_url <- n + 1
    } else {
      xi <- xi[1]; li <- xi; ri <- xi
      while (li > 1 && is_conn(urls[[li - 1]][2], urls[[li]][1])) li <- li - 1
      while (ri < length(urls) && is_conn(urls[[ri]][2], urls[[ri + 1]][1])) ri <- ri + 1
      grp_l <- urls[[li]][1]; grp_r <- urls[[ri]][2]
      left_url <- if (li > 1) urls[[li - 1]][2] else 0
      right_url <- if (ri < length(urls)) urls[[ri + 1]][1] else n + 1
    }
    left_pun <- max(c(0, pun[pun < grp_l])); right_pun <- min(c(n + 1, pun[pun > grp_r]))
    lb <- max(left_url, left_pun) + 1; rb <- min(right_url, right_pun) - 1
    if (rb >= lb) spans <- c(spans, trimws(substr(text, lb, rb)))
  }
  unique(spans[nchar(spans) > 0])
}

has_nonneg_prereg <- function(spans) {
  hit <- spans[grepl(PREREG_KW, spans, perl = TRUE, ignore.case = TRUE)]
  if (!length(hit)) return(FALSE)
  any(!grepl(NEG_PREREG, hit, perl = TRUE, ignore.case = TRUE))
}

# full sentence(s) containing the link - for the LLM, which can attribute a
# coordinated shared subject ("The initial registration ... and an update ...")
# across a whole sentence while still judging this specific link
oer_link_sentence <- function(text, guid) {
  if (is.na(text) || !nzchar(text)) return(character(0))
  sents <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
  unique(trimws(sents[grepl(tolower(guid), tolower(sents), fixed = TRUE)]))
}

# manuscript evidence per link: attributed clause(s); fallback to paper prereg
# sentences when the link is not in the body text (low-confidence, flagged)
# containing sentence(s) - same context the LLM uses; when the link is not in the
# body, no manuscript evidence (the LLM then relies on the OSF description, the
# text-gate does not confirm)
ev_list <- lapply(seq_len(nrow(scaf)), function(i) {
  sp <- oer_link_sentence(scaf$link_context[i], scaf$guid[i])
  if (length(sp)) list(text = sp, src = "attributed") else list(text = character(0), src = "fallback")
})
ev_source <- vapply(ev_list, `[[`, character(1), "src")
textgate_pred <- vapply(ev_list, function(e) has_nonneg_prereg(e$text), logical(1))

# ---- LLM ----------------------------------------------------------------------
llm_use(TRUE); LLM_MODEL <- "openai/gpt-5-mini"; llm_model(LLM_MODEL); llm_max_calls(200)
PROMPT_VERSION <- "v3-demote-desc"
system_prompt <- paste(
  "Decide whether a specific OSF link is used as a PREREGISTRATION or is just an ARCHIVE of data/materials.",
  "Base the decision primarily on the MANUSCRIPT text describing this link:",
  "(1) If the manuscript describes this link's content (data, code, materials, supplement) and does NOT call it",
  "a preregistration, pre-analysis plan, or study/registered protocol, answer FALSE - even if the OSF description",
  "mentions a preregistration, which may refer to the broader project or a different link.",
  "(2) If the manuscript labels this link a preregistration / pre-analysis plan / protocol, answer TRUE; trust the",
  "authors' explicit label (do not judge whether it was methodologically ideal).",
  "(3) Only if the manuscript text is uninformative about this link (e.g. a bare URL) fall back to the OSF",
  "description: answer TRUE if that description is itself a preregistration (states hypotheses and/or an analysis",
  "plan set in advance) or labels the object as one; otherwise FALSE.",
  "Answer FALSE if the preregistration mention is negated. Answer with exactly one word: TRUE or FALSE."
)
inputs <- vapply(seq_len(nrow(scaf)), function(i) {
  ev <- paste(oer_link_sentence(scaf$link_context[i], scaf$guid[i]), collapse = " ")
  paste0("OSF object description: ", scaf$oer_summary[i],
         "\n\nManuscript text describing THIS link: ", ev)
}, character(1))

res <- llm(inputs, system_prompt, params = list(seed = 1))
llm_raw <- trimws(res$answer)
llm_pred <- grepl("^TRUE", toupper(llm_raw))

# ---- write predictions + provenance ------------------------------------------
now <- as.character(Sys.time())
out <- data.frame(tei_file = scaf$tei_file, doi = scaf$doi, guid = scaf$guid,
                  ev_source = ev_source, textgate_pred = textgate_pred,
                  llm_pred = llm_pred, llm_raw = llm_raw, stringsAsFactors = FALSE)
write.csv(out, paste0(OER, "oer_predictions.csv"), row.names = FALSE)
prov <- data.frame(guid = scaf$guid, model = LLM_MODEL, prompt_version = PROMPT_VERSION,
                   input = inputs, llm_raw = llm_raw, run_at = now, stringsAsFactors = FALSE)
write.csv(prov, paste0(OER, "oer_llm_provenance.csv"), row.names = FALSE)

cat("\n== FROZEN run ==\nmodel:", LLM_MODEL, "| prompt:", PROMPT_VERSION, "\n")
cat("evidence: attributed=", sum(ev_source == "attributed"),
    " fallback=", sum(ev_source == "fallback"), "\n")
cat("text-gate prereg:", sum(textgate_pred), "/", nrow(out),
    " | LLM prereg:", sum(llm_pred), "/", nrow(out),
    " | disagree:", sum(textgate_pred != llm_pred), "\n")
