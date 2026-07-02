#!/usr/bin/env python3
"""
Build a manual-labeling scaffold for the OER validation set.

Input : data/oer_candidates.csv (rows with oer == True)
Output: data/oer_labeling_scaffold.csv

One row per (paper, OER). Gives the human coder full sentence context around the
OSF link in the manuscript, plus the OER's own content, so they can decide
whether the paper used that registration as a study PREREGISTRATION or as a
data/materials ARCHIVE. Model predictions are deliberately EXCLUDED to avoid
anchoring; they are joined in later for scoring.

Columns:
  tei_file, doi, guid, osf_url, oer_title, oer_summary,
  link_context (the sentence with the link + neighbours),
  prereg_sentences (manuscript sentences mentioning preregistration),
  label  <- BLANK: coder enters 'prereg' or 'archive' (or 'unclear')
  notes  <- BLANK
"""
import csv, os, re, json, time
import urllib.request, urllib.error
import xml.etree.ElementTree as ET

TEI_DIR = "../pilot_pilot_teis/tei"
IN_CSV  = "data/oer_candidates.csv"
SUMMARY_CACHE = "data/osf_oer_summaries.csv"
OUT_CSV = "data/oer_labeling_scaffold.csv"
UA = "metacheck-oer-validation (research; lukas.wallrich@gmail.com)"
DELAY = 0.34

PREREG_RE = re.compile(r"pre[-\s]*regist|aspredicted", re.I)


def sentences(text):
    parts = re.split(r"(?<=[.!?])\s+", text)
    return [s.strip() for s in parts if s.strip()]


def tei_body_and_doi(path):
    try:
        tree = ET.parse(path); root = tree.getroot()
    except Exception:
        txt = open(path, encoding="utf-8", errors="ignore").read()
        m = re.search(r"10\.\d{4,}/[^\s\"<]+", txt)
        return txt, (m.group(0) if m else "")
    # DOI from idno
    doi = ""
    for el in root.iter():
        if el.tag.split("}")[-1] == "idno" and (el.get("type", "").upper() == "DOI"):
            doi = (el.text or "").strip(); break
    # strip bibliography
    for parent in root.iter():
        for child in list(parent):
            if child.tag.split("}")[-1] == "listBibl":
                parent.remove(child)
    body = " ".join(root.itertext())
    body = re.sub(r"\s+", " ", body)
    if not doi:
        m = re.search(r"10\.\d{4,}/[^\s\"<]+", body)
        doi = m.group(0) if m else ""
    return body, doi


def link_context(body, guid):
    sents = sentences(body)
    out = []
    pat = re.compile(re.escape("osf.io/") + r"(?:project/)?" + re.escape(guid), re.I)
    for i, s in enumerate(sents):
        if pat.search(s):
            ctx = sents[max(0, i - 1): i + 2]
            out.append(" ".join(ctx))
    return "\n---\n".join(dict.fromkeys(out))  # dedupe, keep order


def prereg_context(body):
    sents = sentences(body)
    hits = [s for s in sents if PREREG_RE.search(s)]
    seen, out = set(), []
    for s in hits:
        if s not in seen:
            seen.add(s); out.append(s)
    return "\n".join(out[:8])


def load_summary_cache():
    d = {}
    if os.path.exists(SUMMARY_CACHE):
        with open(SUMMARY_CACHE, encoding="utf-8") as f:
            for row in csv.DictReader(f):
                d[row["guid"]] = row
    return d


def fetch_oer_summary(guid):
    url = f"https://api.osf.io/v2/registrations/{guid}/"
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            j = json.load(r)
    except Exception:
        return {"guid": guid, "title": "", "summary": ""}
    a = (j.get("data") or {}).get("attributes") or {}
    rr = a.get("registration_responses") or {}
    summary = rr.get("summary") or a.get("description") or ""
    return {"guid": guid, "title": (a.get("title") or "").replace("\n", " "),
            "summary": re.sub(r"\s+", " ", str(summary))}


def main():
    with open(IN_CSV, encoding="utf-8") as f:
        oer_rows = [r for r in csv.DictReader(f) if r["oer"] == "True"]
    print(f"OER rows: {len(oer_rows)}")

    sumcache = load_summary_cache()
    need = sorted(set(r["guid"] for r in oer_rows) - set(sumcache))
    print(f"fetching {len(need)} OER summaries...")
    if need:
        sf = open(SUMMARY_CACHE, "a", newline="")
        sw = csv.writer(sf)
        if os.path.getsize(SUMMARY_CACHE) == 0:
            sw.writerow(["guid", "title", "summary"])
        for g in need:
            s = fetch_oer_summary(g)
            sumcache[g] = s
            sw.writerow([s["guid"], s["title"], s["summary"]]); sf.flush()
            time.sleep(DELAY)
        sf.close()

    body_cache = {}
    out = []
    for r in oer_rows:
        fn, guid = r["tei_file"], r["guid"]
        path = os.path.join(TEI_DIR, fn)
        if fn not in body_cache:
            body_cache[fn] = tei_body_and_doi(path)
        body, doi = body_cache[fn]
        sm = sumcache.get(guid, {"title": r.get("title", ""), "summary": ""})
        out.append({
            "tei_file": fn,
            "doi": doi,
            "guid": guid,
            "osf_url": f"https://osf.io/{guid}",
            "oer_title": sm.get("title", ""),
            "oer_summary": sm.get("summary", ""),
            "link_context": link_context(body, guid),
            "prereg_sentences": prereg_context(body),
            "label": "",
            "notes": "",
        })

    cols = ["tei_file", "doi", "guid", "osf_url", "oer_title", "oer_summary",
            "link_context", "prereg_sentences", "label", "notes"]
    with open(OUT_CSV, "w", newline="") as o:
        w = csv.DictWriter(o, fieldnames=cols)
        w.writeheader(); w.writerows(out)
    print(f"wrote {len(out)} rows -> {OUT_CSV}")
    n_no_ctx = sum(1 for r in out if not r["link_context"])
    print(f"rows with no in-text link sentence found: {n_no_ctx} "
          f"(link may be in a footnote/table or only in ref target)")


if __name__ == "__main__":
    main()
