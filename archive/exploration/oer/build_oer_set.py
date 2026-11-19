#!/usr/bin/env python3
"""
Build an OSF Open-Ended Registration (OER) validation set from the existing
4079-TEI psychology corpus (../pilot_pilot_teis/tei).

For each TEI: strip the reference list, extract OSF guids from the body
(text + <ptr>/<ref target>), resolve each guid to an OSF object type and (for
registrations) a template via the OSF API. Output the OER-linked papers.

Reuses + extends the pilot caches (data/cache/osf_guid_types.csv,
data/cache/osf_reg_templates.csv) so it is resumable and makes no redundant calls.

Outputs:
  data/oer_candidates.csv  one row per (tei_file, guid, type, template)
                           for every OSF *registration* guid linked by a paper,
                           with oer = TRUE for Open-Ended Registrations.
"""
import csv, glob, os, re, sys, time, json
import urllib.request, urllib.error
import xml.etree.ElementTree as ET

TEI_DIR   = "../pilot_pilot_teis/tei"
TYPE_CACHE = "data/cache/osf_guid_types.csv"
TMPL_CACHE = "data/cache/osf_reg_templates.csv"
OUT_CSV    = "data/oer_candidates.csv"
LINKS_CSV  = "data/paper_osf_links.csv"   # full paper<->guid map (audit trail)
UA = "metacheck-oer-validation (research; lukas.wallrich@gmail.com)"
DELAY = 0.34

GUID_RE = re.compile(r'osf\.io/(?:project/)?([a-z0-9]{5})\b', re.I)
NS = {"t": "http://www.tei-c.org/ns/1.0"}


def extract_guids(path):
    """Return set of osf guids in the paper body (reference list removed)."""
    try:
        tree = ET.parse(path)
    except Exception:
        # fall back to regex on raw text if XML is malformed
        try:
            txt = open(path, encoding="utf-8", errors="ignore").read()
        except Exception:
            return set()
        return set(m.group(1).lower() for m in GUID_RE.finditer(txt))
    root = tree.getroot()
    # remove bibliography (listBibl) to avoid cited works' links
    for parent in root.iter():
        for child in list(parent):
            tag = child.tag.split("}")[-1]
            if tag == "listBibl":
                parent.remove(child)
    txt = " ".join(root.itertext())
    guids = set(m.group(1).lower() for m in GUID_RE.finditer(txt))
    # also ptr/ref target attributes
    for el in root.iter():
        tag = el.tag.split("}")[-1]
        if tag in ("ptr", "ref"):
            tgt = el.get("target", "")
            for m in GUID_RE.finditer(tgt):
                guids.add(m.group(1).lower())
    return guids


def load_cache(path, key, cols):
    d = {}
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            for row in csv.DictReader(f):
                d[row[key]] = row
    return d


def osf_get(url):
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return r.status, json.load(r)
    except urllib.error.HTTPError as e:
        return e.code, None
    except Exception:
        return None, None


def main():
    files = [f for f in glob.glob(os.path.join(TEI_DIR, "*")) if os.path.isfile(f)]
    print(f"scanning {len(files)} TEI files (refs stripped)...", flush=True)
    paper_guids = {}
    for i, f in enumerate(files):
        g = extract_guids(f)
        if g:
            paper_guids[os.path.basename(f)] = g
        if (i + 1) % 500 == 0:
            print(f"  {i+1}/{len(files)}", flush=True)
    all_guids = sorted(set().union(*paper_guids.values())) if paper_guids else []
    print(f"papers with OSF links (refs stripped): {len(paper_guids)}; "
          f"unique guids: {len(all_guids)}", flush=True)

    # write audit map
    with open(LINKS_CSV, "w", newline="") as o:
        w = csv.writer(o); w.writerow(["tei_file", "guid"])
        for fn, gs in sorted(paper_guids.items()):
            for g in sorted(gs):
                w.writerow([fn, g])

    # ---- resolve types ----
    type_cache = load_cache(TYPE_CACHE, "code", None)
    todo = [g for g in all_guids if g not in type_cache]
    print(f"resolving {len(todo)} new guid types (have {len(type_cache)} cached)...", flush=True)
    tf = open(TYPE_CACHE, "a", newline="")
    tw = csv.writer(tf)
    if os.path.getsize(TYPE_CACHE) == 0:
        tw.writerow(["code", "osf_type", "queried_at"])
    for i, g in enumerate(todo):
        st, j = osf_get(f"https://api.osf.io/v2/guids/{g}/")
        if st == 200 and j:
            ty = (j.get("data") or {}).get("type", "unknown")
        elif st in (401, 403):
            ty = "private"
        else:
            ty = "unknown"
        type_cache[g] = {"code": g, "osf_type": ty}
        tw.writerow([g, ty, time.strftime("%Y-%m-%d %H:%M:%S")]); tf.flush()
        if (i + 1) % 50 == 0:
            print(f"  types {i+1}/{len(todo)}", flush=True)
        time.sleep(DELAY)
    tf.close()

    reg_guids = [g for g in all_guids if type_cache.get(g, {}).get("osf_type") == "registrations"]
    print(f"registration guids: {len(reg_guids)}", flush=True)

    # ---- resolve templates ----
    tmpl_cache = load_cache(TMPL_CACHE, "code", None)
    todo = [g for g in reg_guids if g not in tmpl_cache]
    print(f"resolving {len(todo)} new registration templates (have {len(tmpl_cache)} cached)...", flush=True)
    mf = open(TMPL_CACHE, "a", newline="")
    mw = csv.writer(mf)
    if os.path.getsize(TMPL_CACHE) == 0:
        mw.writerow(["code", "template", "category", "title"])
    for i, g in enumerate(todo):
        st, j = osf_get(f"https://api.osf.io/v2/registrations/{g}/")
        tmpl = cat = title = ""
        if st == 200 and j:
            a = (j.get("data") or {}).get("attributes") or {}
            tmpl = a.get("registration_supplement") or ""
            cat = a.get("category") or ""
            title = (a.get("title") or "").replace("\n", " ")
        tmpl_cache[g] = {"code": g, "template": tmpl, "category": cat, "title": title}
        mw.writerow([g, tmpl, cat, title]); mf.flush()
        if (i + 1) % 25 == 0:
            print(f"  templates {i+1}/{len(todo)}", flush=True)
        time.sleep(DELAY)
    mf.close()

    # ---- build output: every registration guid linked by a paper ----
    rows = []
    for fn, gs in paper_guids.items():
        for g in gs:
            if type_cache.get(g, {}).get("osf_type") != "registrations":
                continue
            tc = tmpl_cache.get(g, {})
            tmpl = tc.get("template", "")
            rows.append({
                "tei_file": fn, "guid": g, "type": "registrations",
                "template": tmpl, "title": tc.get("title", ""),
                "oer": str(tmpl == "Open-Ended Registration"),
            })
    with open(OUT_CSV, "w", newline="") as o:
        w = csv.DictWriter(o, fieldnames=["tei_file", "guid", "type", "template", "title", "oer"])
        w.writeheader(); w.writerows(rows)

    n_oer = sum(1 for r in rows if r["oer"] == "True")
    n_oer_papers = len(set(r["tei_file"] for r in rows if r["oer"] == "True"))
    print("\n==== RESULT ====", flush=True)
    print(f"registration-linked rows: {len(rows)}", flush=True)
    print(f"Open-Ended Registration rows: {n_oer}  (papers: {n_oer_papers})", flush=True)
    from collections import Counter
    print("template distribution:")
    for t, c in Counter(r["template"] for r in rows).most_common():
        print(f"  {c:4d}  {t or '(empty)'}")


if __name__ == "__main__":
    main()
