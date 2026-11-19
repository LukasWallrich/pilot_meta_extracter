#!/usr/bin/env python3
"""Build the current-only release ZIP from committed content.

Usage (repository root, clean working tree):
    python3 release/build_release.py --out release/dist

Reads release/MANIFEST.txt, copies each [include] path from HEAD (not from the
working tree), pseudonymises the Coder Initials column where the manifest says so,
adds release/DEPOSIT_README.md as RELEASE_README.md, and writes a deterministic
ZIP, SHA256SUMS.txt and DEPOSIT_README.md to --out. Standard library only.
"""
import argparse
import csv
import hashlib
import io
import pathlib
import re
import subprocess
import sys
import zipfile

ROOT = pathlib.Path(__file__).resolve().parent.parent
MANIFEST = "release/MANIFEST.txt"
README_TEMPLATE = "release/DEPOSIT_README.md"
CODER_COLUMNS = {"1.1. Coder Initials", "X1.1..Coder.Initials"}
ZIP_TIME = (2026, 1, 1, 0, 0, 0)

FORBIDDEN_PATH = [
    (re.compile(r"^(archive|exploration)/"), "archive or exploration path"),
    (re.compile(r"\.tei\.xml$", re.I), "TEI full text"),
    (re.compile(r"(^|/)(\.env|.*\.env|api_keys.*|\.Renviron|\.Rprofile)$", re.I), "credentials file"),
    (re.compile(r"(^|/)(scratch|\.)[^/]*$"), "scratch or hidden file"),
]
FORBIDDEN_TEXT = [
    (re.compile(rb"/Users/[A-Za-z]"), "absolute home path"),
    (re.compile(rb"CloudStorage"), "machine-specific cloud path"),
    (re.compile(rb"(sk-[A-Za-z0-9_-]{20,}|ghp_[A-Za-z0-9]{20,}|AKIA[0-9A-Z]{16})"), "API token"),
    (re.compile(rb"(api[_-]?key|secret|token)\s*[=:]\s*['\"][A-Za-z0-9_\-]{16,}['\"]", re.I), "hard-coded key"),
]


def git(*args, binary=False):
    out = subprocess.run(["git", *args], cwd=ROOT, check=True, capture_output=True)
    return out.stdout if binary else out.stdout.decode()


def fail(msg):
    sys.exit(f"build_release: {msg}")


def read_manifest(text):
    include, exclude, section = [], {}, None
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        if line in ("[include]", "[exclude]"):
            section = line
            continue
        path, _, rest = line.partition("  ")
        if section == "[include]":
            include.append((path, rest.strip() == "pseudonymise"))
        elif section == "[exclude]":
            exclude[path] = rest.strip()
        else:
            fail(f"manifest line outside a section: {line}")
    return include, exclude


def split_records(text):
    """Yield (raw_fields, separators) per CSV record, preserving every byte."""
    i, n = 0, len(text)
    fields, seps = [], []
    while i < n:
        if text[i] == '"':
            j = i + 1
            while True:
                j = text.find('"', j)
                if j < 0:
                    fail("unterminated quoted CSV field")
                if text[j + 1:j + 2] == '"':
                    j += 2
                    continue
                j += 1
                break
        else:
            m = re.compile(r"[,\r\n]").search(text, i)
            j = m.start() if m else n
        fields.append(text[i:j])
        i = j
        if i >= n:
            seps.append("")
            break
        if text[i] == ",":
            seps.append(",")
            i += 1
            if i == n:
                fields.append("")
                seps.append("")
            continue
        sep = "\r\n" if text.startswith("\r\n", i) else text[i]
        seps.append(sep)
        i += len(sep)
        yield fields, seps
        fields, seps = [], []
    if fields:
        yield fields, seps


def unquote(raw):
    return raw[1:-1].replace('""', '"') if raw.startswith('"') else raw


def pseudonymise(data, path, mapping):
    text = data.decode("utf-8")
    records = list(split_records(text))
    header = [unquote(f).lstrip("﻿") for f in records[0][0]]
    cols = [k for k, h in enumerate(header) if h in CODER_COLUMNS]
    if len(cols) != 1:
        fail(f"{path}: expected one coder-initials column, found {len(cols)}")
    col = cols[0]
    out = []
    for r, (fields, seps) in enumerate(records):
        if r > 0 and col < len(fields):
            raw = fields[col]
            value = unquote(raw)
            if value.strip() and value != "NA":
                if value not in mapping:
                    mapping[value] = f"C{len(mapping) + 1:02d}"
                code = mapping[value]
                fields[col] = f'"{code}"' if raw.startswith('"') else code
        out.append("".join(f + s for f, s in zip(fields, seps)))
    new = "".join(out).encode("utf-8")
    # Independent check with the csv module: only the coder column changed,
    # and it changed one-to-one.
    old_rows = list(csv.reader(io.StringIO(text, newline="")))
    new_rows = list(csv.reader(io.StringIO(new.decode("utf-8"), newline="")))
    if len(old_rows) != len(new_rows):
        fail(f"{path}: row count changed")
    pairs = set()
    for a, b in zip(old_rows, new_rows):
        if len(a) != len(b) or any(x != y for k, (x, y) in enumerate(zip(a, b)) if k != col):
            fail(f"{path}: a non-coder field changed")
        if len(a) > col:
            pairs.add((a[col], b[col]))
    if len({a for a, _ in pairs}) != len({b for _, b in pairs}):
        fail(f"{path}: coder pseudonyms are not one-to-one")
    return new


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default="release/dist")
    ap.add_argument("--version", help="default: <commit date>-<short sha>")
    args = ap.parse_args()

    if git("status", "--porcelain").strip():
        fail("working tree is not clean; commit first so the bundle matches a commit")
    commit = git("rev-parse", "HEAD").strip()
    date = git("show", "-s", "--format=%cs", "HEAD").strip()
    version = args.version or f"{date}-{commit[:7]}"
    top = f"pilot-meta-extractor-{version}"

    include, exclude = read_manifest(git("show", f"HEAD:{MANIFEST}"))
    tracked = set(git("ls-files").splitlines())
    current = {p for p in tracked if not p.startswith("archive/")}
    inc_paths = [p for p, _ in include]
    if len(set(inc_paths)) != len(inc_paths):
        fail("duplicate path in [include]")
    unlisted = current - set(inc_paths) - set(exclude)
    if unlisted:
        fail("tracked files in neither [include] nor [exclude]: " + ", ".join(sorted(unlisted)))
    stale = (set(inc_paths) | set(exclude)) - tracked
    if stale:
        fail("manifest paths not tracked at HEAD: " + ", ".join(sorted(stale)))
    if set(inc_paths) & set(exclude):
        fail("path both included and excluded")

    files, originals, mapping = {}, {}, {}
    for path, pseudo in include:
        for rx, why in FORBIDDEN_PATH:
            if rx.search(path):
                fail(f"{path}: {why}")
        if path.lower().endswith(".pdf") and not path.startswith("docs/figures/"):
            fail(f"{path}: PDF outside docs/figures/")
        data = git("show", f"HEAD:{path}", binary=True)
        if not path.lower().endswith((".png", ".pdf")):
            for rx, why in FORBIDDEN_TEXT:
                if rx.search(data):
                    fail(f"{path}: {why}")
        if pseudo:
            originals[path] = hashlib.sha256(data).hexdigest()
            data = pseudonymise(data, path, mapping)
        files[path] = data

    readme = git("show", f"HEAD:{README_TEMPLATE}")
    orig_lines = "\n".join(f"- `{p}`: `{h}`" for p, h in originals.items())
    readme = (readme.replace("@@VERSION@@", version).replace("@@COMMIT@@", commit)
              .replace("@@DATE@@", date).replace("@@N_FILES@@", str(len(files) + 2))
              .replace("@@ORIGINAL_SHA256@@", orig_lines))
    if "@@" in readme:
        fail("unfilled placeholder in DEPOSIT_README.md")
    files["RELEASE_README.md"] = readme.encode("utf-8")

    inner = "".join(f"{hashlib.sha256(d).hexdigest()}  {p}\n" for p, d in sorted(files.items()))
    files["SHA256SUMS.txt"] = inner.encode("utf-8")

    out = (ROOT / args.out).resolve()
    out.mkdir(parents=True, exist_ok=True)
    zpath = out / f"{top}.zip"
    with zipfile.ZipFile(zpath, "w", zipfile.ZIP_DEFLATED, compresslevel=9) as z:
        for p in sorted(files):
            info = zipfile.ZipInfo(f"{top}/{p}", ZIP_TIME)
            info.external_attr = 0o644 << 16
            info.compress_type = zipfile.ZIP_DEFLATED
            z.writestr(info, files[p])
    zhash = hashlib.sha256(zpath.read_bytes()).hexdigest()
    (out / "DEPOSIT_README.md").write_bytes(files["RELEASE_README.md"])
    rhash = hashlib.sha256(files["RELEASE_README.md"]).hexdigest()
    (out / "SHA256SUMS.txt").write_text(
        f"{zhash}  {zpath.name}\n{rhash}  DEPOSIT_README.md\n\n# Files inside {zpath.name}\n"
        + "".join(f"{hashlib.sha256(d).hexdigest()}  {top}/{p}\n" for p, d in sorted(files.items())))
    print(f"commit   {commit}\nversion  {version}\nfiles    {len(files)}\n"
          f"coders   {len(mapping)} pseudonymised\nzip      {zpath} ({zpath.stat().st_size} bytes)\n"
          f"sha256   {zhash}")


if __name__ == "__main__":
    main()
