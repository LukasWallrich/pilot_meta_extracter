#!/usr/bin/env python3
"""Create a Google Sheet from data/oer_labeling_scaffold.csv via gws."""
import csv, json, subprocess, sys

GWS = "/opt/homebrew/bin/gws"
CSV = "data/oer_labeling_scaffold.csv"
TITLE = "OER preregistration labeling (metacheck #324)"


def gws(args, json_body=None, params=None):
    cmd = [GWS] + args
    if params is not None:
        cmd += ["--params", json.dumps(params)]
    if json_body is not None:
        cmd += ["--json", json.dumps(json_body)]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit(f"gws failed: {' '.join(args)}\n{r.stderr}\n{r.stdout}")
    return r.stdout


# 1. create
out = gws(["sheets", "spreadsheets", "create"],
          json_body={"properties": {"title": TITLE}})
meta = json.loads(out)
sid = meta["spreadsheetId"]
url = meta.get("spreadsheetUrl", f"https://docs.google.com/spreadsheets/d/{sid}")
print("spreadsheetId:", sid)

# 2. load values
rows = list(csv.reader(open(CSV, encoding="utf-8")))
# Sheets cell hard limit is 50000 chars; truncate defensively
rows = [[(c[:48000] if len(c) > 48000 else c) for c in r] for r in rows]
gws(["sheets", "spreadsheets", "values", "update"],
    params={"spreadsheetId": sid, "range": "Sheet1!A1", "valueInputOption": "RAW"},
    json_body={"values": rows})
print("rows written:", len(rows))

# 3. freeze header + bold it
gws(["sheets", "spreadsheets", "batchUpdate"],
    params={"spreadsheetId": sid},
    json_body={"requests": [
        {"updateSheetProperties": {
            "properties": {"sheetId": 0, "gridProperties": {"frozenRowCount": 1}},
            "fields": "gridProperties.frozenRowCount"}},
        {"repeatCell": {
            "range": {"sheetId": 0, "startRowIndex": 0, "endRowIndex": 1},
            "cell": {"userEnteredFormat": {"textFormat": {"bold": True}}},
            "fields": "userEnteredFormat.textFormat.bold"}},
    ]})

print("URL:", url)
