# Preregistration Detection (binary + platform)

Detects, for each pilot 1 and pilot 2 article, whether the paper reports a **preregistration** and on which **platform**, directly from the GROBID TEI full text plus targeted OSF API lookups.

> **Module vs. driver.** `R/3_extract_prereg.R` is now a **function library** (sourcing it only defines `parse_tei`, `classify_paper`, `resolve_osf_caches`, `keyword_scan`, … and constants — no side effects). It is the single source of truth used by `run_pipeline.R`; `classify_paper()` takes the OSF type/template maps as arguments and emits an `is_registered_report` column, and `registration_any` = `prereg_found` OR trial registry OR registered report (keyword **or** an OSF "Registered Report Protocol" template). The pilot-1 + pilot-2 batch run that writes `data/stages/prereg_extracted_pilot1-and-pilot2.csv` and the manual-coding validation live in `validation/pilot_extract_prereg.R` (run from the repo root). That output adds the `is_registered_report` column relative to the pre-refactor stage file; every other column is unchanged.

The logic is inspired by [scienceverse/metacheck](https://github.com/scienceverse/metacheck) (`open_practices` and `prereg_check`) but is recreated so it returns just the binary + platform we need, and so it corrects a false-positive failure mode in those modules (see [Divergence from metacheck](#divergence-from-metacheck)).

## What is produced

Per paper (`data/stages/prereg_extracted_pilot1-and-pilot2.csv`):

| Column | Description |
|---|---|
| `prereg_found` | Logical. A **study preregistration** is present (OSF registration with a genuine prereg template, AsPredicted, or an explicit non-negated preregistration statement). Preprints and trial registries are **excluded** from this flag. |
| `registration_any` | Logical. `prereg_found` **OR** a trial registry **OR** a Registered Report. Broader "some form of registration" flag. |
| `prereg_platform` | Platform(s), ` || `-separated: e.g. `OSF registration`, `AsPredicted`, `OSF project`, `preprint`, `ClinicalTrials.gov`, `ISRCTN`, `statement`. |
| `osf_reg_template` | For OSF `registrations`-type links, the OSF `registration_supplement` template that drove the decision (auditable). |
| `prereg_urls` | Registration-relevant URLs found (` || `-separated). |
| `prereg_statement` | Short matched snippet, when detection was statement-based. |
| `is_trial_registry` | Logical. A clinical-trial / health registry was detected. |
| `has_negated_prereg` | Logical. The text contains an explicit "not preregistered" statement. |
| Manual ground-truth columns | `x21_rr`, `x22`, `manual_prereg`, `manual_reg_any`, `manual_rr` (from the coding form). |

## Approach

1. **Parse the TEI** (namespace stripped) and remove the reference list (`//listBibl` and reference divs) **before** scanning — cited works' OSF/registry links would otherwise be counted as the paper's own (this inflated unique OSF codes by ~30%).
2. **Collect URLs** from the body text and from `<ptr>/<ref target=...>` attributes.
3. **Classify each registration signal:**
   - `aspredicted.org/...` → **AsPredicted** (always a preregistration).
   - Preprint DOIs/hosts (`10.31234` PsyArXiv, `10.31219` OSF Preprints, `10.1101` bio/medRxiv, `osf.io/preprints/...`) → **preprint**, *not* a preregistration.
   - Trial registries (`ClinicalTrials.gov`, `ISRCTN`, `DRKS`, `ANZCTR`, `PROSPERO`, `ChiCTR`) → flagged via `is_trial_registry`; detected from URLs **and** from plain-text mentions co-occurring with a trial-id pattern (`NCT\d{8}`, `ISRCTN\d+`, …), since many papers cite the registry by name + ID without a clickable link.
   - Bare `osf.io/<code>` → the URL does not reveal the object type, so call the OSF API (see below).
4. **OSF object resolution (the key step).** A bare `osf.io/<code>` is resolved via `https://api.osf.io/v2/guids/<code>/` to a type (`registrations`, `preprints`, `nodes`, …). Only `registrations` are candidate preregistrations; `preprints` are excluded; `nodes` (projects) are ambiguous.
   - For a `registrations` object, additionally fetch `https://api.osf.io/v2/registrations/<code>/` and read its **template** (`registration_supplement`):
     - **Dedicated prereg template** → counts as a preregistration with no manuscript-text needed. Dedicated = OSF Preregistration, Preregistration Template from AsPredicted.org, Prereg Challenge, Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla), Replication Recipe (Brandt et al.), OSF-Standard Pre-Data Collection Registration, Registered Report Protocol Preregistration.
     - **"Open-Ended Registration"** (OSF's default free-text template, frequently used to *archive data/materials*) → **ambiguous**. Counts as a preregistration only if the manuscript contains an explicit, **non-negated** "preregist…" statement; if the only mention is negated ("not preregistered") or absent, it is not counted.
   - All OSF API results are cached (`data/cache/osf_guid_types.csv`, `data/cache/osf_reg_templates.csv`) so reruns make no network calls.
5. **`prereg_found`** = AsPredicted OR OSF registration (per the template rule above) OR an explicit non-negated preregistration statement. Preprints and trial registries are excluded. **`registration_any`** additionally counts trial registries and Registered Reports.

The fragile manuscript-text check is therefore confined to exactly the Open-Ended-Registration cases; everywhere else the decision rests on structured signals (URL form, OSF object type, OSF template).

## Divergence from metacheck

Both metacheck modules miscount OSF registrations that are really data/materials archives:

- `open_practices` is purely keyword-based (`pre-?regist|aspredicted` near a repo word) and never inspects the OSF object.
- `prereg_check` resolves OSF link **type** and dispatches on the registration **template**, but it *handles* "Open-Ended Registration" (via its `oer()` branch) and so reports a preregistration for open-ended registrations used as data archives. It never reads the manuscript, so it cannot catch in-text statements like "the analyses were not preregistered".

This was reported upstream (issue drafted for `scienceverse/metacheck`: *prereg_check counts OSF Open-Ended Registrations as preregistrations*). Our `R/3_extract_prereg.R` resolves it with the template-gating rule above.

### Validation of the gating rule

Across the 22 manually-coded papers that link an OSF `registrations` object, the registration template aligns with the human "is this a preregistration?" judgement:

| template class | human: not prereg | human: prereg |
|---|---|---|
| dedicated prereg template | 1 | 17 |
| Open-Ended Registration only | 3 | 1 |

A dedicated template is a strong positive signal (17/18; the single mismatch is a paper whose AsPredicted prereg covers only a supplemental study). Open-Ended alone is not (1/4). Adding the scoped manuscript-text check to the Open-Ended bucket resolves all four example cases correctly:

| Open-Ended registration | manuscript text | decision |
|---|---|---|
| `osf.io/u67zj` (`10.1177/09567976221093575`) | "the sample sizes and analysis plans … were preregistered" | preregistration ✓ |
| `osf.io/85kwv` (`10.1177/09567976221121348`) | "design and analysis plans … were **not** preregistered" | excluded ✓ |
| `osf.io/pgz2c` (`10.1080/00224545.2023.2216880`) | no preregistration mention | excluded ✓ |
| `osf.io/7pf3c` (`10.1080/00224545.2023.2224546`) | no preregistration mention | excluded ✓ |

### Trial registrations are not assumed to be "pre"

A clinical-trial registry entry (e.g. ClinicalTrials.gov) registers a trial's existence and outcomes but is not, by itself, an analysis preregistration — it may be registered retrospectively, and it need not lock the specific analyses a given paper reports (e.g. a secondary analysis with post-hoc hypotheses). Trial registrations therefore feed `registration_any` but **not** `prereg_found`.

## Detection performance

Validated against the manual coding (`X2.2` preregistration item; Registered Reports from `X2.1`), over the papers with a usable manual binary (n = 140 for `prereg_found`, n = 145 for `registration_any`).

| Metric (pooled) | Accuracy | Precision | Recall | F1 |
|---|---|---|---|---|
| `prereg_found` | 0.950 | 0.923 | 0.900 | 0.911 |
| `registration_any` (incl. RR + trial) | 0.952 | 0.896 | 0.956 | 0.925 |

`prereg_found` confusion matrix: TP 36, FP 3, FN 4, TN 97. Per pilot: pilot 1 F1 0.917, pilot 2 F1 0.903. The template-gating refinement raised `prereg_found` from F1 0.878 (pre-gating: precision 0.857) by removing the OSF Open-Ended-Registration false positives; text-only trial-ID detection cut `registration_any` false negatives from 6 to 2.

Of the residual discrepancies: 3 false positives — one is a manual-coding error (`10.1080/13506285.2023.2192993`, see below), one is a real OSF preregistration of a *supplemental* study coded "no" for the main studies (`10.1111/jopy.12855`, defensible), and one is an aspirational generic sentence (`10.3758/s13428-022-01804-9`). 4 false negatives — two are trial-registration-only papers the coders treated as preregistrations (`10.1002/eat.23864`, `10.1177/21677026231164958`; both captured by `registration_any`), one is a manual-coding error (`10.1111/cdev.13845`), one a GROBID extraction gap (`10.1111/desc.13244`).

## Manual-coding issues surfaced

Adjudicating disagreements against the source PDFs turned up two manual-coding errors worth re-checking:
- `10.1080/13506285.2023.2192993` — manuscript clearly preregistered on AsPredicted (three links); coded "no".
- `10.1111/cdev.13845` — manuscript clearly states "were not preregistered"; coded "yes".

## Files

| File | Description |
|---|---|
| `R/3_extract_prereg.R` | Detection script (TEI + OSF API), validation, output. |
| `data/stages/prereg_extracted_pilot1-and-pilot2.csv` | Per-paper results + manual ground truth. |
| `data/cache/osf_guid_types.csv` | Cache: OSF GUID → object type. |
| `data/cache/osf_reg_templates.csv` | Cache: OSF registration → template / category. |

## Requirements

```r
dplyr; stringr; xml2; httr; jsonlite   # plus metacheck (optional, for comparison)
```
