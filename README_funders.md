# Funder → Country pipeline (`4_link_funders_locations.R`)

This pipeline produces, for each article in the pilot1+pilot2 corpus, a clean
list of **funders** each linked to a **country**, by reconciling two
complementary sources of funder identity.

> **Module vs. driver.** `4_link_funders_locations.R` is now a **function
> library** (sourcing it only defines `resolve_funder_ids`,
> `resolve_funder_names`, `resolve_paper_funders`, `cluster_funders`, … and
> constants — no side effects, and it never reads the combined CSV). The
> Crossref funder columns are passed in as **arguments**. It is the single source
> of truth used by `run_pipeline.R`. The pilot-1 + pilot-2 batch run that writes
> `data/funders_with_locations_pilot1-and-pilot2.csv` and
> `data/funder_locations_long.csv` lives in `validation/pilot_link_funders.R`
> (run from the repo root) — the **only** file allowed to read
> `data/2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv` for the
> Crossref funder columns.

## Sources and division of labour

| Source | Provides | Geocoded by |
|---|---|---|
| **(A) Crossref Funder-Registry IDs** (`funding_funder_ids_crossref` in the combined CSV) | Authoritative funder *identity* deposited with the article | OpenAlex Funders entity (`/funders?filter=ids.crossref:`), falling back to the Crossref Funder Registry (`/funders/{id}`) for the country |
| **(B) TEI funder names** (`rx_funder_names`, GROBID-parsed full text) | A noisy *supplement* that catches funders the publisher never deposited as a registry ID | Registry name search (OpenAlex/Crossref `/funders?search=`) and, failing that, the country named *inside* the funder string |

Note on what is **not** used: OpenAlex's per-paper `grants` field is empty for
this corpus, so OpenAlex is not a funder *source* here — Crossref supplies funder
identity; OpenAlex/Crossref registries are only the geocoder.

## Cleaning rules for TEI names

GROBID over-splits funding statements, so a single statement can yield the real
funder plus a bare country fragment, a journal name, and duplicate spellings.
The following screens are applied (all recomputed every run — see *Caching*):

1. **Whole-string-country junk.** A TEI name is dropped only if it is
   *essentially just a country*: a country is present **and** the string carries
   no funder-type token (`council|foundation|ministry|university|institute|
   research|science|…`, multilingual stems). This keeps real funders that merely
   *contain* a country word ("Israel Science Foundation", "Research Council of
   Norway", "National Natural Science Foundation of China") while dropping bare
   "Switzerland" / "Taiwan" / "UK". (The earlier version wrongly junked any name
   whose string `countrycode()` could match — discarding ~63 real funders.)
2. **Journal / publisher screen.** A TEI "funder" is dropped if it matches the
   paper's own journal title (≥0.90 normalised similarity) or an obvious
   journal/publisher pattern (`^BMC`, `^PLOS`, `^Frontiers`, `journal of …`,
   `proceedings of …`, …). This removes cases like "BMC Medical Education".
3. **Misparse screen.** An unresolved fragment with no funder token and no
   contained country is dropped (GROBID title-fragment misparses, e.g.
   "Challenges in social adaptation and interactions amongst international
   medical").
4. **Per-paper dedup.** Funders are clustered and merged when they share a
   Crossref Funder ID, are token-subsets/substrings of one another, or one is the
   acronym (initials) of the other — so
   `NIHR` == `National Institute for Health Research (NIHR)` ==
   `National Institute for Health Research NIHR`. The **most complete spelling**
   is kept as canonical.

Acronym-only names (`NIH`, `DFG`, `NSERC`, …) carry no funder token, so they are
not junked on that basis; they are resolved against the registry using the
candidate's `alternate_titles` / initials.

## Country-resolution priority (recorded in `country_source`)

For each funder, the country is taken from the first rule that fires:

| `country_source` | Rule |
|---|---|
| `crossref_id` | (a) Crossref Funder-Registry ID → registry country. **Authoritative default.** |
| `tei_override_crossref` | A **narrow correction** of a known-wrong registry country (see below). |
| `tei_registry` | (b) TEI name → registry name match → registry country. |
| `tei_contained` | (c) Country named *inside* the TEI funder string (`countrycode`, plus a UK/US-abbreviation supplement; "European Union" for supranational names). |
| *(NA)* | (d) Funder known, country unknown. |

### The override (deliberate, visible deviation from a→d)

A small number of articles deposit a funder ID that the Crossref registry maps to
the **wrong country** (a deposit-time entity-matching error). The clearest case
is `10.1080/10447318.2023.2276529`: the article links Taiwan's *National Science
and Technology Council* to the registry entry for the **Zambian** council of the
same name, even though the funding statement gives a Taiwanese MOST grant number.

To correct only these cases without undermining the authoritative default, the
override fires **only** when all of the following hold:

* the paper's TEI contains a **standalone bare-country fragment** (e.g. a lone
  "Taiwan" GROBID-split next to its funder) — the strongest author-stated
  location signal;
* those bare fragments resolve to **exactly one** country;
* the registry country is **not** that country; and
* the funder's name **actually appears** in the TEI (so the bare country refers
  to it).

Country hints for the override are taken *only* from bare-country fragments, never
from a country contained in another funder's name (which would wrongly relocate a
correctly-registered funder in a multi-funder paper). On this corpus the override
fires on exactly **1** funder (the NSTC→Taiwan case).

## "No funding" convention

`rx_funder_names` carries the literal token `no funding` when the extractor read
an explicit no-funding statement. When that token is present and no funder is
found from either source, `funding_status = "no funding"` and `best_funders =
"no funding"`.

## Output schema

### `data/funders_with_locations_pilot1-and-pilot2.csv` — one row per paper
| Column | Meaning |
|---|---|
| `doi`, `coding` | Article DOI (normalised) and pilot wave |
| `best_funders` | Merged, deduped canonical funder names (`||`-separated); or the literal `no funding` |
| `best_funder_countries` | Unique funder countries (`||`-separated; `European Union` allowed) |
| `funding_status` | `funded` / `no funding` / `unknown` |
| `reconciliation` | `both` / `crossref_only` / `tei_only` / `conflict` / `no_funding` / `none` (agreement between the two sources, for transparency) |
| `n_funders`, `n_funders_crossref`, `n_funders_tei` | Counts (merged / from each source) |
| `funders_crossref`, `funders_tei` | Per-source display strings with countries (audit trail) |

### `data/funder_locations_long.csv` — audit trail, one row per (paper × funder)
| Column | Meaning |
|---|---|
| `doi`, `coding` | Article + wave |
| `funder_name` | Canonical (kept) name, or the raw screened-out string (junk rows) |
| `crossref_funder_id` | Registry ID where known |
| `country_code`, `country` | Resolved country |
| `source` | `crossref_id` / `tei_name` / `both` (`both` = merged across sources — downstream filters on `source=="tei_name"` would miss merged rows) |
| `country_source` | For kept rows, the a–d rule; for junk rows, the **screen reason** (`bare_country` / `journal` / `misparse` / `junk_term`) |
| `confidence` | `high` (Crossref ID, registry name match, or override) / `medium` (fuzzy registry match or contained-country — review these) / `low` (funder kept but no country) / `junk` |
| `junk` | `FALSE` = a kept funder (these form `best_funders`); `TRUE` = a screened-out TEI fragment, retained for transparency |

The kept rows (`junk == FALSE`) are the union behind `best_funders`; the junk
rows document what was dropped and why.

### Caches (so reruns make **zero** network calls)
* `data/funder_id_country.csv` — Crossref Funder ID → name / country.
* `data/funder_name_country.csv` — TEI name → registry match (`reg_*` columns).
  Only the **expensive registry lookup** is cached; the cheap local signals
  (funder token, contained country, bare-country, journal screen) are recomputed
  every run, so changing a cleaning rule takes effect without re-querying.

API volume on a cold run: ~164 Crossref-ID lookups (batched) + ~529 TEI-name
registry lookups, all free OpenAlex/Crossref calls, cached thereafter.

## Coverage (current run)

* 347 papers; 164 unique Crossref funder IDs, **100%** geocoded.
* 529 unique TEI name strings → 182 registry-resolved, 261 kept via funder token,
  6 bare-country (dropped), 79 misparses (dropped).
* **Junk fix:** of the 67 names the old code wrongly country-junked, **62 are now
  retained** as real funders. The 5 still dropped are 3 genuine bare countries
  (Switzerland / Taiwan / UK), 1 misparse ("Ngogo chimpanzees…Uganda"), and 1
  borderline body lacking any funder token ("State of Rhineland-Palatinate,
  Germany").
* Long table: **576 kept** funder rows across 229 papers (71% carry a country)
  plus **141 junk audit rows** (50 junk-term, 84 misparse, 5 bare-country, 2
  journal). Kept `country_source`: 257 `crossref_id`, 98 `tei_registry`, 54
  `tei_contained`, 1 `tei_override_crossref`, 166 none. Kept `confidence`: 336
  high / 74 medium / 166 low.
* `funding_status`: 260 funded / 30 no funding / 57 unknown.
* Reconciliation: 109 both, 73 tei_only, 27 crossref_only, 88 none, 30
  no_funding, 20 conflict.

## Validation against manual coding

Validated against the manual column `X8.1..Does.the.article.include.a.funding.
statement.` on coded papers. **The manual negative class is unreliable:** the
field holds the transcribed statement text, and a blank does not mean "no
funding" — 96 of the 210 manual-blank papers carry an authoritative Crossref
funder ID, i.e. they were funded but the field was left empty. Raw funded-vs-not
agreement is therefore an uninformative ~48%.

The meaningful metric is **recall against manually-confirmed funding statements**:
of 136 papers where a coder transcribed a statement, the pipeline marks **107
(78.7%) `funded`**. The 29 misses are upstream TEI-extraction gaps, not linking
errors — 15 where GROBID found no funding text (`unknown`) and 14 where the
extractor read an explicit "no funding" that contradicts the coder. The apparent
"false positives" (manual blank, pipeline funded) are overwhelmingly papers with
a real Crossref funder ID, i.e. correct pipeline calls against incomplete manual
coding.

## Known limitations

* **Translation variants** are not always merged when they share neither a
  registry ID nor enough string overlap — e.g. `Deutsche Forschungsgemeinschaft`
  vs `German Research Foundation (DFG)` can appear as two funders on one paper.
* **Medium-confidence fuzzy registry matches** carry false-positive risk: a
  short name can match a different same-sounding body (e.g. "German Research
  Foundation" matched the German-Israeli Foundation → Israel rather than the DFG →
  Germany). These are flagged `confidence == "medium"` in the long table so they
  can be filtered or reviewed.
* **Contained-country** can mislocate a multinational funder named after a
  country (rare here; the override is deliberately limited to bare-country
  evidence to avoid this).
* A funder body with **no funder-type token and no registry hit** is dropped even
  if real (e.g. "State of Rhineland-Palatinate, Germany").
* `funding_status` inherits any GROBID funding-text extraction gaps; it is a
  byproduct of the linking pipeline, not an independent funding-statement
  detector.
