# Combined Auto- and Manual-Coded Dataset

**File:** `2026-06-11_auto-and-manual-combined_pilot1-and-pilot2.csv`

This file combines manual coding data from both pilot rounds with automatically extracted metadata. Each row represents one coded article. Columns 1–59 contain manual coding form responses (see the coding form for definitions); the columns documented here are the automatically extracted fields appended during processing.

The combined file is produced by `compare_manual_auto.R`, which joins the manual coding sheets with the outputs of `extract_doi_metadata.R` (API-based extraction) and `extract_tei_coi_funding.R` (TEI-based extraction).

---

## Derived identifier column

| Column | Description |
|--------|-------------|
| `doi` | Cleaned DOI, derived from the manually entered `X1.2..Article.DOI` field. URL prefixes (`https://doi.org/`, `doi:`) are stripped and the value is trimmed. Used as the join key between manual and auto-coded data. |

---

## Basic bibliographic metadata

Extracted via the [OpenAlex Works API](https://openalex.org/) (`extract_doi_metadata.R`).

| Column | Description |
|--------|-------------|
| `year` | Publication year as reported by OpenAlex (`publication_year` field). |
| `journal` | Journal or venue name from the `primary_location.source.display_name` field in OpenAlex. Falls back to `host_venue` for older records. |
| `issn` | Primary ISSN for the journal, taken from the first entry in `primary_location.source.issn`. |

---

## Journal quality metrics

| Column | How obtained | Description |
|--------|--------------|-------------|
| `sjr` | Matched by ISSN and year against the [`sjrdata`](https://github.com/ikashnitsky/sjrdata) R package (Scimago Journal Rankings). Falls back to nearest available year if the exact year is not in the dataset. | Scimago Journal Rank (SJR) score for the journal at the time of publication. Higher values indicate higher prestige/impact. |
| `top_factor` | Matched by ISSN or journal name against the TOP Factor dataset downloaded by `download_top_factor.R` (stored in `top_factor_data.RData`). | Transparency and Openness Promotion (TOP) Factor score for the journal. Reflects journal-level open science policy requirements. |

---

## Author location

Three location columns are extracted from OpenAlex, each representing a different approach to resolving author affiliation country. All country names are converted from ISO-2 codes using the `countrycode` package. Generic institution names (e.g., "Department of Psychology") are skipped in favour of the next available specific institution.

| Column | How obtained | Description |
|--------|--------------|-------------|
| `article_location` | First author's institution listed in the article-level authorship metadata (`authorships[].institutions`) in OpenAlex. | Country of the first author's institutional affiliation **as recorded in the article**. |
| `first_author_location` | First author's OpenAlex author profile, queried via the Authors API. Priority order: `last_known_institution` → `last_known_institutions` → `affiliations`. | Country of the first author's **most recent known institution** according to their OpenAlex profile (may differ from article-level affiliation). |
| `last_author_location` | Same approach as `first_author_location`, applied to the last author. | Country of the last author's **most recent known institution** according to their OpenAlex profile. |

### Location labels

Human-readable versions of each location, formatted as `"Institution name, City, Region, Country"` (omitting any missing components).

| Column | Description |
|--------|-------------|
| `article_location_label` | Formatted label for the first author's article-level location. |
| `first_author_location_label` | Formatted label for the first author's profile location. |
| `last_author_location_label` | Formatted label for the last author's profile location. |

### Detailed location columns

For each of the three location sources, ten detail columns are appended. The column names follow the pattern `{prefix}_detail_{field}`, where `{prefix}` is one of `article_location`, `first_author_location`, or `last_author_location`.

| Field suffix | Description |
|--------------|-------------|
| `_id` | OpenAlex institution ID (e.g., `https://openalex.org/I27837315`). |
| `_display_name` | Full institution name as it appears in OpenAlex. |
| `_type` | Institution type (e.g., `education`, `healthcare`, `government`). |
| `_country_code` | ISO-2 country code (e.g., `US`, `GB`, `NL`). |
| `_country` | Full country name converted from ISO-2 code. |
| `_city` | City from OpenAlex geo data. |
| `_region` | Region or state from OpenAlex geo data. |
| `_latitude` | Latitude coordinate of the institution. |
| `_longitude` | Longitude coordinate of the institution. |
| `_source` | Indicates how the location was resolved: `"article"` (from article-level authorship) or `"author-profile"` (from the OpenAlex author profile API). |

---

## Research topic

Extracted from OpenAlex's `primary_topic` field. NOTE: decided not to be informative, so will be scratched for final version.

| Column | Description |
|--------|-------------|
| `primary_topic_display_name` | Display name of the article's primary topic according to OpenAlex's topic classification (e.g., `"Cognitive Psychology and Memory"`). |
| `primary_topic_id` | OpenAlex ID for the primary topic. |

---

## Funding information

Funding data are extracted from two sources; both are included because Crossref tends to be more complete for funders but OpenAlex preserves structured funder identifiers. Multiple values within a column are separated by ` || `.

### From OpenAlex (`grants` field)

| Column | Description |
|--------|-------------|
| `funding_funders_openalex` | Names of funding organisations as listed in OpenAlex's `grants` field (`funder_display_name`). |
| `funding_funder_ids_openalex` | OpenAlex funder IDs corresponding to the funders above. |
| `funding_award_ids_openalex` | Grant or award numbers from the OpenAlex `grants` field (`award_id`). |

### From Crossref (`funder` field)

| Column | Description |
|--------|-------------|
| `funding_funders_crossref` | Funder names from Crossref's `funder` metadata field. |
| `funding_funder_ids_crossref` | Funder identifiers (typically Crossref funder DOIs) from Crossref metadata. |
| `funding_award_ids_crossref` | Award or grant numbers from Crossref's `funder.award` field. |

---

## Conflict of interest statement (Crossref)

| Column | How obtained | Description |
|--------|--------------|-------------|
| `coi_crossref` | Shallow recursive scan of all Crossref metadata fields for text matching COI-related phrases: `"conflict(s) of interest"`, `"competing interest(s)"`, `"no conflict"`, `"disclos"`, `"declar"`. Multiple matching strings are concatenated with ` || `. | Raw COI statement text found anywhere in the Crossref metadata for the article. `NA` if nothing was found. |

---

## Abstract (Crossref)

| Column | How obtained | Description |
|--------|--------------|-------------|
| `cr_abstract` | Retrieved from Crossref's `abstract` field via the polite Crossref API. | Full abstract text from Crossref metadata. `NA` if Crossref does not hold the abstract. |

---

## TEI-based funding and COI extraction *(pilot 2 only)*

These columns were extracted from GROBID-generated TEI XML files of the article PDFs using `extract_tei_coi_funding.R`. TEI files were only available for the pilot 2 article set; these columns are therefore `NA` for all pilot 1 articles.

Extraction uses cascading XPath and regex strategies to locate funding acknowledgements and COI statements within the parsed PDF structure. See `README_tei_extraction.md` for a full description of the approach and validation results.

| Column | Description |
|--------|-------------|
| `rx_funding_text` | Funding statement(s) extracted from the TEI XML. Multiple sections are separated by ` || `. `NA` if no funding-related text was found. |
| `rx_coi_text` | Conflict of interest statement(s) extracted from the TEI XML. `NA` if no COI-related text was found. |
| `rx_funding_found` | Boolean (`TRUE`/`FALSE`): whether any funding text was extracted (`!is.na(rx_funding_text)`). |
| `rx_coi_found` | Boolean (`TRUE`/`FALSE`): whether any COI text was extracted (`!is.na(rx_coi_text)`). |

---

## Notes

- The `coding` column (from the manual coding data) indicates which pilot the article belongs to (`"pilot1"` or `"pilot2"`).
- All API-extracted columns can be `NA` if the DOI was not found in OpenAlex or Crossref, or if the relevant metadata field was absent.
- The TEI-based columns (`rx_*`) are structurally present for all rows but will be `NA` for all pilot 1 articles.
- It would be helpful to have input from the analysts on whether the automatically extracted funding and coi information is useful, this could imply we could remove the manual coding.
