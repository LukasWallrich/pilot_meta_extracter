# DOI Metadata Extractor

Minimal R scripts for extracting publication metadata from DOIs using OpenAlex for article details, institution locations, and author affiliations, supplemented by Crossref for funding information, conflict of interest statements, and abstracts.

## What you get

For each DOI, `process_dois()` returns:
- Publication year and journal title (from OpenAlex host venue metadata)
- ISSN (primary plus optional vector of alternates)
- Scimago Journal Rank (matched on ISSN/year via `sjrdata`)
- TOP Factor (if `top_factor_data.RData` has been downloaded)
- The OpenAlex `primary_topic` field (full object, plus display name and id helpers)
- Three country columns:
  - `article_location` – country of the first author's article-level affiliation
  - `first_author_location` – country of the first author's profile (most recent institution)
  - `last_author_location` – country of the last author's profile (most recent institution)
- Optional detailed columns for each location (`return_location_details = TRUE`), including institution name, type, city, region, latitude, longitude, and data source
- Funding information from both OpenAlex and Crossref:
  - `funding_funders_openalex` / `funding_funders_crossref` – funder names (separated by ` || `)
  - `funding_funder_ids_openalex` / `funding_funder_ids_crossref` – funder identifiers (ROR IDs or DOIs)
  - `funding_award_ids_openalex` / `funding_award_ids_crossref` – grant/award numbers
- Conflict of interest (COI) statement extracted from Crossref metadata (`coi_crossref`)
- Abstract text from Crossref metadata (`cr_abstract`)

## Quick start

```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, countrycode, dplyr, sjrdata)

source("download_top_factor.R")   # one-time setup (optional but recommended)
source("extract_doi_metadata.R")

my_dois <- c(
  "10.1037/amp0001385",
  "10.1037/met0000351",
  "10.1146/annurev-psych-020821-114157"
)

results <- process_dois(my_dois, return_location_details = FALSE)
write.csv(results, "results.csv", row.names = FALSE)
```

## Suggested manual test

After sourcing `extract_doi_metadata.R`, run:

```r
test_dois <- c(
  "10.1037/amp0001385",
  "10.1037/met0000351",
  "10.3389/fpsyg.2022.896741"
)

process_dois(test_dois, return_location_details = TRUE)
```

Expected output:
- `article_location` contains the country of the first author's article-level affiliation
- `first_author_location` and `last_author_location` contain countries from the authors' profile data when available
- `first_author_location_detail_source` and `last_author_location_detail_source` indicate the data source (`author-profile` or `NA`)
- Location labels provide full institution details when `return_location_details = TRUE`

## How it works

1. **OpenAlex Works API** supplies publication metadata, host venue, location-rich authorship information and the primary topic.
2. **Article location** – extracted from the first author's article-level institution affiliations; generic department names are skipped.
3. **First author location** – the first author's profile is queried for the most recent institution via OpenAlex Authors API (`last_known_institution` → `last_known_institutions` → `affiliations`).
4. **Last author location** – the last author's profile is queried independently using the same priority order.
5. **Metrics** – SJR values are pulled from `sjrdata`, while TOP Factor scores are matched against the optional download file.
6. **Country extraction** – all locations extract the `country_code` field from OpenAlex institutions and convert it to the full country name using ISO-2 to country name mapping.
7. **Funding information** – extracted from both OpenAlex (grants field) and Crossref (funder/funding fields). Includes funder names, funder identifiers (ROR IDs or DOIs), and award/grant numbers. Multiple values are concatenated with ` || ` separator.
8. **Conflict of interest (COI)** – extracted from Crossref metadata using heuristic keyword matching for common COI-related phrases (e.g., "conflict of interest", "competing interests", "disclosure").
9. **Abstract** – retrieved from Crossref metadata when available.

## Repository layout

- `extract_doi_metadata.R` – core extraction and helper routines.
- `download_top_factor.R` – helper for retrieving the TOP Factor dataset (creates `top_factor_data.RData`).
- `top_factor_manual_setup.R` – fallback instructions if automated download fails.

## Requirements

```r
httr        # OpenAlex and Crossref API queries
countrycode # ISO-to-name conversion
dplyr       # light data manipulation
sjrdata     # Scimago Journal Rank reference data
```

## Citation

Please cite all data sources you rely on:
- **OpenAlex**: https://openalex.org/
- **Crossref**: https://www.crossref.org/
- **Scimago Journal Rank**: https://www.scimagojr.com/
- **Center for Open Science (TOP Factor)**: https://www.cos.io/initiatives/top-guidelines

## License

MIT License – free for academic and commercial use.
