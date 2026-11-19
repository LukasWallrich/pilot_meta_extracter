# Article Classification

Classifies psychology articles from `full_sample.csv` into (1) a psychology subfield and (2) whether the article presents empirical research, using a single LLM call per article.

## Approach

Each article's title, abstract, and journal title are sent to `gpt-5-mini` (via the `ellmer` R package) with a combined prompt that performs both classifications in one call. The model returns structured output with:

- **field_id** — one of 10 psychology subfields constrained via `type_enum` to valid values from the schema
- **field_reasoning** — brief justification for the subfield choice
- **is_empirical** — boolean indicating empirical research
- **empirical_reasoning** — brief justification for the empirical classification

### Field classification

Uses the MaRCo project's psychology subfield schema (`schemas/psychology_fields_schema.json`) with 10 subfields (e.g., Cognitive, Social, Clinical, Developmental). The prompt prioritises article content over journal title and includes disambiguation rules for borderline cases (e.g., workplace-as-setting vs occupational psychology).

### Empirical classification

Empirical research is defined as involving the collection and/or analysis of data (quantitative or qualitative), including meta-analyses and secondary data analyses. Non-empirical work includes theoretical papers, narrative reviews, commentaries, and methodological proposals without data application.

## Usage

```bash
# Classify first 20 articles (default)
Rscript classify_articles.R

# Classify all articles
Rscript classify_articles.R 4075
```

Requires `OPENAI_API_KEY` environment variable.

### R dependencies

`ellmer`, `jsonlite`, `readr`, `dplyr`, `glue`, `purrr`, `tidyr`, `gt`, `webshot2`

## Output

- **`classified_articles.csv`** — one row per article with columns: `doi`, `title`, `journal`, `abstract`, `research_area`, `research_area_reasoning`, `empirical` (yes/no), `empirical_reasoning`
- **`classification_summary.png`** — cross-tabulation of research area by empirical status
