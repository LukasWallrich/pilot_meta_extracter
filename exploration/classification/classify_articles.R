library(ellmer)
library(jsonlite)
library(readr)
library(dplyr)
library(glue)
library(purrr)
library(tidyr)
library(gt)

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
n_records <- if (length(args) >= 1) as.integer(args[1]) else 20L
cat("Classifying", n_records, "articles\n")

# Load data
articles <- read_csv("../full_sample.csv", show_col_types = FALSE) |>
  select(
    doi = DOI,
    title = Title,
    abstract = `Abstract Note`,
    journal = `Publication Title`
  ) |>
  slice_head(n = n_records) |>
  mutate(
    title = replace_na(title, ""),
    abstract = replace_na(abstract, "")
  )

cat("Loaded", nrow(articles), "articles\n")

# Load schema
schema <- fromJSON("schemas/psychology_fields_schema.json")
fields_json <- toJSON(schema$fields, pretty = TRUE, auto_unbox = TRUE)
valid_field_ids <- schema$fields$id

# Build lookup from field_id to label
field_labels <- setNames(schema$fields$label, schema$fields$id)

# Combined system prompt
system_prompt <- glue("
You classify psychology articles and determine whether they present empirical research.

## Task 1: Field Classification

Classify the article into a single primary psychology subfield.

Use ONLY the fields defined in the JSON array below.
Choose the subfield that best reflects the article's main theoretical framing.
Prioritise article content (title and abstract). Use journal title only to resolve ambiguity.
If interdisciplinary, pick the subfield that best fits the core psychological contribution.
Never invent new fields; always return one of the existing id values.

Key distinctions:
- Psychology (General): Last resort only when no single subfield clearly dominates
- Occupational Psychology: Requires BOTH work-specific constructs AND theoretical claims about work
  - Workplace as setting only -> classify by underlying theory (e.g., prejudice reduction -> Social)
  - Work-specific constructs (leadership, burnout, job satisfaction) -> Occupational
- Educational Psychology: Learning/motivation/assessment in educational settings only
- Methods: Primary contribution must be methodological (new measure, technique, or meta-analytic method)

AVAILABLE FIELDS (JSON array):
{fields_json}

## Task 2: Empirical Classification

Determine whether this paper presents empirical research. Empirical research involves the \
collection and/or analysis of data (quantitative or qualitative). This includes experiments, \
surveys, observational studies, meta-analyses of empirical data, and secondary data analyses. \
Non-empirical work includes theoretical papers, literature reviews (without quantitative \
synthesis), commentaries, editorials, and methodological proposals without data application.

You will receive articles as JSON with: article_title, article_abstract, journal_title
")

# Define structured output type
classification_type <- type_object(
  field_id = type_enum(
    "The id of the matching psychology subfield",
    values = valid_field_ids
  ),
  field_reasoning = type_string("Brief reasoning for the field classification choice"),
  is_empirical = type_boolean("Whether the paper presents empirical research"),
  empirical_reasoning = type_string("Brief reasoning for the empirical classification")
)

# Build per-article prompts
prompts <- articles |>
  pmap(function(doi, title, abstract, journal) {
    toJSON(
      list(
        article_title = unbox(title),
        article_abstract = unbox(abstract),
        journal_title = unbox(journal)
      ),
      pretty = TRUE
    )
  })

# Create chat object
chat <- chat_openai(
  system_prompt = system_prompt,
  model = "gpt-5-mini"
)

# Run classification
cat("Running classification...\n")
results <- parallel_chat_structured(
  chat,
  prompts,
  type = classification_type,
  max_active = 5
)

# Build output dataframe
output <- articles |>
  mutate(
    research_area = results$field_id,
    research_area_reasoning = results$field_reasoning,
    empirical = if_else(results$is_empirical, "yes", "no"),
    empirical_reasoning = results$empirical_reasoning
  ) |>
  select(doi, title, journal, abstract, research_area, research_area_reasoning, empirical, empirical_reasoning)

# Write output
write_csv(output, "classified_articles.csv")
cat("Output written to classified_articles.csv\n\n")

# Print summary
cat("=== Summary ===\n")
cat("\nEmpirical status:\n")
print(table(output$empirical))
cat("\nResearch area distribution:\n")
print(sort(table(output$research_area), decreasing = TRUE))

# Create summary table with gt
summary_data <- output |>
  mutate(research_area_label = field_labels[research_area]) |>
  count(research_area_label, empirical) |>
  pivot_wider(names_from = empirical, values_from = n, values_fill = 0) |>
  mutate(total = yes + no) |>
  arrange(desc(total)) |>
  rename(`Research Area` = research_area_label, Empirical = yes, `Non-Empirical` = no, Total = total)

totals <- summary_data |>
  summarise(`Research Area` = "Total", Empirical = sum(Empirical), `Non-Empirical` = sum(`Non-Empirical`), Total = sum(Total))

summary_table <- bind_rows(summary_data, totals) |>
  gt() |>
  tab_header(
    title = "Article Classification Summary",
    subtitle = glue("{nrow(output)} articles classified")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = `Research Area` == "Total")
  ) |>
  tab_style(
    style = cell_borders(sides = "top", weight = px(2)),
    locations = cells_body(rows = `Research Area` == "Total")
  )

gtsave(summary_table, "classification_summary.png")
cat("\nSummary table saved to classification_summary.png\n")
