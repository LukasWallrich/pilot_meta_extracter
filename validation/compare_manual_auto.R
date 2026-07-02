# combine automated and manual coding
library(tidyverse)
library(stringr)

## read manual coding file
manual1 = read.csv("data/2025-12-09_Codingform_PilotingAssessment_pilot.csv")
manual1$coding = "pilot1"
manual2 = read.csv("data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv")
manual2$coding = "pilot2"
# combine both pilots
manual = bind_rows(manual1, manual2)

# make clean doi column, remove .org inclusion
manual <- manual %>%
  mutate(
    doi = X1.2..Article.DOI |>
      sub("^https?://doi\\.org/", "", x = _) |>
      sub("^doi\\.org/", "", x = _) |>
      sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
      str_trim()
  )
# select dois with manual coding
dois = manual$doi
## remove "test" entries
dois = dois[dois != "test"]
## keep only unique entries
dois = unique(dois)

# extract automated coding
source("1_extract_doi_metadata.R")

auto_databases <- process_dois(dois, return_location_details = TRUE)

# add meta check-inspired coding using TEIs
# created using 2_extract_tei_coi_funding.R
# fixme add through csv for now, needs TEIs
# only done for pilot2
auto_tei = read.csv("data/tei_coi_funding_extracted.csv")


# combine files 
comb = 
  manual %>%
  full_join(
    auto_databases, 
    by = "doi"
    ) %>%
  full_join(
    auto_tei,
    by = "doi"
  )

write.csv(comb, "data/2026-04-03_auto-and-manual-combined_pilot1-and-pilot2.csv", row.names = FALSE)

comb_pilot2 = 
  comb %>%
  filter(coding == "pilot2") %>%
  select(where(~ !all(is.na(.))))
  
write.csv(comb_pilot2, "data/2026-04-03_auto-and-manual-combined_pilot2.csv", row.names = FALSE)

## compare manual to auto coding

## remove test rows and select only manually confirmed relevant studies
comb = comb_pilot2 %>% # fixme or comb if interested in both
  filter(doi != "test") %>%
  # rough selection
  filter(str_detect(X1.3..Is.there.a.statement.in.the.article.that.there.was.a.pilot.study.conducted.,
                    "^Yes,"))

# compare funding info
comb_fund = 
  comb %>%
  select(doi, contains("fund"))

## add whether any statement or coding available at all (also includes "no funding" statements)
comb_fund <- comb_fund %>%
  mutate(
    auto_extracted = if_any(starts_with("funding"), ~ 
                              !is.na(.) & str_trim(.) != "" & !str_to_lower(.) %in% c("na", "n/a")
    ),
    manual_extracted = !is.na(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) &
      str_trim(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) != "" &
      !str_to_lower(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) %in% c("na", "n/a")
  )

## rough estimate of overlap in extractions
comb_fund %>%
  group_by(auto_extracted, manual_extracted) %>%
  summarise(n = n(),
            perc = n/nrow(comb_fund))
#   auto_extracted manual_extracted     n   perc
#   <lgl>          <lgl>            <int>  <dbl>
# 1 FALSE          FALSE               10 0.0671
# 2 FALSE          TRUE                59 0.396 
# 3 TRUE           FALSE                2 0.0134
# 4 TRUE           TRUE                78 0.523

## inspect manually
fund_inspect = 
  comb_fund %>%
  filter(manual_extracted, auto_extracted)
## added value auto is the dois and easily readable funding body names etc
## NOTE: openalex extraction is missing; coding error or lack of information available?

# compare COI info
comb_coi = 
  comb %>%
  select(doi, contains("coi"), contains("conflict"), coding)

## add whether any statement or coding available at all (also includes "no coi" statements)
comb_coi <- comb_coi %>%
  mutate(
    auto_extracted = if_any(starts_with("coi"), ~ 
                              !is.na(.) & str_trim(.) != "" & !str_to_lower(.) %in% c("na", "n/a")|
                              if_any(starts_with("rx_coi"), ~ !is.na(.))
    ),
    manual_extracted_content_available = !is.na(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) &
      str_trim(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) != "" &
      !str_to_lower(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) %in% c("na", "n/a")
  ) %>%
  mutate(manual_does_include_coi = 
           ifelse(coding == "pilot1", 
                  X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest.,
                  X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest..COI..)) 

## check amount of manual coding errors / missing
comb_coi %>%
  #filter(coding == "pilot2") %>%
  group_by(manual_does_include_coi,
           manual_extracted_content_available) %>%
  summarise(n = n(),
            perc = n/nrow(comb_coi))

# manual_does_include_coi                                                  manual_extracted_content_available     n   perc
# <chr>                                                                    <lgl>                              <int>  <dbl>
# 1 No, there is no conflict of interest statement                           FALSE                                 37 0.248 
# 2 No, there is no conflict of interest statement                           TRUE                                   2 0.0134
# 3 Yes, the statement says that there are one or more conflicts of interest FALSE                                  6 0.0403
# 4 Yes, the statement says that there are one or more conflicts of interest TRUE                                  10 0.0671
# 5 Yes, the statement says that there is no conflict of interest            FALSE                                 31 0.208 
# 6 Yes, the statement says that there is no conflict of interest            TRUE                                  63 0.423 

## rough estimate of overlap in extractions
comb_coi %>%
  group_by(auto_extracted, manual_extracted_content_available) %>%
  summarise(n = n(),
            perc = n/nrow(comb_coi))
# auto_extracted manual_extracted     n   perc
# <lgl>          <lgl>            <int>  <dbl>
# 1 FALSE          FALSE               38 0.585 
# 2 FALSE          TRUE                21 0.323 
# 3 TRUE           FALSE                4 0.0615
# 4 TRUE           TRUE                 2 0.0308

coi_inspect = 
  comb_coi %>%
  filter(manual_extracted, auto_extracted)
# where they agree there is complete overlap