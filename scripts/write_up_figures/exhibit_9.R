library(dplyr)
library(readr)

# Assumes the working directory is the project root
weighted_components <- read_csv(
  "data/output/intermediate/08_proposed_weighted_component_summary.csv",
  show_col_types = FALSE
)

input_order <- c(
  "Low income",
  "Active multilingual learner",
  "Total enrollment",
  "Basic special education",
  "Intensive special education",
  "Complex special education",
  "Vocational enrollment"
)

exhibit9_raw <- weighted_components |>
  filter(StatewideScope == "Excludes DAFB") |>
  transmute(
    `Funding stream` = case_when(
      FundingSection == "Opportunity Funding (State Support)" ~
        "Opportunity",
      FundingSection == "Operational Funding (State Support)" ~
        "Operational",
      TRUE ~ NA_character_
    ),
    
    Input = case_when(
      Component == "Opportunity Funding - Low Income" ~
        "Low income",
      Component == "Opportunity Funding - Multilingual Learner" ~
        "Active multilingual learner",
      Component == "Operational Funding - Enrollment" ~
        "Total enrollment",
      Component == "Operational Funding - Low Income" ~
        "Low income",
      Component == "Operational Funding - Multilingual Learner" ~
        "Active multilingual learner",
      Component == "Operational Funding - Basic Special Education" ~
        "Basic special education",
      Component == "Operational Funding - Intensive Special Education" ~
        "Intensive special education",
      Component == "Operational Funding - Complex Special Education" ~
        "Complex special education",
      Component == "Operational Funding - Vocational" ~
        "Vocational enrollment",
      TRUE ~ NA_character_
    ),
    
    `Raw count` = RawCount,
    Weight = AppliedWeight,
    `Weighted count` = WeightedCount
  ) |>
  filter(
    !is.na(`Funding stream`),
    !is.na(Input)
  ) |>
  mutate(
    `Funding stream` = factor(
      `Funding stream`,
      levels = c("Opportunity", "Operational")
    ),
    Input = factor(
      Input,
      levels = input_order
    )
  ) |>
  arrange(`Funding stream`, Input) |>
  mutate(
    `Funding stream` = as.character(`Funding stream`),
    Input = as.character(Input)
  )

exhibit9_raw


