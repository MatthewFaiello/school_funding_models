library(dplyr)
library(readr)
library(tibble)

# Assumes the working directory is the project root
weighted_funding_lea <- read_csv(
  "data/output/final/11_opportunity_operational_lea_comparison.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# Pull and aggregate the proposed allocations
# -----------------------------------------------------------------------------

exhibit10_raw <- weighted_funding_lea |>
  filter(
    AnalysisSection == "Opportunity and Operational Funding",
    ReportingScope ==
      "Includes BASSE; excludes DAFB pending confirmation"
  ) |>
  group_by(FundingCategory) |>
  summarise(
    `Statewide pool` = sum(
      ProposedFundingAmount,
      na.rm = TRUE
    ),
    
    `District allocation` = sum(
      ProposedFundingAmount[LEAType == "District"],
      na.rm = TRUE
    ),
    
    `Charter allocation` = sum(
      ProposedFundingAmount[LEAType == "Charter"],
      na.rm = TRUE
    ),
    
    `Current comparison` = if_else(
      all(is.na(CurrentFundingAmount)),
      "No confirmed current analogue provided",
      "Current amount available"
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    `District share` =
      100 * `District allocation` / `Statewide pool`,
    
    `Charter share` =
      100 * `Charter allocation` / `Statewide pool`,
    
    FundingCategory = factor(
      FundingCategory,
      levels = c(
        "Opportunity Funding",
        "Operational Funding"
      )
    )
  ) |>
  arrange(FundingCategory) |>
  rename(
    `Funding stream` = FundingCategory
  )

exhibit10_raw
