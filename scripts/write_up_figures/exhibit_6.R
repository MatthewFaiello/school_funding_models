library(dplyr)
library(readr)

# Assumes your working directory is the school_funding_model project root
staffing <- read_csv(
  "data/output/final/11_staffing_component_comparison.csv",
  show_col_types = FALSE
)

categories <- c(
  "Assistant Principal",
  "Administrative Support Professionals",
  "Instructional Supports",
  "Principal",
  "Superintendent",
  "Food Services Supervisor",
  "11-Month Supervisor",
  "Director",
  "Reading Cadre",
  "Assistant Superintendent"
)

exhibit6 <- staffing |>
  filter(ComparisonCategory %in% categories) |>
  mutate(
    Category = factor(ComparisonCategory, levels = categories),
    position_difference = ProposedKnownQuantity - CurrentKnownQuantity,
    funding_difference = ProposedKnownFundingAmount - CurrentKnownFundingAmount,
    Status = case_when(
      ComparisonStatus == "Confirmed" ~ "Confirmed",
      TRUE ~ "Provisional"
    )
  ) |>
  arrange(Category) |>
  transmute(
    Category = as.character(Category),
    Current = CurrentKnownQuantity,
    Proposed = ProposedKnownQuantity,
    `Position difference` = position_difference,
    `Funding difference` = funding_difference,
    Status
  )

exhibit6
