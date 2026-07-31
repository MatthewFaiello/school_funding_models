library(dplyr)
library(readr)
library(tibble)

# Read the final statewide staffing comparison
staffing_state <- read_csv(
  "data/output/final/11_staffing_statewide_comparison.csv",
  show_col_types = FALSE
) |>
  filter(AnalysisSection == "Staffing rules") |>
  slice(1)

# Build the report table
staffing_view_table <- tibble(
  View = c(
    "Working estimate",
    "Comparable-amount subtotal",
    "Confirmed subtotal"
  ),
  
  current_raw = c(
    staffing_state$WorkingCurrentFundingAmount,
    staffing_state$ComparableAmountCurrentFundingAmount,
    staffing_state$ConfirmedCurrentFundingAmount
  ),
  
  proposed_raw = c(
    staffing_state$WorkingProposedFundingAmount,
    staffing_state$ComparableAmountProposedFundingAmount,
    staffing_state$ConfirmedProposedFundingAmount
  ),
) |>
  mutate(difference_raw = proposed_raw - current_raw) |>
  transmute(
    View,
    `Recreated current` = current_raw,
    `IV&V proposed` = proposed_raw,
    Difference = difference_raw
  )

staffing_view_table
