library(dplyr)
library(readr)
library(tibble)

# Assumes the working directory is the school_funding_model project root.
staffing_components <- read_csv(
  "data/output/final/11_staffing_component_comparison.csv",
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# 1. Create the unformatted audit table
# -------------------------------------------------------------------------

exhibit5_audit <- staffing_components |>
  filter(
    AnalysisSection == "Staffing rules",
    ComparisonGroup == "Base Division I"
  ) |>
  arrange(DisplayOrder) |>
  transmute(
    Category = ComparisonCategory,
    current_quantity = CurrentKnownQuantity,
    proposed_quantity = ProposedKnownQuantity,
    position_difference = proposed_quantity - current_quantity,
    current_funding = CurrentKnownFundingAmount,
    proposed_funding = ProposedKnownFundingAmount,
    funding_difference = proposed_funding - current_funding,
    common_rate = CommonRate,
    comparison_status = ComparisonStatus
  )

# Add the Total Base Division I row.
exhibit5_audit <- bind_rows(
  exhibit5_audit,
  exhibit5_audit |>
    summarise(
      Category = "Total Base Division I",
      current_quantity = sum(current_quantity, na.rm = TRUE),
      proposed_quantity = sum(proposed_quantity, na.rm = TRUE),
      position_difference = proposed_quantity - current_quantity,
      current_funding = sum(current_funding, na.rm = TRUE),
      proposed_funding = sum(proposed_funding, na.rm = TRUE),
      funding_difference = proposed_funding - current_funding,
      common_rate = NA_real_,
      comparison_status = "Confirmed"
    )
)


exhibit5_audit

