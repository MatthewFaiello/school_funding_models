library(dplyr)
library(readr)
library(tibble)

# =============================================================================
# EXHIBIT 5: BASE DIVISION I STAFFING COMPARISON
#
# Creates:
#   exhibit5_audit
#   exhibit5_context
#
# The exhibit is derived from the final component comparison. All six Base
# Division I categories must be complete and confirmed before the table is
# assembled. The total row is calculated from the displayed categories rather
# than hard-coded.
# =============================================================================

# Assumes the working directory is the school_funding_model project root.

# -----------------------------------------------------------------------------
# 1. Read authoritative final output
# -----------------------------------------------------------------------------

staffing_components <- read_csv(
  "data/output/final/11_staffing_component_comparison.csv",
  show_col_types = FALSE
)

base_division_i <- staffing_components |>
  filter(
    AnalysisSection == "Staffing rules",
    ComparisonGroup == "Base Division I"
  ) |>
  arrange(DisplayOrder)

expected_categories <- c(
  "K-3 Regular Education Teachers",
  "Grades 4-12 Regular Education Teachers",
  "Basic Special Education Teachers",
  "Intensive Special Education Teachers",
  "Complex Special Education Teachers",
  "Net Vocational Division I Positions"
)

# -----------------------------------------------------------------------------
# 2. Validate completeness and comparison status
# -----------------------------------------------------------------------------

stopifnot(
  nrow(base_division_i) == length(expected_categories),
  identical(base_division_i$ComparisonCategory, expected_categories),
  all(base_division_i$ComparisonStatus == "Confirmed"),
  all(base_division_i$IsCompleteForFinalComparison),
  all(base_division_i$IncludedInWorkingTotal),
  all(base_division_i$IncludedInComparableAmountSubtotal),
  all(base_division_i$CurrentQuantityMissingRows == 0),
  all(base_division_i$CurrentFundingMissingRows == 0),
  all(base_division_i$ProposedQuantityMissingRows == 0),
  all(base_division_i$ProposedFundingMissingRows == 0),
  all(!is.na(base_division_i$CommonRate)),
  all(!is.na(base_division_i$CurrentKnownQuantity)),
  all(!is.na(base_division_i$ProposedKnownQuantity)),
  all(!is.na(base_division_i$CurrentKnownFundingAmount)),
  all(!is.na(base_division_i$ProposedKnownFundingAmount))
)

# -----------------------------------------------------------------------------
# 3. Create the unformatted audit table
# -----------------------------------------------------------------------------

exhibit5_detail <- base_division_i |>
  transmute(
    Category = ComparisonCategory,
    current_quantity = CurrentKnownQuantity,
    proposed_quantity = ProposedKnownQuantity,
    position_difference = ProposedKnownQuantity - CurrentKnownQuantity,
    current_funding = CurrentKnownFundingAmount,
    proposed_funding = ProposedKnownFundingAmount,
    funding_difference = ProposedKnownFundingAmount - CurrentKnownFundingAmount,
    common_rate = CommonRate,
    comparison_status = ComparisonStatus
  )

total_row <- exhibit5_detail |>
  summarise(
    Category = "Total Base Division I",
    current_quantity = sum(current_quantity),
    proposed_quantity = sum(proposed_quantity),
    position_difference = sum(position_difference),
    current_funding = sum(current_funding),
    proposed_funding = sum(proposed_funding),
    funding_difference = sum(funding_difference),
    common_rate = NA_real_,
    comparison_status = if_else(
      all(comparison_status == "Confirmed"),
      "Confirmed",
      "Provisional"
    )
  )

exhibit5_audit <- bind_rows(
  exhibit5_detail,
  total_row
)

# Reconcile displayed totals to the underlying component output.
stopifnot(
  isTRUE(all.equal(
    total_row$current_quantity,
    sum(base_division_i$CurrentKnownQuantity),
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    total_row$proposed_quantity,
    sum(base_division_i$ProposedKnownQuantity),
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    total_row$current_funding,
    sum(base_division_i$CurrentKnownFundingAmount),
    tolerance = 0.01,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    total_row$proposed_funding,
    sum(base_division_i$ProposedKnownFundingAmount),
    tolerance = 0.01,
    check.attributes = FALSE
  ))
)

# Supporting context for drafting exhibit notes. This object is not part of the
# displayed table.
exhibit5_context <- total_row |>
  transmute(
    CategoryCount = nrow(base_division_i),
    ComparisonStatus = comparison_status,
    CurrentPositions = current_quantity,
    ProposedPositions = proposed_quantity,
    PositionDifference = position_difference,
    CurrentFunding = current_funding,
    ProposedFunding = proposed_funding,
    FundingDifference = funding_difference
  )

exhibit5_audit
