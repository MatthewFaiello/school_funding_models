library(dplyr)
library(readr)
library(tibble)

# =============================================================================
# EXHIBIT 4: STATEWIDE POSITION-BASED FUNDING COMPARISON VIEWS
#
# Creates:
#   staffing_view_table
#   staffing_view_context
#
# The displayed table retains the report's existing four-column structure.
# Scope and arithmetic checks below ensure that the exhibit reflects the
# authoritative aligned IV&V output: BASSE included, DAFB excluded.
# =============================================================================

# Assumes the working directory is the school_funding_model project root.

# -----------------------------------------------------------------------------
# 1. Read final output and run settings
# -----------------------------------------------------------------------------

staffing_state <- read_csv(
  "data/output/final/11_staffing_statewide_comparison.csv",
  show_col_types = FALSE
) |>
  filter(
    AnalysisSection == "Staffing rules"
  )

run_settings <- read_csv(
  "data/output/audit/00_run_settings.csv",
  show_col_types = FALSE
)

primary_scope <- run_settings |>
  filter(
    Setting == "Primary reporting scope"
  ) |>
  pull(
    Value
  )

# -----------------------------------------------------------------------------
# 2. Validate the aligned reporting scope
# -----------------------------------------------------------------------------

stopifnot(
  nrow(staffing_state) == 1L,
  staffing_state$LEACount == 43,
  staffing_state$ReportingScope == "Includes BASSE; excludes DAFB",
  length(primary_scope) == 1L,
  grepl("includes BASSE", primary_scope, ignore.case = TRUE),
  grepl("excludes DAFB", primary_scope, ignore.case = TRUE)
)

# -----------------------------------------------------------------------------
# 3. Assemble and reconcile the three reporting views
# -----------------------------------------------------------------------------

current_raw <- c(
  staffing_state$WorkingCurrentFundingAmount,
  staffing_state$ComparableAmountCurrentFundingAmount,
  staffing_state$ConfirmedCurrentFundingAmount
)

proposed_raw <- c(
  staffing_state$WorkingProposedFundingAmount,
  staffing_state$ComparableAmountProposedFundingAmount,
  staffing_state$ConfirmedProposedFundingAmount
)

reported_difference <- c(
  staffing_state$WorkingFundingDifference,
  staffing_state$ComparableAmountFundingDifference,
  staffing_state$ConfirmedFundingDifference
)

calculated_difference <- proposed_raw - current_raw

stopifnot(
  isTRUE(
    all.equal(
      calculated_difference,
      reported_difference,
      tolerance = 0.01,
      check.attributes = FALSE
    )
  )
)

# -----------------------------------------------------------------------------
# 4. Build the report table
# -----------------------------------------------------------------------------

staffing_view_table <- tibble(
  View = c(
    "Working estimate",
    "Comparable-amount subtotal",
    "Confirmed subtotal"
  ),
  `Recreated current` = current_raw,
  `IV&V proposed` = proposed_raw,
  Difference = reported_difference
)

# Supporting context for drafting table notes. This object is not part of the
# displayed four-column exhibit.
staffing_view_context <- tibble(
  ReportingScope = staffing_state$ReportingScope,
  LEACount = staffing_state$LEACount,
  ProvisionalCategoryCount = staffing_state$ProvisionalCategoryCount,
  IsCompleteForFinalComparison = staffing_state$IsCompleteForFinalComparison,
  WorkingPercentDifference = staffing_state$WorkingPercentDifference,
  ComparableAmountPercentDifference =
    staffing_state$ComparableAmountPercentDifference,
  ConfirmedPercentDifference = staffing_state$ConfirmedPercentDifference
)

staffing_view_table
