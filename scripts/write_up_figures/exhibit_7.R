library(dplyr)
library(readr)
library(tibble)

# =============================================================================
# EXHIBIT 7: PRINCIPAL-POSITION BRIDGE
#
# Creates:
#   exhibit7
#   exhibit7_context
#
# The bridge explains the difference between recreated current and independently
# reproduced proposed principal positions. It uses only aligned IV&V records:
# BASSE is included and DAFB is excluded.
# =============================================================================

# Assumes the working directory is the school_funding_model project root.

# -----------------------------------------------------------------------------
# 1. Read authoritative quantity and comparison outputs
# -----------------------------------------------------------------------------

current <- read_csv(
  "data/output/intermediate/04_current_model_quantities.csv",
  show_col_types = FALSE
)

proposed <- read_csv(
  "data/output/intermediate/07_proposed_model_quantities.csv",
  show_col_types = FALSE
)

staffing_components <- read_csv(
  "data/output/final/11_staffing_component_comparison.csv",
  show_col_types = FALSE
)

run_settings <- read_csv(
  "data/output/audit/00_run_settings.csv",
  show_col_types = FALSE
)

primary_scope <- run_settings |>
  filter(Setting == "Primary reporting scope") |>
  pull(Value)

principal_comparison <- staffing_components |>
  filter(
    AnalysisSection == "Staffing rules",
    ComparisonCategory == "Principal"
  )

# -----------------------------------------------------------------------------
# 2. Select aligned principal rows
# -----------------------------------------------------------------------------

# Current principal rows correspond to official coded schools.
current_principals <- current |>
  filter(
    Component == "Principal",
    IncludeInStatewide,
    IsSchool
  )

# Proposed principal rows correspond to school calculation units.
proposed_principals <- proposed |>
  filter(
    Component == "Principal",
    IncludeInStatewide,
    IsSchoolCalculationUnit
  )

# -----------------------------------------------------------------------------
# 3. Validate scope and comparison status
# -----------------------------------------------------------------------------

stopifnot(
  length(primary_scope) == 1L,
  grepl("includes BASSE", primary_scope, ignore.case = TRUE),
  grepl("excludes DAFB", primary_scope, ignore.case = TRUE),
  nrow(principal_comparison) == 1L,
  principal_comparison$ComparisonStatus == "Confirmed",
  principal_comparison$IsCompleteForFinalComparison,
  principal_comparison$CurrentQuantityMissingRows == 0,
  principal_comparison$ProposedQuantityMissingRows == 0,
  all(current_principals$IncludeInStatewide),
  all(proposed_principals$IncludeInStatewide),
  !any(current_principals$DistrictName == "Dover Air Force Base"),
  !any(proposed_principals$DistrictName == "Dover Air Force Base")
)

# -----------------------------------------------------------------------------
# 4. Calculate bridge elements
# -----------------------------------------------------------------------------

current_positions <- sum(
  current_principals$FundingQuantity,
  na.rm = TRUE
)

current_charter_codes <- current_principals |>
  filter(LEAType == "Charter") |>
  nrow()

proposed_charter_units <- proposed_principals |>
  filter(LEAType == "Charter") |>
  nrow()

additional_charter_units <-
  proposed_charter_units - current_charter_codes

below_current_minimum <- current_principals |>
  filter(
    RawInputValue < 15,
    FundingQuantity == 0
  ) |>
  nrow()

proposed_positions <- sum(
  proposed_principals$FundingQuantity,
  na.rm = TRUE
)

# -----------------------------------------------------------------------------
# 5. Reconcile the bridge to the authoritative final comparison
# -----------------------------------------------------------------------------

stopifnot(
  additional_charter_units >= 0,
  below_current_minimum >= 0,
  current_positions +
    additional_charter_units +
    below_current_minimum ==
    proposed_positions,
  isTRUE(all.equal(
    current_positions,
    principal_comparison$CurrentKnownQuantity,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    proposed_positions,
    principal_comparison$ProposedKnownQuantity,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    proposed_positions - current_positions,
    principal_comparison$WorkingPositionDifference,
    tolerance = 1e-8,
    check.attributes = FALSE
  ))
)

# -----------------------------------------------------------------------------
# 6. Build Exhibit 7
# -----------------------------------------------------------------------------

exhibit7 <- tibble(
  `Bridge element` = c(
    "Recreated current principal positions",
    "Effect of additional charter building calculation units",
    "Effect of replacing the current 15-unit minimum",
    "IV&V proposed principal positions"
  ),
  Positions = c(
    as.character(current_positions),
    paste0("+", additional_charter_units),
    paste0("+", below_current_minimum),
    as.character(proposed_positions)
  )
)

# Supporting context for drafting the exhibit note. This object is not part of
# the displayed table.
exhibit7_context <- tibble(
  ReportingScope = primary_scope,
  ComparisonStatus = principal_comparison$ComparisonStatus,
  CurrentOfficialSchoolRows = nrow(current_principals),
  ProposedSchoolCalculationUnits = nrow(proposed_principals),
  CurrentCharterOrganizationRows = current_charter_codes,
  ProposedCharterBuildingUnits = proposed_charter_units,
  AdditionalCharterBuildingUnits = additional_charter_units,
  SchoolsBelowCurrentMinimum = below_current_minimum,
  CurrentPrincipalPositions = current_positions,
  ProposedPrincipalPositions = proposed_positions,
  PrincipalPositionDifference = proposed_positions - current_positions
)

exhibit7
