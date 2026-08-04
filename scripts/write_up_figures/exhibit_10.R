library(dplyr)
library(readr)
library(tibble)

# Assumes the working directory is the project root.
source(file.path("scripts", "00_settings.R"))

weighted_funding_lea <- read_csv(
  file.path(final_dir, "11_opportunity_operational_lea_comparison.csv"),
  show_col_types = FALSE
)

weighted_funding_statewide <- read_csv(
  file.path(final_dir, "11_opportunity_operational_comparison.csv"),
  show_col_types = FALSE
)

weighted_rate_summary <- read_csv(
  file.path(intermediate_dir, "08_proposed_weighted_rate_summary.csv"),
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# Validate the aligned Opportunity and Operational Funding scope
# -----------------------------------------------------------------------------

weighted_scope <- weighted_funding_lea |>
  filter(
    AnalysisSection == "Opportunity and Operational Funding",
    ReportingScope == primary_reporting_scope_short
  )

if (n_distinct(weighted_scope$DistrictCode) != 43L) {
  stop("Exhibit 10 expected 43 LEAs in the aligned weighted-funding scope.", call. = FALSE)
}

if (any(weighted_scope$DistrictCode %in% primary_reporting_excluded_lea_codes)) {
  stop("An excluded LEA entered the Exhibit 10 weighted-funding scope.", call. = FALSE)
}

if (!basse_district_code %in% weighted_scope$DistrictCode) {
  stop("BASSE is missing from the Exhibit 10 weighted-funding scope.", call. = FALSE)
}

expected_streams <- c("Opportunity Funding", "Operational Funding")
actual_streams <- sort(unique(weighted_scope$FundingCategory))

if (!identical(actual_streams, sort(expected_streams))) {
  stop("Exhibit 10 expected exactly Opportunity Funding and Operational Funding.", call. = FALSE)
}

if (any(!is.na(weighted_scope$CurrentFundingAmount))) {
  stop(
    "Exhibit 10 expected current Opportunity and Operational analogues to remain unavailable.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Pull and aggregate the proposed allocations
# -----------------------------------------------------------------------------

exhibit10_raw <- weighted_scope |>
  group_by(FundingCategory) |>
  summarise(
    `Statewide pool` = sum(ProposedFundingAmount, na.rm = TRUE),
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
    `District share` = 100 * `District allocation` / `Statewide pool`,
    `Charter share` = 100 * `Charter allocation` / `Statewide pool`,
    FundingCategory = factor(
      FundingCategory,
      levels = expected_streams
    )
  ) |>
  arrange(FundingCategory) |>
  rename(`Funding stream` = FundingCategory)

# -----------------------------------------------------------------------------
# Reconcile the exhibit to final statewide outputs and fixed funding pools
# -----------------------------------------------------------------------------

# LEA allocations are stored to cents. Summing 43 rounded records can differ
# slightly from a statewide amount calculated from unrounded detail. Limit the
# reconciliation allowance to the maximum cumulative half-cent rounding effect.
lea_rounding_tolerance <- max(
  comparison_tolerance,
  n_distinct(weighted_scope$DistrictCode) * 0.005 + 1e-8
)

statewide_reference <- weighted_funding_statewide |>
  filter(
    AnalysisSection == "Opportunity and Operational Funding",
    ReportingScope == primary_reporting_scope_short,
    FundingCategory %in% expected_streams
  ) |>
  select(
    FundingCategory,
    ProposedFundingAmount,
    ComparisonStatus,
    IsCompleteForFinalComparison
  )

rate_reference <- weighted_rate_summary |>
  mutate(
    FundingCategory = case_when(
      grepl("^Opportunity Funding", FundingSection) ~ "Opportunity Funding",
      grepl("^Operational Funding", FundingSection) ~ "Operational Funding",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    StatewideScope == primary_reporting_scope_short,
    FundingCategory %in% expected_streams
  ) |>
  select(
    FundingCategory,
    FundingPool,
    TotalWeightedCount,
    SelectedFundingRate,
    RateMethodSource
  )

reconciliation <- exhibit10_raw |>
  mutate(`Funding stream` = as.character(`Funding stream`)) |>
  left_join(
    statewide_reference,
    by = c("Funding stream" = "FundingCategory")
  ) |>
  left_join(
    rate_reference,
    by = c("Funding stream" = "FundingCategory")
  ) |>
  mutate(
    AllocationReconciliationDifference =
      `District allocation` + `Charter allocation` - `Statewide pool`,
    FinalOutputReconciliationDifference =
      `Statewide pool` - ProposedFundingAmount,
    PoolReconciliationDifference = `Statewide pool` - FundingPool
  )

if (
  any(abs(reconciliation$AllocationReconciliationDifference) > comparison_tolerance) ||
    any(abs(reconciliation$FinalOutputReconciliationDifference) > lea_rounding_tolerance) ||
    any(abs(reconciliation$PoolReconciliationDifference) > lea_rounding_tolerance)
) {
  stop("Exhibit 10 does not reconcile to the final weighted-funding outputs.", call. = FALSE)
}

if (
  any(reconciliation$ComparisonStatus != "Not yet estimable") ||
    any(reconciliation$IsCompleteForFinalComparison)
) {
  stop(
    "Exhibit 10 expected both current comparisons to remain not yet estimable.",
    call. = FALSE
  )
}

exhibit10_context <- reconciliation |>
  transmute(
    `Funding stream`,
    TotalWeightedCount,
    SelectedFundingRate,
    RateMethodSource,
    ComparisonStatus,
    ReportingScope = primary_reporting_scope_short
  )

exhibit10_raw
