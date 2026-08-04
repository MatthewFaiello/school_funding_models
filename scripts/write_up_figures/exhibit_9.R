library(dplyr)
library(readr)
library(tibble)

# =============================================================================
# EXHIBIT 9: PROPOSED OPPORTUNITY AND OPERATIONAL WEIGHTED INPUTS
#
# Creates:
#   exhibit9_raw
#   exhibit9_context
#
# The displayed table reports the raw counts, applied weights, and weighted
# counts used to distribute the fixed statewide Opportunity and Operational
# funding pools. Current-model analogues are not yet available, so this exhibit
# documents the independently reproduced proposed model only.
# =============================================================================

# Assumes the working directory is the school_funding_model project root.
source(file.path("scripts", "00_settings.R"))

# -----------------------------------------------------------------------------
# 1. Read authoritative outputs
# -----------------------------------------------------------------------------

weighted_components <- read_csv(
  file.path(intermediate_dir, "08_proposed_weighted_component_summary.csv"),
  show_col_types = FALSE
)

weighted_rates <- read_csv(
  file.path(intermediate_dir, "08_proposed_weighted_rate_summary.csv"),
  show_col_types = FALSE
)

weighted_comparison <- read_csv(
  file.path(final_dir, "11_opportunity_operational_comparison.csv"),
  show_col_types = FALSE
)

weighted_lea_comparison <- read_csv(
  file.path(final_dir, "11_opportunity_operational_lea_comparison.csv"),
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 2. Validate scope, completeness, and expected components
# -----------------------------------------------------------------------------

weighted_components_scope <- weighted_components |>
  filter(StatewideScope == primary_reporting_scope_short)

weighted_rates_scope <- weighted_rates |>
  filter(StatewideScope == primary_reporting_scope_short)

expected_components <- c(
  "Opportunity Funding - Low Income",
  "Opportunity Funding - Multilingual Learner",
  "Operational Funding - Enrollment",
  "Operational Funding - Low Income",
  "Operational Funding - Multilingual Learner",
  "Operational Funding - Basic Special Education",
  "Operational Funding - Intensive Special Education",
  "Operational Funding - Complex Special Education",
  "Operational Funding - Vocational"
)

if (
  nrow(weighted_components_scope) != length(expected_components) ||
    !setequal(weighted_components_scope$Component, expected_components)
) {
  stop(
    "Exhibit 9 did not find the expected nine Opportunity and Operational weighted components.",
    call. = FALSE
  )
}

if (
  any(weighted_components_scope$RowsMissingInput != 0) ||
    any(weighted_components_scope$RowsMissingRate != 0) ||
    any(is.na(weighted_components_scope$RawCount)) ||
    any(is.na(weighted_components_scope$AppliedWeight)) ||
    any(is.na(weighted_components_scope$WeightedCount))
) {
  stop("Exhibit 9 contains incomplete weighted-funding inputs.", call. = FALSE)
}

if (nrow(weighted_rates_scope) != 2L) {
  stop("Exhibit 9 expected exactly two weighted-funding rate records.", call. = FALSE)
}

if (!all(weighted_rates_scope$SelectedMethod == "recalculated")) {
  stop("Exhibit 9 expected both weighted-funding rates to use the recalculated method.", call. = FALSE)
}

weighted_lea_scope <- weighted_lea_comparison |>
  filter(ReportingScope == primary_reporting_scope_short)

if (n_distinct(weighted_lea_scope$DistrictCode) != 43L) {
  stop("Exhibit 9 expected 43 LEAs in the aligned weighted-funding scope.", call. = FALSE)
}

if (any(weighted_lea_scope$DistrictCode %in% primary_reporting_excluded_lea_codes)) {
  stop("An excluded LEA entered the Exhibit 9 weighted-funding scope.", call. = FALSE)
}

if (!basse_district_code %in% weighted_lea_scope$DistrictCode) {
  stop("BASSE is missing from the Exhibit 9 weighted-funding scope.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 3. Construct the displayed exhibit table
# -----------------------------------------------------------------------------

input_order <- c(
  "Low income",
  "Active multilingual learner",
  "Total enrollment",
  "Basic special education",
  "Intensive special education",
  "Complex special education",
  "Vocational enrollment"
)

exhibit9_raw <- weighted_components_scope |>
  transmute(
    `Funding stream` = case_when(
      FundingSection == "Opportunity Funding (State Support)" ~ "Opportunity",
      FundingSection == "Operational Funding (State Support)" ~ "Operational",
      TRUE ~ NA_character_
    ),
    Input = case_when(
      Component == "Opportunity Funding - Low Income" ~ "Low income",
      Component == "Opportunity Funding - Multilingual Learner" ~
        "Active multilingual learner",
      Component == "Operational Funding - Enrollment" ~ "Total enrollment",
      Component == "Operational Funding - Low Income" ~ "Low income",
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
  filter(!is.na(`Funding stream`), !is.na(Input)) |>
  mutate(
    `Funding stream` = factor(
      `Funding stream`,
      levels = c("Opportunity", "Operational")
    ),
    Input = factor(Input, levels = input_order)
  ) |>
  arrange(`Funding stream`, Input) |>
  mutate(
    `Funding stream` = as.character(`Funding stream`),
    Input = as.character(Input)
  )

# -----------------------------------------------------------------------------
# 4. Reconcile weighted counts and fixed pools to final outputs
# -----------------------------------------------------------------------------

stream_reconciliation <- weighted_components_scope |>
  mutate(
    FundingCategory = case_when(
      FundingSection == "Opportunity Funding (State Support)" ~
        "Opportunity Funding",
      FundingSection == "Operational Funding (State Support)" ~
        "Operational Funding",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(FundingCategory, FundingSection) |>
  summarise(
    ComponentWeightedCount = sum(WeightedCount),
    ComponentFundingAmount = sum(FundingAmount),
    .groups = "drop"
  ) |>
  left_join(
    weighted_rates_scope |>
      select(
        FundingSection,
        RateWeightedCount = TotalWeightedCount,
        FundingPool,
        SelectedFundingRate,
        RateMethodSource,
        RateMethodNote
      ),
    by = "FundingSection"
  ) |>
  left_join(
    weighted_comparison |>
      filter(ReportingScope == primary_reporting_scope_short) |>
      select(
        FundingCategory,
        ComparisonStatus,
        FinalProposedFundingAmount = ProposedFundingAmount,
        IsCompleteForFinalComparison
      ),
    by = "FundingCategory"
  )

if (
  any(
    abs(
      stream_reconciliation$ComponentWeightedCount -
        stream_reconciliation$RateWeightedCount
    ) > 1e-8
  ) ||
    any(
      abs(
        stream_reconciliation$ComponentFundingAmount -
          stream_reconciliation$FundingPool
      ) > comparison_tolerance
    ) ||
    any(
      abs(
        stream_reconciliation$ComponentFundingAmount -
          stream_reconciliation$FinalProposedFundingAmount
      ) > comparison_tolerance
    )
) {
  stop(
    "Exhibit 9 weighted counts or funding pools do not reconcile to the authoritative outputs.",
    call. = FALSE
  )
}

expected_pools <- c(
  "Opportunity Funding" = opportunity_funding_pool,
  "Operational Funding" = operational_funding_pool
)

actual_pools <- setNames(
  stream_reconciliation$FundingPool,
  stream_reconciliation$FundingCategory
)

if (
  any(
    abs(actual_pools[names(expected_pools)] - expected_pools) >
      comparison_tolerance
  )
) {
  stop("Exhibit 9 does not reconcile to the maintained statewide funding pools.", call. = FALSE)
}

# Supporting context for report notes. This object is not part of the displayed
# table. Current analogues remain unavailable, so no current-versus-proposed
# difference should be inferred from this exhibit.
exhibit9_context <- stream_reconciliation |>
  transmute(
    ReportingScope = primary_reporting_scope_short,
    FundingCategory,
    TotalWeightedCount = ComponentWeightedCount,
    FundingPool,
    SelectedFundingRate,
    RateMethodSource,
    ComparisonStatus,
    CurrentAnalogueAvailable = IsCompleteForFinalComparison,
    Note = paste(
      "The fixed statewide pool is distributed using the independently",
      "reproduced weighted counts and a recalculated per-weighted-student rate.",
      "Current FY2025-26 analogues and LEA allocations remain pending OMB/CGO",
      "guidance; DAFB is excluded because it does not receive state funding."
    )
  ) |>
  arrange(match(FundingCategory, c("Opportunity Funding", "Operational Funding")))

exhibit9_raw
