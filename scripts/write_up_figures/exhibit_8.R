library(dplyr)
library(readr)
library(tibble)

# Exhibit 8: LEA-type summary of the comparable staffing subtotal.
# Verified revision: July 31, 2026.
# Assumes the working directory is the project root.
source(file.path("scripts", "00_settings.R"))

staffing_lea <- read_csv(
  file.path(final_dir, "11_staffing_lea_comparison.csv"),
  show_col_types = FALSE
)

staffing_components <- read_csv(
  file.path(final_dir, "11_staffing_component_comparison.csv"),
  show_col_types = FALSE
)

staffing_statewide <- read_csv(
  file.path(final_dir, "11_staffing_statewide_comparison.csv"),
  show_col_types = FALSE
)

proposed_detail <- read_csv(
  file.path(intermediate_dir, "08_proposed_model_funding_detail.csv"),
  show_col_types = FALSE
)

# Validate the aligned reporting scope before constructing the exhibit.
staffing_lea_scope <- staffing_lea |>
  filter(AnalysisSection == "Staffing rules")

lea_count <- n_distinct(staffing_lea_scope$DistrictCode)

if (lea_count != 43L) {
  stop("Exhibit 8 expected 43 LEAs in the aligned staffing comparison.", call. = FALSE)
}

if (any(staffing_lea_scope$DistrictCode %in% primary_reporting_excluded_lea_codes)) {
  stop("An excluded LEA entered the Exhibit 8 staffing scope.", call. = FALSE)
}

if (!basse_district_code %in% staffing_lea_scope$DistrictCode) {
  stop("BASSE is missing from the Exhibit 8 staffing scope.", call. = FALSE)
}

if (
  n_distinct(staffing_lea_scope$DistrictCode[staffing_lea_scope$LEAType == "District"]) != 19L ||
    n_distinct(staffing_lea_scope$DistrictCode[staffing_lea_scope$LEAType == "Charter"]) != 24L
) {
  stop("Exhibit 8 expected 19 districts and 24 charters.", call. = FALSE)
}

# Identify working categories excluded from the comparable-amount subtotal.
noncomparable_categories <- staffing_components |>
  filter(
    IncludedInWorkingTotal,
    !IncludedInComparableAmountSubtotal
  ) |>
  arrange(DisplayOrder) |>
  pull(ComparisonCategory)

if (!identical(noncomparable_categories, "Buildings and Grounds Supervisor")) {
  stop(
    paste(
      "Exhibit 8 expected Buildings and Grounds Supervisor to be the only",
      "category excluded from the comparable-amount subtotal."
    ),
    call. = FALSE
  )
}

# Confirm that outside-formula components cannot enter the core staffing detail.
if (any(proposed_detail$Component %in% outside_formula_current_components)) {
  stop("An outside-formula component entered the proposed staffing detail.", call. = FALSE)
}

# Calculate the proposed amount excluded from the comparable subtotal for each LEA.
noncomparable_proposed_lea <- proposed_detail |>
  filter(
    IncludeInStatewide,
    !DistrictCode %in% primary_reporting_excluded_lea_codes,
    Component %in% noncomparable_categories
  ) |>
  group_by(DistrictCode) |>
  summarise(
    NoncomparableProposedFundingAmount = sum(FundingAmount, na.rm = TRUE),
    .groups = "drop"
  )

# Create LEA-level comparable amounts. The current working total already omits
# unknown current amounts. Food Services Supervisor remains provisional and is
# retained in the comparable subtotal; no missing current amount is imputed.
staffing_lea_comparable <- staffing_lea_scope |>
  left_join(noncomparable_proposed_lea, by = "DistrictCode") |>
  mutate(
    NoncomparableProposedFundingAmount =
      coalesce(NoncomparableProposedFundingAmount, 0),
    ComparableCurrentFundingAmount = WorkingCurrentFundingAmount,
    ComparableProposedFundingAmount =
      WorkingProposedFundingAmount - NoncomparableProposedFundingAmount,
    ComparableFundingDifference =
      ComparableProposedFundingAmount - ComparableCurrentFundingAmount,
    ComparablePercentDifference = if_else(
      abs(ComparableCurrentFundingAmount) > 1e-8,
      100 * ComparableFundingDifference / ComparableCurrentFundingAmount,
      NA_real_
    )
  )

summarize_lea_group <- function(data, lea_type_label) {
  current_total <- sum(data$ComparableCurrentFundingAmount, na.rm = TRUE)
  proposed_total <- sum(data$ComparableProposedFundingAmount, na.rm = TRUE)

  tibble(
    `LEA type` = lea_type_label,
    LEAs = n_distinct(data$DistrictCode),
    Current = current_total,
    Proposed = proposed_total,
    Difference = proposed_total - current_total,
    `Aggregate change` =
      100 * (proposed_total - current_total) / current_total,
    `Median LEA change` =
      median(data$ComparablePercentDifference, na.rm = TRUE),
    Increases = sum(data$ComparableFundingDifference > 0, na.rm = TRUE),
    Decreases = sum(data$ComparableFundingDifference < 0, na.rm = TRUE),
    No_change = sum(data$ComparableFundingDifference == 0, na.rm = TRUE)
  )
}

# Calculate district, charter, and all-LEA rows from LEA-level records.
exhibit8_derived <- bind_rows(
  staffing_lea_comparable |>
    filter(LEAType == "District") |>
    summarize_lea_group("Districts"),
  staffing_lea_comparable |>
    filter(LEAType == "Charter") |>
    summarize_lea_group("Charters"),
  staffing_lea_comparable |>
    summarize_lea_group("All LEAs")
)

# Reconcile the all-LEA row to the authoritative statewide comparable subtotal.
# LEA files store dollar amounts to cents, while the statewide output is created
# from the underlying records before final rounding. Summing 43 rounded LEA
# records can therefore differ by a few cents. The tolerance below is the
# maximum cumulative half-cent rounding difference, plus the pipeline tolerance.
statewide_reference <- staffing_statewide |>
  filter(AnalysisSection == "Staffing rules")

if (nrow(statewide_reference) != 1L) {
  stop("Exhibit 8 expected exactly one statewide staffing record.", call. = FALSE)
}

all_leas_derived <- exhibit8_derived |>
  filter(`LEA type` == "All LEAs")

lea_rounding_tolerance <- lea_count * 0.005 + comparison_tolerance
funding_difference_rounding_tolerance <- 2 * lea_rounding_tolerance

reconciliation_difference <- tibble(
  Measure = c("Current", "Proposed", "Difference"),
  LEASummedAmount = c(
    all_leas_derived$Current,
    all_leas_derived$Proposed,
    all_leas_derived$Difference
  ),
  StatewideAmount = c(
    statewide_reference$ComparableAmountCurrentFundingAmount,
    statewide_reference$ComparableAmountProposedFundingAmount,
    statewide_reference$ComparableAmountFundingDifference
  ),
  Difference = LEASummedAmount - StatewideAmount,
  AllowedTolerance = c(
    lea_rounding_tolerance,
    lea_rounding_tolerance,
    funding_difference_rounding_tolerance
  )
)

if (any(abs(reconciliation_difference$Difference) > reconciliation_difference$AllowedTolerance)) {
  stop(
    "Exhibit 8 does not reconcile to the statewide comparable subtotal within cumulative cent-rounding tolerance.",
    call. = FALSE
  )
}

# Use the authoritative statewide funding values in the displayed All LEAs row,
# while retaining LEA-derived medians and counts.
all_leas_authoritative <- all_leas_derived |>
  mutate(
    Current = statewide_reference$ComparableAmountCurrentFundingAmount,
    Proposed = statewide_reference$ComparableAmountProposedFundingAmount,
    Difference = statewide_reference$ComparableAmountFundingDifference,
    `Aggregate change` = statewide_reference$ComparableAmountPercentDifference
  )

exhibit8_raw <- bind_rows(
  exhibit8_derived |> filter(`LEA type` != "All LEAs"),
  all_leas_authoritative
)

# Context for the report note accompanying the exhibit.
provisional_comparable_categories <- staffing_components |>
  filter(
    IncludedInComparableAmountSubtotal,
    ComparisonStatus != "Confirmed"
  ) |>
  arrange(DisplayOrder) |>
  pull(ComparisonCategory)

exhibit8_context <- tibble(
  ReportingScope = primary_reporting_scope_short,
  LEACount = lea_count,
  DistrictCount = n_distinct(
    staffing_lea_scope$DistrictCode[staffing_lea_scope$LEAType == "District"]
  ),
  CharterCount = n_distinct(
    staffing_lea_scope$DistrictCode[staffing_lea_scope$LEAType == "Charter"]
  ),
  ComparableCategoryCount = sum(
    staffing_components$IncludedInComparableAmountSubtotal
  ),
  ExcludedCategory = paste(noncomparable_categories, collapse = "; "),
  ProvisionalIncludedCategories = if_else(
    length(provisional_comparable_categories) == 0,
    "None",
    paste(provisional_comparable_categories, collapse = "; ")
  ),
  CurrentAmountsImputed = FALSE,
  Note = paste(
    "Comparable amounts exclude Buildings and Grounds Supervisor because the",
    "current amount is not yet estimable. Food Services Supervisor remains",
    "provisional and is included where a current amount is available; missing",
    "current amounts are not imputed. The All LEAs funding totals use the",
    "authoritative statewide output; district and charter rows are summed from",
    "LEA records stored to cents."
  )
)

exhibit8_raw
