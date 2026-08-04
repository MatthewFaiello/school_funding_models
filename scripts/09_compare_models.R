# =============================================================================
# 09_compare_models.R
# =============================================================================
# Primary current-versus-proposed analysis.
#   A. Staffing rules use the same common rates for comparable positions.
#   B. Opportunity and Operational Funding are compared separately.
#
# All formula-based calculations are retained. Confirmed, provisional, and
# not-yet-estimable results are clearly flagged. Components confirmed as funded
# outside the proposed position formula are preserved in a separate Step 04 audit
# output and are excluded from the staffing comparison by design.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

collapse_nonblank_pair <- function(x, y) {
  map2_chr(x, y, function(first_value, second_value) {
    values <- str_squish(c(first_value, second_value))
    values <- unique(values[!is.na(values) & values != ""])
    paste(values, collapse = " | ")
  })
}

current_detail_path <- file.path(
  intermediate_dir,
  "05_current_model_funding_detail.csv"
)
proposed_detail_path <- file.path(
  intermediate_dir,
  "08_proposed_model_funding_detail.csv"
)
outside_formula_detail_path <- file.path(
  intermediate_dir,
  "04_current_outside_formula_components.csv"
)

staffing_component_path <- file.path(
  intermediate_dir,
  "09_staffing_component_comparison.csv"
)
staffing_lea_path <- file.path(
  intermediate_dir,
  "09_staffing_lea_comparison.csv"
)
staffing_state_path <- file.path(
  intermediate_dir,
  "09_staffing_statewide_comparison.csv"
)
weighted_state_path <- file.path(
  intermediate_dir,
  "09_opportunity_operational_comparison.csv"
)
weighted_lea_path <- file.path(
  intermediate_dir,
  "09_opportunity_operational_lea_comparison.csv"
)
comparison_qc_path <- file.path(audit_dir, "09_comparison_qc.csv")

check_required_files(c(
  current_detail_path,
  proposed_detail_path,
  outside_formula_detail_path,
  model_comparison_crosswalk_path,
  current_opportunity_operational_path,
  lea_crosswalk_path
))

current_detail <- read_csv(current_detail_path, show_col_types = FALSE) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    CalculationComplete = as.logical(CalculationComplete),
    FundingComplete = as.logical(FundingComplete)
  ) |>
  filter(
    IncludeInStatewide,
    !DistrictCode %in% primary_reporting_excluded_lea_codes
  )

proposed_detail <- read_csv(proposed_detail_path, show_col_types = FALSE) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    CalculationComplete = as.logical(CalculationComplete),
    FundingComplete = as.logical(FundingComplete)
  ) |>
  filter(
    IncludeInStatewide,
    !DistrictCode %in% primary_reporting_excluded_lea_codes
  )

outside_formula_detail <- read_csv(
  outside_formula_detail_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    Component = coalesce(Component, ""),
    InclusionStatus = coalesce(InclusionStatus, "")
  )

comparison_crosswalk <- read_csv(
  model_comparison_crosswalk_path,
  show_col_types = FALSE
) |>
  mutate(
    IncludeInPreliminary = as.logical(IncludeInPreliminary),
    IncludeInFinal = as.logical(IncludeInFinal),
    DisplayOrder = as.integer(DisplayOrder),
    OutstandingQuestion = coalesce(OutstandingQuestion, ""),
    Notes = coalesce(Notes, "")
  )

outside_formula_crosswalk <- comparison_crosswalk |>
  filter(
    AnalysisSection == "Staffing rules",
    SourceModel == "Current",
    SourceComponent %in% outside_formula_current_components
  )

staffing_comparison_crosswalk <- comparison_crosswalk |>
  filter(
    AnalysisSection == "Staffing rules",
    ComparisonGroup != "Outside formula"
  )

missing_outside_formula_crosswalk <- tibble(
  Component = setdiff(
    outside_formula_current_components,
    outside_formula_crosswalk$SourceComponent
  )
)

unexpected_outside_formula_crosswalk <- outside_formula_crosswalk |>
  filter(
    ComparisonGroup != "Outside formula" |
      MappingStatus != "Confirmed" |
      QuantityStatus != "Not required" |
      RateStatus != "Not applicable" |
      IncludeInPreliminary |
      IncludeInFinal
  )

missing_outside_formula_documentation <- tibble(
  Component = setdiff(
    outside_formula_current_components,
    unique(outside_formula_detail$Component)
  )
)

outside_formula_in_core_detail <- bind_rows(
  current_detail |>
    filter(Component %in% outside_formula_current_components) |>
    transmute(Model = "Current", DistrictCode, Component),
  proposed_detail |>
    filter(Component %in% outside_formula_current_components) |>
    transmute(Model = "Proposed", DistrictCode, Component)
)

stop_if_rows(
  missing_outside_formula_crosswalk,
  "An outside-formula current component is missing from the comparison crosswalk."
)

stop_if_rows(
  unexpected_outside_formula_crosswalk,
  paste(
    "An outside-formula crosswalk row is not classified as confirmed,",
    "not required, not applicable, and excluded from both comparison totals."
  )
)

stop_if_rows(
  missing_outside_formula_documentation,
  "An outside-formula current component is missing from the Step 04 audit output."
)

stop_if_rows(
  outside_formula_in_core_detail,
  paste(
    "An outside-formula component entered the core position-based funding detail.",
    "It must remain only in 04_current_outside_formula_components.csv."
  )
)

scope_leas <- proposed_detail |>
  distinct(DistrictCode, DistrictName, LEAType) |>
  arrange(DistrictCode)


# A. STAFFING-RULE COMPARISON ---------------------------------------------------

# Keep every formula-comparison crosswalk row. Outside-formula rows remain in
# the maintained crosswalk and Step 04 audit output but do not enter this table.
# IncludeInPreliminary controls the working total for modeled categories.
current_staffing_map <- staffing_comparison_crosswalk |>
  filter(SourceModel == "Current") |>
  select(
    SourceComponent,
    ComparisonCategory,
    ComparisonGroup,
    MappingStatus,
    QuantityStatus,
    RateStatus,
    IncludeInPreliminary,
    IncludeInFinal,
    DisplayOrder,
    OutstandingQuestion,
    Notes
  )

proposed_staffing_map <- staffing_comparison_crosswalk |>
  filter(SourceModel == "Proposed") |>
  select(
    SourceComponent,
    ComparisonCategory,
    ComparisonGroup,
    MappingStatus,
    QuantityStatus,
    RateStatus,
    IncludeInPreliminary,
    IncludeInFinal,
    DisplayOrder,
    OutstandingQuestion,
    Notes
  )

current_staffing_detail <- current_detail |>
  inner_join(current_staffing_map, by = c("Component" = "SourceComponent"))

proposed_staffing_detail <- proposed_detail |>
  inner_join(proposed_staffing_map, by = c("Component" = "SourceComponent"))

current_staffing_state <- current_staffing_detail |>
  group_by(ComparisonCategory) |>
  summarise(
    ComparisonGroup = first(ComparisonGroup),
    DisplayOrder = min(DisplayOrder),
    CurrentMappingStatus = paste(sort(unique(MappingStatus)), collapse = "; "),
    CurrentQuantityStatus = paste(sort(unique(QuantityStatus)), collapse = "; "),
    CurrentRateStatus = paste(sort(unique(RateStatus)), collapse = "; "),
    CurrentIncludeInWorkingTotal = all(IncludeInPreliminary),
    CurrentIncludeInFinal = all(IncludeInFinal),
    CurrentKnownQuantity = sum(FundingQuantity, na.rm = TRUE),
    CurrentQuantityMissingRows = sum(
      is.na(FundingQuantity) | !CalculationComplete
    ),
    CurrentKnownFundingAmount = if_else(
      any(
        FundingComplete &
          !is.na(FundingAmount) &
          abs(FundingAmount) > comparison_tolerance
      ) | all(FundingComplete),
      sum(FundingAmount[FundingComplete], na.rm = TRUE),
      NA_real_
    ),
    CurrentFundingMissingRows = sum(
      is.na(FundingAmount) | !FundingComplete
    ),
    CurrentRateValues = list(sort(unique(na.omit(FundingRate)))),
    CurrentOutstandingQuestion = paste(
      unique(OutstandingQuestion[OutstandingQuestion != ""]),
      collapse = " | "
    ),
    CurrentNotes = paste(unique(Notes[Notes != ""]), collapse = " | "),
    .groups = "drop"
  ) |>
  mutate(
    CurrentQuantity = if_else(
      CurrentQuantityMissingRows == 0,
      CurrentKnownQuantity,
      NA_real_
    ),
    CurrentFundingAmount = if_else(
      CurrentFundingMissingRows == 0,
      CurrentKnownFundingAmount,
      NA_real_
    ),
    CurrentCommonRate = map_dbl(
      CurrentRateValues,
      ~ if (length(.x) == 1) .x else NA_real_
    )
  ) |>
  select(-CurrentRateValues)

proposed_staffing_state <- proposed_staffing_detail |>
  group_by(ComparisonCategory) |>
  summarise(
    ComparisonGroup = first(ComparisonGroup),
    DisplayOrder = min(DisplayOrder),
    ProposedMappingStatus = paste(sort(unique(MappingStatus)), collapse = "; "),
    ProposedQuantityStatus = paste(sort(unique(QuantityStatus)), collapse = "; "),
    ProposedRateStatus = paste(sort(unique(RateStatus)), collapse = "; "),
    ProposedIncludeInWorkingTotal = all(IncludeInPreliminary),
    ProposedIncludeInFinal = all(IncludeInFinal),
    ProposedKnownQuantity = sum(FundingQuantity, na.rm = TRUE),
    ProposedQuantityMissingRows = sum(
      is.na(FundingQuantity) | !CalculationComplete
    ),
    ProposedKnownFundingAmount = if_else(
      any(
        FundingComplete &
          !is.na(FundingAmount) &
          abs(FundingAmount) > comparison_tolerance
      ) | all(FundingComplete),
      sum(FundingAmount[FundingComplete], na.rm = TRUE),
      NA_real_
    ),
    ProposedFundingMissingRows = sum(
      is.na(FundingAmount) | !FundingComplete
    ),
    ProposedRateValues = list(sort(unique(na.omit(FundingRate)))),
    ProposedOutstandingQuestion = paste(
      unique(OutstandingQuestion[OutstandingQuestion != ""]),
      collapse = " | "
    ),
    ProposedNotes = paste(unique(Notes[Notes != ""]), collapse = " | "),
    .groups = "drop"
  ) |>
  mutate(
    ProposedQuantity = if_else(
      ProposedQuantityMissingRows == 0,
      ProposedKnownQuantity,
      NA_real_
    ),
    ProposedFundingAmount = if_else(
      ProposedFundingMissingRows == 0,
      ProposedKnownFundingAmount,
      NA_real_
    ),
    ProposedCommonRate = map_dbl(
      ProposedRateValues,
      ~ if (length(.x) == 1) .x else NA_real_
    )
  ) |>
  select(-ProposedRateValues)

staffing_component_comparison <- full_join(
  current_staffing_state,
  proposed_staffing_state,
  by = c("ComparisonCategory", "ComparisonGroup", "DisplayOrder")
) |>
  mutate(
    across(
      c(
        CurrentKnownQuantity,
        ProposedKnownQuantity
      ),
      ~ coalesce(.x, 0)
    ),
    across(
      c(
        CurrentQuantityMissingRows,
        CurrentFundingMissingRows,
        ProposedQuantityMissingRows,
        ProposedFundingMissingRows
      ),
      ~ coalesce(.x, 0L)
    ),
    CurrentMappingStatus = coalesce(CurrentMappingStatus, "Not applicable"),
    ProposedMappingStatus = coalesce(ProposedMappingStatus, "Not applicable"),
    CurrentQuantityStatus = coalesce(CurrentQuantityStatus, "Not applicable"),
    ProposedQuantityStatus = coalesce(ProposedQuantityStatus, "Not applicable"),
    CurrentRateStatus = coalesce(CurrentRateStatus, "Not applicable"),
    ProposedRateStatus = coalesce(ProposedRateStatus, "Not applicable"),
    CurrentIncludeInWorkingTotal = coalesce(
      CurrentIncludeInWorkingTotal,
      FALSE
    ),
    ProposedIncludeInWorkingTotal = coalesce(
      ProposedIncludeInWorkingTotal,
      FALSE
    ),
    CurrentIncludeInFinal = coalesce(CurrentIncludeInFinal, FALSE),
    ProposedIncludeInFinal = coalesce(ProposedIncludeInFinal, FALSE),
    CurrentOutstandingQuestion = coalesce(CurrentOutstandingQuestion, ""),
    ProposedOutstandingQuestion = coalesce(ProposedOutstandingQuestion, ""),
    CurrentNotes = coalesce(CurrentNotes, ""),
    ProposedNotes = coalesce(ProposedNotes, ""),
    AnalysisSection = "Staffing rules",
    CurrentModel = current_model_label,
    ProposedModel = proposed_model_label,
    CommonRate = coalesce(ProposedCommonRate, CurrentCommonRate),
    IncludedInWorkingTotal =
      CurrentIncludeInWorkingTotal & ProposedIncludeInWorkingTotal,
    IncludedInComparableAmountSubtotal =
      IncludedInWorkingTotal &
      !is.na(CurrentKnownFundingAmount) &
      !is.na(ProposedKnownFundingAmount),
    MappingReady =
      CurrentMappingStatus == "Confirmed" &
      ProposedMappingStatus == "Confirmed",
    QuantityReady =
      CurrentQuantityStatus == "Confirmed" &
      ProposedQuantityStatus == "Confirmed" &
      CurrentQuantityMissingRows == 0 &
      ProposedQuantityMissingRows == 0,
    RateReady =
      CurrentRateStatus == "Confirmed" &
      ProposedRateStatus == "Confirmed" &
      CurrentFundingMissingRows == 0 &
      ProposedFundingMissingRows == 0,
    IsCompleteForFinalComparison =
      CurrentIncludeInFinal &
      ProposedIncludeInFinal &
      MappingReady &
      QuantityReady &
      RateReady,
    WorkingCurrentQuantity = if_else(
      CurrentIncludeInWorkingTotal,
      CurrentKnownQuantity,
      NA_real_
    ),
    WorkingProposedQuantity = if_else(
      ProposedIncludeInWorkingTotal,
      ProposedKnownQuantity,
      NA_real_
    ),
    WorkingPositionDifference = if_else(
      IncludedInWorkingTotal,
      WorkingProposedQuantity - WorkingCurrentQuantity,
      NA_real_
    ),
    WorkingCurrentFundingAmount = if_else(
      CurrentIncludeInWorkingTotal,
      CurrentKnownFundingAmount,
      NA_real_
    ),
    WorkingProposedFundingAmount = if_else(
      ProposedIncludeInWorkingTotal,
      ProposedKnownFundingAmount,
      NA_real_
    ),
    WorkingFundingDifference = if_else(
      IncludedInWorkingTotal,
      WorkingProposedFundingAmount - WorkingCurrentFundingAmount,
      NA_real_
    ),
    WorkingPercentDifference = if_else(
      IncludedInWorkingTotal & abs(WorkingCurrentFundingAmount) > 1e-8,
      100 * WorkingFundingDifference / WorkingCurrentFundingAmount,
      NA_real_
    ),
    ConfirmedCurrentQuantity = if_else(
      IsCompleteForFinalComparison,
      CurrentQuantity,
      NA_real_
    ),
    ConfirmedProposedQuantity = if_else(
      IsCompleteForFinalComparison,
      ProposedQuantity,
      NA_real_
    ),
    ConfirmedPositionDifference = if_else(
      IsCompleteForFinalComparison,
      ConfirmedProposedQuantity - ConfirmedCurrentQuantity,
      NA_real_
    ),
    ConfirmedCurrentFundingAmount = if_else(
      IsCompleteForFinalComparison,
      CurrentFundingAmount,
      NA_real_
    ),
    ConfirmedProposedFundingAmount = if_else(
      IsCompleteForFinalComparison,
      ProposedFundingAmount,
      NA_real_
    ),
    ConfirmedFundingDifference = if_else(
      IsCompleteForFinalComparison,
      ConfirmedProposedFundingAmount - ConfirmedCurrentFundingAmount,
      NA_real_
    ),
    ConfirmedPercentDifference = if_else(
      IsCompleteForFinalComparison & abs(ConfirmedCurrentFundingAmount) > 1e-8,
      100 * ConfirmedFundingDifference / ConfirmedCurrentFundingAmount,
      NA_real_
    ),
    ComparisonStatus = case_when(
      IsCompleteForFinalComparison ~ "Confirmed",
      IncludedInWorkingTotal ~ "Provisional",
      TRUE ~ "Not yet estimable"
    ),
    OutstandingQuestion = collapse_nonblank_pair(
      CurrentOutstandingQuestion,
      ProposedOutstandingQuestion
    ),
    Notes = collapse_nonblank_pair(CurrentNotes, ProposedNotes)
  ) |>
  arrange(DisplayOrder, ComparisonCategory) |>
  select(
    AnalysisSection,
    DisplayOrder,
    ComparisonGroup,
    ComparisonCategory,
    ComparisonStatus,
    IncludedInWorkingTotal,
    IncludedInComparableAmountSubtotal,
    IsCompleteForFinalComparison,
    CurrentModel,
    ProposedModel,
    CommonRate,
    CurrentKnownQuantity,
    ProposedKnownQuantity,
    WorkingCurrentQuantity,
    WorkingProposedQuantity,
    WorkingPositionDifference,
    ConfirmedCurrentQuantity,
    ConfirmedProposedQuantity,
    ConfirmedPositionDifference,
    CurrentKnownFundingAmount,
    ProposedKnownFundingAmount,
    WorkingCurrentFundingAmount,
    WorkingProposedFundingAmount,
    WorkingFundingDifference,
    WorkingPercentDifference,
    ConfirmedCurrentFundingAmount,
    ConfirmedProposedFundingAmount,
    ConfirmedFundingDifference,
    ConfirmedPercentDifference,
    CurrentMappingStatus,
    ProposedMappingStatus,
    CurrentQuantityStatus,
    ProposedQuantityStatus,
    CurrentRateStatus,
    ProposedRateStatus,
    CurrentQuantityMissingRows,
    CurrentFundingMissingRows,
    ProposedQuantityMissingRows,
    ProposedFundingMissingRows,
    MappingReady,
    QuantityReady,
    RateReady,
    OutstandingQuestion,
    Notes
  )

current_staffing_lea_detail <- current_staffing_detail |>
  group_by(DistrictCode, ComparisonCategory) |>
  summarise(
    CurrentKnownQuantity = sum(FundingQuantity, na.rm = TRUE),
    CurrentQuantityMissingRows = sum(
      is.na(FundingQuantity) | !CalculationComplete
    ),
    CurrentKnownFundingAmount = sum(FundingAmount, na.rm = TRUE),
    CurrentFundingMissingRows = sum(
      is.na(FundingAmount) | !FundingComplete
    ),
    .groups = "drop"
  )

proposed_staffing_lea_detail <- proposed_staffing_detail |>
  group_by(DistrictCode, ComparisonCategory) |>
  summarise(
    ProposedKnownQuantity = sum(FundingQuantity, na.rm = TRUE),
    ProposedQuantityMissingRows = sum(
      is.na(FundingQuantity) | !CalculationComplete
    ),
    ProposedKnownFundingAmount = sum(FundingAmount, na.rm = TRUE),
    ProposedFundingMissingRows = sum(
      is.na(FundingAmount) | !FundingComplete
    ),
    .groups = "drop"
  )

staffing_category_status <- staffing_component_comparison |>
  select(
    ComparisonCategory,
    IncludedInWorkingTotal,
    IsCompleteForFinalComparison
  )

staffing_lea_comparison <- crossing(
  scope_leas,
  staffing_component_comparison |>
    distinct(ComparisonCategory)
) |>
  left_join(
    current_staffing_lea_detail,
    by = c("DistrictCode", "ComparisonCategory")
  ) |>
  left_join(
    proposed_staffing_lea_detail,
    by = c("DistrictCode", "ComparisonCategory")
  ) |>
  left_join(staffing_category_status, by = "ComparisonCategory") |>
  mutate(
    across(
      c(
        CurrentKnownQuantity,
        CurrentKnownFundingAmount,
        ProposedKnownQuantity,
        ProposedKnownFundingAmount
      ),
      ~ coalesce(.x, 0)
    ),
    across(
      c(
        CurrentQuantityMissingRows,
        CurrentFundingMissingRows,
        ProposedQuantityMissingRows,
        ProposedFundingMissingRows
      ),
      ~ coalesce(.x, 0L)
    )
  ) |>
  group_by(DistrictCode, DistrictName, LEAType) |>
  summarise(
    WorkingCurrentPositions = sum(
      CurrentKnownQuantity[IncludedInWorkingTotal],
      na.rm = TRUE
    ),
    WorkingProposedPositions = sum(
      ProposedKnownQuantity[IncludedInWorkingTotal],
      na.rm = TRUE
    ),
    WorkingCurrentFundingAmount = sum(
      CurrentKnownFundingAmount[IncludedInWorkingTotal],
      na.rm = TRUE
    ),
    WorkingProposedFundingAmount = sum(
      ProposedKnownFundingAmount[IncludedInWorkingTotal],
      na.rm = TRUE
    ),
    ConfirmedCurrentPositions = sum(
      CurrentKnownQuantity[IsCompleteForFinalComparison],
      na.rm = TRUE
    ),
    ConfirmedProposedPositions = sum(
      ProposedKnownQuantity[IsCompleteForFinalComparison],
      na.rm = TRUE
    ),
    ConfirmedCurrentFundingAmount = sum(
      CurrentKnownFundingAmount[IsCompleteForFinalComparison],
      na.rm = TRUE
    ),
    ConfirmedProposedFundingAmount = sum(
      ProposedKnownFundingAmount[IsCompleteForFinalComparison],
      na.rm = TRUE
    ),
    ProvisionalCategoryCount = sum(
      IncludedInWorkingTotal & !IsCompleteForFinalComparison
    ),
    NotYetEstimableCategoryCount = sum(!IncludedInWorkingTotal),
    IsCompleteForFinalComparison = all(
      .data$IsCompleteForFinalComparison[.data$IncludedInWorkingTotal]
    ),
    .groups = "drop"
  ) |>
  mutate(
    AnalysisSection = "Staffing rules",
    ReportingScope = primary_reporting_scope_short,
    CurrentModel = current_model_label,
    ProposedModel = proposed_model_label,
    WorkingFundingDifference =
      WorkingProposedFundingAmount - WorkingCurrentFundingAmount,
    WorkingPercentDifference = if_else(
      abs(WorkingCurrentFundingAmount) > 1e-8,
      100 * WorkingFundingDifference / WorkingCurrentFundingAmount,
      NA_real_
    ),
    ConfirmedFundingDifference =
      ConfirmedProposedFundingAmount - ConfirmedCurrentFundingAmount,
    ConfirmedPercentDifference = if_else(
      abs(ConfirmedCurrentFundingAmount) > 1e-8,
      100 * ConfirmedFundingDifference / ConfirmedCurrentFundingAmount,
      NA_real_
    ),
    ComparisonStatus = if_else(
      IsCompleteForFinalComparison,
      "Confirmed",
      "Provisional"
    )
  ) |>
  select(
    AnalysisSection,
    ReportingScope,
    DistrictCode,
    DistrictName,
    LEAType,
    ComparisonStatus,
    CurrentModel,
    ProposedModel,
    WorkingCurrentPositions,
    WorkingProposedPositions,
    WorkingCurrentFundingAmount,
    WorkingProposedFundingAmount,
    WorkingFundingDifference,
    WorkingPercentDifference,
    ConfirmedCurrentPositions,
    ConfirmedProposedPositions,
    ConfirmedCurrentFundingAmount,
    ConfirmedProposedFundingAmount,
    ConfirmedFundingDifference,
    ConfirmedPercentDifference,
    ProvisionalCategoryCount,
    NotYetEstimableCategoryCount,
    IsCompleteForFinalComparison
  ) |>
  arrange(LEAType, DistrictName)

staffing_statewide_comparison <- staffing_component_comparison |>
  summarise(
    AnalysisSection = "Staffing rules",
    ReportingScope = primary_reporting_scope_short,
    CurrentModel = current_model_label,
    ProposedModel = proposed_model_label,
    LEACount = nrow(scope_leas),
    WorkingCurrentPositions = sum(
      .data$WorkingCurrentQuantity,
      na.rm = TRUE
    ),
    WorkingProposedPositions = sum(
      .data$WorkingProposedQuantity,
      na.rm = TRUE
    ),
    WorkingCurrentFundingAmount = sum(
      .data$WorkingCurrentFundingAmount,
      na.rm = TRUE
    ),
    WorkingProposedFundingAmount = sum(
      .data$WorkingProposedFundingAmount,
      na.rm = TRUE
    ),
    ComparableAmountCurrentPositions = sum(
      CurrentKnownQuantity[IncludedInComparableAmountSubtotal],
      na.rm = TRUE
    ),
    ComparableAmountProposedPositions = sum(
      ProposedKnownQuantity[IncludedInComparableAmountSubtotal],
      na.rm = TRUE
    ),
    ComparableAmountCurrentFundingAmount = sum(
      CurrentKnownFundingAmount[IncludedInComparableAmountSubtotal],
      na.rm = TRUE
    ),
    ComparableAmountProposedFundingAmount = sum(
      ProposedKnownFundingAmount[IncludedInComparableAmountSubtotal],
      na.rm = TRUE
    ),
    IncludesComponentsWithMissingCurrentAmount = any(
      IncludedInWorkingTotal &
        is.na(CurrentKnownFundingAmount) &
        !is.na(ProposedKnownFundingAmount)
    ),
    ConfirmedCurrentPositions = sum(
      .data$ConfirmedCurrentQuantity,
      na.rm = TRUE
    ),
    ConfirmedProposedPositions = sum(
      .data$ConfirmedProposedQuantity,
      na.rm = TRUE
    ),
    ConfirmedCurrentFundingAmount = sum(
      .data$ConfirmedCurrentFundingAmount,
      na.rm = TRUE
    ),
    ConfirmedProposedFundingAmount = sum(
      .data$ConfirmedProposedFundingAmount,
      na.rm = TRUE
    ),
    NotYetEstimableCurrentKnownPositions = sum(
      CurrentKnownQuantity[!IncludedInWorkingTotal],
      na.rm = TRUE
    ),
    NotYetEstimableCurrentKnownFundingAmount = if_else(
      any(!is.na(
        CurrentKnownFundingAmount[!IncludedInWorkingTotal]
      )),
      sum(
        CurrentKnownFundingAmount[!IncludedInWorkingTotal],
        na.rm = TRUE
      ),
      NA_real_
    ),
    ProvisionalCategoryCount = sum(
      IncludedInWorkingTotal & !IsCompleteForFinalComparison
    ),
    NotYetEstimableCategoryCount = sum(!IncludedInWorkingTotal),
    IsCompleteForFinalComparison = all(
      .data$IsCompleteForFinalComparison[.data$IncludedInWorkingTotal]
    )
  ) |>
  mutate(
    WorkingFundingDifference =
      WorkingProposedFundingAmount - WorkingCurrentFundingAmount,
    WorkingPercentDifference = if_else(
      abs(WorkingCurrentFundingAmount) > 1e-8,
      100 * WorkingFundingDifference / WorkingCurrentFundingAmount,
      NA_real_
    ),
    WorkingComparisonBasis = paste(
      "Known current amounts versus all proposed amounts for working",
      "categories; may include proposed amounts where the current amount",
      "is not yet estimable."
    ),
    ComparableAmountFundingDifference =
      ComparableAmountProposedFundingAmount -
      ComparableAmountCurrentFundingAmount,
    ComparableAmountPercentDifference = if_else(
      abs(ComparableAmountCurrentFundingAmount) > 1e-8,
      100 * ComparableAmountFundingDifference /
        ComparableAmountCurrentFundingAmount,
      NA_real_
    ),
    ComparableAmountComparisonBasis = paste(
      "Working categories with a known funding amount on both the current",
      "and proposed sides. Provisional quantities and mappings remain included."
    ),
    ConfirmedFundingDifference =
      ConfirmedProposedFundingAmount - ConfirmedCurrentFundingAmount,
    ConfirmedPercentDifference = if_else(
      abs(ConfirmedCurrentFundingAmount) > 1e-8,
      100 * ConfirmedFundingDifference / ConfirmedCurrentFundingAmount,
      NA_real_
    ),
    ComparisonStatus = if_else(
      IsCompleteForFinalComparison,
      "Confirmed",
      "Provisional"
    )
  )


# B. OPPORTUNITY AND OPERATIONAL FUNDING COMPARISON ----------------------------

weighted_categories <- tibble(
  FundingCategory = c("Opportunity Funding", "Operational Funding"),
  ProposedFundingSection = c(
    "Opportunity Funding (State Support)",
    "Operational Funding (State Support)"
  ),
  ProposedComparisonBasis = c(
    "Fixed statewide pool distributed using low-income and multilingual-learner weighted counts",
    "Fixed statewide pool distributed using enrollment, student-need, and vocational weighted counts"
  )
)

current_weighted_input <- read_csv(
  current_opportunity_operational_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    CurrentFundingAmount = as.numeric(CurrentFundingAmount),
    Source = coalesce(Source, ""),
    Notes = coalesce(Notes, "")
  ) |>
  filter(!DistrictCode %in% primary_reporting_excluded_lea_codes)

invalid_weighted_categories <- current_weighted_input |>
  filter(!FundingCategory %in% weighted_categories$FundingCategory)

stop_if_rows(
  invalid_weighted_categories,
  "current_opportunity_operational_funding.csv contains an invalid FundingCategory."
)

current_weighted_lea <- current_weighted_input |>
  group_by(DistrictCode, FundingCategory) |>
  summarise(
    CurrentFundingAmount = sum(CurrentFundingAmount, na.rm = FALSE),
    CurrentSource = paste(unique(Source[Source != ""]), collapse = " | "),
    CurrentNotes = paste(unique(Notes[Notes != ""]), collapse = " | "),
    CurrentRows = n(),
    .groups = "drop"
  )

proposed_weighted_lea <- proposed_detail |>
  filter(FundingSection %in% weighted_categories$ProposedFundingSection) |>
  left_join(
    weighted_categories,
    by = c("FundingSection" = "ProposedFundingSection")
  ) |>
  group_by(DistrictCode, FundingCategory) |>
  summarise(
    ProposedFundingAmount = sum(FundingAmount, na.rm = FALSE),
    .groups = "drop"
  )

weighted_lea_comparison <- crossing(
  scope_leas,
  weighted_categories |>
    select(FundingCategory, ProposedComparisonBasis)
) |>
  left_join(
    current_weighted_lea,
    by = c("DistrictCode", "FundingCategory")
  ) |>
  left_join(
    proposed_weighted_lea,
    by = c("DistrictCode", "FundingCategory")
  ) |>
  mutate(
    AnalysisSection = "Opportunity and Operational Funding",
    ReportingScope = primary_reporting_scope_short,
    CurrentModel = current_model_label,
    ProposedModel = proposed_model_label,
    CurrentComparisonBasis = if_else(
      is.na(CurrentRows),
      "Pending confirmed FY2025-26 current analogue and LEA allocation",
      "Current LEA allocation input"
    ),
    CurrentComplete = !is.na(CurrentRows) & !is.na(CurrentFundingAmount),
    ProposedComplete = !is.na(ProposedFundingAmount),
    IsCompleteForFinalComparison = CurrentComplete & ProposedComplete,
    WorkingFundingDifference = if_else(
      CurrentComplete,
      ProposedFundingAmount - CurrentFundingAmount,
      NA_real_
    ),
    WorkingPercentDifference = if_else(
      CurrentComplete & abs(CurrentFundingAmount) > 1e-8,
      100 * WorkingFundingDifference / CurrentFundingAmount,
      NA_real_
    ),
    ComparisonStatus = if_else(
      IsCompleteForFinalComparison,
      "Confirmed",
      "Not yet estimable"
    )
  ) |>
  select(
    AnalysisSection,
    ReportingScope,
    FundingCategory,
    DistrictCode,
    DistrictName,
    LEAType,
    ComparisonStatus,
    CurrentModel,
    ProposedModel,
    CurrentComparisonBasis,
    ProposedComparisonBasis,
    CurrentFundingAmount,
    ProposedFundingAmount,
    WorkingFundingDifference,
    WorkingPercentDifference,
    CurrentSource,
    CurrentNotes,
    IsCompleteForFinalComparison
  ) |>
  arrange(FundingCategory, LEAType, DistrictName)

weighted_state_comparison <- weighted_lea_comparison |>
  group_by(
    AnalysisSection,
    ReportingScope,
    FundingCategory,
    CurrentModel,
    ProposedModel,
    ProposedComparisonBasis
  ) |>
  summarise(
    ExpectedLEACount = n(),
    CurrentLEAsProvided = sum(!is.na(CurrentFundingAmount)),
    CurrentKnownFundingAmount = if (
      all(is.na(CurrentFundingAmount))
    ) {
      NA_real_
    } else {
      sum(CurrentFundingAmount, na.rm = TRUE)
    },
    ProposedFundingAmount = sum(ProposedFundingAmount, na.rm = FALSE),
    IsCompleteForFinalComparison = all(IsCompleteForFinalComparison),
    .groups = "drop"
  ) |>
  mutate(
    CurrentComparisonBasis = case_when(
      IsCompleteForFinalComparison ~ "Complete current LEA allocation input",
      CurrentLEAsProvided > 0 ~ "Partial current LEA allocation input",
      TRUE ~ "Pending confirmed FY2025-26 current analogue and LEA allocation"
    ),
    WorkingFundingDifference = if_else(
      CurrentLEAsProvided > 0,
      ProposedFundingAmount - CurrentKnownFundingAmount,
      NA_real_
    ),
    WorkingPercentDifference = if_else(
      CurrentLEAsProvided > 0 & abs(CurrentKnownFundingAmount) > 1e-8,
      100 * WorkingFundingDifference / CurrentKnownFundingAmount,
      NA_real_
    ),
    ComparisonStatus = case_when(
      IsCompleteForFinalComparison ~ "Confirmed",
      CurrentLEAsProvided > 0 ~ "Provisional",
      TRUE ~ "Not yet estimable"
    )
  ) |>
  select(
    AnalysisSection,
    ReportingScope,
    FundingCategory,
    ComparisonStatus,
    CurrentModel,
    ProposedModel,
    CurrentComparisonBasis,
    ProposedComparisonBasis,
    ExpectedLEACount,
    CurrentLEAsProvided,
    CurrentKnownFundingAmount,
    ProposedFundingAmount,
    WorkingFundingDifference,
    WorkingPercentDifference,
    IsCompleteForFinalComparison
  )


# QC AND OUTPUT -----------------------------------------------------------------

expected_scope_leas <- read_csv(lea_crosswalk_path, show_col_types = FALSE) |>
  mutate(DistrictCode = as.integer(DistrictCode)) |>
  filter(!DistrictCode %in% primary_reporting_excluded_lea_codes)

unmapped_current_staffing <- current_detail |>
  filter(FundingSection %in% c(
    "Base Funding (State Support)",
    "Central Office Funding (State Support)"
  )) |>
  distinct(Component) |>
  anti_join(
    staffing_comparison_crosswalk |>
      filter(SourceModel == "Current") |>
      distinct(Component = SourceComponent),
    by = "Component"
  )

unmapped_proposed_staffing <- proposed_detail |>
  filter(FundingSection %in% c(
    "Base Funding (State Support)",
    "Central Office Funding (State Support)"
  )) |>
  distinct(Component) |>
  anti_join(
    staffing_comparison_crosswalk |>
      filter(SourceModel == "Proposed") |>
      distinct(Component = SourceComponent),
    by = "Component"
  )

comparison_qc <- tibble(
  CheckType = "Integrity",
  Check = c(
    "Primary scope LEA count matches maintained crosswalk",
    "BASSE is included in the primary scope",
    "DAFB is excluded from the primary scope",
    "All current staffing components are represented in the comparison crosswalk",
    "All proposed staffing components are represented in the comparison crosswalk",
    "Proposed Opportunity Funding sums to the fixed pool",
    "Proposed Operational Funding sums to the fixed pool"
  ),
  Expected = c(
    as.character(nrow(expected_scope_leas)),
    "TRUE",
    "TRUE",
    "0",
    "0",
    as.character(opportunity_funding_pool),
    as.character(operational_funding_pool)
  ),
  Actual = c(
    as.character(nrow(scope_leas)),
    as.character(basse_district_code %in% scope_leas$DistrictCode),
    as.character(!dafb_district_code %in% scope_leas$DistrictCode),
    as.character(nrow(unmapped_current_staffing)),
    as.character(nrow(unmapped_proposed_staffing)),
    as.character(
      weighted_state_comparison |>
        filter(FundingCategory == "Opportunity Funding") |>
        pull(ProposedFundingAmount)
    ),
    as.character(
      weighted_state_comparison |>
        filter(FundingCategory == "Operational Funding") |>
        pull(ProposedFundingAmount)
    )
  ),
  Pass = c(
    nrow(scope_leas) == nrow(expected_scope_leas),
    basse_district_code %in% scope_leas$DistrictCode,
    !dafb_district_code %in% scope_leas$DistrictCode,
    nrow(unmapped_current_staffing) == 0,
    nrow(unmapped_proposed_staffing) == 0,
    abs(
      weighted_state_comparison |>
        filter(FundingCategory == "Opportunity Funding") |>
        pull(ProposedFundingAmount) - opportunity_funding_pool
    ) <= comparison_tolerance,
    abs(
      weighted_state_comparison |>
        filter(FundingCategory == "Operational Funding") |>
        pull(ProposedFundingAmount) - operational_funding_pool
    ) <= comparison_tolerance
  )
)

newly_confirmed_categories <- c(
  "Administrative Support Professionals",
  "Instructional Supports"
)

newly_confirmed_category_qc <- tibble(
  ComparisonCategory = newly_confirmed_categories
) |>
  left_join(
    staffing_component_comparison |>
      select(
        ComparisonCategory,
        IsCompleteForFinalComparison,
        ComparisonStatus,
        OutstandingQuestion
      ),
    by = "ComparisonCategory"
  )

comparison_qc <- bind_rows(
  comparison_qc,
  tibble(
    CheckType = "New guidance",
    Check = c(
      "Outside-formula components are absent from core funding detail",
      "Outside-formula crosswalk rows are excluded from both comparison totals",
      "Outside-formula components are preserved in the Step 04 audit output",
      "Newly confirmed functional crosswalk categories enter the confirmed subtotal",
      "Newly confirmed functional crosswalk categories have no outstanding question"
    ),
    Expected = c(
      "0",
      "0",
      as.character(length(outside_formula_current_components)),
      as.character(length(newly_confirmed_categories)),
      "0"
    ),
    Actual = c(
      as.character(nrow(outside_formula_in_core_detail)),
      as.character(nrow(unexpected_outside_formula_crosswalk)),
      as.character(
        sum(
          outside_formula_current_components %in%
            unique(outside_formula_detail$Component)
        )
      ),
      as.character(sum(
        newly_confirmed_category_qc$IsCompleteForFinalComparison %in% TRUE
      )),
      as.character(sum(
        is.na(newly_confirmed_category_qc$OutstandingQuestion) |
          newly_confirmed_category_qc$OutstandingQuestion != ""
      ))
    ),
    Pass = c(
      nrow(outside_formula_in_core_detail) == 0,
      nrow(unexpected_outside_formula_crosswalk) == 0,
      all(
        outside_formula_current_components %in%
          unique(outside_formula_detail$Component)
      ),
      all(newly_confirmed_category_qc$IsCompleteForFinalComparison %in% TRUE),
      all(
        !is.na(newly_confirmed_category_qc$OutstandingQuestion) &
          newly_confirmed_category_qc$OutstandingQuestion == ""
      )
    )
  )
)

write_model_csv(staffing_component_comparison, staffing_component_path)
write_model_csv(staffing_lea_comparison, staffing_lea_path)
write_model_csv(staffing_statewide_comparison, staffing_state_path)
write_model_csv(weighted_state_comparison, weighted_state_path)
write_model_csv(weighted_lea_comparison, weighted_lea_path)
write_review_csv(comparison_qc, comparison_qc_path)

if (any(!comparison_qc$Pass)) {
  print(comparison_qc |> filter(!Pass))
  stop("Step 09 integrity checks failed.", call. = FALSE)
}

message("Step 09 complete: current-versus-proposed comparisons created.")
