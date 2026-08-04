# =============================================================================
# 10_reconcile_pefc_workbook.R
# =============================================================================
# Separately reconciles the PEFC workbook against the independently reproduced
# proposed model. This is validation work, not part of the primary current-
# versus-proposed comparison.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

proposed_detail_path <- file.path(
  intermediate_dir,
  "08_proposed_model_funding_detail.csv"
)
charter_units_path <- file.path(
  intermediate_dir,
  "06_proposed_school_calculation_units.csv"
)

pefc_state_path <- file.path(audit_dir, "10_pefc_statewide_as_presented.csv")
pefc_component_path <- file.path(audit_dir, "10_pefc_component_comparison.csv")
pefc_lea_path <- file.path(audit_dir, "10_pefc_lea_comparison.csv")
pefc_reconciliation_path <- file.path(
  audit_dir,
  "10_pefc_reconciliation_summary.csv"
)
pefc_qc_path <- file.path(audit_dir, "10_pefc_qc.csv")
pefc_dafb_scope_path <- file.path(
  audit_dir,
  "10_pefc_dafb_scope_discrepancy.csv"
)
charter_building_path <- file.path(
  audit_dir,
  "10_charter_building_treatment.csv"
)

check_required_files(c(
  calculator_path,
  proposed_detail_path,
  charter_units_path,
  lea_crosswalk_path
))

proposed_detail <- read_csv(proposed_detail_path, show_col_types = FALSE) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    FundingQuantity = as.numeric(FundingQuantity),
    FundingRate = as.numeric(FundingRate),
    FundingAmount = as.numeric(FundingAmount)
  )

lea_crosswalk <- read_csv(lea_crosswalk_path, show_col_types = FALSE) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    CalculatorKey = normalize_name(CalculatorLEAName)
  )

calculator_raw <- read_excel(
  calculator_path,
  sheet = "Calculator",
  col_names = FALSE
)

summary_raw <- read_excel(
  calculator_path,
  sheet = "Summary"
)

cell_number <- function(data, row_number, column_number) {
  as.numeric(data[[column_number]][row_number])
}


# PEFC WORKBOOK EXTRACTION ------------------------------------------------------

pefc_component_map <- tribble(
  ~DisplayOrder, ~RowNumber, ~FundingSection, ~Component,
  10L, 6L, "Base Funding", "Division I Teacher - K-3 Regular Education",
  20L, 7L, "Base Funding", "Division I Teacher - 4-12 Regular Education",
  30L, 8L, "Base Funding", "Division I Teacher - Pre-K-12 Basic Special Education",
  40L, 9L, "Base Funding", "Division I Teacher - Intensive Special Education",
  50L, 10L, "Base Funding", "Division I Teacher - Complex Special Education",
  60L, 11L, "Base Funding", "Vocational Deduct",
  70L, 12L, "Base Funding", "Vocational Division I",
  80L, 13L, "Base Funding", "Principal",
  90L, 14L, "Base Funding", "Assistant Principal",
  100L, 15L, "Base Funding", "Administrative Support Professionals",
  110L, 16L, "Base Funding", "Instructional Supports",
  200L, 20L, "Opportunity Funding", "Opportunity Funding - Low Income",
  210L, 21L, "Opportunity Funding", "Opportunity Funding - Multilingual Learner",
  300L, 25L, "Operational Funding", "Operational Funding - Enrollment",
  310L, 26L, "Operational Funding", "Operational Funding - Low Income",
  320L, 27L, "Operational Funding", "Operational Funding - Multilingual Learner",
  330L, 28L, "Operational Funding", "Operational Funding - Basic Special Education",
  340L, 29L, "Operational Funding", "Operational Funding - Intensive Special Education",
  350L, 30L, "Operational Funding", "Operational Funding - Complex Special Education",
  360L, 31L, "Operational Funding", "Operational Funding - Vocational",
  400L, 35L, "Central Office Funding", "Superintendent",
  410L, 36L, "Central Office Funding", "Administrative Assistant",
  420L, 37L, "Central Office Funding", "Assistant Superintendent",
  430L, 38L, "Central Office Funding", "Director",
  440L, 39L, "Central Office Funding", "11-Month Supervisor",
  450L, 40L, "Central Office Funding", "Buildings and Grounds Supervisor",
  460L, 41L, "Central Office Funding", "Food Services Supervisor",
  470L, 42L, "Central Office Funding", "Transportation Supervisor",
  480L, 43L, "Central Office Funding", "Reading Cadre"
)

pefc_component_detail <- pefc_component_map |>
  mutate(
    PEFCQuantity = map_dbl(
      RowNumber,
      ~ cell_number(calculator_raw, .x, 4L)
    ),
    PEFCRate = map_dbl(
      RowNumber,
      ~ cell_number(calculator_raw, .x, 5L)
    ),
    PEFCFundingAmount = map_dbl(
      RowNumber,
      ~ cell_number(calculator_raw, .x, 6L)
    )
  )

pefc_statewide_as_presented <- tribble(
  ~FundingMetric, ~PEFCWorkbookAmount,
  "Base Funding", cell_number(calculator_raw, 17L, 6L),
  "Central Office Funding", cell_number(calculator_raw, 44L, 6L),
  "Opportunity Funding", cell_number(calculator_raw, 22L, 6L),
  "Operational Funding", cell_number(calculator_raw, 32L, 6L),
  "Total modeled funding", cell_number(calculator_raw, 45L, 6L)
) |>
  mutate(
    Model = pefc_model_label,
    ReportingScope = pefc_as_presented_scope_label,
    .before = 1
  )

pefc_lea <- summary_raw |>
  filter(
    !is.na(.data[["District"]]),
    !.data[["District"]] %in% c("CHECK:", "Statewide")
  ) |>
  mutate(
    CalculatorLEAName = as.character(.data[["District"]]),
    CalculatorTotalRow = as.character(.data[["Child"]]),
    IsLEATotalRow = coalesce(.data[["Type"]] == "Central Office", FALSE) |
      normalize_name(CalculatorTotalRow) == normalize_name(CalculatorLEAName) |
      (
        CalculatorLEAName == "DAFB" &
        CalculatorTotalRow == "Dover Air Force Base"
      ),
    CalculatorKey = normalize_name(CalculatorLEAName)
  ) |>
  filter(IsLEATotalRow) |>
  transmute(
    CalculatorLEAName,
    CalculatorTotalRow,
    CalculatorKey,
    PEFCBaseFundingAmount = as.numeric(.data[["Base Total"]]),
    PEFCOpportunityFundingAmount = as.numeric(.data[["Opportunity Total"]]),
    PEFCOperationalFundingAmount = as.numeric(.data[["Operational Total"]]),
    PEFCCentralOfficeFundingAmount = as.numeric(.data[["Central Office Total"]]),
    PEFCTotalFundingAmount = as.numeric(.data[["Grand Total"]])
  ) |>
  left_join(
    lea_crosswalk |>
      select(
        DistrictCode,
        DistrictName,
        LEAType,
        IncludeInStatewide,
        CalculatorKey
      ),
    by = "CalculatorKey"
  ) |>
  select(
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    CalculatorLEAName,
    CalculatorTotalRow,
    starts_with("PEFC")
  ) |>
  arrange(DistrictCode)

unmatched_pefc_leas <- pefc_lea |>
  filter(is.na(DistrictCode))

stop_if_rows(
  unmatched_pefc_leas,
  "One or more PEFC Summary LEA rows did not match lea_crosswalk.csv."
)

# DAFB is retained below only to audit the PEFC workbook as presented. The
# aligned IV&V scope is controlled by IncludeInStatewide and must exclude DAFB.
dafb_pefc_row <- pefc_lea |>
  filter(DistrictCode == dafb_district_code)

dafb_independent_rows <- proposed_detail |>
  filter(DistrictCode == dafb_district_code)

if (nrow(dafb_pefc_row) != 1L) {
  stop(
    "The PEFC Summary must contain exactly one DAFB LEA-total row for audit.",
    call. = FALSE
  )
}

if (nrow(dafb_independent_rows) == 0L) {
  stop(
    "The independent proposed detail is missing the DAFB source-audit rows.",
    call. = FALSE
  )
}

if (any(dafb_independent_rows$IncludeInStatewide %in% TRUE, na.rm = TRUE)) {
  stop(
    "DAFB is marked for inclusion in aligned IV&V totals.",
    call. = FALSE
  )
}


# CHARTER BUILDING TREATMENT ----------------------------------------------------

charter_units <- read_csv(charter_units_path, show_col_types = FALSE) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    CalculationUnitSequence = as.integer(CalculationUnitSequence),
    SchoolCalculationUnitCount = as.integer(SchoolCalculationUnitCount)
  ) |>
  filter(LEAType == "Charter")

charter_base_detail <- proposed_detail |>
  filter(
    LEAType == "Charter",
    CalculationLevel == "School",
    FundingSection == "Base Funding (State Support)"
  ) |>
  group_by(
    DistrictCode,
    SchoolCode,
    CalculationUnitName,
    CalculationUnitSequence
  ) |>
  summarise(
    BasePositionQuantity = sum(FundingQuantity, na.rm = FALSE),
    BaseFundingAmount = sum(FundingAmount, na.rm = FALSE),
    PrincipalPositions = sum(
      FundingQuantity[Component == "Principal"],
      na.rm = TRUE
    ),
    AssistantPrincipalPositions = sum(
      FundingQuantity[Component == "Assistant Principal"],
      na.rm = TRUE
    ),
    AdministrativeSupportPositions = sum(
      FundingQuantity[
        Component == "Administrative Support Professionals"
      ],
      na.rm = TRUE
    ),
    InstructionalSupportPositions = sum(
      FundingQuantity[Component == "Instructional Supports"],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

charter_building_treatment <- charter_units |>
  left_join(
    charter_base_detail,
    by = c(
      "DistrictCode",
      "SchoolCode",
      "CalculationUnitName",
      "CalculationUnitSequence"
    )
  ) |>
  mutate(
    OfficialSchoolCode = SchoolCode,
    OfficialSchoolName = SchoolName,
    PEFCBuildingName = CalculationUnitName,
    PEFCBuildingSequence = CalculationUnitSequence,
    PEFCBuildingCount = SchoolCalculationUnitCount,
    IsMultiBuildingCharter = PEFCBuildingCount > 1,
    IsAdditionalPEFCBuildingRow = PEFCBuildingSequence > 1,
    OfficialSchoolCodeRepeatedAcrossPEFCBuildings =
      IsMultiBuildingCharter,
    AdditionalCalculationUnitsBeyondOfficialSchoolCode =
      pmax(PEFCBuildingCount - 1L, 0L),
    BuildingClassification = case_when(
      IsAdditionalPEFCBuildingRow ~
        "Additional PEFC-created building calculation unit",
      IsMultiBuildingCharter ~
        "First PEFC building calculation unit in a multi-building charter",
      TRUE ~ "Single PEFC building calculation unit"
    ),
    AdditionalPrincipalPositionsFromExtraBuildingRows = if_else(
      IsAdditionalPEFCBuildingRow,
      PrincipalPositions,
      0
    ),
    BaseStaffingTreatment = paste(
      "Each PEFC building row is treated as a separate school calculation",
      "unit for proposed Base staffing rules."
    ),
    OpportunityOperationalTreatment = paste(
      "Official charter LEA totals are used once for Opportunity and",
      "Operational Funding; building rows do not duplicate weighted counts."
    ),
    ReviewNote = if_else(
      IsMultiBuildingCharter,
      paste(
        "This charter has multiple PEFC building rows under one official",
        "school code. Review school-based positions generated for each row."
      ),
      "The charter has one PEFC building row under its official school code."
    )
  ) |>
  select(
    DistrictCode,
    DistrictName,
    OfficialSchoolCode,
    OfficialSchoolName,
    PEFCBuildingName,
    PEFCBuildingSequence,
    PEFCBuildingCount,
    BuildingClassification,
    IsMultiBuildingCharter,
    IsAdditionalPEFCBuildingRow,
    OfficialSchoolCodeRepeatedAcrossPEFCBuildings,
    AdditionalCalculationUnitsBeyondOfficialSchoolCode,
    CalculationUnitSource,
    AllocationStatus,
    EnrollmentK3,
    Enrollment4_12,
    BasicPreK12Enrollment,
    EnrollmentIntense,
    EnrollmentComplex,
    Enrollment,
    PrincipalPositions,
    AdditionalPrincipalPositionsFromExtraBuildingRows,
    AssistantPrincipalPositions,
    AdministrativeSupportPositions,
    InstructionalSupportPositions,
    BasePositionQuantity,
    BaseFundingAmount,
    BaseStaffingTreatment,
    OpportunityOperationalTreatment,
    ReviewNote
  ) |>
  arrange(DistrictName, PEFCBuildingSequence)


# SOURCE-AUDIT REPRODUCTION OF THE PEFC AS-PRESENTED SCOPE ---------------------

# This section intentionally reproduces the workbook's inconsistent DAFB scope
# solely for audit: Base, Opportunity, and Operational include DAFB, while
# Central Office excludes it. These amounts are not valid aligned IV&V totals.
independent_as_presented_position_components <- proposed_detail |>
  filter(
    FundingSection == "Base Funding (State Support)" |
      (
        FundingSection == "Central Office Funding (State Support)" &
        DistrictCode != dafb_district_code
      )
  ) |>
  mutate(
    FundingSection = recode(
      FundingSection,
      "Base Funding (State Support)" = "Base Funding",
      "Central Office Funding (State Support)" = "Central Office Funding"
    )
  ) |>
  group_by(FundingSection, Component) |>
  summarise(
    IndependentQuantity = sum(FundingQuantity, na.rm = FALSE),
    IndependentRateValues = list(sort(unique(na.omit(FundingRate)))),
    IndependentFundingAmount = sum(FundingAmount, na.rm = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    IndependentRate = map_dbl(
      IndependentRateValues,
      ~ if (length(.x) == 1) .x else NA_real_
    )
  ) |>
  select(-IndependentRateValues)

# Opportunity and Operational rates are recalculated across all PEFC-scope
# organizations so each statewide pool remains fixed.
independent_as_presented_weighted_components <- proposed_detail |>
  filter(FundingSection %in% c(
    "Opportunity Funding (State Support)",
    "Operational Funding (State Support)"
  )) |>
  mutate(
    FundingSection = recode(
      FundingSection,
      "Opportunity Funding (State Support)" = "Opportunity Funding",
      "Operational Funding (State Support)" = "Operational Funding"
    )
  ) |>
  group_by(FundingSection, Component) |>
  summarise(
    IndependentQuantity = sum(FundingQuantity, na.rm = FALSE),
    .groups = "drop"
  ) |>
  group_by(FundingSection) |>
  mutate(
    SectionWeightedCount = sum(IndependentQuantity),
    IndependentRate = case_when(
      FundingSection == "Opportunity Funding" ~
        opportunity_funding_pool / SectionWeightedCount,
      FundingSection == "Operational Funding" ~
        operational_funding_pool / SectionWeightedCount,
      TRUE ~ NA_real_
    ),
    IndependentFundingAmount = IndependentQuantity * IndependentRate
  ) |>
  ungroup() |>
  select(
    FundingSection,
    Component,
    IndependentQuantity,
    IndependentRate,
    IndependentFundingAmount
  )

independent_as_presented_components <- bind_rows(
  independent_as_presented_position_components,
  independent_as_presented_weighted_components
)

pefc_component_comparison <- pefc_component_detail |>
  left_join(
    independent_as_presented_components,
    by = c("FundingSection", "Component")
  ) |>
  mutate(
    ComparisonType = "Component formula/input comparison",
    PEFCModel = pefc_model_label,
    IndependentModel = proposed_model_label,
    ReportingScope = pefc_as_presented_scope_label,
    QuantityDifference = IndependentQuantity - PEFCQuantity,
    RateDifference = IndependentRate - PEFCRate,
    FundingDifference = IndependentFundingAmount - PEFCFundingAmount,
    FundingPercentDifference = if_else(
      abs(PEFCFundingAmount) > 1e-8,
      100 * FundingDifference / PEFCFundingAmount,
      NA_real_
    ),
    MatchWithinTolerance = abs(FundingDifference) <= comparison_tolerance
  ) |>
  select(
    ComparisonType,
    ReportingScope,
    DisplayOrder,
    FundingSection,
    Component,
    PEFCModel,
    IndependentModel,
    PEFCQuantity,
    IndependentQuantity,
    QuantityDifference,
    PEFCRate,
    IndependentRate,
    RateDifference,
    PEFCFundingAmount,
    IndependentFundingAmount,
    FundingDifference,
    FundingPercentDifference,
    MatchWithinTolerance
  ) |>
  arrange(DisplayOrder)


# LEA-LEVEL AS-PRESENTED AND NORMALIZED COMPARISONS ----------------------------

independent_as_presented_position_lea <- proposed_detail |>
  filter(
    FundingSection == "Base Funding (State Support)" |
      (
        FundingSection == "Central Office Funding (State Support)" &
        DistrictCode != dafb_district_code
      )
  ) |>
  mutate(
    FundingMetric = recode(
      FundingSection,
      "Base Funding (State Support)" = "Base Funding",
      "Central Office Funding (State Support)" = "Central Office Funding"
    )
  ) |>
  group_by(DistrictCode, FundingMetric) |>
  summarise(
    IndependentFundingAmount = sum(FundingAmount, na.rm = FALSE),
    .groups = "drop"
  )

independent_as_presented_weighted_lea <- proposed_detail |>
  filter(FundingSection %in% c(
    "Opportunity Funding (State Support)",
    "Operational Funding (State Support)"
  )) |>
  mutate(
    FundingMetric = recode(
      FundingSection,
      "Opportunity Funding (State Support)" = "Opportunity Funding",
      "Operational Funding (State Support)" = "Operational Funding"
    )
  ) |>
  group_by(DistrictCode, FundingMetric) |>
  summarise(
    WeightedCount = sum(FundingQuantity, na.rm = FALSE),
    .groups = "drop"
  ) |>
  group_by(FundingMetric) |>
  mutate(
    SectionWeightedCount = sum(WeightedCount),
    IndependentRate = case_when(
      FundingMetric == "Opportunity Funding" ~
        opportunity_funding_pool / SectionWeightedCount,
      FundingMetric == "Operational Funding" ~
        operational_funding_pool / SectionWeightedCount,
      TRUE ~ NA_real_
    ),
    IndependentFundingAmount = WeightedCount * IndependentRate
  ) |>
  ungroup() |>
  select(
    DistrictCode,
    FundingMetric,
    IndependentFundingAmount
  )

independent_as_presented_lea_long <- bind_rows(
  independent_as_presented_position_lea,
  independent_as_presented_weighted_lea
)

pefc_lea_long <- pefc_lea |>
  select(
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    PEFCBaseFundingAmount,
    PEFCCentralOfficeFundingAmount,
    PEFCOpportunityFundingAmount,
    PEFCOperationalFundingAmount
  ) |>
  pivot_longer(
    cols = starts_with("PEFC"),
    names_to = "FundingMetric",
    values_to = "PEFCFundingAmount"
  ) |>
  mutate(
    FundingMetric = recode(
      FundingMetric,
      "PEFCBaseFundingAmount" = "Base Funding",
      "PEFCCentralOfficeFundingAmount" = "Central Office Funding",
      "PEFCOpportunityFundingAmount" = "Opportunity Funding",
      "PEFCOperationalFundingAmount" = "Operational Funding"
    )
  )

pefc_dafb_scope_discrepancy <- pefc_lea_long |>
  filter(DistrictCode == dafb_district_code) |>
  mutate(
    WorkbookSectionTreatment = if_else(
      abs(PEFCFundingAmount) > comparison_tolerance,
      "Included in PEFC workbook funding",
      "Excluded from PEFC workbook funding"
    ),
    CorrectAlignedIVVTreatment =
      "Excluded because DAFB does not receive state funding",
    WorkbookScopeStatus = case_when(
      FundingMetric %in% c(
        "Base Funding",
        "Opportunity Funding",
        "Operational Funding"
      ) & abs(PEFCFundingAmount) > comparison_tolerance ~
        "Confirmed PEFC workbook scope discrepancy",
      FundingMetric == "Central Office Funding" &
        abs(PEFCFundingAmount) <= comparison_tolerance ~
        "PEFC workbook already excludes DAFB",
      TRUE ~ "Unexpected PEFC workbook treatment; review required"
    ),
    ScopeDecision = dafb_scope_decision,
    ScopeDiscrepancyNote = pefc_dafb_scope_discrepancy_note
  ) |>
  select(
    DistrictCode,
    DistrictName,
    FundingMetric,
    PEFCFundingAmount,
    WorkbookSectionTreatment,
    CorrectAlignedIVVTreatment,
    WorkbookScopeStatus,
    ScopeDecision,
    ScopeDiscrepancyNote
  ) |>
  arrange(match(
    FundingMetric,
    c(
      "Base Funding",
      "Central Office Funding",
      "Opportunity Funding",
      "Operational Funding"
    )
  ))

pefc_lea_as_presented_comparison <- pefc_lea_long |>
  left_join(
    independent_as_presented_lea_long,
    by = c("DistrictCode", "FundingMetric")
  ) |>
  mutate(
    IndependentFundingAmount = case_when(
      DistrictCode == dafb_district_code &
        FundingMetric == "Central Office Funding" &
        is.na(IndependentFundingAmount) ~ 0,
      TRUE ~ IndependentFundingAmount
    ),
    ComparisonScope = "PEFC workbook as presented",
    ReportingScope = pefc_as_presented_scope_label,
    FundingDifference = IndependentFundingAmount - PEFCFundingAmount,
    FundingPercentDifference = if_else(
      abs(PEFCFundingAmount) > 1e-8,
      100 * FundingDifference / PEFCFundingAmount,
      NA_real_
    ),
    MatchWithinTolerance = abs(FundingDifference) <= comparison_tolerance
  )

independent_primary_lea_long <- proposed_detail |>
  filter(
    IncludeInStatewide %in% TRUE,
    !DistrictCode %in% primary_reporting_excluded_lea_codes
  ) |>
  mutate(
    FundingMetric = recode(
      FundingSection,
      "Base Funding (State Support)" = "Base Funding",
      "Central Office Funding (State Support)" = "Central Office Funding",
      "Opportunity Funding (State Support)" = "Opportunity Funding",
      "Operational Funding (State Support)" = "Operational Funding"
    )
  ) |>
  group_by(DistrictCode, FundingMetric) |>
  summarise(
    IndependentFundingAmount = sum(FundingAmount, na.rm = FALSE),
    .groups = "drop"
  )

pefc_primary_lea_comparison <- pefc_lea_long |>
  filter(
    IncludeInStatewide %in% TRUE,
    !DistrictCode %in% primary_reporting_excluded_lea_codes
  ) |>
  left_join(
    independent_primary_lea_long,
    by = c("DistrictCode", "FundingMetric")
  ) |>
  mutate(
    ComparisonScope = "Primary comparable-model scope",
    ReportingScope = primary_reporting_scope_short,
    FundingDifference = IndependentFundingAmount - PEFCFundingAmount,
    FundingPercentDifference = if_else(
      abs(PEFCFundingAmount) > 1e-8,
      100 * FundingDifference / PEFCFundingAmount,
      NA_real_
    ),
    MatchWithinTolerance = abs(FundingDifference) <= comparison_tolerance
  )

pefc_lea_comparison <- bind_rows(
  pefc_lea_as_presented_comparison,
  pefc_primary_lea_comparison
) |>
  select(
    ComparisonScope,
    ReportingScope,
    FundingMetric,
    DistrictCode,
    DistrictName,
    LEAType,
    PEFCFundingAmount,
    IndependentFundingAmount,
    FundingDifference,
    FundingPercentDifference,
    MatchWithinTolerance
  ) |>
  arrange(ComparisonScope, FundingMetric, LEAType, DistrictName)


# RECONCILIATION SUMMARY --------------------------------------------------------

pefc_summary_state <- pefc_lea_long |>
  group_by(FundingMetric) |>
  summarise(
    SummaryLEAAmount = sum(PEFCFundingAmount, na.rm = FALSE),
    .groups = "drop"
  )

pefc_summary_state <- bind_rows(
  pefc_summary_state,
  tibble(
    FundingMetric = "Position-based Funding",
    SummaryLEAAmount = sum(
      pefc_summary_state$SummaryLEAAmount[
        pefc_summary_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding"
        )
      ]
    )
  ),
  tibble(
    FundingMetric = "Total modeled funding",
    SummaryLEAAmount = sum(
      pefc_summary_state$SummaryLEAAmount[
        pefc_summary_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding",
          "Opportunity Funding",
          "Operational Funding"
        )
      ]
    )
  )
)

pefc_primary_state <- pefc_lea_long |>
  filter(
    IncludeInStatewide %in% TRUE,
    !DistrictCode %in% primary_reporting_excluded_lea_codes
  ) |>
  group_by(FundingMetric) |>
  summarise(
    PEFCPrimaryScopeAmount = sum(PEFCFundingAmount, na.rm = FALSE),
    .groups = "drop"
  )

pefc_primary_state <- bind_rows(
  pefc_primary_state,
  tibble(
    FundingMetric = "Position-based Funding",
    PEFCPrimaryScopeAmount = sum(
      pefc_primary_state$PEFCPrimaryScopeAmount[
        pefc_primary_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding"
        )
      ]
    )
  ),
  tibble(
    FundingMetric = "Total modeled funding",
    PEFCPrimaryScopeAmount = sum(
      pefc_primary_state$PEFCPrimaryScopeAmount[
        pefc_primary_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding",
          "Opportunity Funding",
          "Operational Funding"
        )
      ]
    )
  )
)

independent_as_presented_state <- independent_as_presented_lea_long |>
  group_by(FundingMetric) |>
  summarise(
    IndependentAsPresentedAmount = sum(
      IndependentFundingAmount,
      na.rm = FALSE
    ),
    .groups = "drop"
  )

independent_as_presented_state <- bind_rows(
  independent_as_presented_state,
  tibble(
    FundingMetric = "Position-based Funding",
    IndependentAsPresentedAmount = sum(
      independent_as_presented_state$IndependentAsPresentedAmount[
        independent_as_presented_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding"
        )
      ]
    )
  ),
  tibble(
    FundingMetric = "Total modeled funding",
    IndependentAsPresentedAmount = sum(
      independent_as_presented_state$IndependentAsPresentedAmount[
        independent_as_presented_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding",
          "Opportunity Funding",
          "Operational Funding"
        )
      ]
    )
  )
)

independent_primary_state <- independent_primary_lea_long |>
  group_by(FundingMetric) |>
  summarise(
    IndependentPrimaryScopeAmount = sum(
      IndependentFundingAmount,
      na.rm = FALSE
    ),
    .groups = "drop"
  )

independent_primary_state <- bind_rows(
  independent_primary_state,
  tibble(
    FundingMetric = "Position-based Funding",
    IndependentPrimaryScopeAmount = sum(
      independent_primary_state$IndependentPrimaryScopeAmount[
        independent_primary_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding"
        )
      ]
    )
  ),
  tibble(
    FundingMetric = "Total modeled funding",
    IndependentPrimaryScopeAmount = sum(
      independent_primary_state$IndependentPrimaryScopeAmount[
        independent_primary_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding",
          "Opportunity Funding",
          "Operational Funding"
        )
      ]
    )
  )
)

pefc_calculator_state <- pefc_statewide_as_presented |>
  select(FundingMetric, CalculatorAmount = PEFCWorkbookAmount)

pefc_calculator_state <- bind_rows(
  pefc_calculator_state,
  tibble(
    FundingMetric = "Position-based Funding",
    CalculatorAmount = sum(
      pefc_calculator_state$CalculatorAmount[
        pefc_calculator_state$FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding"
        )
      ]
    )
  )
) |>
  distinct(FundingMetric, .keep_all = TRUE)

internal_reconciliation <- pefc_calculator_state |>
  left_join(pefc_summary_state, by = "FundingMetric") |>
  mutate(
    Scope = "PEFC workbook internal reconciliation",
    SourceA = "PEFC Calculator statewide display",
    SourceB = "Sum of PEFC Summary LEA rows",
    AmountA = CalculatorAmount,
    AmountB = SummaryLEAAmount,
    Difference = AmountB - AmountA,
    PercentDifference = if_else(
      abs(AmountA) > 1e-8,
      100 * Difference / AmountA,
      NA_real_
    ),
    ComparisonType = if_else(
      abs(Difference) <= comparison_tolerance,
      "Statewide-summary reconciliation",
      "Statewide-summary discrepancy"
    )
  )

as_presented_reconciliation <- pefc_calculator_state |>
  left_join(independent_as_presented_state, by = "FundingMetric") |>
  mutate(
    ComparisonType = "Scope and formula comparison",
    Scope = pefc_as_presented_scope_label,
    SourceA = pefc_model_label,
    SourceB = proposed_model_label,
    AmountA = CalculatorAmount,
    AmountB = IndependentAsPresentedAmount,
    Difference = AmountB - AmountA,
    PercentDifference = if_else(
      abs(AmountA) > 1e-8,
      100 * Difference / AmountA,
      NA_real_
    )
  )

primary_scope_reconciliation <- pefc_primary_state |>
  left_join(independent_primary_state, by = "FundingMetric") |>
  mutate(
    ComparisonType = "Primary-scope normalized comparison",
    Scope = primary_reporting_scope_short,
    SourceA = paste(pefc_model_label, "restricted to primary scope"),
    SourceB = proposed_model_label,
    AmountA = PEFCPrimaryScopeAmount,
    AmountB = IndependentPrimaryScopeAmount,
    Difference = AmountB - AmountA,
    PercentDifference = if_else(
      abs(AmountA) > 1e-8,
      100 * Difference / AmountA,
      NA_real_
    )
  )

section_reconciliation <- bind_rows(
  internal_reconciliation,
  as_presented_reconciliation,
  primary_scope_reconciliation
) |>
  select(
    ComparisonType,
    Scope,
    FundingMetric,
    SourceA,
    SourceB,
    AmountA,
    AmountB,
    Difference,
    PercentDifference
  ) |>
  mutate(
    Unit = "Dollars",
    MatchWithinTolerance = abs(Difference) <= comparison_tolerance,
    LEAsWithDifference = case_when(
      ComparisonType %in% c(
        "Statewide-summary reconciliation",
        "Statewide-summary discrepancy"
      ) ~ NA_integer_,
      ComparisonType == "Scope and formula comparison" &
        FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding",
          "Opportunity Funding",
          "Operational Funding"
        ) ~ map_int(
          FundingMetric,
          ~ pefc_lea_as_presented_comparison |>
            filter(FundingMetric == .x, !MatchWithinTolerance) |>
            summarise(N = n_distinct(DistrictCode)) |>
            pull(N)
        ),
      ComparisonType == "Primary-scope normalized comparison" &
        FundingMetric %in% c(
          "Base Funding",
          "Central Office Funding",
          "Opportunity Funding",
          "Operational Funding"
        ) ~ map_int(
          FundingMetric,
          ~ pefc_primary_lea_comparison |>
            filter(FundingMetric == .x, !MatchWithinTolerance) |>
            summarise(N = n_distinct(DistrictCode)) |>
            pull(N)
        ),
      TRUE ~ NA_integer_
    )
  )

component_reconciliation <- pefc_component_comparison |>
  transmute(
    ComparisonType,
    Scope = ReportingScope,
    FundingMetric = paste(FundingSection, Component, sep = " - "),
    SourceA = PEFCModel,
    SourceB = IndependentModel,
    AmountA = PEFCFundingAmount,
    AmountB = IndependentFundingAmount,
    Difference = FundingDifference,
    PercentDifference = FundingPercentDifference,
    Unit = "Dollars",
    MatchWithinTolerance,
    LEAsWithDifference = NA_integer_
  )

pefc_section_scope_count <- nrow(pefc_lea)
pefc_central_scope_count <- sum(pefc_lea$IncludeInStatewide %in% TRUE)
primary_scope_pefc_count <- pefc_central_scope_count
primary_scope_independent_count <- n_distinct(
  independent_primary_lea_long$DistrictCode
)

scope_reconciliation <- tribble(
  ~ComparisonType, ~Scope, ~FundingMetric, ~SourceA, ~SourceB, ~AmountA, ~AmountB, ~Difference, ~PercentDifference, ~Unit, ~MatchWithinTolerance, ~LEAsWithDifference,
  "Scope-treatment discrepancy", pefc_as_presented_scope_label, "DAFB treatment across PEFC funding sections", "PEFC Base, Opportunity, and Operational sections", "PEFC Central Office section", pefc_section_scope_count, pefc_central_scope_count, pefc_central_scope_count - pefc_section_scope_count, 100 * (pefc_central_scope_count - pefc_section_scope_count) / pefc_section_scope_count, "LEAs", FALSE, NA_integer_,
  "Scope alignment", primary_reporting_scope_short, "Organizations in primary comparison", paste(pefc_model_label, "restricted to primary scope"), proposed_model_label, primary_scope_pefc_count, primary_scope_independent_count, primary_scope_independent_count - primary_scope_pefc_count, 0, "LEAs", primary_scope_pefc_count == primary_scope_independent_count, NA_integer_
)

pefc_reconciliation_summary <- bind_rows(
  scope_reconciliation,
  section_reconciliation,
  component_reconciliation
) |>
  mutate(
    DifferenceClassification = case_when(
      ComparisonType == "Scope-treatment discrepancy" ~
        "Scope-treatment discrepancy",
      ComparisonType == "Scope alignment" ~ "Scope alignment",
      ComparisonType == "Statewide-summary reconciliation" ~
        "Matches",
      ComparisonType == "Statewide-summary discrepancy" ~
        "Workbook statewide-summary discrepancy",
      ComparisonType == "Primary-scope normalized comparison" &
        FundingMetric %in% c(
          "Opportunity Funding",
          "Operational Funding"
        ) ~ "Expected scope redistribution",
      ComparisonType == "Component formula/input comparison" &
        MatchWithinTolerance ~ "Component matches",
      ComparisonType == "Component formula/input comparison" ~
        "Component formula/input discrepancy",
      MatchWithinTolerance ~ "Matches",
      FundingMetric %in% c("Base Funding", "Central Office Funding") ~
        "Formula/input discrepancy",
      TRUE ~ "Combined discrepancy"
    ),
    Interpretation = case_when(
      DifferenceClassification == "Expected scope redistribution" ~ paste(
        "The PEFC amount is the workbook allocation after removing DAFB,",
        "while the independent reproduction redistributes the full fixed",
        "statewide pool across the 43-LEA primary scope."
      ),
      ComparisonType == "Statewide-summary reconciliation" ~
        "The PEFC statewide display equals the sum of the PEFC Summary LEA rows.",
      DifferenceClassification == "Workbook statewide-summary discrepancy" ~
        "The PEFC Calculator statewide display does not equal the sum of PEFC Summary LEA rows.",
      DifferenceClassification == "Scope-treatment discrepancy" ~ paste(
        "Confirmed workbook scope discrepancy: the PEFC workbook includes",
        "DAFB in Base, Opportunity, and Operational Funding even though DAFB",
        "does not receive state funding; Central Office already excludes it."
      ),
      DifferenceClassification == "Scope alignment" ~
        "Both sources contain the same 43 LEAs in the maintained primary comparison scope.",
      DifferenceClassification %in% c(
        "Formula/input discrepancy",
        "Component formula/input discrepancy"
      ) ~ paste(
        "Review the detailed component comparison and charter building",
        "treatment output for the underlying formula or input difference."
      ),
      DifferenceClassification == "Combined discrepancy" ~ paste(
        "The difference combines component-level formula/input effects and",
        "any applicable scope effects."
      ),
      TRUE ~ "The compared amounts match within the maintained tolerance."
    ),
    Status = case_when(
      DifferenceClassification == "Expected scope redistribution" ~
        "Expected scope effect",
      DifferenceClassification == "Scope alignment" ~ "Scope aligned",
      DifferenceClassification == "Scope-treatment discrepancy" ~
        "Discrepancy identified",
      MatchWithinTolerance ~ "Matches within tolerance",
      TRUE ~ "Discrepancy identified"
    )
  )


# QC AND OUTPUT -----------------------------------------------------------------

calculator_section_total <- pefc_statewide_as_presented |>
  filter(FundingMetric %in% c(
    "Base Funding",
    "Central Office Funding",
    "Opportunity Funding",
    "Operational Funding"
  )) |>
  summarise(Value = sum(PEFCWorkbookAmount)) |>
  pull(Value)

calculator_total <- pefc_statewide_as_presented |>
  filter(FundingMetric == "Total modeled funding") |>
  pull(PEFCWorkbookAmount)

pefc_qc <- tibble(
  CheckType = "Integrity",
  Check = c(
    "All PEFC Summary LEAs matched the maintained LEA crosswalk",
    "PEFC Summary contains one row per maintained LEA",
    "BASSE appears in the PEFC Summary",
    "DAFB is excluded from aligned independent IV&V totals",
    "DAFB is excluded from the primary PEFC comparison scope",
    "PEFC workbook DAFB treatment matches the documented scope discrepancy",
    "DAFB Central Office Funding is zero in the PEFC Summary",
    "PEFC Calculator total equals its section totals",
    "All PEFC calculator components matched the independent component list",
    "All PEFC as-presented LEA funding rows matched the independent reproduction",
    "All primary-scope PEFC LEA funding rows matched the independent reproduction",
    "Independent as-presented Opportunity Funding equals the fixed pool",
    "Independent as-presented Operational Funding equals the fixed pool",
    "Primary-scope independent Opportunity Funding equals the fixed pool",
    "Primary-scope independent Operational Funding equals the fixed pool",
    "All charter PEFC building rows are represented in the treatment output",
    "Every charter building row has proposed Base funding detail"
  ),
  Expected = c(
    "0 unmatched",
    as.character(nrow(lea_crosswalk)),
    "TRUE",
    "TRUE",
    "TRUE",
    "3 discrepant sections and 1 correctly excluded section",
    "0",
    as.character(calculator_section_total),
    "0 missing",
    "0 missing",
    "0 missing",
    as.character(opportunity_funding_pool),
    as.character(operational_funding_pool),
    as.character(opportunity_funding_pool),
    as.character(operational_funding_pool),
    as.character(nrow(charter_units)),
    "0 missing"
  ),
  Actual = c(
    as.character(nrow(unmatched_pefc_leas)),
    as.character(nrow(pefc_lea)),
    as.character(basse_district_code %in% pefc_lea$DistrictCode),
    as.character(!any(
      dafb_independent_rows$IncludeInStatewide %in% TRUE,
      na.rm = TRUE
    )),
    as.character(!any(
      pefc_primary_lea_comparison$DistrictCode == dafb_district_code
    )),
    paste0(
      sum(
        pefc_dafb_scope_discrepancy$WorkbookScopeStatus ==
          "Confirmed PEFC workbook scope discrepancy"
      ),
      " discrepant sections and ",
      sum(
        pefc_dafb_scope_discrepancy$WorkbookScopeStatus ==
          "PEFC workbook already excludes DAFB"
      ),
      " correctly excluded section"
    ),
    as.character(
      pefc_lea |>
        filter(DistrictCode == dafb_district_code) |>
        pull(PEFCCentralOfficeFundingAmount)
    ),
    as.character(calculator_total),
    as.character(sum(is.na(pefc_component_comparison$IndependentFundingAmount))),
    as.character(sum(is.na(
      pefc_lea_as_presented_comparison$IndependentFundingAmount
    ))),
    as.character(sum(is.na(
      pefc_primary_lea_comparison$IndependentFundingAmount
    ))),
    as.character(
      independent_as_presented_state |>
        filter(FundingMetric == "Opportunity Funding") |>
        pull(IndependentAsPresentedAmount)
    ),
    as.character(
      independent_as_presented_state |>
        filter(FundingMetric == "Operational Funding") |>
        pull(IndependentAsPresentedAmount)
    ),
    as.character(
      independent_primary_state |>
        filter(FundingMetric == "Opportunity Funding") |>
        pull(IndependentPrimaryScopeAmount)
    ),
    as.character(
      independent_primary_state |>
        filter(FundingMetric == "Operational Funding") |>
        pull(IndependentPrimaryScopeAmount)
    ),
    as.character(nrow(charter_building_treatment)),
    as.character(sum(is.na(charter_building_treatment$BaseFundingAmount)))
  ),
  Pass = c(
    nrow(unmatched_pefc_leas) == 0,
    nrow(pefc_lea) == nrow(lea_crosswalk),
    basse_district_code %in% pefc_lea$DistrictCode,
    !any(dafb_independent_rows$IncludeInStatewide %in% TRUE, na.rm = TRUE),
    !any(pefc_primary_lea_comparison$DistrictCode == dafb_district_code),
    sum(
      pefc_dafb_scope_discrepancy$WorkbookScopeStatus ==
        "Confirmed PEFC workbook scope discrepancy"
    ) == 3L &
      sum(
        pefc_dafb_scope_discrepancy$WorkbookScopeStatus ==
          "PEFC workbook already excludes DAFB"
      ) == 1L,
    abs(
      pefc_lea |>
        filter(DistrictCode == dafb_district_code) |>
        pull(PEFCCentralOfficeFundingAmount)
    ) <= comparison_tolerance,
    abs(calculator_total - calculator_section_total) <= comparison_tolerance,
    sum(is.na(pefc_component_comparison$IndependentFundingAmount)) == 0,
    sum(is.na(pefc_lea_as_presented_comparison$IndependentFundingAmount)) == 0,
    sum(is.na(pefc_primary_lea_comparison$IndependentFundingAmount)) == 0,
    abs(
      independent_as_presented_state |>
        filter(FundingMetric == "Opportunity Funding") |>
        pull(IndependentAsPresentedAmount) - opportunity_funding_pool
    ) <= comparison_tolerance,
    abs(
      independent_as_presented_state |>
        filter(FundingMetric == "Operational Funding") |>
        pull(IndependentAsPresentedAmount) - operational_funding_pool
    ) <= comparison_tolerance,
    abs(
      independent_primary_state |>
        filter(FundingMetric == "Opportunity Funding") |>
        pull(IndependentPrimaryScopeAmount) - opportunity_funding_pool
    ) <= comparison_tolerance,
    abs(
      independent_primary_state |>
        filter(FundingMetric == "Operational Funding") |>
        pull(IndependentPrimaryScopeAmount) - operational_funding_pool
    ) <= comparison_tolerance,
    nrow(charter_building_treatment) == nrow(charter_units),
    sum(is.na(charter_building_treatment$BaseFundingAmount)) == 0
  )
)

write_review_csv(pefc_statewide_as_presented, pefc_state_path)
write_review_csv(pefc_component_comparison, pefc_component_path)
write_review_csv(pefc_lea_comparison, pefc_lea_path)
write_review_csv(pefc_reconciliation_summary, pefc_reconciliation_path)
write_review_csv(pefc_dafb_scope_discrepancy, pefc_dafb_scope_path)
write_review_csv(charter_building_treatment, charter_building_path)
write_review_csv(pefc_qc, pefc_qc_path)

if (any(!pefc_qc$Pass)) {
  print(pefc_qc |> filter(!Pass))
  stop("Step 10 PEFC extraction integrity checks failed.", call. = FALSE)
}

message("Step 10 complete: PEFC workbook reconciliation created.")
