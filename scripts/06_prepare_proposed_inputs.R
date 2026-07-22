# =============================================================================
# 06_prepare_proposed_inputs.R
# =============================================================================
# Creates the three transparent inputs used by the proposed model:
#
#   1. School calculation units
#      Districts use school codes. Charters use calculator building rows.
#
#   2. Weighted-funding inputs
#      Official shared school or charter totals are used once.
#
#   3. LEA inputs
#      Official shared LEA totals are used for Central Office formulas.
#
# The shared charter totals remain authoritative. Calculator values determine
# how those totals are distributed across charter buildings.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

shared_input_path <- file.path(output_dir, "02_shared_model_input.csv")
proposed_school_input_path <- file.path(
  output_dir,
  "06_proposed_school_calculation_units.csv"
)
proposed_weighted_input_path <- file.path(
  output_dir,
  "06_proposed_weighted_input.csv"
)
proposed_lea_input_path <- file.path(
  output_dir,
  "06_proposed_lea_input.csv"
)
proposed_charter_buildings_path <- file.path(
  output_dir,
  "06_proposed_charter_buildings.csv"
)
proposed_charter_reconciliation_path <- file.path(
  output_dir,
  "06_proposed_charter_reconciliation.csv"
)
proposed_input_qc_path <- file.path(
  output_dir,
  "06_proposed_input_qc.csv"
)

check_required_files(c(
  shared_input_path,
  calculator_path,
  lea_crosswalk_path,
  entity_crosswalk_path
))


# READ SHARED INPUT AND CROSSWALK ----------------------------------------------

shared_input <- read_csv(shared_input_path, show_col_types = FALSE) |>
  filter(SchoolYear == school_year) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    IsSchool = as.logical(IsSchool)
  )

lea_crosswalk <- read_csv(lea_crosswalk_path, show_col_types = FALSE) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    CalculatorLEAKey = normalize_name(CalculatorLEAName)
  )


entity_crosswalk <- read_csv(
  entity_crosswalk_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode)
  )

check_required_columns(
  entity_crosswalk,
  c(
    "CrosswalkType",
    "DistrictCode",
    "SchoolCode",
    "SharedSchoolName",
    "ExternalSchoolName",
    "Notes"
  ),
  "entity_crosswalk.csv"
)

calculator_entity_crosswalk <- entity_crosswalk |>
  filter(CrosswalkType == "Proposed calculator entity")

stop_if_rows(
  calculator_entity_crosswalk |>
    count(DistrictCode, SchoolCode) |>
    filter(n > 1),
  "entity_crosswalk.csv contains duplicate calculator entity keys."
)

check_required_columns(
  shared_input,
  c(
    "AggregationLevel",
    "SchoolYear",
    "CountDate",
    "DistrictCode",
    "DistrictName",
    "SchoolCode",
    "SchoolName",
    "IsSchool",
    "EnrollmentPreK",
    "EnrollmentK3",
    "Enrollment4_12",
    "EnrollmentBasicK12",
    "EnrollmentIntense",
    "EnrollmentComplex",
    "RegularEdEnrollment",
    "SpecialEdEnrollment",
    "Enrollment",
    "LI",
    "MLL",
    "UnitsVocational",
    "UnitsVocationalDeduct",
    "UnitsTotal"
  ),
  "02_shared_model_input.csv"
)

check_required_columns(
  lea_crosswalk,
  c(
    "DistrictCode",
    "DistrictName",
    "LEAType",
    "CalculatorLEAName",
    "IncludeInStatewide"
  ),
  "lea_crosswalk.csv"
)

stop_if_rows(
  lea_crosswalk |>
    count(DistrictCode) |>
    filter(n > 1),
  "lea_crosswalk.csv contains duplicate district codes."
)

stop_if_rows(
  lea_crosswalk |>
    count(CalculatorLEAKey) |>
    filter(n > 1),
  "lea_crosswalk.csv contains duplicate calculator LEA names."
)

stop_if_rows(
  shared_input |>
    filter(AggregationLevel != "Statewide") |>
    distinct(DistrictCode, DistrictName) |>
    anti_join(lea_crosswalk, by = "DistrictCode"),
  "One or more shared-input LEAs are missing from lea_crosswalk.csv."
)

school_input <- shared_input |>
  filter(AggregationLevel == "School") |>
  left_join(
    lea_crosswalk |>
      select(
        DistrictCode,
        LEAType,
        CalculatorLEAName,
        CalculatorLEAKey,
        IncludeInStatewide
      ),
    by = "DistrictCode"
  )

lea_input <- shared_input |>
  filter(AggregationLevel == "District") |>
  left_join(
    lea_crosswalk |>
      select(
        DistrictCode,
        LEAType,
        CalculatorLEAName,
        CalculatorLEAKey,
        IncludeInStatewide
      ),
    by = "DistrictCode"
  ) |>
  mutate(
    SchoolCode = NA_integer_,
    SchoolName = "LEA Total",
    IsSchool = FALSE
  )


# READ CALCULATOR DATA ----------------------------------------------------------

calculator_data <- read_excel(
  calculator_path,
  sheet = "Data"
) |>
  transmute(
    CalculatorRow = row_number() + 1L,
    CalculatorLEANameFromWorkbook = as.character(`District/Charter`),
    CalculatorEntityName = as.character(`School/District`),
    CalculatorK3 = as.numeric(`Regular -  K-3`),
    Calculator4To12 = as.numeric(`Regular - 4-12`),
    CalculatorBasicPreK12 = as.numeric(`Basic - Pre-K-12`),
    CalculatorIntensive = as.numeric(`Intense - Pre-K-12`),
    CalculatorComplex = as.numeric(`Complex - Pre-K-12`),
    CalculatorEnrollment = as.numeric(Enrollment),
    CalculatorVocationalDeduct = as.numeric(`Vocational Deduct`),
    CalculatorVocationalDivisionI = as.numeric(`Vocational Division I`),
    CalculatorVocationalEnrollment = as.numeric(Vocational)
  ) |>
  filter(!is.na(CalculatorLEANameFromWorkbook)) |>
  mutate(
    CalculatorLEAKey = normalize_name(CalculatorLEANameFromWorkbook),
    CalculatorEntityKey = normalize_name(CalculatorEntityName)
  )

stop_if_rows(
  calculator_data |>
    count(CalculatorLEAKey, CalculatorEntityKey) |>
    filter(n > 1),
  "The calculator Data sheet contains duplicate LEA/entity rows."
)


# IDENTIFY CHARTER BUILDINGS ----------------------------------------------------

charter_crosswalk <- lea_crosswalk |>
  filter(LEAType == "Charter") |>
  select(
    DistrictCode,
    DistrictName,
    LEAType,
    CalculatorLEAName,
    CalculatorLEAKey,
    IncludeInStatewide
  )

charter_school_input <- school_input |>
  filter(LEAType == "Charter")

stop_if_rows(
  charter_school_input |>
    count(DistrictCode) |>
    filter(n != 1),
  "Each charter must have exactly one shared school-level input row."
)

stop_if_rows(
  charter_school_input |>
    filter(!IsSchool),
  "Each charter shared-input row must have a school code."
)

charter_calculator_rows <- calculator_data |>
  inner_join(charter_crosswalk, by = "CalculatorLEAKey") |>
  arrange(DistrictCode, CalculatorRow) |>
  mutate(
    CalculatorRowCount = n(),
    IsOrganizationTotal = CalculatorEntityKey == CalculatorLEAKey,
    .by = DistrictCode
  )

stop_if_rows(
  charter_crosswalk |>
    anti_join(
      charter_calculator_rows |>
        distinct(DistrictCode),
      by = "DistrictCode"
    ),
  "One or more charters do not appear on the calculator Data sheet."
)

stop_if_rows(
  charter_calculator_rows |>
    summarise(
      OrganizationTotalRows = sum(IsOrganizationTotal),
      .by = c(DistrictCode, DistrictName)
    ) |>
    filter(OrganizationTotalRows != 1),
  "Each charter must have exactly one organization-total calculator row."
)

proposed_charter_buildings <- charter_calculator_rows |>
  filter(CalculatorRowCount == 1 | !IsOrganizationTotal) |>
  arrange(DistrictCode, CalculatorRow) |>
  mutate(
    CalculationUnitSequence = row_number(),
    SchoolCalculationUnitCount = n(),
    .by = DistrictCode
  ) |>
  select(
    DistrictCode,
    DistrictName,
    CalculatorLEAName,
    IncludeInStatewide,
    CalculatorRow,
    CalculationUnitSequence,
    SchoolCalculationUnitCount,
    CalculationUnitName = CalculatorEntityName,
    CalculatorK3,
    Calculator4To12,
    CalculatorBasicPreK12,
    CalculatorIntensive,
    CalculatorComplex,
    CalculatorEnrollment,
    CalculatorVocationalDeduct,
    CalculatorVocationalDivisionI
  )


# BUILD LONG CHARTER ALLOCATION TABLE ------------------------------------------

charter_calculator_long <- proposed_charter_buildings |>
  transmute(
    DistrictCode,
    DistrictName,
    CalculatorLEAName,
    IncludeInStatewide,
    CalculatorRow,
    CalculationUnitSequence,
    SchoolCalculationUnitCount,
    CalculationUnitName,
    CalculatorEnrollment,
    EnrollmentK3 = CalculatorK3,
    Enrollment4_12 = Calculator4To12,
    BasicPreK12Enrollment = CalculatorBasicPreK12,
    EnrollmentIntense = CalculatorIntensive,
    EnrollmentComplex = CalculatorComplex,
    UnitsVocationalDeduct = CalculatorVocationalDeduct,
    UnitsVocational = CalculatorVocationalDivisionI
  ) |>
  pivot_longer(
    cols = c(
      EnrollmentK3,
      Enrollment4_12,
      BasicPreK12Enrollment,
      EnrollmentIntense,
      EnrollmentComplex,
      UnitsVocationalDeduct,
      UnitsVocational
    ),
    names_to = "InputCategory",
    values_to = "CalculatorValue"
  )

charter_organization_long <- charter_calculator_rows |>
  filter(IsOrganizationTotal) |>
  transmute(
    DistrictCode,
    EnrollmentK3 = CalculatorK3,
    Enrollment4_12 = Calculator4To12,
    BasicPreK12Enrollment = CalculatorBasicPreK12,
    EnrollmentIntense = CalculatorIntensive,
    EnrollmentComplex = CalculatorComplex,
    UnitsVocationalDeduct = CalculatorVocationalDeduct,
    UnitsVocational = CalculatorVocationalDivisionI
  ) |>
  pivot_longer(
    cols = c(
      EnrollmentK3,
      Enrollment4_12,
      BasicPreK12Enrollment,
      EnrollmentIntense,
      EnrollmentComplex,
      UnitsVocationalDeduct,
      UnitsVocational
    ),
    names_to = "InputCategory",
    values_to = "CalculatorOrganizationTotal"
  )

charter_shared_long <- charter_school_input |>
  transmute(
    DistrictCode,
    ParentSchoolCode = SchoolCode,
    ParentSchoolName = SchoolName,
    SchoolYear,
    CountDate,
    SharedUnitsTotal = UnitsTotal,
    EnrollmentK3,
    Enrollment4_12,
    BasicPreK12Enrollment = EnrollmentPreK + EnrollmentBasicK12,
    EnrollmentIntense,
    EnrollmentComplex,
    UnitsVocationalDeduct,
    UnitsVocational
  ) |>
  pivot_longer(
    cols = c(
      EnrollmentK3,
      Enrollment4_12,
      BasicPreK12Enrollment,
      EnrollmentIntense,
      EnrollmentComplex,
      UnitsVocationalDeduct,
      UnitsVocational
    ),
    names_to = "InputCategory",
    values_to = "SharedLEATotal"
  )

manual_allocations <- if (file.exists(proposed_manual_allocations_path)) {
  read_csv(
    proposed_manual_allocations_path,
    show_col_types = FALSE
  ) |>
    transmute(
      DistrictCode = as.integer(DistrictCode),
      CalculationUnitName = as.character(CalculationUnitName),
      InputCategory = as.character(InputCategory),
      ManualShare = as.numeric(ManualShare)
    )
} else {
  tibble(
    DistrictCode = integer(),
    CalculationUnitName = character(),
    InputCategory = character(),
    ManualShare = double()
  )
}

stop_if_rows(
  manual_allocations |>
    count(DistrictCode, CalculationUnitName, InputCategory) |>
    filter(n > 1),
  "proposed_charter_manual_allocations.csv contains duplicate rows."
)

stop_if_rows(
  manual_allocations |>
    filter(
      !is.na(ManualShare),
      ManualShare < 0 | ManualShare > 1
    ),
  "Manual charter allocation shares must be between 0 and 1."
)

charter_allocation_long <- charter_calculator_long |>
  left_join(
    charter_shared_long,
    by = c("DistrictCode", "InputCategory")
  ) |>
  left_join(
    charter_organization_long,
    by = c("DistrictCode", "InputCategory")
  ) |>
  mutate(
    CalculatorBuildingTotal = sum(CalculatorValue, na.rm = TRUE),
    CalculatorAbsoluteTotal = sum(abs(CalculatorValue), na.rm = TRUE),
    CalculatorEnrollmentTotal = sum(CalculatorEnrollment, na.rm = TRUE),
    .by = c(DistrictCode, InputCategory)
  ) |>
  mutate(
    DefaultShare = case_when(
      abs(SharedLEATotal) < 1e-8 ~ 0,
      CalculatorAbsoluteTotal > 1e-8 ~
        abs(CalculatorValue) / CalculatorAbsoluteTotal,
      CalculatorEnrollmentTotal > 1e-8 ~
        CalculatorEnrollment / CalculatorEnrollmentTotal,
      TRUE ~ NA_real_
    )
  ) |>
  left_join(
    manual_allocations,
    by = c(
      "DistrictCode",
      "CalculationUnitName",
      "InputCategory"
    )
  ) |>
  mutate(
    AllocationShare = coalesce(DefaultShare, ManualShare),

    AllocationMethod = case_when(
      abs(SharedLEATotal) < 1e-8 ~ "Shared total is zero",
      !is.na(DefaultShare) & CalculatorAbsoluteTotal > 1e-8 ~
        "Calculator category share",
      !is.na(DefaultShare) & CalculatorEnrollmentTotal > 1e-8 ~
        "Calculator enrollment share",
      !is.na(ManualShare) ~ "Manual share",
      TRUE ~ "Manual share required"
    ),

    ProportionalAdjustedValue = case_when(
      abs(SharedLEATotal) < 1e-8 ~ 0,
      !is.na(AllocationShare) ~ SharedLEATotal * AllocationShare,
      TRUE ~ NA_real_
    ),

    BuildingVsSharedDifference =
      SharedLEATotal - CalculatorBuildingTotal,

    OrganizationVsSharedDifference =
      SharedLEATotal - CalculatorOrganizationTotal
  )

manual_allocation_needed <- charter_allocation_long |>
  filter(AllocationMethod == "Manual share required") |>
  distinct(
    DistrictCode,
    DistrictName,
    CalculationUnitName,
    InputCategory
  ) |>
  mutate(ManualShare = NA_real_)

if (nrow(manual_allocation_needed) > 0) {
  write_csv(
    manual_allocation_needed,
    proposed_manual_allocations_path,
    na = ""
  )

  print(manual_allocation_needed)

  stop(
    "A calculator share could not be derived. Complete ManualShare in: ",
    proposed_manual_allocations_path,
    call. = FALSE
  )
}

manual_share_check <- charter_allocation_long |>
  filter(AllocationMethod == "Manual share") |>
  summarise(
    ManualShareTotal = sum(AllocationShare, na.rm = TRUE),
    .by = c(DistrictCode, DistrictName, InputCategory)
  ) |>
  filter(abs(ManualShareTotal - 1) > 1e-8)

stop_if_rows(
  manual_share_check,
  "Manual charter shares must total 1 within each charter and category."
)


# APPLY LARGEST REMAINDER TO STUDENT COUNTS ------------------------------------

# Student enrollment categories must remain whole students at each charter
# building. Calculator shares determine the proportional allocation. The floor
# of each proportional value is assigned first, then remaining students go to
# the largest decimal remainders. Exact ties are resolved by building sequence.
# Vocational units and deductions remain continuous and retain full precision.

stop_if_rows(
  charter_allocation_long |>
    filter(
      InputCategory %in% charter_integer_allocation_categories,
      SharedLEATotal < 0 |
        abs(SharedLEATotal - floor(SharedLEATotal)) > 1e-8
    ) |>
    distinct(DistrictCode, DistrictName, InputCategory, SharedLEATotal),
  paste(
    "Charter student-allocation totals must be nonnegative whole students.",
    "Review the shared input."
  )
)

charter_allocation_long <- charter_allocation_long |>
  mutate(
    UsesLargestRemainder =
      InputCategory %in% charter_integer_allocation_categories,
    IntegerAdjustedValue = if_else(
      UsesLargestRemainder,
      floor(ProportionalAdjustedValue),
      NA_real_
    ),
    Remainder = if_else(
      UsesLargestRemainder,
      ProportionalAdjustedValue - IntegerAdjustedValue,
      NA_real_
    ),
    StudentsRemaining = if_else(
      UsesLargestRemainder,
      as.integer(
        floor(first(SharedLEATotal) - sum(IntegerAdjustedValue) + 1e-8)
      ),
      NA_integer_
    ),
    .by = c(DistrictCode, InputCategory)
  ) |>
  arrange(
    DistrictCode,
    InputCategory,
    desc(Remainder),
    CalculationUnitSequence
  ) |>
  mutate(
    RemainderRank = if_else(
      UsesLargestRemainder,
      row_number(),
      NA_integer_
    ),
    StudentsAddedByLargestRemainder = if_else(
      UsesLargestRemainder & RemainderRank <= StudentsRemaining,
      1L,
      0L
    ),
    AdjustedValue = if_else(
      UsesLargestRemainder,
      IntegerAdjustedValue + StudentsAddedByLargestRemainder,
      ProportionalAdjustedValue
    ),
    AllocationRule = if_else(
      UsesLargestRemainder,
      "Largest remainder; ties by CalculationUnitSequence",
      "Full-precision proportional allocation"
    ),
    .by = c(DistrictCode, InputCategory)
  ) |>
  arrange(DistrictCode, CalculationUnitSequence, InputCategory)

stop_if_rows(
  charter_allocation_long |>
    filter(
      UsesLargestRemainder,
      StudentsRemaining < 0 |
        StudentsRemaining > SchoolCalculationUnitCount
    ) |>
    distinct(
      DistrictCode,
      DistrictName,
      InputCategory,
      SharedLEATotal,
      StudentsRemaining,
      SchoolCalculationUnitCount
    ),
  "Largest-remainder allocation produced an invalid remaining-student count."
)

stop_if_rows(
  charter_allocation_long |>
    filter(
      UsesLargestRemainder,
      abs(AdjustedValue - floor(AdjustedValue)) > 1e-8
    ) |>
    select(
      DistrictCode,
      DistrictName,
      CalculationUnitName,
      InputCategory,
      AdjustedValue
    ),
  "A charter student allocation is not a whole number."
)

charter_reconciliation_check <- charter_allocation_long |>
  summarise(
    SharedLEATotal = first(SharedLEATotal),
    AdjustedBuildingTotal = sum(AdjustedValue, na.rm = TRUE),
    .by = c(DistrictCode, DistrictName, InputCategory)
  ) |>
  filter(abs(AdjustedBuildingTotal - SharedLEATotal) > 1e-6)

stop_if_rows(
  charter_reconciliation_check,
  "Adjusted charter building values do not reconcile to shared totals."
)

proposed_charter_reconciliation <- charter_allocation_long |>
  select(
    DistrictCode,
    DistrictName,
    ParentSchoolCode,
    ParentSchoolName,
    CalculatorLEAName,
    CalculatorRow,
    CalculationUnitSequence,
    SchoolCalculationUnitCount,
    CalculationUnitName,
    InputCategory,
    CalculatorValue,
    CalculatorBuildingTotal,
    CalculatorOrganizationTotal,
    SharedLEATotal,
    BuildingVsSharedDifference,
    OrganizationVsSharedDifference,
    AllocationShare,
    AllocationMethod,
    AllocationRule,
    ProportionalAdjustedValue,
    IntegerAdjustedValue,
    Remainder,
    RemainderRank,
    StudentsRemaining,
    StudentsAddedByLargestRemainder,
    FinalAdjustedValue = AdjustedValue
  ) |>
  arrange(DistrictName, CalculationUnitSequence, InputCategory)


# CREATE SCHOOL CALCULATION UNITS ----------------------------------------------

charter_unit_status <- charter_allocation_long |>
  summarise(
    CategoriesAdjusted = n_distinct(
      InputCategory[abs(BuildingVsSharedDifference) > 1e-8]
    ),
    UsedEnrollmentFallback =
      any(AllocationMethod == "Calculator enrollment share"),
    UsedManualShare = any(AllocationMethod == "Manual share"),
    UsedLargestRemainder = any(
      UsesLargestRemainder & abs(BuildingVsSharedDifference) > 1e-8
    ),
    .by = c(DistrictCode, CalculationUnitName)
  ) |>
  mutate(
    AllocationStatus = case_when(
      UsedManualShare ~ "Manual building shares applied",
      UsedEnrollmentFallback ~ "Calculator enrollment shares applied",
      UsedLargestRemainder ~
        "Largest-remainder student allocation applied",
      CategoriesAdjusted > 0 ~
        "Calculator category shares adjusted to shared totals",
      TRUE ~ "Calculator category values reconcile to shared totals"
    )
  )

charter_school_units <- charter_allocation_long |>
  select(
    DistrictCode,
    DistrictName,
    IncludeInStatewide,
    ParentSchoolCode,
    ParentSchoolName,
    SchoolYear,
    CountDate,
    SharedUnitsTotal,
    CalculatorRow,
    CalculationUnitSequence,
    SchoolCalculationUnitCount,
    CalculationUnitName,
    InputCategory,
    AdjustedValue
  ) |>
  pivot_wider(
    names_from = InputCategory,
    values_from = AdjustedValue
  ) |>
  left_join(
    charter_unit_status |>
      select(DistrictCode, CalculationUnitName, AllocationStatus),
    by = c("DistrictCode", "CalculationUnitName")
  ) |>
  transmute(
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType = "Charter",
    IncludeInStatewide,
    SchoolCode = ParentSchoolCode,
    SchoolName = ParentSchoolName,
    IsSchool = TRUE,
    IsSchoolCalculationUnit = TRUE,
    CalculationUnitType = "Charter building",
    CalculationUnitName,
    CalculationUnitSequence,
    SchoolCalculationUnitCount,
    CalculationUnitSource = "Calculator Data sheet building row",
    AllocationStatus,
    EnrollmentK3,
    Enrollment4_12,
    BasicPreK12Enrollment,
    EnrollmentIntense,
    EnrollmentComplex,
    RegularEdEnrollment = EnrollmentK3 + Enrollment4_12,
    SpecialEdEnrollment =
      BasicPreK12Enrollment + EnrollmentIntense + EnrollmentComplex,
    Enrollment =
      EnrollmentK3 +
      Enrollment4_12 +
      BasicPreK12Enrollment +
      EnrollmentIntense +
      EnrollmentComplex,
    UnitsVocationalDeduct,
    UnitsVocational,
    SharedUnitsTotal,
    OperationalEnrollmentBasis = operational_enrollment_basis,
    CharterBuildingPolicy = charter_building_policy
  )

noncharter_school_units <- school_input |>
  filter(LEAType != "Charter") |>
  transmute(
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    IsSchool,
    IsSchoolCalculationUnit = IsSchool,
    CalculationUnitType = case_when(
      IsSchool ~ "School-code record",
      TRUE ~ "Vocational/program adjustment"
    ),
    CalculationUnitName = SchoolName,
    CalculationUnitSequence = 1L,
    SchoolCalculationUnitCount = if_else(IsSchool, 1L, 0L),
    CalculationUnitSource = "Shared model input",
    AllocationStatus = "Shared input used directly",
    EnrollmentK3,
    Enrollment4_12,
    BasicPreK12Enrollment = EnrollmentPreK + EnrollmentBasicK12,
    EnrollmentIntense,
    EnrollmentComplex,
    RegularEdEnrollment,
    SpecialEdEnrollment,
    Enrollment,
    UnitsVocationalDeduct,
    UnitsVocational,
    SharedUnitsTotal = UnitsTotal,
    OperationalEnrollmentBasis = operational_enrollment_basis,
    CharterBuildingPolicy = charter_building_policy
  )

proposed_school_input <- bind_rows(
  noncharter_school_units,
  charter_school_units
) |>
  arrange(
    DistrictName,
    SchoolName,
    CalculationUnitSequence,
    CalculationUnitName
  )

lea_calculation_unit_counts <- proposed_school_input |>
  summarise(
    LEASchoolCalculationUnitCount = sum(IsSchoolCalculationUnit),
    .by = DistrictCode
  )


# CREATE WEIGHTED-FUNDING INPUT -------------------------------------------------

calculator_vocational <- calculator_data |>
  transmute(
    CalculatorLEAKey,
    CalculatorEntityKey,
    VocationalEnrollment = CalculatorVocationalEnrollment
  )

proposed_weighted_input <- school_input |>
  left_join(
    calculator_entity_crosswalk |>
      select(
        DistrictCode,
        SchoolCode,
        SharedSchoolName,
        ExternalSchoolName
      ),
    by = c("DistrictCode", "SchoolCode")
  ) |>
  mutate(
    CalculatorEntityName = case_when(
      LEAType == "Charter" ~ CalculatorLEAName,
      !is.na(ExternalSchoolName) ~ ExternalSchoolName,
      TRUE ~ SchoolName
    ),
    CalculatorEntityKey = normalize_name(CalculatorEntityName)
  ) |>
  left_join(
    calculator_vocational,
    by = c("CalculatorLEAKey", "CalculatorEntityKey")
  ) |>
  left_join(
    lea_calculation_unit_counts,
    by = "DistrictCode"
  ) |>
  mutate(
    LEASchoolCalculationUnitCount =
      coalesce(LEASchoolCalculationUnitCount, 0L),
    SchoolCalculationUnitCount = case_when(
      !IsSchool ~ 0L,
      LEAType == "Charter" ~ LEASchoolCalculationUnitCount,
      TRUE ~ 1L
    ),

    VocationalEnrollment = case_when(
      !is.na(VocationalEnrollment) ~ VocationalEnrollment,
      !IsSchool & Enrollment == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    VocationalEnrollmentStatus = case_when(
      !is.na(VocationalEnrollment) & LEAType == "Charter" ~
        "Calculator charter organization-total row",
      !is.na(VocationalEnrollment) & IsSchool ~
        "Calculator school row",
      !IsSchool & Enrollment == 0 ~
        "Zero for a zero-enrollment record without a school code",
      TRUE ~ "Missing"
    ),

    OperationalEnrollmentCount = if (
      operational_enrollment_basis == "total"
    ) {
      Enrollment
    } else {
      RegularEdEnrollment
    },

    OperationalEnrollmentBasis = operational_enrollment_basis,
    CharterBuildingPolicy = charter_building_policy
  ) |>
  select(
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    SharedSchoolName,
    ExternalSchoolName,
    IsSchool,
    SchoolCalculationUnitCount,
    LEASchoolCalculationUnitCount,
    EnrollmentPreK,
    EnrollmentK3,
    Enrollment4_12,
    EnrollmentBasicK12,
    EnrollmentIntense,
    EnrollmentComplex,
    RegularEdEnrollment,
    SpecialEdEnrollment,
    Enrollment,
    LI,
    MLL,
    VocationalEnrollment,
    VocationalEnrollmentStatus,
    OperationalEnrollmentCount,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy
  ) |>
  arrange(DistrictName, SchoolName)


stop_if_rows(
  calculator_entity_crosswalk |>
    anti_join(
      school_input |>
        distinct(DistrictCode, SchoolCode),
      by = c("DistrictCode", "SchoolCode")
    ),
  "entity_crosswalk.csv contains a stale proposed-calculator school key."
)

stop_if_rows(
  proposed_weighted_input |>
    filter(
      !is.na(SharedSchoolName),
      normalize_name(SchoolName) != normalize_name(SharedSchoolName)
    ) |>
    select(
      DistrictCode,
      SchoolCode,
      SchoolName,
      SharedSchoolName,
      ExternalSchoolName
    ),
  paste0(
    "A shared school name no longer matches entity_crosswalk.csv. ",
    "Review the source name and crosswalk."
  )
)

proposed_weighted_input <- proposed_weighted_input |>
  select(-SharedSchoolName, -ExternalSchoolName)

stop_if_rows(
  proposed_weighted_input |>
    filter(IsSchool, is.na(VocationalEnrollment)) |>
    select(DistrictCode, DistrictName, SchoolCode, SchoolName),
  "A school-code row did not match a calculator vocational enrollment value."
)

provided_vocational_total <- calculator_data |>
  filter(
    CalculatorLEAKey == "statewide",
    CalculatorEntityKey == "statewide"
  ) |>
  pull(CalculatorVocationalEnrollment)

modeled_vocational_total <- sum(
  proposed_weighted_input$VocationalEnrollment,
  na.rm = TRUE
)

if (
  length(provided_vocational_total) == 1 &&
    abs(modeled_vocational_total - provided_vocational_total) > 1e-8
) {
  stop(
    "School vocational enrollment totals ",
    modeled_vocational_total,
    ", but the calculator statewide total is ",
    provided_vocational_total,
    ".",
    call. = FALSE
  )
}


# CREATE LEA INPUT --------------------------------------------------------------

proposed_lea_input <- lea_input |>
  left_join(
    lea_calculation_unit_counts,
    by = "DistrictCode"
  ) |>
  mutate(
    LEASchoolCalculationUnitCount =
      coalesce(LEASchoolCalculationUnitCount, 0L),
    SchoolCalculationUnitCount = LEASchoolCalculationUnitCount,
    OperationalEnrollmentBasis = operational_enrollment_basis,
    CharterBuildingPolicy = charter_building_policy
  ) |>
  arrange(DistrictName)


# INPUT QC ----------------------------------------------------------------------

adjusted_charter_categories <- charter_allocation_long |>
  filter(abs(BuildingVsSharedDifference) > 1e-8) |>
  distinct(DistrictCode, InputCategory) |>
  nrow()

largest_remainder_charter_categories <- charter_allocation_long |>
  filter(UsesLargestRemainder) |>
  distinct(DistrictCode, InputCategory) |>
  nrow()

adjusted_largest_remainder_categories <- charter_allocation_long |>
  filter(
    UsesLargestRemainder,
    abs(BuildingVsSharedDifference) > 1e-8
  ) |>
  distinct(DistrictCode, InputCategory) |>
  nrow()

fallback_charter_categories <- charter_allocation_long |>
  filter(AllocationMethod == "Calculator enrollment share") |>
  distinct(DistrictCode, InputCategory) |>
  nrow()

manual_charter_categories <- charter_allocation_long |>
  filter(AllocationMethod == "Manual share") |>
  distinct(DistrictCode, InputCategory) |>
  nrow()

proposed_input_qc <- tibble(
  Check = c(
    "Charter LEAs",
    "Charter calculator buildings",
    "District and DAFB school-code calculation units",
    "Total proposed school calculation units excluding DAFB",
    "Total proposed school calculation units including DAFB",
    "Charter categories adjusted to shared totals",
    "Charter student categories using largest remainder",
    "Adjusted charter student categories resolved by largest remainder",
    "Charter categories using enrollment fallback",
    "Charter categories using manual shares",
    "Raw operational enrollment excluding DAFB",
    "Raw operational enrollment including DAFB",
    "Operational enrollment basis"
  ),
  Value = c(
    n_distinct(charter_school_input$DistrictCode),
    nrow(proposed_charter_buildings),
    proposed_school_input |>
      filter(LEAType != "Charter", IsSchoolCalculationUnit) |>
      nrow(),
    proposed_school_input |>
      filter(IncludeInStatewide, IsSchoolCalculationUnit) |>
      nrow(),
    proposed_school_input |>
      filter(IsSchoolCalculationUnit) |>
      nrow(),
    adjusted_charter_categories,
    largest_remainder_charter_categories,
    adjusted_largest_remainder_categories,
    fallback_charter_categories,
    manual_charter_categories,
    proposed_weighted_input |>
      filter(IncludeInStatewide) |>
      summarise(Value = sum(OperationalEnrollmentCount, na.rm = TRUE)) |>
      pull(Value),
    proposed_weighted_input |>
      summarise(Value = sum(OperationalEnrollmentCount, na.rm = TRUE)) |>
      pull(Value),
    operational_enrollment_basis
  ),
  Status = c(
    rep("Info", 5),
    if_else(adjusted_charter_categories > 0, "Review", "Pass"),
    "Info",
    if_else(
      adjusted_largest_remainder_categories > 0,
      "Review",
      "Pass"
    ),
    if_else(fallback_charter_categories > 0, "Review", "Pass"),
    if_else(manual_charter_categories > 0, "Review", "Pass"),
    "Info",
    "Info",
    "Info"
  )
)


# EXPORT -----------------------------------------------------------------------

write_model_csv(
  proposed_charter_buildings,
  proposed_charter_buildings_path
)
write_model_csv(
  proposed_charter_reconciliation,
  proposed_charter_reconciliation_path
)
write_model_csv(proposed_school_input, proposed_school_input_path)
write_model_csv(proposed_weighted_input, proposed_weighted_input_path)
write_model_csv(proposed_lea_input, proposed_lea_input_path)
write_review_csv(proposed_input_qc, proposed_input_qc_path)

message("Created proposed school inputs: ", proposed_school_input_path)
message("Created proposed weighted inputs: ", proposed_weighted_input_path)
message("Created proposed LEA inputs: ", proposed_lea_input_path)
message("Review charter reconciliation: ", proposed_charter_reconciliation_path)
message("Raw operational enrollment is recorded in: ", proposed_input_qc_path)
