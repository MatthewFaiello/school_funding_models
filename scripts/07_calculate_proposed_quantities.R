# =============================================================================
# 07_calculate_proposed_quantities.R
# =============================================================================
# Applies proposed Base, Opportunity, Operational, and Central Office formulas
# to the three prepared inputs from Script 06.
#
# RawInputValue and AppliedFactor remain visible so weighted counts and simple
# ratio formulas can be audited without reverse engineering the code.
#
# Interpretation rule: when the proposed model does not explicitly change a
# calculation detail, the corresponding current-model treatment is retained.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

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
proposed_charter_reconciliation_path <- file.path(
  output_dir,
  "06_proposed_charter_reconciliation.csv"
)
proposed_quantities_path <- file.path(
  output_dir,
  "07_proposed_model_quantities.csv"
)
proposed_quantity_summary_path <- file.path(
  output_dir,
  "07_proposed_model_quantity_summary.csv"
)
proposed_quantity_issues_path <- file.path(
  output_dir,
  "07_proposed_model_issues.csv"
)
proposed_rules_path <- file.path(
  output_dir,
  "07_proposed_model_rules.csv"
)

check_required_files(c(
  proposed_school_input_path,
  proposed_weighted_input_path,
  proposed_lea_input_path,
  proposed_charter_reconciliation_path
))


# FORMULA HELPERS ---------------------------------------------------------------

assistant_principal_positions <- function(positions) {
  case_when(
    positions < 25 ~ 0,
    positions < 30 ~ 0.65,
    positions < 50 ~ 1,
    positions < 55 ~ 1.65,
    TRUE ~ 2 + floor((positions - 55) / 20)
  )
}

administrative_support_positions <- function(positions) {
  case_when(
    positions < 100 ~ floor(positions / 10),
    TRUE ~ 10 + floor((positions - 100) / 12)
  )
}

director_positions <- function(positions) {
  case_when(
    positions < 200 ~ 0,
    TRUE ~ pmin(6, 1 + floor((positions - 200) / 100))
  )
}


# READ PREPARED INPUTS ----------------------------------------------------------

school_input <- read_csv(
  proposed_school_input_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    IsSchool = as.logical(IsSchool),
    IsSchoolCalculationUnit = as.logical(IsSchoolCalculationUnit),
    IncludeInStatewide = as.logical(IncludeInStatewide)
  )

weighted_input <- read_csv(
  proposed_weighted_input_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    IsSchool = as.logical(IsSchool),
    IncludeInStatewide = as.logical(IncludeInStatewide)
  )

lea_input <- read_csv(
  proposed_lea_input_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide)
  )


# PROPOSED-MODEL RULES ----------------------------------------------------------

proposed_model_rules <- tribble(
  ~FundingSection, ~Component, ~RateComponent, ~QuantityType, ~SourceField, ~Rule,

  "Base Funding (State Support)",
  "Division I Teacher - K-3 Regular Education",
  "Division I Teacher - K-3 Regular Education",
  "Position",
  "EnrollmentK3",
  "Regular K-3 enrollment divided by 16.2.",

  "Base Funding (State Support)",
  "Division I Teacher - 4-12 Regular Education",
  "Division I Teacher - 4-12 Regular Education",
  "Position",
  "Enrollment4_12",
  "Regular 4-12 enrollment divided by 20.",

  "Base Funding (State Support)",
  "Division I Teacher - Pre-K-12 Basic Special Education",
  "Division I Teacher - Basic Special Education",
  "Position",
  "BasicPreK12Enrollment",
  "Basic Pre-K special education plus Basic K-12 enrollment divided by 8.4.",

  "Base Funding (State Support)",
  "Division I Teacher - Intensive Special Education",
  "Division I Teacher - Intensive Special Education",
  "Position",
  "EnrollmentIntense",
  "Intensive enrollment divided by 6.",

  "Base Funding (State Support)",
  "Division I Teacher - Complex Special Education",
  "Division I Teacher - Complex Special Education",
  "Position",
  "EnrollmentComplex",
  "Complex enrollment divided by 2.6.",

  "Base Funding (State Support)",
  "Vocational Deduct",
  "Vocational Division I / Vocational Deduct",
  "Position",
  "UnitsVocationalDeduct",
  "Use the shared vocational deduction, allocated across charter buildings when needed.",

  "Base Funding (State Support)",
  "Vocational Division I",
  "Vocational Division I / Vocational Deduct",
  "Position",
  "UnitsVocational",
  "Use the shared vocational positions, allocated across charter buildings when needed.",

  "Base Funding (State Support)",
  "Principal",
  "Principal",
  "Position",
  "School calculation unit",
  "One per district school code or charter calculator building.",

  "Base Funding (State Support)",
  "Instructional Supports",
  "Instructional Supports",
  "Position",
  "BaseDivisionIPositions",
  "Twenty percent of all Base Division I positions, including vocational/program adjustment rows.",

  "Base Funding (State Support)",
  "Administrative Support Professionals",
  "Administrative Support Professionals",
  "Position",
  "AdministrativeSupportBase",
  "One per 10 positions through 100; one per completed 12 thereafter.",

  "Base Funding (State Support)",
  "Assistant Principal",
  "Assistant Principal",
  "Position",
  "AssistantPrincipalBase",
  "Apply the 25, 30, 50, 55, and additional 20-position thresholds.",

  "Opportunity Funding (State Support)",
  "Opportunity Funding - Low Income",
  "Opportunity Funding - Low Income",
  "Weighted student",
  "LI",
  "Low-income enrollment multiplied by 0.675.",

  "Opportunity Funding (State Support)",
  "Opportunity Funding - Multilingual Learner",
  "Opportunity Funding - Multilingual Learner",
  "Weighted student",
  "MLL",
  "Multilingual learner enrollment multiplied by 0.47.",

  "Operational Funding (State Support)",
  "Operational Funding - Enrollment",
  "Operational Funding - Enrollment",
  "Weighted student",
  "OperationalEnrollmentCount",
  "The selected operational enrollment count multiplied by 0.20.",

  "Operational Funding (State Support)",
  "Operational Funding - Low Income",
  "Operational Funding - Low Income",
  "Weighted student",
  "LI",
  "Low-income enrollment multiplied by 0.675.",

  "Operational Funding (State Support)",
  "Operational Funding - Multilingual Learner",
  "Operational Funding - Multilingual Learner",
  "Weighted student",
  "MLL",
  "Multilingual learner enrollment multiplied by 0.47.",

  "Operational Funding (State Support)",
  "Operational Funding - Basic Special Education",
  "Operational Funding - Basic Special Education",
  "Weighted student",
  "EnrollmentPreK + EnrollmentBasicK12",
  "Basic Pre-K special education plus Basic K-12 enrollment multiplied by 0.48.",

  "Operational Funding (State Support)",
  "Operational Funding - Intensive Special Education",
  "Operational Funding - Intensive Special Education",
  "Weighted student",
  "EnrollmentIntense",
  "Intensive enrollment multiplied by 0.67.",

  "Operational Funding (State Support)",
  "Operational Funding - Complex Special Education",
  "Operational Funding - Complex Special Education",
  "Weighted student",
  "EnrollmentComplex",
  "Complex enrollment multiplied by 1.54.",

  "Operational Funding (State Support)",
  "Operational Funding - Vocational",
  "Operational Funding - Vocational",
  "Weighted student",
  "VocationalEnrollment",
  "Calculator vocational enrollment multiplied by 0.20.",

  "Central Office Funding (State Support)",
  "Superintendent",
  "Superintendent",
  "Position",
  "LEA",
  "One position per LEA.",

  "Central Office Funding (State Support)",
  "Administrative Assistant",
  "Administrative Assistant",
  "Position",
  "LEA",
  "One position per LEA.",

  "Central Office Funding (State Support)",
  "Assistant Superintendent",
  "Assistant Superintendent",
  "Position",
  "TotalBasePositions",
  paste(
    "One per completed 300 Total Base positions, capped at two.",
    "The proposed materials do not explicitly authorize fractional positions,",
    "so the current-model completed-threshold rule is retained."
  ),

  "Central Office Funding (State Support)",
  "Director",
  "Director",
  "Position",
  "TotalBasePositions",
  "One at 200 Total Base positions plus one per completed additional 100, capped at six.",

  "Central Office Funding (State Support)",
  "11-Month Supervisor",
  "11-Month Supervisor",
  "Position",
  "TotalBasePositions",
  "Total Base positions divided by 150; fractional positions are retained.",

  "Central Office Funding (State Support)",
  "Buildings and Grounds Supervisor",
  "Buildings and Grounds Supervisor",
  "Position",
  "LEA",
  "One position per LEA.",

  "Central Office Funding (State Support)",
  "Food Services Supervisor",
  "Food Services Supervisor",
  "Position",
  "BaseDivisionIPositions",
  "One per LEA plus one per completed 500 Base Division I positions beyond the first 500.",

  "Central Office Funding (State Support)",
  "Transportation Supervisor",
  "Transportation Supervisor",
  "Position",
  "Enrollment",
  "Enrollment divided by 7,500.",

  "Central Office Funding (State Support)",
  "Reading Cadre",
  "Reading Cadre",
  "Position",
  "LEA",
  "One position per LEA."
)


# BASE FUNDING CALCULATIONS -----------------------------------------------------

school_calculations <- school_input |>
  mutate(
    K3TeacherPositions = EnrollmentK3 / 16.2,
    Grade4To12TeacherPositions = Enrollment4_12 / 20,
    BasicTeacherPositions = BasicPreK12Enrollment / 8.4,
    IntensiveTeacherPositions = EnrollmentIntense / 6,
    ComplexTeacherPositions = EnrollmentComplex / 2.6,

    BaseDivisionIPositions =
      K3TeacherPositions +
      Grade4To12TeacherPositions +
      BasicTeacherPositions +
      IntensiveTeacherPositions +
      ComplexTeacherPositions +
      UnitsVocationalDeduct +
      UnitsVocational,

    PrincipalPositions = case_when(
      IsSchoolCalculationUnit ~ 1,
      TRUE ~ 0
    ),

    InstructionalSupportPositions = BaseDivisionIPositions * 0.20,

    AdministrativeSupportBase =
      BaseDivisionIPositions +
      PrincipalPositions +
      InstructionalSupportPositions,

    AdministrativeSupportPositions = case_when(
      IsSchoolCalculationUnit ~
        administrative_support_positions(AdministrativeSupportBase),
      TRUE ~ 0
    ),

    AssistantPrincipalBase =
      BaseDivisionIPositions +
      AdministrativeSupportPositions +
      InstructionalSupportPositions,

    AssistantPrincipalPositions = case_when(
      IsSchoolCalculationUnit ~
        assistant_principal_positions(AssistantPrincipalBase),
      TRUE ~ 0
    )
  )

base_division_i_check <- school_calculations |>
  summarise(
    CalculatedBaseDivisionIPositions =
      sum(BaseDivisionIPositions, na.rm = TRUE),
    SharedUnitsTotal = first(SharedUnitsTotal),
    .by = c(DistrictCode, DistrictName, SchoolCode, SchoolName)
  ) |>
  filter(
    abs(CalculatedBaseDivisionIPositions - SharedUnitsTotal) > 0.05 + 1e-8
  )

stop_if_rows(
  base_division_i_check,
  "Proposed Base Division I positions do not reconcile to shared UnitsTotal."
)

base_components <- bind_rows(
  school_calculations |>
    mutate(
      Component = "Division I Teacher - K-3 Regular Education",
      RawInputValue = EnrollmentK3,
      AppliedFactor = 1 / 16.2,
      FundingQuantity = K3TeacherPositions,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "Calculated from adjusted charter building input",
        TRUE ~ "Calculated from shared school-code input"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Division I Teacher - 4-12 Regular Education",
      RawInputValue = Enrollment4_12,
      AppliedFactor = 1 / 20,
      FundingQuantity = Grade4To12TeacherPositions,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "Calculated from adjusted charter building input",
        TRUE ~ "Calculated from shared school-code input"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Division I Teacher - Pre-K-12 Basic Special Education",
      RawInputValue = BasicPreK12Enrollment,
      AppliedFactor = 1 / 8.4,
      FundingQuantity = BasicTeacherPositions,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "Calculated from adjusted charter building input",
        TRUE ~ "Calculated from shared school-code input"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Division I Teacher - Intensive Special Education",
      RawInputValue = EnrollmentIntense,
      AppliedFactor = 1 / 6,
      FundingQuantity = IntensiveTeacherPositions,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "Calculated from adjusted charter building input",
        TRUE ~ "Calculated from shared school-code input"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Division I Teacher - Complex Special Education",
      RawInputValue = EnrollmentComplex,
      AppliedFactor = 1 / 2.6,
      FundingQuantity = ComplexTeacherPositions,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "Calculated from adjusted charter building input",
        TRUE ~ "Calculated from shared school-code input"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Vocational Deduct",
      RawInputValue = UnitsVocationalDeduct,
      AppliedFactor = 1,
      FundingQuantity = UnitsVocationalDeduct,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "Shared charter total allocated across buildings",
        TRUE ~ "Provided by shared unit-count input"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Vocational Division I",
      RawInputValue = UnitsVocational,
      AppliedFactor = 1,
      FundingQuantity = UnitsVocational,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "Shared charter total allocated across buildings",
        TRUE ~ "Provided by shared unit-count input"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Principal",
      RawInputValue = as.numeric(IsSchoolCalculationUnit),
      AppliedFactor = 1,
      FundingQuantity = PrincipalPositions,
      CalculationStatus = case_when(
        CalculationUnitType == "Charter building" ~
          "One principal per calculator charter building",
        IsSchoolCalculationUnit ~ "One principal per school code",
        TRUE ~ "Not a school calculation unit"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Instructional Supports",
      RawInputValue = BaseDivisionIPositions,
      AppliedFactor = 0.20,
      FundingQuantity = InstructionalSupportPositions,
      CalculationStatus = case_when(
        IsSchoolCalculationUnit ~
          "Calculated from school calculation-unit Base Division I positions",
        abs(BaseDivisionIPositions) > 1e-8 ~
          paste(
            "Calculated from non-school vocational/program",
            "Base Division I positions"
          ),
        TRUE ~
          "Calculated; Base Division I positions equal zero"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Administrative Support Professionals",
      RawInputValue = AdministrativeSupportBase,
      AppliedFactor = NA_real_,
      FundingQuantity = AdministrativeSupportPositions,
      CalculationStatus = case_when(
        IsSchoolCalculationUnit ~ "Calculated using threshold formula",
        TRUE ~ "Not a school calculation unit"
      )
    ),
  school_calculations |>
    mutate(
      Component = "Assistant Principal",
      RawInputValue = AssistantPrincipalBase,
      AppliedFactor = NA_real_,
      FundingQuantity = AssistantPrincipalPositions,
      CalculationStatus = case_when(
        IsSchoolCalculationUnit ~ "Calculated using threshold formula",
        TRUE ~ "Not a school calculation unit"
      )
    )
) |>
  transmute(
    CalculationLevel = "School",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    IsSchool,
    IsSchoolCalculationUnit,
    CalculationUnitType,
    CalculationUnitName,
    CalculationUnitSequence,
    SchoolCalculationUnitCount,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy,
    Component,
    RawInputValue,
    AppliedFactor,
    FundingQuantity,
    CalculationStatus,
    CalculationComplete = !is.na(FundingQuantity)
  )


# OPPORTUNITY AND OPERATIONAL CALCULATIONS -------------------------------------

weighted_components <- bind_rows(
  weighted_input |>
    mutate(
      Component = "Opportunity Funding - Low Income",
      RawInputValue = LI,
      AppliedFactor = 0.675,
      FundingQuantity = LI * AppliedFactor,
      CalculationStatus = "Calculated from official shared school/charter total"
    ),
  weighted_input |>
    mutate(
      Component = "Opportunity Funding - Multilingual Learner",
      RawInputValue = MLL,
      AppliedFactor = 0.47,
      FundingQuantity = MLL * AppliedFactor,
      CalculationStatus = "Calculated from official shared school/charter total"
    ),
  weighted_input |>
    mutate(
      Component = "Operational Funding - Enrollment",
      RawInputValue = OperationalEnrollmentCount,
      AppliedFactor = 0.20,
      FundingQuantity = OperationalEnrollmentCount * AppliedFactor,
      CalculationStatus = paste0(
        "Calculated using ",
        OperationalEnrollmentBasis,
        " enrollment"
      )
    ),
  weighted_input |>
    mutate(
      Component = "Operational Funding - Low Income",
      RawInputValue = LI,
      AppliedFactor = 0.675,
      FundingQuantity = LI * AppliedFactor,
      CalculationStatus = "Calculated from official shared school/charter total"
    ),
  weighted_input |>
    mutate(
      Component = "Operational Funding - Multilingual Learner",
      RawInputValue = MLL,
      AppliedFactor = 0.47,
      FundingQuantity = MLL * AppliedFactor,
      CalculationStatus = "Calculated from official shared school/charter total"
    ),
  weighted_input |>
    mutate(
      Component = "Operational Funding - Basic Special Education",
      RawInputValue = EnrollmentPreK + EnrollmentBasicK12,
      AppliedFactor = 0.48,
      FundingQuantity = RawInputValue * AppliedFactor,
      CalculationStatus = "Calculated from official shared school/charter total"
    ),
  weighted_input |>
    mutate(
      Component = "Operational Funding - Intensive Special Education",
      RawInputValue = EnrollmentIntense,
      AppliedFactor = 0.67,
      FundingQuantity = RawInputValue * AppliedFactor,
      CalculationStatus = "Calculated from official shared school/charter total"
    ),
  weighted_input |>
    mutate(
      Component = "Operational Funding - Complex Special Education",
      RawInputValue = EnrollmentComplex,
      AppliedFactor = 1.54,
      FundingQuantity = RawInputValue * AppliedFactor,
      CalculationStatus = "Calculated from official shared school/charter total"
    ),
  weighted_input |>
    mutate(
      Component = "Operational Funding - Vocational",
      RawInputValue = VocationalEnrollment,
      AppliedFactor = 0.20,
      FundingQuantity = VocationalEnrollment * AppliedFactor,
      CalculationStatus = VocationalEnrollmentStatus
    )
) |>
  transmute(
    CalculationLevel = "School",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    IsSchool,
    IsSchoolCalculationUnit = FALSE,
    CalculationUnitType = case_when(
      LEAType == "Charter" ~ "Charter organization total",
      TRUE ~ "Official school/program record"
    ),
    CalculationUnitName = SchoolName,
    CalculationUnitSequence = NA_integer_,
    SchoolCalculationUnitCount,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy,
    Component,
    RawInputValue,
    AppliedFactor,
    FundingQuantity,
    CalculationStatus,
    CalculationComplete = !is.na(FundingQuantity)
  )


# CENTRAL OFFICE CALCULATIONS ---------------------------------------------------
#
# The proposed workbook distinguishes between:
#   1. Base Division I positions: enrollment-driven teachers plus net vocational
#      positions; and
#   2. Total Base positions: Base Division I plus Principal, Assistant Principal,
#      Administrative Support Professionals, and Instructional Supports.
#
# Assistant Superintendent, Director, and 11-Month Supervisor use Total Base
# positions. Food Services Supervisor uses the narrower Base Division I subtotal.
# When the proposed materials do not explicitly change threshold completion or
# fractional-position treatment, the corresponding current-model rule is retained.

lea_base_totals <- school_calculations |>
  summarise(
    BaseDivisionIPositions = sum(BaseDivisionIPositions, na.rm = TRUE),
    PrincipalPositions = sum(PrincipalPositions, na.rm = TRUE),
    AssistantPrincipalPositions = sum(AssistantPrincipalPositions, na.rm = TRUE),
    AdministrativeSupportPositions =
      sum(AdministrativeSupportPositions, na.rm = TRUE),
    InstructionalSupportPositions =
      sum(InstructionalSupportPositions, na.rm = TRUE),
    .by = c(DistrictCode, DistrictName)
  ) |>
  mutate(
    TotalBasePositions =
      BaseDivisionIPositions +
      PrincipalPositions +
      AssistantPrincipalPositions +
      AdministrativeSupportPositions +
      InstructionalSupportPositions
  )

lea_calculations <- lea_input |>
  left_join(
    lea_base_totals,
    by = c("DistrictCode", "DistrictName")
  ) |>
  mutate(
    Superintendent = 1,
    `Administrative Assistant` = 1,

    # The proposed rule does not explicitly authorize fractional Assistant
    # Superintendent positions. Retain the current-model completed-threshold rule.
    `Assistant Superintendent` =
      pmin(floor(TotalBasePositions / 300), 2),

    Director = director_positions(TotalBasePositions),
    `11-Month Supervisor` = TotalBasePositions / 150,
    `Buildings and Grounds Supervisor` = 1,
    `Food Services Supervisor` =
      1 + floor(pmax(BaseDivisionIPositions - 500, 0) / 500),
    `Transportation Supervisor` = Enrollment / 7500,
    `Reading Cadre` = 1
  )

stop_if_rows(
  lea_calculations |>
    filter(
      is.na(BaseDivisionIPositions) |
      is.na(TotalBasePositions)
    ),
  "An LEA is missing its proposed Base-position totals."
)

central_components <- bind_rows(
  lea_calculations |>
    mutate(
      Component = "Superintendent",
      RawInputValue = 1,
      AppliedFactor = 1,
      FundingQuantity = Superintendent,
      CalculationStatus = "Calculated"
    ),
  lea_calculations |>
    mutate(
      Component = "Administrative Assistant",
      RawInputValue = 1,
      AppliedFactor = 1,
      FundingQuantity = `Administrative Assistant`,
      CalculationStatus = "Calculated"
    ),
  lea_calculations |>
    mutate(
      Component = "Assistant Superintendent",
      RawInputValue = TotalBasePositions,
      AppliedFactor = NA_real_,
      FundingQuantity = `Assistant Superintendent`,
      CalculationStatus = paste(
        "Calculated from Total Base positions using completed",
        "300-position thresholds; maximum two"
      )
    ),
  lea_calculations |>
    mutate(
      Component = "Director",
      RawInputValue = TotalBasePositions,
      AppliedFactor = NA_real_,
      FundingQuantity = Director,
      CalculationStatus =
        "Calculated from Total Base positions using completed thresholds"
    ),
  lea_calculations |>
    mutate(
      Component = "11-Month Supervisor",
      RawInputValue = TotalBasePositions,
      AppliedFactor = 1 / 150,
      FundingQuantity = `11-Month Supervisor`,
      CalculationStatus =
        "Calculated from Total Base positions; fractional positions retained"
    ),
  lea_calculations |>
    mutate(
      Component = "Buildings and Grounds Supervisor",
      RawInputValue = 1,
      AppliedFactor = 1,
      FundingQuantity = `Buildings and Grounds Supervisor`,
      CalculationStatus = "Calculated"
    ),
  lea_calculations |>
    mutate(
      Component = "Food Services Supervisor",
      RawInputValue = BaseDivisionIPositions,
      AppliedFactor = NA_real_,
      FundingQuantity = `Food Services Supervisor`,
      CalculationStatus =
        "Calculated from Base Division I positions using completed thresholds"
    ),
  lea_calculations |>
    mutate(
      Component = "Transportation Supervisor",
      RawInputValue = Enrollment,
      AppliedFactor = 1 / 7500,
      FundingQuantity = `Transportation Supervisor`,
      CalculationStatus = "Calculated"
    ),
  lea_calculations |>
    mutate(
      Component = "Reading Cadre",
      RawInputValue = 1,
      AppliedFactor = 1,
      FundingQuantity = `Reading Cadre`,
      CalculationStatus = "Calculated"
    )
) |>
  transmute(
    CalculationLevel = "LEA",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode = NA_integer_,
    SchoolName = "LEA Total",
    IsSchool = FALSE,
    IsSchoolCalculationUnit = FALSE,
    CalculationUnitType = "LEA total",
    CalculationUnitName = "LEA Total",
    CalculationUnitSequence = NA_integer_,
    SchoolCalculationUnitCount,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy,
    Component,
    RawInputValue,
    AppliedFactor,
    FundingQuantity,
    CalculationStatus = case_when(
      DistrictCode == dafb_district_code ~
        paste0(CalculationStatus, "; excluded from statewide totals"),
      TRUE ~ CalculationStatus
    ),
    CalculationComplete = !is.na(FundingQuantity)
  )


# COMBINE QUANTITIES AND ADD RULES ---------------------------------------------

proposed_model_quantities <- bind_rows(
  base_components,
  weighted_components,
  central_components
) |>
  left_join(proposed_model_rules, by = "Component") |>
  mutate(Model = "Proposed model", .before = 1) |>
  select(
    Model,
    CalculationLevel,
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    IsSchool,
    IsSchoolCalculationUnit,
    CalculationUnitType,
    CalculationUnitName,
    CalculationUnitSequence,
    SchoolCalculationUnitCount,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy,
    FundingSection,
    Component,
    RateComponent,
    QuantityType,
    SourceField,
    Rule,
    RawInputValue,
    AppliedFactor,
    FundingQuantity,
    CalculationStatus,
    CalculationComplete
  ) |>
  arrange(
    DistrictName,
    CalculationLevel,
    SchoolName,
    CalculationUnitSequence,
    CalculationUnitName,
    FundingSection,
    Component
  )

stop_if_rows(
  proposed_model_quantities |>
    filter(is.na(FundingSection) | is.na(RateComponent)),
  "A proposed-model component is missing from the rule table."
)


# SUMMARY AND ISSUES ------------------------------------------------------------

proposed_quantity_summary <- proposed_model_quantities |>
  summarise(
    RawInputValue = sum(RawInputValue, na.rm = TRUE),
    FundingQuantity = sum(FundingQuantity, na.rm = TRUE),
    RowsMissingInput = sum(!CalculationComplete),
    CalculationComplete = all(CalculationComplete),
    .by = c(
      OperationalEnrollmentBasis,
      CharterBuildingPolicy,
      FundingSection,
      QuantityType,
      Component
    )
  ) |>
  mutate(SummaryScope = "Includes DAFB", .before = 1) |>
  arrange(FundingSection, Component)

missing_input_issues <- proposed_model_quantities |>
  filter(!CalculationComplete) |>
  count(Component, name = "AffectedRows") |>
  transmute(
    Priority = "Needs data",
    Component,
    Issue = "A required input is missing.",
    AffectedRows,
    CurrentTreatment = "The funding quantity remains blank.",
    Action = "Correct the source, crosswalk, or manual allocation."
  )

charter_adjustment_count <- read_csv(
  proposed_charter_reconciliation_path,
  show_col_types = FALSE
) |>
  filter(abs(BuildingVsSharedDifference) > 1e-8) |>
  distinct(DistrictCode, InputCategory) |>
  nrow()

charter_adjustment_issue <- if (charter_adjustment_count > 0) {
  tibble(
    Priority = "Documented adjustment",
    Component = "Charter building inputs",
    Issue = "A calculator building total differs from the shared charter total.",
    AffectedRows = charter_adjustment_count,
    CurrentTreatment = paste(
      "The shared total is retained. Student counts use largest remainder",
      "after applying calculator shares; vocational values retain full precision."
    ),
    Action = "Review 06_proposed_charter_reconciliation.csv."
  )
} else {
  tibble(
    Priority = character(),
    Component = character(),
    Issue = character(),
    AffectedRows = integer(),
    CurrentTreatment = character(),
    Action = character()
  )
}

assumption_issues <- tribble(
  ~Priority, ~Component, ~Issue, ~AffectedRows, ~CurrentTreatment, ~Action,

  "Calculator-reproduced structure",
  "School-based positions",
  "Districts use school codes while charters use calculator building rows.",
  NA_integer_,
  "School-based Base formulas are calculated for each unit and rolled to the official school or charter.",
  "Maintain the calculator structure and LEA crosswalk when sources change.",

  "Documented method",
  "Charter building inputs",
  "The shared charter total remains authoritative.",
  NA_integer_,
  paste(
    "Calculator category shares distribute the shared total;",
    "student counts use largest remainder and enrollment shares are the fallback."
  ),
  "Review the charter reconciliation after each source update.",

  "Confirmed policy",
  "Operational Funding - Enrollment",
  "Operational Enrollment uses total enrollment.",
  NA_integer_,
  paste0("The pipeline applies ", operational_enrollment_basis, " enrollment multiplied by 0.20."),
  "Revise only if the confirmed policy changes.",
  
  "Externally provided implementation guidance",
  "Opportunity and Operational Funding",
  paste(
    "Per guidance from", weighted_rate_guidance_source,
    "Opportunity and Operational Funding are treated as fixed statewide pools."
  ),
  NA_integer_,
  paste(
    "The pool amounts come from", weighted_pool_amount_source,
    "and the per-weighted-student rates are recalculated whenever eligible",
    "weighted counts or the reporting scope change."
  ),
  paste(
    "Update the pool amounts when the authoritative source changes;",
    "revise the recalculation method only if the implementation guidance changes."
  ),

  "Documented assumption",
  "Dover Air Force Base",
  "DAFB is calculated normally and excluded from statewide totals.",
  NA_integer_,
  "IncludeInStatewide is FALSE for DAFB.",
  "Change lea_crosswalk.csv only if the statewide universe changes.",

  "Documented implementation choice",
  "Assistant Superintendent",
  paste(
    "The workbook formula appears to allow a proportional second position,",
    "but the proposed rule does not explicitly authorize fractions."
  ),
  NA_integer_,
  paste(
    "The pipeline applies completed 300-position thresholds to Total Base",
    "positions, capped at two, following the current-model default rule."
  ),
  "Revise only if fractional eligibility is explicitly confirmed.",

  "Documented interpretation",
  "Central Office position base",
  paste(
    "Assistant Superintendent, Director, and 11-Month Supervisor use Total Base",
    "positions: Base Division I plus Principal, Assistant Principal,",
    "Administrative Support Professionals, and Instructional Supports."
  ),
  NA_integer_,
  paste(
    "Food Services Supervisor continues to use the narrower Base Division I",
    "subtotal, consistent with the calculator rule."
  ),
  "Maintain the Base subtotal definitions when formulas or source workbooks change.",

  "Calculator-reproduced rule",
  "Instructional Supports",
  "Instructional Supports equal 20 percent of all Base Division I positions.",
  NA_integer_,
  paste(
    "The factor is applied to school calculation units and to non-school",
    "vocational/program adjustment rows with Base Division I positions."
  ),
  "Revise the 0.20 factor or eligible position base only if the policy changes.",

  "Difference from calculator",
  "Additional State Funding",
  "The calculator includes an optional 50/50 split without a supporting written rule.",
  NA_integer_,
  "The optional amount is excluded.",
  "Add it only after written confirmation."
)

proposed_quantity_issues <- bind_rows(
  missing_input_issues,
  charter_adjustment_issue,
  assumption_issues
)

write_model_csv(proposed_model_quantities, proposed_quantities_path)
write_review_csv(proposed_model_rules, proposed_rules_path)
write_review_csv(proposed_quantity_summary, proposed_quantity_summary_path)
write_review_csv(proposed_quantity_issues, proposed_quantity_issues_path)

message("Created proposed-model quantities: ", proposed_quantities_path)
message("Raw student counts and weights are visible in the quantity output.")
message("Review proposed-model issues: ", proposed_quantity_issues_path)
