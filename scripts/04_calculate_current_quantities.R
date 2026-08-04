# =============================================================================
# 04_calculate_current_quantities.R
# =============================================================================
# Applies the current-model formulas to the prepared school and LEA inputs.
# Funding rates are not applied in this script.
#
# Main output:
#   04_current_model_quantities.csv
#
# Review outputs:
#   04_current_model_rules.csv
#   04_current_model_quantity_summary.csv
#   04_current_model_issues.csv
#   04_current_outside_formula_components.csv
# =============================================================================

source(file.path("scripts", "00_settings.R"))

current_school_input_path <- file.path(
  output_dir,
  "03_current_school_input.csv"
)
current_lea_input_path <- file.path(
  output_dir,
  "03_current_lea_input.csv"
)
current_quantities_path <- file.path(
  output_dir,
  "04_current_model_quantities.csv"
)
current_quantity_summary_path <- file.path(
  output_dir,
  "04_current_model_quantity_summary.csv"
)
current_quantity_issues_path <- file.path(
  output_dir,
  "04_current_model_issues.csv"
)
current_rules_path <- file.path(
  output_dir,
  "04_current_model_rules.csv"
)
current_district_cafeteria_input_path <- file.path(
  output_dir,
  "03_current_district_cafeteria_allocation.csv"
)
current_outside_formula_path <- file.path(
  output_dir,
  "04_current_outside_formula_components.csv"
)

check_required_files(
  c(
    current_school_input_path,
    current_lea_input_path,
    current_district_cafeteria_input_path
  )
)


# FORMULA HELPERS ---------------------------------------------------------------

assistant_principal_positions <- function(units) {
  case_when(
    units < 25 ~ 0,
    units < 30 ~ 0.65,
    units < 50 ~ 1,
    units < 55 ~ 1.65,
    TRUE ~ 2 + floor((units - 55) / 20)
  )
}

secretary_positions <- function(units) {
  case_when(
    units < 100 ~ floor(units / 10),
    TRUE ~ 10 + floor((units - 100) / 12)
  )
}

director_positions <- function(units) {
  case_when(
    units < 200 ~ 0,
    TRUE ~ pmin(6, 1 + floor((units - 200) / 100))
  )
}

nurse_positions <- function(units) {
  raw_positions <- units / 40

  floor(raw_positions) +
    (raw_positions - floor(raw_positions)) * 0.30
}


# READ PREPARED INPUTS ----------------------------------------------------------

school_input <- read_csv(
  current_school_input_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    IsSchool = as.logical(IsSchool),
    IncludeInStatewide = as.logical(IncludeInStatewide)
  )

lea_input <- read_csv(
  current_lea_input_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IsSchool = as.logical(IsSchool),
    IncludeInStatewide = as.logical(IncludeInStatewide)
  )


district_cafeteria_allocation <- read_csv(
  current_district_cafeteria_input_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolYear = as.integer(SchoolYear)
  )

check_required_columns(
  school_input,
  c(
    "SchoolYear",
    "CountDate",
    "DistrictCode",
    "DistrictName",
    "LEAType",
    "IncludeInStatewide",
    "SchoolCode",
    "SchoolName",
    "IsSchool",
    "Enrollment",
    "K8Enrollment",
    "Grade10Enrollment",
    "UnitsPreK",
    "UnitsK3",
    "Units4_12",
    "UnitsBasicK12",
    "UnitsIntense",
    "UnitsComplex",
    "UnitsVocational",
    "UnitsVocationalDeduct",
    "UnitsTotal"
  ),
  "03_current_school_input.csv"
)

check_required_columns(
  lea_input,
  c(
    "SchoolYear",
    "CountDate",
    "DistrictCode",
    "DistrictName",
    "LEAType",
    "IncludeInStatewide",
    "Enrollment",
    "UnitsBasicK12",
    "UnitsIntense",
    "UnitsComplex",
    "UnitsTotal",
    "CustodialUnits",
    "SchoolLunchBuildingCount"
  ),
  "03_current_lea_input.csv"
)

stop_if_rows(
  school_input |>
    filter(IsSchool != !is.na(SchoolCode)),
  "The current school input has an invalid IsSchool flag."
)


# CURRENT-MODEL RULES -----------------------------------------------------------

current_model_rules <- tribble(
  ~CalculationLevel, ~Component, ~QuantityType, ~SourceField, ~Rule,

  "School", "Division I Teacher - Pre-K", "Position", "UnitsPreK",
  "Use reported Basic Pre-K special education positions; the available Unit Count source does not include the corresponding Pre-K headcount needed to independently reproduce the 1:8.4 ratio.",

  "School", "Division I Teacher - K-3 Regular Education", "Position", "UnitsK3",
  "Use reported positions.",

  "School", "Division I Teacher - 4-12 Regular Education", "Position", "Units4_12",
  "Use reported positions.",

  "School", "Division I Teacher - Basic Special Education", "Position", "UnitsBasicK12",
  "Use reported positions.",

  "School", "Division I Teacher - Intensive Special Education", "Position", "UnitsIntense",
  "Use reported positions.",

  "School", "Division I Teacher - Complex Special Education", "Position", "UnitsComplex",
  "Use reported positions.",

  "School", "Vocational Division I", "Position", "UnitsVocational",
  "Use reported vocational Division I positions.",

  "School", "Vocational Deduct", "Position", "UnitsVocationalDeduct",
  "Use the reported negative vocational deduction.",

  "School", "Principal", "Position", "UnitsTotal",
  "One principal when total Division I units are at least 15.",

  "School", "Assistant Principal", "Position", "UnitsTotal",
  "Apply the 25, 30, 50, 55, and additional 20-unit thresholds.",

  "School", "Counselor / Social Worker", "Position", "K8Enrollment",
  "K-8 enrollment divided by 250.",

  "School", "School Psychologist", "Position", "K8Enrollment",
  "K-8 enrollment divided by 700.",

  "School", "Nurse", "Position", "UnitsTotal",
  "One per 40 units; the fractional portion is funded at 30 percent.",

  "School", "Academic Excellence", "Position", "Enrollment",
  "Enrollment divided by 250.",

  "School", "Secretary", "Position", "UnitsTotal",
  "One per 10 units through 100; one per completed 12 thereafter.",

  "School", "Driver Education Teacher", "Position", "Grade10Enrollment",
  "Grade 10 enrollment divided by 125.",

  "LEA", "Superintendent", "Position", "LEAType",
  "One per funded district; charters and DAFB are ineligible.",

  "LEA", "Assistant Superintendent", "Position", "UnitsTotal",
  "One per completed 300 Division I units, capped at two, for funded districts.",

  "LEA", "Director", "Position", "UnitsTotal",
  "One at 200 units plus one per completed additional 100, capped at six.",

  "LEA", "Administrative Assistant", "Position", "LEA",
  "One per LEA.",

  "LEA", "11-Month Supervisor", "Position", "UnitsTotal",
  "Division I units divided by 150.",

  "LEA", "Related Services Specialist - Basic", "Position", "UnitsBasicK12",
  "Basic units divided by 57.",

  "LEA", "Related Services Specialist - Intensive", "Position", "UnitsIntense",
  "Intensive units divided by 5.5.",

  "LEA", "Related Services Specialist - Complex", "Position", "UnitsComplex",
  "Complex units divided by 3.",

  "LEA", "Visiting Teacher", "Position", "UnitsTotal",
  "Division I units divided by 250.",

  "LEA", "Buildings and Grounds Supervisor", "Position", "CustodialUnits",
  "One when custodial units are at least 95.",

  "LEA", "Food Services Supervisor", "Position", "SchoolLunchBuildingCount",
  "Funded districts earn one at 500 units or with at least four lunch-program buildings.",

  "LEA", "Transportation Supervisor", "Position", "Enrollment",
  "Enrollment divided by 7,500; fractional positions are allowed.",

  "LEA", "Reading Cadre", "Position", "LEAType",
  "One per funded district; charters and DAFB are ineligible."
)


# SCHOOL-LEVEL QUANTITIES -------------------------------------------------------

school_calculations <- school_input |>
  mutate(
    Principal = case_when(
      !IsSchool ~ 0,
      UnitsTotal >= 15 ~ 1,
      TRUE ~ 0
    ),
    `Assistant Principal` = case_when(
      !IsSchool ~ 0,
      TRUE ~ assistant_principal_positions(UnitsTotal)
    ),
    `Counselor / Social Worker` = case_when(
      !IsSchool ~ 0,
      TRUE ~ K8Enrollment / 250
    ),
    `School Psychologist` = case_when(
      !IsSchool ~ 0,
      TRUE ~ K8Enrollment / 700
    ),
    Nurse = case_when(
      !IsSchool ~ 0,
      TRUE ~ nurse_positions(UnitsTotal)
    ),
    `Academic Excellence` = case_when(
      !IsSchool ~ 0,
      TRUE ~ Enrollment / 250
    ),
    Secretary = case_when(
      !IsSchool ~ 0,
      TRUE ~ secretary_positions(UnitsTotal)
    ),
    `Driver Education Teacher` = case_when(
      !IsSchool ~ 0,
      TRUE ~ Grade10Enrollment / 125
    )
  )

school_components <- current_model_rules |>
  filter(CalculationLevel == "School") |>
  pull(Component)

school_quantities <- school_calculations |>
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
    UnitsPreK,
    UnitsK3,
    Units4_12,
    UnitsBasicK12,
    UnitsIntense,
    UnitsComplex,
    UnitsVocational,
    UnitsVocationalDeduct,
    UnitsTotal,
    K8Enrollment,
    Grade10Enrollment,
    Enrollment,
    `Division I Teacher - Pre-K` = UnitsPreK,
    `Division I Teacher - K-3 Regular Education` = UnitsK3,
    `Division I Teacher - 4-12 Regular Education` = Units4_12,
    `Division I Teacher - Basic Special Education` = UnitsBasicK12,
    `Division I Teacher - Intensive Special Education` = UnitsIntense,
    `Division I Teacher - Complex Special Education` = UnitsComplex,
    `Vocational Division I` = UnitsVocational,
    `Vocational Deduct` = UnitsVocationalDeduct,
    Principal,
    `Assistant Principal`,
    `Counselor / Social Worker`,
    `School Psychologist`,
    Nurse,
    `Academic Excellence`,
    Secretary,
    `Driver Education Teacher`
  ) |>
  pivot_longer(
    cols = all_of(school_components),
    names_to = "Component",
    values_to = "FundingQuantity"
  ) |>
  mutate(
    CalculationLevel = "School",
    QuantityType = "Position",
    RawInputValue = case_when(
      Component == "Division I Teacher - Pre-K" ~ UnitsPreK,
      Component == "Division I Teacher - K-3 Regular Education" ~ UnitsK3,
      Component == "Division I Teacher - 4-12 Regular Education" ~ Units4_12,
      Component == "Division I Teacher - Basic Special Education" ~ UnitsBasicK12,
      Component == "Division I Teacher - Intensive Special Education" ~ UnitsIntense,
      Component == "Division I Teacher - Complex Special Education" ~ UnitsComplex,
      Component == "Vocational Division I" ~ UnitsVocational,
      Component == "Vocational Deduct" ~ UnitsVocationalDeduct,
      Component %in% c("Principal", "Assistant Principal", "Nurse", "Secretary") ~ UnitsTotal,
      Component %in% c("Counselor / Social Worker", "School Psychologist") ~ K8Enrollment,
      Component == "Academic Excellence" ~ Enrollment,
      Component == "Driver Education Teacher" ~ Grade10Enrollment,
      TRUE ~ NA_real_
    ),
    AppliedFactor = case_when(
      Component %in% c(
        "Division I Teacher - Pre-K",
        "Division I Teacher - K-3 Regular Education",
        "Division I Teacher - 4-12 Regular Education",
        "Division I Teacher - Basic Special Education",
        "Division I Teacher - Intensive Special Education",
        "Division I Teacher - Complex Special Education",
        "Vocational Division I",
        "Vocational Deduct"
      ) ~ 1,
      Component == "Counselor / Social Worker" ~ 1 / 250,
      Component == "School Psychologist" ~ 1 / 700,
      Component == "Academic Excellence" ~ 1 / 250,
      Component == "Driver Education Teacher" ~ 1 / 125,
      TRUE ~ NA_real_
    ),
    CalculationStatus = case_when(
      !IsSchool &
        !Component %in% c("Vocational Division I", "Vocational Deduct") ~
        "Not a school-code row",
      Component %in% school_components[1:8] ~ "Reported",
      Component == "Nurse" ~ "Calculated using the 30-percent fractional rule",
      TRUE ~ "Calculated"
    ),
    InputComplete = TRUE,
    QuantityProvisional = FALSE,
    CalculationComplete = !is.na(FundingQuantity)
  ) |>
  select(
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
    QuantityType,
    Component,
    RawInputValue,
    AppliedFactor,
    FundingQuantity,
    CalculationStatus,
    InputComplete,
    QuantityProvisional,
    CalculationComplete
  )


# LEA-LEVEL QUANTITIES ----------------------------------------------------------

lea_calculations <- lea_input |>
  mutate(
    IsDistrict = LEAType == "District" & IncludeInStatewide,
    Superintendent = case_when(IsDistrict ~ 1, TRUE ~ 0),
    `Assistant Superintendent` = case_when(
      IsDistrict ~ pmin(floor(UnitsTotal / 300), 2),
      TRUE ~ 0
    ),
    Director = director_positions(UnitsTotal),
    `Administrative Assistant` = 1,
    `11-Month Supervisor` = UnitsTotal / 150,
    `Related Services Specialist - Basic` = UnitsBasicK12 / 57,
    `Related Services Specialist - Intensive` = UnitsIntense / 5.5,
    `Related Services Specialist - Complex` = UnitsComplex / 3,
    `Visiting Teacher` = UnitsTotal / 250,
    `Buildings and Grounds Supervisor` = case_when(
      !IsDistrict ~ 0,
      is.na(CustodialUnits) ~ NA_real_,
      CustodialUnits >= 95 ~ 1,
      TRUE ~ 0
    ),
    `Food Services Supervisor` = case_when(
      !IsDistrict ~ 0,
      UnitsTotal >= 500 ~ 1,
      is.na(SchoolLunchBuildingCount) ~ NA_real_,
      SchoolLunchBuildingCount >= 4 ~ 1,
      TRUE ~ 0
    ),
    `Transportation Supervisor` = Enrollment / 7500,
    `Reading Cadre` = case_when(IsDistrict ~ 1, TRUE ~ 0)
  )

lea_components <- current_model_rules |>
  filter(CalculationLevel == "LEA") |>
  pull(Component)

lea_quantities <- lea_calculations |>
  transmute(
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode = NA_integer_,
    SchoolName = "LEA Total",
    IsSchool = FALSE,
    IsDistrict,
    Enrollment,
    UnitsTotal,
    UnitsBasicK12,
    UnitsIntense,
    UnitsComplex,
    CustodialUnits,
    SchoolLunchBuildingCount,
    Superintendent,
    `Assistant Superintendent`,
    Director,
    `Administrative Assistant`,
    `11-Month Supervisor`,
    `Related Services Specialist - Basic`,
    `Related Services Specialist - Intensive`,
    `Related Services Specialist - Complex`,
    `Visiting Teacher`,
    `Buildings and Grounds Supervisor`,
    `Food Services Supervisor`,
    `Transportation Supervisor`,
    `Reading Cadre`
  ) |>
  pivot_longer(
    cols = all_of(lea_components),
    names_to = "Component",
    values_to = "FundingQuantity"
  ) |>
  mutate(
    CalculationLevel = "LEA",
    QuantityType = "Position",
    RawInputValue = case_when(
      Component %in% c("Superintendent", "Administrative Assistant", "Reading Cadre") ~ 1,
      Component %in% c(
        "Assistant Superintendent",
        "Director",
        "11-Month Supervisor",
        "Visiting Teacher"
      ) ~ UnitsTotal,
      Component == "Related Services Specialist - Basic" ~ UnitsBasicK12,
      Component == "Related Services Specialist - Intensive" ~ UnitsIntense,
      Component == "Related Services Specialist - Complex" ~ UnitsComplex,
      Component == "Buildings and Grounds Supervisor" ~ CustodialUnits,
      Component == "Food Services Supervisor" ~ SchoolLunchBuildingCount,
      Component == "Transportation Supervisor" ~ Enrollment,
      TRUE ~ NA_real_
    ),
    AppliedFactor = case_when(
      Component == "11-Month Supervisor" ~ 1 / 150,
      Component == "Related Services Specialist - Basic" ~ 1 / 57,
      Component == "Related Services Specialist - Intensive" ~ 1 / 5.5,
      Component == "Related Services Specialist - Complex" ~ 1 / 3,
      Component == "Visiting Teacher" ~ 1 / 250,
      Component == "Transportation Supervisor" ~ 1 / 7500,
      Component %in% c("Superintendent", "Administrative Assistant", "Reading Cadre") ~ 1,
      TRUE ~ NA_real_
    ),
    CalculationStatus = case_when(
      is.na(FundingQuantity) ~ "Missing input",
      DistrictCode == dafb_district_code ~
        "Retained for source audit; excluded from modeled state funding",
      Component %in% c(
        "Superintendent",
        "Assistant Superintendent",
        "Buildings and Grounds Supervisor",
        "Food Services Supervisor",
        "Reading Cadre"
      ) & !IsDistrict ~ "Ineligible",
      TRUE ~ "Calculated"
    ),
    InputComplete = case_when(
      Component == "Buildings and Grounds Supervisor" ~
        !IsDistrict | !is.na(CustodialUnits),
      Component == "Food Services Supervisor" ~
        !IsDistrict |
        UnitsTotal >= 500 |
        !is.na(SchoolLunchBuildingCount),
      TRUE ~ TRUE
    ),
    QuantityProvisional = FALSE,
    CalculationComplete = !is.na(FundingQuantity)
  ) |>
  select(
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
    QuantityType,
    Component,
    RawInputValue,
    AppliedFactor,
    FundingQuantity,
    CalculationStatus,
    InputComplete,
    QuantityProvisional,
    CalculationComplete
  )


# OUTSIDE-FORMULA REFERENCE COMPONENTS -----------------------------------------

# Brian's FY2025-26 implementation guidance identifies ASPIRA and MOT as the
# only charters receiving one additional 0.73 cafeteria-manager allocation for
# a satellite cafeteria. These reference quantities are documented here but do
# not enter the position-based model comparison.
charter_satellite_cafeterias <- tribble(
  ~DistrictCode, ~SatelliteCafeteriaCount,
  69L, 1,
  88L, 1
)

primary_charters <- lea_input |>
  filter(LEAType == "Charter", IncludeInStatewide) |>
  distinct(
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    Enrollment
  ) |>
  left_join(charter_satellite_cafeterias, by = "DistrictCode") |>
  mutate(SatelliteCafeteriaCount = coalesce(SatelliteCafeteriaCount, 0))

stop_if_rows(
  charter_satellite_cafeterias |>
    anti_join(primary_charters, by = "DistrictCode"),
  "A documented charter satellite cafeteria does not match the primary charter scope."
)

outside_formula_custodians <- tibble(
  Model = "Current model",
  RecordType = "Policy documentation",
  SchoolYear = school_year,
  CountDate = count_date,
  DistrictCode = NA_integer_,
  DistrictName = "Statewide documentation",
  LEAType = NA_character_,
  IncludeInStatewide = FALSE,
  Component = "Custodians",
  QuantityType = "Site-evaluated positions",
  RawInputValue = NA_real_,
  AppliedFactor = NA_real_,
  ReferenceQuantity = NA_real_,
  FundingAmount = NA_real_,
  AllocationBasis = "Positions are determined through site evaluations.",
  Source = "FY2025-26 implementation guidance from Brian",
  InclusionStatus = "Outside formula; excluded from position-based comparison",
  Notes = paste(
    "School-level custodian allocations are not required for the IV&V comparison.",
    "Custodial-unit data remain required for Buildings and Grounds Supervisor eligibility."
  )
)

outside_formula_charter_managers <- primary_charters |>
  transmute(
    Model = "Current model",
    RecordType = "Reference quantity",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    Component = "Cafeteria Manager",
    QuantityType = "Charter reference position quantity",
    RawInputValue = as.numeric(SatelliteCafeteriaCount),
    AppliedFactor = 0.73,
    ReferenceQuantity = 0.73 * (1 + SatelliteCafeteriaCount),
    FundingAmount = NA_real_,
    AllocationBasis = "0.73 per charter plus 0.73 per satellite cafeteria.",
    Source = "FY2025-26 implementation guidance from Brian",
    InclusionStatus = "Outside formula; excluded from position-based comparison",
    Notes = case_when(
      SatelliteCafeteriaCount > 0 ~
        "ASPIRA and MOT are the only charters documented as qualifying for the satellite addition.",
      TRUE ~ "Base charter allocation only."
    )
  )

outside_formula_charter_workers <- primary_charters |>
  transmute(
    Model = "Current model",
    RecordType = "Reference quantity",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    Component = "Cafeteria Worker",
    QuantityType = "Charter reference position quantity",
    RawInputValue = Enrollment,
    AppliedFactor = 0.0062,
    ReferenceQuantity = Enrollment * 0.0062,
    FundingAmount = NA_real_,
    AllocationBasis = "0.62 of a cafeteria worker per 100 students.",
    Source = "FY2025-26 implementation guidance from Brian",
    InclusionStatus = "Outside formula; excluded from position-based comparison",
    Notes = "Reference quantity retained for documentation only."
  )

outside_formula_district_cafeteria <- district_cafeteria_allocation |>
  transmute(
    Model = "Current model",
    RecordType = "Salary allocation",
    SchoolYear,
    CountDate = count_date,
    DistrictCode,
    DistrictName,
    LEAType = "District",
    IncludeInStatewide = TRUE,
    Component = "District Cafeteria Salary Allocation",
    QuantityType = "Outside-formula funding allocation",
    RawInputValue = HoursRequested,
    AppliedFactor = NA_real_,
    ReferenceQuantity = NA_real_,
    FundingAmount = TotalStateAllocation,
    AllocationBasis = paste(
      "Salary allocation based on meals, operating days, requested hours,",
      "salary amounts, worker and manager state shares, and termination pay."
    ),
    Source = SourceFile,
    InclusionStatus = "Outside formula; excluded from position-based comparison",
    Notes
  )

current_outside_formula_components <- bind_rows(
  outside_formula_custodians,
  outside_formula_charter_managers,
  outside_formula_charter_workers,
  outside_formula_district_cafeteria
) |>
  arrange(Component, DistrictName)

stop_if_rows(
  current_outside_formula_components |>
    filter(
      Component %in% outside_formula_current_components,
      InclusionStatus !=
        "Outside formula; excluded from position-based comparison"
    ),
  "An outside-formula component is not classified correctly."
)



# COMBINE AND ADD RULE DESCRIPTIONS --------------------------------------------

current_model_quantities <- bind_rows(
  school_quantities,
  lea_quantities
) |>
  left_join(
    current_model_rules |>
      select(CalculationLevel, Component, SourceField, Rule),
    by = c("CalculationLevel", "Component")
  ) |>
  mutate(Model = "Current model", .before = 1) |>
  arrange(DistrictName, CalculationLevel, SchoolName, Component)

stop_if_rows(
  current_model_quantities |>
    filter(is.na(SourceField) | is.na(Rule)),
  "A current-model component is missing from the rule table."
)


# RECONCILE REPORTED DIVISION I POSITIONS --------------------------------------

reported_components <- school_components[1:8]

base_check <- current_model_quantities |>
  filter(
    CalculationLevel == "School",
    Component %in% reported_components
  ) |>
  summarise(
    ReportedPositions = sum(FundingQuantity, na.rm = TRUE),
    .by = c(DistrictCode, DistrictName, SchoolCode, SchoolName)
  ) |>
  left_join(
    school_input |>
      select(DistrictCode, SchoolCode, SchoolName, UnitsTotal),
    by = c("DistrictCode", "SchoolCode", "SchoolName")
  ) |>
  filter(abs(ReportedPositions - UnitsTotal) > 0.05 + 1e-8)

stop_if_rows(
  base_check,
  "Reported current-model Division I positions do not reconcile to UnitsTotal."
)


# SUMMARY AND ISSUES ------------------------------------------------------------

current_quantity_summary <- current_model_quantities |>
  summarise(
    FundingQuantity = sum(FundingQuantity, na.rm = TRUE),
    RowsMissingInput = sum(!InputComplete),
    RowsProvisional = sum(QuantityProvisional),
    InputComplete = all(InputComplete),
    CalculationComplete = all(CalculationComplete),
    .by = c(CalculationLevel, QuantityType, Component)
  ) |>
  mutate(
    SummaryScope =
      "All source LEAs; DAFB retained for audit and excluded from primary totals",
    .before = 1
  ) |>
  arrange(CalculationLevel, Component)

missing_input_issues <- current_model_quantities |>
  filter(!InputComplete) |>
  count(Component, name = "AffectedRows") |>
  transmute(
    Priority = "Needs data",
    Component,
    Issue = "Required input is missing.",
    AffectedRows,
    CurrentTreatment =
      "The funding quantity remains blank and the component remains incomplete.",
    Action = "Populate the applicable supplemental input."
  )

policy_issues <- tribble(
  ~Priority, ~Component, ~Issue, ~AffectedRows, ~CurrentTreatment, ~Action,

  "Confirmed Unit Count source rule",
  "School-based positions",
  "An officially coded Unit Count school is the recognized school-building calculation unit.",
  NA_integer_,
  paste(
    "Multiple physical sites under one school code are treated as one calculation unit;",
    "rows without a school code retain vocational adjustments but receive no other school-based positions."
  ),
  "Revise only if the official Unit Count school-building definition changes.",

  "Source limitation",
  "Division I Teacher - Pre-K",
  paste(
    "Unit Count does not provide the special education Pre-K student headcount",
    "needed to independently reproduce the 1:8.4 position ratio."
  ),
  NA_integer_,
  "The pipeline uses the reported UnitsPreK quantity without recalculation.",
  "Retain the reported quantity unless a validated Pre-K headcount source becomes available.",

  "Policy question",
  "Nurse",
  "The 30-percent fractional rule still needs confirmation.",
  NA_integer_,
  "The fractional portion is multiplied by 30 percent.",
  "Confirm the interpretation.",

  "Confirmed scope decision",
  "Dover Air Force Base",
  "DAFB does not receive state funding and is excluded from the modeled scope.",
  NA_integer_,
  "Source records may be retained for audit, but DAFB is excluded from primary totals.",
  "No further action is required unless the confirmed funding treatment changes.",

  "Outside formula",
  "Custodians and cafeteria support",
  paste(
    "Custodians, Cafeteria Managers, and Cafeteria Workers are funded outside",
    "the proposed position formula. District cafeteria support is a separate salary allocation."
  ),
  NA_integer_,
  paste(
    "Reference quantities and the FY26 district allocation are written to",
    "04_current_outside_formula_components.csv and excluded from model totals."
  ),
  "Retain for documentation; do not add these items to the position-based comparison."
)

current_quantity_issues <- bind_rows(
  missing_input_issues,
  policy_issues
)

write_model_csv(current_model_quantities, current_quantities_path)
write_review_csv(current_model_rules, current_rules_path)
write_review_csv(current_quantity_summary, current_quantity_summary_path)
write_review_csv(current_quantity_issues, current_quantity_issues_path)
write_review_csv(current_outside_formula_components, current_outside_formula_path)

message("Created current-model quantities: ", current_quantities_path)
message("Review current-model rules: ", current_rules_path)
message("Review current-model issues: ", current_quantity_issues_path)
message("Review outside-formula components: ", current_outside_formula_path)
