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

check_required_files(c(current_school_input_path, current_lea_input_path))


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
    "UnitsTotal",
    "CustodianPositions",
    "SatelliteCafeteriaCount"
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

  "School", "Custodians", "Position", "CustodianPositions",
  "Use site-evaluated custodian positions.",

  "School", "Cafeteria Manager", "Position", "SatelliteCafeteriaCount",
  "Charters earn 0.73 plus 0.73 per satellite.",

  "School", "Cafeteria Worker", "Position", "Enrollment",
  "Charters earn 0.0062 positions per student.",

  "LEA", "Superintendent", "Position", "LEAType",
  "One per district; DAFB is treated as a district; charters are ineligible.",

  "LEA", "Assistant Superintendent", "Position", "UnitsTotal",
  "One per completed 300 Division I units, capped at two, for districts and DAFB.",

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
  "Districts and DAFB earn one at 500 units or with at least four lunch buildings.",

  "LEA", "Transportation Supervisor", "Position", "Enrollment",
  "Enrollment divided by 7,500; fractional positions are allowed.",

  "LEA", "Reading Cadre", "Position", "LEAType",
  "One per district; DAFB is treated as a district; charters are ineligible."
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
    ),
    Custodians = case_when(
      !IsSchool ~ 0,
      TRUE ~ CustodianPositions
    ),
    `Cafeteria Manager` = case_when(
      !IsSchool | LEAType != "Charter" ~ 0,
      TRUE ~ 0.73 * (1 + coalesce(SatelliteCafeteriaCount, 0))
    ),
    `Cafeteria Worker` = case_when(
      !IsSchool | LEAType != "Charter" ~ 0,
      TRUE ~ Enrollment * 0.0062
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
    CustodianPositions,
    SatelliteCafeteriaCount,
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
    `Driver Education Teacher`,
    Custodians,
    `Cafeteria Manager`,
    `Cafeteria Worker`
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
      Component == "Custodians" ~ CustodianPositions,
      Component == "Cafeteria Manager" ~ SatelliteCafeteriaCount,
      Component == "Cafeteria Worker" ~ Enrollment,
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
        "Vocational Deduct",
        "Custodians"
      ) ~ 1,
      Component == "Counselor / Social Worker" ~ 1 / 250,
      Component == "School Psychologist" ~ 1 / 700,
      Component == "Academic Excellence" ~ 1 / 250,
      Component == "Driver Education Teacher" ~ 1 / 125,
      Component == "Cafeteria Worker" ~ 0.0062,
      TRUE ~ NA_real_
    ),
    CalculationStatus = case_when(
      !IsSchool &
        !Component %in% c("Vocational Division I", "Vocational Deduct") ~
        "Not a school-code row",
      Component %in% school_components[1:8] ~ "Reported",
      Component == "Custodians" & is.na(FundingQuantity) ~ "Missing input",
      Component == "Custodians" ~ "Provided input",
      Component == "Cafeteria Manager" & LEAType != "Charter" ~
        "Not modeled for districts",
      Component == "Cafeteria Manager" & is.na(SatelliteCafeteriaCount) ~
        "Base only; satellite count missing",
      Component == "Cafeteria Worker" & LEAType != "Charter" ~
        "Not modeled for districts",
      Component == "Nurse" ~ "Calculated using the 30-percent fractional rule",
      TRUE ~ "Calculated"
    ),
    InputComplete = case_when(
      !IsSchool &
        !Component %in% c("Vocational Division I", "Vocational Deduct") ~
        TRUE,
      Component == "Custodians" ~ !is.na(CustodianPositions),
      Component == "Cafeteria Manager" & LEAType == "Charter" ~
        !is.na(SatelliteCafeteriaCount),
      TRUE ~ TRUE
    ),
    QuantityProvisional =
      IsSchool &
      LEAType == "Charter" &
      Component == "Cafeteria Manager" &
      is.na(SatelliteCafeteriaCount),
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
    IsDistrict = LEAType %in% c("District", "Dover Air Force Base"),
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
        "Calculated normally; excluded from statewide totals",
      Component %in% c(
        "Superintendent",
        "Assistant Superintendent",
        "Food Services Supervisor",
        "Reading Cadre"
      ) & !IsDistrict ~ "Ineligible",
      TRUE ~ "Calculated"
    ),
    InputComplete = case_when(
      Component == "Buildings and Grounds Supervisor" ~
        !is.na(CustodialUnits),
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
  mutate(SummaryScope = "Includes DAFB", .before = 1) |>
  arrange(CalculationLevel, Component)

missing_input_issues <- current_model_quantities |>
  filter(!InputComplete, Component != "Cafeteria Manager") |>
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

satellite_issue <- school_input |>
  filter(
    IsSchool,
    LEAType == "Charter",
    is.na(SatelliteCafeteriaCount)
  ) |>
  summarise(AffectedRows = n()) |>
  filter(AffectedRows > 0) |>
  transmute(
    Priority = "Needs review",
    Component = "Cafeteria Manager",
    Issue = "Charter satellite counts are missing.",
    AffectedRows,
    CurrentTreatment = "Only the base 0.73 position is calculated.",
    Action = "Provide satellite counts or confirm that all are zero."
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

  "Documented assumption",
  "Dover Air Force Base",
  "DAFB is treated like a district for LEA-level formulas.",
  NA_integer_,
  "DAFB is calculated normally and excluded from statewide summaries.",
  "Revise only if DAFB eligibility changes.",

  "Not modeled",
  "District cafeteria positions",
  "District cafeteria funding follows a separate process.",
  NA_integer_,
  "Only charter cafeteria formulas are included.",
  "Add the district process when it becomes available."
)

current_quantity_issues <- bind_rows(
  missing_input_issues,
  satellite_issue,
  policy_issues
)

write_model_csv(current_model_quantities, current_quantities_path)
write_review_csv(current_model_rules, current_rules_path)
write_review_csv(current_quantity_summary, current_quantity_summary_path)
write_review_csv(current_quantity_issues, current_quantity_issues_path)

message("Created current-model quantities: ", current_quantities_path)
message("Review current-model rules: ", current_rules_path)
message("Review current-model issues: ", current_quantity_issues_path)
