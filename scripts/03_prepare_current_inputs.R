# =============================================================================
# 03_prepare_current_inputs.R
# =============================================================================
# Adds the manually maintained current-model inputs to the shared model input.
# No funding formulas are calculated in this script.
#
# Main outputs:
#   03_current_school_input.csv
#   03_current_lea_input.csv
#   03_current_district_cafeteria_allocation.csv
#
# Review output:
#   03_current_input_qc.csv
# =============================================================================

source(file.path("scripts", "00_settings.R"))

shared_input_path <- file.path(output_dir, "02_shared_model_input.csv")
current_school_input_path <- file.path(
  output_dir,
  "03_current_school_input.csv"
)
current_lea_input_path <- file.path(
  output_dir,
  "03_current_lea_input.csv"
)
current_district_cafeteria_output_path <- file.path(
  output_dir,
  "03_current_district_cafeteria_allocation.csv"
)
current_input_qc_path <- file.path(
  output_dir,
  "03_current_input_qc.csv"
)

check_required_files(
  c(
    shared_input_path,
    lea_crosswalk_path,
    current_district_cafeteria_allocation_path
  )
)


# READ SHARED INPUT AND LEA CROSSWALK ------------------------------------------

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
    IncludeInStatewide = as.logical(IncludeInStatewide)
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
  shared_input |>
    filter(AggregationLevel != "Statewide") |>
    distinct(DistrictCode, DistrictName) |>
    anti_join(lea_crosswalk, by = "DistrictCode"),
  "One or more LEAs in the shared input are missing from lea_crosswalk.csv."
)

school_input <- shared_input |>
  filter(AggregationLevel == "School") |>
  left_join(
    lea_crosswalk |>
      select(DistrictCode, LEAType, IncludeInStatewide),
    by = "DistrictCode"
  )

lea_input <- shared_input |>
  filter(AggregationLevel == "District") |>
  left_join(
    lea_crosswalk |>
      select(DistrictCode, LEAType, IncludeInStatewide),
    by = "DistrictCode"
  ) |>
  mutate(
    SchoolCode = NA_integer_,
    SchoolName = "LEA Total",
    IsSchool = FALSE
  )


# CREATE SUPPLEMENTAL INPUT TEMPLATES WHEN NEEDED ------------------------------

school_supplement_template <- school_input |>
  filter(IsSchool) |>
  transmute(
    SchoolYear,
    DistrictCode,
    DistrictName,
    SchoolCode,
    SchoolName,
    HasSchoolLunchProgram = NA_real_,
    Notes = NA_character_
  )

if (!file.exists(current_school_supplement_path)) {
  write_csv(
    school_supplement_template,
    current_school_supplement_path,
    na = ""
  )

  message(
    "Created blank school supplement: ",
    current_school_supplement_path
  )
}

lea_supplement_template <- lea_input |>
  transmute(
    SchoolYear,
    DistrictCode,
    DistrictName,
    CustodialUnits = NA_real_,
    Notes = NA_character_
  )

if (!file.exists(current_lea_supplement_path)) {
  write_csv(
    lea_supplement_template,
    current_lea_supplement_path,
    na = ""
  )

  message(
    "Created blank LEA supplement: ",
    current_lea_supplement_path
  )
}


# READ AND CHECK SUPPLEMENTAL INPUTS -------------------------------------------

school_supplement <- read_csv(
  current_school_supplement_path,
  show_col_types = FALSE
) |>
  mutate(
    SchoolYear = as.integer(SchoolYear),
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    HasSchoolLunchProgram = as.numeric(HasSchoolLunchProgram)
  )

check_required_columns(
  school_supplement,
  names(school_supplement_template),
  "current_school_supplement.csv"
)

stop_if_rows(
  school_supplement |>
    count(SchoolYear, DistrictCode, SchoolCode) |>
    filter(n > 1),
  "current_school_supplement.csv contains duplicate school keys."
)

school_supplement_key <- c(
  "SchoolYear",
  "DistrictCode",
  "SchoolCode"
)

stop_if_rows(
  school_supplement_template |>
    anti_join(school_supplement, by = school_supplement_key) |>
    select(all_of(school_supplement_key), DistrictName, SchoolName),
  paste0(
    "current_school_supplement.csv is missing a coded school. ",
    "Add the displayed row before continuing."
  )
)

stop_if_rows(
  school_supplement |>
    anti_join(school_supplement_template, by = school_supplement_key) |>
    select(all_of(school_supplement_key), DistrictName, SchoolName),
  paste0(
    "current_school_supplement.csv contains a stale or non-school row. ",
    "Remove the displayed row before continuing."
  )
)

stop_if_rows(
  school_supplement |>
    filter(
      !is.na(HasSchoolLunchProgram) &
        !HasSchoolLunchProgram %in% c(0, 1)
    ),
  "The school supplement contains an invalid lunch-program indicator."
)

lea_supplement <- read_csv(
  current_lea_supplement_path,
  show_col_types = FALSE
) |>
  mutate(
    SchoolYear = as.integer(SchoolYear),
    DistrictCode = as.integer(DistrictCode),
    CustodialUnits = as.numeric(CustodialUnits)
  )

check_required_columns(
  lea_supplement,
  names(lea_supplement_template),
  "current_lea_supplement.csv"
)

stop_if_rows(
  lea_supplement |>
    count(SchoolYear, DistrictCode) |>
    filter(n > 1),
  "current_lea_supplement.csv contains duplicate LEA keys."
)

lea_supplement_key <- c("SchoolYear", "DistrictCode")

stop_if_rows(
  lea_supplement_template |>
    anti_join(lea_supplement, by = lea_supplement_key) |>
    select(all_of(lea_supplement_key), DistrictName),
  paste0(
    "current_lea_supplement.csv is missing an LEA. ",
    "Add the displayed row before continuing."
  )
)

stop_if_rows(
  lea_supplement |>
    anti_join(lea_supplement_template, by = lea_supplement_key) |>
    select(all_of(lea_supplement_key), DistrictName),
  paste0(
    "current_lea_supplement.csv contains a stale LEA row. ",
    "Remove the displayed row before continuing."
  )
)

stop_if_rows(
  lea_supplement |>
    filter(!is.na(CustodialUnits), CustodialUnits < 0),
  "The LEA supplement contains a negative custodial-unit value."
)


# READ AND CHECK DISTRICT CAFETERIA REFERENCE ----------------------------------

district_cafeteria_allocation <- read_csv(
  current_district_cafeteria_allocation_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    across(
      c(
        Meals,
        BaseMonthDays,
        AverageDailyMeals,
        HoursAllowed,
        HoursRequested,
        AmountPerDay,
        TotalAnnualSalaryAmount,
        WorkerStateLiability,
        ManagerSalaries,
        ManagerStateLiability,
        TerminationPayStatePortion,
        TotalStateAllocation,
        Preload90,
        AmountDue
      ),
      as.numeric
    ),
    SchoolYear = school_year,
    .before = 1
  )

check_required_columns(
  district_cafeteria_allocation,
  c(
    "SchoolYear",
    "DistrictCode",
    "DistrictName",
    "Meals",
    "BaseMonthDays",
    "AverageDailyMeals",
    "HoursAllowed",
    "HoursRequested",
    "AmountPerDay",
    "TotalAnnualSalaryAmount",
    "WorkerStateLiability",
    "ManagerSalaries",
    "ManagerStateLiability",
    "TerminationPayStatePortion",
    "TotalStateAllocation",
    "Preload90",
    "AmountDue",
    "SourceFile",
    "Notes"
  ),
  "current_district_cafeteria_allocation.csv"
)

stop_if_rows(
  district_cafeteria_allocation |>
    count(DistrictCode) |>
    filter(n > 1),
  "current_district_cafeteria_allocation.csv contains duplicate district codes."
)

stop_if_rows(
  district_cafeteria_allocation |>
    filter(
      is.na(DistrictCode) |
        is.na(DistrictName) |
        DistrictName == "" |
        if_any(
          c(
            Meals,
            BaseMonthDays,
            AverageDailyMeals,
            HoursAllowed,
            HoursRequested,
            AmountPerDay,
            TotalAnnualSalaryAmount,
            WorkerStateLiability,
            ManagerSalaries,
            ManagerStateLiability,
            TerminationPayStatePortion,
            TotalStateAllocation,
            Preload90,
            AmountDue
          ),
          ~ is.na(.x) | .x < 0
        )
    ),
  "current_district_cafeteria_allocation.csv contains a missing or invalid value."
)

primary_districts <- lea_input |>
  filter(LEAType == "District", IncludeInStatewide) |>
  distinct(DistrictCode, DistrictName)

stop_if_rows(
  primary_districts |>
    anti_join(district_cafeteria_allocation, by = "DistrictCode"),
  paste0(
    "current_district_cafeteria_allocation.csv is missing a primary-scope ",
    "district."
  )
)

stop_if_rows(
  district_cafeteria_allocation |>
    anti_join(primary_districts, by = "DistrictCode"),
  paste0(
    "current_district_cafeteria_allocation.csv contains a non-primary or ",
    "non-district record."
  )
)


# JOIN SUPPLEMENTAL VALUES ------------------------------------------------------

lunch_indicator_requirements <- lea_input |>
  transmute(
    SchoolYear,
    DistrictCode,
    FoodServiceLunchIndicatorRequired =
      LEAType == "District" &
      IncludeInStatewide &
      UnitsTotal < 500
  )

current_school_input <- school_input |>
  left_join(
    school_supplement |>
      select(
        SchoolYear,
        DistrictCode,
        SchoolCode,
        HasSchoolLunchProgram
      ),
    by = c("SchoolYear", "DistrictCode", "SchoolCode")
  ) |>
  left_join(
    lunch_indicator_requirements,
    by = c("SchoolYear", "DistrictCode")
  ) |>
  arrange(DistrictName, desc(IsSchool), SchoolName)

school_lunch_counts <- current_school_input |>
  filter(IsSchool) |>
  summarise(
    FoodServiceLunchIndicatorRequired =
      first(FoodServiceLunchIndicatorRequired),
    SchoolLunchBuildingsExpected = if (
      first(FoodServiceLunchIndicatorRequired)
    ) {
      n()
    } else {
      0L
    },
    SchoolLunchIndicatorsCompleted = if (
      first(FoodServiceLunchIndicatorRequired)
    ) {
      sum(!is.na(HasSchoolLunchProgram))
    } else {
      0L
    },
    SchoolLunchIndicatorsMissing = if (
      first(FoodServiceLunchIndicatorRequired)
    ) {
      sum(is.na(HasSchoolLunchProgram))
    } else {
      0L
    },
    SchoolLunchBuildingCount = if (
      !first(FoodServiceLunchIndicatorRequired)
    ) {
      NA_real_
    } else if (any(is.na(HasSchoolLunchProgram))) {
      NA_real_
    } else {
      sum(HasSchoolLunchProgram)
    },
    .by = c(SchoolYear, DistrictCode)
  )

current_lea_input <- lea_input |>
  left_join(
    lea_supplement |>
      select(SchoolYear, DistrictCode, CustodialUnits),
    by = c("SchoolYear", "DistrictCode")
  ) |>
  left_join(
    school_lunch_counts,
    by = c("SchoolYear", "DistrictCode")
  ) |>
  arrange(DistrictName)


# INPUT QC ----------------------------------------------------------------------

current_input_qc <- tibble(
  Check = c(
    "Coded schools",
    "No-code vocational/program records",
    "Schools requiring lunch-program indicators",
    "Required lunch-program indicators completed",
    "Required lunch-program indicators missing",
    "Formula-relevant LEAs with incomplete lunch-program indicators",
    "Primary districts missing custodial units",
    "Formula-relevant LEAs missing school-lunch building counts",
    "District cafeteria allocation records",
    "Primary districts missing cafeteria allocations",
    "Non-primary records in cafeteria allocation",
    "FY26 district cafeteria total state allocation"
  ),
  Value = c(
    sum(current_school_input$IsSchool),
    sum(!current_school_input$IsSchool),
    sum(current_lea_input$SchoolLunchBuildingsExpected, na.rm = TRUE),
    sum(current_lea_input$SchoolLunchIndicatorsCompleted, na.rm = TRUE),
    sum(current_lea_input$SchoolLunchIndicatorsMissing, na.rm = TRUE),
    current_lea_input |>
      filter(
        FoodServiceLunchIndicatorRequired,
        SchoolLunchIndicatorsMissing > 0
      ) |>
      nrow(),
    current_lea_input |>
      filter(
        LEAType == "District",
        IncludeInStatewide,
        is.na(CustodialUnits)
      ) |>
      nrow(),
    current_lea_input |>
      filter(
        FoodServiceLunchIndicatorRequired,
        is.na(SchoolLunchBuildingCount)
      ) |>
      nrow(),
    nrow(district_cafeteria_allocation),
    primary_districts |>
      anti_join(district_cafeteria_allocation, by = "DistrictCode") |>
      nrow(),
    district_cafeteria_allocation |>
      anti_join(primary_districts, by = "DistrictCode") |>
      nrow(),
    sum(district_cafeteria_allocation$TotalStateAllocation)
  ),
  Status = c(
    "Info",
    "Info",
    "Info",
    "Info",
    "Needs data",
    "Needs data",
    "Needs data",
    "Needs data",
    "Info",
    "Needs data",
    "Needs review",
    "Info"
  ),
  Treatment = c(
    "Used in school-based current-model formulas.",
    "Retain vocational positions and deductions only.",
    paste(
      "Indicators are required only for primary-scope districts",
      "with fewer than 500 Division I units."
    ),
    "Completed required 0/1 indicators are counted.",
    "Missing required indicators are not treated as zero.",
    paste(
      "The relevant LEA lunch-building count remains blank until",
      "all required school indicators are completed."
    ),
    paste(
      "Custodial units remain required only to evaluate current",
      "Buildings and Grounds Supervisor eligibility."
    ),
    paste(
      "Food Services Supervisor remains incomplete when the relevant",
      "lunch-program building count is missing."
    ),
    paste(
      "Reference records are retained for all 19 primary-scope districts",
      "but do not enter the position-based staffing estimate."
    ),
    "All primary-scope districts should appear in the FY26 reference file.",
    paste(
      "DAFB, charters, and other non-primary records should not appear",
      "in the district cafeteria allocation reference."
    ),
    paste(
      "Documented outside-formula salary allocation from FY26 Cafeteria.xlsx;",
      "not added to the position-based comparison."
    )
  )
) |>
  mutate(
    Status = case_when(
      Check %in% c(
        "Coded schools",
        "No-code vocational/program records",
        "Schools requiring lunch-program indicators",
        "Required lunch-program indicators completed",
        "District cafeteria allocation records",
        "FY26 district cafeteria total state allocation"
      ) ~ "Info",
      Value == 0 ~ "Pass",
      TRUE ~ Status
    )
  )


write_model_csv(current_school_input, current_school_input_path)
write_model_csv(current_lea_input, current_lea_input_path)
write_model_csv(
  district_cafeteria_allocation,
  current_district_cafeteria_output_path
)
write_review_csv(current_input_qc, current_input_qc_path)

message("Created current school input: ", current_school_input_path)
message("Created current LEA input: ", current_lea_input_path)
message(
  "Created district cafeteria reference output: ",
  current_district_cafeteria_output_path
)
message("Review current input QC: ", current_input_qc_path)
