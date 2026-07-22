# =============================================================================
# 03_prepare_current_inputs.R
# =============================================================================
# Adds the manually maintained current-model inputs to the shared model input.
# No funding formulas are calculated in this script.
#
# Main outputs:
#   03_current_school_input.csv
#   03_current_lea_input.csv
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
current_input_qc_path <- file.path(
  output_dir,
  "03_current_input_qc.csv"
)

check_required_files(c(shared_input_path, lea_crosswalk_path))


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
    CustodianPositions = NA_real_,
    SatelliteCafeteriaCount = NA_real_,
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
    CustodianPositions = as.numeric(CustodianPositions),
    SatelliteCafeteriaCount = as.numeric(SatelliteCafeteriaCount),
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
      (!is.na(CustodianPositions) & CustodianPositions < 0) |
        (!is.na(SatelliteCafeteriaCount) &
          SatelliteCafeteriaCount < 0) |
        (!is.na(HasSchoolLunchProgram) &
          !HasSchoolLunchProgram %in% c(0, 1))
    ),
  "The school supplement contains an invalid value."
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


# JOIN SUPPLEMENTAL VALUES ------------------------------------------------------

lunch_indicator_requirements <- lea_input |>
  transmute(
    SchoolYear,
    DistrictCode,
    FoodServiceLunchIndicatorRequired =
      LEAType %in% c("District", "Dover Air Force Base") &
      UnitsTotal < 500
  )

current_school_input <- school_input |>
  left_join(
    school_supplement |>
      select(
        SchoolYear,
        DistrictCode,
        SchoolCode,
        CustodianPositions,
        SatelliteCafeteriaCount,
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
    "Schools missing custodian positions",
    "Charters missing satellite cafeteria counts",
    "Schools requiring lunch-program indicators",
    "Required lunch-program indicators completed",
    "Required lunch-program indicators missing",
    "Formula-relevant LEAs with incomplete lunch-program indicators",
    "LEAs missing custodial units",
    "Formula-relevant LEAs missing school-lunch building counts"
  ),
  Value = c(
    sum(current_school_input$IsSchool),
    sum(!current_school_input$IsSchool),
    current_school_input |>
      filter(IsSchool, is.na(CustodianPositions)) |>
      nrow(),
    current_school_input |>
      filter(
        IsSchool,
        LEAType == "Charter",
        is.na(SatelliteCafeteriaCount)
      ) |>
      nrow(),
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
      filter(is.na(CustodialUnits)) |>
      nrow(),
    current_lea_input |>
      filter(
        FoodServiceLunchIndicatorRequired,
        is.na(SchoolLunchBuildingCount)
      ) |>
      nrow()
  ),
  Status = c(
    "Info",
    "Info",
    "Needs data",
    "Needs review",
    "Info",
    "Info",
    "Needs data",
    "Needs data",
    "Needs data",
    "Needs data"
  ),
  Treatment = c(
    "Used in school-based current-model formulas.",
    "Retain vocational positions and deductions only.",
    "Custodian positions remain missing.",
    "A provisional base 0.73 cafeteria-manager quantity is shown but marked incomplete.",
    paste(
      "Indicators are required only for district or DAFB LEAs",
      "with fewer than 500 units."
    ),
    "Completed required 0/1 indicators are counted.",
    "Missing required indicators are not treated as zero.",
    paste(
      "The relevant LEA lunch-building count remains blank until",
      "all required school indicators are completed."
    ),
    "Buildings and Grounds Supervisor remains missing.",
    "Food Services Supervisor remains incomplete when the relevant count is missing."
  )
) |>
  mutate(
    Status = case_when(
      Check %in% c(
        "Coded schools",
        "No-code vocational/program records",
        "Schools requiring lunch-program indicators",
        "Required lunch-program indicators completed"
      ) ~ "Info",
      Value == 0 ~ "Pass",
      Check == "Charters missing satellite cafeteria counts" ~
        "Needs review",
      TRUE ~ "Needs data"
    )
  )


write_model_csv(current_school_input, current_school_input_path)
write_model_csv(current_lea_input, current_lea_input_path)
write_review_csv(current_input_qc, current_input_qc_path)

message("Created current school input: ", current_school_input_path)
message("Created current LEA input: ", current_lea_input_path)
message("Review current input QC: ", current_input_qc_path)
