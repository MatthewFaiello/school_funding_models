# =============================================================================
# 02_build_shared_input.R
# =============================================================================
# Combines the unit-count workbook with the SQL student-count export.
#
# Main output:
#   02_shared_model_input.csv
#
# Review outputs:
#   02_shared_input_qc_summary.csv
#   02_shared_input_qc_detail.csv
# =============================================================================

source(file.path("scripts", "00_settings.R"))

shared_input_path <- file.path(output_dir, "02_shared_model_input.csv")
shared_qc_summary_path <- file.path(
  output_dir,
  "02_shared_input_qc_summary.csv"
)
shared_qc_detail_path <- file.path(
  output_dir,
  "02_shared_input_qc_detail.csv"
)

check_required_files(c(unit_count_path, student_counts_path, entity_crosswalk_path))


# READ VISIBLE NAME CROSSWALK ---------------------------------------------------

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

student_name_crosswalk <- entity_crosswalk |>
  filter(CrosswalkType == "Student counts to shared input")

stop_if_rows(
  student_name_crosswalk |>
    count(DistrictCode, SchoolCode) |>
    filter(n > 1),
  "entity_crosswalk.csv contains duplicate student-count school keys."
)


# READ UNIT-COUNT WORKBOOK ------------------------------------------------------

# EnrollmentPreK and UnitsPreK are treated as Basic Pre-K special education.
# Intensive and Complex Pre-K remain in their separate Pre-K-12 categories.

workbook_columns <- c(
  "SourceDistrictName",
  "SourceSchoolName",
  "EnrollmentPreK",
  "EnrollmentK3",
  "Enrollment4_12",
  "EnrollmentBasicK12",
  "EnrollmentIntense",
  "EnrollmentComplex",
  "EnrollmentTotal",
  "UnitsPreK",
  "UnitsK3",
  "Units4_12",
  "UnitsBasicK12",
  "UnitsIntense",
  "UnitsComplex",
  "UnitsVocational",
  "UnitsVocationalDeduct",
  "UnitsTotal"
)

workbook_numeric_columns <- workbook_columns[3:length(workbook_columns)]

unit_count <- read_excel(
  unit_count_path,
  sheet = 1,
  skip = 2,
  col_names = workbook_columns,
  col_types = c("text", "text", rep("numeric", 16))
) |>
  mutate(SourceExcelRow = row_number() + 2L)

add_enrollment_totals <- function(data) {
  data |>
    rename(BaseUnitEnrollmentTotal = EnrollmentTotal) |>
    mutate(
      RegularEdEnrollment = EnrollmentK3 + Enrollment4_12,
      SpecialEdEnrollment =
        EnrollmentPreK +
        EnrollmentBasicK12 +
        EnrollmentIntense +
        EnrollmentComplex
    )
}


# SEPARATE SCHOOL, LEA, AND STATE ROWS -----------------------------------------

state_base <- unit_count |>
  filter(str_starts(SourceDistrictName, "State Total")) |>
  transmute(
    AggregationLevel = "Statewide",
    SchoolYear = school_year,
    CountDate = count_date,
    SourceExcelRow,
    SourceDistrictName,
    SourceSchoolName,
    across(all_of(workbook_numeric_columns))
  ) |>
  add_enrollment_totals()

district_base <- unit_count |>
  filter(
    SourceSchoolName == "District Totals",
    !str_starts(SourceDistrictName, "State Total")
  ) |>
  transmute(
    AggregationLevel = "District",
    SchoolYear = school_year,
    CountDate = count_date,
    SourceExcelRow,
    SourceDistrictName,
    SourceSchoolName,
    across(all_of(workbook_numeric_columns))
  ) |>
  add_enrollment_totals()

school_source <- unit_count |>
  filter(
    SourceSchoolName != "District Totals",
    !str_starts(SourceDistrictName, "State Total")
  )


# RESOLVE REPEATED NCCVT ROWS ---------------------------------------------------

# Repeated source rows must agree on enrollment and nonvocational units.
# The row with the smallest absolute vocational values is retained.

nonvocational_columns <- c(
  "EnrollmentPreK",
  "EnrollmentK3",
  "Enrollment4_12",
  "EnrollmentBasicK12",
  "EnrollmentIntense",
  "EnrollmentComplex",
  "EnrollmentTotal",
  "UnitsPreK",
  "UnitsK3",
  "Units4_12",
  "UnitsBasicK12",
  "UnitsIntense",
  "UnitsComplex"
)

duplicate_conflicts <- school_source |>
  distinct(
    SourceDistrictName,
    SourceSchoolName,
    across(all_of(nonvocational_columns))
  ) |>
  count(SourceDistrictName, SourceSchoolName, name = "DistinctBaseRows") |>
  left_join(
    school_source |>
      count(SourceDistrictName, SourceSchoolName, name = "SourceRowCount"),
    by = c("SourceDistrictName", "SourceSchoolName")
  ) |>
  filter(SourceRowCount > 1, DistinctBaseRows > 1)

stop_if_rows(
  duplicate_conflicts,
  "Repeated school rows contain conflicting enrollment or nonvocational values."
)

school_base <- school_source |>
  add_count(SourceDistrictName, SourceSchoolName, name = "SourceRowCount") |>
  arrange(
    SourceDistrictName,
    SourceSchoolName,
    abs(UnitsVocational),
    abs(UnitsVocationalDeduct),
    SourceExcelRow
  ) |>
  distinct(SourceDistrictName, SourceSchoolName, .keep_all = TRUE) |>
  transmute(
    AggregationLevel = "School",
    SchoolYear = school_year,
    CountDate = count_date,
    SourceExcelRow,
    SourceRowCount,
    SelectionRule = case_when(
      SourceRowCount > 1 ~ "Selected lowest absolute vocational row",
      TRUE ~ "Only source row"
    ),
    SourceDistrictName,
    SourceSchoolName,
    across(all_of(workbook_numeric_columns))
  ) |>
  add_enrollment_totals()


# READ SQL STUDENT COUNTS -------------------------------------------------------

student_count_columns <- c(
  "Enrollment",
  "LI",
  "MLL",
  "K8Enrollment",
  "Grade10Enrollment"
)

student_counts <- read_csv(student_counts_path, show_col_types = FALSE)

check_required_columns(
  student_counts,
  c(
    "AggregationLevel",
    "SchoolYear",
    "CountDate",
    "DistrictCode",
    "DistrictName",
    "SchoolCode",
    "SchoolName",
    student_count_columns
  ),
  "student_counts.csv"
)

student_counts <- student_counts |>
  filter(
    AggregationLevel == "School",
    SchoolYear == school_year,
    as.Date(CountDate) == count_date
  ) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    across(all_of(student_count_columns), as.numeric)
  ) |>
  left_join(
    student_name_crosswalk |>
      select(
        DistrictCode,
        SchoolCode,
        SharedSchoolName,
        ExternalSchoolName
      ),
    by = c("DistrictCode", "SchoolCode")
  ) |>
  mutate(
    JoinDistrictName = normalize_name(DistrictName),
    JoinSchoolName = normalize_name(
      coalesce(SharedSchoolName, SchoolName)
    )
  )

stop_if_rows(
  student_counts |>
    filter(
      !is.na(ExternalSchoolName),
      normalize_name(SchoolName) != normalize_name(ExternalSchoolName)
    ) |>
    select(
      DistrictCode,
      SchoolCode,
      SchoolName,
      ExternalSchoolName,
      SharedSchoolName
    ),
  paste0(
    "A student-count name no longer matches entity_crosswalk.csv. ",
    "Review the source name and crosswalk."
  )
)

stop_if_rows(
  student_name_crosswalk |>
    anti_join(
      student_counts |>
        distinct(DistrictCode, SchoolCode),
      by = c("DistrictCode", "SchoolCode")
    ),
  "entity_crosswalk.csv contains a stale student-count school key."
)

stop_if_rows(
  student_counts |>
    count(DistrictCode, SchoolCode) |>
    filter(n > 1),
  "student_counts.csv contains duplicate school rows."
)

stop_if_rows(
  student_counts |>
    filter(
      if_any(all_of(student_count_columns), ~ is.na(.x) | .x < 0) |
        LI > Enrollment |
        MLL > Enrollment |
        K8Enrollment > Enrollment |
        Grade10Enrollment > Enrollment |
        K8Enrollment + Grade10Enrollment > Enrollment
    ),
  "student_counts.csv contains missing or invalid values."
)


# JOIN SCHOOL COUNTS TO WORKBOOK ROWS ------------------------------------------

district_lookup <- student_counts |>
  distinct(JoinDistrictName, DistrictCode)

stop_if_rows(
  district_lookup |>
    count(JoinDistrictName) |>
    filter(n > 1),
  "A district name maps to more than one district code."
)

school_model <- school_base |>
  mutate(
    JoinDistrictName = normalize_name(SourceDistrictName),
    JoinSchoolName = normalize_name(SourceSchoolName)
  ) |>
  left_join(
    student_counts |>
      select(
        JoinDistrictName,
        JoinSchoolName,
        StudentDistrictCode = DistrictCode,
        SchoolCode,
        all_of(student_count_columns)
      ),
    by = c("JoinDistrictName", "JoinSchoolName")
  ) |>
  left_join(
    district_lookup |>
      rename(WorkbookDistrictCode = DistrictCode),
    by = "JoinDistrictName"
  ) |>
  mutate(
    DistrictCode = coalesce(StudentDistrictCode, WorkbookDistrictCode),
    across(
      all_of(student_count_columns),
      ~ case_when(
        !is.na(.x) ~ .x,
        BaseUnitEnrollmentTotal == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  )

stop_if_rows(
  school_model |>
    filter(BaseUnitEnrollmentTotal > 0, is.na(SchoolCode)) |>
    select(SourceDistrictName, SourceSchoolName, BaseUnitEnrollmentTotal),
  "A positive-enrollment workbook school did not match student_counts.csv."
)

stop_if_rows(
  student_counts |>
    anti_join(
      school_model,
      by = c("JoinDistrictName", "JoinSchoolName")
    ) |>
    select(DistrictCode, DistrictName, SchoolCode, SchoolName, Enrollment),
  "A student-count school did not match the unit-count workbook."
)

stop_if_rows(
  school_model |>
    filter(abs(Enrollment - BaseUnitEnrollmentTotal) > 1e-8) |>
    select(
      SourceDistrictName,
      SourceSchoolName,
      BaseUnitEnrollmentTotal,
      Enrollment
    ),
  "School enrollment does not match the workbook enrollment total."
)


# CREATE LEA AND STATE STUDENT COUNTS ------------------------------------------

district_student_counts <- student_counts |>
  summarise(
    across(all_of(student_count_columns), ~ sum(.x, na.rm = TRUE)),
    .by = c(DistrictCode, DistrictName, JoinDistrictName)
  )

district_model <- district_base |>
  mutate(JoinDistrictName = normalize_name(SourceDistrictName)) |>
  left_join(district_student_counts, by = "JoinDistrictName")

stop_if_rows(
  district_model |>
    filter(
      is.na(Enrollment) |
        abs(Enrollment - BaseUnitEnrollmentTotal) > 1e-8
    ) |>
    select(SourceDistrictName, BaseUnitEnrollmentTotal, Enrollment),
  "LEA enrollment does not match the workbook LEA total."
)

state_student_counts <- bind_rows(
  student_counts |>
    summarise(across(all_of(student_count_columns), ~ sum(.x, na.rm = TRUE))) |>
    mutate(
      JoinDistrictName =
        normalize_name("State Total Including Dover Air Force Base")
    ),
  student_counts |>
    filter(DistrictCode != dafb_district_code) |>
    summarise(across(all_of(student_count_columns), ~ sum(.x, na.rm = TRUE))) |>
    mutate(
      JoinDistrictName =
        normalize_name("State Total Excluding Dover Air Force Base")
    )
)

state_model <- state_base |>
  mutate(JoinDistrictName = normalize_name(SourceDistrictName)) |>
  left_join(state_student_counts, by = "JoinDistrictName")

stop_if_rows(
  state_model |>
    filter(
      is.na(Enrollment) |
        abs(Enrollment - BaseUnitEnrollmentTotal) > 1e-8
    ) |>
    select(SourceDistrictName, BaseUnitEnrollmentTotal, Enrollment),
  "Statewide enrollment does not match the workbook statewide total."
)


# BUILD SHARED MODEL INPUT ------------------------------------------------------

model_value_columns <- c(
  "EnrollmentPreK",
  "EnrollmentK3",
  "Enrollment4_12",
  "EnrollmentBasicK12",
  "EnrollmentIntense",
  "EnrollmentComplex",
  "RegularEdEnrollment",
  "SpecialEdEnrollment",
  "BaseUnitEnrollmentTotal",
  "Enrollment",
  "LI",
  "MLL",
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
)

school_output <- school_model |>
  transmute(
    AggregationLevel,
    RecordType = case_when(
      !is.na(SchoolCode) ~ "School",
      TRUE ~ "Vocational/program record"
    ),
    SchoolYear,
    CountDate,
    DistrictCode = as.integer(DistrictCode),
    DistrictName = SourceDistrictName,
    SchoolCode = as.integer(SchoolCode),
    SchoolName = SourceSchoolName,
    IsSchool = !is.na(SchoolCode),
    SourceExcelRow,
    SourceRowCount,
    SelectionRule,
    across(all_of(model_value_columns))
  )

district_output <- district_model |>
  transmute(
    AggregationLevel,
    RecordType = "LEA total",
    SchoolYear,
    CountDate,
    DistrictCode = as.integer(DistrictCode),
    DistrictName = SourceDistrictName,
    SchoolCode = NA_integer_,
    SchoolName = SourceSchoolName,
    IsSchool = FALSE,
    SourceExcelRow,
    SourceRowCount = NA_integer_,
    SelectionRule = NA_character_,
    across(all_of(model_value_columns))
  )

state_output <- state_model |>
  transmute(
    AggregationLevel,
    RecordType = "Statewide total",
    SchoolYear,
    CountDate,
    DistrictCode = NA_integer_,
    DistrictName = SourceDistrictName,
    SchoolCode = NA_integer_,
    SchoolName = SourceSchoolName,
    IsSchool = FALSE,
    SourceExcelRow,
    SourceRowCount = NA_integer_,
    SelectionRule = NA_character_,
    across(all_of(model_value_columns))
  )

shared_model_input <- bind_rows(
  school_output,
  district_output,
  state_output
) |>
  arrange(
    factor(
      AggregationLevel,
      levels = c("School", "District", "Statewide")
    ),
    DistrictName,
    SchoolName
  )

stop_if_rows(
  shared_model_input |>
    filter(
      abs(
        RegularEdEnrollment +
          SpecialEdEnrollment -
          BaseUnitEnrollmentTotal
      ) > 1e-8 |
        abs(Enrollment - BaseUnitEnrollmentTotal) > 1e-8 |
        if_any(
          all_of(student_count_columns),
          ~ is.na(.x) | .x < 0 | .x > Enrollment
        ) |
        K8Enrollment + Grade10Enrollment > Enrollment
    ),
  "The shared model input contains invalid enrollment values."
)

stop_if_rows(
  shared_model_input |>
    filter(
      (AggregationLevel == "School" & IsSchool != !is.na(SchoolCode)) |
        (AggregationLevel != "School" & IsSchool)
    ),
  "The shared model input contains an invalid IsSchool flag."
)


# ROLLUP QC ---------------------------------------------------------------------

school_rollup <- school_output |>
  summarise(
    across(all_of(model_value_columns), ~ sum(.x, na.rm = TRUE)),
    .by = DistrictName
  )

district_comparison <- district_output |>
  select(DistrictName, all_of(model_value_columns)) |>
  pivot_longer(
    -DistrictName,
    names_to = "Variable",
    values_to = "ReportedValue"
  ) |>
  left_join(
    school_rollup |>
      pivot_longer(
        -DistrictName,
        names_to = "Variable",
        values_to = "RolledValue"
      ),
    by = c("DistrictName", "Variable")
  ) |>
  mutate(
    CheckLevel = "School to LEA",
    Difference = RolledValue - ReportedValue,
    Tolerance = case_when(
      str_starts(Variable, "Units") ~ district_unit_tolerance,
      TRUE ~ 0
    ),
    Status = case_when(
      abs(Difference) <= Tolerance + 1e-8 ~ "Pass",
      TRUE ~ "Review"
    ),
    .before = 1
  )

state_rollup <- bind_rows(
  district_output |>
    summarise(across(all_of(model_value_columns), ~ sum(.x, na.rm = TRUE))) |>
    mutate(DistrictName = "State Total Including Dover Air Force Base"),
  district_output |>
    filter(DistrictCode != dafb_district_code) |>
    summarise(across(all_of(model_value_columns), ~ sum(.x, na.rm = TRUE))) |>
    mutate(DistrictName = "State Total Excluding Dover Air Force Base")
)

state_comparison <- state_output |>
  select(DistrictName, all_of(model_value_columns)) |>
  pivot_longer(
    -DistrictName,
    names_to = "Variable",
    values_to = "ReportedValue"
  ) |>
  left_join(
    state_rollup |>
      pivot_longer(
        -DistrictName,
        names_to = "Variable",
        values_to = "RolledValue"
      ),
    by = c("DistrictName", "Variable")
  ) |>
  mutate(
    CheckLevel = "LEA to State",
    Difference = RolledValue - ReportedValue,
    Tolerance = case_when(
      Variable == "UnitsTotal" ~ state_units_total_tolerance,
      str_starts(Variable, "Units") ~ district_unit_tolerance,
      TRUE ~ 0
    ),
    Status = case_when(
      abs(Difference) <= Tolerance + 1e-8 ~ "Pass",
      TRUE ~ "Review"
    ),
    .before = 1
  )

shared_input_qc_detail <- bind_rows(
  district_comparison,
  state_comparison
) |>
  arrange(CheckLevel, DistrictName, Variable)

shared_input_qc_summary <- tibble(
  Check = c(
    "School/program rows",
    "Coded school rows",
    "No-code vocational/program rows",
    "LEA rows",
    "Statewide rows",
    "School-to-LEA checks requiring review",
    "LEA-to-state checks requiring review",
    "Statewide enrollment excluding DAFB",
    "Statewide enrollment including DAFB"
  ),
  Value = c(
    nrow(school_output),
    sum(school_output$IsSchool),
    sum(!school_output$IsSchool),
    nrow(district_output),
    nrow(state_output),
    sum(district_comparison$Status == "Review"),
    sum(state_comparison$Status == "Review"),
    state_output |>
      filter(str_detect(DistrictName, "Excluding")) |>
      pull(Enrollment),
    state_output |>
      filter(str_detect(DistrictName, "Including")) |>
      pull(Enrollment)
  ),
  Status = c(
    rep("Info", 5),
    if_else(any(district_comparison$Status == "Review"), "Review", "Pass"),
    if_else(any(state_comparison$Status == "Review"), "Review", "Pass"),
    "Info",
    "Info"
  )
)

write_model_csv(shared_model_input, shared_input_path)
write_review_csv(shared_input_qc_summary, shared_qc_summary_path)
write_review_csv(shared_input_qc_detail, shared_qc_detail_path)

stop_if_rows(
  shared_input_qc_detail |>
    filter(Status == "Review"),
  "A shared-input rollup exceeds its allowed tolerance."
)

message("Created shared input: ", shared_input_path)
message("Review QC summary: ", shared_qc_summary_path)
message("Review QC detail: ", shared_qc_detail_path)
