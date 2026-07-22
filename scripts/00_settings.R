# =============================================================================
# 00_settings.R
# =============================================================================
# Edit this file when the school year, source files, model options, or funding
# pools change. All other R scripts read their settings from this file.
# =============================================================================

# PROJECT FOLDERS ---------------------------------------------------------------

project_dir <- normalizePath(".", winslash = "/", mustWork = FALSE)
scripts_dir <- file.path(project_dir, "scripts")
input_dir <- file.path(project_dir, "data", "input")
output_dir <- file.path(project_dir, "data", "output")

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# PACKAGES ----------------------------------------------------------------------

required_packages <- c("tidyverse", "readxl")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  stop(
    "Install the missing R package(s) before running the pipeline: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  invisible(lapply(required_packages, library, character.only = TRUE))
)


# RUN SETTINGS ------------------------------------------------------------------

school_year <- 2026L
count_date <- as.Date("2025-09-30")
dafb_district_code <- 14L

# Choose one:
#   "total"      = total enrollment
#   "regular_ed" = regular education enrollment only
operational_enrollment_basis <- "total"

# Choose one:
#   "provided"     = use the weighted rates in funding_rates.csv
#   "recalculated" = divide each statewide pool by its weighted count
weighted_rate_method <- "recalculated"

opportunity_funding_pool <- 163000000
operational_funding_pool <- 279026800

district_unit_tolerance <- 0.05
state_units_total_tolerance <- 0.10

charter_integer_allocation_categories <- c(
  "EnrollmentK3",
  "Enrollment4_12",
  "BasicPreK12Enrollment",
  "EnrollmentIntense",
  "EnrollmentComplex"
)

charter_student_allocation_method <- paste(
  "Student enrollment categories use largest remainder",
  "with ties resolved by CalculationUnitSequence;",
  "vocational units and deductions retain full precision"
)

charter_building_policy <- paste(
  "Districts use one school code per calculation unit;",
  "charters use calculator building rows and retain the shared charter totals;",
  "student enrollment categories use largest remainder"
)


# INPUT FILES -------------------------------------------------------------------

unit_count_path <- file.path(input_dir, "unit_count.xlsx")
student_counts_path <- file.path(input_dir, "student_counts.csv")
calculator_path <- file.path(input_dir, "proposed_calculator.xlsm")
funding_rates_path <- file.path(input_dir, "funding_rates.csv")
lea_crosswalk_path <- file.path(input_dir, "lea_crosswalk.csv")
entity_crosswalk_path <- file.path(input_dir, "entity_crosswalk.csv")
current_rate_map_path <- file.path(input_dir, "current_rate_map.csv")
current_school_supplement_path <- file.path(
  input_dir,
  "current_school_supplement.csv"
)
current_lea_supplement_path <- file.path(
  input_dir,
  "current_lea_supplement.csv"
)
proposed_manual_allocations_path <- file.path(
  input_dir,
  "proposed_charter_manual_allocations.csv"
)


# OUTPUT FILES ------------------------------------------------------------------

# Each numbered script defines its own output filenames near the top. This keeps
# the inputs and outputs for that step visible in the same file.


# VALIDATE SETTINGS -------------------------------------------------------------

valid_operational_bases <- c("total", "regular_ed")
valid_rate_methods <- c("provided", "recalculated")

if (!operational_enrollment_basis %in% valid_operational_bases) {
  stop(
    "operational_enrollment_basis must be total or regular_ed.",
    call. = FALSE
  )
}

if (!weighted_rate_method %in% valid_rate_methods) {
  stop(
    "weighted_rate_method must be provided or recalculated.",
    call. = FALSE
  )
}


# SMALL GENERAL HELPERS ---------------------------------------------------------

normalize_name <- function(x) {
  x |>
    str_replace_all("_", " ") |>
    str_replace_all("&", " and ") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

stop_if_rows <- function(data, message_text) {
  if (nrow(data) > 0) {
    print(data)
    stop(message_text, call. = FALSE)
  }
}

check_required_columns <- function(data, required_columns, file_label) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      file_label,
      " is missing: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

check_required_files <- function(paths) {
  missing_paths <- paths[!file.exists(paths)]

  if (length(missing_paths) > 0) {
    stop(
      "Missing required input file(s):\n",
      paste(missing_paths, collapse = "\n"),
      call. = FALSE
    )
  }
}


# OUTPUT HELPERS ----------------------------------------------------------------

# Model files retain full numeric precision. No rounding is applied unless it is
# explicitly part of a documented business rule, such as floor-based thresholds.
write_model_csv <- function(data, path) {
  write_csv(data, path, na = "")
}

# Review files are terminal presentation outputs. Rounding here never feeds back
# into a later model calculation.
round_review_output <- function(data) {
  is_review_number <- function(x) {
    is.double(x) && !inherits(x, c("Date", "POSIXct", "POSIXt"))
  }

  data |>
    mutate(
      across(
        where(is_review_number),
        ~ {
          column_name <- cur_column()

          digits <- case_when(
            str_detect(column_name, "Rate|Percent") ~ 6,
            str_detect(column_name, "Funding|Amount|Difference") ~ 2,
            TRUE ~ 6
          )

          round(.x, digits)
        }
      )
    )
}

write_review_csv <- function(data, path) {
  write_csv(round_review_output(data), path, na = "")
}
