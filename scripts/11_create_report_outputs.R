# =============================================================================
# 11_create_report_outputs.R
# =============================================================================
# Creates report-specific tables, figure datasets, and audit crosswalks.
#
# Step 11 does not change the model calculations established in Steps 01-10.
# It reads validated pipeline outputs and the PEFC calculator, extracts the
# calculator as presented, and creates reproducible three-way comparisons among:
#   1. Recreated Current Model
#   2. PEFC Proposed Model as Presented
#   3. Independent Technical Review and Reproduction of the Proposed Model
# =============================================================================

source(file.path("scripts", "00_settings.R"))

step11_script_version <- "2026-07-23-v3"
message("Step 11 reporting script version: ", step11_script_version)


# SETTINGS ----------------------------------------------------------------------

current_model_name <- "Recreated Current Model"
pefc_model_name <- "PEFC Proposed Model as Presented"
reproduction_model_name <- paste(
  "Independent Technical Review and Reproduction",
  "of the Proposed Model"
)

excluded_lea_code <- 9615L
excluded_lea_name <- "Bryan Allen Stevenson School of Excellence"
final_scope_label <- paste("Excludes DAFB and", excluded_lea_name)
pefc_scope_label <- paste(
  "PEFC calculator as presented; includes DAFB and Bryan Allen Stevenson",
  "unless a specific calculator formula excludes an entity"
)

base_section <- "Base Funding (State Support)"
central_section <- "Central Office Funding (State Support)"
opportunity_section <- "Opportunity Funding (State Support)"
operational_section <- "Operational Funding (State Support)"
weighted_sections <- c(opportunity_section, operational_section)

comparison_tolerance <- 0.01
reconciliation_tolerance <- 0.01


# FILES -------------------------------------------------------------------------

input_paths <- c(
  run_settings = file.path(output_dir, "00_run_settings.csv"),
  shared = file.path(output_dir, "02_shared_model_input.csv"),
  current_issues = file.path(output_dir, "04_current_model_issues.csv"),
  current_rules = file.path(output_dir, "04_current_model_rules.csv"),
  current_detail = file.path(output_dir, "05_current_model_funding_detail.csv"),
  charter_reconciliation = file.path(output_dir, "06_proposed_charter_reconciliation.csv"),
  proposed_issues = file.path(output_dir, "07_proposed_model_issues.csv"),
  proposed_rules = file.path(output_dir, "07_proposed_model_rules.csv"),
  proposed_detail = file.path(output_dir, "08_proposed_model_funding_detail.csv"),
  proposed_state = file.path(output_dir, "08_proposed_model_state_summary.csv"),
  step08_rates = file.path(output_dir, "08_proposed_weighted_rate_summary.csv"),
  adjusted_lea = file.path(output_dir, "10_adjusted_lea_comparison.csv"),
  adjusted_state = file.path(output_dir, "10_adjusted_state_summary.csv"),
  adjusted_components = file.path(output_dir, "10_adjusted_weighted_component_summary.csv"),
  adjusted_rates = file.path(output_dir, "10_adjusted_weighted_rate_summary.csv"),
  exclusion_audit = file.path(output_dir, "10_exclusion_audit.csv"),
  lea_distribution = file.path(output_dir, "10_lea_distribution_summary.csv"),
  lea_type = file.path(output_dir, "10_lea_type_summary.csv"),
  report_summary = file.path(output_dir, "10_report_summary.csv"),
  reporting_qc = file.path(output_dir, "10_reporting_qc.csv"),
  component_crosswalk = report_component_crosswalk_path,
  calculator = calculator_path,
  lea_crosswalk = lea_crosswalk_path,
  funding_rates = funding_rates_path,
  current_rate_map = current_rate_map_path
)
check_required_files(input_paths)

output_paths <- c(
  analysis_version = file.path(output_dir, "11_analysis_version.csv"),
  model_definitions = file.path(output_dir, "11_model_version_definitions.csv"),
  rounding_rules = file.path(output_dir, "11_report_rounding_rules.csv"),
  pefc_statewide = file.path(output_dir, "11_pefc_statewide_as_presented.csv"),
  pefc_component = file.path(output_dir, "11_pefc_component_detail_as_presented.csv"),
  pefc_lea = file.path(output_dir, "11_pefc_lea_as_presented.csv"),
  pefc_reconciliation = file.path(output_dir, "11_pefc_internal_reconciliation.csv"),
  input_comparison = file.path(output_dir, "11_three_way_input_comparison.csv"),
  position_comparison = file.path(output_dir, "11_three_way_position_comparison.csv"),
  funding_comparison = file.path(output_dir, "11_three_way_funding_comparison.csv"),
  rate_comparison = file.path(output_dir, "11_three_way_rate_comparison.csv"),
  pefc_reproduction_differences = file.path(
    output_dir,
    "11_pefc_vs_reproduction_differences.csv"
  ),
  difference_drivers = file.path(output_dir, "11_difference_driver_summary.csv"),
  scope_rate_bridge = file.path(output_dir, "11_scope_and_rate_bridge.csv"),
  pefc_scope_normalized = file.path(
    output_dir,
    "11_pefc_scope_normalized_comparison.csv"
  ),
  position_crosswalk = file.path(
    output_dir,
    "11_current_vs_reproduction_position_crosswalk.csv"
  ),
  key_metrics = file.path(output_dir, "11_key_report_metrics.csv"),
  statewide_report = file.path(output_dir, "11_statewide_summary_for_report.csv"),
  lea_report = file.path(output_dir, "11_lea_summary_for_report.csv"),
  lea_distribution_report = file.path(output_dir, "11_lea_distribution_for_report.csv"),
  lea_type_report = file.path(output_dir, "11_lea_type_for_report.csv"),
  figure_funding = file.path(output_dir, "11_figure_statewide_funding_sections.csv"),
  figure_positions = file.path(output_dir, "11_figure_position_comparison.csv"),
  figure_lea = file.path(output_dir, "11_figure_lea_change_by_type.csv"),
  figure_drivers = file.path(output_dir, "11_figure_difference_drivers.csv"),
  decision_log = file.path(output_dir, "11_decision_discrepancy_log.csv"),
  exhibit_registry = file.path(output_dir, "11_exhibit_registry.csv"),
  report_qc = file.path(output_dir, "11_report_qc.csv")
)


# HELPERS -----------------------------------------------------------------------

sum_preserve_missing <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

all_true_or_false <- function(x) {
  all(coalesce(as.logical(x), FALSE))
}

percent_difference <- function(new_amount, old_amount) {
  case_when(
    is.na(new_amount) | is.na(old_amount) ~ NA_real_,
    abs(old_amount) < 1e-8 ~ NA_real_,
    TRUE ~ 100 * (new_amount - old_amount) / old_amount
  )
}

cell_text <- function(data, row, column) {
  value <- data[[column]][row]

  if (length(value) == 0 || is.na(value)) {
    return(NA_character_)
  }

  as.character(value)
}

cell_number <- function(data, row, column) {
  value <- cell_text(data, row, column)

  if (is.na(value) || str_squish(value) == "") {
    return(NA_real_)
  }

  suppressWarnings(parse_double(value, na = c("", "NA", "N/A")))
}

first_value <- function(data, column) {
  data[[column]][[1]]
}

metric_value <- function(data, metric_name) {
  values <- data |>
    filter(Metric == metric_name) |>
    pull(Value)

  if (length(values) != 1) {
    stop(
      "Expected exactly one PEFC statewide metric named: ",
      metric_name,
      call. = FALSE
    )
  }

  values[[1]]
}

status_from_complete <- function(is_complete, partial_label = "Incomplete") {
  if_else(coalesce(is_complete, FALSE), "Complete", partial_label)
}

safe_git_value <- function(arguments) {
  result <- tryCatch(
    suppressWarnings(
      system2("git", arguments, stdout = TRUE, stderr = TRUE)
    ),
    error = function(e) character()
  )

  command_status <- attr(result, "status")

  if (
    length(result) == 0 ||
    (!is.null(command_status) && command_status != 0)
  ) {
    return(NA_character_)
  }

  str_squish(result[[1]])
}

build_long_metric <- function(
    group,
    metric,
    value,
    unit,
    source_sheet,
    source_cell,
    note = NA_character_) {
  tibble(
    MetricGroup = group,
    Metric = metric,
    Value = as.numeric(value),
    Unit = unit,
    SourceWorkbook = basename(calculator_path),
    SourceSheet = source_sheet,
    SourceCell = source_cell,
    ReportingScope = pefc_scope_label,
    Note = note
  )
}


# READ PIPELINE OUTPUTS ---------------------------------------------------------

run_settings <- read_csv(input_paths[["run_settings"]], show_col_types = FALSE)
shared <- read_csv(input_paths[["shared"]], show_col_types = FALSE)
current_issues <- read_csv(input_paths[["current_issues"]], show_col_types = FALSE)
current_rules <- read_csv(input_paths[["current_rules"]], show_col_types = FALSE)
current_detail <- read_csv(input_paths[["current_detail"]], show_col_types = FALSE)
charter_reconciliation <- read_csv(
  input_paths[["charter_reconciliation"]],
  show_col_types = FALSE
)
proposed_issues <- read_csv(input_paths[["proposed_issues"]], show_col_types = FALSE)
proposed_rules <- read_csv(input_paths[["proposed_rules"]], show_col_types = FALSE)
proposed_detail <- read_csv(input_paths[["proposed_detail"]], show_col_types = FALSE)
proposed_state <- read_csv(input_paths[["proposed_state"]], show_col_types = FALSE)
step08_rates <- read_csv(input_paths[["step08_rates"]], show_col_types = FALSE)
adjusted_lea <- read_csv(input_paths[["adjusted_lea"]], show_col_types = FALSE)
adjusted_state <- read_csv(input_paths[["adjusted_state"]], show_col_types = FALSE)
adjusted_components <- read_csv(
  input_paths[["adjusted_components"]],
  show_col_types = FALSE
)
adjusted_rates <- read_csv(input_paths[["adjusted_rates"]], show_col_types = FALSE)
exclusion_audit <- read_csv(input_paths[["exclusion_audit"]], show_col_types = FALSE)
lea_distribution <- read_csv(
  input_paths[["lea_distribution"]],
  show_col_types = FALSE
)
lea_type <- read_csv(input_paths[["lea_type"]], show_col_types = FALSE)
report_summary <- read_csv(input_paths[["report_summary"]], show_col_types = FALSE)
reporting_qc <- read_csv(input_paths[["reporting_qc"]], show_col_types = FALSE)
component_crosswalk <- read_csv(
  input_paths[["component_crosswalk"]],
  show_col_types = FALSE
)
lea_crosswalk <- read_csv(input_paths[["lea_crosswalk"]], show_col_types = FALSE)
funding_rates <- read_csv(input_paths[["funding_rates"]], show_col_types = FALSE)
current_rate_map <- read_csv(input_paths[["current_rate_map"]], show_col_types = FALSE)

if (nrow(adjusted_state) != 1 || nrow(report_summary) != 1) {
  stop("Step 11 requires one adjusted statewide summary row.", call. = FALSE)
}


# A. ANALYSIS METADATA AND REPORT CONTROLS -------------------------------------

run_setting <- function(name) {
  value <- run_settings |>
    filter(Setting == name) |>
    pull(Value)

  if (length(value) == 0) {
    return(NA_character_)
  }

  value[[1]]
}

analysis_version <- tibble(
  Field = c(
    "Step 11 script version",
    "Pipeline run started",
    "School year",
    "Count date",
    "Final reporting scope",
    "Calculator filename",
    "R version",
    "Git commit",
    "Git tag",
    "Operational enrollment basis",
    "Weighted-rate method",
    "Weighted pool amount source",
    "Weighted-rate guidance source",
    "Opportunity funding pool",
    "Operational funding pool"
  ),
  Value = c(
    step11_script_version,
    run_setting("Run started"),
    as.character(school_year),
    as.character(count_date),
    first_value(adjusted_state, "ReportingScope"),
    basename(calculator_path),
    R.version.string,
    safe_git_value(c("rev-parse", "--short", "HEAD")),
    safe_git_value(c("describe", "--tags", "--exact-match")),
    operational_enrollment_basis,
    weighted_rate_method,
    weighted_pool_amount_source,
    weighted_rate_guidance_source,
    as.character(opportunity_funding_pool),
    as.character(operational_funding_pool)
  ),
  Source = c(
    "scripts/11_create_report_outputs.R",
    "00_run_settings.csv",
    "scripts/00_settings.R",
    "scripts/00_settings.R",
    "10_adjusted_state_summary.csv",
    "scripts/00_settings.R",
    "R runtime",
    "Git repository, when available",
    "Git repository, when available",
    "scripts/00_settings.R",
    "scripts/00_settings.R",
    "scripts/00_settings.R",
    "scripts/00_settings.R",
    "scripts/00_settings.R",
    "scripts/00_settings.R"
  )
)

model_definitions <- tribble(
  ~DisplayOrder, ~ModelVersion, ~ShortName, ~Definition, ~PrimarySource,
  1L,
  current_model_name,
  "Current reconstruction",
  paste(
    "The current Delaware funding model as reconstructed in the pipeline,",
    "including reported Unit Count quantities, calculated staffing, available",
    "rates, documented crosswalks, analytical assumptions, and missing inputs."
  ),
  "Steps 02-05 and adjusted Step 10 outputs",
  2L,
  pefc_model_name,
  "PEFC calculator",
  paste(
    "The proposed hybrid model exactly as displayed in Copy of Calculator for",
    "25-26 w Charter (003).xlsm, without silently applying pipeline corrections."
  ),
  "PEFC calculator workbook",
  3L,
  reproduction_model_name,
  "Independent reproduction",
  paste(
    "The proposed model independently reviewed and reproduced with reconciled",
    "source data, documented guidance, explicit interpretation rules, and QC."
  ),
  "Steps 06-10 and Step 11 reporting outputs"
)

rounding_rules <- tribble(
  ~MeasureType, ~CalculationPrecision, ~PresentationRule, ~Note,
  "Counts of students, LEAs, or calculation units",
  "Full precision from validated source outputs",
  "Whole numbers",
  "Whole-student charter allocations are completed before reporting.",
  "Position quantities",
  "Full precision",
  "Three decimals in narrative tables; more when needed for reconciliation",
  "Threshold calculations use full precision unless the rule explicitly floors.",
  "Funding amounts",
  "Full precision",
  "Nearest dollar in detailed tables; millions to one decimal in narrative text",
  "Rounded values never feed back into calculations.",
  "Rates",
  "Full precision",
  "Six decimals for weighted rates; two decimals for position rates",
  "Step 10 weighted rates are the final adjusted rates.",
  "Percent differences",
  "Full precision",
  "One decimal in narrative; two decimals in detailed exhibits",
  "Official percentage fields remain blank when the comparison is incomplete."
)


# B. PEFC CALCULATOR EXTRACTION -------------------------------------------------

calculator_raw <- read_excel(
  calculator_path,
  sheet = "Calculator",
  range = "A1:G45",
  col_names = FALSE,
  col_types = "text",
  .name_repair = "minimal"
)
state_totals_raw <- read_excel(
  calculator_path,
  sheet = "State Totals",
  range = "A1:D25",
  col_names = FALSE,
  col_types = "text",
  .name_repair = "minimal"
)
summary_raw <- read_excel(
  calculator_path,
  sheet = "Summary",
  range = "A1:AD300",
  .name_repair = "unique"
)

pefc_component_map <- tribble(
  ~WorkbookRow, ~FundingSection, ~WorkbookLabel, ~Component, ~QuantityType,
  6L, base_section, "Regular - K-3", "Division I Teacher - K-3 Regular Education", "Position",
  7L, base_section, "Regular - 4-12", "Division I Teacher - 4-12 Regular Education", "Position",
  8L, base_section, "Basic - Pre-K-12", "Division I Teacher - Pre-K-12 Basic Special Education", "Position",
  9L, base_section, "Intense - Pre-K-12", "Division I Teacher - Intensive Special Education", "Position",
  10L, base_section, "Complex - Pre-K-12", "Division I Teacher - Complex Special Education", "Position",
  11L, base_section, "Vocational Deduct", "Vocational Deduct", "Position",
  12L, base_section, "Vocational Division I", "Vocational Division I", "Position",
  13L, base_section, "Principal", "Principal", "Position",
  14L, base_section, "Assistant Principal", "Assistant Principal", "Position",
  15L, base_section, "Administrative Support Professionals", "Administrative Support Professionals", "Position",
  16L, base_section, "Instructional Supports", "Instructional Supports", "Position",
  20L, opportunity_section, "LI", "Opportunity Funding - Low Income", "Weighted student",
  21L, opportunity_section, "MLL", "Opportunity Funding - Multilingual Learner", "Weighted student",
  25L, operational_section, "Enrollment", "Operational Funding - Enrollment", "Weighted student",
  26L, operational_section, "LI", "Operational Funding - Low Income", "Weighted student",
  27L, operational_section, "MLL", "Operational Funding - Multilingual Learner", "Weighted student",
  28L, operational_section, "Basic - Pre-K-12", "Operational Funding - Basic Special Education", "Weighted student",
  29L, operational_section, "Intense - Pre-K-12", "Operational Funding - Intensive Special Education", "Weighted student",
  30L, operational_section, "Complex - Pre-K-12", "Operational Funding - Complex Special Education", "Weighted student",
  31L, operational_section, "Vocational", "Operational Funding - Vocational", "Weighted student",
  35L, central_section, "Superintendent", "Superintendent", "Position",
  36L, central_section, "Administrative Assistant", "Administrative Assistant", "Position",
  37L, central_section, "Assistant Superintendent", "Assistant Superintendent", "Position",
  38L, central_section, "Director", "Director", "Position",
  39L, central_section, "11-Month Supervisor", "11-Month Supervisor", "Position",
  40L, central_section, "Buildings and Grounds Supervisor", "Buildings and Grounds Supervisor", "Position",
  41L, central_section, "Food Services Supervisor", "Food Services Supervisor", "Position",
  42L, central_section, "Transportation Supervisor", "Transportation Supervisor", "Position",
  43L, central_section, "Reading Cadre", "Reading Cadre", "Position"
)

pefc_component_detail <- pefc_component_map |>
  mutate(
    InputValue = map_dbl(WorkbookRow, ~ cell_number(calculator_raw, .x, 2L)),
    WeightOrRatio = map_dbl(WorkbookRow, ~ cell_number(calculator_raw, .x, 3L)),
    PositionQuantity = map_dbl(WorkbookRow, ~ cell_number(calculator_raw, .x, 4L)),
    FundingRate = map_dbl(WorkbookRow, ~ cell_number(calculator_raw, .x, 5L)),
    FundingAmount = map_dbl(WorkbookRow, ~ cell_number(calculator_raw, .x, 6L)),
    FundingQuantity = case_when(
      QuantityType == "Position" ~ PositionQuantity,
      QuantityType == "Weighted student" ~ InputValue * WeightOrRatio,
      TRUE ~ NA_real_
    ),
    Note = map_chr(WorkbookRow, ~ coalesce(cell_text(calculator_raw, .x, 7L), "")),
    SourceWorkbook = basename(calculator_path),
    SourceSheet = "Calculator",
    SourceCellRange = paste0("Calculator!A", WorkbookRow, ":G", WorkbookRow),
    ModelVersion = pefc_model_name,
    ReportingScope = pefc_scope_label
  ) |>
  select(
    ModelVersion,
    ReportingScope,
    FundingSection,
    WorkbookRow,
    WorkbookLabel,
    Component,
    QuantityType,
    InputValue,
    WeightOrRatio,
    FundingQuantity,
    FundingRate,
    FundingAmount,
    Note,
    SourceWorkbook,
    SourceSheet,
    SourceCellRange
  )

pefc_base_division_i <- pefc_component_detail |>
  filter(FundingSection == base_section, WorkbookRow <= 12) |>
  summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
  pull(Value)

pefc_additional_base <- pefc_component_detail |>
  filter(FundingSection == base_section, WorkbookRow >= 13, WorkbookRow <= 16) |>
  summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
  pull(Value)

pefc_statewide <- bind_rows(
  build_long_metric("Input", "Total enrollment", cell_number(calculator_raw, 17L, 2L), "Students", "Calculator", "B17"),
  build_long_metric("Input", "Regular K-3 enrollment", cell_number(calculator_raw, 6L, 2L), "Students", "Calculator", "B6"),
  build_long_metric("Input", "Regular Grades 4-12 enrollment", cell_number(calculator_raw, 7L, 2L), "Students", "Calculator", "B7"),
  build_long_metric("Input", "Basic Pre-K-12 enrollment", cell_number(calculator_raw, 8L, 2L), "Students", "Calculator", "B8"),
  build_long_metric("Input", "Intensive enrollment", cell_number(calculator_raw, 9L, 2L), "Students", "Calculator", "B9"),
  build_long_metric("Input", "Complex enrollment", cell_number(calculator_raw, 10L, 2L), "Students", "Calculator", "B10"),
  build_long_metric("Input", "Low-income enrollment", cell_number(calculator_raw, 20L, 2L), "Students", "Calculator", "B20"),
  build_long_metric("Input", "Active MLL enrollment", cell_number(calculator_raw, 21L, 2L), "Students", "Calculator", "B21"),
  build_long_metric("Input", "Vocational enrollment", cell_number(calculator_raw, 31L, 2L), "Students", "Calculator", "B31"),
  build_long_metric(
    "Entity count",
    "LEAs receiving proposed Central Office positions",
    cell_number(calculator_raw, 35L, 4L),
    "LEAs",
    "Calculator",
    "D35",
    "The calculator grants 43 superintendent positions; DAFB is not included in this count."
  ),
  build_long_metric(
    "Calculation unit",
    "School calculation units / principal positions",
    cell_number(calculator_raw, 13L, 4L),
    "Units",
    "Calculator",
    "D13"
  ),
  build_long_metric("Position", "Base Division I positions", pefc_base_division_i, "Positions", "Calculator", "D6:D12"),
  build_long_metric("Position", "Additional Base positions", pefc_additional_base, "Positions", "Calculator", "D13:D16"),
  build_long_metric("Position", "Total Base positions", cell_number(calculator_raw, 17L, 4L), "Positions", "Calculator", "D17"),
  build_long_metric("Position", "Total Central Office positions", cell_number(calculator_raw, 44L, 4L), "Positions", "Calculator", "D44"),
  build_long_metric("Position", "Total position-based positions", cell_number(calculator_raw, 45L, 4L), "Positions", "Calculator", "D45"),
  build_long_metric("Weighted count", "Opportunity weighted count", cell_number(state_totals_raw, 11L, 4L), "Weighted students", "State Totals", "D11"),
  build_long_metric("Weighted count", "Operational weighted count", cell_number(state_totals_raw, 23L, 4L), "Weighted students", "State Totals", "D23"),
  build_long_metric("Rate", "Opportunity weighted rate", cell_number(state_totals_raw, 13L, 4L), "Dollars per weighted student", "State Totals", "D13"),
  build_long_metric("Rate", "Operational weighted rate", cell_number(state_totals_raw, 25L, 4L), "Dollars per weighted student", "State Totals", "D25"),
  build_long_metric("Funding", "Base Funding", cell_number(calculator_raw, 17L, 6L), "Dollars", "Calculator", "F17"),
  build_long_metric("Funding", "Opportunity Funding", cell_number(calculator_raw, 22L, 6L), "Dollars", "Calculator", "F22"),
  build_long_metric("Funding", "Operational Funding", cell_number(calculator_raw, 32L, 6L), "Dollars", "Calculator", "F32"),
  build_long_metric("Funding", "Central Office Funding", cell_number(calculator_raw, 44L, 6L), "Dollars", "Calculator", "F44"),
  build_long_metric(
    "Funding",
    "Position-based Funding",
    cell_number(calculator_raw, 17L, 6L) + cell_number(calculator_raw, 44L, 6L),
    "Dollars",
    "Calculator",
    "F17 + F44"
  ),
  build_long_metric(
    "Funding",
    "Weighted Funding",
    cell_number(calculator_raw, 22L, 6L) + cell_number(calculator_raw, 32L, 6L),
    "Dollars",
    "Calculator",
    "F22 + F32"
  ),
  build_long_metric("Funding", "Total modeled funding", cell_number(calculator_raw, 45L, 6L), "Dollars", "Calculator", "F45")
)

# Identify one total row for each calculator LEA. District rows use the row
# labeled Central Office; charter organization rows use the row whose Child
# matches the calculator LEA name. DAFB uses its organization-total row.
summary_lea_rows <- summary_raw |>
  filter(!is.na(District), District != "CHECK:", District != "Statewide") |>
  mutate(
    IsLEATotalRow = coalesce(Type == "Central Office", FALSE) |
      normalize_name(Child) == normalize_name(District) |
      (District == "DAFB" & Child == "Dover Air Force Base")
  ) |>
  filter(IsLEATotalRow) |>
  left_join(
    lea_crosswalk,
    by = c("District" = "CalculatorLEAName")
  )

stop_if_rows(
  summary_lea_rows |> filter(is.na(DistrictCode)),
  "One or more PEFC Summary LEA rows did not match lea_crosswalk.csv."
)

if (nrow(summary_lea_rows) != nrow(lea_crosswalk)) {
  stop(
    "The PEFC Summary extraction did not produce exactly one row per LEA crosswalk entry.",
    call. = FALSE
  )
}

pefc_lea <- summary_lea_rows |>
  transmute(
    ModelVersion = pefc_model_name,
    ReportingScope = pefc_scope_label,
    DistrictCode,
    DistrictName,
    LEAType,
    CalculatorLEAName = District,
    CalculatorTotalRow = Child,
    IncludedInPipelineStatewide = IncludeInStatewide,
    IncludedInFinalPipelineScope = IncludeInStatewide & DistrictCode != excluded_lea_code,
    BaseFundingAmount = `Base Total`,
    OpportunityFundingAmount = `Opportunity Total`,
    OperationalFundingAmount = `Operational Total`,
    CentralOfficeFundingAmount = `Central Office Total`,
    PositionBasedFundingAmount = `Base Total` + `Central Office Total`,
    WeightedFundingAmount = `Opportunity Total` + `Operational Total`,
    TotalFundingAmount = `Grand Total`,
    TotalBasePositions = `BasePositions (D17)`,
    TotalCentralOfficePositions = `CentralOfficePositions (D44)`,
    K3TeacherPositions = D6,
    Grades4_12TeacherPositions = D7,
    BasicTeacherPositions = D8,
    IntensiveTeacherPositions = D9,
    ComplexTeacherPositions = D10,
    VocationalDeductPositions = D11,
    VocationalDivisionIPositions = D12,
    PrincipalPositions = D13,
    AssistantPrincipalPositions = D14,
    AdministrativeSupportPositions = D15,
    InstructionalSupportPositions = D16,
    SuperintendentPositions = D35,
    AdministrativeAssistantPositions = D36,
    AssistantSuperintendentPositions = D37,
    DirectorPositions = D38,
    ElevenMonthSupervisorPositions = D39,
    BuildingsGroundsSupervisorPositions = D40,
    FoodServicesSupervisorPositions = D41,
    TransportationSupervisorPositions = D42,
    ReadingCadrePositions = D43,
    SourceWorkbook = basename(calculator_path),
    SourceSheet = "Summary"
  ) |>
  arrange(LEAType, DistrictName)

summary_check_rows <- summary_raw |>
  filter(District == "CHECK:")

check_row_value <- function(label_pattern, column) {
  matching <- summary_check_rows |>
    filter(str_detect(Child, fixed(label_pattern)))

  if (nrow(matching) != 1) {
    stop("Expected one Summary CHECK row matching: ", label_pattern, call. = FALSE)
  }

  matching[[column]][[1]]
}

summary_reconciliation_map <- tribble(
  ~Metric, ~SummaryColumn, ~Unit,
  "Base Funding", "Base Total", "Dollars",
  "Opportunity Funding", "Opportunity Total", "Dollars",
  "Operational Funding", "Operational Total", "Dollars",
  "Central Office Funding", "Central Office Total", "Dollars",
  "Total modeled funding", "Grand Total", "Dollars",
  "Total Base positions", "BasePositions (D17)", "Positions",
  "Total Central Office positions", "CentralOfficePositions (D44)", "Positions"
)

summary_reconciliation <- summary_reconciliation_map |>
  mutate(
    ValueA = map_dbl(
      SummaryColumn,
      ~ as.numeric(check_row_value("Calculator when C1=Statewide", .x))
    ),
    ValueB = map_dbl(
      SummaryColumn,
      ~ as.numeric(check_row_value("Sum of all rows", .x))
    ),
    SourceA = "Summary CHECK row: Calculator when C1=Statewide",
    SourceB = "Summary CHECK row: Sum of all rows",
    Difference = ValueA - ValueB,
    Tolerance = reconciliation_tolerance,
    Status = if_else(abs(Difference) <= Tolerance, "Pass", "Review"),
    CheckGroup = "PEFC Summary internal reconciliation",
    Interpretation = if_else(
      Status == "Pass",
      "The displayed statewide result reconciles to summed Summary rows within tolerance.",
      "The workbook's displayed statewide result differs from the sum of Summary rows."
    )
  ) |>
  select(
    CheckGroup,
    Metric,
    Unit,
    ValueA,
    SourceA,
    ValueB,
    SourceB,
    Difference,
    Tolerance,
    Status,
    Interpretation
  )

weighted_reconciliation <- bind_rows(
  tibble(
    CheckGroup = "PEFC weighted-funding reconciliation",
    Metric = "Opportunity weighted count",
    Unit = "Weighted students",
    ValueA = cell_number(state_totals_raw, 11L, 4L),
    SourceA = "State Totals!D11",
    ValueB = pefc_component_detail |>
      filter(FundingSection == opportunity_section) |>
      summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
      pull(Value),
    SourceB = "Sum of PEFC Opportunity component weighted counts"
  ),
  tibble(
    CheckGroup = "PEFC weighted-funding reconciliation",
    Metric = "Operational weighted count",
    Unit = "Weighted students",
    ValueA = cell_number(state_totals_raw, 23L, 4L),
    SourceA = "State Totals!D23",
    ValueB = pefc_component_detail |>
      filter(FundingSection == operational_section) |>
      summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
      pull(Value),
    SourceB = "Sum of PEFC Operational component weighted counts"
  ),
  tibble(
    CheckGroup = "PEFC weighted-funding reconciliation",
    Metric = "Opportunity funding pool",
    Unit = "Dollars",
    ValueA = cell_number(state_totals_raw, 12L, 4L),
    SourceA = "State Totals!D12",
    ValueB = cell_number(calculator_raw, 22L, 6L),
    SourceB = "Calculator!F22"
  ),
  tibble(
    CheckGroup = "PEFC weighted-funding reconciliation",
    Metric = "Operational funding pool",
    Unit = "Dollars",
    ValueA = cell_number(state_totals_raw, 24L, 4L),
    SourceA = "State Totals!D24",
    ValueB = cell_number(calculator_raw, 32L, 6L),
    SourceB = "Calculator!F32"
  )
) |>
  mutate(
    Difference = ValueA - ValueB,
    Tolerance = reconciliation_tolerance,
    Status = if_else(abs(Difference) <= Tolerance, "Pass", "Review"),
    Interpretation = if_else(
      Status == "Pass",
      "The workbook values reconcile within tolerance.",
      "The workbook values require review."
    )
  )

pefc_internal_reconciliation <- bind_rows(
  summary_reconciliation,
  weighted_reconciliation
) |>
  arrange(CheckGroup, Metric)


# C. THREE-WAY STATEWIDE COMPARISONS ------------------------------------------

state <- adjusted_state |> slice(1)
report <- report_summary |> slice(1)

shared_final_school <- shared |>
  filter(
    AggregationLevel == "School",
    DistrictCode != dafb_district_code,
    DistrictCode != excluded_lea_code
  )

shared_final_totals <- shared_final_school |>
  summarise(
    EnrollmentK3 = sum(EnrollmentK3, na.rm = TRUE),
    Enrollment4_12 = sum(Enrollment4_12, na.rm = TRUE)
  )

current_central_office_recipient_count <- current_detail |>
  filter(
    IncludeInStatewide,
    DistrictCode != excluded_lea_code,
    FundingSection == central_section,
    coalesce(FundingQuantity, 0) > 0
  ) |>
  distinct(DistrictCode) |>
  nrow()

pefc_central_office_recipient_count <- pefc_lea |>
  filter(coalesce(TotalCentralOfficePositions, 0) > 0) |>
  distinct(DistrictCode) |>
  nrow()

independent_central_office_recipient_count <- proposed_detail |>
  filter(
    IncludeInStatewide,
    DistrictCode != excluded_lea_code,
    FundingSection == central_section,
    coalesce(FundingQuantity, 0) > 0
  ) |>
  distinct(DistrictCode) |>
  nrow()

three_way_input_comparison <- tribble(
  ~DisplayOrder, ~Metric, ~Unit, ~RecreatedCurrentModel, ~PEFCProposedAsPresented, ~IndependentReproduction, ~ComparisonNote,
  10L, "Total enrollment", "Students", state$Enrollment, metric_value(pefc_statewide, "Total enrollment"), state$Enrollment, "Current and independent values use the final Step 10 scope; PEFC uses the calculator scope as presented.",
  20L, "Regular K-3 enrollment", "Students", shared_final_totals$EnrollmentK3, metric_value(pefc_statewide, "Regular K-3 enrollment"), shared_final_totals$EnrollmentK3, "Shared source input for the current and independent models.",
  30L, "Regular Grades 4-12 enrollment", "Students", shared_final_totals$Enrollment4_12, metric_value(pefc_statewide, "Regular Grades 4-12 enrollment"), shared_final_totals$Enrollment4_12, "Shared source input for the current and independent models.",
  40L, "Basic Pre-K-12 enrollment", "Students", state$EnrollmentPreK + state$EnrollmentBasicK12, metric_value(pefc_statewide, "Basic Pre-K-12 enrollment"), state$EnrollmentPreK + state$EnrollmentBasicK12, "The current model retains separate Basic Pre-K and Basic K-12 unit categories; the proposed models combine them.",
  50L, "Intensive enrollment", "Students", state$EnrollmentIntense, metric_value(pefc_statewide, "Intensive enrollment"), state$EnrollmentIntense, "Shared source input for the current and independent models.",
  60L, "Complex enrollment", "Students", state$EnrollmentComplex, metric_value(pefc_statewide, "Complex enrollment"), state$EnrollmentComplex, "Shared source input for the current and independent models.",
  70L, "Low-income enrollment", "Students", state$LI, metric_value(pefc_statewide, "Low-income enrollment"), state$LI, "Low income is not a separate weighted current-model component but is shown as an underlying input.",
  80L, "Active MLL enrollment", "Students", state$MLL, metric_value(pefc_statewide, "Active MLL enrollment"), state$MLL, "MLL is not a separate weighted current-model component but is shown as an underlying input.",
  90L, "Vocational enrollment", "Students", NA_real_, metric_value(pefc_statewide, "Vocational enrollment"), adjusted_components |> filter(Component == "Operational Funding - Vocational") |> pull(RawInputCount), "Vocational enrollment is a proposed Operational input and is not used as a distinct current-model input.",
  100L, "Organizations represented in the applicable source scope", "Organizations", state$LEACount, nrow(pefc_lea), state$LEACount, "The PEFC calculator contains 44 organization rows, including DAFB and Bryan Allen Stevenson. The final pipeline scope contains 42 LEAs after excluding both.",
  105L, "Organizations receiving at least one modeled central-office position", "Organizations", current_central_office_recipient_count, pefc_central_office_recipient_count, independent_central_office_recipient_count, "All 42 final-scope LEAs receive at least one modeled current and independently reproduced central-office position. The PEFC calculator provides at least one Central Office position to 43 of its 44 organization rows because DAFB is excluded from those formulas.",
  110L, "School calculation units", "Units", state$CodedSchoolCount, metric_value(pefc_statewide, "School calculation units / principal positions"), state$ProposedCalculationUnitCount, "Current uses official coded schools; PEFC and the independent reproduction use proposed school/building calculation units."
) |>
  mutate(
    RecreatedCurrentScope = final_scope_label,
    PEFCScope = pefc_scope_label,
    IndependentReproductionScope = final_scope_label
  )

current_crosswalk <- component_crosswalk |>
  filter(SourceModel == "Current")
proposed_crosswalk <- component_crosswalk |>
  filter(SourceModel == "Proposed")

current_final <- current_detail |>
  filter(IncludeInStatewide, DistrictCode != excluded_lea_code) |>
  left_join(
    current_crosswalk,
    by = c("Component" = "SourceComponent")
  )

stop_if_rows(
  current_final |> filter(is.na(ReportComponent)),
  "The report component crosswalk is missing a current-model component."
)

reproduction_final <- proposed_detail |>
  filter(IncludeInStatewide, DistrictCode != excluded_lea_code) |>
  left_join(
    proposed_crosswalk,
    by = c("Component" = "SourceComponent")
  )

stop_if_rows(
  reproduction_final |> filter(is.na(ReportComponent)),
  "The report component crosswalk is missing a proposed-model component."
)

pefc_mapped <- pefc_component_detail |>
  left_join(
    proposed_crosswalk,
    by = c("Component" = "SourceComponent")
  )

stop_if_rows(
  pefc_mapped |> filter(is.na(ReportComponent)),
  "The report component crosswalk is missing a PEFC component."
)

aggregate_position_components <- function(data, model_label) {
  data |>
    filter(QuantityType == "Position", IncludeInMainReport) |>
    summarise(
      Quantity = sum_preserve_missing(FundingQuantity),
      FundingAmount = sum_preserve_missing(FundingAmount),
      Complete = if ("CalculationComplete" %in% names(data)) {
        all_true_or_false(CalculationComplete)
      } else {
        TRUE
      },
      .by = c(
        ReportComponent,
        ReportSection,
        ComparisonGroup,
        DirectComparability,
        DisplayOrder
      )
    ) |>
    mutate(ModelVersion = model_label)
}

current_position_agg <- aggregate_position_components(current_final, current_model_name)
reproduction_position_agg <- aggregate_position_components(
  reproduction_final,
  reproduction_model_name
)

pefc_position_agg <- pefc_mapped |>
  filter(QuantityType == "Position", IncludeInMainReport) |>
  summarise(
    Quantity = sum_preserve_missing(FundingQuantity),
    FundingAmount = sum_preserve_missing(FundingAmount),
    Complete = TRUE,
    .by = c(
      ReportComponent,
      ReportSection,
      ComparisonGroup,
      DirectComparability,
      DisplayOrder
    )
  ) |>
  mutate(ModelVersion = pefc_model_name)

position_metadata <- bind_rows(
  current_position_agg,
  pefc_position_agg,
  reproduction_position_agg
) |>
  distinct(
    ReportComponent,
    ReportSection,
    ComparisonGroup,
    DirectComparability,
    DisplayOrder
  )

position_wide <- bind_rows(
  current_position_agg,
  pefc_position_agg,
  reproduction_position_agg
) |>
  select(ReportComponent, ModelVersion, Quantity, Complete) |>
  pivot_wider(
    names_from = ModelVersion,
    values_from = c(Quantity, Complete),
    names_glue = "{.value}__{ModelVersion}"
  )

names(position_wide) <- names(position_wide) |>
  str_replace_all(fixed("Quantity__Recreated Current Model"), "RecreatedCurrentModel") |>
  str_replace_all(fixed("Quantity__PEFC Proposed Model as Presented"), "PEFCProposedAsPresented") |>
  str_replace_all(
    fixed(paste0("Quantity__", reproduction_model_name)),
    "IndependentReproduction"
  ) |>
  str_replace_all(fixed("Complete__Recreated Current Model"), "RecreatedCurrentComplete") |>
  str_replace_all(fixed("Complete__PEFC Proposed Model as Presented"), "PEFCComplete") |>
  str_replace_all(
    fixed(paste0("Complete__", reproduction_model_name)),
    "IndependentReproductionComplete"
  )

base_division_i_row <- tibble(
  ReportComponent = "Base Division I positions",
  ReportSection = "Base / Division I",
  ComparisonGroup = "Derived total",
  DirectComparability = "Direct with scope differences",
  DisplayOrder = 5L,
  RecreatedCurrentModel = current_position_agg |>
    filter(ComparisonGroup == "Base Division I") |>
    summarise(Value = sum(Quantity, na.rm = TRUE)) |>
    pull(Value),
  PEFCProposedAsPresented = metric_value(pefc_statewide, "Base Division I positions"),
  IndependentReproduction = reproduction_position_agg |>
    filter(ComparisonGroup == "Base Division I") |>
    summarise(Value = sum(Quantity, na.rm = TRUE)) |>
    pull(Value),
  RecreatedCurrentComplete = current_position_agg |>
    filter(ComparisonGroup == "Base Division I") |>
    summarise(Value = all(Complete)) |>
    pull(Value),
  PEFCComplete = TRUE,
  IndependentReproductionComplete = TRUE
)

total_base_row <- tibble(
  ReportComponent = "Total Base positions",
  ReportSection = "Base Funding",
  ComparisonGroup = "Derived total",
  DirectComparability = "Not directly defined for the current model",
  DisplayOrder = 120L,
  RecreatedCurrentModel = NA_real_,
  PEFCProposedAsPresented = metric_value(pefc_statewide, "Total Base positions"),
  IndependentReproduction = reproduction_final |>
    filter(QuantityType == "Position", FundingSection == base_section) |>
    summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
    pull(Value),
  RecreatedCurrentComplete = FALSE,
  PEFCComplete = TRUE,
  IndependentReproductionComplete = TRUE
)

total_central_row <- tibble(
  ReportComponent = "Total Central Office positions",
  ReportSection = "Central Office",
  ComparisonGroup = "Derived total",
  DirectComparability = "Partial because the current total is incomplete",
  DisplayOrder = 290L,
  RecreatedCurrentModel = current_final |>
    filter(QuantityType == "Position", ReportSection == "Central Office") |>
    summarise(Value = sum_preserve_missing(FundingQuantity)) |>
    pull(Value),
  PEFCProposedAsPresented = metric_value(pefc_statewide, "Total Central Office positions"),
  IndependentReproduction = reproduction_final |>
    filter(QuantityType == "Position", FundingSection == central_section) |>
    summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
    pull(Value),
  RecreatedCurrentComplete = current_final |>
    filter(QuantityType == "Position", ReportSection == "Central Office") |>
    summarise(Value = all_true_or_false(CalculationComplete)) |>
    pull(Value),
  PEFCComplete = TRUE,
  IndependentReproductionComplete = TRUE
)

total_position_row <- tibble(
  ReportComponent = "Total position-based positions",
  ReportSection = "All position-based sections",
  ComparisonGroup = "Derived total",
  DirectComparability = "Partial because current quantities are incomplete and categories differ",
  DisplayOrder = 300L,
  RecreatedCurrentModel = current_final |>
    filter(QuantityType == "Position") |>
    summarise(Value = sum_preserve_missing(FundingQuantity)) |>
    pull(Value),
  PEFCProposedAsPresented = metric_value(pefc_statewide, "Total position-based positions"),
  IndependentReproduction = reproduction_final |>
    filter(QuantityType == "Position") |>
    summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
    pull(Value),
  RecreatedCurrentComplete = current_final |>
    filter(QuantityType == "Position") |>
    summarise(Value = all_true_or_false(CalculationComplete)) |>
    pull(Value),
  PEFCComplete = TRUE,
  IndependentReproductionComplete = TRUE
)

three_way_position_comparison <- position_metadata |>
  left_join(position_wide, by = "ReportComponent") |>
  bind_rows(
    base_division_i_row,
    total_base_row,
    total_central_row,
    total_position_row
  ) |>
  mutate(
    RecreatedCurrentStatus = status_from_complete(RecreatedCurrentComplete),
    PEFCStatus = status_from_complete(PEFCComplete),
    IndependentReproductionStatus = status_from_complete(
      IndependentReproductionComplete
    ),
    RecreatedCurrentScope = final_scope_label,
    PEFCScope = pefc_scope_label,
    IndependentReproductionScope = final_scope_label
  ) |>
  arrange(DisplayOrder, ReportComponent)

current_base_funding <- current_final |>
  filter(FundingSection == base_section) |>
  summarise(Value = sum(FundingAmount, na.rm = TRUE)) |>
  pull(Value)
current_central_funding <- current_final |>
  filter(FundingSection == central_section) |>
  summarise(Value = sum(FundingAmount, na.rm = TRUE)) |>
  pull(Value)

three_way_funding_comparison <- tribble(
  ~DisplayOrder, ~FundingMeasure, ~RecreatedCurrentModel, ~PEFCProposedAsPresented, ~IndependentReproduction, ~Comparability, ~Interpretation,
  10L, "Base Funding", current_base_funding, metric_value(pefc_statewide, "Base Funding"), state$ProposedBaseFundingAmount, "Partial", "Current Base Funding omits components with missing inputs or rates and uses comparison rate mappings.",
  20L, "Central Office Funding", current_central_funding, metric_value(pefc_statewide, "Central Office Funding"), state$ProposedCentralOfficeFundingAmount, "Partial", "Current Central Office Funding is incomplete where required inputs are missing.",
  30L, "Position-based Funding", state$CurrentModelFundingAmount, metric_value(pefc_statewide, "Position-based Funding"), state$ProposedPositionBasedFundingAmount, "Closest available comparison", "Compares the partial recreated current model with proposed Base plus Central Office Funding.",
  40L, "Opportunity Funding", NA_real_, metric_value(pefc_statewide, "Opportunity Funding"), state$ProposedOpportunityFundingAmount, "No direct current equivalent", "The current pipeline does not recreate a directly comparable weighted Opportunity section.",
  50L, "Operational Funding", NA_real_, metric_value(pefc_statewide, "Operational Funding"), state$ProposedOperationalFundingAmount, "No direct current equivalent", "The current pipeline does not recreate a directly comparable weighted Operational section.",
  60L, "Weighted Funding", NA_real_, metric_value(pefc_statewide, "Weighted Funding"), state$ProposedWeightedFundingAmount, "No direct current equivalent", "Opportunity and Operational Funding are combined.",
  70L, "Total modeled funding", state$CurrentModelFundingAmount, metric_value(pefc_statewide, "Total modeled funding"), state$ProposedModelFundingAmount, "Gross comparison only", "The current amount is partial and current appropriations outside the recreated baseline have not been netted against proposed weighted funding."
) |>
  mutate(
    Unit = "Dollars",
    RecreatedCurrentScope = final_scope_label,
    PEFCScope = pefc_scope_label,
    IndependentReproductionScope = final_scope_label,
    PEFCDifferenceFromCurrent = PEFCProposedAsPresented - RecreatedCurrentModel,
    IndependentDifferenceFromCurrent = IndependentReproduction - RecreatedCurrentModel,
    PEFCPercentDifferenceFromCurrent = percent_difference(
      PEFCProposedAsPresented,
      RecreatedCurrentModel
    ),
    IndependentPercentDifferenceFromCurrent = percent_difference(
      IndependentReproduction,
      RecreatedCurrentModel
    )
  )

step08_rate_lookup <- step08_rates |>
  select(FundingSection, Step08SelectedRate = SelectedFundingRate)
step10_rate_lookup <- adjusted_rates |>
  select(FundingSection, Step10AdjustedRate = AdjustedFundingRate)


# Scope-normalized PEFC bridge:
# 1. workbook statewide display;
# 2. sum of all PEFC LEA total rows;
# 3. PEFC LEA total rows restricted to the final Step 10 scope;
# 4. independent reproduction on that same final scope.
#
# Keeping the full-scope LEA sum as a separate stage distinguishes workbook
# internal reconciliation differences from the effect of the final exclusions.
pefc_full_scope_summary <- pefc_lea |>
  summarise(
    BaseFunding = sum(BaseFundingAmount, na.rm = TRUE),
    CentralOfficeFunding = sum(CentralOfficeFundingAmount, na.rm = TRUE),
    PositionBasedFunding = sum(PositionBasedFundingAmount, na.rm = TRUE),
    OpportunityFunding = sum(OpportunityFundingAmount, na.rm = TRUE),
    OperationalFunding = sum(OperationalFundingAmount, na.rm = TRUE),
    WeightedFunding = sum(WeightedFundingAmount, na.rm = TRUE),
    TotalModeledFunding = sum(TotalFundingAmount, na.rm = TRUE),
    TotalBasePositions = sum(TotalBasePositions, na.rm = TRUE),
    TotalCentralOfficePositions = sum(
      TotalCentralOfficePositions,
      na.rm = TRUE
    ),
    TotalPositionBasedPositions = sum(
      TotalBasePositions + TotalCentralOfficePositions,
      na.rm = TRUE
    )
  )

pefc_final_scope_summary <- pefc_lea |>
  filter(IncludedInFinalPipelineScope) |>
  summarise(
    BaseFunding = sum(BaseFundingAmount, na.rm = TRUE),
    CentralOfficeFunding = sum(CentralOfficeFundingAmount, na.rm = TRUE),
    PositionBasedFunding = sum(PositionBasedFundingAmount, na.rm = TRUE),
    OpportunityFunding = sum(OpportunityFundingAmount, na.rm = TRUE),
    OperationalFunding = sum(OperationalFundingAmount, na.rm = TRUE),
    WeightedFunding = sum(WeightedFundingAmount, na.rm = TRUE),
    TotalModeledFunding = sum(TotalFundingAmount, na.rm = TRUE),
    TotalBasePositions = sum(TotalBasePositions, na.rm = TRUE),
    TotalCentralOfficePositions = sum(
      TotalCentralOfficePositions,
      na.rm = TRUE
    ),
    TotalPositionBasedPositions = sum(
      TotalBasePositions + TotalCentralOfficePositions,
      na.rm = TRUE
    )
  )

independent_position_value <- function(component) {
  three_way_position_comparison |>
    filter(ReportComponent == component) |>
    pull(IndependentReproduction) |>
    first()
}

build_pefc_scope_bridge <- function(
  display_order,
  metric,
  unit,
  pefc_displayed_value,
  pefc_full_scope_value,
  pefc_final_scope_value,
  independent_value
) {
  result <- tibble(
    Metric = metric,
    Unit = unit,
    StageOrder = 1:4,
    Stage = c(
      "PEFC displayed statewide",
      "Sum of PEFC LEA total rows",
      "PEFC restricted to final pipeline scope",
      "Independent reproduction"
    ),
    Scope = c(
      pefc_scope_label,
      "PEFC source scope: 44 organization rows",
      final_scope_label,
      final_scope_label
    ),
    Value = c(
      pefc_displayed_value,
      pefc_full_scope_value,
      pefc_final_scope_value,
      independent_value
    ),
    Interpretation = c(
      "Workbook statewide display retained as presented.",
      paste(
        "Summed PEFC LEA total rows; the change from Stage 1 reflects",
        "any workbook internal reconciliation difference."
      ),
      paste(
        "PEFC LEA rows restricted to the same 42-LEA scope used by the",
        "final pipeline; the change from Stage 2 reflects the exclusions."
      ),
      paste(
        "Independent reproduction on the aligned scope; the remaining",
        "change reflects inputs, formulas, calculation units, rates, and",
        "other implementation differences rather than scope."
      )
    )
  ) |>
    mutate(
      DisplayOrder = display_order * 10L + StageOrder,
      IncrementalChange = Value - lag(Value),
      DifferenceFromIndependent = Value - independent_value
    )

  result
}

pefc_scope_normalized_comparison <- bind_rows(
  build_pefc_scope_bridge(
    10L,
    "Base Funding",
    "Dollars",
    metric_value(pefc_statewide, "Base Funding"),
    pefc_full_scope_summary$BaseFunding,
    pefc_final_scope_summary$BaseFunding,
    state$ProposedBaseFundingAmount
  ),
  build_pefc_scope_bridge(
    20L,
    "Central Office Funding",
    "Dollars",
    metric_value(pefc_statewide, "Central Office Funding"),
    pefc_full_scope_summary$CentralOfficeFunding,
    pefc_final_scope_summary$CentralOfficeFunding,
    state$ProposedCentralOfficeFundingAmount
  ),
  build_pefc_scope_bridge(
    30L,
    "Position-based Funding",
    "Dollars",
    metric_value(pefc_statewide, "Position-based Funding"),
    pefc_full_scope_summary$PositionBasedFunding,
    pefc_final_scope_summary$PositionBasedFunding,
    state$ProposedPositionBasedFundingAmount
  ),
  build_pefc_scope_bridge(
    40L,
    "Opportunity Funding",
    "Dollars",
    metric_value(pefc_statewide, "Opportunity Funding"),
    pefc_full_scope_summary$OpportunityFunding,
    pefc_final_scope_summary$OpportunityFunding,
    state$ProposedOpportunityFundingAmount
  ),
  build_pefc_scope_bridge(
    50L,
    "Operational Funding",
    "Dollars",
    metric_value(pefc_statewide, "Operational Funding"),
    pefc_full_scope_summary$OperationalFunding,
    pefc_final_scope_summary$OperationalFunding,
    state$ProposedOperationalFundingAmount
  ),
  build_pefc_scope_bridge(
    60L,
    "Weighted Funding",
    "Dollars",
    metric_value(pefc_statewide, "Weighted Funding"),
    pefc_full_scope_summary$WeightedFunding,
    pefc_final_scope_summary$WeightedFunding,
    state$ProposedWeightedFundingAmount
  ),
  build_pefc_scope_bridge(
    70L,
    "Total modeled funding",
    "Dollars",
    metric_value(pefc_statewide, "Total modeled funding"),
    pefc_full_scope_summary$TotalModeledFunding,
    pefc_final_scope_summary$TotalModeledFunding,
    state$ProposedModelFundingAmount
  ),
  build_pefc_scope_bridge(
    80L,
    "Total Base positions",
    "Positions",
    metric_value(pefc_statewide, "Total Base positions"),
    pefc_full_scope_summary$TotalBasePositions,
    pefc_final_scope_summary$TotalBasePositions,
    independent_position_value("Total Base positions")
  ),
  build_pefc_scope_bridge(
    90L,
    "Total Central Office positions",
    "Positions",
    metric_value(pefc_statewide, "Total Central Office positions"),
    pefc_full_scope_summary$TotalCentralOfficePositions,
    pefc_final_scope_summary$TotalCentralOfficePositions,
    independent_position_value("Total Central Office positions")
  ),
  build_pefc_scope_bridge(
    100L,
    "Total position-based positions",
    "Positions",
    metric_value(pefc_statewide, "Total position-based positions"),
    pefc_full_scope_summary$TotalPositionBasedPositions,
    pefc_final_scope_summary$TotalPositionBasedPositions,
    independent_position_value("Total position-based positions")
  )
) |>
  arrange(DisplayOrder)

three_way_rate_comparison <- funding_rates |>
  rename(
    RateComponent = Component,
    FundingSection = `Funding Section`,
    PEFCProposedAsPresentedRate = `Funding Rate`,
    RateBasis = `Rate Basis`,
    SourceCell = `Source Cell`
  ) |>
  left_join(step08_rate_lookup, by = "FundingSection") |>
  left_join(step10_rate_lookup, by = "FundingSection") |>
  mutate(
    UsedInCurrentModel = RateComponent %in% current_rate_map$RateComponent,
    RecreatedCurrentModelRate = if_else(
      UsedInCurrentModel,
      PEFCProposedAsPresentedRate,
      NA_real_
    ),
    IndependentReproductionStep08Rate = case_when(
      FundingSection %in% weighted_sections ~ Step08SelectedRate,
      TRUE ~ PEFCProposedAsPresentedRate
    ),
    IndependentReproductionFinalRate = case_when(
      FundingSection %in% weighted_sections ~ Step10AdjustedRate,
      TRUE ~ PEFCProposedAsPresentedRate
    ),
    RateTreatment = case_when(
      FundingSection %in% weighted_sections ~ paste(
        "Calculator rate retained for reference; Step 08 and Step 10 rates are",
        "recalculated from fixed pools under Nick Johnson's guidance."
      ),
      TRUE ~ "The independent reproduction applies the calculator-supplied position rate."
    )
  ) |>
  select(
    FundingSection,
    RateComponent,
    RateBasis,
    RecreatedCurrentModelRate,
    PEFCProposedAsPresentedRate,
    IndependentReproductionStep08Rate,
    IndependentReproductionFinalRate,
    UsedInCurrentModel,
    SourceCell,
    RateTreatment
  )


# D. DIFFERENCE EXPLANATIONS ----------------------------------------------------

position_difference_rows <- three_way_position_comparison |>
  transmute(
    MeasureType = "Position quantity",
    Measure = ReportComponent,
    PEFCProposedAsPresented,
    IndependentReproduction,
    Difference = IndependentReproduction - PEFCProposedAsPresented,
    PercentDifference = percent_difference(
      IndependentReproduction,
      PEFCProposedAsPresented
    ),
    PrimaryDriver = case_when(
      ReportComponent == "Principal" ~ "Calculation units and reporting scope",
      ReportComponent == "Assistant Principal" ~ "Formula interpretation, inputs, and reporting scope",
      ReportComponent == "Administrative Support Professionals" ~ "Inputs, calculation units, and reporting scope",
      ReportComponent == "Instructional Supports" ~ "Source inputs and reporting scope",
      ReportComponent %in% c(
        "Assistant Superintendent",
        "Director",
        "11-Month Supervisor"
      ) ~ "Formula interpretation and reporting scope",
      TRUE ~ "Source inputs and reporting scope"
    ),
    DifferenceAttribution = paste(
      "The displayed difference is not a single-factor decomposition because",
      "the PEFC and final independent estimates use different scopes."
    )
  )

funding_difference_rows <- three_way_funding_comparison |>
  filter(!is.na(PEFCProposedAsPresented), !is.na(IndependentReproduction)) |>
  transmute(
    MeasureType = "Funding amount",
    Measure = FundingMeasure,
    PEFCProposedAsPresented,
    IndependentReproduction,
    Difference = IndependentReproduction - PEFCProposedAsPresented,
    PercentDifference = percent_difference(
      IndependentReproduction,
      PEFCProposedAsPresented
    ),
    PrimaryDriver = case_when(
      FundingMeasure %in% c("Opportunity Funding", "Operational Funding", "Weighted Funding") ~
        "Fixed funding pools; rates change but pool totals do not",
      FundingMeasure == "Central Office Funding" ~
        "Formula interpretation, eligibility, rates, and reporting scope",
      TRUE ~ "Combined inputs, formulas, calculation units, and reporting scope"
    ),
    DifferenceAttribution = paste(
      "The funding difference combines multiple drivers and should not be",
      "interpreted as a formula error or a formal fiscal-impact decomposition."
    )
  )

input_difference_rows <- three_way_input_comparison |>
  transmute(
    MeasureType = "Input or calculation unit",
    Measure = Metric,
    PEFCProposedAsPresented,
    IndependentReproduction,
    Difference = IndependentReproduction - PEFCProposedAsPresented,
    PercentDifference = percent_difference(
      IndependentReproduction,
      PEFCProposedAsPresented
    ),
    PrimaryDriver = case_when(
      Metric == "School calculation units" ~ "Calculation units and reporting scope",
      TRUE ~ "Source inputs and reporting scope"
    ),
    DifferenceAttribution = ComparisonNote
  )

pefc_vs_reproduction_differences <- bind_rows(
  input_difference_rows,
  position_difference_rows,
  funding_difference_rows
) |>
  mutate(
    MaterialDifference = case_when(
      MeasureType == "Funding amount" ~ abs(Difference) > 1,
      TRUE ~ abs(Difference) > comparison_tolerance
    ),
    PEFCScope = pefc_scope_label,
    IndependentReproductionScope = final_scope_label
  ) |>
  arrange(MeasureType, desc(abs(Difference)))

difference_driver_summary <- pefc_vs_reproduction_differences |>
  filter(MaterialDifference) |>
  summarise(
    MaterialMeasureCount = n(),
    ExampleMeasures = paste(head(Measure, 5), collapse = "; "),
    NumericDecompositionAvailable = FALSE,
    Interpretation = first(DifferenceAttribution),
    .by = PrimaryDriver
  ) |>
  arrange(desc(MaterialMeasureCount), PrimaryDriver)

step08_funding <- proposed_state |>
  summarise(
    BaseFunding = sum(FundingAmount[FundingSection == base_section], na.rm = TRUE),
    CentralOfficeFunding = sum(FundingAmount[FundingSection == central_section], na.rm = TRUE),
    OpportunityFunding = sum(FundingAmount[FundingSection == opportunity_section], na.rm = TRUE),
    OperationalFunding = sum(FundingAmount[FundingSection == operational_section], na.rm = TRUE)
  ) |>
  mutate(
    PositionBasedFunding = BaseFunding + CentralOfficeFunding,
    TotalFunding = PositionBasedFunding + OpportunityFunding + OperationalFunding
  )

step08_opportunity_rate <- step08_rates |>
  filter(FundingSection == opportunity_section) |>
  pull(SelectedFundingRate)
step10_opportunity_rate <- adjusted_rates |>
  filter(FundingSection == opportunity_section) |>
  pull(AdjustedFundingRate)
step08_operational_rate <- step08_rates |>
  filter(FundingSection == operational_section) |>
  pull(SelectedFundingRate)
step10_operational_rate <- adjusted_rates |>
  filter(FundingSection == operational_section) |>
  pull(AdjustedFundingRate)

scope_rate_bridge <- bind_rows(
  tibble(
    BridgeType = "Total modeled funding",
    DisplayOrder = 10L,
    Stage = "PEFC calculator as presented",
    Scope = pefc_scope_label,
    Value = metric_value(pefc_statewide, "Total modeled funding"),
    IncrementalChange = NA_real_,
    Interpretation = "Literal statewide total displayed in the PEFC calculator."
  ),
  tibble(
    BridgeType = "Total modeled funding",
    DisplayOrder = 20L,
    Stage = "Independent reproduction before final Step 10 exclusion",
    Scope = "Excludes DAFB; includes Bryan Allen Stevenson",
    Value = step08_funding$TotalFunding,
    IncrementalChange = step08_funding$TotalFunding - metric_value(
      pefc_statewide,
      "Total modeled funding"
    ),
    Interpretation = paste(
      "Combined effect of reconciled inputs, independent formula treatments,",
      "calculation-unit differences, and the DAFB scope change; not decomposed."
    )
  ),
  tibble(
    BridgeType = "Total modeled funding",
    DisplayOrder = 30L,
    Stage = "Final independent reproduction",
    Scope = final_scope_label,
    Value = state$ProposedModelFundingAmount,
    IncrementalChange = state$ProposedModelFundingAmount - step08_funding$TotalFunding,
    Interpretation = paste(
      "Bryan Allen Stevenson is removed. The fixed Opportunity and Operational",
      "pools are retained and redistributed, so the total weighted pools do not change."
    )
  ),
  tibble(
    BridgeType = "Opportunity weighted rate",
    DisplayOrder = 40L,
    Stage = "PEFC calculator as presented",
    Scope = pefc_scope_label,
    Value = metric_value(pefc_statewide, "Opportunity weighted rate"),
    IncrementalChange = NA_real_,
    Interpretation = "Calculator-supplied rate."
  ),
  tibble(
    BridgeType = "Opportunity weighted rate",
    DisplayOrder = 50L,
    Stage = "Step 08 independent reproduction",
    Scope = "Excludes DAFB; includes Bryan Allen Stevenson",
    Value = step08_opportunity_rate,
    IncrementalChange = step08_opportunity_rate -
      metric_value(pefc_statewide, "Opportunity weighted rate"),
    Interpretation = "Fixed pool divided by the Step 08 eligible weighted count."
  ),
  tibble(
    BridgeType = "Opportunity weighted rate",
    DisplayOrder = 60L,
    Stage = "Final Step 10 independent reproduction",
    Scope = final_scope_label,
    Value = step10_opportunity_rate,
    IncrementalChange = step10_opportunity_rate - step08_opportunity_rate,
    Interpretation = "Fixed pool divided by the final eligible weighted count."
  ),
  tibble(
    BridgeType = "Operational weighted rate",
    DisplayOrder = 70L,
    Stage = "PEFC calculator as presented",
    Scope = pefc_scope_label,
    Value = metric_value(pefc_statewide, "Operational weighted rate"),
    IncrementalChange = NA_real_,
    Interpretation = "Calculator-supplied rate."
  ),
  tibble(
    BridgeType = "Operational weighted rate",
    DisplayOrder = 80L,
    Stage = "Step 08 independent reproduction",
    Scope = "Excludes DAFB; includes Bryan Allen Stevenson",
    Value = step08_operational_rate,
    IncrementalChange = step08_operational_rate -
      metric_value(pefc_statewide, "Operational weighted rate"),
    Interpretation = "Fixed pool divided by the Step 08 eligible weighted count."
  ),
  tibble(
    BridgeType = "Operational weighted rate",
    DisplayOrder = 90L,
    Stage = "Final Step 10 independent reproduction",
    Scope = final_scope_label,
    Value = step10_operational_rate,
    IncrementalChange = step10_operational_rate - step08_operational_rate,
    Interpretation = "Fixed pool divided by the final eligible weighted count."
  )
) |>
  arrange(DisplayOrder)

current_base_division_i_funding <- current_position_agg |>
  filter(ComparisonGroup == "Base Division I") |>
  summarise(Value = sum(FundingAmount, na.rm = TRUE)) |>
  pull(Value)

reproduction_base_division_i_funding <- reproduction_position_agg |>
  filter(ComparisonGroup == "Base Division I") |>
  summarise(Value = sum(FundingAmount, na.rm = TRUE)) |>
  pull(Value)

current_component_funding <- current_position_agg |>
  select(ReportComponent, CurrentFundingAmount = FundingAmount)

reproduction_component_funding <- reproduction_position_agg |>
  select(
    ReportComponent,
    IndependentReproductionFundingAmount = FundingAmount
  )

current_vs_reproduction_position_crosswalk <- three_way_position_comparison |>
  filter(DisplayOrder < 300) |>
  left_join(current_component_funding, by = "ReportComponent") |>
  left_join(reproduction_component_funding, by = "ReportComponent") |>
  mutate(
    CurrentFundingAmount = case_when(
      ReportComponent == "Base Division I positions" ~
        current_base_division_i_funding,
      ReportComponent == "Total Central Office positions" ~ current_central_funding,
      TRUE ~ CurrentFundingAmount
    ),
    IndependentReproductionFundingAmount = case_when(
      ReportComponent == "Base Division I positions" ~
        reproduction_base_division_i_funding,
      ReportComponent == "Total Base positions" ~ state$ProposedBaseFundingAmount,
      ReportComponent == "Total Central Office positions" ~
        state$ProposedCentralOfficeFundingAmount,
      TRUE ~ IndependentReproductionFundingAmount
    )
  ) |>
  transmute(
    DisplayOrder,
    ReportSection,
    ReportComponent,
    ComparisonGroup,
    DirectComparability,
    CurrentQuantity = RecreatedCurrentModel,
    CurrentFundingAmount,
    CurrentStatus = RecreatedCurrentStatus,
    IndependentReproductionQuantity = IndependentReproduction,
    IndependentReproductionFundingAmount,
    IndependentReproductionStatus,
    QuantityDifference = IndependentReproduction - RecreatedCurrentModel,
    FundingDifference = IndependentReproductionFundingAmount - CurrentFundingAmount,
    CurrentScope = RecreatedCurrentScope,
    IndependentReproductionScope
  ) |>
  arrange(DisplayOrder, ReportComponent)


# E. REPORT-READY STATEWIDE AND LEA SUMMARIES ----------------------------------

key_report_metrics <- tribble(
  ~DisplayOrder, ~Metric, ~Value, ~Unit, ~SourceOutput, ~Interpretation,
  10L, "Final enrollment", state$Enrollment, "Students", "10_adjusted_state_summary.csv", final_scope_label,
  20L, "Included LEAs", state$LEACount, "LEAs", "10_adjusted_state_summary.csv", "19 districts and 23 charters.",
  30L, "Partial recreated current-model funding", state$CurrentModelFundingAmount, "Dollars", "10_adjusted_state_summary.csv", "Incomplete because current quantities or rates remain missing.",
  40L, "Independent proposed position-based funding", state$ProposedPositionBasedFundingAmount, "Dollars", "10_adjusted_state_summary.csv", "Proposed Base plus Central Office Funding.",
  50L, "Position-based modeled difference", state$PositionBasedFundingDifference, "Dollars", "10_adjusted_state_summary.csv", "Closest available comparison; still preliminary because the current baseline is incomplete.",
  60L, "Position-based modeled percent difference", state$ReportPositionBasedPercentDifference, "Percent", "10_adjusted_state_summary.csv", "Preliminary arithmetic comparison.",
  70L, "Independent proposed weighted funding", state$ProposedWeightedFundingAmount, "Dollars", "10_adjusted_state_summary.csv", "Fixed Opportunity and Operational pools.",
  80L, "Independent proposed total modeled funding", state$ProposedModelFundingAmount, "Dollars", "10_adjusted_state_summary.csv", "All four proposed funding sections.",
  90L, "Gross full-model difference", state$FullModelFundingDifference, "Dollars", "10_adjusted_state_summary.csv", "Not a confirmed net fiscal impact.",
  100L, "Gross full-model percent difference", state$ReportGrossFullModelPercentDifference, "Percent", "10_adjusted_state_summary.csv", "Not a confirmed net fiscal impact.",
  110L, "PEFC total modeled funding as presented", metric_value(pefc_statewide, "Total modeled funding"), "Dollars", "11_pefc_statewide_as_presented.csv", pefc_scope_label,
  120L, "PEFC versus independent total difference", state$ProposedModelFundingAmount - metric_value(pefc_statewide, "Total modeled funding"), "Dollars", "11_three_way_funding_comparison.csv", "Combines source, formula, calculation-unit, scope, and rate-method differences.",
  130L, "LEAs increasing under position-based comparison", report$PositionBasedLEAsIncreasing, "LEAs", "10_report_summary.csv", "One included LEA decreases under the preliminary position-based comparison.",
  140L, "LEAs increasing under gross full-model comparison", report$FullModelLEAsIncreasing, "LEAs", "10_report_summary.csv", "All 42 included LEAs increase under the gross comparison."
)

statewide_summary_for_report <- tibble(
  SchoolYear = state$SchoolYear,
  CountDate = state$CountDate,
  FinalReportingScope = state$ReportingScope,
  Enrollment = state$Enrollment,
  LEACount = state$LEACount,
  CodedSchoolCount = state$CodedSchoolCount,
  ProposedCalculationUnitCount = state$ProposedCalculationUnitCount,
  RecreatedCurrentModelFunding = state$CurrentModelFundingAmount,
  PEFCBaseFundingAsPresented = metric_value(pefc_statewide, "Base Funding"),
  PEFCCentralOfficeFundingAsPresented = metric_value(
    pefc_statewide,
    "Central Office Funding"
  ),
  PEFCPositionBasedFundingAsPresented = metric_value(
    pefc_statewide,
    "Position-based Funding"
  ),
  PEFCOpportunityFundingAsPresented = metric_value(
    pefc_statewide,
    "Opportunity Funding"
  ),
  PEFCOperationalFundingAsPresented = metric_value(
    pefc_statewide,
    "Operational Funding"
  ),
  PEFCTotalFundingAsPresented = metric_value(
    pefc_statewide,
    "Total modeled funding"
  ),
  IndependentBaseFunding = state$ProposedBaseFundingAmount,
  IndependentCentralOfficeFunding = state$ProposedCentralOfficeFundingAmount,
  IndependentPositionBasedFunding = state$ProposedPositionBasedFundingAmount,
  IndependentOpportunityFunding = state$ProposedOpportunityFundingAmount,
  IndependentOperationalFunding = state$ProposedOperationalFundingAmount,
  IndependentTotalFunding = state$ProposedModelFundingAmount,
  PositionBasedDifferenceFromCurrent = state$PositionBasedFundingDifference,
  PositionBasedPercentDifferenceFromCurrent = state$ReportPositionBasedPercentDifference,
  GrossDifferenceFromCurrent = state$FullModelFundingDifference,
  GrossPercentDifferenceFromCurrent = state$ReportGrossFullModelPercentDifference,
  ComparisonComplete = state$ComparisonComplete,
  ComparisonStatus = state$ComparisonStatus,
  Interpretation = state$ComparisonInterpretation
)

lea_summary_for_report <- adjusted_lea |>
  left_join(
    pefc_lea |>
      filter(IncludedInFinalPipelineScope) |>
      select(
        DistrictCode,
        PEFCBaseFundingAmount = BaseFundingAmount,
        PEFCOpportunityFundingAmount = OpportunityFundingAmount,
        PEFCOperationalFundingAmount = OperationalFundingAmount,
        PEFCCentralOfficeFundingAmount = CentralOfficeFundingAmount,
        PEFCPositionBasedFundingAmount = PositionBasedFundingAmount,
        PEFCTotalFundingAmount = TotalFundingAmount
      ),
    by = "DistrictCode"
  ) |>
  transmute(
    DistrictCode,
    DistrictName,
    LEAType,
    Enrollment,
    CurrentModelFundingAmount,
    PEFCBaseFundingAmount,
    PEFCCentralOfficeFundingAmount,
    PEFCPositionBasedFundingAmount,
    PEFCOpportunityFundingAmount,
    PEFCOperationalFundingAmount,
    PEFCTotalFundingAmount,
    IndependentBaseFundingAmount = ProposedBaseFundingAmount,
    IndependentCentralOfficeFundingAmount = ProposedCentralOfficeFundingAmount,
    IndependentPositionBasedFundingAmount = ProposedPositionBasedFundingAmount,
    IndependentOpportunityFundingAmount = ProposedOpportunityFundingAmount,
    IndependentOperationalFundingAmount = ProposedOperationalFundingAmount,
    IndependentWeightedFundingAmount = ProposedWeightedFundingAmount,
    IndependentTotalFundingAmount = ProposedModelFundingAmount,
    PositionBasedFundingDifference,
    PositionBasedPercentDifference = ReportPositionBasedPercentDifference,
    GrossFullModelDifference = FullModelFundingDifference,
    GrossFullModelPercentDifference = ReportGrossFullModelPercentDifference,
    PositionBasedFundingDifferencePerStudent,
    GrossFullModelFundingDifferencePerStudent =
      FullModelFundingDifferencePerStudent,
    CurrentModelComplete,
    ComparisonStatus,
    PositionBasedPercentRank,
    GrossFullModelPercentRank,
    ReportingScope = final_scope_label
  ) |>
  arrange(LEAType, DistrictName)

lea_distribution_for_report <- lea_distribution |>
  mutate(ReportingScope = final_scope_label)

lea_type_for_report <- lea_type |>
  mutate(ReportingScope = final_scope_label)


# F. FIGURE-READY DATASETS ------------------------------------------------------

figure_statewide_funding_sections <- three_way_funding_comparison |>
  select(
    DisplayOrder,
    FundingMeasure,
    RecreatedCurrentModel,
    PEFCProposedAsPresented,
    IndependentReproduction,
    Comparability
  ) |>
  pivot_longer(
    cols = c(
      RecreatedCurrentModel,
      PEFCProposedAsPresented,
      IndependentReproduction
    ),
    names_to = "ModelVersionKey",
    values_to = "FundingAmount"
  ) |>
  mutate(
    ModelVersion = recode(
      ModelVersionKey,
      RecreatedCurrentModel = current_model_name,
      PEFCProposedAsPresented = pefc_model_name,
      IndependentReproduction = reproduction_model_name
    )
  ) |>
  select(-ModelVersionKey)

figure_position_comparison <- three_way_position_comparison |>
  filter(
    ReportComponent %in% c(
      "Base Division I positions",
      "Principal",
      "Assistant Principal",
      "Administrative Support Professionals",
      "Instructional Supports",
      "Total Base positions",
      "Total Central Office positions"
    )
  ) |>
  select(
    DisplayOrder,
    ReportComponent,
    DirectComparability,
    RecreatedCurrentModel,
    PEFCProposedAsPresented,
    IndependentReproduction
  ) |>
  pivot_longer(
    cols = c(
      RecreatedCurrentModel,
      PEFCProposedAsPresented,
      IndependentReproduction
    ),
    names_to = "ModelVersionKey",
    values_to = "PositionQuantity"
  ) |>
  mutate(
    ModelVersion = recode(
      ModelVersionKey,
      RecreatedCurrentModel = current_model_name,
      PEFCProposedAsPresented = pefc_model_name,
      IndependentReproduction = reproduction_model_name
    )
  ) |>
  select(-ModelVersionKey)

figure_lea_change_by_type <- adjusted_lea |>
  transmute(
    DistrictCode,
    DistrictName,
    LEAType,
    Enrollment,
    Comparison = "Position-based",
    PercentDifference = ReportPositionBasedPercentDifference,
    DollarDifference = PositionBasedFundingDifference
  ) |>
  bind_rows(
    adjusted_lea |>
      transmute(
        DistrictCode,
        DistrictName,
        LEAType,
        Enrollment,
        Comparison = "Gross full model",
        PercentDifference = ReportGrossFullModelPercentDifference,
        DollarDifference = FullModelFundingDifference
      )
  ) |>
  mutate(ReportingScope = final_scope_label)

figure_difference_drivers <- difference_driver_summary |>
  select(
    DriverCategory = PrimaryDriver,
    MaterialMeasureCount,
    ExampleMeasures,
    NumericDecompositionAvailable,
    Interpretation
  )


# G. AUDIT AND QUALITY-CONTROL OUTPUTS -----------------------------------------

decision_discrepancy_log <- bind_rows(
  current_issues |>
    transmute(
      ModelArea = "Recreated Current Model",
      EvidenceClassification = Priority,
      Component,
      Issue,
      AffectedRows,
      ImplementedTreatment = CurrentTreatment,
      RemainingAction = Action,
      PrimarySource = "04_current_model_issues.csv"
    ),
  proposed_issues |>
    transmute(
      ModelArea = "Independent Technical Review and Reproduction",
      EvidenceClassification = Priority,
      Component,
      Issue,
      AffectedRows,
      ImplementedTreatment = CurrentTreatment,
      RemainingAction = Action,
      PrimarySource = "07_proposed_model_issues.csv"
    ),
  tibble(
    ModelArea = "PEFC Proposed Model as Presented",
    EvidenceClassification = "Workbook note/formula conflict",
    Component = "Assistant Principal",
    Issue = paste(
      "The written calculator note explicitly identifies 0.65 and 1.65",
      "positions, while the literal workbook formula rounds those values down."
    ),
    AffectedRows = NA_integer_,
    ImplementedTreatment = paste(
      "The PEFC-as-presented extraction retains the literal workbook result.",
      "The independent reproduction follows the written fractional-position rule."
    ),
    RemainingAction = "Confirm the intended assistant-principal formula.",
    PrimarySource = "Calculator!G14 and Data-sheet assistant-principal formulas"
  ),
  pefc_internal_reconciliation |>
    filter(Status == "Review") |>
    transmute(
      ModelArea = "PEFC Proposed Model as Presented",
      EvidenceClassification = "Workbook internal discrepancy",
      Component = Metric,
      Issue = paste(SourceA, "does not equal", SourceB, "within tolerance."),
      AffectedRows = NA_integer_,
      ImplementedTreatment = paste(
        "The PEFC-as-presented section reports the displayed statewide value;",
        "the discrepancy is retained in the reconciliation appendix."
      ),
      RemainingAction = "Confirm the intended workbook total or formula.",
      PrimarySource = "11_pefc_internal_reconciliation.csv"
    )
) |>
  mutate(
    DecisionID = sprintf("D%03d", row_number()),
    .before = 1
  )

exhibit_registry <- tribble(
  ~ExhibitID, ~ReportSection, ~ExhibitTitle, ~Step11Output, ~UpstreamSources, ~Scope, ~Rounding,
  "Exhibit 1", "Executive Summary", "Statewide funding summary across three model versions", "11_three_way_funding_comparison.csv", "PEFC Calculator; 10_adjusted_state_summary.csv; 05 and 08 funding detail", "Model-specific scopes shown in the exhibit", "Funding rounded for presentation only",
  "Exhibit 2", "Purpose, Scope, and Methods", "Pipeline and model-version overview", "11_analysis_version.csv; 11_model_version_definitions.csv", "00_run_settings.csv; scripts/00_settings.R", "Final Step 10 scope unless otherwise noted", "Not applicable",
  "Exhibit 3", "Recreated Current Model", "Current model structure, quantities, and completeness", "11_current_vs_reproduction_position_crosswalk.csv", "04_current_model_rules.csv; 05_current_model_funding_detail.csv", final_scope_label, "Positions displayed to three decimals",
  "Exhibit 4", "PEFC Proposed Model as Presented", "PEFC architecture and statewide estimates", "11_pefc_statewide_as_presented.csv", "Copy of Calculator for 25-26 w Charter (003).xlsm", pefc_scope_label, "Workbook cached values retained at full precision",
  "Exhibit 5", "Independent Technical Review and Reproduction", "Key review findings and implemented treatments", "11_decision_discrepancy_log.csv", "04 and 07 issue files; PEFC internal reconciliation", "Issue-specific", "Not applicable",
  "Exhibit 6", "Three-Way Comparison", "Three-way structural comparison", "11_model_version_definitions.csv; report_component_crosswalk.csv", "Model definitions and maintained comparison crosswalk", "Model-specific", "Not applicable",
  "Exhibit 7", "Three-Way Comparison", "Three-way statewide positions and funding", "11_three_way_position_comparison.csv; 11_three_way_funding_comparison.csv", "PEFC extraction; 05 and 08 detail; Step 10 adjusted outputs", "Model-specific scopes shown in the exhibit", "Presentation rounding only",
  "Exhibit 7A", "Three-Way Comparison", "PEFC scope-normalized bridge", "11_pefc_scope_normalized_comparison.csv", "11_pefc_statewide_as_presented.csv; 11_pefc_lea_as_presented.csv; Step 10 adjusted outputs", "PEFC displayed scope, PEFC LEA-row scope, and final 42-LEA scope", "Full precision retained; presentation rounding only",
  "Figure 1", "Three-Way Comparison", "LEA modeled percentage differences by type", "11_figure_lea_change_by_type.csv", "10_adjusted_lea_comparison.csv", final_scope_label, "Percentages displayed to one or two decimals",
  "Exhibit 8", "Key Findings", "Like-for-like and gross comparison guardrails", "11_key_report_metrics.csv", "10_adjusted_state_summary.csv; 10_report_summary.csv", final_scope_label, "Presentation rounding only",
  "Exhibit 9", "Limitations and Next Steps", "Limitations and unresolved questions", "11_decision_discrepancy_log.csv", "04 and 07 issue files; PEFC reconciliation", "Issue-specific", "Not applicable"
)

report_qc <- bind_rows(
  tibble(
    Check = "PEFC component rows extracted",
    Value = nrow(pefc_component_detail),
    Expected = nrow(pefc_component_map),
    Status = if_else(nrow(pefc_component_detail) == nrow(pefc_component_map), "Pass", "Fail")
  ),
  tibble(
    Check = "PEFC LEA total rows extracted",
    Value = nrow(pefc_lea),
    Expected = nrow(lea_crosswalk),
    Status = if_else(nrow(pefc_lea) == nrow(lea_crosswalk), "Pass", "Fail")
  ),
  tibble(
    Check = "Final LEAs in report-ready LEA table",
    Value = nrow(lea_summary_for_report),
    Expected = state$LEACount,
    Status = if_else(nrow(lea_summary_for_report) == state$LEACount, "Pass", "Fail")
  ),
  tibble(
    Check = "PEFC LEA rows in final normalized scope",
    Value = nrow(pefc_lea |> filter(IncludedInFinalPipelineScope)),
    Expected = state$LEACount,
    Status = if_else(Value == Expected, "Pass", "Fail")
  ),
  tibble(
    Check = "Current LEAs receiving a central-office position",
    Value = current_central_office_recipient_count,
    Expected = state$LEACount,
    Status = if_else(Value == Expected, "Pass", "Fail")
  ),
  tibble(
    Check = "Independent LEAs receiving a central-office position",
    Value = independent_central_office_recipient_count,
    Expected = state$LEACount,
    Status = if_else(Value == Expected, "Pass", "Fail")
  ),
  tibble(
    Check = "PEFC scope-normalized bridge rows",
    Value = nrow(pefc_scope_normalized_comparison),
    Expected = 40,
    Status = if_else(Value == Expected, "Pass", "Fail")
  ),
  tibble(
    Check = "PEFC scope-normalized bridge stages per metric",
    Value = pefc_scope_normalized_comparison |>
      count(Metric) |>
      filter(n != 4) |>
      nrow(),
    Expected = 0,
    Status = if_else(Value == Expected, "Pass", "Fail")
  ),
  tibble(
    Check = "Independent Base plus Central equals position-based funding",
    Value = state$ProposedBaseFundingAmount + state$ProposedCentralOfficeFundingAmount -
      state$ProposedPositionBasedFundingAmount,
    Expected = 0,
    Status = if_else(abs(Value) <= comparison_tolerance, "Pass", "Fail")
  ),
  tibble(
    Check = "Independent weighted pools equal fixed total",
    Value = state$ProposedWeightedFundingAmount -
      (opportunity_funding_pool + operational_funding_pool),
    Expected = 0,
    Status = if_else(abs(Value) <= comparison_tolerance, "Pass", "Fail")
  ),
  tibble(
    Check = "Independent full model reconciles",
    Value = state$ProposedPositionBasedFundingAmount + state$ProposedWeightedFundingAmount -
      state$ProposedModelFundingAmount,
    Expected = 0,
    Status = if_else(abs(Value) <= comparison_tolerance, "Pass", "Fail")
  ),
  tibble(
    Check = "Current Base plus Central equals partial current total",
    Value = current_base_funding + current_central_funding -
      state$CurrentModelFundingAmount,
    Expected = 0,
    Status = if_else(abs(Value) <= comparison_tolerance, "Pass", "Fail")
  ),
  tibble(
    Check = "PEFC Base plus Central equals displayed position-based funding",
    Value = metric_value(pefc_statewide, "Base Funding") +
      metric_value(pefc_statewide, "Central Office Funding") -
      metric_value(pefc_statewide, "Position-based Funding"),
    Expected = 0,
    Status = if_else(abs(Value) <= comparison_tolerance, "Pass", "Fail")
  ),
  tibble(
    Check = "Report component crosswalk duplicate source keys",
    Value = component_crosswalk |>
      count(SourceModel, SourceComponent) |>
      filter(n > 1) |>
      nrow(),
    Expected = 0,
    Status = if_else(Value == 0, "Pass", "Fail")
  ),
  tibble(
    Check = "PEFC displayed full model reconciles to displayed sections",
    Value = metric_value(pefc_statewide, "Position-based Funding") +
      metric_value(pefc_statewide, "Weighted Funding") -
      metric_value(pefc_statewide, "Total modeled funding"),
    Expected = 0,
    Status = if_else(abs(Value) <= comparison_tolerance, "Pass", "Fail")
  ),
  tibble(
    Check = "Current components missing report crosswalk",
    Value = nrow(current_final |> filter(is.na(ReportComponent))),
    Expected = 0,
    Status = if_else(Value == 0, "Pass", "Fail")
  ),
  tibble(
    Check = "Proposed components missing report crosswalk",
    Value = nrow(reproduction_final |> filter(is.na(ReportComponent))),
    Expected = 0,
    Status = if_else(Value == 0, "Pass", "Fail")
  ),
  tibble(
    Check = "Upstream Step 10 QC failures",
    Value = nrow(reporting_qc |> filter(Status != "Pass")),
    Expected = 0,
    Status = if_else(Value == 0, "Pass", "Fail")
  )
)

stop_if_rows(
  report_qc |> filter(Status == "Fail"),
  "One or more Step 11 report QC checks failed."
)


# WRITE OUTPUTS -----------------------------------------------------------------

write_review_csv(analysis_version, output_paths[["analysis_version"]])
write_review_csv(model_definitions, output_paths[["model_definitions"]])
write_review_csv(rounding_rules, output_paths[["rounding_rules"]])
write_model_csv(pefc_statewide, output_paths[["pefc_statewide"]])
write_model_csv(pefc_component_detail, output_paths[["pefc_component"]])
write_model_csv(pefc_lea, output_paths[["pefc_lea"]])
write_model_csv(pefc_internal_reconciliation, output_paths[["pefc_reconciliation"]])
write_model_csv(three_way_input_comparison, output_paths[["input_comparison"]])
write_model_csv(three_way_position_comparison, output_paths[["position_comparison"]])
write_model_csv(three_way_funding_comparison, output_paths[["funding_comparison"]])
write_model_csv(three_way_rate_comparison, output_paths[["rate_comparison"]])
write_model_csv(
  pefc_vs_reproduction_differences,
  output_paths[["pefc_reproduction_differences"]]
)
write_model_csv(difference_driver_summary, output_paths[["difference_drivers"]])
write_model_csv(scope_rate_bridge, output_paths[["scope_rate_bridge"]])
write_model_csv(
  pefc_scope_normalized_comparison,
  output_paths[["pefc_scope_normalized"]]
)
write_model_csv(
  current_vs_reproduction_position_crosswalk,
  output_paths[["position_crosswalk"]]
)
write_review_csv(key_report_metrics, output_paths[["key_metrics"]])
write_review_csv(statewide_summary_for_report, output_paths[["statewide_report"]])
write_review_csv(lea_summary_for_report, output_paths[["lea_report"]])
write_review_csv(
  lea_distribution_for_report,
  output_paths[["lea_distribution_report"]]
)
write_review_csv(lea_type_for_report, output_paths[["lea_type_report"]])
write_review_csv(figure_statewide_funding_sections, output_paths[["figure_funding"]])
write_review_csv(figure_position_comparison, output_paths[["figure_positions"]])
write_review_csv(figure_lea_change_by_type, output_paths[["figure_lea"]])
write_review_csv(figure_difference_drivers, output_paths[["figure_drivers"]])
write_review_csv(decision_discrepancy_log, output_paths[["decision_log"]])
write_review_csv(exhibit_registry, output_paths[["exhibit_registry"]])
write_review_csv(report_qc, output_paths[["report_qc"]])

message("Step 11 report outputs written to: ", output_dir)
message("Review Step 11 QC: ", output_paths[["report_qc"]])
