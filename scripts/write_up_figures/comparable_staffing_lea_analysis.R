# =============================================================================
# 12_export_comparable_staffing_lea_analysis.R
# =============================================================================
# Creates a simple companion workbook with:
#   1. LEA Summary             - one row per in-scope LEA
#   2. Category Detail         - one row per LEA and comparable staffing category
#   3. Instructional Supports  - current component detail and proposed total
#   4. Category Guide          - concise current/proposed treatments and notes
#
# The script validates the workbook against the statewide comparable subtotal
# but keeps technical reconciliation checks out of the presentation workbook.
#
# LATEST REVISION: starts from the simplified workbook and adds only the
# Instructional Supports detail tab requested for the consolidated category.
#
# Run from the school_funding_model project root after the main pipeline.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install the openxlsx package before running this script.", call. = FALSE)
}

# Helpers ----------------------------------------------------------------------

stop_if_missing_columns <- function(data, required, source_name) {
  missing <- setdiff(required, names(data))

  if (length(missing) > 0) {
    stop(
      paste(
        source_name,
        "is missing required columns:",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

collapse_unique <- function(x) {
  values <- sort(unique(as.character(x[!is.na(x) & trimws(x) != ""])))
  if (length(values) == 0) "" else paste(values, collapse = "; ")
}

# Paths ------------------------------------------------------------------------

current_detail_path <- file.path(
  intermediate_dir,
  "05_current_model_funding_detail.csv"
)
proposed_detail_path <- file.path(
  intermediate_dir,
  "08_proposed_model_funding_detail.csv"
)
current_rules_path <- file.path(
  intermediate_dir,
  "04_current_model_rules.csv"
)
proposed_rules_path <- file.path(
  intermediate_dir,
  "07_proposed_model_rules.csv"
)
component_path <- file.path(
  final_dir,
  "11_staffing_component_comparison.csv"
)
statewide_path <- file.path(
  final_dir,
  "11_staffing_statewide_comparison.csv"
)
output_path <- file.path(
  final_dir,
  "comparable_staffing_lea_analysis.xlsx"
)

check_required_files(c(
  current_detail_path,
  proposed_detail_path,
  current_rules_path,
  proposed_rules_path,
  component_path,
  statewide_path,
  model_comparison_crosswalk_path
))

# Read inputs ------------------------------------------------------------------

read_detail <- function(path, source_name) {
  detail <- read_csv(path, show_col_types = FALSE)

  stop_if_missing_columns(
    detail,
    c(
      "SchoolYear",
      "CountDate",
      "DistrictCode",
      "DistrictName",
      "LEAType",
      "IncludeInStatewide",
      "Component",
      "FundingQuantity",
      "FundingAmount",
      "FundingComplete"
    ),
    source_name
  )

  detail |>
    mutate(
      SchoolYear = as.integer(SchoolYear),
      CountDate = as.Date(CountDate),
      DistrictCode = as.integer(DistrictCode),
      IncludeInStatewide = as.logical(IncludeInStatewide),
      FundingQuantity = as.numeric(FundingQuantity),
      FundingAmount = as.numeric(FundingAmount),
      FundingComplete = coalesce(as.logical(FundingComplete), FALSE)
    ) |>
    filter(IncludeInStatewide)
}

current_detail <- read_detail(
  current_detail_path,
  "05_current_model_funding_detail.csv"
)
proposed_detail <- read_detail(
  proposed_detail_path,
  "08_proposed_model_funding_detail.csv"
)

component_comparison <- read_csv(
  component_path,
  show_col_types = FALSE
)

stop_if_missing_columns(
  component_comparison,
  c(
    "AnalysisSection",
    "ComparisonCategory",
    "ComparisonStatus",
    "IncludedInComparableAmountSubtotal",
    "IsCompleteForFinalComparison",
    "CurrentFundingMissingRows"
  ),
  "11_staffing_component_comparison.csv"
)

component_comparison <- component_comparison |>
  mutate(
    IncludedInComparableAmountSubtotal = as.logical(
      IncludedInComparableAmountSubtotal
    ),
    IsCompleteForFinalComparison = as.logical(
      IsCompleteForFinalComparison
    ),
    CurrentFundingMissingRows = as.integer(CurrentFundingMissingRows)
  ) |>
  filter(AnalysisSection == "Staffing rules")

crosswalk <- read_csv(
  model_comparison_crosswalk_path,
  show_col_types = FALSE
)

stop_if_missing_columns(
  crosswalk,
  c(
    "AnalysisSection",
    "SourceModel",
    "SourceComponent",
    "ComparisonCategory",
    "ComparisonGroup",
    "DisplayOrder"
  ),
  "model_comparison_crosswalk.csv"
)

crosswalk <- crosswalk |>
  mutate(DisplayOrder = as.integer(DisplayOrder)) |>
  filter(AnalysisSection == "Staffing rules")

statewide <- read_csv(
  statewide_path,
  show_col_types = FALSE
) |>
  filter(AnalysisSection == "Staffing rules")

stop_if_missing_columns(
  statewide,
  c(
    "ComparableAmountCurrentFundingAmount",
    "ComparableAmountProposedFundingAmount",
    "ComparableAmountCurrentPositions",
    "ComparableAmountProposedPositions"
  ),
  "11_staffing_statewide_comparison.csv"
)

if (nrow(statewide) != 1L) {
  stop(
    "Expected one staffing-rules row in the statewide output.",
    call. = FALSE
  )
}

current_rules <- read_csv(current_rules_path, show_col_types = FALSE)
proposed_rules <- read_csv(proposed_rules_path, show_col_types = FALSE)

stop_if_missing_columns(
  current_rules,
  c("Component", "Rule"),
  "04_current_model_rules.csv"
)
stop_if_missing_columns(
  proposed_rules,
  c("Component", "Rule"),
  "07_proposed_model_rules.csv"
)

# Scope checks -----------------------------------------------------------------

if (dafb_district_code %in% c(
  current_detail$DistrictCode,
  proposed_detail$DistrictCode
)) {
  stop(
    "DAFB must not appear in the aligned comparable staffing workbook.",
    call. = FALSE
  )
}

outside_formula_rows <- bind_rows(
  current_detail |>
    filter(Component %in% outside_formula_current_components) |>
    transmute(SourceModel = "Current", Component),
  proposed_detail |>
    filter(Component %in% outside_formula_current_components) |>
    transmute(SourceModel = "Proposed", Component)
)

stop_if_rows(
  outside_formula_rows,
  paste(
    "Outside-formula components must remain outside the staffing detail",
    "used for this workbook."
  )
)

# Comparable categories --------------------------------------------------------

category_status <- component_comparison |>
  filter(IncludedInComparableAmountSubtotal) |>
  select(
    ComparisonCategory,
    ComparisonStatus,
    IsCompleteForFinalComparison,
    CurrentFundingMissingRows
  ) |>
  distinct()

status_conflicts <- category_status |>
  count(ComparisonCategory) |>
  filter(n != 1L)

stop_if_rows(
  status_conflicts,
  "Each comparable category must have exactly one status row."
)

comparable_categories <- category_status$ComparisonCategory

comparison_map <- crosswalk |>
  filter(
    SourceModel %in% c("Current", "Proposed"),
    ComparisonCategory %in% comparable_categories
  ) |>
  distinct(
    SourceModel,
    SourceComponent,
    ComparisonCategory,
    ComparisonGroup,
    DisplayOrder
  )

duplicate_mappings <- comparison_map |>
  count(SourceModel, SourceComponent) |>
  filter(n > 1L)

stop_if_rows(
  duplicate_mappings,
  "A source component maps to more than one comparison category."
)

missing_current_map <- setdiff(
  comparable_categories,
  comparison_map |>
    filter(SourceModel == "Current") |>
    pull(ComparisonCategory) |>
    unique()
)
missing_proposed_map <- setdiff(
  comparable_categories,
  comparison_map |>
    filter(SourceModel == "Proposed") |>
    pull(ComparisonCategory) |>
    unique()
)

if (length(missing_current_map) > 0 || length(missing_proposed_map) > 0) {
  stop(
    "Every comparable category must have both current and proposed mappings.",
    call. = FALSE
  )
}

category_order <- comparison_map |>
  group_by(ComparisonCategory) |>
  summarise(
    CategoryOrder = min(DisplayOrder, na.rm = TRUE),
    CategoryGroup = first(ComparisonGroup),
    CurrentComponents = collapse_unique(
      SourceComponent[SourceModel == "Current"]
    ),
    ProposedComponents = collapse_unique(
      SourceComponent[SourceModel == "Proposed"]
    ),
    .groups = "drop"
  )

summarize_treatments <- function(rules, source_model) {
  map <- comparison_map |>
    filter(SourceModel == source_model) |>
    distinct(SourceComponent, ComparisonCategory)

  missing_rules <- setdiff(map$SourceComponent, rules$Component)

  if (length(missing_rules) > 0) {
    stop(
      paste(
        source_model,
        "rule output is missing mapped components:",
        paste(missing_rules, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  rules |>
    inner_join(map, by = c("Component" = "SourceComponent")) |>
    mutate(Treatment = paste0(Component, ": ", Rule)) |>
    group_by(ComparisonCategory) |>
    summarise(Treatment = collapse_unique(Treatment), .groups = "drop")
}

current_treatment_summary <- summarize_treatments(current_rules, "Current") |>
  rename(CurrentTreatment = Treatment)
proposed_treatment_summary <- summarize_treatments(proposed_rules, "Proposed") |>
  rename(ProposedTreatment = Treatment)

# Concise interpretation guide -------------------------------------------------

interpretation_reference <- tribble(
  ~ComparisonCategory, ~ComparisonNote,

  "K-3 Regular Education Teachers",
  "Same enrollment category and equivalent staffing ratio; small differences reflect precision and aggregation.",

  "Grades 4-12 Regular Education Teachers",
  "Same enrollment category and equivalent staffing ratio; small differences reflect precision and aggregation.",

  "Basic Special Education Teachers",
  "Current Basic Pre-K and Basic K-12 positions are combined and compared with the proposed Pre-K-12 Basic category.",

  "Intensive Special Education Teachers",
  "Same enrollment category and equivalent staffing ratio; small differences reflect precision and aggregation.",

  "Complex Special Education Teachers",
  "Same enrollment category and equivalent staffing ratio; small differences reflect precision and aggregation.",

  "Net Vocational Division I Positions",
  "Both sides combine vocational positions and the vocational deduction as one net quantity.",

  "Principal",
  "The proposed model removes the 15-unit threshold and provides one position per school calculation unit.",

  "Assistant Principal",
  "The proposed thresholds use a broader position base and are applied separately to each school calculation unit.",

  "Administrative Support Professionals",
  "Current Secretaries are compared with the broader proposed Administrative Support Professionals category.",

  "Instructional Supports",
  "The current side combines separately funded support functions; the proposed side equals 20% of Base Division I positions.",

  "Superintendent",
  "The current model funds districts only; the proposed model provides one position per included LEA.",

  "Administrative Assistant",
  "Both models provide one position per included LEA.",

  "Assistant Superintendent",
  "The current formula uses reported Division I units; the proposed formula uses Total Base positions and includes charters.",

  "Director",
  "The same thresholds are applied to reported Division I units currently and Total Base positions under the proposal.",

  "11-Month Supervisor",
  "Both models retain fractions, but the current formula uses Division I units and the proposed formula uses Total Base positions.",

  "Food Services Supervisor",
  "The current quantity is incomplete for some districts; the proposed rule expands eligibility to all included LEAs and adds a scale adjustment.",

  "Transportation Supervisor",
  "Both models divide enrollment by 7,500 and retain fractional positions.",

  "Reading Cadre",
  "The current model funds districts only; the proposed model provides one position per included LEA."
)

if (!setequal(
  interpretation_reference$ComparisonCategory,
  comparable_categories
)) {
  stop(
    paste(
      "The interpretation guide must contain exactly the categories in the",
      "comparable subtotal."
    ),
    call. = FALSE
  )
}

category_guide <- category_order |>
  left_join(category_status, by = "ComparisonCategory") |>
  left_join(current_treatment_summary, by = "ComparisonCategory") |>
  left_join(proposed_treatment_summary, by = "ComparisonCategory") |>
  left_join(interpretation_reference, by = "ComparisonCategory") |>
  mutate(
    CurrentTreatment = if_else(
      ComparisonCategory == "Instructional Supports",
      "Separately funded current support functions; see the Instructional Supports tab.",
      CurrentTreatment
    ),
    ProposedTreatment = if_else(
      ComparisonCategory == "Instructional Supports",
      "One consolidated allocation equal to 20% of Base Division I positions.",
      ProposedTreatment
    )
  ) |>
  arrange(CategoryOrder, ComparisonCategory) |>
  transmute(
    `Category` = ComparisonCategory,
    `Status` = ComparisonStatus,
    `Current Treatment` = CurrentTreatment,
    `Proposed Treatment` = ProposedTreatment,
    `Comparison Note` = ComparisonNote
  )

# Map funding detail -----------------------------------------------------------

map_detail <- function(detail, source_model) {
  map <- comparison_map |>
    filter(SourceModel == source_model) |>
    distinct(SourceComponent, ComparisonCategory)

  detail |>
    inner_join(map, by = c("Component" = "SourceComponent"))
}

current_comparable <- map_detail(current_detail, "Current")
proposed_comparable <- map_detail(proposed_detail, "Proposed")

# LEA metadata -----------------------------------------------------------------

lea_metadata <- bind_rows(
  current_detail |>
    distinct(
      SchoolYear,
      CountDate,
      DistrictCode,
      DistrictName,
      LEAType
    ),
  proposed_detail |>
    distinct(
      SchoolYear,
      CountDate,
      DistrictCode,
      DistrictName,
      LEAType
    )
)

metadata_conflicts <- lea_metadata |>
  group_by(DistrictCode) |>
  summarise(
    Names = n_distinct(DistrictName),
    Types = n_distinct(LEAType),
    SchoolYears = n_distinct(SchoolYear),
    CountDates = n_distinct(CountDate),
    .groups = "drop"
  ) |>
  filter(Names > 1 | Types > 1 | SchoolYears > 1 | CountDates > 1)

stop_if_rows(
  metadata_conflicts,
  "Current and proposed LEA metadata do not agree."
)

lea_universe <- lea_metadata |>
  distinct(DistrictCode, .keep_all = TRUE)

# Long-format category detail --------------------------------------------------

summarize_lea_category <- function(data, prefix) {
  output <- data |>
    group_by(DistrictCode, ComparisonCategory) |>
    summarise(
      Positions = sum(FundingQuantity, na.rm = TRUE),
      Funding = sum(
        FundingAmount[FundingComplete & !is.na(FundingAmount)],
        na.rm = TRUE
      ),
      MissingAmountRows = sum(!FundingComplete | is.na(FundingAmount)),
      .groups = "drop"
    )

  names(output)[-(1:2)] <- paste0(prefix, names(output)[-(1:2)])
  output
}

current_category <- summarize_lea_category(
  current_comparable,
  "Current"
)
proposed_category <- summarize_lea_category(
  proposed_comparable,
  "Proposed"
)

category_metadata <- category_order |>
  left_join(category_status, by = "ComparisonCategory")

lea_category <- tidyr::crossing(
  DistrictCode = lea_universe$DistrictCode,
  ComparisonCategory = category_metadata$ComparisonCategory
) |>
  left_join(lea_universe, by = "DistrictCode") |>
  left_join(category_metadata, by = "ComparisonCategory") |>
  left_join(
    current_category,
    by = c("DistrictCode", "ComparisonCategory")
  ) |>
  left_join(
    proposed_category,
    by = c("DistrictCode", "ComparisonCategory")
  ) |>
  mutate(
    across(
      c(
        CurrentPositions,
        CurrentFunding,
        CurrentMissingAmountRows,
        ProposedPositions,
        ProposedFunding,
        ProposedMissingAmountRows
      ),
      ~ coalesce(.x, 0)
    ),
    PositionDifference = ProposedPositions - CurrentPositions,
    FundingDifference = ProposedFunding - CurrentFunding,
    PercentDifference = if_else(
      abs(CurrentFunding) > comparison_tolerance,
      FundingDifference / CurrentFunding,
      NA_real_
    ),
    Direction = case_when(
      FundingDifference > comparison_tolerance ~ "Increase",
      FundingDifference < -comparison_tolerance ~ "Decrease",
      TRUE ~ "No change"
    ),
    CurrentAmountComplete = if_else(
      CurrentMissingAmountRows == 0,
      "Yes",
      "No"
    ),
    LEAType = factor(LEAType, levels = c("District", "Charter"))
  ) |>
  group_by(DistrictCode) |>
  mutate(
    DriverRank = min_rank(desc(abs(FundingDifference)))
  ) |>
  ungroup() |>
  arrange(
    LEAType,
    DistrictName,
    CategoryOrder,
    ComparisonCategory
  )

expected_rows <- nrow(lea_universe) * nrow(category_metadata)

duplicate_rows <- lea_category |>
  count(DistrictCode, ComparisonCategory) |>
  filter(n != 1L)

stop_if_rows(
  duplicate_rows,
  "Expected exactly one row per LEA and comparison category."
)

if (nrow(lea_category) != expected_rows) {
  stop(
    "LEA-category row count does not equal LEAs multiplied by categories.",
    call. = FALSE
  )
}

category_detail <- lea_category |>
  transmute(
    `LEA Code` = DistrictCode,
    `LEA Name` = DistrictName,
    `LEA Type` = as.character(LEAType),
    `Category` = ComparisonCategory,
    `Known Current Positions` = CurrentPositions,
    `Proposed Positions` = ProposedPositions,
    `Position Difference` = PositionDifference,
    `Known Current Funding` = CurrentFunding,
    `Proposed Funding` = ProposedFunding,
    `Funding Difference` = FundingDifference,
    `Current Amount Complete` = CurrentAmountComplete,
    `Category Status` = ComparisonStatus,
    `Driver Rank Within LEA` = DriverRank
  )

# Instructional Supports detail ------------------------------------------------

instructional_support_category <- "Instructional Supports"

expected_instructional_support_components <- c(
  "Academic Excellence",
  "Counselor / Social Worker",
  "Driver Education Teacher",
  "Nurse",
  "Related Services Specialist - Basic",
  "Related Services Specialist - Complex",
  "Related Services Specialist - Intensive",
  "School Psychologist",
  "Visiting Teacher"
)

current_instructional_support_components <- comparison_map |>
  filter(
    SourceModel == "Current",
    ComparisonCategory == instructional_support_category
  ) |>
  distinct(SourceComponent) |>
  pull(SourceComponent)

proposed_instructional_support_components <- comparison_map |>
  filter(
    SourceModel == "Proposed",
    ComparisonCategory == instructional_support_category
  ) |>
  distinct(SourceComponent) |>
  pull(SourceComponent)

if (!setequal(
  current_instructional_support_components,
  expected_instructional_support_components
)) {
  stop(
    paste(
      "The current Instructional Supports component set changed.",
      "Update the detail-tab mapping before rerunning this script."
    ),
    call. = FALSE
  )
}

if (!identical(
  sort(proposed_instructional_support_components),
  "Instructional Supports"
)) {
  stop(
    "Expected one proposed Instructional Supports component.",
    call. = FALSE
  )
}

instructional_support_component_detail <- current_detail |>
  filter(Component %in% current_instructional_support_components) |>
  mutate(
    DisplayComponent = case_when(
      Component == "Counselor / Social Worker" ~
        "Counselors / Social Workers",
      Component == "School Psychologist" ~
        "School Psychologists",
      Component == "Nurse" ~ "Nurses",
      Component == "Academic Excellence" ~ "Academic Excellence",
      Component == "Driver Education Teacher" ~ "Driver Education",
      str_starts(Component, "Related Services Specialist") ~
        "Related Services",
      Component == "Visiting Teacher" ~ "Visiting Teachers",
      TRUE ~ Component
    )
  ) |>
  group_by(DistrictCode, DisplayComponent) |>
  summarise(Positions = sum(FundingQuantity, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from = DisplayComponent,
    values_from = Positions,
    values_fill = 0
  )

instructional_support_current_totals <- current_detail |>
  filter(Component %in% current_instructional_support_components) |>
  group_by(DistrictCode) |>
  summarise(
    CurrentPositions = sum(FundingQuantity, na.rm = TRUE),
    CurrentFunding = sum(
      FundingAmount[FundingComplete & !is.na(FundingAmount)],
      na.rm = TRUE
    ),
    CurrentMissingAmountRows = sum(
      !FundingComplete | is.na(FundingAmount)
    ),
    .groups = "drop"
  )

instructional_support_proposed_totals <- proposed_detail |>
  filter(Component %in% proposed_instructional_support_components) |>
  group_by(DistrictCode) |>
  summarise(
    ProposedPositions = sum(FundingQuantity, na.rm = TRUE),
    ProposedFunding = sum(
      FundingAmount[FundingComplete & !is.na(FundingAmount)],
      na.rm = TRUE
    ),
    ProposedMissingAmountRows = sum(
      !FundingComplete | is.na(FundingAmount)
    ),
    .groups = "drop"
  )

instructional_supports <- lea_universe |>
  left_join(
    instructional_support_component_detail,
    by = "DistrictCode"
  ) |>
  left_join(
    instructional_support_current_totals,
    by = "DistrictCode"
  ) |>
  left_join(
    instructional_support_proposed_totals,
    by = "DistrictCode"
  ) |>
  mutate(
    across(
      c(
        `Counselors / Social Workers`,
        `School Psychologists`,
        Nurses,
        `Academic Excellence`,
        `Driver Education`,
        `Related Services`,
        `Visiting Teachers`,
        CurrentPositions,
        CurrentFunding,
        CurrentMissingAmountRows,
        ProposedPositions,
        ProposedFunding,
        ProposedMissingAmountRows
      ),
      ~ coalesce(.x, 0)
    ),
    PositionDifference = ProposedPositions - CurrentPositions,
    FundingDifference = ProposedFunding - CurrentFunding,
    LEAType = factor(LEAType, levels = c("District", "Charter"))
  ) |>
  arrange(LEAType, DistrictName) |>
  transmute(
    `LEA Code` = DistrictCode,
    `LEA Name` = DistrictName,
    `LEA Type` = as.character(LEAType),
    `Counselors / Social Workers`,
    `School Psychologists`,
    Nurses,
    `Academic Excellence`,
    `Driver Education`,
    `Related Services`,
    `Visiting Teachers`,
    `Current Total Positions` = CurrentPositions,
    `Proposed Positions` = ProposedPositions,
    `Position Difference` = PositionDifference,
    `Current Funding` = CurrentFunding,
    `Proposed Funding` = ProposedFunding,
    `Funding Difference` = FundingDifference
  )

instructional_support_reconciliation <- lea_category |>
  filter(ComparisonCategory == instructional_support_category) |>
  summarise(
    CurrentPositions = sum(CurrentPositions),
    ProposedPositions = sum(ProposedPositions),
    CurrentFunding = sum(CurrentFunding),
    ProposedFunding = sum(ProposedFunding)
  )

stopifnot(
  nrow(instructional_supports) == 43L,
  anyDuplicated(instructional_supports$`LEA Code`) == 0L,
  sum(instructional_support_current_totals$CurrentMissingAmountRows) == 0L,
  sum(instructional_support_proposed_totals$ProposedMissingAmountRows) == 0L,
  abs(
    sum(instructional_supports$`Current Total Positions`) -
      instructional_support_reconciliation$CurrentPositions
  ) <= comparison_tolerance,
  abs(
    sum(instructional_supports$`Proposed Positions`) -
      instructional_support_reconciliation$ProposedPositions
  ) <= comparison_tolerance,
  abs(
    sum(instructional_supports$`Current Funding`) -
      instructional_support_reconciliation$CurrentFunding
  ) <= comparison_tolerance,
  abs(
    sum(instructional_supports$`Proposed Funding`) -
      instructional_support_reconciliation$ProposedFunding
  ) <= comparison_tolerance
)

# LEA summary ------------------------------------------------------------------

lea_summary <- lea_category |>
  group_by(
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType
  ) |>
  summarise(
    CurrentFunding = sum(CurrentFunding),
    ProposedFunding = sum(ProposedFunding),
    IncompleteCurrentCategories = sum(CurrentAmountComplete == "No"),
    .groups = "drop"
  ) |>
  mutate(
    FundingDifference = ProposedFunding - CurrentFunding,
    PercentDifference = if_else(
      abs(CurrentFunding) > comparison_tolerance,
      FundingDifference / CurrentFunding,
      NA_real_
    ),
    Direction = case_when(
      FundingDifference > comparison_tolerance ~ "Increase",
      FundingDifference < -comparison_tolerance ~ "Decrease",
      TRUE ~ "No change"
    ),
    CurrentAmountComplete = if_else(
      IncompleteCurrentCategories == 0,
      "Yes",
      "No"
    ),
    LEAType = factor(LEAType, levels = c("District", "Charter"))
  ) |>
  arrange(LEAType, DistrictName) |>
  transmute(
    `School Year` = SchoolYear,
    `Count Date` = CountDate,
    `LEA Code` = DistrictCode,
    `LEA Name` = DistrictName,
    `LEA Type` = as.character(LEAType),
    `Known Current Comparable Funding` = CurrentFunding,
    `Proposed Comparable Funding` = ProposedFunding,
    `Funding Difference` = FundingDifference,
    `Percent Difference` = PercentDifference,
    `Direction` = Direction,
    `Current Amount Complete` = CurrentAmountComplete
  )

# Validation -------------------------------------------------------------------

stopifnot(
  nrow(lea_summary) == 43L,
  sum(lea_summary$`LEA Type` == "District") == 19L,
  sum(lea_summary$`LEA Type` == "Charter") == 24L,
  anyDuplicated(lea_summary$`LEA Code`) == 0L,
  basse_district_code %in% lea_summary$`LEA Code`,
  !dafb_district_code %in% lea_summary$`LEA Code`,
  nrow(category_detail) == expected_rows,
  nrow(instructional_supports) == 43L,
  sum(lea_category$ProposedMissingAmountRows) == 0L
)

reconciliation <- tibble(
  Check = c(
    "Current comparable funding",
    "Proposed comparable funding",
    "Current comparable positions",
    "Proposed comparable positions"
  ),
  Calculated = c(
    sum(lea_category$CurrentFunding),
    sum(lea_category$ProposedFunding),
    sum(lea_category$CurrentPositions),
    sum(lea_category$ProposedPositions)
  ),
  Expected = c(
    statewide$ComparableAmountCurrentFundingAmount,
    statewide$ComparableAmountProposedFundingAmount,
    statewide$ComparableAmountCurrentPositions,
    statewide$ComparableAmountProposedPositions
  )
) |>
  mutate(
    Difference = Calculated - Expected,
    Passed = abs(Difference) <= comparison_tolerance
  )

if (!all(reconciliation$Passed)) {
  print(reconciliation)
  stop(
    "Workbook totals do not reconcile to the statewide comparable subtotal.",
    call. = FALSE
  )
}

# Workbook ---------------------------------------------------------------------

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "LEA Summary", gridLines = FALSE)
openxlsx::addWorksheet(wb, "Category Detail", gridLines = FALSE)
openxlsx::addWorksheet(wb, "Instructional Supports", gridLines = FALSE)
openxlsx::addWorksheet(wb, "Category Guide", gridLines = FALSE)

header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#2C7FB8",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)
wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

# LEA Summary ------------------------------------------------------------------

openxlsx::writeDataTable(
  wb,
  "LEA Summary",
  lea_summary,
  tableName = "LEASummary",
  tableStyle = "TableStyleMedium2"
)
openxlsx::freezePane(wb, "LEA Summary", firstRow = TRUE)
openxlsx::addStyle(
  wb,
  "LEA Summary",
  header_style,
  rows = 1,
  cols = 1:ncol(lea_summary),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::setRowHeights(wb, "LEA Summary", rows = 1, heights = 40)
openxlsx::setColWidths(
  wb,
  "LEA Summary",
  cols = 1:ncol(lea_summary),
  widths = c(11, 12, 10, 45, 12, 25, 25, 18, 17, 12, 22)
)

summary_rows <- 2:(nrow(lea_summary) + 1)
openxlsx::addStyle(
  wb,
  "LEA Summary",
  openxlsx::createStyle(numFmt = "mm/dd/yyyy"),
  rows = summary_rows,
  cols = which(names(lea_summary) == "Count Date"),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::addStyle(
  wb,
  "LEA Summary",
  openxlsx::createStyle(numFmt = "$#,##0;[Red]-$#,##0"),
  rows = summary_rows,
  cols = which(str_detect(names(lea_summary), "Funding")),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::addStyle(
  wb,
  "LEA Summary",
  openxlsx::createStyle(numFmt = "0.0%;[Red]-0.0%"),
  rows = summary_rows,
  cols = which(names(lea_summary) == "Percent Difference"),
  gridExpand = TRUE,
  stack = TRUE
)

# Category Detail --------------------------------------------------------------

openxlsx::writeDataTable(
  wb,
  "Category Detail",
  category_detail,
  tableName = "LEACategoryDetail",
  tableStyle = "TableStyleMedium2"
)
openxlsx::freezePane(wb, "Category Detail", firstRow = TRUE)
openxlsx::addStyle(
  wb,
  "Category Detail",
  header_style,
  rows = 1,
  cols = 1:ncol(category_detail),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::setRowHeights(wb, "Category Detail", rows = 1, heights = 45)
openxlsx::setColWidths(
  wb,
  "Category Detail",
  cols = 1:ncol(category_detail),
  widths = c(
    10, 45, 12, 36,
    20, 18, 18,
    20, 18, 18,
    22, 18, 19
  )
)

detail_rows <- 2:(nrow(category_detail) + 1)
openxlsx::addStyle(
  wb,
  "Category Detail",
  openxlsx::createStyle(numFmt = "0.00;[Red]-0.00"),
  rows = detail_rows,
  cols = which(str_detect(names(category_detail), "Positions|Position Difference")),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::addStyle(
  wb,
  "Category Detail",
  openxlsx::createStyle(numFmt = "$#,##0;[Red]-$#,##0"),
  rows = detail_rows,
  cols = which(str_detect(names(category_detail), "Funding")),
  gridExpand = TRUE,
  stack = TRUE
)

# Instructional Supports -------------------------------------------------------

instructional_support_notes <- c(
  paste0(
    "Current component columns show positions for the separately funded support functions combined in the current comparison. ",
    "Proposed Instructional Supports equals 20% of Base Division I positions."
  ),
  paste(
    "The position and funding differences are broad functional comparisons,",
    "not one-to-one reductions in identical positions or services."
  )
)

openxlsx::mergeCells(
  wb,
  "Instructional Supports",
  cols = 1:ncol(instructional_supports),
  rows = 1
)
openxlsx::writeData(
  wb,
  "Instructional Supports",
  "How to read this sheet",
  startRow = 1,
  colNames = FALSE
)
openxlsx::addStyle(
  wb,
  "Instructional Supports",
  openxlsx::createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#2C7FB8",
    textDecoration = "bold"
  ),
  rows = 1,
  cols = 1:ncol(instructional_supports),
  gridExpand = TRUE
)

for (i in seq_along(instructional_support_notes)) {
  row <- i + 2
  openxlsx::mergeCells(
    wb,
    "Instructional Supports",
    cols = 1:ncol(instructional_supports),
    rows = row
  )
  openxlsx::writeData(
    wb,
    "Instructional Supports",
    instructional_support_notes[i],
    startRow = row,
    colNames = FALSE
  )
  openxlsx::addStyle(
    wb,
    "Instructional Supports",
    wrap_style,
    rows = row,
    cols = 1:ncol(instructional_supports),
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::setRowHeights(
    wb,
    "Instructional Supports",
    rows = row,
    heights = 28
  )
}

instructional_support_start_row <- length(instructional_support_notes) + 4

openxlsx::writeDataTable(
  wb,
  "Instructional Supports",
  instructional_supports,
  startRow = instructional_support_start_row,
  tableName = "InstructionalSupportsDetail",
  tableStyle = "TableStyleMedium2"
)
openxlsx::addStyle(
  wb,
  "Instructional Supports",
  header_style,
  rows = instructional_support_start_row,
  cols = 1:ncol(instructional_supports),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::setRowHeights(
  wb,
  "Instructional Supports",
  rows = instructional_support_start_row,
  heights = 45
)
openxlsx::freezePane(
  wb,
  "Instructional Supports",
  firstActiveRow = instructional_support_start_row + 1
)
openxlsx::setColWidths(
  wb,
  "Instructional Supports",
  cols = 1:ncol(instructional_supports),
  widths = c(
    10, 45, 12,
    22, 20, 12, 20, 18, 18, 16,
    20, 18, 18,
    18, 18, 18
  )
)

instructional_support_rows <-
  (instructional_support_start_row + 1):(
    instructional_support_start_row + nrow(instructional_supports)
  )

openxlsx::addStyle(
  wb,
  "Instructional Supports",
  openxlsx::createStyle(numFmt = "0.00;[Red]-0.00"),
  rows = instructional_support_rows,
  cols = which(names(instructional_supports) %in% c(
    "Counselors / Social Workers",
    "School Psychologists",
    "Nurses",
    "Academic Excellence",
    "Driver Education",
    "Related Services",
    "Visiting Teachers",
    "Current Total Positions",
    "Proposed Positions",
    "Position Difference"
  )),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::addStyle(
  wb,
  "Instructional Supports",
  openxlsx::createStyle(numFmt = "$#,##0;[Red]-$#,##0"),
  rows = instructional_support_rows,
  cols = which(str_detect(names(instructional_supports), "Funding")),
  gridExpand = TRUE,
  stack = TRUE
)

# Category Guide ---------------------------------------------------------------

notes <- c(
  paste0(
    "Scope: 43 LEAs (19 districts and 24 charters). ",
    primary_reporting_scope_short,
    "."
  ),
  paste0(
    "Comparable subtotal: ",
    length(comparable_categories),
    " categories with funding amounts available on both sides. Buildings and Grounds Supervisor is excluded; Food Services Supervisor remains provisional."
  ),
  "Broad functional crosswalks support comparison but do not mean that current and proposed positions or services are identical. The workbook passed its internal validation checks."
)

openxlsx::mergeCells(wb, "Category Guide", cols = 1:5, rows = 1)
openxlsx::writeData(
  wb,
  "Category Guide",
  "Scope and Interpretation Notes",
  startRow = 1,
  colNames = FALSE
)
openxlsx::addStyle(
  wb,
  "Category Guide",
  openxlsx::createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#2C7FB8",
    textDecoration = "bold"
  ),
  rows = 1,
  cols = 1:5,
  gridExpand = TRUE
)

for (i in seq_along(notes)) {
  row <- i + 2
  openxlsx::mergeCells(wb, "Category Guide", cols = 1:5, rows = row)
  openxlsx::writeData(
    wb,
    "Category Guide",
    notes[i],
    startRow = row,
    colNames = FALSE
  )
  openxlsx::addStyle(
    wb,
    "Category Guide",
    wrap_style,
    rows = row,
    cols = 1:5,
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::setRowHeights(wb, "Category Guide", rows = row, heights = 30)
}

guide_start_row <- length(notes) + 4
openxlsx::writeDataTable(
  wb,
  "Category Guide",
  category_guide,
  startRow = guide_start_row,
  tableName = "CategoryGuide",
  tableStyle = "TableStyleMedium2"
)
openxlsx::addStyle(
  wb,
  "Category Guide",
  header_style,
  rows = guide_start_row,
  cols = 1:ncol(category_guide),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::addStyle(
  wb,
  "Category Guide",
  wrap_style,
  rows = (guide_start_row + 1):(guide_start_row + nrow(category_guide)),
  cols = 1:ncol(category_guide),
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::setRowHeights(
  wb,
  "Category Guide",
  rows = (guide_start_row + 1):(guide_start_row + nrow(category_guide)),
  heights = 84
)
openxlsx::setColWidths(
  wb,
  "Category Guide",
  cols = 1:ncol(category_guide),
  widths = c(36, 18, 85, 75, 70)
)
openxlsx::freezePane(
  wb,
  "Category Guide",
  firstActiveRow = guide_start_row + 1
)

# Save -------------------------------------------------------------------------

openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

message("Created: ", output_path)
message("LEA rows: ", nrow(lea_summary), ".")
message(
  "Category-detail rows: ",
  nrow(category_detail),
  " (",
  nrow(lea_summary),
  " LEAs x ",
  nrow(category_guide),
  " categories)."
)
message("Instructional Supports rows: ", nrow(instructional_supports), ".")
message(
  "Comparable totals: current $",
  format(round(sum(lea_summary$`Known Current Comparable Funding`)), big.mark = ","),
  "; proposed $",
  format(round(sum(lea_summary$`Proposed Comparable Funding`)), big.mark = ","),
  "."
)
