# =============================================================================
# comparable_staffing_lea_comparison.R
# =============================================================================
# Creates one workbook with:
#   - LEA Comparison: one row per in-scope LEA
#   - Data Dictionary: column definitions and scope notes
#
# Run from the school_funding_model project root after the main pipeline.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install the openxlsx package before running this script.", call. = FALSE)
}

# Paths ------------------------------------------------------------------------

current_path <- file.path(intermediate_dir, "05_current_model_funding_detail.csv")
proposed_path <- file.path(intermediate_dir, "08_proposed_model_funding_detail.csv")
component_path <- file.path(final_dir, "11_staffing_component_comparison.csv")
statewide_path <- file.path(final_dir, "11_staffing_statewide_comparison.csv")
output_path <- file.path(final_dir, "comparable_staffing_lea_comparison.xlsx")

check_required_files(c(
  current_path,
  proposed_path,
  component_path,
  statewide_path,
  model_comparison_crosswalk_path
))

# Read inputs ------------------------------------------------------------------

read_detail <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(
      DistrictCode = as.integer(DistrictCode),
      IncludeInStatewide = as.logical(IncludeInStatewide),
      FundingComplete = coalesce(as.logical(FundingComplete), FALSE)
    ) |>
    filter(IncludeInStatewide)
}

current_detail <- read_detail(current_path)
proposed_detail <- read_detail(proposed_path)

component_comparison <- read_csv(component_path, show_col_types = FALSE) |>
  mutate(
    IncludedInComparableAmountSubtotal = as.logical(
      IncludedInComparableAmountSubtotal
    ),
    IsCompleteForFinalComparison = as.logical(IsCompleteForFinalComparison)
  ) |>
  filter(AnalysisSection == "Staffing rules")

crosswalk <- read_csv(
  model_comparison_crosswalk_path,
  show_col_types = FALSE
) |>
  filter(AnalysisSection == "Staffing rules")

statewide <- read_csv(statewide_path, show_col_types = FALSE) |>
  filter(AnalysisSection == "Staffing rules")

if (nrow(statewide) != 1) {
  stop("Expected one staffing-rules row in the statewide output.", call. = FALSE)
}

# Comparable categories --------------------------------------------------------

comparable_categories <- component_comparison |>
  filter(IncludedInComparableAmountSubtotal) |>
  distinct(ComparisonCategory) |>
  pull(ComparisonCategory)

provisional_categories <- component_comparison |>
  filter(
    IncludedInComparableAmountSubtotal,
    !IsCompleteForFinalComparison
  ) |>
  distinct(ComparisonCategory) |>
  pull(ComparisonCategory)

map_detail <- function(detail, source_model) {
  component_map <- crosswalk |>
    filter(
      SourceModel == source_model,
      ComparisonCategory %in% comparable_categories
    ) |>
    distinct(SourceComponent, ComparisonCategory)

  duplicate_mappings <- component_map |>
    count(SourceComponent) |>
    filter(n > 1)

  stop_if_rows(
    duplicate_mappings,
    paste(source_model, "components must map to only one comparison category.")
  )

  detail |>
    inner_join(component_map, by = c("Component" = "SourceComponent"))
}

current_comparable <- map_detail(current_detail, "Current")
proposed_comparable <- map_detail(proposed_detail, "Proposed")

missing_current_categories <- setdiff(
  comparable_categories,
  unique(current_comparable$ComparisonCategory)
)
missing_proposed_categories <- setdiff(
  comparable_categories,
  unique(proposed_comparable$ComparisonCategory)
)

if (length(missing_current_categories) > 0 ||
    length(missing_proposed_categories) > 0) {
  stop(
    "One or more comparable categories are missing from the mapped detail data.",
    call. = FALSE
  )
}

# LEA totals -------------------------------------------------------------------

summarize_lea <- function(data, prefix) {
  output <- data |>
    group_by(DistrictCode) |>
    summarise(
      Positions = sum(FundingQuantity, na.rm = TRUE),
      Funding = sum(
        FundingAmount[FundingComplete & !is.na(FundingAmount)],
        na.rm = TRUE
      ),
      MissingAmountRows = sum(!FundingComplete | is.na(FundingAmount)),
      .groups = "drop"
    )

  names(output)[-1] <- paste0(prefix, names(output)[-1])
  output
}

current_lea <- summarize_lea(current_comparable, "Current")
proposed_lea <- summarize_lea(proposed_comparable, "Proposed")

lea_metadata <- bind_rows(
  current_detail |>
    distinct(SchoolYear, CountDate, DistrictCode, DistrictName, LEAType),
  proposed_detail |>
    distinct(SchoolYear, CountDate, DistrictCode, DistrictName, LEAType)
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

lea_audit <- lea_universe |>
  left_join(current_lea, by = "DistrictCode") |>
  left_join(proposed_lea, by = "DistrictCode") |>
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
  arrange(LEAType, DistrictName)

lea_comparison <- lea_audit |>
  transmute(
    `School Year` = as.integer(SchoolYear),
    `Count Date` = as.Date(CountDate),
    `LEA Code` = DistrictCode,
    `LEA Name` = DistrictName,
    `LEA Type` = as.character(LEAType),
    `Current Comparable Funding` = CurrentFunding,
    `Proposed Comparable Funding` = ProposedFunding,
    `Funding Difference` = FundingDifference,
    `Percent Difference` = PercentDifference,
    Direction,
    `Current Amount Complete` = CurrentAmountComplete
  )

# Validation -------------------------------------------------------------------

stopifnot(
  nrow(lea_comparison) == 43,
  sum(lea_comparison$`LEA Type` == "District") == 19,
  sum(lea_comparison$`LEA Type` == "Charter") == 24,
  anyDuplicated(lea_comparison$`LEA Code`) == 0,
  basse_district_code %in% lea_comparison$`LEA Code`,
  !dafb_district_code %in% lea_comparison$`LEA Code`,
  sum(lea_audit$ProposedMissingAmountRows) == 0
)

reconciliation <- tibble(
  Check = c(
    "Current comparable funding",
    "Proposed comparable funding",
    "Current comparable positions",
    "Proposed comparable positions"
  ),
  Calculated = c(
    sum(lea_audit$CurrentFunding),
    sum(lea_audit$ProposedFunding),
    sum(lea_audit$CurrentPositions),
    sum(lea_audit$ProposedPositions)
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
  stop("LEA totals do not reconcile to the statewide comparable subtotal.", call. = FALSE)
}

# Data dictionary --------------------------------------------------------------

data_dictionary <- tribble(
  ~Column, ~Type, ~`Excel Format`, ~Description,
  "School Year", "Integer", "0", "School year represented by the analysis; 2026 denotes SY 2025-26.",
  "Count Date", "Date", "mm/dd/yyyy", "Student count date used in both IV&V model recreations.",
  "LEA Code", "Integer", "0", "Official DDOE LEA code.",
  "LEA Name", "Text", "Text", "Official LEA name used in the IV&V pipeline.",
  "LEA Type", "Text", "Text", "District or Charter.",
  "Current Comparable Funding", "Currency", "$#,##0", "Known funding under the recreated current model for categories in the comparable subtotal.",
  "Proposed Comparable Funding", "Currency", "$#,##0", "Known funding under the IV&V proposed model for categories in the comparable subtotal.",
  "Funding Difference", "Currency", "$#,##0", "Proposed comparable funding minus current comparable funding.",
  "Percent Difference", "Percentage", "0.0%", "Funding difference divided by current comparable funding.",
  "Direction", "Text", "Text", "Increase, decrease, or no change based on the funding difference.",
  "Current Amount Complete", "Text", "Text", "No means an unknown current amount was not imputed for that LEA."
)

notes <- c(
  paste0(
    "Scope: ", nrow(lea_comparison),
    " LEAs (19 districts and 24 charters). BASSE is included; DAFB is excluded."
  ),
  paste0(
    "Comparable subtotal: ", length(comparable_categories),
    " staffing categories included in the statewide comparable-amount definition."
  ),
  paste0(
    "Provisional categories remain included: ",
    paste(provisional_categories, collapse = ", "), "."
  ),
  paste(
    "Unknown current Food Services Supervisor amounts are not imputed;",
    "affected LEAs are flagged under Current Amount Complete."
  ),
  paste(
    "Validation: LEA totals reconcile to the comparable funding and position",
    "subtotals in 11_staffing_statewide_comparison.csv."
  ),
  paste(
    "Sources: 05_current_model_funding_detail.csv,",
    "08_proposed_model_funding_detail.csv,",
    "11_staffing_component_comparison.csv,",
    "11_staffing_statewide_comparison.csv, and model_comparison_crosswalk.csv."
  )
)

# Workbook ---------------------------------------------------------------------

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "LEA Comparison", gridLines = FALSE)
openxlsx::addWorksheet(wb, "Data Dictionary", gridLines = FALSE)

openxlsx::writeDataTable(
  wb,
  "LEA Comparison",
  lea_comparison,
  tableName = "LEAComparableFunding",
  tableStyle = "TableStyleMedium2"
)
openxlsx::freezePane(wb, "LEA Comparison", firstRow = TRUE)

body_rows <- 2:(nrow(lea_comparison) + 1)
add_num_style <- function(columns, format) {
  openxlsx::addStyle(
    wb,
    "LEA Comparison",
    openxlsx::createStyle(numFmt = format),
    rows = body_rows,
    cols = columns,
    gridExpand = TRUE,
    stack = TRUE
  )
}

add_num_style(which(names(lea_comparison) == "Count Date"), "mm/dd/yyyy")
add_num_style(which(names(lea_comparison) %in% c("School Year", "LEA Code")), "0")
add_num_style(which(str_detect(names(lea_comparison), "Funding")), "$#,##0;[Red]-$#,##0")
add_num_style(which(names(lea_comparison) == "Percent Difference"), "0.0%;[Red]-0.0%")

openxlsx::setColWidths(
  wb,
  "LEA Comparison",
  cols = 1:ncol(lea_comparison),
  widths = c(11, 12, 10, 38, 12, 22, 23, 18, 17, 12, 22)
)
openxlsx::setRowHeights(wb, "LEA Comparison", rows = 1, heights = 28)

header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#2C7FB8",
  textDecoration = "bold"
)
wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

openxlsx::mergeCells(wb, "Data Dictionary", cols = 1:4, rows = 1)
openxlsx::writeData(wb, "Data Dictionary", "Scope and Method Notes", startRow = 1, colNames = FALSE)
openxlsx::addStyle(wb, "Data Dictionary", header_style, rows = 1, cols = 1:4, gridExpand = TRUE)

for (i in seq_along(notes)) {
  row <- i + 2
  openxlsx::mergeCells(wb, "Data Dictionary", cols = 1:4, rows = row)
  openxlsx::writeData(wb, "Data Dictionary", notes[i], startRow = row, colNames = FALSE)
  openxlsx::addStyle(wb, "Data Dictionary", wrap_style, rows = row, cols = 1:4, gridExpand = TRUE)
  openxlsx::setRowHeights(wb, "Data Dictionary", rows = row, heights = 32)
}

dictionary_row <- length(notes) + 4
openxlsx::writeDataTable(
  wb,
  "Data Dictionary",
  data_dictionary,
  startRow = dictionary_row,
  tableName = "LEAComparableDictionary",
  tableStyle = "TableStyleMedium2"
)
openxlsx::addStyle(
  wb,
  "Data Dictionary",
  wrap_style,
  rows = (dictionary_row + 1):(dictionary_row + nrow(data_dictionary)),
  cols = 1:4,
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::freezePane(wb, "Data Dictionary", firstActiveRow = dictionary_row + 1)
openxlsx::setColWidths(wb, "Data Dictionary", cols = 1:4, widths = c(32, 14, 17, 85))

openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

message("Created: ", output_path)
message("Rows: ", nrow(lea_comparison), " LEAs.")
message(
  "Comparable totals: current $",
  format(round(sum(lea_comparison$`Current Comparable Funding`)), big.mark = ","),
  "; proposed $",
  format(round(sum(lea_comparison$`Proposed Comparable Funding`)), big.mark = ","),
  "."
)
