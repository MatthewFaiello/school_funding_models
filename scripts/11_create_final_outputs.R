# =============================================================================
# 11_create_final_outputs.R
# =============================================================================
# Packages the report-ready outputs.
#
# All available calculations are retained. Readiness is reported separately
# from technical QC so unresolved policy questions do not look like pipeline
# failures.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

input_paths <- c(
  staffing_statewide = file.path(
    intermediate_dir,
    "09_staffing_statewide_comparison.csv"
  ),
  staffing_components = file.path(
    intermediate_dir,
    "09_staffing_component_comparison.csv"
  ),
  staffing_leas = file.path(
    intermediate_dir,
    "09_staffing_lea_comparison.csv"
  ),
  weighted_statewide = file.path(
    intermediate_dir,
    "09_opportunity_operational_comparison.csv"
  ),
  weighted_leas = file.path(
    intermediate_dir,
    "09_opportunity_operational_lea_comparison.csv"
  ),
  pefc_reconciliation = file.path(
    audit_dir,
    "10_pefc_reconciliation_summary.csv"
  ),
  charter_buildings = file.path(
    audit_dir,
    "10_charter_building_treatment.csv"
  ),
  outside_formula = file.path(
    intermediate_dir,
    "04_current_outside_formula_components.csv"
  ),
  district_cafeteria = file.path(
    intermediate_dir,
    "03_current_district_cafeteria_allocation.csv"
  ),
  dafb_scope_discrepancy = file.path(
    audit_dir,
    "10_pefc_dafb_scope_discrepancy.csv"
  ),
  comparison_qc = file.path(audit_dir, "09_comparison_qc.csv"),
  pefc_qc = file.path(audit_dir, "10_pefc_qc.csv"),
  current_issues = file.path(
    intermediate_dir,
    "04_current_model_issues.csv"
  ),
  proposed_issues = file.path(
    intermediate_dir,
    "07_proposed_model_issues.csv"
  ),
  comparison_crosswalk = model_comparison_crosswalk_path,
  lea_crosswalk = lea_crosswalk_path
)

check_required_files(input_paths)

staffing_statewide <- read_csv(
  input_paths[["staffing_statewide"]],
  show_col_types = FALSE
)
staffing_components <- read_csv(
  input_paths[["staffing_components"]],
  show_col_types = FALSE
)
staffing_leas <- read_csv(
  input_paths[["staffing_leas"]],
  show_col_types = FALSE
)
weighted_statewide <- read_csv(
  input_paths[["weighted_statewide"]],
  show_col_types = FALSE
)
weighted_leas <- read_csv(
  input_paths[["weighted_leas"]],
  show_col_types = FALSE
)
pefc_reconciliation <- read_csv(
  input_paths[["pefc_reconciliation"]],
  show_col_types = FALSE
)
charter_buildings <- read_csv(
  input_paths[["charter_buildings"]],
  show_col_types = FALSE
)
outside_formula <- read_csv(
  input_paths[["outside_formula"]],
  show_col_types = FALSE
)
district_cafeteria <- read_csv(
  input_paths[["district_cafeteria"]],
  show_col_types = FALSE
)
dafb_scope_discrepancy <- read_csv(
  input_paths[["dafb_scope_discrepancy"]],
  show_col_types = FALSE
)
comparison_qc <- read_csv(
  input_paths[["comparison_qc"]],
  show_col_types = FALSE
)
pefc_qc <- read_csv(
  input_paths[["pefc_qc"]],
  show_col_types = FALSE
)
current_issues <- read_csv(
  input_paths[["current_issues"]],
  show_col_types = FALSE
)
proposed_issues <- read_csv(
  input_paths[["proposed_issues"]],
  show_col_types = FALSE
)
comparison_crosswalk <- read_csv(
  input_paths[["comparison_crosswalk"]],
  show_col_types = FALSE
)
lea_crosswalk <- read_csv(
  input_paths[["lea_crosswalk"]],
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide)
  )

expected_primary_district_count <- lea_crosswalk |>
  filter(LEAType == "District", IncludeInStatewide) |>
  distinct(DistrictCode) |>
  nrow()

final_paths <- c(
  staffing_statewide = file.path(
    final_dir,
    "11_staffing_statewide_comparison.csv"
  ),
  staffing_components = file.path(
    final_dir,
    "11_staffing_component_comparison.csv"
  ),
  staffing_leas = file.path(
    final_dir,
    "11_staffing_lea_comparison.csv"
  ),
  weighted_statewide = file.path(
    final_dir,
    "11_opportunity_operational_comparison.csv"
  ),
  weighted_leas = file.path(
    final_dir,
    "11_opportunity_operational_lea_comparison.csv"
  ),
  pefc_reconciliation = file.path(
    final_dir,
    "11_pefc_reconciliation_summary.csv"
  ),
  charter_buildings = file.path(
    final_dir,
    "11_charter_building_treatment.csv"
  ),
  outside_formula = file.path(
    final_dir,
    "11_current_outside_formula_components.csv"
  ),
  district_cafeteria = file.path(
    final_dir,
    "11_current_district_cafeteria_allocation.csv"
  ),
  dafb_scope_discrepancy = file.path(
    final_dir,
    "11_pefc_dafb_scope_discrepancy.csv"
  ),
  open_items = file.path(
    final_dir,
    "11_open_items_and_assumptions.csv"
  ),
  final_qc = file.path(
    final_dir,
    "11_final_qc.csv"
  ),
  final_readiness = file.path(
    final_dir,
    "11_final_readiness.csv"
  )
)


# PEFC SUMMARY ------------------------------------------------------------------

# Component detail stays in audit/10_pefc_component_comparison.csv. The final
# summary keeps scope, statewide, and normalized comparisons only.
pefc_reconciliation_final <- pefc_reconciliation |>
  filter(ComparisonType != "Component formula/input comparison") |>
  mutate(
    ComparisonOrder = case_when(
      ComparisonType == "Scope-treatment discrepancy" ~ 1L,
      ComparisonType == "Scope alignment" ~ 2L,
      ComparisonType %in% c(
        "Statewide-summary reconciliation",
        "Statewide-summary discrepancy"
      ) ~ 3L,
      ComparisonType == "Scope and formula comparison" ~ 4L,
      ComparisonType == "Primary-scope normalized comparison" ~ 5L,
      TRUE ~ 9L
    ),
    FundingOrder = case_when(
      FundingMetric == "DAFB treatment across PEFC funding sections" ~ 1L,
      FundingMetric == "Organizations in primary comparison" ~ 2L,
      FundingMetric == "Base Funding" ~ 10L,
      FundingMetric == "Central Office Funding" ~ 20L,
      FundingMetric == "Position-based Funding" ~ 30L,
      FundingMetric == "Opportunity Funding" ~ 40L,
      FundingMetric == "Operational Funding" ~ 50L,
      FundingMetric == "Total modeled funding" ~ 60L,
      TRUE ~ 99L
    )
  ) |>
  arrange(ComparisonOrder, FundingOrder) |>
  select(-ComparisonOrder, -FundingOrder)


# OPEN ITEMS AND MATERIAL ASSUMPTIONS -------------------------------------------

crosswalk_items <- comparison_crosswalk |>
  mutate(
    IncludeInFinal = as.logical(IncludeInFinal),
    OutstandingQuestion = coalesce(OutstandingQuestion, ""),
    Notes = coalesce(Notes, ""),
    HasOpenItem =
      OutstandingQuestion != "" |
      !MappingStatus %in% c("Confirmed", "Not applicable") |
      !QuantityStatus %in% c("Confirmed", "Not required", "Not applicable") |
      !RateStatus %in% c("Confirmed", "Not applicable")
  ) |>
  filter(HasOpenItem) |>
  group_by(
    AnalysisSection,
    ComparisonCategory,
    SourceModel,
    MappingStatus,
    QuantityStatus,
    RateStatus,
    IncludeInFinal,
    OutstandingQuestion
  ) |>
  summarise(
    SourceComponents = paste(sort(unique(SourceComponent)), collapse = " | "),
    Notes = paste(unique(Notes[Notes != ""]), collapse = " | "),
    .groups = "drop"
  ) |>
  mutate(
    RecordSource = "Comparison crosswalk",
    RecordType = "Open item",
    Status = case_when(
      QuantityStatus == "Missing" | RateStatus == "Missing" ~ "Missing",
      MappingStatus == "Provisional" |
        QuantityStatus == "Provisional" |
        RateStatus == "Provisional" ~ "Provisional",
      TRUE ~ "Pending confirmation"
    ),
    Priority = case_when(
      Status == "Missing" ~ "Needs data or rate",
      Status == "Provisional" ~ "Needs confirmation",
      TRUE ~ "Needs review"
    ),
    SourceModelLabel = case_when(
      SourceModel == "Current" ~ current_model_label,
      SourceModel == "Proposed" ~ proposed_model_label,
      TRUE ~ SourceModel
    ),
    Issue = OutstandingQuestion,
    CurrentTreatment = Notes,
    Action = if_else(
      IncludeInFinal,
      "Update the maintained crosswalk or input when confirmation is received.",
      "Document outside the directly comparable staffing total."
    )
  ) |>
  transmute(
    RecordSource,
    AnalysisSection,
    RecordType,
    Status,
    Priority,
    ComparisonCategory,
    SourceModel = SourceModelLabel,
    SourceComponents,
    Issue,
    CurrentTreatment,
    Action,
    Notes
  )

current_issue_records <- current_issues |>
  filter(Component != "Dover Air Force Base") |>
  mutate(
    RecordType = case_when(
      Priority %in% c(
        "Confirmed Unit Count source rule",
        "Documented assumption",
        "Source limitation",
        "Confirmed scope decision",
        "Outside formula"
      ) ~ "Assumption or implementation choice",
      TRUE ~ "Open item"
    ),
    Status = case_when(
      Priority == "Needs data" ~ "Missing",
      Priority %in% c("Needs review", "Policy question") ~ "Provisional",
      Priority == "Not modeled" ~ "Missing",
      Priority %in% c(
        "Confirmed Unit Count source rule",
        "Confirmed scope decision",
        "Outside formula"
      ) ~ "Confirmed",
      TRUE ~ "Documented"
    )
  ) |>
  transmute(
    RecordSource = "Current model issues",
    AnalysisSection = "Current staffing rules",
    RecordType,
    Status,
    Priority,
    ComparisonCategory = Component,
    SourceModel = current_model_label,
    SourceComponents = Component,
    Issue,
    CurrentTreatment,
    Action,
    Notes = if_else(
      is.na(AffectedRows),
      "",
      paste("Affected rows:", AffectedRows)
    )
  )

proposed_issue_records <- proposed_issues |>
  filter(Component != "Dover Air Force Base") |>
  mutate(
    RecordType = case_when(
      Priority %in% c(
        "Documented adjustment",
        "Calculator-reproduced structure",
        "Documented method",
        "Confirmed policy",
        "Externally provided implementation guidance",
        "Documented assumption",
        "Confirmed scope decision",
        "Documented implementation choice",
        "Documented interpretation",
        "Calculator-reproduced rule",
        "Difference from calculator"
      ) ~ "Assumption or implementation choice",
      TRUE ~ "Open item"
    ),
    Status = case_when(
      Priority == "Needs data" ~ "Missing",
      Priority %in% c(
        "Confirmed policy",
        "Externally provided implementation guidance",
        "Confirmed scope decision"
      ) ~ "Confirmed",
      Priority == "Difference from calculator" ~ "Documented difference",
      Priority == "Documented implementation choice" ~ "Provisional",
      TRUE ~ "Documented"
    )
  ) |>
  transmute(
    RecordSource = "Proposed model issues",
    AnalysisSection = "Proposed staffing rules and PEFC reconciliation",
    RecordType,
    Status,
    Priority,
    ComparisonCategory = Component,
    SourceModel = proposed_model_label,
    SourceComponents = Component,
    Issue,
    CurrentTreatment,
    Action,
    Notes = if_else(
      is.na(AffectedRows),
      "",
      paste("Affected rows:", AffectedRows)
    )
  )

scope_record <- tibble(
  RecordSource = "Scope decision",
  AnalysisSection = "Scope",
  RecordType = "Assumption or implementation choice",
  Status = "Confirmed",
  Priority = "Confirmed scope decision",
  ComparisonCategory = "DAFB",
  SourceModel = "Both comparisons",
  SourceComponents = "DAFB treatment",
  Issue = dafb_scope_decision,
  CurrentTreatment = paste(
    "DAFB is excluded from aligned Base, Central Office, Opportunity,",
    "Operational, and total modeled funding. Source rows are retained only",
    "where needed to audit the PEFC workbook discrepancy."
  ),
  Action = "No further action is required unless the confirmed funding treatment changes.",
  Notes = pefc_dafb_scope_discrepancy_note
)

first_nonblank <- function(x) {
  values <- as.character(x)
  values <- values[!is.na(values) & trimws(values) != ""]
  if (length(values) == 0) "" else values[[1]]
}

collapse_unique_nonblank <- function(x) {
  values <- as.character(x)
  values <- unique(values[!is.na(values) & trimws(values) != ""])
  paste(values, collapse = " | ")
}

raw_open_items_and_assumptions <- bind_rows(
  crosswalk_items,
  current_issue_records,
  proposed_issue_records,
  scope_record
) |>
  distinct()

generic_issue_text <- c(
  "Required input is missing.",
  "Required rate is missing.",
  "Required input or rate is missing."
)

consolidated_open_items <- raw_open_items_and_assumptions |>
  filter(RecordType == "Open item") |>
  mutate(
    SourceRank = case_when(
      RecordSource == "Current model issues" ~ 1L,
      RecordSource == "Proposed model issues" ~ 1L,
      RecordSource == "Comparison crosswalk" ~ 2L,
      TRUE ~ 9L
    ),
    IssueRank = if_else(
      is.na(Issue) | trimws(Issue) == "" | Issue %in% generic_issue_text,
      2L,
      1L
    )
  ) |>
  group_by(ComparisonCategory, SourceModel) |>
  summarise(
    AnalysisSection = first_nonblank(AnalysisSection[order(SourceRank)]),
    RecordType = "Open item",
    Status = case_when(
      any(Status == "Missing") ~ "Missing",
      any(Status == "Provisional") ~ "Provisional",
      any(Status == "Pending confirmation") ~ "Pending confirmation",
      TRUE ~ first_nonblank(Status)
    ),
    Priority = first_nonblank(Priority[order(SourceRank)]),
    SourceComponents = collapse_unique_nonblank(SourceComponents),
    Issue = first_nonblank(Issue[order(IssueRank, SourceRank)]),
    CurrentTreatment = first_nonblank(CurrentTreatment[order(SourceRank)]),
    Action = first_nonblank(Action[order(SourceRank)]),
    Notes = collapse_unique_nonblank(Notes),
    .groups = "drop"
  ) |>
  select(
    AnalysisSection,
    RecordType,
    Status,
    Priority,
    ComparisonCategory,
    SourceModel,
    SourceComponents,
    Issue,
    CurrentTreatment,
    Action,
    Notes
  )

assumption_and_choice_records <- raw_open_items_and_assumptions |>
  filter(RecordType != "Open item") |>
  select(-RecordSource) |>
  distinct()

open_items_and_assumptions <- bind_rows(
  consolidated_open_items,
  assumption_and_choice_records
) |>
  arrange(
    factor(RecordType, levels = c("Open item", "Assumption or implementation choice")),
    AnalysisSection,
    Priority,
    ComparisonCategory,
    SourceModel
  )


# TECHNICAL QC AND ANALYSIS READINESS -------------------------------------------

outside_formula_component_count <- outside_formula |>
  filter(Component %in% outside_formula_current_components) |>
  distinct(Component) |>
  nrow()

outside_formula_comparison_categories <- comparison_crosswalk |>
  filter(ComparisonGroup == "Outside formula") |>
  distinct(ComparisonCategory) |>
  pull(ComparisonCategory)

outside_formula_open_item_count <- open_items_and_assumptions |>
  filter(
    ComparisonCategory %in% c(
      outside_formula_current_components,
      outside_formula_comparison_categories,
      "Custodians and cafeteria support"
    ),
    RecordType == "Open item" |
      Status %in% c("Missing", "Provisional", "Pending confirmation")
  ) |>
  nrow()

duplicate_open_item_key_count <- open_items_and_assumptions |>
  filter(RecordType == "Open item") |>
  count(ComparisonCategory, SourceModel, name = "RecordCount") |>
  filter(RecordCount > 1L) |>
  nrow()

cafeteria_district_count <- district_cafeteria |>
  distinct(DistrictCode) |>
  nrow()

cafeteria_state_allocation <- district_cafeteria |>
  summarise(Value = sum(TotalStateAllocation, na.rm = TRUE)) |>
  pull(Value)

cafeteria_outside_formula_total <- outside_formula |>
  filter(Component == "District Cafeteria Salary Allocation") |>
  summarise(Value = sum(FundingAmount, na.rm = TRUE)) |>
  pull(Value)

dafb_discrepant_sections <- dafb_scope_discrepancy |>
  filter(
    WorkbookScopeStatus == "Confirmed PEFC workbook scope discrepancy"
  ) |>
  nrow()

dafb_correct_sections <- dafb_scope_discrepancy |>
  filter(
    WorkbookScopeStatus == "PEFC workbook already excludes DAFB"
  ) |>
  nrow()

final_packaging_qc <- tibble(
  CheckType = "Integrity",
  Check = c(
    "All outside-formula current components are preserved in the final audit source",
    "Outside-formula components do not remain as unresolved final open items",
    "Final open items are unique by comparison category and source model",
    "FY26 district cafeteria allocation contains all primary-scope districts",
    "District cafeteria allocation reconciles to the outside-formula audit output",
    "DAFB workbook scope discrepancy is classified across all four funding sections"
  ),
  Expected = c(
    as.character(length(outside_formula_current_components)),
    "0",
    "0 duplicate open-item keys",
    as.character(expected_primary_district_count),
    as.character(cafeteria_state_allocation),
    "3 discrepant sections and 1 correctly excluded section"
  ),
  Actual = c(
    as.character(outside_formula_component_count),
    as.character(outside_formula_open_item_count),
    paste0(duplicate_open_item_key_count, " duplicate open-item keys"),
    as.character(cafeteria_district_count),
    as.character(cafeteria_outside_formula_total),
    paste0(
      dafb_discrepant_sections,
      " discrepant sections and ",
      dafb_correct_sections,
      " correctly excluded section"
    )
  ),
  Pass = c(
    outside_formula_component_count ==
      length(outside_formula_current_components),
    outside_formula_open_item_count == 0,
    duplicate_open_item_key_count == 0L,
    cafeteria_district_count == expected_primary_district_count,
    isTRUE(all.equal(
      cafeteria_outside_formula_total,
      cafeteria_state_allocation,
      tolerance = comparison_tolerance
    )),
    dafb_discrepant_sections == 3L & dafb_correct_sections == 1L
  )
)

final_qc <- bind_rows(
  comparison_qc |>
    mutate(CheckStage = "Step 09 current-versus-proposed", .before = 1),
  pefc_qc |>
    mutate(CheckStage = "Step 10 PEFC reconciliation", .before = 1),
  final_packaging_qc |>
    mutate(CheckStage = "Step 11 final packaging", .before = 1)
)

unique_open_item_count <- open_items_and_assumptions |>
  filter(RecordType == "Open item") |>
  nrow()

provisional_implementation_choice_count <- open_items_and_assumptions |>
  filter(
    RecordType == "Assumption or implementation choice",
    Status == "Provisional"
  ) |>
  nrow()

staffing_provisional_choice_count <- open_items_and_assumptions |>
  filter(
    RecordType == "Assumption or implementation choice",
    Status == "Provisional",
    AnalysisSection %in% c(
      "Current staffing rules",
      "Proposed staffing rules and PEFC reconciliation"
    )
  ) |>
  nrow()

final_readiness <- tibble(
  ReadinessArea = c(
    "Working staffing comparison",
    "Confirmed staffing subtotal",
    "Opportunity and Operational comparison",
    "Outside-formula current funding documentation",
    "DAFB scope decision",
    "PEFC workbook reconciliation",
    "Outstanding decisions and inputs"
  ),
  Status = c(
    "Available with provisional flags",
    if_else(
      all(as.logical(staffing_statewide$IsCompleteForFinalComparison)),
      "Complete",
      "Partial confirmed subtotal available"
    ),
    if_else(
      all(as.logical(weighted_statewide$IsCompleteForFinalComparison)),
      "Complete",
      "Not yet estimable"
    ),
    if_else(
      outside_formula_component_count ==
        length(outside_formula_current_components) &
        cafeteria_district_count == expected_primary_district_count,
      "Complete and excluded from position-based totals",
      "Incomplete documentation"
    ),
    if_else(
      dafb_discrepant_sections == 3L & dafb_correct_sections == 1L,
      "Confirmed and documented",
      "Review required"
    ),
    "Complete for the current workbook and inputs",
    if_else(
      unique_open_item_count == 0 &
        provisional_implementation_choice_count == 0,
      "Complete",
      "Outstanding items remain"
    )
  ),
  Ready = c(
    TRUE,
    all(as.logical(staffing_statewide$IsCompleteForFinalComparison)),
    all(as.logical(weighted_statewide$IsCompleteForFinalComparison)),
    outside_formula_component_count ==
      length(outside_formula_current_components) &
      cafeteria_district_count == expected_primary_district_count,
    dafb_discrepant_sections == 3L & dafb_correct_sections == 1L,
    all(as.logical(pefc_qc$Pass)),
    unique_open_item_count == 0 &
      provisional_implementation_choice_count == 0
  ),
  OpenItemCount = c(
    sum(staffing_components$ComparisonStatus != "Confirmed"),
    sum(staffing_components$ComparisonStatus == "Provisional"),
    sum(weighted_statewide$ComparisonStatus != "Confirmed"),
    outside_formula_open_item_count,
    0L,
    0L,
    unique_open_item_count
  ),
  ProvisionalImplementationChoiceCount = c(
    staffing_provisional_choice_count,
    staffing_provisional_choice_count,
    0L,
    0L,
    0L,
    0L,
    provisional_implementation_choice_count
  ),
  Notes = c(
    paste(
      "All available current and proposed staffing calculations are retained;",
      "working totals may change as confirmations are received."
    ),
    paste(
      "Confirmed subtotals include only categories with confirmed mappings,",
      "quantities, and rates. Administrative Support Professionals and",
      "Instructional Supports use externally confirmed functional crosswalks."
    ),
    paste(
      "Proposed allocations are available; current analogues and complete",
      "LEA allocations are still needed."
    ),
    paste(
      "Custodians, Cafeteria Managers, and Cafeteria Workers are retained in",
      "11_current_outside_formula_components.csv. The FY26 district cafeteria",
      "salary allocation is retained separately and is not added to staffing totals."
    ),
    paste(
      "DAFB does not receive state funding. It is excluded from all aligned",
      "IV&V totals and retained only for PEFC workbook scope auditing."
    ),
    paste(
      "Review the final PEFC summary, detailed audit files, the DAFB scope",
      "discrepancy output, and the charter building treatment table."
    ),
    paste0(
      "See 11_open_items_and_assumptions.csv. Unique open items: ",
      unique_open_item_count,
      "; provisional implementation choices: ",
      provisional_implementation_choice_count,
      "."
    )
  )
)


# WRITE FINAL FILES -------------------------------------------------------------

write_review_csv(staffing_statewide, final_paths[["staffing_statewide"]])
write_review_csv(staffing_components, final_paths[["staffing_components"]])
write_review_csv(staffing_leas, final_paths[["staffing_leas"]])
write_review_csv(weighted_statewide, final_paths[["weighted_statewide"]])
write_review_csv(weighted_leas, final_paths[["weighted_leas"]])
write_review_csv(
  pefc_reconciliation_final,
  final_paths[["pefc_reconciliation"]]
)
write_review_csv(charter_buildings, final_paths[["charter_buildings"]])
write_review_csv(outside_formula, final_paths[["outside_formula"]])
write_review_csv(district_cafeteria, final_paths[["district_cafeteria"]])
write_review_csv(
  dafb_scope_discrepancy,
  final_paths[["dafb_scope_discrepancy"]]
)
write_review_csv(
  open_items_and_assumptions,
  final_paths[["open_items"]]
)
write_review_csv(final_qc, final_paths[["final_qc"]])
write_review_csv(final_readiness, final_paths[["final_readiness"]])

integrity_failures <- final_qc |>
  filter(CheckType == "Integrity", !as.logical(Pass))

if (nrow(integrity_failures) > 0) {
  print(integrity_failures)
  stop("Final output integrity checks failed.", call. = FALSE)
}

message("Step 11 complete: ", length(final_paths), " report-ready files created in data/output/final/.")
