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
  comparison_crosswalk = model_comparison_crosswalk_path
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
      MappingStatus != "Confirmed" |
      QuantityStatus != "Confirmed" |
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
  mutate(
    RecordType = case_when(
      Priority %in% c(
        "Confirmed Unit Count source rule",
        "Documented assumption",
        "Source limitation"
      ) ~ "Assumption or implementation choice",
      TRUE ~ "Open item"
    ),
    Status = case_when(
      Priority == "Needs data" ~ "Missing",
      Priority %in% c("Needs review", "Policy question") ~ "Provisional",
      Priority == "Not modeled" ~ "Missing",
      Priority == "Confirmed Unit Count source rule" ~ "Confirmed",
      TRUE ~ "Documented"
    )
  ) |>
  transmute(
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
  mutate(
    RecordType = case_when(
      Priority %in% c(
        "Documented adjustment",
        "Calculator-reproduced structure",
        "Documented method",
        "Confirmed policy",
        "Externally provided implementation guidance",
        "Documented assumption",
        "Documented implementation choice",
        "Documented interpretation",
        "Calculator-reproduced rule",
        "Difference from calculator"
      ) ~ "Assumption or implementation choice",
      TRUE ~ "Open item"
    ),
    Status = case_when(
      Priority == "Confirmed policy" ~ "Confirmed",
      Priority == "Externally provided implementation guidance" ~ "Confirmed",
      Priority == "Difference from calculator" ~ "Documented difference",
      Priority == "Documented implementation choice" ~ "Provisional",
      TRUE ~ "Documented"
    )
  ) |>
  transmute(
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
  AnalysisSection = "Scope",
  RecordType = "Open item",
  Status = "Pending confirmation",
  Priority = "Needs review",
  ComparisonCategory = "DAFB",
  SourceModel = "Both comparisons",
  SourceComponents = "DAFB treatment",
  Issue = paste(
    "Confirm whether the current model treats DAFB like the PEFC workbook",
    "and whether DAFB belongs in the final statewide comparison."
  ),
  CurrentTreatment =
    "DAFB is excluded from the primary comparison pending confirmation.",
  Action = "Update the scope rule in 00_settings.R when confirmed.",
  Notes = primary_reporting_scope_short
)

open_items_and_assumptions <- bind_rows(
  crosswalk_items,
  current_issue_records,
  proposed_issue_records,
  scope_record
) |>
  distinct() |>
  arrange(
    factor(RecordType, levels = c("Open item", "Assumption or implementation choice")),
    AnalysisSection,
    Priority,
    ComparisonCategory,
    SourceModel
  )


# TECHNICAL QC AND ANALYSIS READINESS -------------------------------------------

final_qc <- bind_rows(
  comparison_qc |>
    mutate(CheckStage = "Step 09 current-versus-proposed", .before = 1),
  pefc_qc |>
    mutate(CheckStage = "Step 10 PEFC reconciliation", .before = 1)
)

open_item_count <- open_items_and_assumptions |>
  filter(
    RecordType == "Open item" |
      Status %in% c("Missing", "Provisional", "Pending confirmation")
  ) |>
  nrow()

final_readiness <- tibble(
  ReadinessArea = c(
    "Working staffing comparison",
    "Confirmed staffing subtotal",
    "Opportunity and Operational comparison",
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
    "Complete for the current workbook and inputs",
    if_else(open_item_count == 0, "Complete", "Outstanding items remain")
  ),
  Ready = c(
    TRUE,
    all(as.logical(staffing_statewide$IsCompleteForFinalComparison)),
    all(as.logical(weighted_statewide$IsCompleteForFinalComparison)),
    all(as.logical(pefc_qc$Pass)),
    open_item_count == 0
  ),
  OpenItemCount = c(
    sum(staffing_components$ComparisonStatus != "Confirmed"),
    sum(staffing_components$ComparisonStatus == "Provisional"),
    sum(weighted_statewide$ComparisonStatus != "Confirmed"),
    0L,
    open_item_count
  ),
  Notes = c(
    paste(
      "All available current and proposed staffing calculations are retained;",
      "working totals may change as confirmations are received."
    ),
    paste(
      "Confirmed subtotals include only categories with confirmed mappings,",
      "quantities, and rates."
    ),
    paste(
      "Proposed allocations are available; current analogues and complete",
      "LEA allocations are still needed."
    ),
    paste(
      "Review the final PEFC summary, detailed audit files, and the charter",
      "building treatment table."
    ),
    "See 11_open_items_and_assumptions.csv."
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

message("Step 11 complete: ten report-ready files created in data/output/final/.")
