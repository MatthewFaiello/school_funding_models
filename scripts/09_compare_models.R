# =============================================================================
# 09_compare_models.R
# =============================================================================
# Compares current and proposed modeled funding at the school, LEA, and state
# levels.
#
# Position-based comparison:
#   School: current funding versus proposed Base Funding
#   LEA/state: current funding versus proposed Base + Central Office Funding
#
# Full-model comparison:
#   Current funding versus all proposed funding
# =============================================================================

source(file.path("scripts", "00_settings.R"))

current_school_summary_path <- file.path(
  output_dir,
  "05_current_model_school_summary.csv"
)
current_lea_summary_path <- file.path(
  output_dir,
  "05_current_model_lea_summary.csv"
)
current_state_summary_path <- file.path(
  output_dir,
  "05_current_model_state_summary.csv"
)
proposed_school_summary_path <- file.path(
  output_dir,
  "08_proposed_model_school_summary.csv"
)
proposed_lea_summary_path <- file.path(
  output_dir,
  "08_proposed_model_lea_summary.csv"
)
proposed_state_summary_path <- file.path(
  output_dir,
  "08_proposed_model_state_summary.csv"
)
school_comparison_path <- file.path(output_dir, "09_school_comparison.csv")
lea_comparison_path <- file.path(output_dir, "09_lea_comparison.csv")
state_comparison_path <- file.path(output_dir, "09_state_comparison.csv")
comparison_qc_path <- file.path(output_dir, "09_comparison_qc.csv")

check_required_files(c(
  current_school_summary_path,
  current_lea_summary_path,
  current_state_summary_path,
  proposed_school_summary_path,
  proposed_lea_summary_path,
  proposed_state_summary_path
))


# SMALL HELPERS ----------------------------------------------------------------

percent_difference <- function(new_amount, old_amount) {
  case_when(
    is.na(new_amount) | is.na(old_amount) ~ NA_real_,
    abs(old_amount) < 1e-8 ~ NA_real_,
    TRUE ~ 100 * (new_amount - old_amount) / old_amount
  )
}

comparison_status <- function(current_complete, proposed_complete) {
  case_when(
    is.na(current_complete) | is.na(proposed_complete) ~
      "Cannot assess; one model is missing",
    current_complete & proposed_complete ~ "Complete",
    !current_complete & proposed_complete ~
      "Partial because the current model has missing inputs or rates",
    current_complete & !proposed_complete ~
      "Partial because the proposed model has missing inputs or rates",
    TRUE ~ "Partial because both models have missing inputs or rates"
  )
}


# STANDARDIZE CURRENT-MODEL SUMMARIES -----------------------------------------

current_school_records <- read_csv(
  current_school_summary_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    IsSchool = as.logical(IsSchool),
    OverallComplete = as.logical(OverallComplete)
  ) |>
  transmute(
    RecordLevel = "School",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictNameCurrent = DistrictName,
    LEATypeCurrent = LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    RecordType = case_when(
      IsSchool ~ "School",
      TRUE ~ "Vocational/program record"
    ),
    CurrentCalculatorSuppliedFundingAmount = CalculatorSuppliedFundingAmount,
    CurrentDocumentedCrosswalkFundingAmount = DocumentedCrosswalkFundingAmount,
    CurrentAnalyticalAssumptionFundingAmount = AnalyticalAssumptionFundingAmount,
    CurrentModelFundingAmount = TotalModeledFundingAmount,
    CurrentComponentsMissingInput = ComponentsMissingInput,
    CurrentRowsMissingInput = NA_integer_,
    CurrentComponentsMissingRate = ComponentsMissingRate,
    CurrentRowsMissingRate = NA_integer_,
    CurrentComplete = OverallComplete
  )

current_lea_records <- read_csv(
  current_lea_summary_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    OverallComplete = as.logical(OverallComplete)
  ) |>
  transmute(
    RecordLevel = "LEA",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictNameCurrent = DistrictName,
    LEATypeCurrent = LEAType,
    IncludeInStatewide,
    SchoolCode = NA_integer_,
    SchoolName = "LEA Total",
    RecordType = "LEA",
    CurrentCalculatorSuppliedFundingAmount = CalculatorSuppliedFundingAmount,
    CurrentDocumentedCrosswalkFundingAmount = DocumentedCrosswalkFundingAmount,
    CurrentAnalyticalAssumptionFundingAmount = AnalyticalAssumptionFundingAmount,
    CurrentModelFundingAmount = TotalModeledFundingAmount,
    CurrentComponentsMissingInput = ComponentsMissingInput,
    CurrentRowsMissingInput = NA_integer_,
    CurrentComponentsMissingRate = ComponentsMissingRate,
    CurrentRowsMissingRate = NA_integer_,
    CurrentComplete = OverallComplete
  )

current_state_source <- read_csv(
  current_state_summary_path,
  show_col_types = FALSE
)

current_state_records <- current_state_source |>
  summarise(
    CurrentCalculatorSuppliedFundingAmount =
      sum(CalculatorSuppliedFundingAmount, na.rm = TRUE),
    CurrentDocumentedCrosswalkFundingAmount =
      sum(DocumentedCrosswalkFundingAmount, na.rm = TRUE),
    CurrentAnalyticalAssumptionFundingAmount =
      sum(AnalyticalAssumptionFundingAmount, na.rm = TRUE),
    CurrentModelFundingAmount =
      sum(TotalModeledFundingAmount, na.rm = TRUE),
    CurrentComponentsMissingInput = n_distinct(
      Component[RowsMissingInput > 0]
    ),
    CurrentRowsMissingInput = sum(RowsMissingInput, na.rm = TRUE),
    CurrentComponentsMissingRate = n_distinct(
      Component[RowsMissingRate > 0]
    ),
    CurrentRowsMissingRate = sum(RowsMissingRate, na.rm = TRUE),
    CurrentComplete = all(as.logical(OverallComplete))
  ) |>
  mutate(
    RecordLevel = "Statewide",
    SchoolYear = school_year,
    CountDate = count_date,
    DistrictCode = NA_integer_,
    DistrictNameCurrent = "Statewide excluding DAFB",
    LEATypeCurrent = "Statewide",
    IncludeInStatewide = TRUE,
    SchoolCode = NA_integer_,
    SchoolName = "Statewide",
    RecordType = "Statewide",
    .before = 1
  )

current_records <- bind_rows(
  current_school_records,
  current_lea_records,
  current_state_records
)


# STANDARDIZE PROPOSED-MODEL SUMMARIES ----------------------------------------

proposed_school_records <- read_csv(
  proposed_school_summary_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    SchoolCode = as.integer(SchoolCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    IsSchool = as.logical(IsSchool),
    OverallComplete = as.logical(OverallComplete)
  ) |>
  transmute(
    RecordLevel = "School",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictNameProposed = DistrictName,
    LEATypeProposed = LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    RecordType = case_when(
      IsSchool ~ "School",
      TRUE ~ "Vocational/program record"
    ),
    ProposedCalculationUnitCount = BaseCalculationUnitCount,
    ProposedPrincipalPositions,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy,
    ProposedBaseFundingAmount = BaseFundingAmount,
    ProposedOpportunityFundingAmount = OpportunityFundingAmount,
    ProposedOperationalFundingAmount = OperationalFundingAmount,
    ProposedCentralOfficeFundingAmount = 0,
    ProposedWeightedFundingAmount =
      OpportunityFundingAmount + OperationalFundingAmount,
    ProposedPositionBasedFundingAmount = BaseFundingAmount,
    ProposedModelFundingAmount = TotalModeledFundingAmount,
    ProposedComponentsMissingInput = ComponentsMissingInput,
    ProposedRowsMissingInput = NA_integer_,
    ProposedComponentsMissingRate = ComponentsMissingRate,
    ProposedRowsMissingRate = NA_integer_,
    ProposedComplete = OverallComplete
  )

proposed_lea_records <- read_csv(
  proposed_lea_summary_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    OverallComplete = as.logical(OverallComplete)
  ) |>
  transmute(
    RecordLevel = "LEA",
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictNameProposed = DistrictName,
    LEATypeProposed = LEAType,
    IncludeInStatewide,
    SchoolCode = NA_integer_,
    SchoolName = "LEA Total",
    RecordType = "LEA",
    ProposedCalculationUnitCount = SchoolCalculationUnitCount,
    ProposedPrincipalPositions,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy,
    ProposedBaseFundingAmount = BaseFundingAmount,
    ProposedOpportunityFundingAmount = OpportunityFundingAmount,
    ProposedOperationalFundingAmount = OperationalFundingAmount,
    ProposedCentralOfficeFundingAmount = CentralOfficeFundingAmount,
    ProposedWeightedFundingAmount =
      OpportunityFundingAmount + OperationalFundingAmount,
    ProposedPositionBasedFundingAmount =
      BaseFundingAmount + CentralOfficeFundingAmount,
    ProposedModelFundingAmount = TotalModeledFundingAmount,
    ProposedComponentsMissingInput = ComponentsMissingInput,
    ProposedRowsMissingInput = NA_integer_,
    ProposedComponentsMissingRate = ComponentsMissingRate,
    ProposedRowsMissingRate = NA_integer_,
    ProposedComplete = OverallComplete
  )

proposed_state_source <- read_csv(
  proposed_state_summary_path,
  show_col_types = FALSE
)

proposed_state_records <- proposed_state_source |>
  summarise(
    ProposedBaseFundingAmount = sum(
      FundingAmount[
        FundingSection == "Base Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    ProposedOpportunityFundingAmount = sum(
      FundingAmount[
        FundingSection == "Opportunity Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    ProposedOperationalFundingAmount = sum(
      FundingAmount[
        FundingSection == "Operational Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    ProposedCentralOfficeFundingAmount = sum(
      FundingAmount[
        FundingSection == "Central Office Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    ProposedComponentsMissingInput = n_distinct(
      Component[RowsMissingInput > 0]
    ),
    ProposedRowsMissingInput = sum(RowsMissingInput, na.rm = TRUE),
    ProposedComponentsMissingRate = n_distinct(
      Component[RowsMissingRate > 0]
    ),
    ProposedRowsMissingRate = sum(RowsMissingRate, na.rm = TRUE),
    ProposedComplete = all(as.logical(OverallComplete)),
    OperationalEnrollmentBasis = first(OperationalEnrollmentBasis),
    CharterBuildingPolicy = first(CharterBuildingPolicy)
  ) |>
  mutate(
    ProposedWeightedFundingAmount =
      ProposedOpportunityFundingAmount + ProposedOperationalFundingAmount,
    ProposedPositionBasedFundingAmount =
      ProposedBaseFundingAmount + ProposedCentralOfficeFundingAmount,
    ProposedModelFundingAmount =
      ProposedBaseFundingAmount +
      ProposedOpportunityFundingAmount +
      ProposedOperationalFundingAmount +
      ProposedCentralOfficeFundingAmount,
    ProposedCalculationUnitCount = proposed_lea_records |>
      filter(IncludeInStatewide) |>
      summarise(
        Value = sum(ProposedCalculationUnitCount, na.rm = TRUE)
      ) |>
      pull(Value),
    ProposedPrincipalPositions = proposed_state_source |>
      filter(Component == "Principal") |>
      summarise(Value = sum(FundingQuantity, na.rm = TRUE)) |>
      pull(Value),
    RecordLevel = "Statewide",
    SchoolYear = school_year,
    CountDate = count_date,
    DistrictCode = NA_integer_,
    DistrictNameProposed = "Statewide excluding DAFB",
    LEATypeProposed = "Statewide",
    IncludeInStatewide = TRUE,
    SchoolCode = NA_integer_,
    SchoolName = "Statewide",
    RecordType = "Statewide",
    .before = 1
  )

proposed_records <- bind_rows(
  proposed_school_records,
  proposed_lea_records,
  proposed_state_records
)


# JOIN AND CALCULATE DIFFERENCES ------------------------------------------------

comparison_key <- c(
  "RecordLevel",
  "SchoolYear",
  "CountDate",
  "DistrictCode",
  "IncludeInStatewide",
  "SchoolCode",
  "SchoolName",
  "RecordType"
)

stop_if_rows(
  current_records |>
    count(across(all_of(comparison_key))) |>
    filter(n > 1),
  "The current-model summaries contain duplicate comparison keys."
)

stop_if_rows(
  proposed_records |>
    count(across(all_of(comparison_key))) |>
    filter(n > 1),
  "The proposed-model summaries contain duplicate comparison keys."
)

model_comparison <- full_join(
  current_records,
  proposed_records,
  by = comparison_key
) |>
  mutate(
    DistrictName = coalesce(DistrictNameCurrent, DistrictNameProposed),
    LEAType = coalesce(LEATypeCurrent, LEATypeProposed),
    ModelMatchStatus = case_when(
      is.na(CurrentModelFundingAmount) ~ "Missing from current model",
      is.na(ProposedModelFundingAmount) ~ "Missing from proposed model",
      TRUE ~ "Matched"
    ),
    PositionBasedComparisonBasis = case_when(
      RecordLevel == "School" ~
        "Current modeled funding versus proposed Base Funding",
      TRUE ~
        "Current modeled funding versus proposed Base plus Central Office Funding"
    ),
    FullModelComparisonBasis =
      "Current modeled funding versus all proposed funding",
    CurrentModelComplete = coalesce(CurrentComplete, FALSE),
    ProposedModelComplete = coalesce(ProposedComplete, FALSE),
    ComparisonComplete = CurrentModelComplete & ProposedModelComplete,
    PositionBasedFundingDifference =
      ProposedPositionBasedFundingAmount - CurrentModelFundingAmount,
    PreliminaryPositionBasedPercentDifference = percent_difference(
      ProposedPositionBasedFundingAmount,
      CurrentModelFundingAmount
    ),
    PositionBasedPercentDifference = if_else(
      ComparisonComplete,
      PreliminaryPositionBasedPercentDifference,
      NA_real_
    ),
    FullModelFundingDifference =
      ProposedModelFundingAmount - CurrentModelFundingAmount,
    PreliminaryFullModelPercentDifference = percent_difference(
      ProposedModelFundingAmount,
      CurrentModelFundingAmount
    ),
    FullModelPercentDifference = if_else(
      ComparisonComplete,
      PreliminaryFullModelPercentDifference,
      NA_real_
    ),
    ComparisonStatus = comparison_status(
      CurrentModelComplete,
      ProposedModelComplete
    ),
    ComparisonInterpretation = if_else(
      ComparisonComplete,
      "Complete modeled comparison",
      paste(
        "Preliminary arithmetic comparison only;",
        "at least one model has missing inputs or rates"
      )
    )
  ) |>
  select(
    RecordLevel,
    RecordType,
    SchoolYear,
    CountDate,
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    SchoolCode,
    SchoolName,
    ModelMatchStatus,
    CurrentCalculatorSuppliedFundingAmount,
    CurrentDocumentedCrosswalkFundingAmount,
    CurrentAnalyticalAssumptionFundingAmount,
    CurrentModelFundingAmount,
    CurrentComponentsMissingInput,
    CurrentRowsMissingInput,
    CurrentComponentsMissingRate,
    CurrentRowsMissingRate,
    ProposedCalculationUnitCount,
    ProposedPrincipalPositions,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy,
    ProposedBaseFundingAmount,
    ProposedCentralOfficeFundingAmount,
    ProposedPositionBasedFundingAmount,
    ProposedOpportunityFundingAmount,
    ProposedOperationalFundingAmount,
    ProposedWeightedFundingAmount,
    ProposedModelFundingAmount,
    ProposedComponentsMissingInput,
    ProposedRowsMissingInput,
    ProposedComponentsMissingRate,
    ProposedRowsMissingRate,
    PositionBasedComparisonBasis,
    PositionBasedFundingDifference,
    PreliminaryPositionBasedPercentDifference,
    PositionBasedPercentDifference,
    FullModelComparisonBasis,
    FullModelFundingDifference,
    PreliminaryFullModelPercentDifference,
    FullModelPercentDifference,
    CurrentModelComplete,
    ProposedModelComplete,
    ComparisonComplete,
    ComparisonStatus,
    ComparisonInterpretation
  ) |>
  arrange(
    factor(RecordLevel, levels = c("School", "LEA", "Statewide")),
    DistrictName,
    SchoolName
  )

school_comparison <- model_comparison |>
  filter(RecordLevel == "School") |>
  select(-RecordLevel)

lea_comparison <- model_comparison |>
  filter(RecordLevel == "LEA") |>
  select(-RecordLevel)

state_comparison <- model_comparison |>
  filter(RecordLevel == "Statewide") |>
  select(-RecordLevel)


# COMPARISON QC -----------------------------------------------------------------

current_lea_state_difference <-
  current_lea_records |>
  filter(IncludeInStatewide) |>
  summarise(Value = sum(CurrentModelFundingAmount, na.rm = TRUE)) |>
  pull(Value) -
  current_state_records$CurrentModelFundingAmount[[1]]

proposed_lea_state_difference <-
  proposed_lea_records |>
  filter(IncludeInStatewide) |>
  summarise(Value = sum(ProposedModelFundingAmount, na.rm = TRUE)) |>
  pull(Value) -
  proposed_state_records$ProposedModelFundingAmount[[1]]

current_lea_state_difference_review <- if_else(
  abs(current_lea_state_difference) <= 0.10,
  0,
  current_lea_state_difference
)

proposed_lea_state_difference_review <- if_else(
  abs(proposed_lea_state_difference) <= 0.10,
  0,
  proposed_lea_state_difference
)


comparison_qc <- tibble(
  Check = c(
    "Matched school/program records",
    "Unmatched school/program records",
    "Incomplete school/program comparisons",
    "Matched LEAs",
    "Unmatched LEAs",
    "Incomplete LEA comparisons",
    "Statewide records",
    "Statewide comparison complete",
    "Official percent fields populated on incomplete rows",
    "Current LEA-to-state funding difference",
    "Proposed LEA-to-state funding difference",
    "Operational enrollment basis",
    "Weighted rate method"
  ),
  Value = c(
    sum(school_comparison$ModelMatchStatus == "Matched"),
    sum(school_comparison$ModelMatchStatus != "Matched"),
    sum(!school_comparison$ComparisonComplete),
    sum(lea_comparison$ModelMatchStatus == "Matched"),
    sum(lea_comparison$ModelMatchStatus != "Matched"),
    sum(!lea_comparison$ComparisonComplete),
    nrow(state_comparison),
    all(state_comparison$ComparisonComplete),
    model_comparison |>
      filter(
        !ComparisonComplete,
        !is.na(PositionBasedPercentDifference) |
          !is.na(FullModelPercentDifference)
      ) |>
      nrow(),
    current_lea_state_difference_review,
    proposed_lea_state_difference_review,
    operational_enrollment_basis,
    weighted_rate_method
  ),
  Status = c(
    "Info",
    if_else(
      any(school_comparison$ModelMatchStatus != "Matched"),
      "Review",
      "Pass"
    ),
    "Info",
    "Info",
    if_else(
      any(lea_comparison$ModelMatchStatus != "Matched"),
      "Review",
      "Pass"
    ),
    "Info",
    if_else(nrow(state_comparison) == 1, "Pass", "Review"),
    "Info",
    if_else(
      any(
        !model_comparison$ComparisonComplete &
          (
            !is.na(model_comparison$PositionBasedPercentDifference) |
              !is.na(model_comparison$FullModelPercentDifference)
          )
      ),
      "Review",
      "Pass"
    ),
    if_else(abs(current_lea_state_difference) <= 0.10, "Pass", "Review"),
    if_else(abs(proposed_lea_state_difference) <= 0.10, "Pass", "Review"),
    "Info",
    "Info"
  )
)


write_review_csv(school_comparison, school_comparison_path)
write_review_csv(lea_comparison, lea_comparison_path)
write_review_csv(state_comparison, state_comparison_path)
write_review_csv(comparison_qc, comparison_qc_path)

stop_if_rows(
  comparison_qc |>
    filter(Status == "Review"),
  "One or more model-comparison QC checks require review."
)

message("Created school comparison: ", school_comparison_path)
message("Created LEA comparison: ", lea_comparison_path)
message("Created state comparison: ", state_comparison_path)
message("Review comparison QC: ", comparison_qc_path)
