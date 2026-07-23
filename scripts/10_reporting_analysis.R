# =============================================================================
# 10_reporting_analysis.R
# =============================================================================
# Creates adjusted, report-ready comparisons after Step 09.
#
# Reporting scope:
#   1. DAFB remains excluded through IncludeInStatewide == FALSE.
#   2. Bryan Allen Stevenson School of Excellence is removed from school,
#      LEA, weighted-rate, and statewide reporting.
#   3. Opportunity and Operational dollars remain fixed statewide pools, so
#      their rates are recalculated across the remaining eligible weighted units.
#
# Core outputs:
#   10_adjusted_school_comparison.csv
#   10_adjusted_lea_comparison.csv
#   10_adjusted_state_summary.csv
#   10_adjusted_weighted_rate_summary.csv
#   10_adjusted_weighted_component_summary.csv
#   10_lea_distribution_summary.csv
#   10_lea_type_summary.csv
#   10_lea_rankings.csv
#   10_report_summary.csv
#   10_report_ready_text.csv
#   10_exclusion_audit.csv
#   10_reporting_qc.csv
# =============================================================================

source(file.path("scripts", "00_settings.R"))

reporting_script_version <- "2026-07-22-v2"
message("Step 10 reporting script version: ", reporting_script_version)


# SETTINGS ----------------------------------------------------------------------

excluded_lea_code <- 9615L
excluded_lea_name <- "Bryan Allen Stevenson School of Excellence"
excluded_lea_reason <- "Excluded because 2025-26 was its final year of operation."
comparison_zero_tolerance <- 0.01
top_n_leas <- 10L

base_section <- "Base Funding (State Support)"
central_section <- "Central Office Funding (State Support)"
opportunity_section <- "Opportunity Funding (State Support)"
operational_section <- "Operational Funding (State Support)"
weighted_sections <- c(opportunity_section, operational_section)


# FILES -------------------------------------------------------------------------

input_paths <- c(
  shared = file.path(output_dir, "02_shared_model_input.csv"),
  current_detail = file.path(output_dir, "05_current_model_funding_detail.csv"),
  proposed_detail = file.path(output_dir, "08_proposed_model_funding_detail.csv"),
  school_comparison = file.path(output_dir, "09_school_comparison.csv"),
  lea_comparison = file.path(output_dir, "09_lea_comparison.csv"),
  state_comparison = file.path(output_dir, "09_state_comparison.csv")
)
check_required_files(input_paths)

output_paths <- c(
  school = file.path(output_dir, "10_adjusted_school_comparison.csv"),
  lea = file.path(output_dir, "10_adjusted_lea_comparison.csv"),
  state = file.path(output_dir, "10_adjusted_state_summary.csv"),
  rates = file.path(output_dir, "10_adjusted_weighted_rate_summary.csv"),
  components = file.path(output_dir, "10_adjusted_weighted_component_summary.csv"),
  distribution = file.path(output_dir, "10_lea_distribution_summary.csv"),
  type = file.path(output_dir, "10_lea_type_summary.csv"),
  rankings = file.path(output_dir, "10_lea_rankings.csv"),
  report = file.path(output_dir, "10_report_summary.csv"),
  text = file.path(output_dir, "10_report_ready_text.csv"),
  exclusion = file.path(output_dir, "10_exclusion_audit.csv"),
  qc = file.path(output_dir, "10_reporting_qc.csv")
)


# HELPERS -----------------------------------------------------------------------

percent_difference <- function(new_amount, old_amount) {
  case_when(
    is.na(new_amount) | is.na(old_amount) ~ NA_real_,
    abs(old_amount) < 1e-8 ~ NA_real_,
    TRUE ~ 100 * (new_amount - old_amount) / old_amount
  )
}

safe_divide <- function(numerator, denominator) {
  if_else(abs(coalesce(denominator, 0)) < 1e-8, NA_real_, numerator / denominator)
}

is_excluded_lea <- function(code, name) {
  coalesce(code == excluded_lea_code, FALSE) |
    coalesce(name == excluded_lea_name, FALSE)
}

direction <- function(difference) {
  case_when(
    is.na(difference) ~ "Not calculated",
    difference > comparison_zero_tolerance ~ "Increase",
    difference < -comparison_zero_tolerance ~ "Decrease",
    TRUE ~ "No material change"
  )
}

q_value <- function(x, probability) {
  as.numeric(quantile(x, probability, na.rm = TRUE, names = FALSE))
}

format_count <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_money_m <- function(x, show_plus = FALSE) {
  prefix <- case_when(x < 0 ~ "-", show_plus & x > 0 ~ "+", TRUE ~ "")
  paste0(prefix, "$", formatC(abs(x) / 1e6, format = "f", digits = 1), "M")
}

format_pct <- function(x, show_plus = FALSE) {
  prefix <- case_when(x < 0 ~ "-", show_plus & x > 0 ~ "+", TRUE ~ "")
  paste0(prefix, formatC(abs(x), format = "f", digits = 1), "%")
}


# READ --------------------------------------------------------------------------

shared <- read_csv(input_paths[["shared"]], show_col_types = FALSE)
current_detail <- read_csv(input_paths[["current_detail"]], show_col_types = FALSE)
proposed_detail <- read_csv(input_paths[["proposed_detail"]], show_col_types = FALSE)
school_source <- read_csv(input_paths[["school_comparison"]], show_col_types = FALSE)
lea_source <- read_csv(input_paths[["lea_comparison"]], show_col_types = FALSE)
state_source <- read_csv(input_paths[["state_comparison"]], show_col_types = FALSE)

if (nrow(lea_source |> filter(is_excluded_lea(DistrictCode, DistrictName))) != 1) {
  stop("The excluded LEA was not found exactly once in 09_lea_comparison.csv.", call. = FALSE)
}


# ADJUSTED SCOPE ----------------------------------------------------------------

shared_school <- shared |>
  filter(
    AggregationLevel == "School",
    DistrictCode != dafb_district_code,
    !is_excluded_lea(DistrictCode, DistrictName)
  )

current_adjusted <- current_detail |>
  filter(IncludeInStatewide, !is_excluded_lea(DistrictCode, DistrictName))

proposed_adjusted <- proposed_detail |>
  filter(IncludeInStatewide, !is_excluded_lea(DistrictCode, DistrictName))

school_source_adjusted <- school_source |>
  filter(IncludeInStatewide, !is_excluded_lea(DistrictCode, DistrictName))

lea_source_adjusted <- lea_source |>
  filter(IncludeInStatewide, !is_excluded_lea(DistrictCode, DistrictName))


# RECALCULATE FIXED WEIGHTED POOLS ----------------------------------------------

pool_lookup <- tibble(
  FundingSection = weighted_sections,
  FundingPool = c(opportunity_funding_pool, operational_funding_pool)
)

original_rates <- proposed_detail |>
  filter(IncludeInStatewide, FundingSection %in% weighted_sections) |>
  summarise(
    OriginalWeightedCount = sum(FundingQuantity, na.rm = TRUE),
    OriginalFundingRate = first(FundingRate),
    .by = FundingSection
  )

excluded_weighted <- proposed_detail |>
  filter(
    is_excluded_lea(DistrictCode, DistrictName),
    FundingSection %in% weighted_sections
  ) |>
  summarise(
    ExcludedWeightedCount = sum(FundingQuantity, na.rm = TRUE),
    ExcludedOriginalFundingAmount = sum(FundingAmount, na.rm = TRUE),
    .by = FundingSection
  )

adjusted_rates <- proposed_adjusted |>
  filter(FundingSection %in% weighted_sections) |>
  summarise(AdjustedWeightedCount = sum(FundingQuantity, na.rm = TRUE), .by = FundingSection) |>
  left_join(pool_lookup, by = "FundingSection") |>
  left_join(original_rates, by = "FundingSection") |>
  left_join(excluded_weighted, by = "FundingSection") |>
  mutate(
    across(c(ExcludedWeightedCount, ExcludedOriginalFundingAmount), ~ coalesce(.x, 0)),
    AdjustedFundingRate = FundingPool / AdjustedWeightedCount,
    RateChange = AdjustedFundingRate - OriginalFundingRate,
    RatePercentChange = percent_difference(AdjustedFundingRate, OriginalFundingRate),
    AdjustedFundingAmount = AdjustedWeightedCount * AdjustedFundingRate,
    ReportingScope = paste("Excludes DAFB and", excluded_lea_name),
    FundingPoolSource = weighted_pool_amount_source,
    RateMethodSource = weighted_rate_guidance_source,
    RateMethodNote = weighted_rate_guidance_note,
    PoolTreatment = "Fixed pool retained and redistributed across the adjusted reporting scope"
  ) |>
  arrange(FundingSection)

stop_if_rows(
  adjusted_rates |> filter(is.na(AdjustedFundingRate) | AdjustedWeightedCount <= 0),
  "The adjusted weighted-rate calculation is incomplete."
)

adjusted_weighted_detail <- proposed_adjusted |>
  filter(FundingSection %in% weighted_sections) |>
  left_join(adjusted_rates |> select(FundingSection, AdjustedFundingRate), by = "FundingSection") |>
  mutate(AdjustedFundingAmount = FundingQuantity * AdjustedFundingRate)


# REFRESH SCHOOL AND LEA COMPARISONS -------------------------------------------

old_calculated_columns <- c(
  "ProposedOpportunityFundingAmount", "ProposedOperationalFundingAmount",
  "ProposedWeightedFundingAmount", "ProposedModelFundingAmount",
  "PositionBasedFundingDifference", "PreliminaryPositionBasedPercentDifference",
  "PositionBasedPercentDifference", "FullModelFundingDifference",
  "PreliminaryFullModelPercentDifference", "FullModelPercentDifference"
)

refresh_comparison <- function(source_data, level = c("School", "LEA")) {
  level <- match.arg(level)

  keys <- if (level == "School") {
    c("SchoolYear", "CountDate", "DistrictCode", "DistrictName", "LEAType", "SchoolCode", "SchoolName")
  } else {
    c("SchoolYear", "CountDate", "DistrictCode", "DistrictName", "LEAType")
  }

  weighted_keys <- intersect(keys, names(adjusted_weighted_detail))

  missing_weighted_keys <- setdiff(keys, weighted_keys)

  if (length(missing_weighted_keys) > 0) {
    stop(
      paste(
        "Step 10 cannot group the proposed weighted detail because these",
        "comparison keys are missing:",
        paste(missing_weighted_keys, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  weighted <- adjusted_weighted_detail |>
    summarise(
      ProposedOpportunityFundingAmount = sum(
        AdjustedFundingAmount[FundingSection == opportunity_section], na.rm = TRUE
      ),
      ProposedOperationalFundingAmount = sum(
        AdjustedFundingAmount[FundingSection == operational_section], na.rm = TRUE
      ),
      .by = all_of(weighted_keys)
    )

  # LEAType is available in the Step 08 and Step 09 outputs, but it is not a
  # field in 02_shared_model_input.csv. Use explicit shared-input keys rather
  # than recycling the comparison keys.
  enrollment_keys <- if (level == "School") {
    c(
      "SchoolYear",
      "CountDate",
      "DistrictCode",
      "DistrictName",
      "SchoolCode",
      "SchoolName"
    )
  } else {
    c("SchoolYear", "CountDate", "DistrictCode", "DistrictName")
  }

  enrollment_keys <- intersect(enrollment_keys, names(shared_school))

  enrollment <- if (level == "School") {
    shared_school |>
      select(
        any_of(c(
          enrollment_keys,
          "IsSchool",
          "Enrollment",
          "LI",
          "MLL",
          "K8Enrollment",
          "Grade10Enrollment"
        ))
      )
  } else {
    shared_school |>
      summarise(
        Enrollment = sum(Enrollment, na.rm = TRUE),
        LI = sum(LI, na.rm = TRUE),
        MLL = sum(MLL, na.rm = TRUE),
        CodedSchoolCount = sum(IsSchool),
        ProgramRecordCount = sum(!IsSchool),
        .by = all_of(enrollment_keys)
      )
  }

  result <- source_data |>
    select(-any_of(old_calculated_columns)) |>
    left_join(weighted, by = weighted_keys) |>
    left_join(enrollment, by = enrollment_keys) |>
    mutate(
      across(
        c(ProposedOpportunityFundingAmount, ProposedOperationalFundingAmount),
        ~ coalesce(.x, 0)
      ),
      ProposedWeightedFundingAmount = ProposedOpportunityFundingAmount + ProposedOperationalFundingAmount,
      ProposedModelFundingAmount = ProposedPositionBasedFundingAmount + ProposedWeightedFundingAmount,
      PositionBasedFundingDifference = ProposedPositionBasedFundingAmount - CurrentModelFundingAmount,
      PreliminaryPositionBasedPercentDifference = percent_difference(
        ProposedPositionBasedFundingAmount, CurrentModelFundingAmount
      ),
      PositionBasedPercentDifference = if_else(
        ComparisonComplete, PreliminaryPositionBasedPercentDifference, NA_real_
      ),
      FullModelFundingDifference = ProposedModelFundingAmount - CurrentModelFundingAmount,
      PreliminaryFullModelPercentDifference = percent_difference(
        ProposedModelFundingAmount, CurrentModelFundingAmount
      ),
      FullModelPercentDifference = if_else(
        ComparisonComplete, PreliminaryFullModelPercentDifference, NA_real_
      ),
      ReportPositionBasedPercentDifference = PreliminaryPositionBasedPercentDifference,
      ReportGrossFullModelPercentDifference = PreliminaryFullModelPercentDifference,
      PositionBasedDirection = direction(PositionBasedFundingDifference),
      GrossFullModelDirection = direction(FullModelFundingDifference),
      CurrentModeledFundingPerStudent = safe_divide(CurrentModelFundingAmount, Enrollment),
      ProposedPositionBasedFundingPerStudent = safe_divide(
        ProposedPositionBasedFundingAmount, Enrollment
      ),
      ProposedWeightedFundingPerStudent = safe_divide(ProposedWeightedFundingAmount, Enrollment),
      ProposedFullModelFundingPerStudent = safe_divide(ProposedModelFundingAmount, Enrollment),
      AdjustedReportingScope = TRUE,
      ExcludedLEA = excluded_lea_name,
      WeightedPoolTreatment = "Fixed pools redistributed across the adjusted reporting scope",
      WeightedRateMethodSource = weighted_rate_guidance_source
    )

  if (level == "LEA") {
    result <- result |>
      mutate(
        PositionBasedFundingDifferencePerStudent = safe_divide(
          PositionBasedFundingDifference, Enrollment
        ),
        FullModelFundingDifferencePerStudent = safe_divide(
          FullModelFundingDifference, Enrollment
        ),
        GrossFullModelPercentRank = min_rank(desc(PreliminaryFullModelPercentDifference)),
        PositionBasedPercentRank = min_rank(desc(PreliminaryPositionBasedPercentDifference))
      )
  }

  result
}

adjusted_school <- refresh_comparison(school_source_adjusted, "School") |>
  arrange(DistrictName, SchoolName)

adjusted_lea <- refresh_comparison(lea_source_adjusted, "LEA") |>
  arrange(DistrictName)


# ADJUSTED STATE SUMMARY ---------------------------------------------------------

scope_counts <- shared_school |>
  summarise(
    Enrollment = sum(Enrollment, na.rm = TRUE),
    RegularEdEnrollment = sum(RegularEdEnrollment, na.rm = TRUE),
    SpecialEdEnrollment = sum(SpecialEdEnrollment, na.rm = TRUE),
    LI = sum(LI, na.rm = TRUE),
    MLL = sum(MLL, na.rm = TRUE),
    K8Enrollment = sum(K8Enrollment, na.rm = TRUE),
    Grade10Enrollment = sum(Grade10Enrollment, na.rm = TRUE),
    EnrollmentPreK = sum(EnrollmentPreK, na.rm = TRUE),
    EnrollmentBasicK12 = sum(EnrollmentBasicK12, na.rm = TRUE),
    EnrollmentIntense = sum(EnrollmentIntense, na.rm = TRUE),
    EnrollmentComplex = sum(EnrollmentComplex, na.rm = TRUE),
    CodedSchoolCount = sum(IsSchool),
    ProgramRecordCount = sum(!IsSchool),
    LEACount = n_distinct(DistrictCode)
  )

current_state <- current_adjusted |>
  summarise(
    CurrentCalculatorSuppliedFundingAmount = sum(CalculatorSuppliedFundingAmount, na.rm = TRUE),
    CurrentDocumentedCrosswalkFundingAmount = sum(DocumentedCrosswalkFundingAmount, na.rm = TRUE),
    CurrentAnalyticalAssumptionFundingAmount = sum(AnalyticalAssumptionFundingAmount, na.rm = TRUE),
    CurrentModelFundingAmount = sum(FundingAmount, na.rm = TRUE),
    CurrentComponentsMissingInput = n_distinct(Component[!InputComplete]),
    CurrentRowsMissingInput = sum(!InputComplete),
    CurrentComponentsMissingRate = n_distinct(
      Component[CalculationComplete & abs(FundingQuantity) > 1e-8 & !RateAvailable]
    ),
    CurrentRowsMissingRate = sum(
      CalculationComplete & abs(FundingQuantity) > 1e-8 & !RateAvailable
    ),
    CurrentModelComplete = all(FundingComplete)
  )

proposed_state <- proposed_adjusted |>
  filter(FundingSection %in% c(base_section, central_section)) |>
  summarise(
    ProposedBaseFundingAmount = sum(FundingAmount[FundingSection == base_section], na.rm = TRUE),
    ProposedCentralOfficeFundingAmount = sum(
      FundingAmount[FundingSection == central_section], na.rm = TRUE
    ),
    ProposedPositionBasedFundingAmount = sum(FundingAmount, na.rm = TRUE),
    ProposedCalculationUnitCount = sum(adjusted_lea$ProposedCalculationUnitCount, na.rm = TRUE),
    ProposedPrincipalPositions = sum(
      proposed_adjusted$FundingQuantity[proposed_adjusted$Component == "Principal"],
      na.rm = TRUE
    ),
    ProposedModelComplete = all(FundingComplete)
  )

adjusted_state <- bind_cols(scope_counts, current_state, proposed_state) |>
  mutate(
    SchoolYear = school_year,
    CountDate = count_date,
    ReportingScope = paste("Excludes DAFB and", excluded_lea_name),
    CharterLEACount = sum(adjusted_lea$LEAType == "Charter"),
    DistrictLEACount = sum(adjusted_lea$LEAType == "District"),
    ProposedOpportunityFundingAmount = opportunity_funding_pool,
    ProposedOperationalFundingAmount = operational_funding_pool,
    ProposedWeightedFundingAmount = opportunity_funding_pool + operational_funding_pool,
    ProposedModelFundingAmount = ProposedPositionBasedFundingAmount + ProposedWeightedFundingAmount,
    PositionBasedFundingDifference = ProposedPositionBasedFundingAmount - CurrentModelFundingAmount,
    PreliminaryPositionBasedPercentDifference = percent_difference(
      ProposedPositionBasedFundingAmount, CurrentModelFundingAmount
    ),
    FullModelFundingDifference = ProposedModelFundingAmount - CurrentModelFundingAmount,
    PreliminaryFullModelPercentDifference = percent_difference(
      ProposedModelFundingAmount, CurrentModelFundingAmount
    ),
    ComparisonComplete = CurrentModelComplete & ProposedModelComplete,
    PositionBasedPercentDifference = if_else(
      ComparisonComplete, PreliminaryPositionBasedPercentDifference, NA_real_
    ),
    FullModelPercentDifference = if_else(
      ComparisonComplete, PreliminaryFullModelPercentDifference, NA_real_
    ),
    ReportPositionBasedPercentDifference = PreliminaryPositionBasedPercentDifference,
    ReportGrossFullModelPercentDifference = PreliminaryFullModelPercentDifference,
    ComparisonStatus = if_else(
      ComparisonComplete,
      "Complete",
      "Preliminary because the current model has missing inputs or rates"
    ),
    ComparisonInterpretation = paste(
      "Gross modeled allocation comparison; current appropriations outside",
      "the recreated baseline have not been netted against proposed weighted funding."
    ),
    ExcludedLEACode = excluded_lea_code,
    ExcludedLEAName = excluded_lea_name,
    ExclusionReason = excluded_lea_reason,
    OperationalEnrollmentBasis = operational_enrollment_basis,
    WeightedRateMethod = "recalculated for adjusted reporting scope",
    .before = 1
  )


# LEA DISTRIBUTION ANALYSES ------------------------------------------------------

make_distribution_row <- function(label, proposed, difference, percent) {
  tibble(
    Comparison = label,
    LEACount = nrow(adjusted_lea),
    CurrentFundingTotal = sum(adjusted_lea$CurrentModelFundingAmount, na.rm = TRUE),
    ProposedFundingTotal = sum(proposed, na.rm = TRUE),
    FundingDifferenceTotal = sum(difference, na.rm = TRUE),
    StatewideAggregatePercentDifference = percent_difference(
      ProposedFundingTotal, CurrentFundingTotal
    ),
    MeanLEAPercentDifference = mean(percent, na.rm = TRUE),
    MedianLEAPercentDifference = median(percent, na.rm = TRUE),
    LEAPercentQ1 = q_value(percent, 0.25),
    LEAPercentQ3 = q_value(percent, 0.75),
    MinimumLEAPercentDifference = min(percent, na.rm = TRUE),
    MaximumLEAPercentDifference = max(percent, na.rm = TRUE),
    MeanLEADollarDifference = mean(difference, na.rm = TRUE),
    MedianLEADollarDifference = median(difference, na.rm = TRUE),
    EnrollmentWeightedMeanPercentDifference = weighted.mean(
      percent, adjusted_lea$Enrollment, na.rm = TRUE
    )
  )
}

lea_distribution <- bind_rows(
  make_distribution_row(
    "Position-based: proposed Base plus Central Office",
    adjusted_lea$ProposedPositionBasedFundingAmount,
    adjusted_lea$PositionBasedFundingDifference,
    adjusted_lea$PreliminaryPositionBasedPercentDifference
  ),
  make_distribution_row(
    "Gross full model: all four proposed sections",
    adjusted_lea$ProposedModelFundingAmount,
    adjusted_lea$FullModelFundingDifference,
    adjusted_lea$PreliminaryFullModelPercentDifference
  )
)

lea_type_summary <- adjusted_lea |>
  summarise(
    LEACount = n(),
    Enrollment = sum(Enrollment, na.rm = TRUE),
    CurrentFundingTotal = sum(CurrentModelFundingAmount, na.rm = TRUE),
    ProposedPositionBasedFundingTotal = sum(ProposedPositionBasedFundingAmount, na.rm = TRUE),
    PositionBasedFundingDifference = sum(PositionBasedFundingDifference, na.rm = TRUE),
    AggregatePositionBasedPercentDifference = percent_difference(
      ProposedPositionBasedFundingTotal, CurrentFundingTotal
    ),
    MeanPositionBasedLEAPercentDifference = mean(
      PreliminaryPositionBasedPercentDifference, na.rm = TRUE
    ),
    MedianPositionBasedLEAPercentDifference = median(
      PreliminaryPositionBasedPercentDifference, na.rm = TRUE
    ),
    PositionBasedIncreaseCount = sum(PositionBasedDirection == "Increase"),
    PositionBasedDecreaseCount = sum(PositionBasedDirection == "Decrease"),
    ProposedWeightedFundingTotal = sum(ProposedWeightedFundingAmount, na.rm = TRUE),
    ProposedFullModelFundingTotal = sum(ProposedModelFundingAmount, na.rm = TRUE),
    FullModelFundingDifference = sum(FullModelFundingDifference, na.rm = TRUE),
    AggregateFullModelPercentDifference = percent_difference(
      ProposedFullModelFundingTotal, CurrentFundingTotal
    ),
    MeanFullModelLEAPercentDifference = mean(
      PreliminaryFullModelPercentDifference, na.rm = TRUE
    ),
    MedianFullModelLEAPercentDifference = median(
      PreliminaryFullModelPercentDifference, na.rm = TRUE
    ),
    FullModelIncreaseCount = sum(GrossFullModelDirection == "Increase"),
    FullModelDecreaseCount = sum(GrossFullModelDirection == "Decrease"),
    .by = LEAType
  ) |>
  arrange(LEAType)

ranking_columns <- c(
  "DistrictCode", "DistrictName", "LEAType", "Enrollment",
  "CurrentModelFundingAmount", "ProposedPositionBasedFundingAmount",
  "ProposedWeightedFundingAmount", "ProposedModelFundingAmount",
  "PositionBasedFundingDifference", "PreliminaryPositionBasedPercentDifference",
  "FullModelFundingDifference", "PreliminaryFullModelPercentDifference"
)

rank_table <- function(data, label) {
  data |>
    select(all_of(ranking_columns)) |>
    mutate(RankingTable = label, Rank = row_number(), .before = 1)
}

lea_rankings <- bind_rows(
  rank_table(
    adjusted_lea |> arrange(desc(PreliminaryFullModelPercentDifference)) |> slice_head(n = top_n_leas),
    "Largest gross full-model percentage increases"
  ),
  rank_table(
    adjusted_lea |> arrange(PreliminaryFullModelPercentDifference) |> slice_head(n = top_n_leas),
    "Smallest gross full-model percentage increases"
  ),
  rank_table(
    adjusted_lea |> arrange(PositionBasedFundingDifference) |> slice_head(n = top_n_leas),
    "Largest position-based dollar decreases"
  )
)


# WEIGHTED COMPONENTS -----------------------------------------------------------

adjusted_weighted_components <- adjusted_weighted_detail |>
  summarise(
    RawInputCount = sum(RawInputValue, na.rm = TRUE),
    WeightedCount = sum(FundingQuantity, na.rm = TRUE),
    AdjustedFundingRate = first(AdjustedFundingRate),
    FundingAmount = sum(AdjustedFundingAmount, na.rm = TRUE),
    .by = c(FundingSection, Component)
  ) |>
  mutate(
    FundingSectionLabel = case_when(
      FundingSection == opportunity_section ~ "Opportunity Funding",
      FundingSection == operational_section ~ "Operational Funding",
      TRUE ~ FundingSection
    ),
    ReportingScope = adjusted_state$ReportingScope,
    .before = 1
  ) |>
  arrange(FundingSection, Component)


# REPORT SUMMARY AND REPORT-READY TEXT -----------------------------------------

position_distribution <- lea_distribution |> slice(1)
full_distribution <- lea_distribution |> slice(2)
charter_summary <- lea_type_summary |> filter(LEAType == "Charter")
district_summary <- lea_type_summary |> filter(LEAType == "District")

report_summary <- adjusted_state |>
  transmute(
    SchoolYear, CountDate, ReportingScope,
    Enrollment, LEACount, DistrictLEACount, CharterLEACount,
    CodedSchoolCount, ProgramRecordCount, ProposedCalculationUnitCount,
    CurrentModelFundingAmount,
    ProposedBaseFundingAmount, ProposedCentralOfficeFundingAmount,
    ProposedPositionBasedFundingAmount, PositionBasedFundingDifference,
    PreliminaryPositionBasedPercentDifference,
    ProposedOpportunityFundingAmount, ProposedOperationalFundingAmount,
    ProposedWeightedFundingAmount, ProposedModelFundingAmount,
    FullModelFundingDifference, PreliminaryFullModelPercentDifference,
    PositionBasedLEAsIncreasing = sum(adjusted_lea$PositionBasedDirection == "Increase"),
    PositionBasedLEAsDecreasing = sum(adjusted_lea$PositionBasedDirection == "Decrease"),
    FullModelLEAsIncreasing = sum(adjusted_lea$GrossFullModelDirection == "Increase"),
    FullModelLEAsDecreasing = sum(adjusted_lea$GrossFullModelDirection == "Decrease"),
    MedianPositionBasedLEAPercentDifference = position_distribution$MedianLEAPercentDifference,
    PositionBasedLEAPercentQ1 = position_distribution$LEAPercentQ1,
    PositionBasedLEAPercentQ3 = position_distribution$LEAPercentQ3,
    MeanPositionBasedLEAPercentDifference = position_distribution$MeanLEAPercentDifference,
    MedianFullModelLEAPercentDifference = full_distribution$MedianLEAPercentDifference,
    FullModelLEAPercentQ1 = full_distribution$LEAPercentQ1,
    FullModelLEAPercentQ3 = full_distribution$LEAPercentQ3,
    MeanFullModelLEAPercentDifference = full_distribution$MeanLEAPercentDifference,
    CharterPositionBasedIncreaseCount = charter_summary$PositionBasedIncreaseCount,
    DistrictPositionBasedDecreaseCount = district_summary$PositionBasedDecreaseCount,
    CurrentComponentsMissingInput, CurrentRowsMissingInput,
    CurrentComponentsMissingRate, CurrentRowsMissingRate,
    ComparisonComplete, ComparisonStatus, ComparisonInterpretation,
    ExcludedLEACode, ExcludedLEAName, ExclusionReason
  )

scope_text <- paste0(
  "The adjusted statewide reporting context includes ", format_count(report_summary$Enrollment),
  " students, ", format_count(report_summary$LEACount), " LEAs, ",
  format_count(report_summary$CodedSchoolCount), " coded schools, and ",
  format_count(report_summary$ProgramRecordCount),
  " vocational/program records without school codes. DAFB and ",
  excluded_lea_name, " are excluded from the reported totals."
)

funding_text <- paste0(
  "The full proposed model allocates ", format_money_m(report_summary$ProposedModelFundingAmount),
  ", compared with ", format_money_m(report_summary$CurrentModelFundingAmount),
  " captured in the partial recreated current model, producing a gross modeled allocation difference of ",
  format_money_m(report_summary$FullModelFundingDifference, TRUE), " (",
  format_pct(report_summary$PreliminaryFullModelPercentDifference, TRUE),
  "). Proposed Base and Central Office Funding totals ",
  format_money_m(report_summary$ProposedPositionBasedFundingAmount), ", a difference of ",
  format_money_m(report_summary$PositionBasedFundingDifference, TRUE), " (",
  format_pct(report_summary$PreliminaryPositionBasedPercentDifference, TRUE),
  ") before weighted funding is added."
)

distribution_text <- paste0(
  "All ", format_count(report_summary$LEACount),
  " included LEAs increase under the gross full-model comparison. The statewide gross increase is ",
  format_pct(report_summary$PreliminaryFullModelPercentDifference, TRUE),
  ". Across LEAs, the median increase is ",
  format_pct(report_summary$MedianFullModelLEAPercentDifference, TRUE),
  ", with the middle half ranging from ",
  format_pct(report_summary$FullModelLEAPercentQ1, TRUE), " to ",
  format_pct(report_summary$FullModelLEAPercentQ3, TRUE),
  ". Under the position-based comparison, ",
  format_count(report_summary$PositionBasedLEAsIncreasing), " LEAs increase and ",
  format_count(report_summary$PositionBasedLEAsDecreasing),
  " decrease. The statewide position-based change is ",
  format_pct(report_summary$PreliminaryPositionBasedPercentDifference, TRUE),
  ", while the median LEA change is ",
  format_pct(report_summary$MedianPositionBasedLEAPercentDifference, TRUE),
  ". All ", format_count(report_summary$CharterLEACount),
  " included charter LEAs increase, while ",
  format_count(report_summary$DistrictPositionBasedDecreaseCount), " of the ",
  format_count(report_summary$DistrictLEACount), " traditional districts decrease."
)

report_ready_text <- tibble(
  DisplayOrder = 1:4,
  ReportSection = c(
    "Key finding: adjusted scope",
    "Key finding: statewide funding",
    "Key finding: LEA distribution",
    "Interpretation guardrail"
  ),
  Text = c(
    scope_text,
    funding_text,
    distribution_text,
    paste(
      "The full-model result is a gross modeled allocation difference, not a confirmed net increase in state spending.",
      "The current baseline remains incomplete, and current appropriations that may continue, be consolidated, or be replaced",
      "by Opportunity or Operational Funding have not yet been subtracted."
    )
  )
)


# EXCLUSION AUDIT ---------------------------------------------------------------

original_state <- state_source |> slice(1)

exclusion_audit <- tibble(
  Metric = c(
    "LEAs", "Coded schools", "Total enrollment", "Proposed calculation units",
    "Current modeled funding", "Proposed position-based funding",
    "Opportunity Funding pool", "Operational Funding pool",
    "Proposed full-model funding", "Gross full-model funding difference"
  ),
  OriginalStatewideValue = c(
    sum(lea_source$IncludeInStatewide),
    sum(shared$AggregationLevel == "School" & shared$DistrictCode != dafb_district_code & shared$IsSchool),
    sum(shared$Enrollment[shared$AggregationLevel == "School" & shared$DistrictCode != dafb_district_code], na.rm = TRUE),
    original_state$ProposedCalculationUnitCount,
    original_state$CurrentModelFundingAmount,
    original_state$ProposedPositionBasedFundingAmount,
    original_state$ProposedOpportunityFundingAmount,
    original_state$ProposedOperationalFundingAmount,
    original_state$ProposedModelFundingAmount,
    original_state$FullModelFundingDifference
  ),
  AdjustedReportingValue = c(
    adjusted_state$LEACount, adjusted_state$CodedSchoolCount, adjusted_state$Enrollment,
    adjusted_state$ProposedCalculationUnitCount, adjusted_state$CurrentModelFundingAmount,
    adjusted_state$ProposedPositionBasedFundingAmount,
    adjusted_state$ProposedOpportunityFundingAmount,
    adjusted_state$ProposedOperationalFundingAmount,
    adjusted_state$ProposedModelFundingAmount,
    adjusted_state$FullModelFundingDifference
  ),
  Difference = AdjustedReportingValue - OriginalStatewideValue,
  ExcludedLEA = excluded_lea_name,
  ExclusionReason = excluded_lea_reason,
  PoolTreatment = if_else(
    Metric %in% c("Opportunity Funding pool", "Operational Funding pool"),
    "Fixed pool retained and redistributed",
    "Excluded from adjusted reporting scope"
  )
)


# QC ----------------------------------------------------------------------------

school_weighted <- sum(adjusted_school$ProposedWeightedFundingAmount, na.rm = TRUE)
lea_weighted <- sum(adjusted_lea$ProposedWeightedFundingAmount, na.rm = TRUE)
current_lea_total <- sum(adjusted_lea$CurrentModelFundingAmount, na.rm = TRUE)
proposed_lea_total <- sum(adjusted_lea$ProposedModelFundingAmount, na.rm = TRUE)

reporting_qc <- tibble(
  Check = c(
    "Excluded LEA rows remaining in school output",
    "Excluded LEA rows remaining in LEA output",
    "DAFB rows remaining in adjusted outputs",
    "Opportunity pool difference",
    "Operational pool difference",
    "School-to-LEA weighted funding difference",
    "LEA-to-state weighted funding difference",
    "Current LEA-to-state funding difference",
    "Proposed LEA-to-state funding difference",
    "Official percent fields populated on incomplete LEA comparisons",
    "Unmatched school records",
    "Unmatched LEA records"
  ),
  Value = c(
    sum(is_excluded_lea(adjusted_school$DistrictCode, adjusted_school$DistrictName)),
    sum(is_excluded_lea(adjusted_lea$DistrictCode, adjusted_lea$DistrictName)),
    sum(adjusted_school$DistrictCode == dafb_district_code) +
      sum(adjusted_lea$DistrictCode == dafb_district_code),
    adjusted_rates$AdjustedFundingAmount[adjusted_rates$FundingSection == opportunity_section] -
      opportunity_funding_pool,
    adjusted_rates$AdjustedFundingAmount[adjusted_rates$FundingSection == operational_section] -
      operational_funding_pool,
    school_weighted - lea_weighted,
    lea_weighted - adjusted_state$ProposedWeightedFundingAmount,
    current_lea_total - adjusted_state$CurrentModelFundingAmount,
    proposed_lea_total - adjusted_state$ProposedModelFundingAmount,
    adjusted_lea |>
      filter(
        !ComparisonComplete,
        !is.na(PositionBasedPercentDifference) | !is.na(FullModelPercentDifference)
      ) |>
      nrow(),
    sum(adjusted_school$ModelMatchStatus != "Matched"),
    sum(adjusted_lea$ModelMatchStatus != "Matched")
  )
) |>
  mutate(
    Status = case_when(
      str_detect(Check, "difference|Difference") & abs(as.numeric(Value)) <= 0.10 ~ "Pass",
      str_detect(Check, "remaining|populated|Unmatched") & as.numeric(Value) == 0 ~ "Pass",
      TRUE ~ "Review"
    )
  )


# EXPORT -----------------------------------------------------------------------

write_model_csv(adjusted_school, output_paths[["school"]])
write_model_csv(adjusted_lea, output_paths[["lea"]])
write_model_csv(adjusted_state, output_paths[["state"]])
write_model_csv(adjusted_rates, output_paths[["rates"]])
write_model_csv(adjusted_weighted_components, output_paths[["components"]])
write_review_csv(lea_distribution, output_paths[["distribution"]])
write_review_csv(lea_type_summary, output_paths[["type"]])
write_review_csv(lea_rankings, output_paths[["rankings"]])
write_review_csv(report_summary, output_paths[["report"]])
write_csv(report_ready_text, output_paths[["text"]], na = "")
write_review_csv(exclusion_audit, output_paths[["exclusion"]])
write_review_csv(reporting_qc, output_paths[["qc"]])

stop_if_rows(
  reporting_qc |> filter(Status == "Review"),
  "One or more Step 10 reporting QC checks require review."
)

message("Created adjusted school comparison: ", output_paths[["school"]])
message("Created adjusted LEA comparison: ", output_paths[["lea"]])
message("Created adjusted state summary: ", output_paths[["state"]])
message("Created LEA distribution summary: ", output_paths[["distribution"]])
message("Created report summary: ", output_paths[["report"]])
message("Created report-ready text: ", output_paths[["text"]])
message("Review Step 10 QC: ", output_paths[["qc"]])
