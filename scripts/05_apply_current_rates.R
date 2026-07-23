# =============================================================================
# 05_apply_current_rates.R
# =============================================================================
# Joins current-model quantities to the reviewed rate map and funding rates.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

current_quantities_path <- file.path(
  output_dir,
  "04_current_model_quantities.csv"
)
current_funding_detail_path <- file.path(
  output_dir,
  "05_current_model_funding_detail.csv"
)
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
current_rate_issues_path <- file.path(
  output_dir,
  "05_current_model_rate_issues.csv"
)

check_required_files(c(
  current_quantities_path,
  funding_rates_path,
  current_rate_map_path
))


# READ INPUTS -------------------------------------------------------------------

quantities <- read_csv(
  current_quantities_path,
  show_col_types = FALSE
) |>
  mutate(
    IncludeInStatewide = as.logical(IncludeInStatewide),
    IsSchool = as.logical(IsSchool),
    InputComplete = as.logical(InputComplete),
    QuantityProvisional = as.logical(QuantityProvisional),
    CalculationComplete = as.logical(CalculationComplete)
  )

funding_rates <- read_csv(
  funding_rates_path,
  show_col_types = FALSE
) |>
  transmute(
    RateComponent = Component,
    FundingSectionFromRate = `Funding Section`,
    FundingRate = as.numeric(`Funding Rate`),
    RateBasis = `Rate Basis`,
    RateSourceCell = `Source Cell`
  )

rate_map <- read_csv(
  current_rate_map_path,
  show_col_types = FALSE
)

check_required_columns(
  rate_map,
  c("Component", "RateComponent", "RateCategory", "RateNote"),
  "current_rate_map.csv"
)

valid_rate_categories <- c(
  "Calculator-supplied",
  "Documented crosswalk",
  "Analytical assumption",
  "Not available"
)

stop_if_rows(
  rate_map |>
    filter(!RateCategory %in% valid_rate_categories),
  "current_rate_map.csv contains an unrecognized RateCategory."
)

stop_if_rows(
  rate_map |>
    count(Component) |>
    filter(n > 1),
  "current_rate_map.csv contains duplicate components."
)

stop_if_rows(
  funding_rates |>
    count(RateComponent) |>
    filter(n > 1),
  "funding_rates.csv contains duplicate rate components."
)

stop_if_rows(
  quantities |>
    distinct(Component) |>
    anti_join(rate_map, by = "Component"),
  "A current-model component is missing from current_rate_map.csv."
)


# APPLY RATES -------------------------------------------------------------------

current_model_funding_detail <- quantities |>
  left_join(rate_map, by = "Component") |>
  left_join(funding_rates, by = "RateComponent") |>
  mutate(
    FundingSection = coalesce(FundingSectionFromRate, "Rate missing"),
    RateAvailable = !is.na(FundingRate),

    FundingAmount = case_when(
      !CalculationComplete ~ NA_real_,
      abs(FundingQuantity) < 1e-8 ~ 0,
      !RateAvailable ~ NA_real_,
      TRUE ~ FundingQuantity * FundingRate
    ),

    CalculatorSuppliedFundingAmount = case_when(
      RateCategory == "Calculator-supplied" & !is.na(FundingAmount) ~
        FundingAmount,
      TRUE ~ 0
    ),

    DocumentedCrosswalkFundingAmount = case_when(
      RateCategory == "Documented crosswalk" & !is.na(FundingAmount) ~
        FundingAmount,
      TRUE ~ 0
    ),

    AnalyticalAssumptionFundingAmount = case_when(
      RateCategory == "Analytical assumption" & !is.na(FundingAmount) ~
        FundingAmount,
      TRUE ~ 0
    ),

    FundingComplete =
      InputComplete &
      CalculationComplete &
      (abs(FundingQuantity) < 1e-8 | RateAvailable)
  ) |>
  select(-FundingSectionFromRate) |>
  arrange(DistrictName, CalculationLevel, SchoolName, Component)


# SCHOOL SUMMARY ---------------------------------------------------------------

current_school_summary <- current_model_funding_detail |>
  filter(CalculationLevel == "School") |>
  summarise(
    CalculatedPositions = sum(FundingQuantity, na.rm = TRUE),
    CalculatorSuppliedFundingAmount = sum(CalculatorSuppliedFundingAmount, na.rm = TRUE),
    DocumentedCrosswalkFundingAmount = sum(DocumentedCrosswalkFundingAmount, na.rm = TRUE),
    AnalyticalAssumptionFundingAmount = sum(AnalyticalAssumptionFundingAmount, na.rm = TRUE),
    TotalModeledFundingAmount = sum(FundingAmount, na.rm = TRUE),
    ComponentsMissingInput = n_distinct(Component[!InputComplete]),
    ComponentsMissingRate = n_distinct(
      Component[
        CalculationComplete &
          abs(FundingQuantity) > 1e-8 &
          !RateAvailable
      ]
    ),
    InputComplete = all(InputComplete),
    ProvisionalQuantityRows = sum(QuantityProvisional),
    QuantityCalculationComplete = all(CalculationComplete),
    FundingRateComplete = all(
      !CalculationComplete |
        abs(coalesce(FundingQuantity, 0)) < 1e-8 |
        RateAvailable
    ),
    OverallComplete = all(FundingComplete),
    .by = c(
      SchoolYear,
      CountDate,
      DistrictCode,
      DistrictName,
      LEAType,
      IncludeInStatewide,
      SchoolCode,
      SchoolName,
      IsSchool
    )
  ) |>
  arrange(DistrictName, SchoolName)


# LEA SUMMARY ------------------------------------------------------------------

current_lea_summary <- current_model_funding_detail |>
  summarise(
    CalculatedPositions = sum(FundingQuantity, na.rm = TRUE),
    CalculatorSuppliedFundingAmount = sum(CalculatorSuppliedFundingAmount, na.rm = TRUE),
    DocumentedCrosswalkFundingAmount = sum(DocumentedCrosswalkFundingAmount, na.rm = TRUE),
    AnalyticalAssumptionFundingAmount = sum(AnalyticalAssumptionFundingAmount, na.rm = TRUE),
    TotalModeledFundingAmount = sum(FundingAmount, na.rm = TRUE),
    ComponentsMissingInput = n_distinct(Component[!InputComplete]),
    ComponentsMissingRate = n_distinct(
      Component[
        CalculationComplete &
          abs(FundingQuantity) > 1e-8 &
          !RateAvailable
      ]
    ),
    InputComplete = all(InputComplete),
    ProvisionalQuantityRows = sum(QuantityProvisional),
    QuantityCalculationComplete = all(CalculationComplete),
    FundingRateComplete = all(
      !CalculationComplete |
        abs(coalesce(FundingQuantity, 0)) < 1e-8 |
        RateAvailable
    ),
    OverallComplete = all(FundingComplete),
    .by = c(
      SchoolYear,
      CountDate,
      DistrictCode,
      DistrictName,
      LEAType,
      IncludeInStatewide
    )
  ) |>
  arrange(DistrictName)


# STATE SUMMARY ----------------------------------------------------------------

current_state_summary <- current_model_funding_detail |>
  filter(IncludeInStatewide) |>
  summarise(
    CalculatedPositions = sum(FundingQuantity, na.rm = TRUE),
    CalculatorSuppliedFundingAmount = sum(CalculatorSuppliedFundingAmount, na.rm = TRUE),
    DocumentedCrosswalkFundingAmount = sum(DocumentedCrosswalkFundingAmount, na.rm = TRUE),
    AnalyticalAssumptionFundingAmount = sum(AnalyticalAssumptionFundingAmount, na.rm = TRUE),
    TotalModeledFundingAmount = sum(FundingAmount, na.rm = TRUE),
    RowsMissingInput = sum(!InputComplete),
    RowsMissingRate = sum(
      CalculationComplete &
        abs(FundingQuantity) > 1e-8 &
        !RateAvailable
    ),
    InputComplete = all(InputComplete),
    ProvisionalQuantityRows = sum(QuantityProvisional),
    QuantityCalculationComplete = all(CalculationComplete),
    FundingRateComplete = all(
      !CalculationComplete |
        abs(coalesce(FundingQuantity, 0)) < 1e-8 |
        RateAvailable
    ),
    OverallComplete = all(FundingComplete),
    .by = c(FundingSection, Component, RateCategory)
  ) |>
  mutate(StatewideScope = "Excludes DAFB", .before = 1) |>
  arrange(FundingSection, Component)


# RATE ISSUES ------------------------------------------------------------------

current_rate_issues <- current_model_funding_detail |>
  filter(
    CalculationComplete,
    abs(FundingQuantity) > 1e-8,
    !RateAvailable
  ) |>
  summarise(
    AffectedRows = n(),
    FundingQuantity = sum(FundingQuantity, na.rm = TRUE),
    .by = c(Component, RateCategory, RateNote)
  ) |>
  mutate(Action = "Provide or approve an applicable funding rate.") |>
  arrange(Component)


# EXPORT -----------------------------------------------------------------------

write_model_csv(
  current_model_funding_detail,
  current_funding_detail_path
)
write_model_csv(current_school_summary, current_school_summary_path)
write_model_csv(current_lea_summary, current_lea_summary_path)
write_model_csv(current_state_summary, current_state_summary_path)
write_review_csv(current_rate_issues, current_rate_issues_path)

message("Created current funding detail: ", current_funding_detail_path)
message("Created current LEA summary: ", current_lea_summary_path)
message("Review current rate issues: ", current_rate_issues_path)
