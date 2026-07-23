# =============================================================================
# 08_apply_proposed_rates.R
# =============================================================================
# Applies funding rates to the proposed-model quantities from Script 07.
#
# The weighted component summary shows the raw count, weight, weighted count,
# funding rate, and funding amount for every Opportunity and Operational item.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

proposed_quantities_path <- file.path(
  output_dir,
  "07_proposed_model_quantities.csv"
)
proposed_funding_detail_path <- file.path(
  output_dir,
  "08_proposed_model_funding_detail.csv"
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
proposed_weighted_component_summary_path <- file.path(
  output_dir,
  "08_proposed_weighted_component_summary.csv"
)
proposed_weighted_rate_summary_path <- file.path(
  output_dir,
  "08_proposed_weighted_rate_summary.csv"
)
proposed_rate_issues_path <- file.path(
  output_dir,
  "08_proposed_model_rate_issues.csv"
)

check_required_files(c(proposed_quantities_path, funding_rates_path))


# READ INPUTS -------------------------------------------------------------------

quantities <- read_csv(
  proposed_quantities_path,
  show_col_types = FALSE
) |>
  mutate(
    IncludeInStatewide = as.logical(IncludeInStatewide),
    IsSchool = as.logical(IsSchool),
    IsSchoolCalculationUnit = as.logical(IsSchoolCalculationUnit),
    CalculationComplete = as.logical(CalculationComplete)
  )

funding_rates <- read_csv(
  funding_rates_path,
  show_col_types = FALSE
) |>
  transmute(
    RateComponent = Component,
    FundingSectionFromRate = `Funding Section`,
    ProvidedFundingRate = as.numeric(`Funding Rate`),
    RateBasis = `Rate Basis`,
    RateSourceCell = `Source Cell`
  )

stop_if_rows(
  funding_rates |>
    count(RateComponent) |>
    filter(n > 1),
  "funding_rates.csv contains duplicate rate components."
)

stop_if_rows(
  quantities |>
    distinct(RateComponent) |>
    anti_join(funding_rates, by = "RateComponent"),
  "A proposed-model component is missing from funding_rates.csv."
)

model_settings <- quantities |>
  distinct(OperationalEnrollmentBasis, CharterBuildingPolicy)

if (nrow(model_settings) != 1) {
  print(model_settings)
  stop(
    "The proposed quantity file contains more than one model-setting combination.",
    call. = FALSE
  )
}


# CALCULATE WEIGHTED FUNDING RATES ---------------------------------------------

weighted_sections <- c(
  "Opportunity Funding (State Support)",
  "Operational Funding (State Support)"
)

provided_weighted_rate_check <- funding_rates |>
  filter(FundingSectionFromRate %in% weighted_sections) |>
  summarise(
    DistinctProvidedRates = n_distinct(ProvidedFundingRate),
    .by = FundingSectionFromRate
  ) |>
  filter(DistinctProvidedRates != 1)

stop_if_rows(
  provided_weighted_rate_check,
  "Weighted components within a funding section must share one provided rate."
)

proposed_weighted_rate_summary <- quantities |>
  filter(
    FundingSection %in% weighted_sections,
    IncludeInStatewide,
    CalculationComplete
  ) |>
  summarise(
    TotalWeightedCount = sum(FundingQuantity, na.rm = TRUE),
    .by = FundingSection
  ) |>
  mutate(
    FundingPool = case_when(
      FundingSection == "Opportunity Funding (State Support)" ~
        opportunity_funding_pool,
      FundingSection == "Operational Funding (State Support)" ~
        operational_funding_pool
    ),
    RecalculatedFundingRate = FundingPool / TotalWeightedCount
  ) |>
  left_join(
    funding_rates |>
      filter(FundingSectionFromRate %in% weighted_sections) |>
      summarise(
        ProvidedFundingRate = first(ProvidedFundingRate),
        .by = FundingSectionFromRate
      ),
    by = c("FundingSection" = "FundingSectionFromRate")
  ) |>
  mutate(
    SelectedMethod = weighted_rate_method,
    SelectedFundingRate = if (
      weighted_rate_method == "recalculated"
    ) {
      RecalculatedFundingRate
    } else {
      ProvidedFundingRate
    },
    FundingPoolSource = weighted_pool_amount_source,
    RateMethodSource = if_else(
      weighted_rate_method == "recalculated",
      weighted_rate_guidance_source,
      weighted_pool_amount_source
    ),
    RateMethodNote = if_else(
      weighted_rate_method == "recalculated",
      weighted_rate_guidance_note,
      "Use the calculator-supplied per-weighted-student rate."
    ),
    OperationalEnrollmentBasis = operational_enrollment_basis,
    CharterBuildingPolicy = charter_building_policy,
    StatewideScope = "Excludes DAFB"
  ) |>
  select(
    StatewideScope,
    FundingSection,
    TotalWeightedCount,
    FundingPool,
    RecalculatedFundingRate,
    ProvidedFundingRate,
    SelectedMethod,
    SelectedFundingRate,
    FundingPoolSource,
    RateMethodSource,
    RateMethodNote,
    OperationalEnrollmentBasis,
    CharterBuildingPolicy
  )


# APPLY RATES -------------------------------------------------------------------

proposed_model_funding_detail <- quantities |>
  left_join(funding_rates, by = "RateComponent") |>
  left_join(
    proposed_weighted_rate_summary |>
      select(
        FundingSection,
        RecalculatedFundingRate,
        SelectedFundingRate,
        SelectedMethod
      ),
    by = "FundingSection"
  ) |>
  mutate(
    FundingRate = case_when(
      FundingSection %in% weighted_sections ~ SelectedFundingRate,
      TRUE ~ ProvidedFundingRate
    ),
    RateMethod = case_when(
      FundingSection %in% weighted_sections ~ SelectedMethod,
      TRUE ~ "provided"
    ),
    RateAvailable = !is.na(FundingRate),
    FundingAmount = case_when(
      !CalculationComplete ~ NA_real_,
      abs(FundingQuantity) < 1e-8 ~ 0,
      !RateAvailable ~ NA_real_,
      TRUE ~ FundingQuantity * FundingRate
    ),
    FundingComplete =
      CalculationComplete &
      (abs(FundingQuantity) < 1e-8 | RateAvailable)
  ) |>
  select(-FundingSectionFromRate) |>
  arrange(
    DistrictName,
    CalculationLevel,
    SchoolName,
    CalculationUnitSequence,
    CalculationUnitName,
    FundingSection,
    Component
  )


# WEIGHTED COMPONENT SUMMARY ---------------------------------------------------

proposed_weighted_component_summary <- proposed_model_funding_detail |>
  filter(
    FundingSection %in% weighted_sections,
    IncludeInStatewide
  ) |>
  summarise(
    RawCount = sum(RawInputValue, na.rm = TRUE),
    AppliedWeight = first(AppliedFactor),
    WeightedCount = sum(FundingQuantity, na.rm = TRUE),
    FundingRate = first(FundingRate),
    FundingAmount = sum(FundingAmount, na.rm = TRUE),
    RowsMissingInput = sum(!CalculationComplete),
    RowsMissingRate = sum(
      CalculationComplete &
        abs(FundingQuantity) > 1e-8 &
        !RateAvailable
    ),
    .by = c(
      FundingSection,
      Component,
      OperationalEnrollmentBasis,
      CharterBuildingPolicy
    )
  ) |>
  mutate(
    StatewideScope = "Excludes DAFB",
    .before = 1
  ) |>
  arrange(FundingSection, Component)


# SCHOOL SUMMARY ---------------------------------------------------------------

proposed_school_summary <- proposed_model_funding_detail |>
  filter(CalculationLevel == "School") |>
  summarise(
    BaseCalculationUnitCount = sum(
      Component == "Principal" & IsSchoolCalculationUnit,
      na.rm = TRUE
    ),
    ProposedPrincipalPositions = sum(
      FundingQuantity[Component == "Principal"],
      na.rm = TRUE
    ),
    BaseFundingAmount = sum(
      FundingAmount[
        FundingSection == "Base Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    OpportunityFundingAmount = sum(
      FundingAmount[
        FundingSection == "Opportunity Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    OperationalFundingAmount = sum(
      FundingAmount[
        FundingSection == "Operational Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    TotalModeledFundingAmount = sum(FundingAmount, na.rm = TRUE),
    ComponentsMissingInput = n_distinct(Component[!CalculationComplete]),
    ComponentsMissingRate = n_distinct(
      Component[
        CalculationComplete &
          abs(FundingQuantity) > 1e-8 &
          !RateAvailable
      ]
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
      IsSchool,
      OperationalEnrollmentBasis,
      CharterBuildingPolicy
    )
  ) |>
  arrange(DistrictName, SchoolName)


# LEA SUMMARY ------------------------------------------------------------------

proposed_lea_summary <- proposed_model_funding_detail |>
  summarise(
    SchoolCalculationUnitCount = sum(
      CalculationLevel == "School" &
        Component == "Principal" &
        IsSchoolCalculationUnit,
      na.rm = TRUE
    ),
    ProposedPrincipalPositions = sum(
      FundingQuantity[
        CalculationLevel == "School" &
          Component == "Principal"
      ],
      na.rm = TRUE
    ),
    BaseFundingAmount = sum(
      FundingAmount[
        FundingSection == "Base Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    OpportunityFundingAmount = sum(
      FundingAmount[
        FundingSection == "Opportunity Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    OperationalFundingAmount = sum(
      FundingAmount[
        FundingSection == "Operational Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    CentralOfficeFundingAmount = sum(
      FundingAmount[
        FundingSection == "Central Office Funding (State Support)"
      ],
      na.rm = TRUE
    ),
    TotalModeledFundingAmount = sum(FundingAmount, na.rm = TRUE),
    ComponentsMissingInput = n_distinct(Component[!CalculationComplete]),
    ComponentsMissingRate = n_distinct(
      Component[
        CalculationComplete &
          abs(FundingQuantity) > 1e-8 &
          !RateAvailable
      ]
    ),
    OverallComplete = all(FundingComplete),
    .by = c(
      SchoolYear,
      CountDate,
      DistrictCode,
      DistrictName,
      LEAType,
      IncludeInStatewide,
      OperationalEnrollmentBasis,
      CharterBuildingPolicy
    )
  ) |>
  arrange(DistrictName)


# STATE SUMMARY ----------------------------------------------------------------

proposed_state_summary <- proposed_model_funding_detail |>
  filter(IncludeInStatewide) |>
  summarise(
    RawInputValue = sum(RawInputValue, na.rm = TRUE),
    FundingQuantity = sum(FundingQuantity, na.rm = TRUE),
    FundingRate = first(FundingRate),
    FundingAmount = sum(FundingAmount, na.rm = TRUE),
    RowsMissingInput = sum(!CalculationComplete),
    RowsMissingRate = sum(
      CalculationComplete &
        abs(FundingQuantity) > 1e-8 &
        !RateAvailable
    ),
    OverallComplete = all(FundingComplete),
    .by = c(
      FundingSection,
      Component,
      QuantityType,
      RateMethod,
      OperationalEnrollmentBasis,
      CharterBuildingPolicy
    )
  ) |>
  mutate(StatewideScope = "Excludes DAFB", .before = 1) |>
  arrange(FundingSection, Component)


# RATE ISSUES ------------------------------------------------------------------

proposed_rate_issues <- proposed_model_funding_detail |>
  filter(
    CalculationComplete,
    abs(FundingQuantity) > 1e-8,
    !RateAvailable
  ) |>
  summarise(
    AffectedRows = n(),
    FundingQuantity = sum(FundingQuantity, na.rm = TRUE),
    .by = c(FundingSection, Component, RateComponent)
  ) |>
  mutate(Action = "Provide an applicable funding rate.") |>
  arrange(FundingSection, Component)


# EXPORT -----------------------------------------------------------------------

write_model_csv(proposed_model_funding_detail, proposed_funding_detail_path)
write_model_csv(proposed_school_summary, proposed_school_summary_path)
write_model_csv(proposed_lea_summary, proposed_lea_summary_path)
write_model_csv(proposed_state_summary, proposed_state_summary_path)
write_review_csv(proposed_weighted_component_summary, proposed_weighted_component_summary_path)
write_review_csv(proposed_weighted_rate_summary, proposed_weighted_rate_summary_path)
write_review_csv(proposed_rate_issues, proposed_rate_issues_path)

message("Created proposed funding detail: ", proposed_funding_detail_path)
message(
  "Review raw counts and weights: ",
  proposed_weighted_component_summary_path
)
message("Review weighted funding rates: ", proposed_weighted_rate_summary_path)
