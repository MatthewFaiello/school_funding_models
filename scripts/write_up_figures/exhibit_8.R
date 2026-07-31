library(dplyr)
library(readr)
library(tibble)

# Assumes the working directory is the project root
staffing_lea <- read_csv(
  "data/output/final/11_staffing_lea_comparison.csv",
  show_col_types = FALSE
)

staffing_components <- read_csv(
  "data/output/final/11_staffing_component_comparison.csv",
  show_col_types = FALSE
)

proposed_detail <- read_csv(
  "data/output/intermediate/08_proposed_model_funding_detail.csv",
  show_col_types = FALSE
)

# Identify working categories excluded from the comparable amount subtotal
noncomparable_categories <- staffing_components |>
  filter(
    IncludedInWorkingTotal,
    !IncludedInComparableAmountSubtotal
  ) |>
  pull(ComparisonCategory)

noncomparable_categories
# Expected: "Buildings and Grounds Supervisor"

# Calculate the proposed amounts to exclude for each LEA
noncomparable_proposed_lea <- proposed_detail |>
  filter(
    IncludeInStatewide,
    Component %in% noncomparable_categories
  ) |>
  group_by(DistrictCode) |>
  summarise(
    NoncomparableProposedFundingAmount =
      sum(FundingAmount, na.rm = TRUE),
    .groups = "drop"
  )

# Create LEA-level comparable amounts
staffing_lea_comparable <- staffing_lea |>
  filter(AnalysisSection == "Staffing rules") |>
  left_join(
    noncomparable_proposed_lea,
    by = "DistrictCode"
  ) |>
  mutate(
    NoncomparableProposedFundingAmount =
      coalesce(NoncomparableProposedFundingAmount, 0),
    
    # The current working total already excludes unknown current amounts
    ComparableCurrentFundingAmount =
      WorkingCurrentFundingAmount,
    
    ComparableProposedFundingAmount =
      WorkingProposedFundingAmount -
      NoncomparableProposedFundingAmount,
    
    ComparableFundingDifference =
      ComparableProposedFundingAmount -
      ComparableCurrentFundingAmount,
    
    ComparablePercentDifference = if_else(
      abs(ComparableCurrentFundingAmount) > 1e-8,
      100 * ComparableFundingDifference /
        ComparableCurrentFundingAmount,
      NA_real_
    )
  )

summarize_lea_group <- function(data, lea_type_label) {
  
  current_total <- sum(
    data$ComparableCurrentFundingAmount,
    na.rm = TRUE
  )
  
  proposed_total <- sum(
    data$ComparableProposedFundingAmount,
    na.rm = TRUE
  )
  
  tibble(
    `LEA type` = lea_type_label,
    LEAs = n_distinct(data$DistrictCode),
    Current = current_total,
    Proposed = proposed_total,
    Difference = proposed_total - current_total,
    `Aggregate change` =
      100 * (proposed_total - current_total) / current_total,
    `Median LEA change` =
      median(data$ComparablePercentDifference, na.rm = TRUE),
    Increases =
      sum(data$ComparableFundingDifference > 0, na.rm = TRUE),
    Decreases =
      sum(data$ComparableFundingDifference < 0, na.rm = TRUE),
    No_change =
      sum(data$ComparableFundingDifference == 0, na.rm = TRUE)
  )
}

# Calculate district, charter, and statewide rows
exhibit8_raw <- bind_rows(
  staffing_lea_comparable |>
    filter(LEAType == "District") |>
    summarize_lea_group("Districts"),
  
  staffing_lea_comparable |>
    filter(LEAType == "Charter") |>
    summarize_lea_group("Charters"),
  
  staffing_lea_comparable |>
    summarize_lea_group("All LEAs")
)

exhibit8_raw