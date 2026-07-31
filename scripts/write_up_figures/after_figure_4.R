# =============================================================================
# LEA ENROLLMENT AND PROPOSED WEIGHTED-FUNDING RANKS
#
# Creates:
#   lea_rankings         Full list of all 43 LEAs with enrollment and funding ranks
#   correlation_summary Overall, district, and charter Spearman correlations
#   narrative_examples  LEAs cited in the Section 3.5 narrative
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Read proposed-model funding detail
# -----------------------------------------------------------------------------

proposed_detail <- read_csv(
  "data/output/intermediate/08_proposed_model_funding_detail.csv",
  show_col_types = FALSE
)

required_columns <- c(
  "DistrictCode",
  "DistrictName",
  "LEAType",
  "IncludeInStatewide",
  "FundingSection",
  "Component",
  "RawInputValue",
  "FundingAmount"
)

missing_columns <- setdiff(
  required_columns,
  names(proposed_detail)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "The input file is missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# -----------------------------------------------------------------------------
# 2. Keep the nine Opportunity and Operational components
# -----------------------------------------------------------------------------

weighted_components <- c(
  "Opportunity Funding - Low Income",
  "Opportunity Funding - Multilingual Learner",
  "Operational Funding - Enrollment",
  "Operational Funding - Low Income",
  "Operational Funding - Multilingual Learner",
  "Operational Funding - Basic Special Education",
  "Operational Funding - Intensive Special Education",
  "Operational Funding - Complex Special Education",
  "Operational Funding - Vocational"
)

weighted_detail <- proposed_detail |>
  filter(
    IncludeInStatewide,
    LEAType %in% c("District", "Charter"),
    FundingSection %in% c(
      "Opportunity Funding (State Support)",
      "Operational Funding (State Support)"
    ),
    Component %in% weighted_components
  )

# -----------------------------------------------------------------------------
# 3. Calculate combined weighted funding by LEA
# -----------------------------------------------------------------------------

lea_funding <- weighted_detail |>
  summarise(
    CombinedWeightedFunding = sum(
      FundingAmount,
      na.rm = TRUE
    ),
    .by = c(
      DistrictCode,
      DistrictName,
      LEAType
    )
  )

# -----------------------------------------------------------------------------
# 4. Calculate total enrollment by LEA
#
# The Operational Funding enrollment component contains the total-enrollment
# input. School-level rows are summed to the LEA level.
# -----------------------------------------------------------------------------

lea_enrollment <- weighted_detail |>
  filter(
    Component == "Operational Funding - Enrollment"
  ) |>
  summarise(
    TotalEnrollment = sum(
      RawInputValue,
      na.rm = TRUE
    ),
    .by = c(
      DistrictCode,
      DistrictName,
      LEAType
    )
  )

# -----------------------------------------------------------------------------
# 5. Join enrollment and funding and calculate ranks
#
# Rank 1 represents the largest enrollment or allocation.
#
# Positive SectorRankShift:
#   Funding rank is higher than enrollment rank.
#
# Negative SectorRankShift:
#   Funding rank is lower than enrollment rank.
# -----------------------------------------------------------------------------

lea_rankings <- lea_enrollment |>
  left_join(
    lea_funding,
    by = c(
      "DistrictCode",
      "DistrictName",
      "LEAType"
    )
  ) |>
  mutate(
    OverallEnrollmentRank = min_rank(
      desc(TotalEnrollment)
    ),
    
    OverallFundingRank = min_rank(
      desc(CombinedWeightedFunding)
    )
  ) |>
  group_by(LEAType) |>
  mutate(
    SectorEnrollmentRank = min_rank(
      desc(TotalEnrollment)
    ),
    
    SectorFundingRank = min_rank(
      desc(CombinedWeightedFunding)
    ),
    
    SectorRankShift =
      SectorEnrollmentRank - SectorFundingRank
  ) |>
  ungroup() |>
  mutate(
    LEAType = factor(
      LEAType,
      levels = c(
        "District",
        "Charter"
      )
    ),
    
    CombinedWeightedFundingMillions =
      CombinedWeightedFunding / 1e6,
    
    RankInterpretation = case_when(
      SectorRankShift > 0 ~
        "Funding rank above enrollment rank",
      
      SectorRankShift < 0 ~
        "Funding rank below enrollment rank",
      
      TRUE ~
        "Funding and enrollment ranks match"
    )
  ) |>
  arrange(
    LEAType,
    SectorEnrollmentRank
  ) |>
  select(
    LEAType,
    DistrictCode,
    DistrictName,
    TotalEnrollment,
    CombinedWeightedFunding,
    CombinedWeightedFundingMillions,
    OverallEnrollmentRank,
    OverallFundingRank,
    SectorEnrollmentRank,
    SectorFundingRank,
    SectorRankShift,
    RankInterpretation
  )

# -----------------------------------------------------------------------------
# 6. Calculate Spearman rank correlations
# -----------------------------------------------------------------------------

overall_correlation <- lea_rankings |>
  summarise(
    Scope = "All LEAs",
    LEAs = n(),
    SpearmanRho = cor(
      TotalEnrollment,
      CombinedWeightedFunding,
      method = "spearman",
      use = "complete.obs"
    )
  )

sector_correlations <- lea_rankings |>
  group_by(LEAType) |>
  summarise(
    Scope = as.character(first(LEAType)),
    LEAs = n(),
    SpearmanRho = cor(
      TotalEnrollment,
      CombinedWeightedFunding,
      method = "spearman",
      use = "complete.obs"
    ),
    .groups = "drop"
  ) |>
  select(
    Scope,
    LEAs,
    SpearmanRho
  )

correlation_summary <- bind_rows(
  overall_correlation,
  sector_correlations
) |>
  mutate(
    SpearmanRhoRounded = round(
      SpearmanRho,
      2
    )
  )

# -----------------------------------------------------------------------------
# 7. Pull the LEAs cited in the narrative
# -----------------------------------------------------------------------------

narrative_examples <- lea_rankings |>
  filter(
    str_detect(
      DistrictName,
      regex(
        paste(
          c(
            "Red Clay",
            "Christina",
            "Appoquinimink",
            "Capital",
            "Newark Charter",
            "Odyssey",
            "ASPIRA",
            "Charter School of Wilmington",
            "Edison"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    )
  ) |>
  arrange(
    LEAType,
    SectorEnrollmentRank
  )

# -----------------------------------------------------------------------------
# 8. Validation checks
# -----------------------------------------------------------------------------

stopifnot(
  # Primary scope
  nrow(lea_rankings) == 43,
  
  sum(
    lea_rankings$LEAType == "District"
  ) == 19,
  
  sum(
    lea_rankings$LEAType == "Charter"
  ) == 24,
  
  # Statewide enrollment
  sum(
    lea_rankings$TotalEnrollment
  ) == 140829,
  
  # Fixed Opportunity and Operational pools
  abs(
    sum(
      lea_rankings$CombinedWeightedFunding
    ) - 442026800
  ) < 0.05,
  
  # Correlations cited in the report
  round(
    overall_correlation$SpearmanRho,
    2
  ) == 0.93,
  
  round(
    sector_correlations$SpearmanRho[
      sector_correlations$Scope == "District"
    ],
    2
  ) == 0.94,
  
  round(
    sector_correlations$SpearmanRho[
      sector_correlations$Scope == "Charter"
    ],
    2
  ) == 0.69
)

message(
  "All LEA ranking and correlation checks passed."
)

# -----------------------------------------------------------------------------
# 9. Review the objects
# -----------------------------------------------------------------------------

correlation_summary

narrative_examples

lea_rankings

# Optional RStudio data viewers
View(correlation_summary)
View(narrative_examples)
View(lea_rankings)