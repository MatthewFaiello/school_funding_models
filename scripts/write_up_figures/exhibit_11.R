library(dplyr)
library(readr)
library(tibble)

# Assumes the working directory is the project root
weighted_funding <- read_csv(
  "data/output/final/11_opportunity_operational_lea_comparison.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# Create one combined Opportunity + Operational allocation per LEA
# -----------------------------------------------------------------------------

lea_allocations <- weighted_funding |>
  filter(
    AnalysisSection == "Opportunity and Operational Funding",
    ReportingScope ==
      "Includes BASSE; excludes DAFB pending confirmation",
    FundingCategory %in% c(
      "Opportunity Funding",
      "Operational Funding"
    )
  ) |>
  group_by(
    DistrictCode,
    DistrictName,
    LEAType
  ) |>
  summarise(
    CombinedAllocation = sum(
      ProposedFundingAmount,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Summarize districts and charters
# -----------------------------------------------------------------------------

sector_summary <- lea_allocations |>
  mutate(
    `LEA type` = recode(
      LEAType,
      "District" = "Districts",
      "Charter" = "Charters"
    )
  ) |>
  group_by(`LEA type`) |>
  summarise(
    LEAs = n_distinct(DistrictCode),
    `Combined allocation` = sum(CombinedAllocation),
    `Median LEA allocation` = median(CombinedAllocation),
    .groups = "drop"
  ) |>
  mutate(
    `Share of combined pools` =
      `Combined allocation` /
      sum(`Combined allocation`)
  )

# Add the statewide row
all_leas <- lea_allocations |>
  summarise(
    `LEA type` = "All LEAs",
    LEAs = n_distinct(DistrictCode),
    `Combined allocation` = sum(CombinedAllocation),
    `Share of combined pools` = 1,
    `Median LEA allocation` = median(CombinedAllocation)
  )

exhibit11_raw <- bind_rows(
  sector_summary,
  all_leas
) |>
  mutate(
    `LEA type` = factor(
      `LEA type`,
      levels = c(
        "Districts",
        "Charters",
        "All LEAs"
      )
    )
  ) |>
  arrange(`LEA type`)

exhibit11_raw

