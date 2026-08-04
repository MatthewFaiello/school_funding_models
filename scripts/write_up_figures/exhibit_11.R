# =============================================================================
# exhibit_11.R
# =============================================================================
# Creates the updated Exhibit 11:
#   Proposed Opportunity and Operational Funding by sector and formula component
#
# Outputs:
#   - exhibit11_raw: numeric report table
#   - exhibit11_display: formatted report table
#   - exhibit11_context: values used in the accompanying Section 3.5 narrative
#   - exhibit11_reconciliation: validation against final Step 11 outputs
#
# Run from the school_funding_model project root after the main pipeline.
# =============================================================================

library(dplyr)
library(readr)
library(tibble)

source(file.path("scripts", "00_settings.R"))

# Paths ------------------------------------------------------------------------

proposed_detail_path <- file.path(
  intermediate_dir,
  "08_proposed_model_funding_detail.csv"
)

lea_comparison_path <- file.path(
  final_dir,
  "11_opportunity_operational_lea_comparison.csv"
)

statewide_comparison_path <- file.path(
  final_dir,
  "11_opportunity_operational_comparison.csv"
)

check_required_files(c(
  proposed_detail_path,
  lea_comparison_path,
  statewide_comparison_path
))

# Read inputs ------------------------------------------------------------------

proposed_detail <- read_csv(
  proposed_detail_path,
  show_col_types = FALSE
)

weighted_funding <- read_csv(
  lea_comparison_path,
  show_col_types = FALSE
)

weighted_statewide <- read_csv(
  statewide_comparison_path,
  show_col_types = FALSE
)

check_columns <- function(data, required, source_name) {
  missing <- setdiff(required, names(data))

  if (length(missing) > 0L) {
    stop(
      paste0(
        source_name,
        " is missing required columns: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

check_columns(
  proposed_detail,
  c(
    "DistrictCode",
    "DistrictName",
    "LEAType",
    "IncludeInStatewide",
    "FundingSection",
    "Component",
    "FundingAmount"
  ),
  "08_proposed_model_funding_detail.csv"
)

check_columns(
  weighted_funding,
  c(
    "AnalysisSection",
    "ReportingScope",
    "DistrictCode",
    "DistrictName",
    "LEAType",
    "FundingCategory",
    "CurrentFundingAmount",
    "ProposedFundingAmount",
    "ComparisonStatus"
  ),
  "11_opportunity_operational_lea_comparison.csv"
)

check_columns(
  weighted_statewide,
  c(
    "AnalysisSection",
    "ReportingScope",
    "FundingCategory",
    "ProposedFundingAmount"
  ),
  "11_opportunity_operational_comparison.csv"
)

# Maintained component order and display labels --------------------------------

component_lookup <- tribble(
  ~Component, ~`Funding stream`, ~`Formula component`, ~RowOrder,

  "Opportunity Funding - Low Income",
  "Opportunity",
  "Low income",
  1L,

  "Opportunity Funding - Multilingual Learner",
  "Opportunity",
  "Active multilingual learner",
  2L,

  "Operational Funding - Enrollment",
  "Operational",
  "Total enrollment",
  4L,

  "Operational Funding - Low Income",
  "Operational",
  "Low income",
  5L,

  "Operational Funding - Multilingual Learner",
  "Operational",
  "Active multilingual learner",
  6L,

  "Operational Funding - Basic Special Education",
  "Operational",
  "Basic special education",
  7L,

  "Operational Funding - Intensive Special Education",
  "Operational",
  "Intensive special education",
  8L,

  "Operational Funding - Complex Special Education",
  "Operational",
  "Complex special education",
  9L,

  "Operational Funding - Vocational",
  "Operational",
  "Vocational enrollment*",
  10L
)

expected_streams <- c(
  "Opportunity Funding",
  "Operational Funding"
)

# Final aligned reporting scope ------------------------------------------------

weighted_funding_scope <- weighted_funding |>
  filter(
    AnalysisSection == "Opportunity and Operational Funding",
    ReportingScope == primary_reporting_scope_short,
    FundingCategory %in% expected_streams
  )

weighted_statewide_scope <- weighted_statewide |>
  filter(
    AnalysisSection == "Opportunity and Operational Funding",
    ReportingScope == primary_reporting_scope_short,
    FundingCategory %in% expected_streams
  )

if (nrow(weighted_statewide_scope) != 2L) {
  stop(
    "Exhibit 11 expected exactly two statewide weighted-funding records.",
    call. = FALSE
  )
}

if (
  n_distinct(weighted_funding_scope$DistrictCode) != 43L ||
    n_distinct(
      weighted_funding_scope$DistrictCode[
        weighted_funding_scope$LEAType == "District"
      ]
    ) != 19L ||
    n_distinct(
      weighted_funding_scope$DistrictCode[
        weighted_funding_scope$LEAType == "Charter"
      ]
    ) != 24L
) {
  stop(
    "Exhibit 11 expected 43 LEAs: 19 districts and 24 charters.",
    call. = FALSE
  )
}

if (
  any(
    weighted_funding_scope$DistrictCode %in%
      primary_reporting_excluded_lea_codes
  )
) {
  stop(
    "An excluded LEA entered the Exhibit 11 reporting scope.",
    call. = FALSE
  )
}

if (
  !basse_district_code %in%
    weighted_funding_scope$DistrictCode
) {
  stop(
    "BASSE is missing from the Exhibit 11 reporting scope.",
    call. = FALSE
  )
}

if (
  !setequal(
    unique(weighted_funding_scope$FundingCategory),
    expected_streams
  )
) {
  stop(
    "Exhibit 11 expected exactly Opportunity and Operational Funding.",
    call. = FALSE
  )
}

if (
  any(!is.na(weighted_funding_scope$CurrentFundingAmount))
) {
  stop(
    paste(
      "Exhibit 11 expected current Opportunity and Operational",
      "analogues to remain unavailable."
    ),
    call. = FALSE
  )
}

if (
  any(
    weighted_funding_scope$ComparisonStatus !=
      "Not yet estimable"
  )
) {
  stop(
    paste(
      "Exhibit 11 expected both weighted-funding streams",
      "to remain not yet estimable for comparison."
    ),
    call. = FALSE
  )
}

# Keep the nine component-level proposed allocations ---------------------------

weighted_detail <- proposed_detail |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide = as.logical(IncludeInStatewide),
    FundingAmount = as.numeric(FundingAmount)
  ) |>
  filter(
    IncludeInStatewide,
    LEAType %in% c("District", "Charter"),
    FundingSection %in% c(
      "Opportunity Funding (State Support)",
      "Operational Funding (State Support)"
    ),
    Component %in% component_lookup$Component
  ) |>
  inner_join(
    component_lookup,
    by = "Component"
  )

if (
  !setequal(
    unique(weighted_detail$Component),
    component_lookup$Component
  )
) {
  stop(
    "One or more expected weighted-funding components are missing.",
    call. = FALSE
  )
}

if (
  !setequal(
    unique(weighted_detail$DistrictCode),
    unique(weighted_funding_scope$DistrictCode)
  )
) {
  stop(
    paste(
      "The component-level proposed detail and final LEA output",
      "do not contain the same LEA universe."
    ),
    call. = FALSE
  )
}

# Component allocations by sector ---------------------------------------------

component_summary <- weighted_detail |>
  summarise(
    Allocation = sum(FundingAmount, na.rm = TRUE),
    .by = c(
      RowOrder,
      `Funding stream`,
      `Formula component`,
      LEAType
    )
  ) |>
  tidyr::pivot_wider(
    names_from = LEAType,
    values_from = Allocation,
    values_fill = 0
  ) |>
  transmute(
    RowOrder,
    `Funding stream`,
    `Formula component`,
    `District allocation` = District,
    `Charter allocation` = Charter,
    `Statewide total` = District + Charter
  )

# Authoritative stream totals from final Step 11 outputs -----------------------

stream_sector_totals <- weighted_funding_scope |>
  mutate(
    `Funding stream` = recode(
      FundingCategory,
      "Opportunity Funding" = "Opportunity",
      "Operational Funding" = "Operational"
    )
  ) |>
  summarise(
    Allocation = sum(ProposedFundingAmount, na.rm = TRUE),
    .by = c(`Funding stream`, LEAType)
  ) |>
  tidyr::pivot_wider(
    names_from = LEAType,
    values_from = Allocation,
    values_fill = 0
  )

stream_statewide_totals <- weighted_statewide_scope |>
  transmute(
    `Funding stream` = recode(
      FundingCategory,
      "Opportunity Funding" = "Opportunity",
      "Operational Funding" = "Operational"
    ),
    `Statewide total` = ProposedFundingAmount
  )

stream_totals <- stream_sector_totals |>
  left_join(
    stream_statewide_totals,
    by = "Funding stream"
  ) |>
  mutate(
    RowOrder = case_when(
      `Funding stream` == "Opportunity" ~ 3L,
      `Funding stream` == "Operational" ~ 11L,
      TRUE ~ NA_integer_
    ),
    `Funding stream` = paste0(
      `Funding stream`,
      " total"
    ),
    `Formula component` = ""
  ) |>
  transmute(
    RowOrder,
    `Funding stream`,
    `Formula component`,
    `District allocation` = District,
    `Charter allocation` = Charter,
    `Statewide total`
  )

combined_total <- stream_totals |>
  summarise(
    RowOrder = 12L,
    `Funding stream` = "Combined total",
    `Formula component` = "",
    `District allocation` = sum(`District allocation`),
    `Charter allocation` = sum(`Charter allocation`),
    `Statewide total` = sum(`Statewide total`)
  )

# Final Exhibit 11 table --------------------------------------------------------

exhibit11_raw <- bind_rows(
  component_summary,
  stream_totals,
  combined_total
) |>
  arrange(RowOrder) |>
  select(
    `Funding stream`,
    `Formula component`,
    `District allocation`,
    `Charter allocation`,
    `Statewide total`
  )

if (
  nrow(exhibit11_raw) != 12L ||
    anyNA(exhibit11_raw)
) {
  stop(
    "Exhibit 11 expected 12 complete rows.",
    call. = FALSE
  )
}

# Reconciliation ---------------------------------------------------------------

detail_stream_sector <- weighted_detail |>
  summarise(
    Calculated = sum(FundingAmount, na.rm = TRUE),
    .by = c(`Funding stream`, LEAType)
  )

final_stream_sector <- weighted_funding_scope |>
  mutate(
    `Funding stream` = recode(
      FundingCategory,
      "Opportunity Funding" = "Opportunity",
      "Operational Funding" = "Operational"
    )
  ) |>
  summarise(
    Expected = sum(ProposedFundingAmount, na.rm = TRUE),
    .by = c(`Funding stream`, LEAType)
  )

sector_reconciliation <- detail_stream_sector |>
  full_join(
    final_stream_sector,
    by = c("Funding stream", "LEAType")
  ) |>
  mutate(
    Check = paste(
      `Funding stream`,
      LEAType,
      "allocation"
    ),
    Difference = Calculated - Expected
  ) |>
  select(
    Check,
    Calculated,
    Expected,
    Difference
  )

detail_stream_statewide <- weighted_detail |>
  summarise(
    Calculated = sum(FundingAmount, na.rm = TRUE),
    .by = `Funding stream`
  )

final_stream_statewide <- weighted_statewide_scope |>
  transmute(
    `Funding stream` = recode(
      FundingCategory,
      "Opportunity Funding" = "Opportunity",
      "Operational Funding" = "Operational"
    ),
    Expected = ProposedFundingAmount
  )

statewide_reconciliation <- detail_stream_statewide |>
  full_join(
    final_stream_statewide,
    by = "Funding stream"
  ) |>
  mutate(
    Check = paste(
      `Funding stream`,
      "statewide allocation"
    ),
    Difference = Calculated - Expected
  ) |>
  select(
    Check,
    Calculated,
    Expected,
    Difference
  )

exhibit11_reconciliation <- bind_rows(
  sector_reconciliation,
  statewide_reconciliation
) |>
  mutate(
    Passed = abs(Difference) < 0.05
  )

if (!all(exhibit11_reconciliation$Passed)) {
  print(exhibit11_reconciliation)

  stop(
    "Exhibit 11 component allocations do not reconcile to Step 11 outputs.",
    call. = FALSE
  )
}

opportunity_total <- weighted_statewide_scope |>
  filter(FundingCategory == "Opportunity Funding") |>
  pull(ProposedFundingAmount)

operational_total <- weighted_statewide_scope |>
  filter(FundingCategory == "Operational Funding") |>
  pull(ProposedFundingAmount)

combined_pool <- opportunity_total + operational_total

stopifnot(
  abs(opportunity_total - 163000000) < 0.05,
  abs(operational_total - 279026800) < 0.05,
  abs(combined_pool - 442026800) < 0.05,
  abs(
    tail(exhibit11_raw$`Statewide total`, 1) -
      combined_pool
  ) < 0.05
)

# Narrative context ------------------------------------------------------------

lea_combined_allocations <- weighted_funding_scope |>
  summarise(
    CombinedAllocation = sum(
      ProposedFundingAmount,
      na.rm = TRUE
    ),
    .by = c(
      DistrictCode,
      DistrictName,
      LEAType
    )
  )

component_statewide <- component_summary |>
  select(
    `Funding stream`,
    `Formula component`,
    `Statewide total`
  )

district_combined <- sum(
  lea_combined_allocations$CombinedAllocation[
    lea_combined_allocations$LEAType == "District"
  ]
)

charter_combined <- sum(
  lea_combined_allocations$CombinedAllocation[
    lea_combined_allocations$LEAType == "Charter"
  ]
)

low_income_total <- component_statewide |>
  filter(`Formula component` == "Low income") |>
  summarise(Value = sum(`Statewide total`)) |>
  pull(Value)

multilingual_total <- component_statewide |>
  filter(
    `Formula component` ==
      "Active multilingual learner"
  ) |>
  summarise(Value = sum(`Statewide total`)) |>
  pull(Value)

special_education_total <- component_statewide |>
  filter(
    `Formula component` %in% c(
      "Basic special education",
      "Intensive special education",
      "Complex special education"
    )
  ) |>
  summarise(Value = sum(`Statewide total`)) |>
  pull(Value)

operational_enrollment_total <- component_statewide |>
  filter(`Formula component` == "Total enrollment") |>
  pull(`Statewide total`)

vocational_total <- component_statewide |>
  filter(
    `Formula component` ==
      "Vocational enrollment*"
  ) |>
  pull(`Statewide total`)

exhibit11_context <- tibble(
  Metric = c(
    "Combined statewide pool",
    "District combined allocation",
    "District share of combined pools",
    "Charter combined allocation",
    "Charter share of combined pools",
    "District median LEA allocation",
    "Charter median LEA allocation",
    "Combined low-income components",
    "Low-income share of combined pools",
    "Operational total-enrollment component",
    "Combined multilingual-learner components",
    "Combined Operational special-education components",
    "Operational vocational-enrollment component",
    "Current comparison status"
  ),
  Value = c(
    combined_pool,
    district_combined,
    district_combined / combined_pool,
    charter_combined,
    charter_combined / combined_pool,
    median(
      lea_combined_allocations$CombinedAllocation[
        lea_combined_allocations$LEAType == "District"
      ]
    ),
    median(
      lea_combined_allocations$CombinedAllocation[
        lea_combined_allocations$LEAType == "Charter"
      ]
    ),
    low_income_total,
    low_income_total / combined_pool,
    operational_enrollment_total,
    multilingual_total,
    special_education_total,
    vocational_total,
    NA_real_
  ),
  Note = c(
    rep("", 13),
    paste(
      "Pending confirmed current Opportunity and Operational",
      "analogues and LEA allocations from OMB/CGO"
    )
  )
)

# Formatted report table --------------------------------------------------------

format_millions <- function(x, digits = 1L) {
  paste0(
    "$",
    format(
      round(x / 1e6, digits),
      nsmall = digits,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    "M"
  )
}

exhibit11_display <- exhibit11_raw |>
  mutate(
    `District allocation` = format_millions(
      `District allocation`,
      1L
    ),
    `Charter allocation` = format_millions(
      `Charter allocation`,
      1L
    ),
    `Statewide total` = case_when(
      `Funding stream` == "Opportunity total" ~ "$163.0M",
      `Funding stream` == "Operational total" ~ "$279.0268M",
      `Funding stream` == "Combined total" ~ "$442.0268M",
      TRUE ~ format_millions(`Statewide total`, 1L)
    )
  )

message(
  paste0(
    "Exhibit 11 validated: nine formula components across 43 LEAs, ",
    "including BASSE and excluding DAFB. District, charter, and statewide ",
    "allocations reconcile to $",
    format(
      combined_pool,
      big.mark = ",",
      scientific = FALSE
    ),
    "."
  )
)

# Review objects ---------------------------------------------------------------

exhibit11_display
exhibit11_context
exhibit11_reconciliation
