# =============================================================================
# EXHIBIT 16: SELECTED STAFFING-RULE DIFFERENCES
#
# Creates:
#   exhibit16_raw
#     Exact PEFC and IV&V quantities, rates, and funding differences.
#
#   exhibit16
#     Report-formatted Exhibit 16.
#
#   instructional_support_lea_comparison
#     Direct PEFC-versus-IV&V comparison for every LEA in the workbook scope.
#
#   instructional_support_charter_building_comparison
#     Direct PEFC-versus-IV&V comparison for all 48 charter building
#     calculation units.
#
#   instructional_support_affected_buildings
#     The 10 charter building rows where the PEFC Total Units base omits the
#     net vocational positions included in the IV&V Base Division I total.
#
#   instructional_support_affected_leas
#     Direct comparison for the seven affected charter LEAs.
#
#   instructional_support_reconciliation
#     Statewide bridge from the vocational-base effect to the final
#     3.430267-position IV&V-minus-PEFC difference.
#
# Compatibility aliases:
#   instructional_support_pinpoint
#   instructional_support_summary
#
# Difference convention:
#   IV&V minus PEFC
#   Positive = IV&V is higher
#   Negative = IV&V is lower
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)

# -----------------------------------------------------------------------------
# 1. Helper functions
# -----------------------------------------------------------------------------

normalize_key <- function(x) {
  x |>
    iconv(
      from = "",
      to = "ASCII//TRANSLIT"
    ) |>
    str_to_lower() |>
    str_replace_all(
      "[^a-z0-9]",
      ""
    )
}

to_logical <- function(x) {
  tolower(
    as.character(x)
  ) == "true"
}

format_signed_positions <- function(x) {
  sprintf(
    "%+.2f",
    x
  )
}

format_signed_millions <- function(x) {
  paste0(
    ifelse(
      x >= 0,
      "+$",
      "-$"
    ),
    sprintf(
      "%.2fM",
      abs(x) / 1e6
    )
  )
}

stop_if_missing_columns <- function(
    data,
    required_columns,
    source_name
) {
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        source_name,
        "is missing required columns:",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
}

# -----------------------------------------------------------------------------
# 2. File paths
# -----------------------------------------------------------------------------

component_comparison_path <- file.path(
  "data",
  "output",
  "audit",
  "10_pefc_component_comparison.csv"
)

proposed_quantities_path <- file.path(
  "data",
  "output",
  "intermediate",
  "07_proposed_model_quantities.csv"
)

calculator_path <- file.path(
  "data",
  "input",
  "Copy of Calculator for 25-26 w Charter (003).xlsm"
)

lea_crosswalk_path <- file.path(
  "data",
  "input",
  "lea_crosswalk.csv"
)

# -----------------------------------------------------------------------------
# 3. Read source files
# -----------------------------------------------------------------------------

pefc_component_comparison <- read_csv(
  component_comparison_path,
  show_col_types = FALSE
)

proposed_quantities <- read_csv(
  proposed_quantities_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode =
      as.integer(DistrictCode),
    
    IncludeInStatewide =
      to_logical(IncludeInStatewide),
    
    IsSchoolCalculationUnit =
      to_logical(IsSchoolCalculationUnit),
    
    CalculationUnitSequence =
      as.integer(CalculationUnitSequence),
    
    RawInputValue =
      as.numeric(RawInputValue),
    
    FundingQuantity =
      as.numeric(FundingQuantity)
  )

lea_crosswalk <- read_csv(
  lea_crosswalk_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode =
      as.integer(DistrictCode),
    
    IncludeInStatewide =
      to_logical(IncludeInStatewide),
    
    CalculatorKey =
      normalize_key(CalculatorLEAName)
  )

calculator_summary <- read_excel(
  calculator_path,
  sheet = "Summary"
)

calculator_data <- read_excel(
  calculator_path,
  sheet = "Data"
) |>
  mutate(
    WorkbookRow =
      row_number()
  )

# -----------------------------------------------------------------------------
# 4. Validate required source columns
# -----------------------------------------------------------------------------

stop_if_missing_columns(
  pefc_component_comparison,
  c(
    "ComparisonType",
    "ReportingScope",
    "Component",
    "PEFCQuantity",
    "IndependentQuantity",
    "QuantityDifference",
    "PEFCRate",
    "IndependentRate",
    "PEFCFundingAmount",
    "IndependentFundingAmount",
    "FundingDifference"
  ),
  "10_pefc_component_comparison.csv"
)

stop_if_missing_columns(
  proposed_quantities,
  c(
    "DistrictCode",
    "DistrictName",
    "LEAType",
    "IncludeInStatewide",
    "CalculationLevel",
    "IsSchoolCalculationUnit",
    "CalculationUnitName",
    "CalculationUnitSequence",
    "FundingSection",
    "Component",
    "RawInputValue",
    "FundingQuantity"
  ),
  "07_proposed_model_quantities.csv"
)

stop_if_missing_columns(
  lea_crosswalk,
  c(
    "DistrictCode",
    "DistrictName",
    "LEAType",
    "IncludeInStatewide",
    "CalculatorLEAName"
  ),
  "lea_crosswalk.csv"
)

stop_if_missing_columns(
  calculator_summary,
  c(
    "District",
    "Child",
    "Type",
    "D16"
  ),
  "PEFC Summary sheet"
)

stop_if_missing_columns(
  calculator_data,
  c(
    "District/Charter",
    "School/District",
    "Total Units",
    "Vocational Deduct",
    "Vocational Division I",
    "Instructional Supports"
  ),
  "PEFC Data sheet"
)

# =============================================================================
# EXHIBIT 16
# =============================================================================

# -----------------------------------------------------------------------------
# 5. Pull the three selected components
# -----------------------------------------------------------------------------

selected_components <- c(
  "Assistant Principal",
  "Assistant Superintendent",
  "Instructional Supports"
)

exhibit16_raw <- pefc_component_comparison |>
  filter(
    ComparisonType ==
      "Component formula/input comparison",
    
    Component %in%
      selected_components
  ) |>
  mutate(
    Area = recode(
      Component,
      
      "Assistant Principal" =
        "Assistant Principals",
      
      "Assistant Superintendent" =
        "Assistant Superintendents",
      
      "Instructional Supports" =
        "Instructional Supports"
    ),
    
    Area = factor(
      Area,
      levels = c(
        "Assistant Principals",
        "Assistant Superintendents",
        "Instructional Supports"
      )
    )
  ) |>
  arrange(
    Area
  ) |>
  transmute(
    Area,
    ReportingScope,
    
    `PEFC positions` =
      PEFCQuantity,
    
    `IV&V positions` =
      IndependentQuantity,
    
    # Rounded value stored in the component comparison output
    `Reported position difference` =
      QuantityDifference,
    
    # Exact difference calculated from the underlying quantities
    `Exact position difference` =
      IndependentQuantity -
      PEFCQuantity,
    
    `PEFC rate` =
      PEFCRate,
    
    `IV&V rate` =
      IndependentRate,
    
    `Rate difference` =
      IndependentRate -
      PEFCRate,
    
    `PEFC funding` =
      PEFCFundingAmount,
    
    `IV&V funding` =
      IndependentFundingAmount,
    
    `Reported funding difference` =
      FundingDifference,
    
    `Exact funding difference` =
      IndependentFundingAmount -
      PEFCFundingAmount
  )

# -----------------------------------------------------------------------------
# 6. Create the report-formatted Exhibit 16
# -----------------------------------------------------------------------------

exhibit16 <- exhibit16_raw |>
  mutate(
    `Observed difference` = paste0(
      "IV&V ",
      format_signed_positions(
        `Exact position difference`
      ),
      " positions; ",
      format_signed_millions(
        `Exact funding difference`
      )
    ),
    
    `Practical explanation` = case_when(
      Area == "Assistant Principals" ~
        paste(
          "The IV&V retains the written 0.65 and 1.65 fractional",
          "awards, while the workbook formula rounds those awards down."
        ),
      
      Area == "Assistant Superintendents" ~
        paste(
          "The IV&V applies completed 300-position thresholds, capped",
          "at two positions, while the workbook produces a proportional",
          "second position."
        ),
      
      Area == "Instructional Supports" ~
        paste(
          "Both implementations use a 20% factor. However, for seven",
          "multi-building charters, the workbook's building-level Total",
          "Units exclude net vocational positions included in the IV&V",
          "Base Division I position total."
        ),
      
      TRUE ~
        NA_character_
    )
  ) |>
  select(
    Area,
    `Observed difference`,
    `Practical explanation`
  )

# -----------------------------------------------------------------------------
# 7. Pull rows used for validation
# -----------------------------------------------------------------------------

assistant_principal_check <- exhibit16_raw |>
  filter(
    Area == "Assistant Principals"
  )

assistant_superintendent_check <- exhibit16_raw |>
  filter(
    Area == "Assistant Superintendents"
  )

instructional_supports_check <- exhibit16_raw |>
  filter(
    Area == "Instructional Supports"
  )

instructional_support_rate <-
  instructional_supports_check$
  `PEFC rate`

stopifnot(
  length(instructional_support_rate) == 1L,
  instructional_support_rate > 0
)

# -----------------------------------------------------------------------------
# 8. Validate Exhibit 16
# -----------------------------------------------------------------------------

stopifnot(
  nrow(exhibit16_raw) == 3,
  
  # PEFC and IV&V use the same component rates
  all(
    near(
      exhibit16_raw$`PEFC rate`,
      exhibit16_raw$`IV&V rate`,
      tol = 0.000001
    )
  ),
  
  all(
    near(
      exhibit16_raw$`Rate difference`,
      0,
      tol = 0.000001
    )
  ),
  
  # Rounded source values reconcile to exact calculated differences
  all(
    near(
      exhibit16_raw$
        `Reported position difference`,
      
      round(
        exhibit16_raw$
          `Exact position difference`,
        2
      ),
      
      tol = 0.000001
    )
  ),
  
  all(
    near(
      exhibit16_raw$
        `Reported funding difference`,
      
      exhibit16_raw$
        `Exact funding difference`,
      
      tol = 0.01
    )
  ),
  
  # Assistant Principals
  near(
    assistant_principal_check$
      `Exact position difference`,
    18.25,
    tol = 0.000001
  ),
  
  near(
    assistant_principal_check$
      `Exact funding difference`,
    2450354.50,
    tol = 0.01
  ),
  
  # Assistant Superintendents
  near(
    assistant_superintendent_check$
      `Exact position difference`,
    -1.97,
    tol = 0.000001
  ),
  
  near(
    assistant_superintendent_check$
      `Exact funding difference`,
    -330680.26,
    tol = 0.01
  ),
  
  # Instructional Supports
  near(
    instructional_supports_check$
      `Exact position difference`,
    3.430267,
    tol = 0.000001
  ),
  
  near(
    instructional_supports_check$
      `Reported position difference`,
    3.43,
    tol = 0.000001
  ),
  
  near(
    instructional_supports_check$
      `Exact funding difference`,
    329837.30,
    tol = 0.01
  )
)

message("Exhibit 16 values validated.")

# =============================================================================
# INSTRUCTIONAL SUPPORTS: DIRECT LEA COMPARISON
# =============================================================================

# -----------------------------------------------------------------------------
# 9. Identify one PEFC Summary row per LEA
#
# District totals use the Central Office row.
# Charter totals use the row whose Child value matches the charter name.
# DAFB uses its workbook-specific total row.
# -----------------------------------------------------------------------------

pefc_summary_lea_rows <- calculator_summary |>
  filter(
    !is.na(
      .data[["District"]]
    ),
    
    !.data[["District"]] %in%
      c(
        "CHECK:",
        "Statewide"
      )
  ) |>
  mutate(
    CalculatorLEAName =
      as.character(
        .data[["District"]]
      ),
    
    CalculatorTotalRow =
      as.character(
        .data[["Child"]]
      ),
    
    CalculatorKey =
      normalize_key(
        CalculatorLEAName
      ),
    
    IsLEATotalRow =
      coalesce(
        .data[["Type"]] ==
          "Central Office",
        FALSE
      ) |
      coalesce(
        normalize_key(
          CalculatorTotalRow
        ) ==
          CalculatorKey,
        FALSE
      ) |
      (
        CalculatorLEAName ==
          "DAFB" &
          CalculatorTotalRow ==
          "Dover Air Force Base"
      )
  ) |>
  filter(
    IsLEATotalRow
  ) |>
  left_join(
    lea_crosswalk |>
      select(
        DistrictCode,
        DistrictName,
        LEAType,
        IncludeInStatewide,
        CalculatorKey
      ),
    by = "CalculatorKey"
  )

unmatched_summary_rows <- pefc_summary_lea_rows |>
  filter(
    is.na(DistrictCode)
  )

if (nrow(unmatched_summary_rows) > 0) {
  print(
    unmatched_summary_rows |>
      select(
        CalculatorLEAName,
        CalculatorTotalRow
      )
  )
  
  stop(
    paste(
      "One or more PEFC Summary LEA rows did not match",
      "lea_crosswalk.csv."
    ),
    call. = FALSE
  )
}

duplicate_summary_leas <- pefc_summary_lea_rows |>
  count(
    DistrictCode,
    DistrictName
  ) |>
  filter(
    n != 1
  )

if (nrow(duplicate_summary_leas) > 0) {
  print(duplicate_summary_leas)
  
  stop(
    paste(
      "The PEFC Summary extraction did not produce exactly",
      "one row for every LEA."
    ),
    call. = FALSE
  )
}

pefc_instructional_support_by_lea <-
  pefc_summary_lea_rows |>
  transmute(
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    
    `PEFC Instructional Supports` =
      as.numeric(
        .data[["D16"]]
      )
  )

# -----------------------------------------------------------------------------
# 10. Sum IV&V Instructional Supports by LEA
# -----------------------------------------------------------------------------

ivv_instructional_support_by_lea <-
  proposed_quantities |>
  filter(
    FundingSection ==
      "Base Funding (State Support)",
    
    Component ==
      "Instructional Supports"
  ) |>
  summarise(
    `IV&V Base Division I positions` =
      sum(
        RawInputValue,
        na.rm = TRUE
      ),
    
    `IV&V Instructional Supports` =
      sum(
        FundingQuantity,
        na.rm = TRUE
      ),
    
    .by = c(
      DistrictCode,
      DistrictName,
      LEAType
    )
  )

# -----------------------------------------------------------------------------
# 11. Direct PEFC-versus-IV&V comparison by LEA
# -----------------------------------------------------------------------------

instructional_support_lea_comparison <-
  pefc_instructional_support_by_lea |>
  left_join(
    ivv_instructional_support_by_lea,
    by = c(
      "DistrictCode",
      "DistrictName",
      "LEAType"
    )
  ) |>
  mutate(
    ReportingScope = if_else(
      IncludeInStatewide,
      "Primary 43-LEA scope",
      "Outside primary scope"
    ),
    
    `Instructional Support difference` =
      `IV&V Instructional Supports` -
      `PEFC Instructional Supports`,
    
    `Funding difference` =
      `Instructional Support difference` *
      instructional_support_rate
  ) |>
  arrange(
    desc(
      abs(
        `Instructional Support difference`
      )
    )
  )

missing_ivv_leas <-
  instructional_support_lea_comparison |>
  filter(
    is.na(
      `IV&V Instructional Supports`
    )
  )

if (nrow(missing_ivv_leas) > 0) {
  print(
    missing_ivv_leas |>
      select(
        DistrictCode,
        DistrictName,
        LEAType,
        ReportingScope
      )
  )
  
  stop(
    paste(
      "One or more PEFC workbook-scope LEAs do not have",
      "corresponding IV&V Instructional Supports rows."
    ),
    call. = FALSE
  )
}

# =============================================================================
# INSTRUCTIONAL SUPPORTS: DIRECT CHARTER-BUILDING COMPARISON
# =============================================================================

# -----------------------------------------------------------------------------
# 12. Create the maintained lookup for the 24 primary-scope charter LEAs
# -----------------------------------------------------------------------------

charter_lookup <- lea_crosswalk |>
  filter(
    LEAType == "Charter",
    IncludeInStatewide
  ) |>
  transmute(
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide,
    
    CrosswalkCalculatorLEAName =
      CalculatorLEAName,
    
    CalculatorKey
  ) |>
  distinct()

stopifnot(
  nrow(charter_lookup) == 24,
  n_distinct(charter_lookup$DistrictCode) == 24,
  n_distinct(charter_lookup$CalculatorKey) == 24
)

# -----------------------------------------------------------------------------
# 13. Keep only Data-sheet rows belonging to the 24 in-scope charters
#
# The workbook Data sheet contains rows outside the maintained charter
# calculation universe. Restricting the join to the charter lookup avoids
# treating unrelated workbook rows as unmatched LEAs.
# -----------------------------------------------------------------------------

workbook_charter_rows <- calculator_data |>
  filter(
    !is.na(
      .data[["District/Charter"]]
    ),
    
    !is.na(
      .data[["School/District"]]
    )
  ) |>
  mutate(
    WorkbookCalculatorLEAName =
      as.character(
        .data[["District/Charter"]]
      ),
    
    CalculatorKey =
      normalize_key(
        WorkbookCalculatorLEAName
      ),
    
    WorkbookCalculationUnitName =
      as.character(
        .data[["School/District"]]
      )
  ) |>
  inner_join(
    charter_lookup,
    by = "CalculatorKey"
  ) |>
  group_by(
    DistrictCode,
    DistrictName
  ) |>
  arrange(
    WorkbookRow,
    .by_group = TRUE
  ) |>
  mutate(
    NameMatchesOrganization =
      normalize_key(
        WorkbookCalculationUnitName
      ) ==
      CalculatorKey,
    
    # Use the explicitly named organization row when present.
    # Otherwise, use the final row in a multi-row charter block.
    IsOrganizationTotal = case_when(
      any(NameMatchesOrganization) ~
        NameMatchesOrganization,
      
      n() > 1 ~
        row_number() == n(),
      
      TRUE ~
        FALSE
    )
  ) |>
  ungroup()

# Confirm that all 24 maintained charter LEAs were found
charter_data_coverage <- charter_lookup |>
  select(
    DistrictCode,
    DistrictName,
    CalculatorKey
  ) |>
  left_join(
    workbook_charter_rows |>
      distinct(
        DistrictCode,
        CalculatorKey
      ) |>
      mutate(
        FoundInWorkbook = TRUE
      ),
    by = c(
      "DistrictCode",
      "CalculatorKey"
    )
  ) |>
  mutate(
    FoundInWorkbook =
      coalesce(
        FoundInWorkbook,
        FALSE
      )
  )

missing_charters <- charter_data_coverage |>
  filter(
    !FoundInWorkbook
  )

if (nrow(missing_charters) > 0) {
  print(missing_charters)
  
  stop(
    paste(
      "One or more maintained charter LEAs were not found",
      "on the PEFC Data sheet."
    ),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 14. Identify PEFC charter building calculation units
#
# Multi-row charters:
#   Exclude the separate charter organization-total row.
#
# Single-row charters:
#   The one row serves as the building calculation unit.
# -----------------------------------------------------------------------------

pefc_charter_buildings <-
  workbook_charter_rows |>
  group_by(
    DistrictCode,
    DistrictName
  ) |>
  arrange(
    WorkbookRow,
    .by_group = TRUE
  ) |>
  mutate(
    WorkbookLEARowCount =
      n(),
    
    HasSeparateOrganizationTotal =
      any(IsOrganizationTotal) &
      WorkbookLEARowCount > 1,
    
    IsBuildingCalculationUnit =
      if_else(
        HasSeparateOrganizationTotal,
        !IsOrganizationTotal,
        TRUE
      )
  ) |>
  filter(
    IsBuildingCalculationUnit
  ) |>
  mutate(
    CalculationUnitSequence =
      row_number()
  ) |>
  ungroup() |>
  transmute(
    DistrictCode,
    DistrictName,
    CalculationUnitSequence,
    WorkbookRow,
    
    `PEFC building` =
      WorkbookCalculationUnitName,
    
    `PEFC Base Division I positions` =
      as.numeric(
        .data[["Total Units"]]
      ),
    
    `PEFC Instructional Supports` =
      as.numeric(
        .data[["Instructional Supports"]]
      ),
    
    `PEFC Vocational Division I` =
      coalesce(
        as.numeric(
          .data[["Vocational Division I"]]
        ),
        0
      ),
    
    `PEFC Vocational deduction` =
      coalesce(
        as.numeric(
          .data[["Vocational Deduct"]]
        ),
        0
      )
  ) |>
  mutate(
    `PEFC net vocational positions` =
      `PEFC Vocational Division I` +
      `PEFC Vocational deduction`
  )

stopifnot(
  nrow(pefc_charter_buildings) == 48,
  n_distinct(pefc_charter_buildings$DistrictCode) == 24
)

# -----------------------------------------------------------------------------
# 15. Pull the corresponding IV&V charter building calculations
# -----------------------------------------------------------------------------

ivv_charter_buildings <-
  proposed_quantities |>
  filter(
    LEAType == "Charter",
    IncludeInStatewide,
    CalculationLevel == "School",
    IsSchoolCalculationUnit,
    
    FundingSection ==
      "Base Funding (State Support)",
    
    Component ==
      "Instructional Supports"
  ) |>
  transmute(
    DistrictCode,
    DistrictName,
    CalculationUnitSequence,
    
    `IV&V building` =
      CalculationUnitName,
    
    `IV&V Base Division I positions` =
      RawInputValue,
    
    `IV&V Instructional Supports` =
      FundingQuantity
  )

stopifnot(
  nrow(ivv_charter_buildings) == 48,
  n_distinct(ivv_charter_buildings$DistrictCode) == 24
)

# -----------------------------------------------------------------------------
# 16. Direct comparison of all 48 charter building calculation units
# -----------------------------------------------------------------------------

instructional_support_charter_building_comparison <-
  pefc_charter_buildings |>
  full_join(
    ivv_charter_buildings,
    by = c(
      "DistrictCode",
      "DistrictName",
      "CalculationUnitSequence"
    )
  ) |>
  mutate(
    BuildingNameMatch =
      normalize_key(
        `PEFC building`
      ) ==
      normalize_key(
        `IV&V building`
      ),
    
    `Base position difference` =
      `IV&V Base Division I positions` -
      `PEFC Base Division I positions`,
    
    `Instructional Support difference` =
      `IV&V Instructional Supports` -
      `PEFC Instructional Supports`,
    
    `Expected difference from Base position difference` =
      `Base position difference` *
      0.20,
    
    `Residual position difference` =
      `Instructional Support difference` -
      `Expected difference from Base position difference`,
    
    `Expected vocational effect` =
      `PEFC net vocational positions` *
      0.20,
    
    `Funding difference` =
      `Instructional Support difference` *
      instructional_support_rate,
    
    VocationalBaseOmittedFromPEFC =
      `PEFC net vocational positions` >
      0.005 &
      near(
        `Base position difference`,
        `PEFC net vocational positions`,
        tol = 0.02
      ),
    
    DifferenceExplanation = case_when(
      VocationalBaseOmittedFromPEFC ~
        paste(
          "The PEFC building Total Units omit the net vocational",
          "positions included in the IV&V Base Division I total."
        ),
      
      abs(
        `Instructional Support difference`
      ) < 0.01 ~
        paste(
          "Minor input, aggregation, or calculation-precision",
          "difference."
        ),
      
      TRUE ~
        paste(
          "Other difference in the constructed Base Division I",
          "position total."
        )
    )
  ) |>
  arrange(
    desc(
      VocationalBaseOmittedFromPEFC
    ),
    DistrictName,
    CalculationUnitSequence
  )

# Confirm that every PEFC row matched an IV&V row and vice versa
unmatched_charter_buildings <-
  instructional_support_charter_building_comparison |>
  filter(
    is.na(`PEFC building`) |
      is.na(`IV&V building`)
  )

if (nrow(unmatched_charter_buildings) > 0) {
  print(unmatched_charter_buildings)
  
  stop(
    paste(
      "One or more charter calculation units did not match",
      "between PEFC and IV&V."
    ),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 17. Pull the 10 building rows with the vocational-base issue
# -----------------------------------------------------------------------------

instructional_support_affected_buildings <-
  instructional_support_charter_building_comparison |>
  filter(
    VocationalBaseOmittedFromPEFC
  ) |>
  select(
    DistrictCode,
    DistrictName,
    CalculationUnitSequence,
    
    `PEFC building`,
    `IV&V building`,
    BuildingNameMatch,
    
    `PEFC Base Division I positions`,
    `IV&V Base Division I positions`,
    `Base position difference`,
    
    `PEFC Vocational Division I`,
    `PEFC Vocational deduction`,
    `PEFC net vocational positions`,
    
    `PEFC Instructional Supports`,
    `IV&V Instructional Supports`,
    `Instructional Support difference`,
    
    `Expected vocational effect`,
    `Residual position difference`,
    `Funding difference`,
    DifferenceExplanation
  ) |>
  arrange(
    DistrictName,
    CalculationUnitSequence
  )

# =============================================================================
# DIRECT COMPARISON FOR THE SEVEN AFFECTED CHARTER LEAS
# =============================================================================

# -----------------------------------------------------------------------------
# 18. Summarize the affected building rows by charter LEA
# -----------------------------------------------------------------------------

affected_building_summary <-
  instructional_support_affected_buildings |>
  summarise(
    `Affected building rows` =
      n(),
    
    `Net vocational base omitted by PEFC` =
      sum(
        `PEFC net vocational positions`
      ),
    
    `Expected position effect at 20%` =
      sum(
        `Expected vocational effect`
      ),
    
    `Actual affected-building position difference` =
      sum(
        `Instructional Support difference`
      ),
    
    .by = c(
      DistrictCode,
      DistrictName
    )
  )

# -----------------------------------------------------------------------------
# 19. Add full LEA totals for the seven affected charters
# -----------------------------------------------------------------------------

instructional_support_affected_leas <-
  instructional_support_lea_comparison |>
  inner_join(
    affected_building_summary,
    by = c(
      "DistrictCode",
      "DistrictName"
    )
  ) |>
  mutate(
    `Other input, aggregation, and precision offset` =
      `Instructional Support difference` -
      `Expected position effect at 20%`
  ) |>
  select(
    DistrictCode,
    DistrictName,
    `Affected building rows`,
    
    `PEFC Instructional Supports`,
    `IV&V Instructional Supports`,
    `Instructional Support difference`,
    
    `Net vocational base omitted by PEFC`,
    `Expected position effect at 20%`,
    `Actual affected-building position difference`,
    `Other input, aggregation, and precision offset`,
    
    `Funding difference`
  ) |>
  arrange(
    desc(
      `Instructional Support difference`
    )
  )

# =============================================================================
# STATEWIDE RECONCILIATION
# =============================================================================

# -----------------------------------------------------------------------------
# 20. Reconcile the vocational-base effect to Exhibit 16
# -----------------------------------------------------------------------------

instructional_support_reconciliation <- tibble(
  `PEFC statewide positions` =
    sum(
      instructional_support_lea_comparison$
        `PEFC Instructional Supports`
    ),
  
  `IV&V statewide positions` =
    sum(
      instructional_support_lea_comparison$
        `IV&V Instructional Supports`
    ),
  
  `Statewide IV&V minus PEFC positions` =
    sum(
      instructional_support_lea_comparison$
        `Instructional Support difference`
    ),
  
  `Affected charter LEAs` =
    nrow(
      instructional_support_affected_leas
    ),
  
  `Affected charter building rows` =
    nrow(
      instructional_support_affected_buildings
    ),
  
  `Net vocational base omitted by PEFC` =
    sum(
      instructional_support_affected_buildings$
        `PEFC net vocational positions`
    ),
  
  `Expected position effect at 20%` =
    sum(
      instructional_support_affected_buildings$
        `Expected vocational effect`
    ),
  
  `Actual difference for affected LEAs` =
    sum(
      instructional_support_affected_leas$
        `Instructional Support difference`
    ),
  
  `Offset within affected LEAs` =
    sum(
      instructional_support_affected_leas$
        `Other input, aggregation, and precision offset`
    ),
  
  `Difference outside affected LEAs` =
    sum(
      instructional_support_lea_comparison$
        `Instructional Support difference`
    ) -
    sum(
      instructional_support_affected_leas$
        `Instructional Support difference`
    ),
  
  `Statewide funding difference` =
    sum(
      instructional_support_lea_comparison$
        `Funding difference`
    )
) |>
  mutate(
    `Reconstructed statewide difference` =
      `Expected position effect at 20%` +
      `Offset within affected LEAs` +
      `Difference outside affected LEAs`,
    
    `Reconciliation difference` =
      `Statewide IV&V minus PEFC positions` -
      `Reconstructed statewide difference`
  )

# Compatibility aliases for the earlier object names
instructional_support_pinpoint <-
  instructional_support_affected_leas

instructional_support_summary <-
  instructional_support_reconciliation

# =============================================================================
# VALIDATION
# =============================================================================

# -----------------------------------------------------------------------------
# 21. Validate the LEA and charter-building comparisons
# -----------------------------------------------------------------------------

stopifnot(
  # Workbook scope contains 43 primary-scope LEAs plus DAFB
  nrow(
    instructional_support_lea_comparison
  ) == 44,
  
  sum(
    instructional_support_lea_comparison$
      IncludeInStatewide
  ) == 43,
  
  # Statewide values reconcile to Exhibit 16
  near(
    sum(
      instructional_support_lea_comparison$
        `PEFC Instructional Supports`
    ),
    instructional_supports_check$
      `PEFC positions`,
    tol = 0.000001
  ),
  
  near(
    sum(
      instructional_support_lea_comparison$
        `IV&V Instructional Supports`
    ),
    instructional_supports_check$
      `IV&V positions`,
    tol = 0.00001
  ),
  
  near(
    sum(
      instructional_support_lea_comparison$
        `Instructional Support difference`
    ),
    instructional_supports_check$
      `Exact position difference`,
    tol = 0.00001
  ),
  
  near(
    sum(
      instructional_support_lea_comparison$
        `Funding difference`
    ),
    instructional_supports_check$
      `Exact funding difference`,
    tol = 0.01
  ),
  
  # All 48 charter calculation units matched
  nrow(
    instructional_support_charter_building_comparison
  ) == 48,
  
  all(
    !is.na(
      instructional_support_charter_building_comparison$
        `PEFC building`
    )
  ),
  
  all(
    !is.na(
      instructional_support_charter_building_comparison$
        `IV&V building`
    )
  ),
  
  # Both implementations apply 20% to their respective bases
  all(
    near(
      instructional_support_charter_building_comparison$
        `PEFC Instructional Supports`,
      
      instructional_support_charter_building_comparison$
        `PEFC Base Division I positions` *
        0.20,
      
      tol = 0.01
    )
  ),
  
  all(
    near(
      instructional_support_charter_building_comparison$
        `IV&V Instructional Supports`,
      
      instructional_support_charter_building_comparison$
        `IV&V Base Division I positions` *
        0.20,
      
      tol = 0.000001
    )
  ),
  
  # The support difference follows the difference in the calculation base
  all(
    near(
      instructional_support_charter_building_comparison$
        `Instructional Support difference`,
      
      instructional_support_charter_building_comparison$
        `Expected difference from Base position difference`,
      
      tol = 0.01
    )
  ),
  
  # Seven affected charter LEAs and 10 affected building rows
  nrow(
    instructional_support_affected_leas
  ) == 7,
  
  nrow(
    instructional_support_affected_buildings
  ) == 10,
  
  n_distinct(
    instructional_support_affected_buildings$
      DistrictCode
  ) == 7,
  
  # Approximately 17.19 net vocational positions are omitted
  near(
    sum(
      instructional_support_affected_buildings$
        `PEFC net vocational positions`
    ),
    17.19,
    tol = 0.01
  ),
  
  # Applying 20% produces approximately 3.438 positions
  near(
    sum(
      instructional_support_affected_buildings$
        `Expected vocational effect`
    ),
    3.438,
    tol = 0.001
  ),
  
  # The statewide reconciliation closes
  near(
    instructional_support_reconciliation$
      `Reconciliation difference`,
    0,
    tol = 0.000001
  )
)

message(
  paste(
    "Instructional Supports comparison validated:",
    "44 workbook-scope LEAs,",
    "48 charter calculation units,",
    "7 affected charter LEAs,",
    "10 affected building rows,",
    "17.19 omitted net vocational positions, and",
    "a statewide IV&V-minus-PEFC difference of",
    "3.430267 Instructional Support positions."
  )
)

# -----------------------------------------------------------------------------
# 22. Review the principal objects
# -----------------------------------------------------------------------------

exhibit16_raw
exhibit16

instructional_support_lea_comparison
instructional_support_charter_building_comparison
instructional_support_affected_buildings
instructional_support_affected_leas
instructional_support_reconciliation

View(exhibit16_raw)
View(exhibit16)

View(
  instructional_support_lea_comparison
)

View(
  instructional_support_charter_building_comparison
)

View(
  instructional_support_affected_buildings
)

View(
  instructional_support_affected_leas
)

View(
  instructional_support_reconciliation
)