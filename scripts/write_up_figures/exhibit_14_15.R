# =============================================================================
# EXHIBITS 14 AND 15: PEFC VS. IV&V PROPOSED PRINCIPAL POSITIONS
# Verified revision: July 31, 2026
#
# Creates:
#   exhibit14                    Formatted statewide principal comparison
#   exhibit14_raw                Unformatted statewide comparison
#   exhibit15                    Formatted LEA comparison
#   exhibit15_raw                Unformatted LEA comparison
#   lea_principal_comparison     Principal totals for all 43 aligned-scope LEAs
#   principal_unit_differences   Five calculation units behind the difference
#   exhibit14_15_context         Scope, rate, and reconciliation context
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)

# Assumes the working directory is the project root.
source(file.path("scripts", "00_settings.R"))

# -----------------------------------------------------------------------------
# 1. Helper function
# -----------------------------------------------------------------------------

normalize_name_local <- function(x) {
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

# -----------------------------------------------------------------------------
# 2. File paths and required inputs
# -----------------------------------------------------------------------------

calculator_path <- file.path(
  input_dir,
  "Copy of Calculator for 25-26 w Charter (003).xlsm"
)

proposed_quantities_path <- file.path(
  intermediate_dir,
  "07_proposed_model_quantities.csv"
)

pefc_component_path <- file.path(
  audit_dir,
  "10_pefc_component_comparison.csv"
)

staffing_component_path <- file.path(
  final_dir,
  "11_staffing_component_comparison.csv"
)

required_paths <- c(
  calculator_path,
  lea_crosswalk_path,
  proposed_quantities_path,
  pefc_component_path,
  staffing_component_path
)

missing_paths <- required_paths[!file.exists(required_paths)]

if (length(missing_paths) > 0L) {
  stop(
    "Missing required Exhibit 14/15 input file(s):\n",
    paste(missing_paths, collapse = "\n"),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 3. Read inputs
# -----------------------------------------------------------------------------

summary_raw <- read_excel(
  calculator_path,
  sheet = "Summary"
)

lea_crosswalk <- read_csv(
  lea_crosswalk_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide =
      tolower(as.character(IncludeInStatewide)) == "true",
    CalculatorKey = normalize_name_local(CalculatorLEAName)
  )

proposed_quantities <- read_csv(
  proposed_quantities_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    IncludeInStatewide =
      tolower(as.character(IncludeInStatewide)) == "true",
    FundingQuantity = as.numeric(FundingQuantity)
  )

pefc_component_comparison <- read_csv(
  pefc_component_path,
  show_col_types = FALSE
)

staffing_component_comparison <- read_csv(
  staffing_component_path,
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 4. Pull PEFC principal totals by LEA
#
# D13 is the Principal quantity in the PEFC Summary sheet.
# District LEA totals are identified by Type == "Central Office."
# Charter LEA totals generally use a Child value matching the District value.
# DAFB is retained in the workbook source only for audit and is removed by the
# maintained IncludeInStatewide flag before the aligned comparison is created.
# -----------------------------------------------------------------------------

pefc_principal_by_lea <- summary_raw |>
  filter(
    !is.na(.data[["District"]]),
    !.data[["District"]] %in% c("CHECK:", "Statewide")
  ) |>
  mutate(
    CalculatorLEAName = as.character(.data[["District"]]),
    CalculatorTotalRow = as.character(.data[["Child"]]),
    CalculatorKey = normalize_name_local(CalculatorLEAName),
    IsLEATotalRow =
      coalesce(.data[["Type"]] == "Central Office", FALSE) |
      coalesce(
        normalize_name_local(CalculatorTotalRow) == CalculatorKey,
        FALSE
      ) |
      (
        CalculatorLEAName == "DAFB" &
          CalculatorTotalRow == "Dover Air Force Base"
      )
  ) |>
  filter(IsLEATotalRow) |>
  transmute(
    CalculatorKey,
    PEFCPrincipals = as.numeric(.data[["D13"]])
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

unmatched_pefc_rows <- pefc_principal_by_lea |>
  filter(is.na(DistrictCode))

if (nrow(unmatched_pefc_rows) > 0L) {
  stop(
    "One or more PEFC principal total rows could not be matched to the LEA crosswalk.",
    call. = FALSE
  )
}

pefc_principal_by_lea <- pefc_principal_by_lea |>
  filter(IncludeInStatewide) |>
  select(
    DistrictCode,
    DistrictName,
    LEAType,
    PEFCPrincipals
  )

# -----------------------------------------------------------------------------
# 5. Pull IV&V proposed principal totals by LEA
# -----------------------------------------------------------------------------

ivv_principal_by_lea <- proposed_quantities |>
  filter(
    IncludeInStatewide,
    !DistrictCode %in% primary_reporting_excluded_lea_codes,
    CalculationLevel == "School",
    FundingSection == "Base Funding (State Support)",
    Component == "Principal"
  ) |>
  summarise(
    IVVPrincipals = sum(FundingQuantity, na.rm = TRUE),
    .by = c(DistrictCode, DistrictName, LEAType)
  )

# -----------------------------------------------------------------------------
# 6. Compare PEFC and IV&V principal totals for all 43 aligned-scope LEAs
# -----------------------------------------------------------------------------

lea_principal_comparison <- pefc_principal_by_lea |>
  full_join(
    ivv_principal_by_lea,
    by = c("DistrictCode", "DistrictName", "LEAType")
  )

missing_principal_sides <- lea_principal_comparison |>
  filter(is.na(PEFCPrincipals) | is.na(IVVPrincipals))

if (nrow(missing_principal_sides) > 0L) {
  stop(
    "The PEFC and IV&V principal totals do not contain the same aligned-scope LEAs.",
    call. = FALSE
  )
}

lea_principal_comparison <- lea_principal_comparison |>
  mutate(
    Difference = IVVPrincipals - PEFCPrincipals
  ) |>
  arrange(LEAType, DistrictName)

# -----------------------------------------------------------------------------
# 7. Pull and reconcile the principal funding rate
# -----------------------------------------------------------------------------

pefc_principal_rate <- pefc_component_comparison |>
  filter(Component == "Principal") |>
  distinct(PEFCRate) |>
  pull(PEFCRate)

staffing_principal_record <- staffing_component_comparison |>
  filter(
    AnalysisSection == "Staffing rules",
    ComparisonCategory == "Principal"
  )

if (
  length(pefc_principal_rate) != 1L ||
    nrow(staffing_principal_record) != 1L
) {
  stop(
    "Exhibits 14 and 15 expected exactly one principal rate and one final principal comparison record.",
    call. = FALSE
  )
}

principal_rate <- as.numeric(staffing_principal_record$CommonRate)

if (
  abs(principal_rate - as.numeric(pefc_principal_rate)) >
    comparison_tolerance
) {
  stop(
    "The PEFC and final staffing outputs do not use the same principal rate.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 8. Exhibit 14: Aligned-scope principal comparison
# -----------------------------------------------------------------------------

exhibit14_raw <- lea_principal_comparison |>
  summarise(
    `PEFC principals` = sum(PEFCPrincipals),
    `IV&V principals` = sum(IVVPrincipals),
    Difference = sum(Difference)
  ) |>
  mutate(
    `Funding effect` = Difference * principal_rate
  )

exhibit14 <- exhibit14_raw |>
  mutate(
    Difference = sprintf("%+d", as.integer(round(Difference))),
    `Funding effect` = paste0(
      if_else(`Funding effect` >= 0, "+$", "-$"),
      sprintf("%.3fM", abs(`Funding effect`) / 1e6)
    )
  )

# -----------------------------------------------------------------------------
# 9. Identify the district calculation units behind the difference
# -----------------------------------------------------------------------------

affected_district_codes <- lea_principal_comparison |>
  filter(
    LEAType == "District",
    abs(Difference) > comparison_tolerance
  ) |>
  pull(DistrictCode)

# PEFC school-level principal quantities for affected districts.
pefc_principal_by_unit <- summary_raw |>
  mutate(
    CalculatorKey = normalize_name_local(as.character(.data[["District"]])),
    UnitName = as.character(.data[["Child"]]),
    UnitKey = normalize_name_local(UnitName)
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
  ) |>
  filter(
    IncludeInStatewide,
    !DistrictCode %in% primary_reporting_excluded_lea_codes,
    LEAType == "District",
    DistrictCode %in% affected_district_codes,
    .data[["Type"]] == "School"
  ) |>
  transmute(
    DistrictCode,
    DistrictName,
    UnitKey,
    PEFCUnitName = UnitName,
    PEFCPrincipals = as.numeric(.data[["D13"]])
  )

# IV&V school-level principal quantities for affected districts.
ivv_principal_by_unit <- proposed_quantities |>
  filter(
    IncludeInStatewide,
    !DistrictCode %in% primary_reporting_excluded_lea_codes,
    LEAType == "District",
    DistrictCode %in% affected_district_codes,
    CalculationLevel == "School",
    FundingSection == "Base Funding (State Support)",
    Component == "Principal"
  ) |>
  transmute(
    DistrictCode,
    DistrictName,
    UnitKey = normalize_name_local(CalculationUnitName),
    IVVUnitName = CalculationUnitName,
    IVVPrincipals = FundingQuantity
  )

principal_unit_differences <- pefc_principal_by_unit |>
  full_join(
    ivv_principal_by_unit,
    by = c("DistrictCode", "DistrictName", "UnitKey")
  ) |>
  mutate(
    UnitName = coalesce(IVVUnitName, PEFCUnitName),
    PEFCPrincipals = coalesce(PEFCPrincipals, 0),
    IVVPrincipals = coalesce(IVVPrincipals, 0),
    Difference = IVVPrincipals - PEFCPrincipals
  ) |>
  filter(abs(Difference) > comparison_tolerance) |>
  select(
    DistrictCode,
    DistrictName,
    UnitName,
    PEFCPrincipals,
    IVVPrincipals,
    Difference
  ) |>
  arrange(DistrictName, UnitName)

# -----------------------------------------------------------------------------
# 10. Create LEA-level explanations
# -----------------------------------------------------------------------------

principal_explanations <- principal_unit_differences |>
  group_by(DistrictCode, DistrictName) |>
  summarise(
    AffectedUnits = str_c(UnitName, collapse = " and "),
    AffectedUnitCount = n(),
    Explanation = if_else(
      AffectedUnitCount == 1,
      str_c(
        AffectedUnits,
        "receives zero in the workbook and one under the IV&V one-per-unit rule.",
        sep = " "
      ),
      str_c(
        AffectedUnits,
        "receive zero in the workbook and one each under the IV&V one-per-unit rule.",
        sep = " "
      )
    ),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 11. Exhibit 15: LEAs with different principal counts
# -----------------------------------------------------------------------------

exhibit15_raw <- lea_principal_comparison |>
  filter(abs(Difference) > comparison_tolerance) |>
  left_join(
    principal_explanations,
    by = c("DistrictCode", "DistrictName")
  ) |>
  mutate(
    LEA = case_when(
      DistrictName == "Capital School District" ~ "Capital",
      DistrictName == "Christina School District" ~ "Christina",
      str_detect(
        DistrictName,
        "^New Castle County Vocational-Technical"
      ) ~ "New Castle County Vo-Tech",
      DistrictName == "Red Clay Consolidated School District" ~ "Red Clay",
      TRUE ~ DistrictName
    )
  ) |>
  select(
    LEA,
    PEFC = PEFCPrincipals,
    `IV&V` = IVVPrincipals,
    Difference,
    Explanation
  ) |>
  arrange(
    match(
      LEA,
      c(
        "Capital",
        "Christina",
        "New Castle County Vo-Tech",
        "Red Clay"
      )
    )
  )

# Add the aligned-scope total row.
exhibit15_raw <- bind_rows(
  exhibit15_raw,
  tibble(
    LEA = "Primary scope",
    PEFC = exhibit14_raw$`PEFC principals`,
    `IV&V` = exhibit14_raw$`IV&V principals`,
    Difference = exhibit14_raw$Difference,
    Explanation = str_c(
      nrow(principal_unit_differences),
      "district calculation units explain the aligned-scope difference."
    )
  )
)

exhibit15 <- exhibit15_raw |>
  mutate(
    Difference = sprintf("%+d", as.integer(round(Difference)))
  )

# -----------------------------------------------------------------------------
# 12. Validation and context
# -----------------------------------------------------------------------------

if (n_distinct(lea_principal_comparison$DistrictCode) != 43L) {
  stop("Exhibits 14 and 15 expected 43 aligned-scope LEAs.", call. = FALSE)
}

if (
  sum(lea_principal_comparison$LEAType == "District") != 19L ||
    sum(lea_principal_comparison$LEAType == "Charter") != 24L
) {
  stop("Exhibits 14 and 15 expected 19 districts and 24 charters.", call. = FALSE)
}

if (
  any(
    lea_principal_comparison$DistrictCode %in%
      primary_reporting_excluded_lea_codes
  )
) {
  stop("An excluded LEA entered Exhibits 14 and 15.", call. = FALSE)
}

if (!basse_district_code %in% lea_principal_comparison$DistrictCode) {
  stop("BASSE is missing from Exhibits 14 and 15.", call. = FALSE)
}

if (
  abs(
    exhibit14_raw$`IV&V principals` -
      staffing_principal_record$ProposedKnownQuantity
  ) > comparison_tolerance
) {
  stop(
    "The Exhibit 14 IV&V principal total does not reconcile to the final staffing output.",
    call. = FALSE
  )
}

if (
  staffing_principal_record$ComparisonStatus != "Confirmed" ||
    !staffing_principal_record$IsCompleteForFinalComparison
) {
  stop(
    "The final staffing output does not classify Principal as a complete confirmed comparison.",
    call. = FALSE
  )
}

if (
  abs(sum(principal_unit_differences$Difference) - exhibit14_raw$Difference) >
    comparison_tolerance
) {
  stop(
    "The calculation-unit differences do not reconcile to the statewide principal difference.",
    call. = FALSE
  )
}

if (
  any(
    abs(
      lea_principal_comparison$Difference[
        lea_principal_comparison$LEAType == "Charter"
      ]
    ) > comparison_tolerance
  )
) {
  stop(
    "At least one charter principal total differs between the PEFC workbook and IV&V reproduction.",
    call. = FALSE
  )
}

# These values are specific to the current verified source workbook and provide
# a visible regression check without replacing the dynamic reconciliations.
if (
  exhibit14_raw$`PEFC principals` != 248 ||
    exhibit14_raw$`IV&V principals` != 253 ||
    exhibit14_raw$Difference != 5 ||
    nrow(principal_unit_differences) != 5L
) {
  stop(
    "The verified principal comparison changed from 248 PEFC, 253 IV&V, and five differing calculation units.",
    call. = FALSE
  )
}

exhibit14_15_context <- tibble(
  ReportingScope = primary_reporting_scope_short,
  LEACount = n_distinct(lea_principal_comparison$DistrictCode),
  DistrictCount = sum(lea_principal_comparison$LEAType == "District"),
  CharterCount = sum(lea_principal_comparison$LEAType == "Charter"),
  PrincipalRate = principal_rate,
  PEFCPrincipals = exhibit14_raw$`PEFC principals`,
  IVVPrincipals = exhibit14_raw$`IV&V principals`,
  PositionDifference = exhibit14_raw$Difference,
  FundingEffect = exhibit14_raw$`Funding effect`,
  AffectedLEACount = sum(abs(lea_principal_comparison$Difference) > comparison_tolerance),
  AffectedCalculationUnitCount = nrow(principal_unit_differences),
  DAFBIncluded = dafb_district_code %in% lea_principal_comparison$DistrictCode,
  BASSEIncluded = basse_district_code %in% lea_principal_comparison$DistrictCode
)

message(
  "Exhibits 14 and 15 validated for ",
  primary_reporting_scope_short,
  ": 248 PEFC principals, 253 IV&V principals, and a +5-position difference."
)

# -----------------------------------------------------------------------------
# 13. Review objects
# -----------------------------------------------------------------------------

exhibit14
exhibit15
principal_unit_differences
lea_principal_comparison
exhibit14_15_context

if (interactive()) {
  View(exhibit14)
  View(exhibit15)
  View(principal_unit_differences)
  View(lea_principal_comparison)
  View(exhibit14_15_context)
}
