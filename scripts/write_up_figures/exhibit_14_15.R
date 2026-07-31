# =============================================================================
# EXHIBITS 14 AND 15: PEFC VS. IV&V PROPOSED PRINCIPAL POSITIONS
#
# Creates:
#   exhibit14                    Formatted statewide principal comparison
#   exhibit14_raw                Unformatted statewide comparison
#   exhibit15                    Formatted LEA comparison
#   exhibit15_raw                Unformatted LEA comparison
#   lea_principal_comparison     Principal totals for all 43 LEAs
#   principal_unit_differences   Five calculation units behind the difference
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)

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
# 2. File paths
# -----------------------------------------------------------------------------

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

proposed_quantities_path <- file.path(
  "data",
  "output",
  "intermediate",
  "07_proposed_model_quantities.csv"
)

pefc_component_path <- file.path(
  "data",
  "output",
  "audit",
  "10_pefc_component_comparison.csv"
)

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
    
    CalculatorKey = normalize_name_local(
      CalculatorLEAName
    )
  )

proposed_quantities <- read_csv(
  proposed_quantities_path,
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode = as.integer(DistrictCode),
    
    IncludeInStatewide =
      tolower(as.character(IncludeInStatewide)) == "true",
    
    FundingQuantity = as.numeric(
      FundingQuantity
    )
  )

pefc_component_comparison <- read_csv(
  pefc_component_path,
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 4. Pull PEFC principal totals by LEA
#
# D13 is the Principal quantity in the PEFC Summary sheet.
#
# District LEA totals are identified by Type == "Central Office."
# Charter LEA totals generally use a Child value matching the District value.
# DAFB has a separate workbook-specific total-row treatment.
# -----------------------------------------------------------------------------

pefc_principal_by_lea <- summary_raw |>
  filter(
    !is.na(.data[["District"]]),
    !.data[["District"]] %in% c(
      "CHECK:",
      "Statewide"
    )
  ) |>
  mutate(
    CalculatorLEAName =
      as.character(.data[["District"]]),
    
    CalculatorTotalRow =
      as.character(.data[["Child"]]),
    
    CalculatorKey = normalize_name_local(
      CalculatorLEAName
    ),
    
    IsLEATotalRow =
      coalesce(
        .data[["Type"]] == "Central Office",
        FALSE
      ) |
      coalesce(
        normalize_name_local(CalculatorTotalRow) ==
          CalculatorKey,
        FALSE
      ) |
      (
        CalculatorLEAName == "DAFB" &
          CalculatorTotalRow ==
          "Dover Air Force Base"
      )
  ) |>
  filter(
    IsLEATotalRow
  ) |>
  transmute(
    CalculatorKey,
    PEFCPrincipals = as.numeric(
      .data[["D13"]]
    )
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
    IncludeInStatewide
  ) |>
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
    CalculationLevel == "School",
    FundingSection ==
      "Base Funding (State Support)",
    Component == "Principal"
  ) |>
  summarise(
    IVVPrincipals = sum(
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
# 6. Compare PEFC and IV&V principal totals for all 43 LEAs
# -----------------------------------------------------------------------------

lea_principal_comparison <- pefc_principal_by_lea |>
  full_join(
    ivv_principal_by_lea,
    by = c(
      "DistrictCode",
      "DistrictName",
      "LEAType"
    )
  ) |>
  mutate(
    PEFCPrincipals = coalesce(
      PEFCPrincipals,
      0
    ),
    
    IVVPrincipals = coalesce(
      IVVPrincipals,
      0
    ),
    
    Difference =
      IVVPrincipals - PEFCPrincipals
  ) |>
  arrange(
    LEAType,
    DistrictName
  )

# -----------------------------------------------------------------------------
# 7. Pull the principal funding rate
# -----------------------------------------------------------------------------

principal_rate <- pefc_component_comparison |>
  filter(
    Component == "Principal"
  ) |>
  distinct(
    PEFCRate
  ) |>
  pull(
    PEFCRate
  )

stopifnot(
  length(principal_rate) == 1L
)

# -----------------------------------------------------------------------------
# 8. Exhibit 14: Primary-scope principal comparison
# -----------------------------------------------------------------------------

exhibit14_raw <- lea_principal_comparison |>
  summarise(
    `PEFC principals` = sum(
      PEFCPrincipals
    ),
    
    `IV&V principals` = sum(
      IVVPrincipals
    ),
    
    Difference = sum(
      Difference
    )
  ) |>
  mutate(
    `Funding effect` =
      Difference * principal_rate
  )

exhibit14 <- exhibit14_raw |>
  mutate(
    Difference = sprintf(
      "%+d",
      as.integer(round(Difference))
    ),
    
    `Funding effect` = paste0(
      if_else(
        `Funding effect` >= 0,
        "+$",
        "-$"
      ),
      sprintf(
        "%.3fM",
        abs(`Funding effect`) / 1e6
      )
    )
  )

# -----------------------------------------------------------------------------
# 9. Identify the district calculation units behind the difference
# -----------------------------------------------------------------------------

affected_district_codes <- lea_principal_comparison |>
  filter(
    LEAType == "District",
    Difference != 0
  ) |>
  pull(
    DistrictCode
  )

# PEFC school-level principal quantities for the affected districts
pefc_principal_by_unit <- summary_raw |>
  mutate(
    CalculatorKey = normalize_name_local(
      as.character(.data[["District"]])
    ),
    
    UnitName = as.character(
      .data[["Child"]]
    ),
    
    UnitKey = normalize_name_local(
      UnitName
    )
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
    LEAType == "District",
    DistrictCode %in%
      affected_district_codes,
    .data[["Type"]] == "School"
  ) |>
  transmute(
    DistrictCode,
    DistrictName,
    UnitKey,
    
    PEFCUnitName = UnitName,
    
    PEFCPrincipals = as.numeric(
      .data[["D13"]]
    )
  )

# IV&V school-level principal quantities for the affected districts
ivv_principal_by_unit <- proposed_quantities |>
  filter(
    IncludeInStatewide,
    LEAType == "District",
    DistrictCode %in%
      affected_district_codes,
    CalculationLevel == "School",
    FundingSection ==
      "Base Funding (State Support)",
    Component == "Principal"
  ) |>
  transmute(
    DistrictCode,
    DistrictName,
    
    UnitKey = normalize_name_local(
      CalculationUnitName
    ),
    
    IVVUnitName =
      CalculationUnitName,
    
    IVVPrincipals =
      FundingQuantity
  )

# Calculation units with different principal quantities
principal_unit_differences <-
  pefc_principal_by_unit |>
  full_join(
    ivv_principal_by_unit,
    by = c(
      "DistrictCode",
      "DistrictName",
      "UnitKey"
    )
  ) |>
  mutate(
    UnitName = coalesce(
      IVVUnitName,
      PEFCUnitName
    ),
    
    PEFCPrincipals = coalesce(
      PEFCPrincipals,
      0
    ),
    
    IVVPrincipals = coalesce(
      IVVPrincipals,
      0
    ),
    
    Difference =
      IVVPrincipals - PEFCPrincipals
  ) |>
  filter(
    Difference != 0
  ) |>
  select(
    DistrictCode,
    DistrictName,
    UnitName,
    PEFCPrincipals,
    IVVPrincipals,
    Difference
  ) |>
  arrange(
    DistrictName,
    UnitName
  )

# -----------------------------------------------------------------------------
# 10. Create LEA-level explanations
# -----------------------------------------------------------------------------

principal_explanations <- principal_unit_differences |>
  group_by(
    DistrictCode,
    DistrictName
  ) |>
  summarise(
    AffectedUnits = str_c(
      UnitName,
      collapse = " and "
    ),
    
    AffectedUnitCount = n(),
    
    Explanation = if_else(
      AffectedUnitCount == 1,
      
      str_c(
        AffectedUnits,
        paste(
          "receives zero in the workbook and one",
          "under the IV&V one-per-unit rule."
        ),
        sep = " "
      ),
      
      str_c(
        AffectedUnits,
        paste(
          "receive zero in the workbook and one each",
          "under the IV&V one-per-unit rule."
        ),
        sep = " "
      )
    ),
    
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 11. Exhibit 15: LEAs with different principal counts
# -----------------------------------------------------------------------------

exhibit15_raw <- lea_principal_comparison |>
  filter(
    Difference != 0
  ) |>
  left_join(
    principal_explanations,
    by = c(
      "DistrictCode",
      "DistrictName"
    )
  ) |>
  mutate(
    LEA = case_when(
      DistrictName == "Capital School District" ~
        "Capital",
      
      DistrictName == "Christina School District" ~
        "Christina",
      
      str_detect(
        DistrictName,
        "^New Castle County Vocational-Technical"
      ) ~
        "New Castle County Vo-Tech",
      
      DistrictName ==
        "Red Clay Consolidated School District" ~
        "Red Clay",
      
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

# Add the primary-scope total row
exhibit15_raw <- bind_rows(
  exhibit15_raw,
  
  tibble(
    LEA = "Primary scope",
    
    PEFC =
      exhibit14_raw$`PEFC principals`,
    
    `IV&V` =
      exhibit14_raw$`IV&V principals`,
    
    Difference =
      exhibit14_raw$Difference,
    
    Explanation = str_c(
      nrow(principal_unit_differences),
      paste(
        "district calculation units explain",
        "the primary-scope difference."
      ),
      sep = " "
    )
  )
)

# Format the displayed difference
exhibit15 <- exhibit15_raw |>
  mutate(
    Difference = sprintf(
      "%+d",
      as.integer(round(Difference))
    )
  )

# -----------------------------------------------------------------------------
# 12. Validation
# -----------------------------------------------------------------------------

stopifnot(
  # Primary scope
  nrow(lea_principal_comparison) == 43,
  
  sum(
    lea_principal_comparison$LEAType ==
      "District"
  ) == 19,
  
  sum(
    lea_principal_comparison$LEAType ==
      "Charter"
  ) == 24,
  
  # Exhibit 14 values
  exhibit14_raw$`PEFC principals` == 248,
  
  exhibit14_raw$`IV&V principals` == 253,
  
  exhibit14_raw$Difference == 5,
  
  near(
    exhibit14_raw$`Funding effect`,
    765325
  ),
  
  # Five district calculation units explain the difference
  nrow(principal_unit_differences) == 5,
  
  all(
    principal_unit_differences$PEFCPrincipals ==
      0
  ),
  
  all(
    principal_unit_differences$IVVPrincipals ==
      1
  ),
  
  # All charter principal totals match
  all(
    lea_principal_comparison$Difference[
      lea_principal_comparison$LEAType ==
        "Charter"
    ] == 0
  )
)

message("Exhibits 14 and 15 validated.")

# -----------------------------------------------------------------------------
# 13. Review objects
# -----------------------------------------------------------------------------

exhibit14
exhibit15
principal_unit_differences
lea_principal_comparison

View(exhibit14)
View(exhibit15)
View(principal_unit_differences)
View(lea_principal_comparison)
