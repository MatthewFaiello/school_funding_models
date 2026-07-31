# =============================================================================
# EXHIBIT 2: IV&V ANALYSIS AT A GLANCE
#
# Creates:
#   exhibit2_numbers
#
# All values are calculated directly from maintained pipeline outputs.
# =============================================================================

library(dplyr)
library(readr)
library(tibble)

# -----------------------------------------------------------------------------
# 1. Helper
# -----------------------------------------------------------------------------

to_logical_flag <- function(x) {
  tolower(
    trimws(
      as.character(x)
    )
  ) %in% c(
    "true",
    "t",
    "1",
    "yes"
  )
}

# -----------------------------------------------------------------------------
# 2. Read pipeline outputs
# -----------------------------------------------------------------------------

run_settings <- read_csv(
  "data/output/audit/00_run_settings.csv",
  show_col_types = FALSE
)

shared_model_input <- read_csv(
  "data/output/intermediate/02_shared_model_input.csv",
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode =
      as.integer(DistrictCode),
    
    SchoolCode =
      as.integer(SchoolCode),
    
    IsSchool =
      to_logical_flag(IsSchool),
    
    Enrollment =
      as.numeric(Enrollment)
  )

proposed_calculation_units <- read_csv(
  "data/output/intermediate/06_proposed_school_calculation_units.csv",
  show_col_types = FALSE
) |>
  mutate(
    DistrictCode =
      as.integer(DistrictCode),
    
    IncludeInStatewide =
      to_logical_flag(IncludeInStatewide),
    
    IsSchoolCalculationUnit =
      to_logical_flag(IsSchoolCalculationUnit)
  )

# -----------------------------------------------------------------------------
# 3. Primary-scope LEAs
# -----------------------------------------------------------------------------

primary_scope_leas <- proposed_calculation_units |>
  distinct(
    DistrictCode,
    DistrictName,
    LEAType,
    IncludeInStatewide
  ) |>
  filter(
    IncludeInStatewide
  )

# -----------------------------------------------------------------------------
# 4. Students
# -----------------------------------------------------------------------------

primary_scope_students <- shared_model_input |>
  filter(
    AggregationLevel == "Statewide",
    
    DistrictName ==
      "State Total Excluding Dover Air Force Base"
  ) |>
  pull(
    Enrollment
  )

stopifnot(
  length(primary_scope_students) == 1L
)

# -----------------------------------------------------------------------------
# 5. Official coded schools
#
# Step 02 contains 232 official coded-school records before scope exclusions:
# 229 in the primary scope and 3 for DAFB.
# -----------------------------------------------------------------------------

official_coded_schools <- shared_model_input |>
  filter(
    AggregationLevel == "School",
    RecordType == "School",
    IsSchool,
    !is.na(SchoolCode)
  ) |>
  inner_join(
    primary_scope_leas |>
      select(
        DistrictCode,
        LEAType
      ),
    by = "DistrictCode"
  )

# -----------------------------------------------------------------------------
# 6. Proposed school calculation units
# -----------------------------------------------------------------------------

proposed_school_units <- proposed_calculation_units |>
  filter(
    IncludeInStatewide,
    IsSchoolCalculationUnit
  )

# -----------------------------------------------------------------------------
# 7. Fixed weighted funding pools
# -----------------------------------------------------------------------------

opportunity_pool <- run_settings |>
  filter(
    Setting == "Opportunity funding pool"
  ) |>
  pull(
    Value
  ) |>
  as.numeric()

operational_pool <- run_settings |>
  filter(
    Setting == "Operational funding pool"
  ) |>
  pull(
    Value
  ) |>
  as.numeric()

stopifnot(
  length(opportunity_pool) == 1L,
  length(operational_pool) == 1L
)

# -----------------------------------------------------------------------------
# 8. Create the numeric audit object
# -----------------------------------------------------------------------------

exhibit2_numbers <- tibble(
  PrimaryLEAs =
    nrow(primary_scope_leas),
  
  DistrictLEAs =
    sum(
      primary_scope_leas$LEAType ==
        "District"
    ),
  
  CharterLEAs =
    sum(
      primary_scope_leas$LEAType ==
        "Charter"
    ),
  
  Students =
    primary_scope_students,
  
  OfficialCodedSchools =
    nrow(official_coded_schools),
  
  DistrictOfficialSchools =
    sum(
      official_coded_schools$LEAType ==
        "District"
    ),
  
  CharterOrganizationRecords =
    sum(
      official_coded_schools$LEAType ==
        "Charter"
    ),
  
  ProposedSchoolCalculationUnits =
    nrow(proposed_school_units),
  
  DistrictSchoolCalculationUnits =
    sum(
      proposed_school_units$LEAType ==
        "District"
    ),
  
  CharterBuildingCalculationUnits =
    sum(
      proposed_school_units$LEAType ==
        "Charter"
    ),
  
  OpportunityFundingPool =
    opportunity_pool,
  
  OperationalFundingPool =
    operational_pool
)

exhibit2_numbers
