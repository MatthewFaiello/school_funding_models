library(dplyr)
library(readr)
library(tibble)

# Assumes the working directory is the project root
current <- read_csv(
  "data/output/intermediate/04_current_model_quantities.csv",
  show_col_types = FALSE
)

proposed <- read_csv(
  "data/output/intermediate/07_proposed_model_quantities.csv",
  show_col_types = FALSE
)

# Current principal rows for official coded schools
current_principals <- current |>
  filter(
    Component == "Principal",
    IncludeInStatewide,
    IsSchool
  )

# Proposed principal rows for school calculation units
proposed_principals <- proposed |>
  filter(
    Component == "Principal",
    IncludeInStatewide,
    IsSchoolCalculationUnit
  )

# Bridge components
current_positions <- sum(
  current_principals$FundingQuantity,
  na.rm = TRUE
)

current_charter_codes <- current_principals |>
  filter(LEAType == "Charter") |>
  nrow()

proposed_charter_units <- proposed_principals |>
  filter(LEAType == "Charter") |>
  nrow()

additional_charter_units <-
  proposed_charter_units - current_charter_codes

below_current_minimum <- current_principals |>
  filter(
    RawInputValue < 15,
    FundingQuantity == 0
  ) |>
  nrow()

proposed_positions <- sum(
  proposed_principals$FundingQuantity,
  na.rm = TRUE
)

# Confirm that the bridge reconciles
stopifnot(
  current_positions +
    additional_charter_units +
    below_current_minimum ==
    proposed_positions
)

# Exhibit 7
exhibit7 <- tibble(
  `Bridge element` = c(
    "Recreated current principal positions",
    "Effect of additional charter building calculation units",
    "Effect of replacing the current 15-unit minimum",
    "IV&V proposed principal positions"
  ),
  Positions = c(
    as.character(current_positions),
    paste0("+", additional_charter_units),
    paste0("+", below_current_minimum),
    as.character(proposed_positions)
  )
)

exhibit7