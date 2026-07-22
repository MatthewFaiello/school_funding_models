# School Funding Model Pipeline

## Design rules

- **One job per script.** Input preparation, formulas, rates, and comparisons are separate steps.
- **One settings file.** School year, funding pools, DAFB treatment, and model toggles are edited in `scripts/00_settings.R`.
- **Inputs and outputs are separated.** Source and maintained files stay in `data/input/`. Generated files go to `data/output/`.
- **No hidden model helpers.** Formula functions remain in the script where they are used.
- **Raw counts stay visible.** Proposed weighted-funding outputs show the raw count, applied weight, weighted count, funding rate, and funding amount.
- **Shared charter totals remain authoritative.** Calculator building values determine the distribution across buildings, not the charter total.
- **Every adjustment is reviewable.** Charter allocations, rollups, missing inputs, provisional values, and rate issues are written to CSV files.
- **Visible name exceptions.** Source-name differences are maintained in `data/input/entity_crosswalk.csv`, not hidden inside R code.
- **Full precision is retained through the model.** Intermediate model files are written without rounding. Only terminal review files are formatted for readability, unless a written business rule explicitly requires an operation such as `floor()`.
- **The pipeline stops on structural errors.** Duplicate keys, failed matches, invalid counts, and failed reconciliations do not pass silently.

## Project structure

```text
transparent_funding_pipeline/
├── school_funding_model.Rproj
├── RUN_PIPELINE.R
├── README.md
├── scripts/
│   ├── 00_settings.R
│   ├── 00_run_all.R
│   ├── 01_student_counts.sql
│   ├── 02_build_shared_input.R
│   ├── 03_prepare_current_inputs.R
│   ├── 04_calculate_current_quantities.R
│   ├── 05_apply_current_rates.R
│   ├── 06_prepare_proposed_inputs.R
│   ├── 07_calculate_proposed_quantities.R
│   ├── 08_apply_proposed_rates.R
│   └── 09_compare_models.R
└── data/
    ├── input/
    └── output/
```

## Run order

| Step | Script | Purpose |
|---:|---|---|
| 1 | `01_student_counts.sql` | Export official school-level Enrollment, LI, MLL, K-8, and Grade 10 counts. |
| 2 | `02_build_shared_input.R` | Join the SQL export to the unit-count workbook and create one shared input. |
| 3 | `03_prepare_current_inputs.R` | Add current-model custodial, cafeteria, and lunch-program inputs. |
| 4 | `04_calculate_current_quantities.R` | Calculate current-model positions. No rates are applied. |
| 5 | `05_apply_current_rates.R` | Apply the current-model rate map and funding rates. |
| 6 | `06_prepare_proposed_inputs.R` | Create district school-code units, charter building units, weighted inputs, and LEA inputs. |
| 7 | `07_calculate_proposed_quantities.R` | Calculate proposed positions and weighted counts. No rates are applied. |
| 8 | `08_apply_proposed_rates.R` | Apply proposed rates and create transparent weighted-funding summaries. |
| 9 | `09_compare_models.R` | Compare current and proposed funding at the school, LEA, and state levels. |

`00_run_all.R` runs Steps 2 through 9. The SQL export must already exist. It removes prior generated CSVs before starting so a failed run cannot leave stale later-step outputs that appear current. Files in `data/input/` are never removed.

Every run also creates:

```text
data/output/00_run_settings.csv
data/output/00_run_manifest.csv
```

The manifest records run status, relative file path, file size, CSV row count, and MD5 hash for maintained inputs, scripts, and generated outputs.

## First run

1. Open `school_funding_model.Rproj`.
2. Install the required packages once:

```r
install.packages(c("tidyverse", "readxl"))
```

3. Run `scripts/01_student_counts.sql` in SQL Server.
4. Save the result as `data/input/student_counts.csv`.
5. Review the choices in `scripts/00_settings.R`.
6. Open and run `RUN_PIPELINE.R`.

The file contains only:

```r
source(file.path("scripts", "00_run_all.R"))
```

## Model choices in one place

The main toggles are near the top of `00_settings.R`:

```r
operational_enrollment_basis <- "total"
weighted_rate_method <- "recalculated"
```

Operational enrollment accepts:

- `"total"`: total enrollment
- `"regular_ed"`: regular education enrollment only

Weighted rate method accepts:

- `"provided"`: rates supplied in `funding_rates.csv`
- `"recalculated"`: fixed funding pool divided by the statewide weighted count

Every run writes the selected values and software versions to:

```text
data/output/00_run_settings.csv
```

## How charter buildings are handled

Districts and DAFB continue to use one school code as one school calculation unit.

For charters:

1. The calculator determines the proposed building rows.
2. The shared charter total remains the official total.
3. Each input category uses its calculator building shares when available.
4. Total-enrollment shares are used only when a category-specific share is unavailable.
5. A manual share is required only when neither share can be calculated.
6. Adjusted building values must sum back to the shared charter total.

Review the full audit trail in:

```text
data/output/06_proposed_charter_reconciliation.csv
```

## Raw enrollment versus weighted enrollment

The raw Operational enrollment count is not the same as the total Operational weighted count.

For example, the enrollment component is shown as:

```text
Raw enrollment × 0.20 = weighted enrollment
```

The clearest review file is:

```text
data/output/08_proposed_weighted_component_summary.csv
```

It contains:

- `RawCount`
- `AppliedWeight`
- `WeightedCount`
- `FundingRate`
- `FundingAmount`

The section-level denominator is separately labeled `TotalWeightedCount` in:

```text
data/output/08_proposed_weighted_rate_summary.csv
```

## Novice review guide

Use `AUDIT_CHECKLIST.md` for the recommended review order and the expected counts for the current draft. `PIPELINE_MAP.csv` shows what every script reads, writes, and asks the reviewer to verify. `FILE_MANIFEST.csv` records the packaged file sizes and SHA-256 hashes.

## Main outputs

For most review and reporting work, start with:

```text
02_shared_model_input.csv
05_current_model_lea_summary.csv
08_proposed_model_lea_summary.csv
08_proposed_weighted_component_summary.csv
09_lea_comparison.csv
09_state_comparison.csv
```

## Audit and issue outputs

These files should be reviewed after every run:

```text
02_shared_input_qc_summary.csv
02_shared_input_qc_detail.csv
03_current_input_qc.csv
04_current_model_issues.csv
05_current_model_rate_issues.csv
06_proposed_charter_reconciliation.csv
06_proposed_input_qc.csv
07_proposed_model_issues.csv
08_proposed_model_rate_issues.csv
09_comparison_qc.csv
```

Blank values are not silently changed to zero unless the rule explicitly says they should be zero.

## Current-model limitations

The recreated current-model total remains preliminary where inputs or rates are missing. The main gaps are:

- custodian positions
- custodial units used for Buildings and Grounds Supervisors
- school-lunch indicators used for Food Services Supervisors
- charter cafeteria satellite counts
- cafeteria manager and cafeteria worker rates
- the separate district cafeteria process

The comparison output identifies the current-model total as partial when these gaps remain. Official percent-difference fields remain blank for incomplete comparisons. Separate `Preliminary...PercentDifference` fields retain the arithmetic percentage for internal review.

A charter cafeteria-manager base quantity of 0.73 may be displayed when satellite counts are missing. That row is explicitly marked `QuantityProvisional = TRUE` and `InputComplete = FALSE`, so it cannot make the model appear complete.

School-lunch building counts remain blank until every coded school in the LEA has a completed 0/1 lunch-program indicator. Missing indicators are never treated as zero.

## Input files

The package includes the input files used for the current draft under simplified names:

| New name | Prior name or source |
|---|---|
| `unit_count.xlsx` | Needs-Based Unit Enrollment Summary workbook |
| `student_counts.csv` | SQL export from Step 1 |
| `proposed_calculator.xlsm` | `Calculator for 25-26.xlsm` |
| `funding_rates.csv` | `shared_funding_rates.csv` |
| `lea_crosswalk.csv` | calculator LEA crosswalk |
| `entity_crosswalk.csv` | visible source-name exceptions used for joins |
| `current_rate_map.csv` | `old_model_rate_map.csv` |
| `current_school_supplement.csv` | old-model school supplemental inputs |
| `current_lea_supplement.csv` | old-model LEA supplemental inputs |

Replace the source files when the reporting year or approved model inputs change. Do not manually edit generated files in `data/output/`.

## Script crosswalk from the previous draft

| Previous draft | Transparent pipeline |
|---|---|
| `02_build_model_input.R` | `02_build_shared_input.R` |
| `03_calculate_old_model_positions.R` | `03_prepare_current_inputs.R` + `04_calculate_current_quantities.R` |
| `04_apply_old_model_rates.R` | `05_apply_current_rates.R` |
| `03_calculate_new_model_positions.R` | `06_prepare_proposed_inputs.R` + `07_calculate_proposed_quantities.R` |
| `04_apply_new_model_rates.R` | `08_apply_proposed_rates.R` |
| `05_compare_old_new_models.R` | `09_compare_models.R` |

## Numeric precision

The model retains full numeric precision from the source inputs through the funding calculations. It does not round positions, weighted counts, rates, or funding amounts unless a written business rule explicitly calls for an operation such as `floor()`. Rounding is used only when terminal review files are written.
