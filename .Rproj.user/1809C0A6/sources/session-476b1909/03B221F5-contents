# School Funding Model

> **Status: Work in progress**
>
> This repository contains a preliminary, reproducible pipeline for reviewing and comparing Delaware's current unit-count funding model with the proposed hybrid funding model for school year 2025–26.
>
> Everything in the repository remains under development, including the source inputs, assumptions, funding rates, calculations, quality checks, outputs, and documentation. Results should not be treated as final or official estimates.

## Pipeline overview

```text
01 Student-count SQL export
          ↓
02 Validated shared school/LEA input
          ↓
   ┌──────┴────────┐
   ↓               ↓
03–05 Current      06–08 Proposed
model branch       model branch
   └──────┬────────┘
          ↓
09 Current/proposed comparison
          ↓
10 Adjusted reporting and exclusions
```

The pipeline is organized into the following stages:

- **Step 01: Student-count SQL export**  
  Manually extracts enrollment, low-income, active multilingual learner (MLL), K–8, and Grade 10 counts from the unit-count warehouse table.

- **Step 02: Shared validated input**  
  Reconciles the SQL student counts to the September 30 unit-count workbook and creates the shared school, LEA, and statewide universe used by both model branches.

- **Steps 03–05: Current-model branch**  
  Recreates the current unit-based system, including reported Division I positions and calculated school- and central-office positions, and then applies the available funding rates.

- **Steps 06–08: Proposed-model branch**  
  Prepares the proposed-model inputs, calculates position and weighted-student quantities, and funds the proposed Base, Opportunity, Operational, and Central Office sections.

- **Step 09: Model comparison**  
  Joins the current and proposed model results using controlled keys. Official percentage comparisons are suppressed when either side of the comparison is incomplete.

- **Step 10: Adjusted reporting and exclusions**  
  Produces preliminary reporting outputs after excluding DAFB and Bryan Allen Stevenson School of Excellence from the adjusted statewide scope and redistributing the two fixed weighted-funding pools.

## Run the model

### 1. Open the R project

Open:

```text
school_funding_model.Rproj
```

### 2. Install the required packages

Run once in R:

```r
install.packages(c("tidyverse", "readxl"))
```

### 3. Create the student-count input

The SQL extract is not run automatically by the R pipeline.

Run:

```text
scripts/01_student_counts.sql
```

Then save the query results as:

```text
data/input/student_counts.csv
```

### 4. Review the model settings

Before running the pipeline, review:

```text
scripts/00_settings.R
```

This file contains shared model settings, assumptions, paths, exclusions, and other values used across the pipeline.

### 5. Run the full pipeline

Open and run:

```text
RUN_PIPELINE.R
```

This calls:

```r
source(file.path("scripts", "00_run_all.R"))
```

The runner executes scripts `02` through `10` in order.

## Script guide

| Script | Purpose |
|---|---|
| `00_settings.R` | Defines shared paths, assumptions, and model settings. |
| `00_run_all.R` | Runs the R portion of the pipeline in sequence. |
| `01_student_counts.sql` | Creates the manually exported student-count input. |
| `02_build_shared_input.R` | Validates and reconciles the shared school and LEA input universe. |
| `03_prepare_current_inputs.R` | Prepares source data used to recreate the current model. |
| `04_calculate_current_quantities.R` | Calculates current-model position and unit quantities. |
| `05_apply_current_rates.R` | Applies the available current-model funding rates. |
| `06_prepare_proposed_inputs.R` | Prepares school, charter, and LEA inputs for the proposed model. |
| `07_calculate_proposed_quantities.R` | Calculates proposed positions and weighted-student quantities. |
| `08_apply_proposed_rates.R` | Applies the proposed Base, Opportunity, Operational, and Central Office rates. |
| `09_compare_models.R` | Builds controlled current-versus-proposed comparisons. |
| `10_reporting_analysis.R` | Creates adjusted statewide, LEA, and review outputs. |

## Project folders

```text
school_funding_model/
├── data/
│   ├── input/          # Source files used by the pipeline
│   └── output/         # Generated model, QC, and reporting files
├── deliverables/       # Draft deliverables created from the analysis
├── documentation/      # Source documentation and working review materials
├── scripts/            # SQL and R pipeline scripts
├── RUN_PIPELINE.R      # Main R entry point
└── README.md
```

## Outputs and quality checks

Generated files are written to:

```text
data/output/
```

The pipeline is intentionally audit-oriented. In addition to model-ready outputs, it creates reconciliation and review files that document how records and totals move through the process.

The pipeline may stop when it detects issues such as:

- duplicate records or calculation keys;
- failed school, charter, or LEA matches;
- missing required source inputs;
- differences between expected and observed totals; or
- reconciliation failures between pipeline stages.

After each run, review both the console messages and the QC files in `data/output/`.

## Important working assumptions

Several implementation choices are still being reviewed and should be interpreted as working assumptions rather than finalized policy decisions. In the current pipeline:

- district schools generally use official school codes as proposed Base calculation units;
- charter organizations may use multiple calculator building rows for school-based Base calculations;
- official charter organization totals are retained for Opportunity and Operational weighted funding to avoid duplicating students across buildings;
- official comparison percentages remain blank when either the current or proposed amount is incomplete; and
- specified reporting exclusions are applied only in the adjusted reporting stage rather than silently removing records earlier in the pipeline.

See the files in `documentation/` and the generated QC outputs for the detailed rationale, known limitations, unresolved questions, and technical implementation notes.

## Working rules

- Do not manually edit generated files in `data/output/`.
- Make source-data corrections in `data/input/` or in the relevant preparation script.
- Document changes to model assumptions in `scripts/00_settings.R` and the supporting technical documentation.
- Re-run the full pipeline after changing inputs, assumptions, allocation logic, or rates.
- Treat all results and comparisons as preliminary until the required source data, policy decisions, rates, and validation steps are complete.
