# School Funding Model

> **Status: Work in progress**
>
> Everything in this repository is still under development, including the scripts, inputs, assumptions, funding rates, calculations, quality checks, outputs, and documentation. Results should be treated as preliminary and should not be used as final or official estimates.

## Run the model

1. Open `school_funding_model.Rproj`.

2. Install the required R packages once:

```r
install.packages(c("tidyverse", "readxl"))
```

3. Confirm that the required input files are available in `data/input/`.

   In particular, `student_counts.csv` must already exist. To recreate it, run:

```text
scripts/01_student_counts.sql
```

   Then save the SQL results as:

```text
data/input/student_counts.csv
```

4. Review the model settings in:

```text
scripts/00_settings.R
```

5. Run the full pipeline by opening and running:

```text
RUN_PIPELINE.R
```

The file runs:

```r
source(file.path("scripts", "00_run_all.R"))
```

## What the pipeline does

The pipeline runs scripts `02` through `10` in order. It prepares the shared inputs, calculates the current and proposed models, applies rates, compares the results, and creates preliminary reporting outputs.

Generated files are written to:

```text
data/output/
```

The SQL script is not run automatically.

## Important notes

- Do not manually edit files in `data/output/`.
- The pipeline may stop when it finds duplicate records, failed matches, missing inputs, or reconciliation issues.
- Review the console messages and the QC files in `data/output/` after each run.
- All model results and comparisons remain preliminary while the project is in progress.
