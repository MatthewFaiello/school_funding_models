# School Funding Model

> **Status: Work in progress**
>
> This repository contains a preliminary, reproducible pipeline for comparing Delaware's current school funding rules with the proposed PEFC model for school year 2025–26.
>
> The pipeline structure is now aligned with the questions the final analysis needs to answer. Results remain preliminary and should not be treated as final or official estimates until the outstanding inputs, mappings, rates, funding analogues, and scope decisions are confirmed.

## Purpose of the analysis

The primary analysis has two parts.

### 1. Staffing-rule comparison

> **How would the current and proposed staffing rules compare if the same dollar value were applied to comparable positions?**

This comparison uses:

- recreated current-model staffing quantities;
- independently reproduced proposed-model staffing quantities;
- the same PEFC rate for comparable position categories; and
- a shared LEA scope.

Holding rates constant is meant to isolate differences in staffing rules, including ratios, weights, thresholds, eligibility, and calculation units. These common-rate estimates are comparison estimates, not confirmed current-system expenditures.

The pipeline carries all available calculations into the final outputs, including provisional estimates. It presents three statewide views:

1. **Working known-current versus proposed comparison**  
   Uses all confirmed and provisional current amounts currently available and compares them with all proposed working categories. This view may include proposed funding for a category where the corresponding current amount is still missing.

2. **Working comparable-amount subtotal**  
   Includes confirmed and provisional categories only when both the current and proposed funding amounts are available.

3. **Confirmed subtotal**  
   Includes only categories with confirmed quantities, mappings, and rates on both sides.

Categories that cannot yet be estimated remain visible with missing values and a documented reason. Missing amounts are not replaced with zero.

### 2. Opportunity and Operational Funding comparison

This comparison examines the current and proposed approaches to non-position funding, including:

- purpose;
- eligibility;
- inputs;
- allocation method;
- statewide amount; and
- LEA-level distribution.

These funding streams are compared separately from staffing because they are not position-based formulas.

Until current analogues and allocations are confirmed, the proposed Opportunity and Operational calculations continue to pass through the pipeline, while unavailable current amounts remain blank and are marked as not yet estimable.

### Supporting PEFC workbook review

The PEFC workbook is not part of the primary current-versus-proposed comparison. It is reviewed separately against the independently reproduced proposed model to identify:

- scope and scope-treatment differences;
- input or formula discrepancies;
- statewide-summary discrepancies;
- expected redistribution effects; and
- the treatment of PEFC charter building rows.

The workbook itself is not modified by the pipeline.

## Current comparison scope

The maintained preliminary scope includes:

- 19 districts;
- 24 charters;
- 43 LEAs total; and
- Bryan Allen Stevenson School of Excellence (BASSE).

Delaware Air Force Base (DAFB) remains excluded pending confirmation of how it should be treated under the current model and in the final statewide comparison.

Scope rules are maintained in one place and applied consistently throughout the pipeline.

## Pipeline overview

```text
01 Student-count SQL export
          ↓
02 Shared validated school and LEA input
          ↓
03–05 Recreated current staffing rules
          ↓
06–08 Independently reproduced proposed rules
          ↓
09 Current-versus-proposed comparisons
   ├─ Staffing-rule comparison
   └─ Opportunity and Operational Funding comparison
          ↓
10 PEFC workbook reconciliation
          ↓
11 Final tables, assumptions, technical QC, and readiness
```

The pipeline is intentionally simple, flat, and linear. Each script has one clear purpose, and maintained assumptions are stored in input or settings files rather than repeated across scripts.

## Pipeline stages

- **Step 01: Student-count SQL export**  
  Manually extracts enrollment, low-income, active multilingual learner (MLL), K–8, and Grade 10 counts from the unit-count warehouse table.

- **Step 02: Shared validated input**  
  Reconciles the SQL student counts to the September 30 unit-count workbook and creates the shared school, LEA, and statewide universe used by both model branches.

- **Steps 03–05: Recreated current staffing rules**  
  Prepares the current-model inputs, calculates current units and positions, and applies the available common comparison rates. Missing quantities or rates remain missing rather than being treated as zero.

- **Steps 06–08: Independently reproduced proposed rules**  
  Prepares the proposed-model inputs, calculates proposed staffing and weighted-student quantities, and applies the proposed rates.

- **Step 09: Current-versus-proposed comparisons**  
  Produces:
  1. the staffing-rule comparison using common rates;
  2. the Opportunity and Operational Funding comparison; and
  3. supplementary working summaries that preserve confirmed and provisional calculations.

- **Step 10: PEFC workbook reconciliation**  
  Compares the independently reproduced proposed model with the PEFC workbook as presented. This step documents:
  - scope alignment and scope-treatment differences;
  - formula and input discrepancies;
  - statewide-summary reconciliation;
  - expected fixed-pool redistribution effects; and
  - charter building treatment.

- **Step 11: Final outputs, assumptions, and QC**  
  Packages the report-ready comparison files, the maintained assumptions and open items, technical QC, and analytical readiness. Detailed calculations remain available in the intermediate and audit folders.

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

### 4. Review the maintained inputs and settings

Before running the pipeline, review:

```text
scripts/00_settings.R
data/input/model_comparison_crosswalk.csv
```

The settings file contains shared paths, scope rules, model labels, and other values used throughout the pipeline.

The comparison crosswalk contains the current-to-proposed category mappings and identifies each item as:

- `Confirmed`
- `Provisional`
- `Missing`

Update the crosswalk when an outstanding item is resolved rather than hardcoding the same decision in multiple scripts.

Also review the current Opportunity and Operational Funding input template in `data/input/`. It should remain incomplete until confirmed statewide and LEA-level allocations are available.

### 5. Run the full pipeline

Open and run:

```text
RUN_PIPELINE.R
```

This calls:

```r
source(file.path("scripts", "00_run_all.R"))
```

The runner executes the R scripts in order.

## Script guide

| Script | Purpose |
|---|---|
| `00_settings.R` | Defines shared paths, scope rules, labels, assumptions, and model settings. |
| `00_run_all.R` | Runs the R portion of the pipeline in sequence. |
| `01_student_counts.sql` | Creates the manually exported student-count input. |
| `02_build_shared_input.R` | Validates and reconciles the shared school and LEA input universe. |
| `03_prepare_current_inputs.R` | Prepares source data used to recreate the current staffing rules. |
| `04_calculate_current_quantities.R` | Calculates current-model position and unit quantities. |
| `05_apply_current_rates.R` | Applies the available common comparison rates to current quantities. |
| `06_prepare_proposed_inputs.R` | Prepares school, charter, and LEA inputs for the proposed model. |
| `07_calculate_proposed_quantities.R` | Calculates proposed staffing and weighted-student quantities. |
| `08_apply_proposed_rates.R` | Applies proposed Base, Opportunity, Operational, and Central Office rates. |
| `09_compare_models.R` | Creates the staffing-rule and Opportunity/Operational comparisons. |
| `10_reconcile_pefc_workbook.R` | Reconciles the PEFC workbook with the independent proposed reproduction. |
| `11_create_final_outputs.R` | Creates report-ready comparisons, assumptions, technical QC, and readiness outputs. |

## Project folders

```text
school_funding_model/
├── data/
│   ├── input/                 # Source files, maintained crosswalks, and manual templates
│   └── output/
│       ├── intermediate/      # Detailed calculation outputs from the model steps
│       ├── audit/             # QC, reconciliation, discrepancy, and run-tracking files
│       └── final/             # Report-ready comparison and readiness files
├── deliverables/              # Draft reports and other deliverables
├── documentation/             # Source documentation and working review materials
├── scripts/                   # SQL and R pipeline scripts
├── RUN_PIPELINE.R             # Main R entry point
└── README.md
```

## Final outputs

The final folder contains a focused set of files:

```text
11_staffing_statewide_comparison.csv
11_staffing_component_comparison.csv
11_staffing_lea_comparison.csv
11_weighted_funding_comparison.csv
11_weighted_funding_lea_comparison.csv
11_pefc_reconciliation_summary.csv
11_charter_building_treatment.csv
11_open_items_and_assumptions.csv
11_final_qc.csv
11_final_readiness.csv
```

### Staffing outputs

- `11_staffing_statewide_comparison.csv` contains the three statewide staffing views.
- `11_staffing_component_comparison.csv` retains every confirmed, provisional, and not-yet-estimable staffing category.
- `11_staffing_lea_comparison.csv` contains the LEA-level working staffing comparison.

Cafeteria Managers, Cafeteria Workers, and Custodians remain separate because they rely on different quantities, rates, and outstanding confirmations.

### Opportunity and Operational outputs

- `11_weighted_funding_comparison.csv` contains the statewide comparison.
- `11_weighted_funding_lea_comparison.csv` contains the LEA-level allocations.

When current allocations have not been provided, current amounts remain blank rather than being reported as zero.

### PEFC reconciliation outputs

- `11_pefc_reconciliation_summary.csv` contains the report-level scope, formula, statewide-summary, and redistribution findings.
- `11_charter_building_treatment.csv` documents how PEFC charter building rows are used in Base staffing calculations and how official charter totals are retained for Opportunity and Operational Funding to avoid duplicated student counts.

Detailed PEFC component comparisons remain in the audit folder rather than being repeated in the final summary.

### QC and readiness

- `11_final_qc.csv` answers whether the pipeline ran correctly. All technical checks should pass.
- `11_final_readiness.csv` answers whether the analysis is complete enough to finalize. Outstanding policy, quantity, rate, mapping, or scope questions appear here without being treated as technical pipeline failures.

## Outputs and quality checks

The pipeline separates generated files by purpose:

- `data/output/intermediate/` contains detailed model calculations;
- `data/output/audit/` contains QC and reconciliation files; and
- `data/output/final/` contains the limited set of files intended to support the final analysis.

The pipeline may stop when it detects issues such as:

- duplicate records or calculation keys;
- failed school, charter, or LEA matches;
- missing required source inputs;
- differences between expected and observed totals;
- fixed funding pools that do not reconcile; or
- failures between pipeline stages.

After each run:

1. review the console messages;
2. confirm that every row in `11_final_qc.csv` passes; and
3. review `11_final_readiness.csv` and `11_open_items_and_assumptions.csv` for items that still require confirmation.

## Important working assumptions

The current pipeline uses the following working rules:

- district schools generally use official school codes as proposed Base calculation units;
- charter organizations may use multiple PEFC building rows for school-based Base calculations;
- additional PEFC charter building rows do not create additional LEAs;
- official charter organization totals are retained for Opportunity and Operational weighted funding to avoid duplicating students across buildings;
- BASSE is included in the primary comparison scope;
- DAFB is excluded pending confirmation;
- common PEFC rates are applied to comparable current and proposed staffing quantities;
- confirmed and provisional calculations are carried into the working comparison;
- not-yet-estimable categories remain visible with missing values;
- missing funding amounts are not replaced with zero;
- technical QC is separated from analytical readiness; and
- the PEFC workbook remains unchanged and is used as a separate validation source.

See the comparison crosswalk, `11_open_items_and_assumptions.csv`, and the audit outputs for the detailed rationale and known limitations.

## Outstanding items

The following items still need confirmation before the analysis can be finalized.

### Current staffing quantities and processes

- Custodian site-evaluation process and school- or LEA-level funded positions
- Custodial units needed to calculate Buildings and Grounds Supervisors
- Lunch-program building counts for Food Services Supervisors
- Charter satellite cafeteria counts for Cafeteria Managers
- District cafeteria funding process and LEA-level allocations

### Common comparison rates and mappings

- Confirmation that Secretary maps to Administrative Support Professional
- Confirmation that Counselor/Social Worker, School Psychologist, and Visiting Teacher map to Instructional Supports
- Rates for Custodians
- Rates for Cafeteria Managers
- Rates for Cafeteria Workers

### Opportunity and Operational Funding

- Current funding streams that should be treated as analogues to proposed Opportunity Funding
- Current funding streams that should be treated as analogues to proposed Operational Funding
- Confirmed statewide amounts
- LEA-level allocation files
- Confirmation of which current appropriations would be replaced, consolidated, or remain separate

### Scope

- Final treatment of DAFB under the current model
- Whether DAFB should be included in the final statewide comparison

### Maintained implementation assumptions

The final assumptions file also tracks implementation choices that may not be blockers but must be documented, including:

- fractional-position rules;
- current-model nurse calculations;
- the proposed Assistant Superintendent interpretation;
- charter building treatment; and
- the exclusion of optional PEFC calculations that are outside the maintained proposed model.

## Steps needed before finalization

The pipeline structure should remain stable. Once the outstanding items are confirmed:

1. Update the maintained crosswalk and manual input files.
2. Add the confirmed current quantities, rates, and Opportunity/Operational allocations.
3. Resolve the DAFB scope decision in `00_settings.R`.
4. Update affected statuses from `Provisional` or `Missing` to `Confirmed`.
5. Re-run the full pipeline.
6. Confirm that all technical QC checks pass.
7. Review how the working, comparable-amount, and confirmed staffing totals changed.
8. Confirm that current and proposed Opportunity and Operational comparisons are complete.
9. Remove temporary provisional notes that no longer apply.
10. Freeze the final settings, inputs, crosswalk, outputs, documentation, and README used for the completed analysis.

The remaining work should be limited to updating confirmed inputs and decisions. The pipeline should not require another structural redesign.

## Working rules

- Keep the code simple, flat, and linear.
- Do not build reusable systems for scenarios this pipeline does not need to support.
- Do not manually edit generated files in `data/output/`.
- Make source-data corrections in `data/input/` or the relevant preparation script.
- Maintain shared scope rules and model labels in `00_settings.R`.
- Maintain comparison mappings and statuses in the comparison crosswalk.
- Re-run the full pipeline after changing inputs, assumptions, allocation logic, scope, or rates.
- Carry confirmed and provisional calculations into the working outputs.
- Keep unavailable quantities and funding amounts blank.
- Do not replace missing amounts with zero.
- Treat all results and comparisons as preliminary until the required source data, policy decisions, rates, and validation steps are complete.
