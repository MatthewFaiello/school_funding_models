# School Funding Model

> **Status: Work in progress**
>
> This repository contains a preliminary, reproducible pipeline for comparing Delaware's current school funding rules with the proposed PEFC model for school year 2025-26.
>
> The scripted workflow is stable, but several current-model inputs and funding analogues remain pending. Results should not be treated as final or official estimates until the remaining source information is incorporated and the full pipeline is rerun and validated.

## Purpose of the analysis

The primary analysis has two parts.

### 1. Position-based staffing comparison

> **How would the current and proposed staffing rules compare if the same dollar value were applied to comparable positions?**

This comparison uses:

- recreated current-model staffing quantities;
- independently reproduced proposed-model staffing quantities;
- the same PEFC rate for comparable position categories; and
- a shared funded-LEA scope.

Holding rates constant is meant to isolate differences in staffing rules, including ratios, weights, thresholds, eligibility, and calculation units. These common-rate estimates are comparison estimates, not confirmed current-system expenditures.

The pipeline presents three statewide views:

1. **Working known-current versus proposed comparison**  
   Uses all confirmed and provisional current amounts currently available and compares them with all proposed working categories. This view may include proposed funding for a category where the corresponding current amount is still missing.

2. **Working comparable-amount subtotal**  
   Includes confirmed and provisional categories only when both the current and proposed funding amounts are available.

3. **Confirmed subtotal**  
   Includes only categories with confirmed quantities, mappings, and rates on both sides.

Categories that cannot yet be estimated remain visible with missing values and a documented reason. Missing amounts are not replaced with zero.

Custodians, Cafeteria Managers, and Cafeteria Workers are funded outside the proposed position formula. They are excluded from the apples-to-apples position-based comparison and retained in separate audit and final documentation outputs.

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

The confirmed primary scope includes:

- 19 districts;
- 24 charters;
- 43 LEAs total; and
- Bryan Allen Stevenson School of Excellence (BASSE).

Delaware Air Force Base (DAFB) is excluded because it does not receive state funding. DAFB may remain in source-level or PEFC audit records only where needed to document the workbook scope discrepancy. The PEFC workbook includes DAFB in Base, Opportunity, and Operational Funding but excludes it from Central Office Funding.

Scope rules are maintained centrally in `scripts/00_settings.R` and applied throughout the pipeline.

## Pipeline overview

```text
01 Student-count SQL export
          ↓
02 Shared validated school and LEA input
          ↓
03-05 Recreated current staffing rules
      and outside-formula documentation
          ↓
06-08 Independently reproduced proposed rules
          ↓
09 Current-versus-proposed comparisons
   ├─ Position-based staffing comparison
   └─ Opportunity and Operational Funding comparison
          ↓
10 PEFC workbook reconciliation
          ↓
11 Final tables, assumptions, technical QC, and readiness
```

The pipeline is intentionally simple, flat, and linear. Each script has one clear purpose, and maintained assumptions are stored in input or settings files rather than repeated across scripts.

## Pipeline stages

- **Step 01: Student-count SQL export**  
  Manually extracts enrollment, low-income, active multilingual learner (MLL), K-8, and Grade 10 counts from the Unit Count warehouse table.

- **Step 02: Shared validated input**  
  Reconciles the SQL student counts to the September 30 Unit Count workbook and creates the shared school, LEA, and statewide universe used by both model branches.

- **Steps 03-05: Recreated current staffing rules**  
  Prepares the current-model inputs, calculates current units and positions, and applies the available common comparison rates. Missing quantities or rates remain missing rather than being treated as zero. The FY26 district cafeteria allocation and the current charter cafeteria formulas are retained separately as outside-formula documentation.

- **Steps 06-08: Independently reproduced proposed rules**  
  Prepares the proposed-model inputs, calculates proposed staffing and weighted-student quantities, and applies the proposed rates. DAFB source rows may be retained for audit but are excluded from aligned IV&V totals.

- **Step 09: Current-versus-proposed comparisons**  
  Produces:
  1. the position-based staffing comparison using common rates;
  2. the Opportunity and Operational Funding comparison; and
  3. supplementary working summaries that preserve confirmed and provisional calculations.

- **Step 10: PEFC workbook reconciliation**  
  Compares the independently reproduced proposed model with the PEFC workbook as presented. This step documents:
  - scope alignment and scope-treatment differences;
  - formula and input discrepancies;
  - statewide-summary reconciliation;
  - expected fixed-pool redistribution effects;
  - charter building treatment; and
  - the confirmed DAFB workbook scope discrepancy.

- **Step 11: Final outputs, assumptions, and QC**  
  Packages the report-ready comparison files, outside-formula documentation, maintained assumptions and open items, technical QC, and analytical readiness. Detailed calculations remain available in the intermediate and audit folders.

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

### 4. Confirm the cafeteria documentation source

The pipeline requires the source workbook:

```text
documentation/FY26 Cafeteria.xlsx
```

The machine-readable district allocation input derived from that workbook is:

```text
data/input/current_district_cafeteria_allocation.csv
```

The workbook remains the documentation source. The CSV is validated and carried through the pipeline for audit and reporting but is not added to position-based funding totals.

### 5. Review the maintained inputs and settings

Before running the pipeline, review:

```text
scripts/00_settings.R
data/input/current_rate_map.csv
data/input/model_comparison_crosswalk.csv
data/input/current_district_cafeteria_allocation.csv
```

The settings file contains shared paths, confirmed scope rules, model labels, outside-formula component definitions, and other values used throughout the pipeline.

The current rate map maintains calculator-supplied rates, documented functional crosswalks, analytical assumptions where still applicable, unavailable rates, and components confirmed as outside the formula.

The comparison crosswalk maintains each current-to-proposed category mapping and its mapping, quantity, rate, preliminary-inclusion, and final-inclusion status. Statuses such as `Not required` and `Not applicable` are used for confirmed outside-formula exclusions rather than treating those components as unresolved.

Also review the current Opportunity and Operational Funding input template in `data/input/`. It should remain incomplete until confirmed statewide and LEA-level allocations are available.

### 6. Run the full pipeline

Open and run:

```text
RUN_PIPELINE.R
```

This calls:

```r
source(file.path("scripts", "00_run_all.R"))
```

The runner:

- validates the required maintained inputs and cafeteria documentation source;
- clears stale generated CSV outputs;
- executes scripts `02` through `11` in order; and
- regenerates the run settings and MD5 manifest.

## Script guide

| Script | Purpose |
|---|---|
| `00_settings.R` | Defines shared paths, confirmed scope rules, labels, outside-formula categories, assumptions, and model settings. |
| `00_run_all.R` | Validates prerequisites, clears stale outputs, runs the R pipeline, and records the run manifest. |
| `01_student_counts.sql` | Creates the manually exported student-count input. |
| `02_build_shared_input.R` | Validates and reconciles the shared school and LEA input universe. |
| `03_prepare_current_inputs.R` | Prepares current staffing inputs and validates the district cafeteria allocation reference. |
| `04_calculate_current_quantities.R` | Calculates current-model position and unit quantities and creates the outside-formula audit output. |
| `05_apply_current_rates.R` | Applies common comparison rates to current quantities and enforces outside-formula exclusions. |
| `06_prepare_proposed_inputs.R` | Prepares school, charter, and LEA inputs for the proposed model. |
| `07_calculate_proposed_quantities.R` | Calculates proposed staffing and weighted-student quantities while retaining DAFB only for source audit. |
| `08_apply_proposed_rates.R` | Applies proposed Base, Opportunity, Operational, and Central Office rates to the aligned scope. |
| `09_compare_models.R` | Creates the position-based staffing and Opportunity/Operational comparisons. |
| `10_reconcile_pefc_workbook.R` | Reconciles the PEFC workbook with the independent proposed reproduction and documents DAFB scope treatment. |
| `11_create_final_outputs.R` | Creates report-ready comparisons, outside-formula documentation, assumptions, technical QC, and readiness outputs. |

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
├── documentation/             # Source documentation, including FY26 Cafeteria.xlsx
├── scripts/                   # SQL and R pipeline scripts
├── RUN_PIPELINE.R             # Main R entry point
└── README.md
```

## Final outputs

After a successful full rerun, the final folder contains 13 focused files:

```text
11_staffing_statewide_comparison.csv
11_staffing_component_comparison.csv
11_staffing_lea_comparison.csv
11_opportunity_operational_comparison.csv
11_opportunity_operational_lea_comparison.csv
11_pefc_reconciliation_summary.csv
11_charter_building_treatment.csv
11_current_outside_formula_components.csv
11_current_district_cafeteria_allocation.csv
11_pefc_dafb_scope_discrepancy.csv
11_open_items_and_assumptions.csv
11_final_qc.csv
11_final_readiness.csv
```

### Staffing outputs

- `11_staffing_statewide_comparison.csv` contains the working, comparable-amount, and confirmed statewide staffing views.
- `11_staffing_component_comparison.csv` retains every confirmed, provisional, and not-yet-estimable position-based category.
- `11_staffing_lea_comparison.csv` contains the LEA-level working staffing comparison.

Administrative Support Professionals and Instructional Supports enter the confirmed subtotal through externally confirmed functional crosswalks. These mappings support a common-rate comparison but do not imply identical current and proposed job definitions.

### Outside-formula outputs

- `11_current_outside_formula_components.csv` documents Custodians, charter Cafeteria Managers, charter Cafeteria Workers, and the district cafeteria allocation process without adding them to the position-based comparison.
- `11_current_district_cafeteria_allocation.csv` preserves the 19-district FY26 salary allocation derived from `documentation/FY26 Cafeteria.xlsx`.

The current charter reference rules retained for documentation are:

- Cafeteria Workers: 0.62 per 100 students.
- Cafeteria Managers: 0.73 per charter plus 0.73 per qualifying satellite cafeteria.
- ASPIRA and MOT are the two charters currently identified as receiving the satellite addition.

District cafeteria support is a separate salary-allocation process based on meals, operating days, requested hours, salary amounts, state salary shares, and termination pay. It is not treated as a district cafeteria position formula.

### Opportunity and Operational outputs

- `11_opportunity_operational_comparison.csv` contains the statewide comparison.
- `11_opportunity_operational_lea_comparison.csv` contains the LEA-level allocations.

When current allocations have not been provided, current amounts remain blank rather than being reported as zero.

### PEFC reconciliation outputs

- `11_pefc_reconciliation_summary.csv` contains the report-level scope, formula, statewide-summary, and redistribution findings.
- `11_charter_building_treatment.csv` documents how PEFC charter building rows are used in Base staffing calculations and how official charter totals are retained for Opportunity and Operational Funding to avoid duplicated student counts.
- `11_pefc_dafb_scope_discrepancy.csv` documents the PEFC workbook's inconsistent treatment of DAFB by funding section.

Detailed PEFC component comparisons remain in the audit folder rather than being repeated in the final summary.

### QC and readiness

- `11_final_qc.csv` answers whether the pipeline ran correctly. All integrity checks must pass.
- `11_final_readiness.csv` answers whether the analysis is complete enough to finalize. Outstanding source, policy, quantity, or funding-analogue questions appear here without being treated as technical pipeline failures.

## Outputs and quality checks

The pipeline separates generated files by purpose:

- `data/output/intermediate/` contains detailed model calculations;
- `data/output/audit/` contains QC and reconciliation files; and
- `data/output/final/` contains the limited set of files intended to support the final analysis.

The pipeline may stop when it detects issues such as:

- duplicate records or calculation keys;
- failed school, charter, or LEA matches;
- missing required source inputs;
- excluded LEAs entering aligned totals;
- outside-formula components reentering the position-based comparison;
- differences between expected and observed totals;
- fixed funding pools that do not reconcile; or
- failures between pipeline stages.

After each run:

1. review the console messages;
2. confirm that every integrity row in `11_final_qc.csv` passes; and
3. review `11_final_readiness.csv` and `11_open_items_and_assumptions.csv` for items that still require confirmation.

## Confirmed implementation decisions

The current pipeline uses the following confirmed rules:

- district schools generally use official school codes as proposed Base calculation units;
- charter organizations may use multiple PEFC building rows for school-based Base calculations;
- additional PEFC charter building rows do not create additional LEAs or official school codes;
- official charter organization totals are retained for Opportunity and Operational weighted funding to avoid duplicating students across buildings;
- whole-student charter categories are allocated across calculation units using largest remainder, with ties resolved by calculation-unit sequence;
- BASSE is included in the primary comparison scope;
- DAFB is excluded because it does not receive state funding;
- common PEFC rates are applied to comparable current and proposed staffing quantities;
- Secretary maps functionally to Administrative Support Professionals at the PEFC rate;
- Counselor/Social Worker, School Psychologist, and Visiting Teacher map functionally to Instructional Supports at the PEFC rate;
- proposed Instructional Supports are calculated at 20% of Base Division I;
- Custodians, Cafeteria Managers, and Cafeteria Workers are excluded from the position-based comparison because they are funded outside the proposed formula;
- fixed statewide Opportunity and Operational pools are retained and per-weighted-student rates are recalculated from eligible weighted counts;
- confirmed and provisional calculations are carried into the working comparison;
- not-yet-estimable categories remain visible with missing values;
- missing funding amounts are not replaced with zero;
- technical QC is separated from analytical readiness; and
- the PEFC workbook remains unchanged and is used as a separate validation source.

See the maintained crosswalks, `11_open_items_and_assumptions.csv`, and the audit outputs for detailed rationale and known limitations.

## Remaining substantive items

### Current staffing quantities

- **Buildings and Grounds Supervisor:** obtain Tamara's current custodial-count spreadsheet. The implemented rule is one position at 95 custodial units, with 12 custodial units equal to one custodian.
- **Food Services Supervisor:** obtain Michele Rush's current calculation or allocation information. The implemented rule is one position at 500 Division I units, or below 500 units with at least four lunch-program buildings.

### Opportunity and Operational Funding

Obtain OMB and CGO guidance on:

- the current funding streams that should be treated as analogues to proposed Opportunity Funding;
- the current funding streams that should be treated as analogues to proposed Operational Funding;
- confirmed statewide amounts;
- LEA-level allocation files; and
- which current appropriations would be replaced, consolidated, or remain separate.

### Maintained implementation assumptions and policy interpretations

The final assumptions file also tracks implementation choices that may not be blockers but must remain documented, including:

- fractional-position rules;
- current-model nurse calculations;
- the proposed Assistant Superintendent interpretation;
- charter building treatment; and
- the exclusion of optional PEFC calculations outside the maintained proposed model.

## Steps needed before finalization

The pipeline structure should remain stable. As the remaining information arrives:

1. Update the maintained input or crosswalk file that owns the decision.
2. Revise only the affected script when calculation logic must change.
3. Preserve missing values for unresolved inputs rather than entering zero.
4. Run the full pipeline from `RUN_PIPELINE.R`.
5. Confirm that all integrity checks in `11_final_qc.csv` pass.
6. Review how the working, comparable-amount, and confirmed staffing totals changed.
7. Confirm that current and proposed Opportunity and Operational comparisons are complete when the current analogues arrive.
8. Review `11_final_readiness.csv` and `11_open_items_and_assumptions.csv` for any remaining limitations.
9. Freeze the final settings, inputs, crosswalks, outputs, source documentation, and README used for the completed analysis.

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
- Retain outside-formula components in documentation outputs rather than the position-based funding comparison.
- Treat all results and comparisons as preliminary until the required source data, policy decisions, and validation steps are complete.
