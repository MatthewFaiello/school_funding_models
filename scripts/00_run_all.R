# =============================================================================
# 00_run_all.R
# =============================================================================
# Run the SQL file first and export student_counts.csv, then source this script.
# Steps run in one linear sequence. Calculation detail is retained in
# intermediate/, report-ready files go to final/, and QC goes to audit/.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

# Remove prior generated CSVs so a failed run cannot leave stale results.
prior_outputs <- list.files(
  output_root_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(prior_outputs) > 0 && !all(file.remove(prior_outputs))) {
  stop("One or more prior output files could not be removed.", call. = FALSE)
}

run_settings_path <- file.path(audit_dir, "00_run_settings.csv")
run_manifest_path <- file.path(audit_dir, "00_run_manifest.csv")
run_started_time <- Sys.time()
run_completed_time <- as.POSIXct(NA)
run_status <- "Started"
run_error <- ""

write_run_settings <- function() {
  run_settings <- tibble(
    Setting = c(
      "Run started",
      "Run completed",
      "Run status",
      "Error message",
      "R version",
      "tidyverse version",
      "readxl version",
      "School year",
      "Count date",
      "Current model label",
      "Proposed model label",
      "PEFC model label",
      "Primary reporting scope",
      "Primary excluded LEA codes",
      "Operational enrollment basis",
      "Weighted rate method",
      "Opportunity funding pool",
      "Operational funding pool"
    ),
    Value = c(
      format(run_started_time, "%Y-%m-%d %H:%M:%S %Z"),
      if_else(
        is.na(run_completed_time),
        "",
        format(run_completed_time, "%Y-%m-%d %H:%M:%S %Z")
      ),
      run_status,
      run_error,
      R.version.string,
      as.character(packageVersion("tidyverse")),
      as.character(packageVersion("readxl")),
      as.character(school_year),
      as.character(count_date),
      current_model_label,
      proposed_model_label,
      pefc_model_label,
      primary_reporting_scope_label,
      paste(primary_reporting_excluded_lea_codes, collapse = ","),
      operational_enrollment_basis,
      weighted_rate_method,
      as.character(opportunity_funding_pool),
      as.character(operational_funding_pool)
    )
  )

  write_review_csv(run_settings, run_settings_path)
}

file_row_count <- function(path) {
  if (!str_detect(path, "\\.csv$")) {
    return(NA_integer_)
  }

  max(length(readLines(path, warn = FALSE)) - 1L, 0L)
}

write_run_manifest <- function() {
  input_files <- list.files(input_dir, recursive = TRUE, full.names = TRUE)
  script_files <- list.files(
    scripts_dir,
    pattern = "\\.(R|sql)$",
    full.names = TRUE
  )
  output_files <- list.files(
    output_root_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  manifest_paths <- c(input_files, script_files, output_files)
  manifest_types <- c(
    rep("Maintained input", length(input_files)),
    rep("Script", length(script_files)),
    rep("Generated output", length(output_files))
  )

  run_manifest <- tibble(
    RunStatus = run_status,
    FileType = manifest_types,
    RelativePath = str_remove(
      normalizePath(manifest_paths, winslash = "/", mustWork = FALSE),
      fixed(paste0(project_dir, "/"))
    ),
    FileSizeBytes = file.info(manifest_paths)$size,
    RowCount = map_int(manifest_paths, file_row_count),
    MD5 = unname(tools::md5sum(manifest_paths))
  ) |>
    arrange(FileType, RelativePath)

  write_review_csv(run_manifest, run_manifest_path)
}

write_run_settings()

pipeline_scripts <- c(
  "02_build_shared_input.R",
  "03_prepare_current_inputs.R",
  "04_calculate_current_quantities.R",
  "05_apply_current_rates.R",
  "06_prepare_proposed_inputs.R",
  "07_calculate_proposed_quantities.R",
  "08_apply_proposed_rates.R",
  "09_compare_models.R",
  "10_reconcile_pefc_workbook.R",
  "11_create_final_outputs.R"
)

tryCatch(
  {
    for (script_name in pipeline_scripts) {
      message("\n=== Running ", script_name, " ===")
      source(file.path(scripts_dir, script_name))
    }

    run_status <- "Completed"
    run_completed_time <- Sys.time()
    write_run_settings()
    write_run_manifest()

    message("\nPipeline complete.")
    message("Final outputs: ", final_dir)
    message("Audit outputs: ", audit_dir)
  },
  error = function(error_condition) {
    run_status <<- "Failed"
    run_completed_time <<- Sys.time()
    run_error <<- conditionMessage(error_condition)

    write_run_settings()
    write_run_manifest()

    stop(error_condition)
  }
)
