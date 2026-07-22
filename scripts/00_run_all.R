# =============================================================================
# 00_run_all.R
# =============================================================================
# Runs every R step in order. Run the SQL file first and export student_counts.csv.
# Creates a run settings file and a run-specific file manifest for audit.
# =============================================================================

source(file.path("scripts", "00_settings.R"))

# Remove prior generated CSVs so a failed run cannot leave stale later-step
# outputs that look current. Source and maintained files are in data/input/ and
# are never removed here.
prior_outputs <- list.files(
  output_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(prior_outputs) > 0) {
  removed_outputs <- file.remove(prior_outputs)

  if (!all(removed_outputs)) {
    stop(
      "One or more prior output files could not be removed.",
      call. = FALSE
    )
  }
}

run_settings_path <- file.path(output_dir, "00_run_settings.csv")
run_manifest_path <- file.path(output_dir, "00_run_manifest.csv")
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
      "DAFB district code",
      "Operational enrollment basis",
      "Weighted rate method",
      "Opportunity funding pool",
      "Operational funding pool",
      "Charter student allocation method",
      "Charter building policy"
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
      as.character(dafb_district_code),
      operational_enrollment_basis,
      weighted_rate_method,
      as.character(opportunity_funding_pool),
      as.character(operational_funding_pool),
      charter_student_allocation_method,
      charter_building_policy
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
  input_files <- list.files(
    input_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  script_files <- list.files(
    scripts_dir,
    pattern = "\\.(R|sql)$",
    full.names = TRUE
  )
  output_files <- list.files(
    output_dir,
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
  "10_reporting_analysis.R"
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

    message("\nPipeline complete. Outputs are in: ", output_dir)
    message("Review run manifest: ", run_manifest_path)
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
