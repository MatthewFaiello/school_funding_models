# Open school_funding_model.Rproj, then run this file from the project root.
# Required source files must already be present, including:
#   data/input/student_counts.csv
#   documentation/FY26 Cafeteria.xlsx
# The pipeline validates maintained inputs, clears stale generated CSV outputs,
# runs scripts 02 through 11, and writes run settings plus an MD5 manifest.

source(file.path("scripts", "00_run_all.R"))
