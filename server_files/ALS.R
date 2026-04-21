# Refactored loader for ALS.R
# Original file split into semantic modules to keep files under 600 lines.

source("server_files/ALS_parts/als_core_algorithms.R", local = TRUE)
source("server_files/ALS_parts/als_iteration_and_external_spectra.R", local = TRUE)
source("server_files/ALS_parts/als_constraints_and_runner.R", local = TRUE)
source("server_files/ALS_parts/als_outputs_and_exports.R", local = TRUE)
source("server_files/ALS_parts/als_ambiguity_workflow.R", local = TRUE)
source("server_files/ALS_parts/als_ambiguity_downloads.R", local = TRUE)
