#!/usr/bin/env Rscript

resolve_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
  }

  script_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(...) NULL)
  if (!is.null(script_file)) {
    return(normalizePath(script_file, winslash = "/", mustWork = FALSE))
  }

  normalizePath(file.path("scripts", "relock_r_skana.R"), winslash = "/", mustWork = FALSE)
}

script_path <- resolve_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
dependency_script <- file.path(dirname(script_path), "_dependencies.R")
renv_root <- file.path(repo_root, ".R_skana", "renv")

Sys.setenv(
  SK_ANA_PROJECT_ROOT = repo_root,
  SK_ANA_RENV_ROOT = renv_root,
  RENV_PATHS_ROOT = renv_root,
  RENV_PATHS_LIBRARY = file.path(renv_root, "library"),
  RENV_PATHS_CACHE = file.path(renv_root, "cache"),
  RENV_PATHS_SANDBOX = file.path(renv_root, "sandbox"),
  RENV_CONFIG_SANDBOX_ENABLED = "FALSE",
  RENV_CONFIG_SHIMS_ENABLED = "FALSE"
)
options(renv.consent = TRUE)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

setwd(repo_root)
renv::load(project = repo_root)
source(dependency_script, local = TRUE)

ensure_sk_ana_dependencies()
renv::snapshot(project = repo_root, prompt = FALSE)

message("renv.lock updated for the current R_skana environment.")
