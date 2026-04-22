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

  normalizePath(file.path("scripts", "setup_r_skana.R"), winslash = "/", mustWork = FALSE)
}

repo_root_from_script <- function(script_path) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, repos = "https://cloud.r-project.org")
  }
}

lockfile_supports_current_runtime <- function(lockfile, minimum_r = "4.2.0") {
  if (!file.exists(lockfile)) {
    return(FALSE)
  }

  contents <- tryCatch(paste(readLines(lockfile, warn = FALSE), collapse = "\n"), error = function(...) "")
  match <- regexec('"Version"\\s*:\\s*"([^"]+)"', contents)
  groups <- regmatches(contents, match)[[1]]
  if (length(groups) < 2) {
    return(FALSE)
  }

  version <- groups[2]
  if (!is.character(version) || !nzchar(version) || is.na(version)) {
    return(FALSE)
  }

  package_version(version) >= package_version(minimum_r)
}

script_path <- resolve_script_path()
repo_root <- repo_root_from_script(script_path)
dependency_script <- file.path(dirname(script_path), "_dependencies.R")
env_file <- file.path(dirname(script_path), "r_skana.environment.yml")
stamp_dir <- file.path(repo_root, ".R_skana")
stamp_file <- file.path(stamp_dir, "setup.stamp")
lockfile <- file.path(repo_root, "renv.lock")

ensure_dir(stamp_dir)
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

install_if_missing("renv")
source(dependency_script, local = TRUE)

state_inputs <- c(lockfile, dependency_script, env_file, file.path(repo_root, ".Rprofile"))
state_inputs <- state_inputs[file.exists(state_inputs)]
current_stamp <- paste(unname(tools::md5sum(state_inputs)), collapse = "|")
existing_stamp <- if (file.exists(stamp_file)) {
  paste(readLines(stamp_file, warn = FALSE), collapse = "")
} else {
  ""
}

if (identical(current_stamp, existing_stamp)) {
  message("R_skana environment is already up to date.")
  quit(save = "no", status = 0)
}

setwd(repo_root)
renv::load(project = repo_root)

if (lockfile_supports_current_runtime(lockfile)) {
  renv::restore(project = repo_root, lockfile = lockfile, prompt = FALSE)
} else {
  message("Skipping restore from stale lockfile; installing current SK-Ana dependencies for R 4.2+.")
}

missing_after_restore <- required_sk_ana_packages[
  !vapply(required_sk_ana_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_after_restore)) {
  ensure_sk_ana_dependencies()
}

writeLines(current_stamp, stamp_file)
message("R_skana environment is ready.")
