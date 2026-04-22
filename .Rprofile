local({
  project_root <- Sys.getenv(
    "SK_ANA_PROJECT_ROOT",
    unset = normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  )
  renv_root <- Sys.getenv("SK_ANA_RENV_ROOT", unset = file.path(project_root, ".R_skana", "renv"))

  Sys.setenv(
    SK_ANA_PROJECT_ROOT = project_root,
    RENV_PATHS_ROOT = renv_root,
    RENV_PATHS_LIBRARY = file.path(renv_root, "library"),
    RENV_PATHS_CACHE = file.path(renv_root, "cache"),
    RENV_PATHS_SANDBOX = file.path(renv_root, "sandbox"),
    RENV_CONFIG_SANDBOX_ENABLED = "FALSE",
    RENV_CONFIG_SHIMS_ENABLED = "FALSE"
  )
  options(renv.consent = TRUE)

  if (requireNamespace("renv", quietly = TRUE)) {
    isolated_library <- renv::paths$library(project = project_root)
    if (!dir.exists(isolated_library)) {
      dir.create(isolated_library, recursive = TRUE, showWarnings = FALSE)
    }
    .libPaths(unique(c(isolated_library, .libPaths())))
  }
})
