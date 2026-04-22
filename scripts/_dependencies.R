required_sk_ana_packages <- c(
  "outliers", "nnls", "Iso", "httpuv",
  "changepoint", "shiny", "shinyBS", "DT", "Rsolnp",
  "fields", "NMFN", "tools", "shinycssloaders",
  "rgenoud", "mvtnorm", "deSolve", "msm", "xtable",
  "shinythemes", "magrittr", "lifecycle",
  "callr", "processx", "RColorBrewer", "viridisLite",
  "signal", "zoo", "rmarkdown", "knitr",
  "sass", "bslib", "htmltools", "jquerylib",
  "renv", "remotes", "VGAM",
  "withr", "purrr", "testthat"
)

preferred_primary_library_packages <- function() {
  if (.Platform$OS.type == "windows") {
    return(c("sass", "bslib", "htmltools", "jquerylib"))
  }

  character()
}

sk_ana_cran_repo <- function() {
  repo <- Sys.getenv("SK_ANA_CRAN_REPO", unset = "https://cloud.r-project.org")
  if (!nzchar(repo)) {
    repo <- "https://cloud.r-project.org"
  }
  repo
}

sk_ana_pkg_type <- function() {
  explicit_type <- Sys.getenv("SK_ANA_R_PKG_TYPE", unset = "")
  if (nzchar(explicit_type)) {
    return(explicit_type)
  }

  if (.Platform$OS.type == "windows") {
    return("binary")
  }

  getOption("pkgType", default = "source")
}

install_cran_packages <- function(packages, repos = sk_ana_cran_repo()) {
  if (!length(packages)) {
    return(invisible(character()))
  }

  pkg_type <- sk_ana_pkg_type()
  message(sprintf(
    "Installing missing R packages from %s (%s): %s",
    repos,
    pkg_type,
    paste(packages, collapse = ", ")
  ))

  utils::install.packages(
    packages,
    repos = repos,
    type = pkg_type,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
  invisible(packages)
}

package_library_root <- function(package) {
  package_path <- suppressWarnings(tryCatch(find.package(package), error = function(...) ""))
  if (!nzchar(package_path)) {
    return("")
  }

  normalizePath(dirname(package_path), winslash = "/", mustWork = FALSE)
}

package_in_primary_library <- function(package) {
  primary_lib <- normalizePath(.libPaths()[1], winslash = "/", mustWork = FALSE)
  identical(package_library_root(package), primary_lib)
}

ensure_primary_library_packages <- function(packages, repos = sk_ana_cran_repo()) {
  if (!length(packages)) {
    return(invisible(character()))
  }

  packages <- unique(packages)
  needs_primary_install <- packages[
    !vapply(packages, package_in_primary_library, logical(1))
  ]

  if (!length(needs_primary_install)) {
    return(invisible(character()))
  }

  message(sprintf(
    "Installing critical runtime packages into the isolated library: %s",
    paste(needs_primary_install, collapse = ", ")
  ))

  install_cran_packages(needs_primary_install, repos = repos)
  invisible(needs_primary_install)
}

ensure_shinybs <- function(repos = sk_ana_cran_repo()) {
  if (requireNamespace("shinyBS", quietly = TRUE)) {
    return(invisible(TRUE))
  }

  message("Installing missing R package: shinyBS")
  tryCatch(
    utils::install.packages(
      "shinyBS",
      repos = repos,
      type = sk_ana_pkg_type(),
      dependencies = c("Depends", "Imports", "LinkingTo")
    ),
    error = function(err) {
      message(sprintf("CRAN install for shinyBS failed: %s", conditionMessage(err)))
    }
  )

  if (requireNamespace("shinyBS", quietly = TRUE)) {
    return(invisible(TRUE))
  }

  if (!requireNamespace("remotes", quietly = TRUE)) {
    install_cran_packages("remotes", repos = repos)
  }

  message("Retrying shinyBS installation from GitHub: ebailey78/shinyBS")
  remotes::install_github(
    "ebailey78/shinyBS",
    upgrade = "never",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )

  if (!requireNamespace("shinyBS", quietly = TRUE)) {
    stop("Failed to install required package 'shinyBS'.", call. = FALSE)
  }

  invisible(TRUE)
}

ensure_sk_ana_dependencies <- function(repos = sk_ana_cran_repo()) {
  ensure_primary_library_packages(preferred_primary_library_packages(), repos = repos)

  missing <- required_sk_ana_packages[
    !vapply(required_sk_ana_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  cran_install <- setdiff(missing, "shinyBS")
  install_cran_packages(cran_install, repos = repos)

  if ("shinyBS" %in% missing) {
    ensure_shinybs(repos = repos)
  }

  still_missing <- required_sk_ana_packages[
    !vapply(required_sk_ana_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(still_missing)) {
    stop(
      sprintf(
        "Missing required R packages after installation attempt: %s",
        paste(still_missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  message("All required R packages are available.")
  invisible(required_sk_ana_packages)
}
