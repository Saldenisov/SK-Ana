#!/usr/bin/env Rscript
# Launch SK-Ana on port 3840 from the repository root.
try({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_path <- if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)
  } else {
    script_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
    if (is.null(script_file)) {
      normalizePath(file.path("scripts", "run_app_3840.R"), winslash = "/", mustWork = FALSE)
    } else {
      normalizePath(script_file, winslash = "/", mustWork = FALSE)
    }
  }
  repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
  setwd(repo_root)
  host <- Sys.getenv("HOST", unset = "127.0.0.1")
  port <- suppressWarnings(as.integer(Sys.getenv("PORT", unset = "3840")))
  if (!is.finite(port) || is.na(port)) {
    port <- 3840L
  }
  options(shiny.port = port, shiny.host = host)
  if (!requireNamespace('shiny', quietly = TRUE)) {
    message('shiny package not found. Exiting with status 11.')
    quit(status = 11)
  }
  message(sprintf('Launching Shiny app in %s on http://%s:%s', getwd(), getOption('shiny.host'), getOption('shiny.port')))
  shiny::runApp('.', launch.browser = TRUE)
}, silent = FALSE)
