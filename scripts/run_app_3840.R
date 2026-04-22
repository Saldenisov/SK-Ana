#!/usr/bin/env Rscript
# Launch SK-Ana on port 3840 from the repository root.

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

  normalizePath(file.path("scripts", "run_app_3840.R"), winslash = "/", mustWork = FALSE)
}

safe_bind_host <- function(default = "127.0.0.1") {
  explicit_host <- trimws(Sys.getenv("SK_ANA_HOST", unset = ""))
  inherited_host <- trimws(Sys.getenv("HOST", unset = ""))

  if (nzchar(explicit_host)) {
    return(explicit_host)
  }

  safe_host_values <- c("127.0.0.1", "0.0.0.0", "localhost", "::1", "::")
  if (inherited_host %in% safe_host_values) {
    return(inherited_host)
  }

  if (nzchar(inherited_host)) {
    message(sprintf(
      "Ignoring HOST=%s because it is not a safe bind address. Use SK_ANA_HOST to override explicitly.",
      inherited_host
    ))
  }

  default
}

port_is_available <- function(port) {
  if (length(list_listening_pids(port))) {
    return(FALSE)
  }

  socket <- tryCatch(serverSocket(port = port), error = function(...) NULL)
  if (is.null(socket)) {
    return(FALSE)
  }

  close(socket)
  TRUE
}

list_listening_pids <- function(port) {
  if (.Platform$OS.type == "windows") {
    output <- tryCatch(
      system2("netstat", c("-ano", "-p", "tcp"), stdout = TRUE, stderr = FALSE),
      error = function(...) character()
    )
    if (!length(output)) {
      return(integer())
    }

    matches_port <- grepl(sprintf(":%d\\s", port), output)
    is_listening <- grepl("LISTENING", output, ignore.case = TRUE)
    candidates <- trimws(output[matches_port & is_listening])
    if (!length(candidates)) {
      return(integer())
    }

    pids <- sub(".*\\s([0-9]+)\\s*$", "\\1", candidates)
    return(unique(as.integer(pids[grepl("^[0-9]+$", pids)])))
  }

  if (nzchar(Sys.which("lsof"))) {
    output <- tryCatch(
      system2(
        "lsof",
        c("-nP", sprintf("-iTCP:%d", port), "-sTCP:LISTEN", "-t"),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(...) character()
    )
    return(unique(as.integer(output[grepl("^[0-9]+$", output)])))
  }

  integer()
}

process_name_for_pid <- function(pid) {
  if (.Platform$OS.type == "windows") {
    output <- tryCatch(
      system2(
        "tasklist",
        c("/FI", sprintf("PID eq %d", pid), "/FO", "CSV", "/NH"),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(...) character()
    )
    if (!length(output) || grepl("No tasks are running", output[1], ignore.case = TRUE)) {
      return("")
    }

    fields <- strsplit(gsub('^"|"$', "", output[1]), '","', fixed = FALSE)[[1]]
    return(trimws(fields[1]))
  }

  output <- tryCatch(
    system2("ps", c("-p", as.character(pid), "-o", "comm="), stdout = TRUE, stderr = FALSE),
    error = function(...) character()
  )
  trimws(paste(output, collapse = " "))
}

is_replaceable_listener <- function(pid) {
  process_name <- basename(process_name_for_pid(pid))
  nzchar(process_name) && grepl("^(R|Rscript|rsession)(\\.exe)?$", process_name, ignore.case = TRUE)
}

pid_is_running <- function(pid) {
  if (.Platform$OS.type == "windows") {
    output <- tryCatch(
      system2(
        "tasklist",
        c("/FI", sprintf("PID eq %d", pid), "/FO", "CSV", "/NH"),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(...) character()
    )
    return(length(output) && !grepl("No tasks are running", output[1], ignore.case = TRUE))
  }

  status <- suppressWarnings(tryCatch(system2("kill", c("-0", as.character(pid))), error = function(...) 1L))
  identical(status, 0L)
}

terminate_pid <- function(pid) {
  if (.Platform$OS.type == "windows") {
    status <- tryCatch(
      system2("taskkill", c("/PID", as.character(pid), "/T", "/F"), stdout = FALSE, stderr = FALSE),
      error = function(...) 1L
    )
    return(identical(status, 0L))
  }

  term_status <- tryCatch(
    system2("kill", c("-TERM", as.character(pid)), stdout = FALSE, stderr = FALSE),
    error = function(...) 1L
  )
  if (identical(term_status, 0L)) {
    for (attempt in seq_len(5)) {
      if (!pid_is_running(pid)) {
        return(TRUE)
      }
      Sys.sleep(0.2)
    }
  }

  kill_status <- tryCatch(
    system2("kill", c("-KILL", as.character(pid)), stdout = FALSE, stderr = FALSE),
    error = function(...) 1L
  )
  identical(kill_status, 0L)
}

release_port_if_possible <- function(port) {
  if (port_is_available(port)) {
    return(TRUE)
  }

  pids <- list_listening_pids(port)
  if (!length(pids)) {
    message(sprintf("Port %d is already in use and no listener PID could be detected.", port))
    return(FALSE)
  }

  replaceable_pids <- pids[vapply(pids, is_replaceable_listener, logical(1))]
  if (!length(replaceable_pids)) {
    message(sprintf(
      "Port %d is in use by a non-R process. SK-Ana will keep the existing server untouched.",
      port
    ))
    return(FALSE)
  }

  message(sprintf(
    "Port %d is busy. Attempting to stop the previous R-based SK-Ana server (PID%s %s).",
    port,
    if (length(replaceable_pids) > 1) "s" else "",
    paste(replaceable_pids, collapse = ", ")
  ))

  invisible(vapply(replaceable_pids, terminate_pid, logical(1)))

  for (attempt in seq_len(10)) {
    if (port_is_available(port)) {
      message(sprintf("Port %d has been released.", port))
      return(TRUE)
    }
    Sys.sleep(0.3)
  }

  message(sprintf("Port %d is still busy after the stop attempt.", port))
  FALSE
}

select_launch_port <- function(requested_port, search_limit = 20L) {
  if (release_port_if_possible(requested_port)) {
    return(requested_port)
  }

  candidate_ports <- seq.int(requested_port + 1L, requested_port + search_limit)
  for (candidate in candidate_ports) {
    if (port_is_available(candidate)) {
      message(sprintf("Falling back to the next available port: %d", candidate))
      return(candidate)
    }
  }

  stop(
    sprintf(
      "Could not free port %d or find an available fallback port up to %d.",
      requested_port,
      requested_port + search_limit
    ),
    call. = FALSE
  )
}

try({
  script_path <- resolve_script_path()
  repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
  dependency_script <- file.path(dirname(script_path), "_dependencies.R")

  if (getRversion() < "4.2.0") {
    stop("SK-Ana requires R 4.2 or newer.", call. = FALSE)
  }

  setwd(repo_root)

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

  if (requireNamespace("renv", quietly = TRUE)) {
    renv::load(project = repo_root)
  }

  source(dependency_script, local = TRUE)
  ensure_sk_ana_dependencies()

  host <- safe_bind_host()
  port <- suppressWarnings(as.integer(Sys.getenv("PORT", unset = "3840")))
  if (!is.finite(port) || is.na(port)) {
    port <- 3840L
  }

  launch_browser <- tolower(Sys.getenv("SK_ANA_LAUNCH_BROWSER", unset = "true")) %in% c(
    "1", "true", "yes", "y", "on"
  )
  check_only <- tolower(Sys.getenv("SK_ANA_CHECK_ONLY", unset = "false")) %in% c(
    "1", "true", "yes", "y", "on"
  )

  message(sprintf("Prepared SK-Ana in %s with R %s", repo_root, getRversion()))

  if (check_only) {
    message("Environment check completed successfully.")
    quit(save = "no", status = 0)
  }

  port <- select_launch_port(port)
  message(sprintf("Launching Shiny app on http://%s:%s", host, port))
  shiny::runApp(
    appDir = ".",
    host = host,
    port = port,
    launch.browser = launch_browser
  )
}, silent = FALSE)
