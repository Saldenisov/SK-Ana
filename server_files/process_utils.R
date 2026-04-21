# Process helper wrappers for background processes
background_runner_available = safely(function() {
  isTRUE(tryCatch(
    requireNamespace("callr", quietly = TRUE),
    error = function(e) FALSE
  ))
}, return_on_error = FALSE)

background_runner_env = safely(function() {
  if (!identical(.Platform$OS.type, "windows")) {
    return(NULL)
  }

  arch <- Sys.getenv("R_ARCH", "")
  rterm_candidate <- file.path(R.home("bin"), arch, "Rterm.exe")
  if (nzchar(arch) && !file.exists(rterm_candidate)) {
    rbgenv <- character(0)
    rbgenv["R_ARCH"] <- ""
    return(rbgenv)
  }

  NULL
}, return_on_error = NULL)

launch_background_job = safely(function(
  fun,
  args = list(),
  stdout = NULL,
  stderr = NULL,
  env = background_runner_env(),
  package = TRUE,
  job_name = "Background job",
  namespace_available = background_runner_available,
  launcher = NULL
) {
  if (!isTRUE(namespace_available())) {
    return(list(
      job = NULL,
      error = paste0(
        job_name,
        " is unavailable because optional package 'callr' is not installed."
      )
    ))
  }

  if (is.null(launcher)) {
    launcher <- callr::r_bg
  }

  params <- list(
    func = fun,
    args = args,
    package = package
  )
  if (!is.null(stdout)) params$stdout <- stdout
  if (!is.null(stderr)) params$stderr <- stderr
  if (!is.null(env)) params$env <- env

  job <- tryCatch(
    do.call(launcher, params),
    error = function(e) e
  )

  if (inherits(job, "error")) {
    return(list(
      job = NULL,
      error = paste0(job_name, " failed to start: ", conditionMessage(job))
    ))
  }

  list(job = job, error = NULL)
}, return_on_error = list(
  job = NULL,
  error = "Background job failed to start."
))

process_id = safely(function(px) {
  if (is.null(px)) return(NULL)
  px$get_pid()
}, return_on_error = NULL)

process_running = safely(function(px) {
  if (is.null(px)) return(NULL)
  px$is_alive()
}, return_on_error = NULL)

process_exit_status = safely(function(px) {
  if (is.null(px)) return(NULL)
  px$get_exit_status()
}, return_on_error = NULL)

process_result = safely(function(px) {
  if (is.null(px)) return(NULL)
  if (is.null(process_running(px))) return(NULL)
  if (process_running(px)) return(NULL)
  if (is.null(process_exit_status(px))) return(NULL)
  if (process_exit_status(px) != 0) return(NULL)
  px$get_result()
}, return_on_error = NULL)

process_status = safely(function(px) {
  list(
    pid         = process_id(px), 
    running     = process_running(px),
    exit_status = process_exit_status(px),
    result      = process_result(px) 
  )
}, return_on_error = NULL)
