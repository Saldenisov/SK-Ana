# Process helper wrappers for background processes
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
