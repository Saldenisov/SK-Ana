# Process helper wrappers for background processes
process_id = function(px) {
  if (is.null(px)) return(NULL)
  px$get_pid()
}
process_running = function(px) {
  if (is.null(px)) return(NULL)
  px$is_alive()
}
process_exit_status = function(px) {
  if (is.null(px)) return(NULL)
  px$get_exit_status()
}
process_result = function(px) {
  if (is.null(px)) return(NULL)
  if (is.null(process_running(px))) return(NULL)
  if (process_running(px)) return(NULL)
  if (is.null(process_exit_status(px))) return(NULL)
  if (process_exit_status(px) != 0) return(NULL)
  px$get_result()
}
process_status = function(px) {
  list(
    pid         = process_id(px), 
    running     = process_running(px),
    exit_status = process_exit_status(px),
    result      = process_result(px) 
  )
}
