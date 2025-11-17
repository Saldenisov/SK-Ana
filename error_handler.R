# Error Handler Decorator for R Functions
# Catches all errors and prevents program termination

# Main error handler function
main_error_handler <- function(error, func_name, func_args) {
  error_msg <- sprintf(
    "\n========== ERROR CAUGHT ==========\n" +
    "Function: %s\n" +
    "Error Type: %s\n" +
    "Error Message: %s\n" +
    "Stack Trace:\n%s\n" +
    "Arguments: %s\n" +
    "Timestamp: %s\n" +
    "==================================\n",
    func_name,
    class(error)[1],
    conditionMessage(error),
    paste(capture.output(traceback()), collapse = "\n"),
    paste(capture.output(str(func_args)), collapse = "\n"),
    Sys.time()
  )
  
  # Print to console
  cat(error_msg, file = stderr())
  
  # Optional: Log to file
  # log_file <- "error_log.txt"
  # cat(error_msg, file = log_file, append = TRUE)
  
  return(NULL)
}

# Decorator function that wraps any function with error handling
safe_function <- function(func, return_on_error = NULL) {
  func_name <- deparse(substitute(func))
  
  wrapped_func <- function(...) {
    tryCatch(
      {
        # Execute the original function
        result <- func(...)
        return(result)
      },
      error = function(e) {
        # Capture function arguments
        func_args <- list(...)
        
        # Send to main error handler
        main_error_handler(e, func_name, func_args)
        
        # Return default value instead of terminating
        return(return_on_error)
      },
      warning = function(w) {
        # Optionally handle warnings
        warning_msg <- sprintf(
          "\n[WARNING in %s]: %s\n",
          func_name,
          conditionMessage(w)
        )
        cat(warning_msg, file = stderr())
        
        # Continue execution
        invokeRestart("muffleWarning")
      }
    )
  }
  
  return(wrapped_func)
}

# Alternative: Decorator that modifies function in place
with_error_handling <- function(func, func_name = NULL, return_on_error = NULL) {
  if (is.null(func_name)) {
    func_name <- deparse(substitute(func))
  }
  
  function(...) {
    tryCatch(
      {
        func(...)
      },
      error = function(e) {
        func_args <- list(...)
        main_error_handler(e, func_name, func_args)
        return(return_on_error)
      },
      warning = function(w) {
        warning_msg <- sprintf(
          "\n[WARNING in %s]: %s\n",
          func_name,
          conditionMessage(w)
        )
        cat(warning_msg, file = stderr())
        invokeRestart("muffleWarning")
      }
    )
  }
}

# Decorator that can be used with assignment
safely <- function(func, return_on_error = NULL) {
  func_name <- as.character(substitute(func))
  
  force(func)
  force(func_name)
  
  return(function(...) {
    tryCatch(
      {
        func(...)
      },
      error = function(e) {
        func_args <- list(...)
        main_error_handler(e, func_name, func_args)
        return(return_on_error)
      }
    )
  })
}

# Example usage:
# 
# # Method 1: Wrap individual function
# my_function <- function(x) {
#   if (x < 0) stop("Negative number not allowed")
#   return(sqrt(x))
# }
# safe_my_function <- safe_function(my_function, return_on_error = NA)
# result <- safe_my_function(-5)  # Won't crash, returns NA
# 
# # Method 2: Wrap with custom name
# my_risky_func <- function(a, b) {
#   return(a / b)  # Might divide by zero
# }
# safe_divide <- with_error_handling(my_risky_func, "safe_divide", return_on_error = 0)
# result <- safe_divide(10, 0)  # Returns 0 instead of crashing
# 
# # Method 3: Use safely() wrapper
# calculate <- safely(function(x) {
#   log(x)
# }, return_on_error = -Inf)
# result <- calculate(-1)  # Returns -Inf instead of crashing
