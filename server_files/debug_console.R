# Debug Console and Logging for Shiny
# ====================================
# Provides real-time logging to a console tab in the Shiny UI
# Captures errors, warnings, and custom log messages

# Create reactive storage for logs
debug_logs <- reactiveValues(
  messages = character(0),
  last_update = 0
)

# Capture console output and errors
capture_log <- safely(function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- paste0("[", timestamp, "] [", level, "] ", msg)
  
  # Add to log
  debug_logs$messages <- c(debug_logs$messages, log_line)
  
  # Keep only last 500 lines to prevent memory issues
  if (length(debug_logs$messages) > 500) {
    debug_logs$messages <- debug_logs$messages[(length(debug_logs$messages)-499):length(debug_logs$messages)]
  }
  
  # Update timestamp
  debug_logs$last_update <- Sys.time()
  
  # Also print to console
  cat(log_line, "\n")
}, return_on_error = NULL)

# Override system messages
log_info <- safely(function(msg) {
  capture_log(msg, "INFO")
}, return_on_error = NULL)

log_warning <- safely(function(msg) {
  capture_log(msg, "WARN")
}, return_on_error = NULL)

log_error <- safely(function(msg) {
  capture_log(msg, "ERROR")
}, return_on_error = NULL)

log_debug <- safely(function(msg) {
  capture_log(msg, "DEBUG")
}, return_on_error = NULL)

# Create debug console output
output$debug_console <- renderUI({
  debug_logs$last_update
  
  # Create scrollable text area with logs
  tagList(
    fluidRow(
      column(
        12,
        h4("Debug Console", style = "margin-top: 0;"),
        actionButton("debug_clear", "Clear Logs", icon = icon("trash"), size = "sm"),
        downloadButton("debug_download", "Download Logs", icon = icon("download"), class = "btn btn-default btn-sm"),
        style = "margin-bottom: 10px;"
      )
    ),
    fluidRow(
      column(
        12,
        div(
          style = "border: 1px solid #ddd; background: #f8f8f8; padding: 10px; height: 400px; overflow-y: auto; font-family: monospace; font-size: 12px; white-space: pre-wrap; word-wrap: break-word;",
          paste(debug_logs$messages, collapse = "\n")
        )
      )
    )
  )
})

# Clear logs button
observeEvent(input$debug_clear, {
  debug_logs$messages <- character(0)
  log_info("Debug logs cleared by user")
})

# Download logs button
output$debug_download <- downloadHandler(
  filename = function() {
    paste0("sk-ana-debug-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
  },
  content = function(file) {
    writeLines(debug_logs$messages, file)
  }
)

# Log initialization is deferred until after UI renders
# to avoid reactive context issues

# Wrapper function to safely execute render functions
safe_render <- function(expr, error_msg = "Render error") {
  tryCatch(
    {
      expr
    },
    error = function(e) {
      log_error(paste(error_msg, ":", conditionMessage(e)))
      # Return empty placeholder
      div("Error: ", conditionMessage(e), style = "color: red; padding: 10px;")
    }
  )
}

# Wrapper for plots
safe_plot <- function(expr, error_msg = "Plot error") {
  tryCatch(
    {
      expr
    },
    error = function(e) {
      log_error(paste(error_msg, ":", conditionMessage(e)))
      # Return empty plot with error message
      plot(1, 1, type="n", main="Error", xlab="", ylab="")
      text(1, 1, paste("Error:", conditionMessage(e)), col="red")
    }
  )
}

# Cross-platform RAM detection for diagnostics panel
get_available_ram_gb <- safely(function() {
  sysname <- Sys.info()[["sysname"]]

  if (identical(sysname, "Windows")) {
    out <- tryCatch(
      system2("wmic", c("OS", "get", "TotalVisibleMemorySize", "/Value"),
              stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    line <- grep("^TotalVisibleMemorySize=", out, value = TRUE)
    if (length(line) > 0) {
      kb <- suppressWarnings(as.numeric(sub("^TotalVisibleMemorySize=", "", line[1])))
      if (is.finite(kb) && kb > 0) return(round(kb / 1024 / 1024, 2))
    }
  }

  if (identical(sysname, "Linux")) {
    out <- tryCatch(readLines("/proc/meminfo", warn = FALSE), error = function(e) character(0))
    line <- grep("^MemTotal:", out, value = TRUE)
    if (length(line) > 0) {
      kb <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", line[1])))
      if (is.finite(kb) && kb > 0) return(round(kb / 1024 / 1024, 2))
    }
  }

  if (identical(sysname, "Darwin")) {
    out <- tryCatch(
      suppressWarnings(
        system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE)
      ),
      error = function(e) character(0)
    )
    if (length(out) > 0) {
      bytes <- suppressWarnings(as.numeric(out[1]))
      if (is.finite(bytes) && bytes > 0) return(round(bytes / (1024^3), 2))
    }
  }

  NA_real_
}, return_on_error = NA_real_)

# System information output
output$system_info <- renderPrint({
  ram_gb <- get_available_ram_gb()
  ram_text <- if (is.finite(ram_gb)) paste(ram_gb, "GB") else "Unknown"

  cat("=== System Information ===", "\n")
  cat("R Version:", R.version$version.string, "\n")
  cat("Platform:", R.version$platform, "\n")
  cat("Working Directory:", getwd(), "\n")
  cat("Available RAM:", ram_text, "\n")
  cat("\n")
  cat("=== Session Info ===", "\n")
  cat("Application Started:", format(Sys.time()), "\n")
  cat("Total Log Lines:", length(debug_logs$messages), "\n")
  cat("\n")
})

# Add error tracking to ambiguity explorer
add_ambiguity_logging <- function() {
  log_debug("Ambiguity explorer initialization")
}

# Track observer events
track_ambiguity_start <- function() {
  log_info("Ambiguity explorer: Run button clicked")
  log_debug(paste("Selected vectors for rotation:", paste(as.numeric(unlist(input$vecsToRotate)), collapse = ",")))
  log_debug(paste("Exploration step:", input$alsRotAmbDens))
  log_debug(paste("Positivity threshold:", input$alsRotAmbEps))
}

track_ambiguity_error <- function(error_msg) {
  log_error(paste("Ambiguity explorer error:", error_msg))
}

track_ambiguity_complete <- function(n_solutions) {
  log_info(paste("Ambiguity explorer completed with", n_solutions, "solutions"))
}
