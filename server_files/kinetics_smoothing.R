# Kinetics Smoothing Functions
# ============================
# Implements Savitzky-Golay filtering for kinetics curves

# Install signal package if needed for SG filter
if (!requireNamespace("signal", quietly = TRUE)) {
  tryCatch({
    install.packages("signal", repos = "https://cloud.r-project.org")
  }, error = function(e) {
    log_warning("Could not install 'signal' package for SG filtering")
  })
}

# Apply Savitzky-Golay filter to smooth kinetics
apply_sg_filter <- safely(function(kinetics, window_length, poly_order) {
  # Ensure window length is odd
  if (window_length %% 2 == 0) {
    window_length <- window_length + 1
  }
  
  # Ensure polynomial order is less than window length
  if (poly_order >= window_length) {
    poly_order <- window_length - 2
  }
  
  # Check if signal package is available
  if (!requireNamespace("signal", quietly = TRUE)) {
    log_warning("signal package not available, using simple moving average instead")
    return(apply_moving_average(kinetics, window_length))
  }
  
  tryCatch({
    # Apply SG filter to each kinetics trace (column)
    smoothed <- apply(kinetics, 2, function(col) {
      # Handle NAs
      if (any(is.na(col))) {
        na_indices <- which(is.na(col))
        col_clean <- col[!is.na(col)]
        if (length(col_clean) < window_length) {
          return(col)  # Return original if too short
        }
        smoothed_clean <- as.numeric(signal::sgolayfilt(col_clean, poly_order, window_length))
        result <- col
        result[!is.na(col)] <- smoothed_clean
        return(result)
      } else {
        if (length(col) < window_length) {
          return(col)  # Return original if too short
        }
        return(as.numeric(signal::sgolayfilt(col, poly_order, window_length)))
      }
    })
    
    return(smoothed)
  }, error = function(e) {
    log_error(paste("SG filter error:", e$message))
    return(kinetics)  # Return original on error
  })
}, return_on_error = NULL)

# Fallback: simple moving average if signal package unavailable
apply_moving_average <- safely(function(kinetics, window_length) {
  if (window_length %% 2 == 0) {
    window_length <- window_length + 1
  }
  
  apply(kinetics, 2, function(col) {
    if (any(is.na(col))) {
      na_indices <- which(is.na(col))
      col_clean <- col[!is.na(col)]
      smoothed_clean <- zoo::rollmean(col_clean, k = window_length, fill = NA)
      result <- col
      result[!is.na(col)] <- smoothed_clean
      return(result)
    } else {
      return(zoo::rollmean(col, k = window_length, fill = NA))
    }
  })
}, return_on_error = NULL)

# Enhanced kinetics plotting with optional smoothing
plot_kinetics_with_smoothing <- function(data, x,
                                       show_smoothed = FALSE,
                                       show_both = FALSE,
                                       window = 5, 
                                       order = 2,
                                       alpha_smooth = 0.7,
                                       ...) {
  
  if (!show_smoothed) {
    # Plot only original
    matplot(x, data, type = "l", ...)
  } else {
    # Calculate smoothed version
    smoothed_data <- apply_sg_filter(data, window, order)
    
    if (!show_both) {
      # Plot only smoothed
      matplot(x, smoothed_data, type = "l", ...)
    } else {
      # Plot both original and smoothed
      # First plot smoothed with transparency
      matplot(x, smoothed_data, type = "l", col = rgb(0, 0, 0, alpha_smooth), ...)
      # Then overlay original with full opacity
      matlines(x, data, type = "l", col = 1:ncol(data), lty = 1, lwd = 1)
    }
  }
}

# Reactive smoothed kinetics data
kinetics_smoothed <- reactive({
  req(kinAlsOut <- kinResults())  # Ensure results available
  
  if (!input$kinDisplaySmoothed) {
    return(NULL)
  }
  
  log_debug(paste("Computing smoothed kinetics: window =", input$kinSGWindow, "order =", input$kinSGOrder))
  
  tryCatch({
    kinetics_data <- kinAlsOut$C
    apply_sg_filter(kinetics_data, input$kinSGWindow, input$kinSGOrder)
  }, error = function(e) {
    log_error(paste("Error smoothing kinetics:", e$message))
    return(NULL)
  })
})

# Add logging for filter parameter changes
observeEvent(input$kinSGWindow, {
  log_debug(paste("Kinetics SG window changed to:", input$kinSGWindow))
})

observeEvent(input$kinSGOrder, {
  log_debug(paste("Kinetics SG order changed to:", input$kinSGOrder))
})

observeEvent(input$kinDisplaySmoothed, {
  if (input$kinDisplaySmoothed) {
    log_info("Kinetics smoothing enabled")
  } else {
    log_info("Kinetics smoothing disabled")
  }
})

# Validate filter parameters
validate_filter_params <- function(window, order, data_length) {
  errors <- c()
  
  # Window must be odd and positive
  if (window < 3 || window > 51) {
    errors <- c(errors, "Window length must be between 3 and 51")
  }
  
  if (window %% 2 == 0) {
    errors <- c(errors, "Window length must be odd")
  }
  
  # Order must be positive and less than window
  if (order < 1 || order >= window) {
    errors <- c(errors, paste("Polynomial order must be between 1 and", window - 1))
  }
  
  # Data must have enough points
  if (data_length < window) {
    errors <- c(errors, paste("Data length (", data_length, ") must be >= window length (", window, ")"))
  }
  
  return(errors)
}
