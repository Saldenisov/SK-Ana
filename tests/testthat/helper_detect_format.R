# Standalone version of detectFileFormat for testing
# This extracts just the detection function without Shiny dependencies

library(purrr)

# Extract just the detectFileFormat function
# Wrapped with safely() to handle errors gracefully
detectFileFormat_safe <- safely(function(dataFile) {
  # Try to detect file format automatically
  # Returns a list with header, sep, dec, datStr
  
  # Read first few lines as text
  lines <- readLines(dataFile, n = 10, warn = FALSE)
  if(length(lines) < 2) return(NULL)
  
  # First, detect decimal separator by looking at numeric patterns
  sample_text <- paste(lines[2:min(5, length(lines))], collapse = " ")
  dot_matches <- gregexpr("\\d+\\.\\d+", sample_text)[[1]]
  comma_matches <- gregexpr("\\d+,\\d+", sample_text)[[1]]
  
  dot_count <- if(dot_matches[1] == -1) 0 else length(dot_matches)
  comma_count <- if(comma_matches[1] == -1) 0 else length(comma_matches)
  
  # Determine decimal separator
  # Prefer dot (user's standard) unless comma is clearly dominant
  if(dot_count == 0 && comma_count == 0) {
    dec <- "."  # default to dot
  } else if(comma_count > dot_count * 1.5) {
    # Only use comma if significantly more common (50% more)
    dec <- ","
  } else {
    dec <- "."  # prefer dot in ambiguous cases
  }
  
  # Now detect field separator
  # Count occurrences but be smart about not counting decimal separators
  # Order preference: tab (most common), semicolon, comma, space
  sep_counts <- list()
  
  for(potential_sep in c("\t", ";", ",", " ")) {
    matches <- gregexpr(potential_sep, lines[1], fixed = (potential_sep != " "))[[1]]
    count <- if(matches[1] == -1) 0 else length(matches)
    
    # If this is also the decimal separator, it's not a field separator
    if(potential_sep == dec && count > 0) {
      count <- 0
    }
    
    sep_counts[[potential_sep]] <- count
  }
  
  # Choose separator with highest count
  # Give slight preference to tab in case of tie (since user primarily uses tabs)
  max_count <- max(unlist(sep_counts))
  if(max_count > 0) {
    # If tab has the max count, or is within 1 of max, prefer tab
    if(sep_counts[["\t"]] == max_count || 
       (sep_counts[["\t"]] > 0 && sep_counts[["\t"]] >= max_count - 1)) {
      sep <- "\t"
    } else {
      sep <- names(which.max(sep_counts))
    }
  } else {
    sep <- "\t"  # default to tab
  }
  
  # Detect if first line is header
  # Try to parse first line as numeric
  first_vals <- unlist(strsplit(lines[1], sep, fixed = (sep != " ")))
  # Remove empty strings
  first_vals <- first_vals[nchar(trimws(first_vals)) > 0]
  
  if(length(first_vals) == 0) {
    header <- FALSE
  } else {
    numeric_count <- sum(!is.na(suppressWarnings(as.numeric(first_vals))))
    # If less than 50% are numeric, it's likely a header
    header <- (numeric_count < length(first_vals) * 0.5)
  }
  
  # Default data structure
  datStr <- "wxd"
  
  return(list(
    header = header,
    sep = sep,
    dec = dec,
    datStr = datStr
  ))
})

# Wrapper to match the original API behavior
detectFileFormat <- function(dataFile) {
  result <- detectFileFormat_safe(dataFile)
  # safely() returns list(result = ..., error = ...)
  # Return result if no error, NULL otherwise
  if (is.null(result$error)) {
    return(result$result)
  } else {
    return(NULL)
  }
}
