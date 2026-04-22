# Standalone version of detectFileFormat for testing
# This extracts just the detection function without Shiny dependencies

inferDataStructureFromCoords <- function(row_coords, col_coords, default = "wxd") {
  row_coords <- suppressWarnings(as.numeric(row_coords))
  col_coords <- suppressWarnings(as.numeric(col_coords))
  row_coords <- row_coords[is.finite(row_coords)]
  col_coords <- col_coords[is.finite(col_coords)]

  if (length(row_coords) < 2 || length(col_coords) < 2) {
    return(default)
  }

  looks_like_wavelength <- function(x) {
    rng <- range(x)
    spread <- diff(rng)
    step <- median(abs(diff(x)), na.rm = TRUE)
    rng[1] >= 150 && rng[2] <= 2000 && spread >= 50 && is.finite(step) && step > 0
  }

  looks_like_delay <- function(x) {
    rng <- range(x)
    step <- median(abs(diff(x)), na.rm = TRUE)
    has_zero <- any(abs(x) < 1e-8)

    small_scale <- rng[1] >= -1e-8 && rng[2] <= 100
    zero_based_scale <- has_zero && rng[2] < 250
    relative_scale <- max(x, na.rm = TRUE) < max(col_coords, na.rm = TRUE) / 2

    (small_scale || zero_based_scale || relative_scale) && is.finite(step) && step > 0
  }

  row_is_delay <- looks_like_delay(row_coords)
  row_is_wavelength <- looks_like_wavelength(row_coords)
  col_is_delay <- looks_like_delay(col_coords)
  col_is_wavelength <- looks_like_wavelength(col_coords)

  if (row_is_delay && col_is_wavelength) {
    return("dxw")
  }
  if (row_is_wavelength && col_is_delay) {
    return("wxd")
  }

  default
}

# Extract just the detectFileFormat function
# Wrapped with safely() to handle errors gracefully
detectFileFormat_safe <- purrr::safely(function(dataFile) {
  # Try to detect file format automatically
  # Returns a list with header, sep, dec, datStr
  
  # Read first few lines as text
  lines <- readLines(dataFile, n = 10, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0]
  if(length(lines) < 2) return(NULL)
  
  # Detect field and decimal separators jointly.
  score_pair <- function(potential_sep, potential_dec) {
    if (potential_sep == potential_dec) return(-Inf)
    n_lines <- min(5, length(lines))
    subset_lines <- lines[1:n_lines]

    split_line <- function(line) {
      if (potential_sep == " ") {
        parts <- strsplit(trimws(line), "\\s+")[[1]]
      } else {
        parts <- strsplit(line, potential_sep, fixed = TRUE)[[1]]
      }
      parts[nchar(trimws(parts)) > 0]
    }

    split_vals <- lapply(subset_lines, split_line)
    token_counts <- vapply(split_vals, length, numeric(1))
    if (any(token_counts < 2)) return(-Inf)

    tokens <- unlist(split_vals, use.names = FALSE)
    tokens <- trimws(tokens)
    if (potential_dec == ",") {
      tokens <- gsub(",", ".", tokens, fixed = TRUE)
    }
    is_num <- !is.na(suppressWarnings(as.numeric(tokens)))

    consistency <- mean(token_counts) / (1 + stats::sd(token_counts))
    numeric_ratio <- mean(is_num)
    consistency * numeric_ratio
  }

  separators <- c("\t", ";", ",", " ")
  decimals <- c(".", ",")
  best_score <- -Inf
  sep <- ","
  dec <- "."
  for (potential_sep in separators) {
    for (potential_dec in decimals) {
      score <- score_pair(potential_sep, potential_dec)
      if (is.finite(score) && score > best_score) {
        best_score <- score
        sep <- potential_sep
        dec <- potential_dec
      }
    }
  }
  
  # Detect if first line is header
  split_first <- if (sep == " ") {
    strsplit(trimws(lines[1]), "\\s+")[[1]]
  } else {
    strsplit(lines[1], sep, fixed = TRUE)[[1]]
  }
  first_vals <- unlist(split_first)
  first_vals <- first_vals[nchar(trimws(first_vals)) > 0]
  
  if(length(first_vals) == 0) {
    header <- FALSE
  } else {
    parsed_vals <- first_vals
    if (dec == ",") {
      parsed_vals <- gsub(",", ".", parsed_vals, fixed = TRUE)
    }
    is_num <- !is.na(suppressWarnings(as.numeric(parsed_vals)))

    # Common file layout: first label then numeric columns.
    if (!is_num[1] && length(is_num) > 1 && mean(is_num[-1]) >= 0.75) {
      header <- TRUE
    } else {
      header <- mean(is_num) < 0.5
    }
  }
  
  parse_numeric_token <- function(token) {
    token <- trimws(token)
    if (dec == ",") {
      token <- gsub(",", ".", token, fixed = TRUE)
    }
    suppressWarnings(as.numeric(token))
  }

  row_coords <- vapply(first_vals[-1], parse_numeric_token, numeric(1))
  col_vals <- vapply(lines[-1], function(line) {
    split_line <- if (sep == " ") {
      strsplit(trimws(line), "\\s+")[[1]]
    } else {
      strsplit(line, sep, fixed = TRUE)[[1]]
    }
    split_line <- split_line[nchar(trimws(split_line)) > 0]
    if (length(split_line) == 0) {
      return(NA_real_)
    }
    parse_numeric_token(split_line[1])
  }, numeric(1))

  datStr <- inferDataStructureFromCoords(row_coords, col_vals, default = "wxd")
  
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
