# Simple smoke test: Source server files and check functions exist

# Test if core files can be sourced
files_to_test <- c(
  "helpers.R",
  "process_utils.R", 
  "ALS_plots.R",
  "SVD.R"
)

cat("Testing server files...\n")

# Source global.R first
if (file.exists("global.R")) {
  source("global.R")
  cat("✓ global.R sourced\n")
} else {
  cat("! global.R not found\n")
}

# Test individual files
for (f in files_to_test) {
  file_path <- file.path("server_files", f)
  if (file.exists(file_path)) {
    tryCatch({
      source(file_path)
      cat("✓", f, "sourced successfully\n")
    }, error = function(e) {
      cat("✗", f, "failed:", e$message, "\n")
    })
  } else {
    cat("✗", f, "not found\n")
  }
}

# Test core functions exist
functions_to_check <- c("showMSE", "getExternalSpectra", "process_status", "lof", "plotAlsVec")

cat("\nTesting function availability...\n")
for (fn in functions_to_check) {
  if (exists(fn)) {
    cat("✓", fn, "exists\n")
  } else {
    cat("✗", fn, "missing\n")
  }
}

# Basic functionality tests
cat("\nTesting basic functionality...\n")

# Test showMSE
if (exists("showMSE")) {
  result <- showMSE("tileDel", 1:3, 2)
  if (result == TRUE) {
    cat("✓ showMSE works correctly\n")
  } else {
    cat("✗ showMSE returned unexpected result\n")
  }
}

# Test lof if it exists
if (exists("lof")) {
  m <- matrix(c(1,2,3,4), nrow=2)
  result <- lof(m, m)
  if (abs(result) < 1e-10) {
    cat("✓ lof works correctly\n")
  } else {
    cat("✗ lof returned unexpected result:", result, "\n")
  }
}

# Test process_status
if (exists("process_status")) {
  result <- process_status(NULL)
  if (is.list(result) && is.null(result$running)) {
    cat("✓ process_status works correctly\n") 
  } else {
    cat("✗ process_status returned unexpected result\n")
  }
}

cat("\nSmoke test complete!\n")