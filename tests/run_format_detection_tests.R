#!/usr/bin/env Rscript
# Standalone test runner for file format detection tests
# Run this script directly: Rscript tests/run_format_detection_tests.R

# Check for required packages
required_packages <- c("testthat", "purrr")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("\n=================================================================\n")
  cat("ERROR: Missing required packages\n")
  cat("=================================================================\n")
  cat("The following packages are not installed:", paste(missing_packages, collapse = ", "), "\n\n")
  cat("Please install them by running R as Administrator and executing:\n")
  cat("  install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n\n", sep = "")
  cat("Or install to your user library in R console:\n")
  cat("  install.packages(c('", paste(missing_packages, collapse = "', '"), "'), lib = Sys.getenv('R_LIBS_USER'))\n\n", sep = "")
  quit(status = 1)
}

# Load libraries
library(testthat)
library(purrr)

cat("=================================================================\n")
cat("Running File Format Detection Tests\n")
cat("=================================================================\n\n")

# Run the tests
test_results <- test_file(
  "tests/testthat/test_file_format_detection.R",
  reporter = "progress"
)

cat("\n=================================================================\n")
cat("Test Summary\n")
cat("=================================================================\n")

# Print summary
if (inherits(test_results, "testthat_results")) {
  n_tests <- length(test_results)
  n_failed <- sum(sapply(test_results, function(x) !is.null(x$failed) && x$failed))
  n_passed <- n_tests - n_failed
  
  cat(sprintf("Total tests: %d\n", n_tests))
  cat(sprintf("Passed: %d\n", n_passed))
  cat(sprintf("Failed: %d\n", n_failed))
  
  if (n_failed > 0) {
    cat("\n❌ Some tests failed!\n")
    quit(status = 1)
  } else {
    cat("\n✓ All tests passed!\n")
    quit(status = 0)
  }
} else {
  cat("Tests completed. See output above for details.\n")
}
