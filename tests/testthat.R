if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_dir("tests/testthat", reporter = "summary")
} else {
  cat("Package 'testthat' is not installed. Install it to run tests.\n")
}
