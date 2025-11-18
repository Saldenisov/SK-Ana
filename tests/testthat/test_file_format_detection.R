# Test suite for detectFileFormat function
# Tests automatic detection of file format parameters

library(testthat)
library(purrr)

# Load the standalone detectFileFormat function
# testthat automatically sources helper files from tests/testthat/
# If helper not loaded, source it explicitly
if (!exists("detectFileFormat")) {
  source("helper_detect_format.R", local = TRUE)
}

context("File Format Detection")

# Helper function to create temporary test files
create_test_file <- function(content, filename = tempfile(fileext = ".txt")) {
  writeLines(content, filename)
  return(filename)
}

# Test 1: CSV with comma separator and dot decimal
test_that("detects CSV format with comma separator", {
  content <- c(
    "100.5,200.3,300.7,400.2",
    "1.234,5.678,9.012,3.456",
    "2.345,6.789,0.123,4.567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, ",")
  expect_equal(result$dec, ".")
  expect_false(result$header)
  expect_equal(result$datStr, "wxd")
  
  unlink(file)
})

# Test 2: TSV with tab separator and dot decimal
test_that("detects TSV format with tab separator", {
  content <- c(
    "100.5\t200.3\t300.7\t400.2",
    "1.234\t5.678\t9.012\t3.456",
    "2.345\t6.789\t0.123\t4.567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, "\t")
  expect_equal(result$dec, ".")
  expect_false(result$header)
  
  unlink(file)
})

# Test 3: Semicolon separator with comma decimal (European format)
test_that("detects semicolon separator with comma decimal", {
  content <- c(
    "100,5;200,3;300,7;400,2",
    "1,234;5,678;9,012;3,456",
    "2,345;6,789;0,123;4,567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, ";")
  expect_equal(result$dec, ",")
  expect_false(result$header)
  
  unlink(file)
})

# Test 4: File with header
test_that("detects header presence", {
  content <- c(
    "Wavelength,Delay1,Delay2,Delay3",
    "100.5,200.3,300.7,400.2",
    "1.234,5.678,9.012,3.456",
    "2.345,6.789,0.123,4.567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_true(result$header)
  expect_equal(result$sep, ",")
  
  unlink(file)
})

# Test 5: File without header (all numeric first line)
test_that("detects absence of header when first line is all numeric", {
  content <- c(
    "100,200,300,400",
    "1.234,5.678,9.012,3.456",
    "2.345,6.789,0.123,4.567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_false(result$header)
  
  unlink(file)
})

# Test 6: Space-separated values
test_that("detects space separator", {
  content <- c(
    "100.5 200.3 300.7 400.2",
    "1.234 5.678 9.012 3.456",
    "2.345 6.789 0.123 4.567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, " ")
  expect_equal(result$dec, ".")
  
  unlink(file)
})

# Test 7: Mixed header (some numeric, some text)
test_that("detects header with mixed content", {
  content <- c(
    "delay,400,500,600",
    "1.234,5.678,9.012,3.456",
    "2.345,6.789,0.123,4.567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_true(result$header)
  
  unlink(file)
})

# Test 8: ELYSE format (tab-separated, no header, dot decimal)
test_that("detects ELYSE-style format", {
  content <- c(
    "350.0\t400.5\t450.2\t500.1",
    "0.001\t0.023\t0.045\t0.067",
    "0.002\t0.024\t0.046\t0.068",
    "0.003\t0.025\t0.047\t0.069"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, "\t")
  expect_equal(result$dec, ".")
  expect_false(result$header)
  
  unlink(file)
})

# Test 9: Streak format (comma-separated with header)
test_that("detects Streak-style format", {
  content <- c(
    "Time,350nm,400nm,450nm,500nm",
    "0.001,0.023,0.045,0.067,0.089",
    "0.002,0.024,0.046,0.068,0.090",
    "0.003,0.025,0.047,0.069,0.091"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, ",")
  expect_true(result$header)
  
  unlink(file)
})

# Test 10: File with only one line (edge case)
test_that("handles file with insufficient lines gracefully", {
  content <- c("100,200,300")
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  # Should return NULL for files with less than 2 lines
  expect_true(is.null(result))
  
  unlink(file)
})

# Test 11: Empty file (edge case)
test_that("handles empty file gracefully", {
  file <- tempfile(fileext = ".txt")
  writeLines(character(0), file)
  
  result <- detectFileFormat(file)
  
  expect_true(is.null(result))
  
  unlink(file)
})

# Test 12: File with scientific notation
test_that("detects format with scientific notation", {
  content <- c(
    "1.0e-3,2.5e-3,5.0e-3,1.0e-2",
    "1.234e-5,5.678e-5,9.012e-5,3.456e-4",
    "2.345e-6,6.789e-6,1.230e-5,4.567e-5"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, ",")
  expect_equal(result$dec, ".")
  
  unlink(file)
})

# Test 13: Large numbers without decimals
test_that("handles integers correctly", {
  content <- c(
    "100,200,300,400",
    "150,250,350,450",
    "200,300,400,500"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, ",")
  # Should default to "." when no decimal numbers are found
  expect_equal(result$dec, ".")
  expect_false(result$header)
  
  unlink(file)
})

# Test 14: Negative numbers
test_that("handles negative numbers", {
  content <- c(
    "-100.5,200.3,-300.7,400.2",
    "-1.234,5.678,-9.012,3.456",
    "2.345,-6.789,0.123,-4.567"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, ",")
  expect_equal(result$dec, ".")
  
  unlink(file)
})

# Test 15: Integration with safely wrapper
test_that("safely wrapper returns proper structure", {
  content <- c(
    "100.5,200.3,300.7,400.2",
    "1.234,5.678,9.012,3.456"
  )
  file <- create_test_file(content)
  
  # The function is wrapped with safely(), so result should have specific structure
  result <- detectFileFormat(file)
  
  # When successful, safely returns the result directly (not wrapped)
  expect_false(is.null(result))
  expect_true(is.list(result))
  expect_true("header" %in% names(result))
  expect_true("sep" %in% names(result))
  expect_true("dec" %in% names(result))
  expect_true("datStr" %in% names(result))
  
  unlink(file)
})

# Test 16: Non-existent file
test_that("handles non-existent file", {
  result <- detectFileFormat("nonexistent_file_xyz123.txt")
  
  # Should return NULL or handle error gracefully
  expect_true(is.null(result))
})

cat("\nAll file format detection tests completed!\n")
