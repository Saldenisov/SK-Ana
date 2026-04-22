# File Format Detection Tests

This document describes the test suite for the automatic file format detection feature in SK-Ana.

## Overview

The `detectFileFormat()` function automatically detects file format parameters by analyzing the structure and content of data files. The test suite verifies that this detection works correctly for various file formats and edge cases.

## Test Coverage

The test suite includes **16 comprehensive tests** covering:

### Basic Format Detection
1. **CSV format** - Comma-separated with dot decimal
2. **TSV format** - Tab-separated with dot decimal
3. **European format** - Semicolon-separated with comma decimal
4. **Space-separated** - Space delimiter with dot decimal

### Header Detection
5. **With header** - Text labels in first line
6. **Without header** - All numeric first line
7. **Mixed header** - Combination of text and numbers

### Special Cases
8. **ELYSE format** - Tab-separated, no header (specific to project)
9. **Streak format** - CSV with header (specific to project)
10. **Scientific notation** - Numbers in exponential format (e.g., 1.23e-5)
11. **Integers only** - Files without decimal points
12. **Negative numbers** - Handling of negative values

### Edge Cases & Error Handling
13. **Single line file** - Insufficient data to determine format
14. **Empty file** - No content
15. **Integration test** - Verify safely() wrapper structure
16. **Non-existent file** - Missing file handling

## Running the Tests

### Option 1: Using RStudio
1. Open RStudio
2. Set working directory to project root: `setwd("E:/dev/SK-Ana")`
3. Run: `source("tests/run_format_detection_tests.R")`

### Option 2: Command Line
```bash
cd E:/dev/SK-Ana
Rscript tests/run_format_detection_tests.R
```

### Option 3: Using testthat directly
```r
library(testthat)
test_file("tests/testthat/test_file_format_detection.R")
```

### Option 4: Run all tests including format detection
```bash
cd E:/dev/SK-Ana
Rscript tests/testthat.R
```

## Test Requirements

The following R packages are required:
- `testthat` - Testing framework
- `purrr` - For safely() wrapper (already used in project)

If missing, the test runner will attempt to install them automatically.

## Test Data

All test data is generated dynamically using temporary files, so no external test data files are required. Each test:
1. Creates a temporary file with specific content
2. Runs the detection function
3. Verifies the results
4. Cleans up the temporary file

## Function Being Tested

The `detectFileFormat()` function is located in `server_files/project.R` and returns:

```r
list(
  header = TRUE/FALSE,  # Whether first line is a header
  sep = ","|";"|"\t"|" ",  # Field separator
  dec = "."|",",  # Decimal separator
  datStr = "wxd"  # Data structure (default)
)
```

## Expected Behavior

### Detection Logic

#### Separator Detection
- Counts occurrences of comma, semicolon, tab, and space in the first line
- Selects the separator with the highest count
- Defaults to tab if no separator is clearly dominant

#### Decimal Detection
- Scans lines 2-5 for patterns like `\d+\.\d+` (dot) or `\d+,\d+` (comma)
- Counts matches for each pattern
- Uses comma decimal if comma pattern is more common, otherwise dot

#### Header Detection
- Attempts to parse first line values as numeric
- If more than 50% of values are non-numeric, assumes header is present
- Otherwise, assumes no header

## Interpreting Results

### Successful Test Run
```
=================================================================
Running File Format Detection Tests
=================================================================

✓ | 16  | File Format Detection

=================================================================
Test Summary
=================================================================
Total tests: 16
Passed: 16
Failed: 0

✓ All tests passed!
```

### Failed Test Example
```
Failed: detects CSV format with comma separator
Expected: ","
Actual: ";"
```

This would indicate an issue with separator detection logic.

## Adding New Tests

To add a new test case:

1. Open `tests/testthat/test_file_format_detection.R`
2. Add a new `test_that()` block following the existing pattern:

```r
test_that("describes what is being tested", {
  content <- c(
    "first line",
    "second line",
    "third line"
  )
  file <- create_test_file(content)
  
  result <- detectFileFormat(file)
  
  expect_false(is.null(result))
  expect_equal(result$sep, "expected_separator")
  expect_equal(result$dec, "expected_decimal")
  expect_true/false(result$header)
  
  unlink(file)
})
```

## Troubleshooting

### Tests fail to find detectFileFormat
- Verify the source path in line 9 of `test_file_format_detection.R`
- Ensure `server_files/project.R` contains the function

### Package installation fails
- Install manually: `install.packages(c("testthat", "purrr"))`
- Check internet connection for CRAN access

### Tests hang or freeze
- Check for infinite loops in detection logic
- Verify temp file cleanup (look for orphaned temp files)

## Integration with CI/CD

These tests can be integrated into continuous integration pipelines:

```yaml
# Example GitHub Actions workflow
- name: Run format detection tests
  run: Rscript tests/run_format_detection_tests.R
```

The test runner exits with status 0 on success, 1 on failure, making it suitable for automated testing.

## Performance

All 16 tests typically complete in **less than 1 second** on modern hardware, as they:
- Use small synthetic data
- Only read first few lines of files
- Clean up immediately after each test

## Related Files

- `server_files/project.R` - Contains detectFileFormat() implementation
- `ui_files/project.R` - UI with "Auto" format selection
- `tests/testthat/test_file_format_detection.R` - Test suite
- `tests/run_format_detection_tests.R` - Standalone test runner
