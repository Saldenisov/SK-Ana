# Quick Start: Running File Format Detection Tests

## Fastest Way to Run Tests

Open PowerShell or Command Prompt in the project directory and run:

```powershell
cd E:\dev\SK-Ana
Rscript tests\run_format_detection_tests.R
```

## What You'll See

```
=================================================================
Running File Format Detection Tests
=================================================================

✓ |  16 | File Format Detection

=================================================================
Test Summary
=================================================================
Total tests: 16
Passed: 16
Failed: 0

✓ All tests passed!
```

## If You Don't Have Rscript

### Using RStudio:
1. Open RStudio
2. Open file: `tests/run_format_detection_tests.R`
3. Click "Source" button

### Using R Console:
```r
setwd("E:/dev/SK-Ana")
source("tests/run_format_detection_tests.R")
```

## What the Tests Check

✅ CSV files (comma-separated)  
✅ TSV files (tab-separated)  
✅ European format (semicolon + comma decimal)  
✅ Space-separated files  
✅ Files with/without headers  
✅ Scientific notation  
✅ Negative numbers  
✅ Edge cases (empty files, single lines, etc.)  

## If Tests Fail

1. Check the error message for which test failed
2. Look at `tests/testthat/test_file_format_detection.R` to see what was expected
3. Verify the `detectFileFormat()` function in `server_files/project.R`

## Need More Info?

See `tests/README_FORMAT_DETECTION_TESTS.md` for comprehensive documentation.

## Test Files Created

- `tests/testthat/test_file_format_detection.R` - Main test suite (16 tests)
- `tests/run_format_detection_tests.R` - Standalone test runner
- `tests/README_FORMAT_DETECTION_TESTS.md` - Full documentation
- `tests/QUICKSTART_TESTS.md` - This file
