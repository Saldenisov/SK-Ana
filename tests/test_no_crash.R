# Test that wrapped functions don't crash the application
# ========================================================

cat("Testing That Wrapped Functions Don't Crash\n")
cat("==========================================\n\n")

# Source global
source("global.R")
source("server_files/helpers.R")
source("server_files/process_utils.R")

cat("✓ All files sourced successfully\n\n")

test_passed <- 0
test_total <- 0

# Test helper
test_no_crash <- function(name, expr) {
  test_total <<- test_total + 1
  cat(sprintf("[%d] Testing %s... ", test_total, name))
  
  tryCatch({
    result <- expr
    cat("✓ NO CRASH\n")
    test_passed <<- test_passed + 1
    return(TRUE)
  }, error = function(e) {
    cat("✗ CRASHED:", e$message, "\n")
    return(FALSE)
  })
}

# Test 1: GetColors with valid input
test_no_crash("GetColors(5)", {
  GetColors(5)
})

# Test 2: GetColors with negative (should handle gracefully)
test_no_crash("GetColors(-5) [error case]", {
  GetColors(-5)
})

# Test 3: string2Num with valid input
test_no_crash("string2Num('2+2')", {
  string2Num("2+2")
})

# Test 4: string2Num with invalid input  
test_no_crash("string2Num('###') [error case]", {
  string2Num("invalid###")
})

# Test 5: showMSE valid
test_no_crash("showMSE('tileDel', 1:3, 2)", {
  showMSE("tileDel", 1:3, 2)
})

# Test 6: showMSE with NULL (error case)
test_no_crash("showMSE(NULL, NULL, NULL) [error case]", {
  showMSE(NULL, NULL, NULL)
})

# Test 7: process_status
test_no_crash("process_status(NULL)", {
  process_status(NULL)
})

# Test 8: col2tr
test_no_crash("col2tr('red', 0.5)", {
  col2tr("red", 0.5)
})

# Test 9: col2tr with invalid color (error case)
test_no_crash("col2tr('notacolor', 0.5) [error case]", {
  col2tr("notacolor", 0.5)
})

# Test 10: getExternalSpectra with NULL (error case)
test_no_crash("getExternalSpectra(NULL, NULL, NULL, NULL) [error case]", {
  getExternalSpectra(NULL, NULL, NULL, NULL)
})

# Summary
cat("\n")
cat("==========================================\n")
cat(sprintf("Results: %d/%d tests passed (no crashes)\n", test_passed, test_total))
cat("==========================================\n\n")

if (test_passed == test_total) {
  cat("✓ SUCCESS: All functions handled errors without crashing!\n")
  cat("   The error handler is working correctly.\n")
  quit(status = 0)
} else {
  failed <- test_total - test_passed
  cat(sprintf("✗ FAILED: %d function(s) crashed unexpectedly\n", failed))
  quit(status = 1)
}
