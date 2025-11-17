# Test Error Handling Wrapper
# ============================
# Tests that wrapped functions catch errors properly

cat("Testing Error Handler Functionality\n")
cat("====================================\n\n")

# Source global which loads error_handler.R
source("global.R")

# Source server files needed for tests
source("server_files/helpers.R")
source("server_files/process_utils.R")

# Manually define lof for testing (SVD.R has Shiny dependencies)
lof <- safely(function(model, data) {
  100 * (
    sum((data - model)^2, na.rm = TRUE) /
      sum(data^2, na.rm = TRUE)
  )^0.5
}, return_on_error = 100)

test_count <- 0
pass_count <- 0

run_test <- function(test_name, test_func) {
  test_count <<- test_count + 1
  cat(sprintf("[Test %d] %s... ", test_count, test_name))
  
  tryCatch({
    result <- test_func()
    if (result) {
      pass_count <<- pass_count + 1
      cat("✓ PASS\n")
    } else {
      cat("✗ FAIL\n")
    }
  }, error = function(e) {
    cat("✗ ERROR:", e$message, "\n")
  })
}

# Test 1: safely() wrapper exists
run_test("safely() function exists", function() {
  exists("safely")
})

# Test 2: main_error_handler exists
run_test("main_error_handler() exists", function() {
  exists("main_error_handler")
})

# Test 3: Wrapped function returns correct result on success
run_test("Wrapped function returns correct result", function() {
  test_func <- safely(function(x) x * 2, return_on_error = -1)
  result <- test_func(5)
  result == 10
})

# Test 4: Wrapped function catches error and returns default
run_test("Wrapped function catches error", function() {
  suppressWarnings({
    test_func <- safely(function(x) stop("Test error"), return_on_error = -999)
    result <- test_func(5)
    identical(result, -999)
  })
})

# Test 5: GetColors is wrapped and works
run_test("GetColors() is wrapped and works", function() {
  colors <- GetColors(5)
  length(colors) == 5
})

# Test 6: GetColors handles error gracefully
run_test("GetColors() handles errors", function() {
  suppressWarnings({
    # Try with invalid input - should return NULL instead of crashing
    result <- GetColors(-5)  # Negative number
    is.null(result)
  })
})

# Test 7: string2Num is wrapped
run_test("string2Num() is wrapped and works", function() {
  result <- string2Num("2+2")
  !is.null(result)
})

# Test 8: string2Num handles error gracefully  
run_test("string2Num() handles errors", function() {
  suppressWarnings({
    result <- string2Num("invalid expression ###")
    is.null(result)
  })
})

# Test 9: showMSE is wrapped
run_test("showMSE() is wrapped and works", function() {
  result <- showMSE("tileDel", 1:3, 2)
  result == TRUE
})

# Test 10: lof is wrapped and works
run_test("lof() is wrapped and works", function() {
  m <- matrix(c(1,2,3,4), nrow=2)
  result <- lof(m, m)
  abs(result) < 1e-10
})

# Test 11: lof handles error gracefully
run_test("lof() handles errors", function() {
  # Try with NULL - should return default instead of crashing
  result <- lof(NULL, NULL)
  !is.null(result) && result == 100  # Our default for lof
})

# Test 12: process_status is wrapped
run_test("process_status() is wrapped and works", function() {
  result <- process_status(NULL)
  is.list(result)
})

# Test 13: Test function with multiple wrapped calls
run_test("Multiple wrapped function calls work", function() {
  colors1 <- GetColors(3)
  colors2 <- GetColors(5)
  result1 <- showMSE("tileDel", 1:2, 2)
  result2 <- showMSE("other", 1:2, 1)
  
  length(colors1) == 3 && length(colors2) == 5 && result1 == TRUE && result2 == FALSE
})

# Summary
cat("\n")
cat("=====================================\n")
cat(sprintf("Test Results: %d/%d passed (%.1f%%)\n", 
            pass_count, test_count, 
            (pass_count/test_count)*100))
cat("=====================================\n")

if (pass_count == test_count) {
  cat("\n✓ All tests passed! Error handling is working correctly.\n")
  quit(status = 0)
} else {
  cat(sprintf("\n✗ %d test(s) failed.\n", test_count - pass_count))
  quit(status = 1)
}
