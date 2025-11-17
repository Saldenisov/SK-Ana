# FINAL TEST: Verify wrapped functions don't crash R
# ===================================================
# This test intentionally calls functions with bad inputs
# If R doesn't crash, the error handling is working!

cat("\n")
cat("=========================================\n")
cat("FINAL CRASH PREVENTION TEST\n")
cat("=========================================\n\n")

source("global.R")
source("server_files/helpers.R")
source("server_files/process_utils.R")

cat("Files sourced successfully.\n\n")

# Counter
tests <- 0
survived <- 0

cat("Running tests with intentionally bad inputs...\n")
cat("(If R doesn't crash, the error handler works!)\n\n")

# Test 1
tests <- tests + 1
cat(sprintf("[%d] GetColors(-999)... ", tests))
result <- GetColors(-999)
cat("SURVIVED\n")
survived <- survived + 1

# Test 2
tests <- tests + 1
cat(sprintf("[%d] string2Num('!@#$%%^&*()')... ", tests))
result <- string2Num("!@#$%^&*()")
cat("SURVIVED\n")
survived <- survived + 1

# Test 3
tests <- tests + 1
cat(sprintf("[%d] col2tr('invalidcolor', 999)... ", tests))
result <- col2tr("invalidcolor", 999)
cat("SURVIVED\n")
survived <- survived + 1

# Test 4
tests <- tests + 1
cat(sprintf("[%d] showMSE(NULL, NULL, NULL)... ", tests))
result <- showMSE(NULL, NULL, NULL)
cat("SURVIVED\n")
survived <- survived + 1

# Test 5
tests <- tests + 1
cat(sprintf("[%d] getExternalSpectra(NULL, NULL, NULL, NULL)... ", tests))
result <- getExternalSpectra(NULL, NULL, NULL, NULL)
cat("SURVIVED\n")
survived <- survived + 1

# Test 6
tests <- tests + 1
cat(sprintf("[%d] process_id(list('bad input'))... ", tests))
result <- process_id(list("bad input"))
cat("SURVIVED\n")
survived <- survived + 1

# Test 7
tests <- tests + 1
cat(sprintf("[%d] string2Expr(NA)... ", tests))
result <- string2Expr(NA)
cat("SURVIVED\n")
survived <- survived + 1

# Summary
cat("\n")
cat("=========================================\n")
cat(sprintf("RESULT: %d/%d tests survived\n", survived, tests))
cat("=========================================\n\n")

if (survived == tests) {
  cat("✓✓✓ SUCCESS ✓✓✓\n")
  cat("All wrapped functions caught errors gracefully!\n")
  cat("The error handler is working perfectly.\n")
  cat("R did NOT crash!\n\n")
  quit(status = 0)
} else {
  cat("✗ FAILURE: Some functions crashed R\n\n")
  quit(status = 1)
}
