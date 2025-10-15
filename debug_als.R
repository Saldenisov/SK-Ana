# Comprehensive ALS debugging script
cat("=== ALS Debug Session Started ===\n")
cat("Timestamp:", as.character(Sys.time()), "\n")
cat("Platform:", .Platform$OS.type, "\n")
cat("R Version:", R.version.string, "\n")
cat("Working Directory:", getwd(), "\n")
cat("User:", Sys.getenv("USER"), "\n")

# Test 1: Package loading
cat("\n=== Test 1: Package Loading ===\n")
required_packages <- c('nnls', 'Iso', 'mvtnorm', 'fields', 'Rsolnp', 'deSolve', 'msm', 'changepoint', 'outliers', 'rgenoud', 'NMFN', 'shiny', 'DT', 'callr', 'processx')
for(pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓", pkg, "loaded successfully\n")
  }, error = function(e) {
    cat("✗", pkg, "failed:", e$message, "\n")
  })
}

# Test 2: Temporary file system
cat("\n=== Test 2: Temporary File System ===\n")
temp_dir <- tempdir()
cat("Temp directory:", temp_dir, "\n")
cat("Temp dir exists:", file.exists(temp_dir), "\n")
cat("Temp dir writable:", file.access(temp_dir, 2) == 0, "\n")

test_file <- tempfile(pattern = "als_test_", tmpdir = temp_dir, fileext = ".log")
cat("Test temp file:", test_file, "\n")
tryCatch({
  writeLines("test content", test_file)
  content <- readLines(test_file)
  file.remove(test_file)
  cat("✓ Temporary file operations successful\n")
}, error = function(e) {
  cat("✗ Temporary file operations failed:", e$message, "\n")
})

# Test 3: Basic nnls functionality
cat("\n=== Test 3: NNLS Functionality ===\n")
tryCatch({
  library(nnls)
  A <- matrix(c(1,1,2,3), nrow=2)
  b <- c(6,14)
  result <- nnls(A, b)
  cat("✓ NNLS computation successful\n")
  cat("Result:", result$x, "\n")
}, error = function(e) {
  cat("✗ NNLS computation failed:", e$message, "\n")
})

# Test 4: callr basic functionality
cat("\n=== Test 4: callr Basic Functionality ===\n")
tryCatch({
  library(callr)
  simple_func <- function(x) x * 2
  result <- r(simple_func, args = list(5))
  cat("✓ callr::r works, result:", result, "\n")
}, error = function(e) {
  cat("✗ callr::r failed:", e$message, "\n")
})

# Test 5: callr background process
cat("\n=== Test 5: callr Background Process ===\n")
tryCatch({
  library(callr)
  bg_func <- function(x) {
    Sys.sleep(0.5)  # Simulate work
    x * 3
  }
  bg_proc <- r_bg(bg_func, args = list(7))
  
  # Wait for completion
  timeout <- 10  # seconds
  start_time <- Sys.time()
  while(bg_proc$is_alive() && difftime(Sys.time(), start_time, units="secs") < timeout) {
    Sys.sleep(0.1)
  }
  
  if(bg_proc$is_alive()) {
    cat("✗ callr::r_bg timeout after", timeout, "seconds\n")
    bg_proc$kill()
  } else {
    result <- bg_proc$get_result()
    cat("✓ callr::r_bg works, result:", result, "\n")
  }
}, error = function(e) {
  cat("✗ callr::r_bg failed:", e$message, "\n")
})

# Test 6: callr with packages
cat("\n=== Test 6: callr with Package Loading ===\n")
tryCatch({
  als_like_func <- function() {
    library(nnls)
    library(Iso)
    A <- matrix(c(1,1,2,3,4,5), nrow=3)
    b <- c(6,14,22)
    result <- nnls(A, b)
    list(success = TRUE, result = result$x)
  }
  
  bg_proc <- r_bg(als_like_func, package = TRUE)
  
  # Wait for completion
  timeout <- 15
  start_time <- Sys.time()
  while(bg_proc$is_alive() && difftime(Sys.time(), start_time, units="secs") < timeout) {
    Sys.sleep(0.1)
  }
  
  if(bg_proc$is_alive()) {
    cat("✗ callr with packages timeout\n")
    bg_proc$kill()
  } else {
    result <- bg_proc$get_result()
    cat("✓ callr with packages works\n")
    cat("Result success:", result$success, "\n")
    cat("NNLS result:", result$result, "\n")
  }
}, error = function(e) {
  cat("✗ callr with packages failed:", e$message, "\n")
})

# Test 7: Environment variables
cat("\n=== Test 7: Environment Variables ===\n")
cat("PATH:", Sys.getenv("PATH"), "\n")
cat("R_HOME:", R.home(), "\n")
cat("R_LIBS:", paste(.libPaths(), collapse=":"), "\n")

# Test 8: Memory and system resources
cat("\n=== Test 8: System Resources ===\n")
cat("Memory info:\n")
system("cat /proc/meminfo | head -5", ignore.stdout = FALSE)
cat("\nDisk space:\n")
system("df -h /SK-Ana", ignore.stdout = FALSE)

# Test 9: Process limits
cat("\n=== Test 9: Process Limits ===\n")
system("ulimit -a", ignore.stdout = FALSE)

cat("\n=== ALS Debug Session Completed ===\n")