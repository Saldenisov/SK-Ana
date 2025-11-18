#!/usr/bin/env Rscript
# Install required packages for testing
# Run this script with administrator privileges or in R console

cat("=================================================================\n")
cat("Installing Test Packages\n")
cat("=================================================================\n\n")

# Required packages
packages <- c("testthat", "purrr")

# Try to create user library if it doesn't exist
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  cat("Creating user library directory:", user_lib, "\n")
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}

# Check which packages are already installed
installed <- sapply(packages, function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
})

cat("\nPackage status:\n")
for (i in seq_along(packages)) {
  status <- if(installed[i]) "✓ INSTALLED" else "✗ NOT INSTALLED"
  cat(sprintf("  %s: %s\n", packages[i], status))
}

# Install missing packages
missing <- packages[!installed]

if (length(missing) > 0) {
  cat("\nInstalling missing packages:", paste(missing, collapse = ", "), "\n\n")
  
  # Try to install to default library first
  tryCatch({
    install.packages(missing, repos = "https://cran.r-project.org")
    cat("\n✓ Packages installed successfully!\n")
  }, error = function(e) {
    cat("\nDefault library not writable. Trying user library...\n")
    
    # Try user library
    tryCatch({
      install.packages(missing, repos = "https://cran.r-project.org", lib = user_lib)
      cat("\n✓ Packages installed to user library successfully!\n")
      cat("User library location:", user_lib, "\n")
    }, error = function(e2) {
      cat("\n❌ Installation failed!\n")
      cat("\nError message:", conditionMessage(e2), "\n\n")
      cat("Please try one of these options:\n")
      cat("1. Run R as Administrator and execute:\n")
      cat("   install.packages(c('testthat', 'purrr'))\n\n")
      cat("2. Or in RStudio, go to: Tools > Install Packages\n")
      cat("   and enter: testthat, purrr\n\n")
      quit(status = 1)
    })
  })
} else {
  cat("\n✓ All required packages are already installed!\n")
}

cat("\n=================================================================\n")
cat("You can now run the tests:\n")
cat("  Rscript tests/run_format_detection_tests.R\n")
cat("=================================================================\n")
