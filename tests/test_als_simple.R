# Simplified ALS test to isolate issues
library(nnls)
library(callr)

# Simple ALS-like function
simple_als <- function(data_matrix, num_components = 2) {
  cat("Starting simple ALS with matrix size:", dim(data_matrix), "\n")
  
  # Initialize with random values
  nrow_data <- nrow(data_matrix)
  ncol_data <- ncol(data_matrix)
  
  C <- matrix(abs(rnorm(nrow_data * num_components)), nrow = nrow_data, ncol = num_components)
  S <- matrix(abs(rnorm(ncol_data * num_components)), nrow = ncol_data, ncol = num_components)
  
  # Simple iteration
  for(iter in 1:5) {
    # Update S
    for(j in 1:ncol_data) {
      result <- nnls(C, data_matrix[, j])
      S[j, ] <- result$x
    }
    
    # Update C  
    for(i in 1:nrow_data) {
      result <- nnls(S, data_matrix[i, ])
      C[i, ] <- result$x
    }
    
    cat("Iteration", iter, "completed\n")
  }
  
  return(list(C = C, S = S, iterations = 5))
}

# Test data
test_data <- matrix(abs(rnorm(20)), nrow = 4, ncol = 5)
cat("Test data created\n")

# Test in background process
cat("Testing ALS in background process...\n")
bg_proc <- r_bg(simple_als, args = list(test_data, 2), package = TRUE)

timeout <- 30
start_time <- Sys.time()
while(bg_proc$is_alive() && difftime(Sys.time(), start_time, units="secs") < timeout) {
  Sys.sleep(0.5)
  cat(".", fill = FALSE)
}

if(bg_proc$is_alive()) {
  cat("\nTimeout reached, killing process\n")
  bg_proc$kill()
} else {
  cat("\nProcess completed successfully\n")
  result <- bg_proc$get_result()
  cat("Result dimensions - C:", dim(result$C), "S:", dim(result$S), "\n")
  cat("Iterations completed:", result$iterations, "\n")
}