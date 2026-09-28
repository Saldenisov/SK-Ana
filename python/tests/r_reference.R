args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) stop("Expected root, matrix, C0, S0, output directory")
root <- args[1]
mat <- as.matrix(read.csv(args[2], header = FALSE))
C0 <- as.matrix(read.csv(args[3], header = FALSE))
S0 <- as.matrix(read.csv(args[4], header = FALSE))
out <- args[5]

# Run the repository's actual numerical functions without starting Shiny.
safely <- function(f, return_on_error = NULL) f
lof <- function(model, data) 100 * sqrt(sum((data - model)^2) / sum(data^2))
load_functions <- function(path, names) {
  for (expr in parse(path)) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.symbol(expr[[2]]) &&
        as.character(expr[[2]]) %in% names) eval(expr, envir = .GlobalEnv)
  }
}
load_functions(file.path(root, "server_files", "ALS_parts", "als_core_algorithms.R"),
               c("getC", "getS", "convolve_spectrum"))
load_functions(file.path(root, "server_files", "ALS_parts", "als_iteration_and_external_spectra.R"),
               "myals")

s <- svd(mat, nu = 2, nv = 2)
write.csv(matrix(s$d, nrow = 1), file.path(out, "singular.csv"), row.names = FALSE)
write.csv(getC(S0, mat, C0, nonnegC = TRUE), file.path(out, "get_c.csv"), row.names = FALSE)
write.csv(getC(S0, mat, C0, nonnegC = FALSE), file.path(out, "get_c_free.csv"), row.names = FALSE)
write.csv(getC(S0, mat, C0, nonnegC = TRUE, closeC = TRUE, wCloseC = 10),
          file.path(out, "get_c_closure.csv"), row.names = FALSE)
write.csv(getS(C0, mat, S0, 1:ncol(mat), TRUE, FALSE, NULL, TRUE, 0,
               FALSE, TRUE, 1, "intensity"), file.path(out, "get_s.csv"), row.names = FALSE)
write.csv(getS(C0, mat, S0, 1:ncol(mat), c(TRUE, FALSE), FALSE, NULL, FALSE, 0,
               FALSE, TRUE, 1, "intensity"), file.path(out, "get_s_mixed.csv"), row.names = FALSE)
write.csv(matrix(convolve_spectrum(S0[, 1], 1.5), nrow = 1),
          file.path(out, "convolution.csv"), row.names = FALSE)
fit <- myals(C0, mat, S0, thresh = 1e-7, maxiter = 150, nonnegC = TRUE,
             nonnegS = TRUE, optS1st = FALSE, normS = TRUE, uniS = FALSE,
             smooth = 0, silent = TRUE, SumS = FALSE)
write.csv(fit$C %*% t(fit$S), file.path(out, "als_model.csv"), row.names = FALSE)
write.csv(matrix(c(fit$lof, fit$rss, fit$iter), nrow = 1),
          file.path(out, "als_metrics.csv"), row.names = FALSE)
