args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) stop("Expected root, time CSV, matrix CSV, output directory")
root <- args[1]
times <- scan(args[2], sep = ",", quiet = TRUE)
mat <- as.matrix(read.csv(args[3], header = FALSE))
out <- args[4]

safely <- function(f, return_on_error = NULL) f
load_functions <- function(path, names) {
  for (expr in parse(path)) {
    if (is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=") &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% names)
      eval(expr, envir = .GlobalEnv)
  }
}
load_functions(file.path(root, "server_files", "kinetParsers.R"), "kinParse")
load_functions(file.path(root, "server_files", "kinetSpectrokineticModel.R"),
               c("C.model", "kinet", "spectra", "model"))

parsed <- kinParse(c("A -> B ; 0.7 / 1.5"))
state <- matrix(c(1, 0), nrow = 2, dimnames = list(c("A", "B"), NULL))
parms <- list(startd = length(times), state = state, kReac = parsed$kReac,
              D = parsed$D, L = parsed$L, times = times,
              active = c(A = TRUE, B = TRUE), eps = c(A = 1, B = 0.8),
              wavl = seq_len(ncol(mat)), mat = mat, nonnegS = FALSE,
              uniS = FALSE, smooth = 0, delay = times)
conc <- kinet(numeric(0), parms)
fit <- model(numeric(0), parms)
write.csv(conc, file.path(out, "kinetic_concentrations.csv"), row.names = FALSE)
write.csv(fit, file.path(out, "kinetic_model.csv"), row.names = FALSE)
if (length(args) > 4L && args[5] == "fit") {
  load_functions(file.path(root, "server_files", "kinetHypercubeTransfo.R"), "parExpand")
  load_functions(file.path(root, "server_files", "kinetBayesian.R"),
                 c("uresid", "uchisq", "ulogL", "ulogP", "mulogP"))
  parms$logPri <- function(x) -0.5 * sum(x^2)
  paropt <- list(logk_1 = paste0("tnorm(", log(0.7), ",", log(1.5), ")"))
  optimum <- Rsolnp::solnp(pars = 0, fun = mulogP, LB = -3, UB = 3,
                           control = list(tol = 1e-8, trace = 0),
                           paropt = paropt, parms = parms)
  write.csv(matrix(c(optimum$pars[1], exp(parExpand(optimum$pars, paropt)$logk_1),
                     optimum$convergence), nrow = 1),
            file.path(out, "kinetic_fit.csv"), row.names = FALSE)
}
