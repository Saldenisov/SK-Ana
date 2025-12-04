

# Functions ####

# Convolution functions for broadening ####
convolve_spectrum <- function(spectrum, sigma) {
  # Apply Gaussian convolution to a single spectrum
  # Args:
  #   spectrum: vector of spectral intensities
  #   sigma: standard deviation of Gaussian kernel (broadening parameter)
  # Returns:
  #   convolved spectrum
  
  if (sigma <= 0 || !is.finite(sigma)) {
    return(spectrum)
  }
  
  # Create Gaussian kernel
  # Kernel size should be ~6*sigma to capture most of the distribution
  kernel_size <- max(3, ceiling(3 * sigma))
  x <- (-kernel_size):kernel_size
  kernel <- exp(-x^2 / (2 * sigma^2))
  kernel <- kernel / sum(kernel)  # Normalize
  
  # Convolve using stats::convolve with "same" mode approximation
  # Pad the signal to handle edges
  n <- length(spectrum)
  padded <- c(rep(spectrum[1], kernel_size), spectrum, rep(spectrum[n], kernel_size))
  
  # Convolve
  conv_result <- stats::convolve(padded, rev(kernel), type = "open")
  
  # Extract center part (same length as input)
  start_idx <- kernel_size + 1
  end_idx <- start_idx + n - 1
  result <- conv_result[start_idx:end_idx]
  
  return(result)
}

convolve_S_matrix <- function(S, G_params) {
  # Apply convolution to all spectra in S matrix
  # Args:
  #   S: matrix of pure spectra (rows = wavelengths, cols = components)
  #   G_params: matrix of broadening parameters (rows = samples, cols = components)
  #             Each component has 1 parameter: [sigma_1, sigma_2, ...]
  # Returns:
  #   List with one element per sample, each containing broadened S matrix
  
  n_samples <- nrow(G_params)
  n_components <- ncol(S)
  
  S_broadened_list <- vector("list", n_samples)
  
  for (i in 1:n_samples) {
    S_broad <- S
    for (k in 1:n_components) {
      # Get sigma parameter for this component and this sample
      sigma <- G_params[i, k]
      
      # Apply single convolution
      S_broad[, k] <- convolve_spectrum(S[, k], sigma)
    }
    S_broadened_list[[i]] <- S_broad
  }
  
  return(S_broadened_list)
}

optimize_broadening_single <- function(data_row, C_row, S, G_init, sigma_max = NULL, broadening_vec = NULL) {
  # Optimize broadening parameters using three-stage grid refinement
  # Stage 1: Coarse grid (0 to max, 10 points)
  # Stage 2: Medium grid (around best from stage 1, 10 points)
  # Stage 3: Fine grid (around best from stage 2, 10 points)
  # Args:
  #   data_row: observed spectrum (vector)
  #   C_row: concentration coefficients for this sample (vector)
  #   S: pure spectral profiles (matrix: wavelengths x components)
  #   G_init: initial broadening parameters (vector of length n_components)
  #   sigma_max: maximum allowed sigma (upper bound)
  #   broadening_vec: logical vector indicating which components to optimize (optional)
  # Returns:
  #   Optimized G parameters (vector of length n_components)
  
  n_components <- ncol(S)
  spectral_range <- nrow(S)
  
  # Set bounds
  sigma_min <- 0  # Start from 0
  if (is.null(sigma_max)) {
    sigma_max <- 0.10 * spectral_range  # 10%
  }
  
  # Default: optimize all components
  if (is.null(broadening_vec)) {
    broadening_vec <- rep(TRUE, n_components)
  }
  
  # Helper function to evaluate error for given G
  eval_error <- function(G_vec) {
    recon <- rep(0, length(data_row))
    for (j in 1:n_components) {
      S_broad <- convolve_spectrum(S[, j], G_vec[j])
      recon <- recon + C_row[j] * S_broad
    }
    return(sum((data_row - recon)^2))
  }
  
  # Optimize each component independently (only enabled ones)
  G_opt <- G_init
  
  for (k in 1:n_components) {
    # Skip if this component is not enabled for broadening
    if (!broadening_vec[k]) {
      next
    }
    # Stage 1: Coarse grid search (0 to max, 10 points)
    grid_coarse <- seq(sigma_min, sigma_max, length.out = 10)
    best_sigma <- G_init[k]
    best_error <- Inf
    
    for (sigma_test in grid_coarse) {
      G_temp <- G_opt
      G_temp[k] <- sigma_test
      error <- eval_error(G_temp)
      
      if (error < best_error) {
        best_error <- error
        best_sigma <- sigma_test
      }
    }
    
    # Stage 2: Medium grid (±10% of range around best from stage 1, 10 points)
    range_medium <- (sigma_max - sigma_min) * 0.1
    grid_medium_min <- max(sigma_min, best_sigma - range_medium)
    grid_medium_max <- min(sigma_max, best_sigma + range_medium)
    grid_medium <- seq(grid_medium_min, grid_medium_max, length.out = 10)
    
    for (sigma_test in grid_medium) {
      G_temp <- G_opt
      G_temp[k] <- sigma_test
      error <- eval_error(G_temp)
      
      if (error < best_error) {
        best_error <- error
        best_sigma <- sigma_test
      }
    }
    
    # Stage 3: Fine grid (±2% of range around best from stage 2, 10 points)
    range_fine <- (sigma_max - sigma_min) * 0.02
    grid_fine_min <- max(sigma_min, best_sigma - range_fine)
    grid_fine_max <- min(sigma_max, best_sigma + range_fine)
    grid_fine <- seq(grid_fine_min, grid_fine_max, length.out = 10)
    
    for (sigma_test in grid_fine) {
      G_temp <- G_opt
      G_temp[k] <- sigma_test
      error <- eval_error(G_temp)
      
      if (error < best_error) {
        best_error <- error
        best_sigma <- sigma_test
      }
    }
    
    # Update this component's sigma with refined value
    G_opt[k] <- best_sigma
  }
  
  return(G_opt)
}

getC <- safely(function(S, data, C, nonnegC = TRUE,
                 nullC = NA, closeC = FALSE, wCloseC = 0,
                 G = NULL) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  # 2025-11-27 : added broadening support via G parameter
  
  S[which(is.nan(S))] <- 1
  
  # Soft closure constraint
  if (closeC) {
    if (wCloseC != 0) {
      S <- rbind(S, rep(wCloseC, ncol(S)))
      data <- cbind(data, rep(wCloseC, nrow(data)))
    }
  }
  
  # Determine if broadening is enabled
  use_broadening <- !is.null(G) && nrow(G) == nrow(data)
  
  for (i in 1:nrow(data)) {
    # Apply broadening if enabled
    S_work <- S
    if (use_broadening) {
      # Convolve each component with its broadening parameter
      for (k in 1:ncol(S)) {
        sigma <- G[i, k]
        S_work[, k] <- convolve_spectrum(S[, k], sigma)
      }
    }
    
    if (nonnegC) {
      cc <- try(nnls::nnls(S_work, data[i, ]))
    } else {
      cc <- try(qr.coef(qr(S_work), data[i, ]))
    }
    
    if (class(cc) == "try-error") {
      sol <- rep(1, ncol(S))
    } else {
      sol <- if (nonnegC) {
        cc$x
      } else {
        cc
      }
    }
    
    cc1 <- rep(NA, ncol(C))
    cc1[is.na(cc1)] <- sol
    C[i, ] <- cc1
  }
  
  if (!anyNA(nullC)) {
    if (ncol(nullC) == ncol(C)) {
      C <- C * nullC
    }
  }
  
  # Hard closure constrained (replaced by soft)
  # if(closeC)
  #   C = C / rowSums(C, na.rm = TRUE)
  
  return(C)
}, return_on_error = NULL)

getS <- safely(function(C, data, S, xS, nonnegS, uniS,
                 S0, normS, smooth, SumS, hardS0,
                 wHardS0, normMode = "intensity", G = NULL) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  # 2017-12-07 : replaced direct substitution of S0 by direct elimination
  # 2025-11-19 : added per-component non-negativity constraints
  # 2025-11-27 : added broadening support via G parameter
  
  C[which(is.nan(C))] <- 1
  
  # Handle nonnegS as either boolean (global) or vector (per-component)
  if (length(nonnegS) == 1) {
    # Global constraint: replicate for all components
    nonnegS_vec <- rep(nonnegS, ncol(C))
  } else {
    # Per-component constraints
    nonnegS_vec <- nonnegS
    if (length(nonnegS_vec) != ncol(C)) {
      warning("nonnegS vector length mismatch, using global TRUE")
      nonnegS_vec <- rep(TRUE, ncol(C))
    }
  }
  
  if (!is.null(S0)) {
    nS0 <- ncol(S0)
    if (hardS0) {
      
      if(ncol(S) == nS0)
        return(S0) # Nothing left to optimize...
      
      # Enforce equality to external spectrum by direct elimination
      C0 <- matrix(C[, 1:nS0], ncol = nS0)
      C <- matrix(C[, (nS0 + 1):ncol(C)], ncol = ncol(C) - nS0)
      S <- matrix(S[, (nS0 + 1):ncol(S)], ncol = ncol(S) - nS0)
      # Adjust nonnegS_vec for remaining components
      nonnegS_vec <- nonnegS_vec[(nS0 + 1):length(nonnegS_vec)]
      
      contrib <- matrix(0, nrow = nrow(data), ncol = ncol(data))
      for (i in 1:nS0)
        contrib <- contrib + C0[, i] %o% S0[, i]
      
      data <- data - contrib
      data[!is.finite(data)] <- 0
    } else {
      # Augment equations system by weighted constraint
      C <- rbind(C, matrix(0, nrow = nS0, ncol = ncol(C)))
      for (i in 1:nS0)
        C[nrow(C) - nS0 + i, i] <- wHardS0
      data <- rbind(data, wHardS0 * t(S0))
    }
  }
  
  # Check if all components have same constraint (optimization)
  all_same <- all(nonnegS_vec == nonnegS_vec[1])
  
  # Determine if broadening is enabled
  use_broadening <- !is.null(G) && nrow(G) == nrow(C)
  
  if (all_same) {
    # Fast path: all components have same constraint
    for (i in 1:ncol(data)) {
      if (use_broadening) {
        # When broadening is active, we need to solve for unbroadened S
        # Use weighted least squares approach
        # For each wavelength, solve: data[:,i] = sum_j(C[j] * convolve(S[i,:], G[j,:]))
        # This is complex, so we use iterative approach:
        # Build augmented system where each sample contributes with its own broadening
        
        # Stack all samples with weighted contributions
        C_aug <- NULL
        data_aug <- NULL
        for (j in 1:nrow(C)) {
          # For sample j, we need C[j,k] * S_broad[i,k] ≈ data[j,i]
          # Rearrange: we solve for S, then will need to account for broadening
          # Simpler approach: solve for S_broadened first
          C_aug <- rbind(C_aug, matrix(C[j,], nrow=1))
          data_aug <- c(data_aug, data[j, i])
        }
        
        # Solve for effective S at this wavelength
        if (nonnegS_vec[1]) {
          s <- try(nnls::nnls(C_aug, data_aug))
        } else {
          s <- try(qr.coef(qr(C_aug), data_aug))
        }
        
        if (class(s) == "try-error") {
          S[i, ] <- rep(1, ncol(C))
        } else {
          S[i, ] <- if (nonnegS_vec[1]) {
            s$x
          } else {
            s
          }
        }
      } else {
        # Standard ALS without broadening
        if (nonnegS_vec[1]) {
          s <- try(nnls::nnls(C, data[, i]))
        } else {
          s <- try(qr.coef(qr(C), data[, i]))
        }
        
        if (class(s) == "try-error") {
          S[i, ] <- rep(1, ncol(C))
        } else {
          S[i, ] <- if (nonnegS_vec[1]) {
            s$x
          } else {
            s
          }
        }
      }
    }
  } else {
    # Slow path: mixed constraints, solve per-component
    for (i in 1:ncol(data)) {
      # Separate positive and unconstrained components
      idx_pos <- which(nonnegS_vec)
      idx_free <- which(!nonnegS_vec)
      
      if (length(idx_pos) > 0 && length(idx_free) > 0) {
        # Mixed: use iterative approach
        # Initialize solution
        sol <- rep(0, ncol(C))
        
        # First solve for unconstrained components
        C_free <- C[, idx_free, drop = FALSE]
        s_free <- try(qr.coef(qr(C_free), data[, i]))
        if (class(s_free) != "try-error") {
          sol[idx_free] <- s_free
        }
        
        # Then solve for positive components with residual
        resid <- data[, i] - C_free %*% sol[idx_free]
        C_pos <- C[, idx_pos, drop = FALSE]
        s_pos <- try(nnls::nnls(C_pos, resid))
        if (class(s_pos) != "try-error") {
          sol[idx_pos] <- s_pos$x
        }
        
        S[i, ] <- sol
      } else if (length(idx_pos) > 0) {
        # All positive
        s <- try(nnls::nnls(C, data[, i]))
        S[i, ] <- if (class(s) == "try-error") rep(1, ncol(C)) else s$x
      } else {
        # All unconstrained
        s <- try(qr.coef(qr(C), data[, i]))
        S[i, ] <- if (class(s) == "try-error") rep(1, ncol(C)) else s
      }
    }
  }
  
  if (uniS) {
    # Enforce unimodality
    for (i in 1:ncol(S))
      S[, i] <- Iso::ufit(y = S[, i], x = xS)$y
  }
  
  if (smooth != 0) {
    # Smooth spectra
    for (i in 1:ncol(S)) {
      y <- S[, i]
      x <- 1:length(y)
      mod <- loess(y ~ x, span = smooth)
      y <- predict(mod)
      y[y < 0] <- 0
      S[, i] <- y
    }
  }
  
  if (normS) {
    # Spectra normalization
    if (SumS) {
      # Area (L1 norm)
      for (i in 1:ncol(S)) {
        # Use absolute values for normalization if negatives present
        norm_factor <- sum(abs(S[, i]))
        S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
      }
    } else {
      # Intensity-based normalization
      if (normMode == "l1") {
        # L1 norm: divide by sum of absolute values
        for (i in 1:ncol(S)) {
          norm_factor <- sum(abs(S[, i]))
          S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
        }
      } else {
        # Intensity (max): divide by max absolute value
        for (i in 1:ncol(S)) {
          norm_factor <- max(abs(S[, i]))
          S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
        }
      }
    }
  }
  
  if (!is.null(S0) & hardS0) {
    # Combine optimized spectra with constrained ones
    S <- cbind(S0, S)
    C <- cbind(C0, C)
  }
  
  return(S)
}, return_on_error = NULL)

als <- safely(function(
  delay, delayId, wavl, mat, nullC, S, C,
  nAls    = 2, 
  nStart  = 2,
  S0      = NULL,
  maxiter = 100,
  uniS    = NA,
  nonnegS = NA,
  nonnegC = NA,
  thresh  = NA,
  normS   = NA,
  hardS0  = NA,
  wHardS0 = NA,
  optS1st = NA,
  smooth  = FALSE,
  SumS    = NA,
  closeC  = NA,
  wCloseC = NA,
  normMode = "intensity",
  state_file = NULL,
  update_interval = 10,
  broadening = FALSE,
  broadening_max_pct = 10) {
  
  # DOCKER FIX: Explicitly load required packages in subprocess
  # This ensures packages are available in callr background processes
  required_packages <- c('nnls', 'Iso', 'mvtnorm', 'fields', 'Rsolnp', 'deSolve', 'msm', 'changepoint', 'outliers', 'rgenoud', 'NMFN')
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }
  
  # Interface to the core ALS code (myals)
  # Implements sequential dimension growth...
  
  # Run
  res <- list()
  for (n in nStart:nAls) { 
    
    cat(paste0('Running ALS (dim = ', n,')<br/>'))
    
    res[[n]] <- myals(
      C = C, Psi = mat, S = S, 
      xC = delay, xS = wavl,
      maxiter = maxiter,
      uniS = uniS,
      nonnegS = nonnegS,
      nonnegC = nonnegC,
      thresh = thresh,
      normS = normS,
      S0 = S0,
      hardS0 = hardS0,
      wHardS0 = wHardS0,
      optS1st = optS1st,
      smooth = smooth,
      SumS = SumS,
      nullC = nullC,
      closeC = closeC,
      wCloseC = wCloseC,
      normMode = normMode,
      silent = FALSE,
      state_file = state_file,
      update_interval = update_interval,
      broadening = broadening,
      broadening_max_pct = broadening_max_pct
    )
    if(is.null(res[[n]]))
      return(NULL)
    
    res[[n]]$hessian <- NA # Compatibility with Kinet
    
    if (n < nAls) {
      # Prepare next iteration
      S <- res[[n]]$S
      C <- res[[n]]$C
      S <- cbind(S, 1)
      C <- cbind(C, 1)
    }
  }
  
  colnames(res[[nAls]]$S) <- paste0("S_", 1:nAls)
  colnames(res[[nAls]]$C) <- paste0("C_", 1:nAls)
  
  res[[nAls]]$nullC <- nullC
  
  return(res[[nAls]])
  
}, return_on_error = NULL)

myals <- function(C, Psi, S,
                  thresh = 0.001,
                  maxiter = 100,
                  xC = 1:nrow(C),
                  xS = 1:nrow(S),
                  nonnegC = TRUE, nonnegS = TRUE, optS1st = TRUE,
                  normS = TRUE, uniS = FALSE, S0 = NULL, smooth = 0,
                  silent = TRUE, SumS = FALSE, hardS0 = TRUE,
                  wHardS0 = 1.0,
                  nullC = NA, closeC = FALSE, wCloseC = 0,
                  normMode = "intensity",
                  state_file = NULL, update_interval = 10,
                  broadening = FALSE, G = NULL,
                  broadening_max_pct = 10) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  
  RD <- 10^20
  
  resid <- mod <- matrix(0, nrow(Psi), ncol(Psi))
  for (i in 1:nrow(Psi))
    resid[i, ] <- Psi[i, ] - C[i, ] %*% t(S)
  
  initialrss <- oldrss <- sum((resid)^2) / sum(Psi^2)
  
  if (!is.null(S0)) {
    if (!is.matrix(S0)) {
      S0 <- matrix(S0, ncol = 1, nrow = length(S0))
    }
    if (normS) {
      # Spectra normalization for S0
      if (SumS) {
        # Area (L1 norm)
        for (i in 1:ncol(S0)) {
          norm_factor <- sum(abs(S0[, i]))
          S0[, i] <- S0[, i] / ifelse(norm_factor > 0, norm_factor, 1)
        }
      } else {
        if (normMode == "l1") {
          # L1 norm
          for (i in 1:ncol(S0)) {
            norm_factor <- sum(abs(S0[, i]))
            S0[, i] <- S0[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          }
        } else {
          # Intensity (max)
          for (i in 1:ncol(S0)) {
            norm_factor <- max(abs(S0[, i]))
            S0[, i] <- S0[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          }
        }
      }
    }
  }
  
  if (!silent) 
    cat("Initial RSS = ", initialrss, "<br/>")
  
  # Normalize broadening parameter (can be logical scalar or vector)
  n_components <- ncol(C)
  if (length(broadening) == 1) {
    # Scalar: apply to all components
    broadening_vec <- rep(broadening, n_components)
  } else {
    # Vector: per-component
    broadening_vec <- broadening
  }
  
  # Check if any broadening is enabled
  broadening_enabled <- any(broadening_vec)
  
  # Initialize broadening parameters if enabled
  if (broadening_enabled) {
    if (is.null(G)) {
      # Initialize G with small uniform broadening (1 param per component)
      # Broadening is expressed as % of spectral range
      # Default: start at 0.1%, max allowed is broadening_max_pct (default 10%)
      
      spectral_range <- length(xS)  # Number of wavelength points
      
      # Convert percentages to sigma values
      # 0.1% of spectral range as starting point
      sigma_init <- 0.001 * spectral_range
      
      # Maximum sigma (user-defined percentage)
      sigma_max <- (broadening_max_pct / 100) * spectral_range
      
      # Initialize G parameters: 0.1% for enabled, 0 for disabled
      # ONE parameter per component
      G <- matrix(0, nrow = nrow(C), ncol = n_components)
      for (k in 1:n_components) {
        if (broadening_vec[k]) {
          G[, k] <- sigma_init
        }
      }
      
      if (!silent) {
        enabled_comps <- which(broadening_vec)
        cat(paste0(
          "Broadening: Components ", paste(enabled_comps, collapse=","),
          " - Start=0.1% (σ=", signif(sigma_init, 3), 
          "), Max=", broadening_max_pct, "% (σ=", signif(sigma_max, 3), ")<br/>"
        ))
      }
    } else {
      # G provided - compute sigma_max for bounds
      spectral_range <- length(xS)
      sigma_max <- (broadening_max_pct / 100) * spectral_range
    }
  }
  
  b <- ifelse(optS1st, 1, 0)
  iter <- 0
  oneMore <- TRUE
  while ((abs(RD) > thresh && maxiter >= iter) || oneMore) {
    iter <- iter + 1
    
    # Three-step optimization when broadening is enabled
    if (broadening_enabled) {
      step <- iter %% 3
      
      if (step == (b %% 3)) {
        # Optimize S
        S <- getS(
          C, Psi, S, xS, nonnegS, uniS,
          S0, normS, smooth, SumS, hardS0, wHardS0, normMode, G
        )
      } else if (step == ((b + 1) %% 3)) {
        # Optimize C
        C <- getC(S, Psi, C, nonnegC, nullC, closeC, wCloseC, G)
      } else {
        # Optimize G (broadening parameters) - only for enabled components
        for (i in 1:nrow(Psi)) {
          G_opt <- optimize_broadening_single(
            data_row = Psi[i, ],
            C_row = C[i, ],
            S = S,
            G_init = G[i, ],
            sigma_max = sigma_max,
            broadening_vec = broadening_vec
          )
          # Only update enabled components
          for (k in 1:n_components) {
            if (broadening_vec[k]) {
              G[i, k] <- G_opt[k]
            }
          }
        }
      }
    } else {
      # Standard two-step optimization without broadening
      if (iter %% 2 == b) {
        S <- getS(
          C, Psi, S, xS, nonnegS, uniS,
          S0, normS, smooth, SumS, hardS0, wHardS0, normMode
        )
      } else {
        C <- getC(S, Psi, C, nonnegC, nullC, closeC, wCloseC)
      }
    }
    
    # Compute reconstruction with broadening if enabled
    for (i in 1:nrow(Psi)) {
      if (broadening_enabled) {
        # Apply broadening to enabled components before reconstruction
        S_broad <- S
        for (k in 1:ncol(S)) {
          if (broadening_vec[k]) {
            sigma <- G[i, k]
            S_broad[, k] <- convolve_spectrum(S[, k], sigma)
          }
        }
        mod[i, ] <- C[i, ] %*% t(S_broad)
      } else {
        mod[i, ] <- C[i, ] %*% t(S)
      }
      resid[i, ] <- Psi[i, ] - mod[i, ]
    }
    
    rss <- sum(resid^2) / sum(Psi^2)
    RD <- ((oldrss - rss) / oldrss)
    if(!is.finite(RD))
      return(NULL)
    oldrss <- rss
    
    if (!silent && (iter %% update_interval == 1)) {
      # Iteration message with LOF (%) computed from current rss
      lof_iter <- 100 * sqrt(rss)
      
      # Determine optimization step for message
      if (broadening_enabled) {
        step_type <- if (iter %% 3 == (b %% 3)) "S" else if (iter %% 3 == ((b + 1) %% 3)) "C" else "G"
      } else {
        step_type <- ifelse(iter %% 2 == b, "S", "C")
      }
      
      msg <- paste0(
        "Iter. (opt. ", step_type, "): ", iter,
        ", |RD| : ", signif(abs(RD), 3), " > ", thresh,
        ", LOF(%) : ", signif(lof_iter, 3)
      )
      
      # Add G values if broadening is enabled (only show enabled components)
      if (broadening_enabled) {
        # Show mean G values across samples for enabled components
        G_mean <- colMeans(G)
        G_mean_enabled <- G_mean[broadening_vec]
        g_str <- paste0("(", paste(signif(G_mean_enabled, 3), collapse=", "), ")")
        msg <- paste0(msg, ", σ=", g_str)
      }
      
      cat(msg, "<br/>")
      # Write live snapshot for UI every update_interval iterations
      if (!is.null(state_file)) {
        vlof_snap <- try(100 * sqrt(rss), silent = TRUE)
        snap <- list(
          C = C, S = S, xC = xC, xS = xS, Psi = Psi,
          rss = rss, resid = resid, iter = iter,
          msg = msg,
          lof = ifelse(class(vlof_snap) == "try-error", NA, vlof_snap),
          nullC = nullC
        )
        # Set column names for consistency with final output
        colnames(snap$S) <- paste0("S_", 1:ncol(snap$S))
        colnames(snap$C) <- paste0("C_", 1:ncol(snap$C))
        
        # Add G parameters if broadening is enabled
        if (broadening_enabled) {
          snap$G <- G
          snap$broadening_vec <- broadening_vec
          colnames(snap$G) <- paste0("G_", 1:ncol(G))
        }
        
        try(saveRDS(snap, state_file), silent = TRUE)
      }
    }
    
    oneMore <- ((iter %% 2 != b) && maxiter != 1 && iter <= 2)
  }
  
  vlof <- lof(model = mod, data = Psi)
  msg <- shiny::HTML(paste0(
    "Dimension :", ncol(S),
    "<br/>|RD| : ", signif(abs(RD), 3), " <= ", thresh,
    "<br/> Lack-of-fit (%) : ", signif(vlof, 3)
  ))
  
  # if (!silent)
  #   cat(msg)
  
  result <- list(
    C = C, S = S, xC = xC, xS = xS, Psi = Psi,
    rss = rss, resid = resid, iter = iter,
    msg = msg, lof = vlof
  )
  
  # Add broadening parameters if enabled
  if (broadening_enabled) {
    result$G <- G
    result$broadening_vec <- broadening_vec
    colnames(result$G) <- paste0("G_", 1:ncol(G))
  }
  
  return(result)
}

rotAmb2 <- function(C0, S0, data, rotVec = 1:2,
                    dens = 0.05, eps = -0.01,
                    updateProgress = NULL,
                    nullC = NA) {
  # TBD: account for externalSpectra...
  
  S <- S0[, rotVec]
  C <- C0[, rotVec]
  
  ttry <- function(i) {dens * i}
  
  ikeep <- 0
  solutions <- list()
  ntry <- 0
  iter <- 0
  
  for (s12 in c(0, -1, 1)) {
    i12 <- 0
    OK1 <- TRUE
    while (OK1) {
      i12 <- i12 + s12
      t12 <- ttry(i12)
      OK1 <- FALSE
      
      for (s21 in c(0, -1, 1)) {
        i21 <- 0
        OK2 <- TRUE
        while (OK2) {
          i21 <- i21 + s21
          t21 <- ttry(i21)
          OK2 <- FALSE
          
          iter <- iter + 1

          # Transformation matrix
          R <- matrix(c(
            1, t12,
            t21, 1
          ),
          nrow = 2, ncol = 2,
          byrow = TRUE
          )
          Ri <- try(solve(R), silent = TRUE)
          
          if (class(Ri) != "try-error") {
            ntry <- ntry + 1
            
            # Transform spectra and kinetics
            S1 <- t(R %*% t(S))
            C1 <- C %*% Ri
            
            # Renormalize spectra
            for (i in 1:2) {
              n <- max(S1[, i], na.rm = TRUE)
              S1[, i] <- S1[, i] / n
              C1[, i] <- C1[, i] * n
            }
            
            if (!anyNA(nullC)) {
              C1 <- C1 * nullC[, rotVec]
            }
            
            # Test for positivity
            if (min(S1, na.rm = TRUE) >= eps * max(S1, na.rm = TRUE) &
                min(C1, na.rm = TRUE) >= eps * max(C1, na.rm = TRUE)
            ) {
              ikeep <- ikeep + 1
              solutions[[ikeep]] <- list(
                S1 = S1, C1 = C1,
                t12 = t12, t21 = t21
              )
              OK1 <- OK2 <- TRUE
              
              if (s21 == 0) OK2 <- FALSE
            }
          }
        }
        if (s12 == 0) OK1 <- FALSE
      }
    }
  }
  
  if (length(solutions) != 0) {
    solutions$rotVec <- rotVec
    solutions$eps <- eps
  }
  
  return(
    list(
      solutions = solutions,
      finished = TRUE
    )
  )
}
rotAmb3 <- function(C0, S0, data, rotVec = 1:3,
                    dens = 0.05, eps = -0.01,
                    updateProgress = NULL,
                    nullC = NA) {
  S <- S0[, rotVec]
  C <- C0[, rotVec]
  
  ttry <- function(i) {dens * i}
  
  ikeep <- 0
  solutions <- list()
  ntry <- 0
  iter <- 0
  
  for (s12 in c(0, -1, 1)) {
    i12 <- 0
    OK1 <- TRUE
    while (OK1) {
      i12 <- i12 + s12
      t12 <- ttry(i12)
      OK1 <- FALSE
      
      for (s21 in c(0, -1, 1)) {
        i21 <- 0
        OK2 <- TRUE
        while (OK2) {
          i21 <- i21 + s21
          t21 <- ttry(i21)
          OK2 <- FALSE
          
          for (s23 in c(0, -1, 1)) {
            i23 <- 0
            OK3 <- TRUE
            while (OK3) {
              i23 <- i23 + s23
              t23 <- ttry(i23)
              OK3 <- FALSE
              
              for (s32 in c(0, -1, 1)) {
                i32 <- 0
                OK4 <- TRUE
                while (OK4) {
                  i32 <- i32 + s32
                  t32 <- ttry(i32)
                  OK4 <- FALSE
                  
                  for (s13 in c(0, -1, 1)) {
                    i13 <- 0
                    OK5 <- TRUE
                    while (OK5) {
                      i13 <- i13 + s13
                      t13 <- ttry(i13)
                      OK5 <- FALSE
                      
                      for (s31 in c(0, -1, 1)) {
                        i31 <- 0
                        OK6 <- TRUE
                        while (OK6) {
                          i31 <- i31 + s31
                          t31 <- ttry(i31)
                          OK6 <- FALSE
                          
                          iter <- iter + 1
                          # if(!is.null(updateProgress))
                          #   updateProgress(value = iter / 100)
                          
                          # Transformation matrix
                          R <- matrix(c(
                            1, t12, t13,
                            t21, 1, t23,
                            t31, t32, 1
                          ),
                          nrow = 3, ncol = 3,
                          byrow = TRUE
                          )
                          Ri <- try(solve(R), silent = TRUE)
                          
                          if (class(Ri) != "try-error") {
                            ntry <- ntry + 1
                            
                            # Transform spectra and kinetics
                            S1 <- t(R %*% t(S))
                            C1 <- C %*% Ri
                            
                            # Renormalize spectra
                            for (i in 1:3) {
                              n <- max(S1[, i], na.rm = TRUE)
                              S1[, i] <- S1[, i] / n
                              C1[, i] <- C1[, i] * n
                            }
                            
                            
                            if (!anyNA(nullC)) {
                              C1 <- C1 * nullC[, rotVec]
                            }
                            
                            # Test for positivity
                            if (min(S1, na.rm = TRUE) >= eps * max(S1, na.rm = TRUE) &
                                min(C1, na.rm = TRUE) >= eps * max(C1, na.rm = TRUE)
                            ) {
                              ikeep <- ikeep + 1
                              solutions[[ikeep]] <- list(
                                S1 = S1, C1 = C1,
                                t12 = t12, t21 = t21,
                                t23 = t23, t32 = t32,
                                t13 = t13, t31 = t31
                              )
                              
                              OK1 <- OK2 <- OK3 <- OK4 <- OK5 <- OK6 <- TRUE
                              
                              if (s31 == 0) OK6 <- FALSE
                            }
                          }
                          # httpuv::service()
                          # if (input$killALSAmb) { # Get out of here
                          #   # print(length(solutions))
                          #   if (length(solutions) != 0) {
                          #     solutions$rotVec <- rotVec
                          #     solutions$eps <- eps
                          #   }
                          #   return(
                          #     list(
                          #       solutions = solutions,
                          #       finished = FALSE
                          #     )
                          #   )
                          # }
                        }
                        if (s13 == 0) OK5 <- FALSE
                      }
                    }
                    if (s32 == 0) OK4 <- FALSE
                  }
                }
                if (s23 == 0) OK3 <- FALSE
              }
            }
            if (s21 == 0) OK2 <- FALSE
          }
        }
        if (s12 == 0) OK1 <- FALSE
      }
    }
  }
  
  if (length(solutions) != 0) {
    solutions$rotVec <- rotVec
    solutions$eps <- eps
  }
  
  return(
    list(
      solutions = solutions,
      finished = TRUE
    )
  )
}

# Interactive ####

## Get external spectra #### 
externalSpectraALS <- list()
output$extSpectraALS <- renderUI({
  req(input$S0File)
  
  wavl = Inputs$wavl[!is.na(Inputs$wavlMask)]
  ui   = list()
  res  = getExternalSpectra(
    ui         = ui,
    inputFile  = input$S0File, 
    wavl       = wavl,
    tag        = 'fixALS_S_')

  externalSpectraALS <<- res$extSpectra
  res$ui
  
})

## Per-component constraints UI ####
output$perComponentSUI <- renderUI({
  req(input$perComponentS)
  req(input$nALS)
  
  nS <- input$nALS
  
  # Create checkboxes for each component
  checkboxes <- lapply(1:nS, function(i) {
    checkboxInput(
      inputId = paste0("nonnegS_", i),
      label = paste0("S_", i, " > 0"),
      value = TRUE
    )
  })
  
  fluidRow(
    column(
      width = 12,
      HTML("<small>Uncheck to allow negative values (e.g., for difference spectra)</small>"),
      br(), br(),
      do.call(tagList, checkboxes)
    )
  )
})

## Per-component broadening UI ####
output$perComponentBroadeningUI <- renderUI({
  req(input$perComponentBroadening)
  req(input$broadeningS)
  req(input$nALS)
  
  nS <- input$nALS
  
  # Create checkboxes for each component
  checkboxes <- lapply(1:nS, function(i) {
    checkboxInput(
      inputId = paste0("broadenComp_", i),
      label = paste0("Broaden C_", i),
      value = TRUE
    )
  })
  
  fluidRow(
    column(
      width = 12,
      HTML("<small>Uncheck to disable broadening for specific components</small>"),
      br(), br(),
      do.call(tagList, checkboxes)
    )
  )
})


output$showMSE <- reactive({
  showMSE(
    input$procMult,
    input$rawData_rows_selected,
    input$nALS
  )
})

## Masks ####
output$maskSpExp_ui <- renderUI({
  if (!showMSE(
    input$procMult,
    input$rawData_rows_selected,
    input$nALS
  )
  ) {
    return(NULL)
  }
  
  nM <- length(input$rawData_rows_selected) # Nb data matrices
  nS <- input$nALS # Nb spectra
  
  if (anyNA(Inputs$maskSpExp)) {
    Inputs$maskSpExp <- matrix(1, nrow = nM, ncol = nS)
  } else
    if (nrow(Inputs$maskSpExp) != nM ||
        ncol(Inputs$maskSpExp) != nS) {
      Inputs$maskSpExp <- matrix(1, nrow = nM, ncol = nS)
    }
  
  matInput <- list(
    h5("Presence Matrix"),
    HTML("<table cellpadding=2 border=0>")
  )
  head <- paste0(paste0("<th>Sp_", 1:nS, "</th>"), collapse = "")
  matInput <- list(
    matInput,
    HTML(paste0("<tr><td>&nbsp;</td>", head, "</tr>"))
  )
  for (i1 in 1:nM) {
    var1 <- paste0("Exp_", i1)
    matInput <- c(matInput, list(HTML(paste0(
      "<tr><th>",
      var1, "&nbsp;</th>"
    ))))
    for (i2 in 1:nS) {
      var2 <- paste0("C_", i2)
      name <- paste0("mCE_", var1, "_", var2)
      value <- Inputs$maskSpExp[i1, i2]
      locInput <- list(
        HTML("<td>"),
        tags$input(
          id = name,
          type = "number",
          value = value,
          min = 0, max = 1,
          class = "shiny-bound-input",
          style = "width: 50px;"
        ),
        HTML("</td>")
      )
      matInput <- c(matInput, locInput)
    }
    matInput <- c(matInput, list(HTML("</tr>")))
  }
  matInput <- list(matInput, HTML("</table>"))
  
  wellPanel(
    verticalLayout(
      matInput,
      br(),
      fixedRow(
        column(12,
               offset = 0,
               actionButton("clear_mCE",
                            "Reset",
                            icon = icon("eraser")
               ),
               actionButton("update_mCE",
                            "Done",
                            icon = icon("check")
               )
        )
      )
    )
  )
})
outputOptions(output, "maskSpExp_ui",
              suspendWhenHidden = FALSE
)
# Update maskSpExp
observe({
  if (is.null(input$update_mCE) ||
      input$update_mCE == 0) {
    return()
  }
  
  isolate({
    if (is.null(Inputs$maskSpExp)) {
      return()
    }
    
    nM <- length(input$rawData_rows_selected)
    nS <- input$nALS
    for (i1 in 1:nM) {
      var1 <- paste0("Exp_", i1)
      for (i2 in 1:nS) {
        var2 <- paste0("C_", i2)
        name <- paste0("mCE_", var1, "_", var2)
        Inputs$maskSpExp[i1, i2] <- input[[name]]
      }
    }
  })
})

# Reset maskSpExp
observe({
  if (is.null(input$clear_mCE) ||
      input$clear_mCE == 0) {
    return()
  }
  
  isolate({
    if (is.null(Inputs$maskSpExp)) {
      return()
    }
    
    nM <- length(input$rawData_rows_selected)
    nS <- input$nALS
    Inputs$maskSpExp <- matrix(1, nrow = nM, ncol = nS)
    for (i1 in 1:nM) {
      var1 <- paste0("Exp_", i1)
      for (i2 in 1:nS) {
        var2 <- paste0("C_", i2)
        name <- paste0("mCE_", var1, "_", var2)
        updateNumericInput(session, inputId = name, value = 1)
      }
    }
  })
})

## ALS ####

### Asynchronous Process ####
bgALSpx = NULL
resALS  = reactiveValues(results = NULL)
bgALS   = reactiveValues(status = process_status(bgALSpx))
alsStdOut <- tempfile(pattern = "als_", tmpdir = tempdir(), fileext = ".stdout")
# Live state snapshot file (updated during ALS)
alsStateFile <- tempfile(pattern = "als_state_", tmpdir = tempdir(), fileext = ".rds")
# Ensure the stdout file exists (portable across OSes)
tryCatch({
  if (!file.exists(dirname(alsStdOut))) {
    dir.create(dirname(alsStdOut), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(alsStdOut)) {
    file.create(alsStdOut, showWarnings = FALSE)
  }
}, error = function(e) {})
obsALSStatus = observe(
  {
    invalidateLater(millis = 500)
    bgALS$status = process_status(bgALSpx)
  },
  suspended = TRUE
)

### Kill current process ####
observeEvent(
  input$killALS,
  isolate({
    if(!is.null(bgALSpx)) {
      if(!is.null(bgALS$status$running)) {
        if(bgALS$status$running) {
          bgALSpx$kill()
          nclicks(0)
          obsALSStatus$suspend()
          id = showNotification(
            "ALS stopped !",
            type = "warning",
            duration = 5
          )
          resALS$results = NULL
        }
      }
    } 
  })
)

### Manage results ####
obsALS = observeEvent(
  bgALS$status$result, 
  {
    obsALSStatus$suspend()
    nclicks(0)
    resALS$results = bgALS$status$result
    
    # Update Reporting
    if(! 'ALS' %in% includeInReport()) {
      todo = c(includeInReport(),'ALS')
      includeInReport(todo) 
      updateCheckboxGroupInput(session,
                               inputId = "inReport",
                               choices = todo,
                               selected = todo
      )
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)


### Run ####
nclicks <- reactiveVal(0)
doALS <- observeEvent(
  input$runALS, {
    if (isolate(!checkInputsSanity())) {
      showNotification(
        "Inputs are incomplete: please load data and define a selection before running ALS.",
        type = "warning",
        duration = 6
      )
      return(NULL)
    }
    if(nclicks() != 0){
      showNotification("Already running ALS")
      return(NULL)
    }

    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)

    id = showNotification(
      "Running ALS...",
      type = "message",
      duration = 10
    )
    
    nAls = input$nALS
    
    # Suppress masked areas from coordinates
    delay <- Inputs$delay[!is.na(Inputs$delayMask)]
    delayId <- Inputs$delayId[!is.na(Inputs$delayMask)]
    wavl <- Inputs$wavl[!is.na(Inputs$wavlMask)]
    
    if (input$useFiltered) {
      # Choose SVD filtered matrix
      svdRES = doSVD()
      mat <- matrix(0, nrow = length(delay), ncol = length(wavl))
      for (ic in 1:input$nSV)
        mat <- mat + svdRES$u[, ic] %o% svdRES$v[, ic] * svdRES$d[ic]
    } else {
      mat <- Inputs$mat
      mat <- mat[!is.na(Inputs$delayMask), ]
      mat <- mat[, !is.na(Inputs$wavlMask) ]
    }
    
    # Null C constraints
    nullC <- NA
    if (!anyNA(Inputs$maskSpExp)) {
      nullC <- matrix(1, nrow = length(delayId), ncol = nAls)
      for (i in 1:nAls) {
        for (j in 1:nrow(Inputs$maskSpExp)) {
          if (Inputs$maskSpExp[j, i] == 0) {
            sel <- which(delayId == j)
            nullC[sel, i] <- 0
          }
        }
      }
    }
    
    # External spectrum shapes
    S0 <- NULL
    if (length(externalSpectraALS) != 0)
      for(sp in names(externalSpectraALS)) {
        pname = paste0('fixALS_',sp)
        if( input[[pname]] )
          S0 = cbind(S0, externalSpectraALS[[sp]])
      }
     
    # S-C Vectors init values
    initALS = input$initALS
    if (initALS == "seq") {
      nStart = 2
    } else {
      nStart = nAls
    }

    if (initALS == "SVD" | initALS == "seq") {
      # Initialize with abs(SVD)
      if (is.null(svdRES <- doSVD()))
        return(NULL)

      S = matrix(abs(svdRES$v[, 1:nStart]), ncol = nStart)
      C = matrix(abs(svdRES$u[, 1:nStart]), ncol = nStart)

    } else if (initALS == "PCA") {
      # Initialize with PCA (centered data)
      # Center the data matrix
      mat_centered <- scale(mat, center = TRUE, scale = FALSE)
      
      # Perform SVD on centered data
      pcaRES <- svd(mat_centered, nu = nStart, nv = nStart)
      
      # Use absolute values and shift to positive range
      S = matrix(abs(pcaRES$v[, 1:nStart]), ncol = nStart)
      C = matrix(abs(pcaRES$u[, 1:nStart]), ncol = nStart)
      
      # Scale back to original data range
      for (i in 1:nStart) {
        S[, i] <- S[, i] * pcaRES$d[i] / max(S[, i])
      }

    } else if (initALS == "NMF") {
      # initialize with SVD + NMF
      if (is.null(svdRES <- doSVD()))
        return(NULL)

      # 1/ filter matrix to avoid negative values (noise)
      fMat = rep(0, nrow = nrow(data), ncol = ncol(data))
      for (i in 1:nStart)
        fMat = fMat + svdRES$u[, i] %o% svdRES$v[, i] * svdRES$d[i]
      # 2/ NMF
      res = NMFN::nnmf(
        abs(fMat),
        k = nStart,
        method = "nnmf_als",
        eps = 1e-8)
      C = res$W
      S = t(res$H)

    } else {

      RES = resALS$results
      # restart from existing solution
      if (is.null(RES))
        return(NULL)

      S <- RES$S[, 1:nStart]
      C <- RES$C[, 1:nStart]
    }
    
    
    
    # Get normMode with fallback for backward compatibility
    normMode <- if (!is.null(input$normMode)) input$normMode else "intensity"
    
    # Build nonnegS vector (per-component or global)
    if (!is.null(input$perComponentS) && input$perComponentS) {
      # Per-component constraints
      nonnegS_vec <- sapply(1:nAls, function(i) {
        val <- input[[paste0("nonnegS_", i)]]
        if (is.null(val)) TRUE else val  # Default to TRUE if not set yet
      })
    } else {
      # Global constraint (backward compatible)
      nonnegS_vec <- input$nonnegS
    }
    
    # Build broadening enable vector (per-component or global)
    if (!is.null(input$perComponentBroadening) && input$perComponentBroadening && 
        !is.null(input$broadeningS) && input$broadeningS) {
      # Per-component broadening
      broadening_vec <- sapply(1:nAls, function(i) {
        val <- input[[paste0("broadenComp_", i)]]
        if (is.null(val)) TRUE else val  # Default to TRUE if not set yet
      })
    } else {
      # Global broadening (backward compatible)
      broadening_vec <- if (!is.null(input$broadeningS)) input$broadeningS else FALSE
    }
    
    # Remove any previous live snapshot
    try({ if (file.exists(alsStateFile)) file.remove(alsStateFile) }, silent = TRUE)

    # On Windows, Conda R often lacks the bin/x64 layout. If Rterm.exe is not found
    # under the current R_ARCH, drop R_ARCH for the child process so callr uses bin\\Rterm.exe.
    # For Docker/Linux, we don't need any special environment variables.
    if (identical(.Platform$OS.type, "windows")) {
      arch <- Sys.getenv("R_ARCH", "")
      rterm_candidate <- file.path(R.home("bin"), arch, "Rterm.exe")
      if (!file.exists(rterm_candidate)) {
        rbgenv <- character(0)
        rbgenv["R_ARCH"] <- ""
        
        rx = callr::r_bg(
          als,
          args = list(
            delay, delayId, wavl, mat, nullC, S, C,
            nAls    = nAls,
            nStart  = nStart,
            S0      = S0,
            maxiter = input$maxiter,
            uniS    = input$uniS,
            nonnegS = nonnegS_vec,
            nonnegC = input$nonnegC,
            thresh  = 10^input$alsThresh,
            normS   = input$normS,
            hardS0  = !input$softS0,
            wHardS0 = 10^input$wSoftS0,
            optS1st = input$optS1st,
            smooth  = input$smooth,
            SumS    = input$SumS,
            closeC  = input$closeC,
            wCloseC = 10^input$wCloseC,
            normMode = normMode,
            state_file = alsStateFile,
            update_interval = 10,
            broadening = broadening_vec,
            broadening_max_pct = if (!is.null(input$broadeningMaxPct)) input$broadeningMaxPct else 10
          ),
          package = TRUE,
          stdout = alsStdOut,
          stderr = alsStdOut,
          env = rbgenv
        )
      } else {
        rx = callr::r_bg(
          als,
          args = list(
            delay, delayId, wavl, mat, nullC, S, C,
            nAls    = nAls,
            nStart  = nStart,
            S0      = S0,
            maxiter = input$maxiter,
            uniS    = input$uniS,
            nonnegS = nonnegS_vec,
            nonnegC = input$nonnegC,
            thresh  = 10^input$alsThresh,
            normS   = input$normS,
            hardS0  = !input$softS0,
            wHardS0 = 10^input$wSoftS0,
            optS1st = input$optS1st,
            smooth  = input$smooth,
            SumS    = input$SumS,
            closeC  = input$closeC,
            wCloseC = 10^input$wCloseC,
          normMode = normMode,
          state_file = alsStateFile,
          update_interval = 10,
          broadening = broadening_vec,
          broadening_max_pct = if (!is.null(input$broadeningMaxPct)) input$broadeningMaxPct else 10
        ),
        package = TRUE,
        stdout = alsStdOut,
        stderr = alsStdOut
      )
      }
    } else {
      # Linux/Docker - no special environment needed
      rx = callr::r_bg(
        als,
        args = list(
          delay, delayId, wavl, mat, nullC, S, C,
          nAls    = nAls,
          nStart  = nStart,
          S0      = S0,
          maxiter = input$maxiter,
          uniS    = input$uniS,
          nonnegS = nonnegS_vec,
          nonnegC = input$nonnegC,
          thresh  = 10^input$alsThresh,
          normS   = input$normS,
          hardS0  = !input$softS0,
          wHardS0 = 10^input$wSoftS0,
          optS1st = input$optS1st,
          smooth  = input$smooth,
          SumS    = input$SumS,
          closeC  = input$closeC,
          wCloseC = 10^input$wCloseC,
        normMode = normMode,
        state_file = alsStateFile,
        update_interval = 10,
        broadening = broadening_vec,
        broadening_max_pct = if (!is.null(input$broadeningMaxPct)) input$broadeningMaxPct else 10
      ),
      package = TRUE,
      stdout = alsStdOut,
      stderr = alsStdOut
    )
    }
    resALS$results = NULL
    resAmb$results = NULL
    obsALSStatus$resume()
    bgALSpx <<- rx
    
  }
)

### Outputs ####
stdALSOut = reactiveFileReader(
  intervalMillis = 500,
  session  = session,
  filePath = alsStdOut,
  readFunc = readLines,
  warn     = FALSE
)
# Live preview of ALS state (updated by background process)
alsPreview <- reactiveFileReader(
  intervalMillis = 500,
  session = session,
  filePath = alsStateFile,
  readFunc = function(fp) {
    if (!file.exists(fp)) return(NULL)
    tryCatch(readRDS(fp), error = function(e) NULL)
  }
)
# Reactive value to cache last valid state
alsLastValid <- reactiveVal(NULL)
# Use final results if available, otherwise fall back to live preview
# Cache last valid state to prevent flickering during updates
alsLiveOut <- reactive({
  if (!is.null(resALS$results)) {
    alsLastValid(resALS$results)
    return(resALS$results)
  }
  preview <- alsPreview()
  if (!is.null(preview)) {
    alsLastValid(preview)
    return(preview)
  }
  # Return cached value if current read failed
  alsLastValid()
})

output$alsPrint <- renderPrint({
  cat(stdALSOut(), sep = '\n')
})
output$alsOpt <- renderUI({
  if (is.null(alsOut <- resALS$results)) {
    if(is.null(bgALSpx))
      h5('Select ALS options and press "Run"\n')
    
  } else {
    list(
      h4(">>> ALS done <<<"),
      # h5(n, " species")
      h5("Terminated in ", alsOut$iter, " iterations"),
      if (alsOut$iter >= input$maxiter)
        strong("Warning : maxiter limit reached !!!"),
      h5(alsOut$msg)
    )
  }
})
# Dynamic slider for row selection
output$alsDataModDelayUI <- renderUI({
  sliderInput(
    "alsDataModDelay",
    "Row index (delay/time)",
    min = 1,
    max = max(1, length(Inputs$delay)),
    value = max(1, floor(length(Inputs$delay) / 2)),
    step = 1,
    sep = "",
    width = '100%'
  )
})

output$alsResid1 <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  
  if (isolate(input$useFiltered)) {
    # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
                  nrow = length(Inputs$delay),
                  ncol = length(Inputs$wavl))
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]
    
    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  
  # Build model matrix
  mod <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:ncol(CS$S))
    mod <- mod + CS$C[, i] %o% CS$S[, i]
  
  # Get row index from slider - use directly as index
  rowIdx <- input$alsDataModDelay
  if (is.null(rowIdx)) rowIdx <- 1
  rowIdx <- max(1, min(rowIdx, nrow(mat)))  # Clamp to valid range
  
  delay <- Inputs$delay
  
  # Extract row data (signal vs wavelength at specific delay)
  dataRow = mat[rowIdx, ]
  modRow  = mod[rowIdx, ]
  if (all(is.na(dataRow))) dataRow = dataRow * 0
  if (all(is.na(modRow)))  modRow  = modRow  * 0
  
  # Create 3-panel comparison plot
  par(
    mfrow = c(1, 3),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  
  # Left panel: Row comparison plot (signal vs wavelength)
  plot(
    Inputs$wavl, dataRow,
    type = "l", col = lineColors[3], lwd = 2,
    xlab = "Wavelength (q)",
    ylab = "O.D.",
    main = paste0(
      "Row ", rowIdx, " at delay: ", signif(delay[rowIdx], 3)
    )
  )
  grid()
  lines(Inputs$wavl, modRow, col = lineColors[6], lwd = 2)
  legend(
    "topright",
    legend = c("Data", "Model"),
    col = lineColors[c(3, 6)],
    lwd = 2,
    bty = "n"
  )
  box()
  
  # Middle panel: Data image
  plotImage(
    Inputs$delay, Inputs$wavl, mat,
    main = "Data",
    cont = if (!is.null(input$alsContours)) input$alsContours else FALSE,
    delayTrans = Inputs$delayTrans
  )
  
  # Right panel: Model image
  plotImage(
    Inputs$delay, Inputs$wavl, mod,
    main = paste0("Model ", ncol(CS$S), " species"),
    cont = if (!is.null(input$alsContours)) input$alsContours else FALSE,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight)
output$alsResid3 <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  
  if (isolate(input$useFiltered)) {
    # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
                  nrow = length(Inputs$delay),
                  ncol = length(Inputs$wavl))
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]
    
    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotResid(Inputs$delay, Inputs$wavl, mat,
            CS$C, CS$S,
            main = main,
            delayTrans = Inputs$delayTrans)
},
height = plotHeight)
output$alsResid2 <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  
  if (isolate(input$useFiltered)) {
    # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
                  nrow = length(Inputs$delay),
                  ncol = length(Inputs$wavl))
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]
    
    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotResidAna(Inputs$delay, Inputs$wavl, mat,
               CS$C, CS$S,
               main = main,
               delayTrans = Inputs$delayTrans)
},
height = plotHeight)

rangesAlsKin <- reactiveValues(x = NULL, y = NULL)

output$alsKinVectors <- renderPlot({
  alsOut <- alsLiveOut()
  req(alsOut)  # Now safe - alsLiveOut caches last valid state
  
  # Use custom X-axis scale if provided
  xlim_custom <- rangesAlsKin$x
  if (!is.null(input$alsKinXmin) && !is.null(input$alsKinXmax)) {
    if (is.finite(input$alsKinXmin) && is.finite(input$alsKinXmax) && 
        input$alsKinXmin < input$alsKinXmax) {
      xlim_custom <- c(input$alsKinXmin, input$alsKinXmax)
    }
  }
  
  # Use custom Y-axis scale if provided
  ylim_custom <- rangesAlsKin$y
  if (!is.null(input$alsKinYmin) && !is.null(input$alsKinYmax)) {
    if (is.finite(input$alsKinYmin) && is.finite(input$alsKinYmax) && 
        input$alsKinYmin < input$alsKinYmax) {
      ylim_custom <- c(input$alsKinYmin, input$alsKinYmax)
    }
  }
  
  # Apply Savitzky-Golay smoothing if enabled
  if (!is.null(input$alsKinDisplaySmoothed) && input$alsKinDisplaySmoothed) {
    # Get smoothing parameters
    window <- input$alsKinSGWindow
    order <- input$alsKinSGOrder
    
    # Validate and adjust parameters
    if (window %% 2 == 0) window <- window + 1
    if (order >= window) order <- window - 2
    
    tryCatch({
      # Apply SG filter to kinetics data
      C_smoothed <- apply_sg_filter(alsOut$C, window, order)
      
      # Create modified alsOut with smoothed kinetics
      alsOut_smooth <- alsOut
      
      if (!is.null(input$alsKinDisplayBoth) && input$alsKinDisplayBoth) {
        # Plot both raw and smoothed
        # First plot raw kinetics
        plotAlsVec(alsOut,
                   type = "Kin",
                   xlim = xlim_custom,
                   ylim = ylim_custom,
                   delayTrans = Inputs$delayTrans
        )
        
        # Overlay smoothed kinetics with transparency
        par(new = TRUE)
        alsOut_smooth$C <- C_smoothed
        x <- alsOut_smooth$xC
        y <- alsOut_smooth$C
        
        matlines(x, y,
                type = "l", lwd = 3, lty = 1,
                col = adjustcolor(lineColors[1:ncol(y)], alpha.f = 0.7)
        )
      } else {
        # Plot only smoothed
        alsOut_smooth$C <- C_smoothed
        plotAlsVec(alsOut_smooth,
                   type = "Kin",
                   xlim = xlim_custom,
                   ylim = ylim_custom,
                   delayTrans = Inputs$delayTrans
        )
      }
    }, error = function(e) {
      log_warning(paste("Error applying SG filter to ALS kinetics:", e$message))
      # Fall back to unsmoothed plot
      plotAlsVec(alsOut,
                 type = "Kin",
                 xlim = xlim_custom,
                 ylim = ylim_custom,
                 delayTrans = Inputs$delayTrans
      )
    })
  } else {
    # No smoothing - plot raw kinetics
    plotAlsVec(alsOut,
               type = "Kin",
               xlim = xlim_custom,
               ylim = ylim_custom,
               delayTrans = Inputs$delayTrans
    )
  }
},
height = plotHeight
)

observeEvent(input$alsKin_dblclick, {
  brush <- input$alsKin_brush
  if (!is.null(brush)) {
    rangesAlsKin$x <- c(brush$xmin, brush$xmax)
    rangesAlsKin$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesAlsKin$x <- NULL
    rangesAlsKin$y <- NULL
  }
})

rangesAlsSp <- reactiveValues(x = NULL, y = NULL)

output$alsSpVectors <- renderPlot({
  alsOut <- alsLiveOut()
  req(alsOut)  # Now safe - alsLiveOut caches last valid state
  
  # Use custom X-axis scale if provided
  xlim_custom <- rangesAlsSp$x
  if (!is.null(input$alsSpXmin) && !is.null(input$alsSpXmax)) {
    if (is.finite(input$alsSpXmin) && is.finite(input$alsSpXmax) && 
        input$alsSpXmin < input$alsSpXmax) {
      xlim_custom <- c(input$alsSpXmin, input$alsSpXmax)
    }
  }
  
  # Use custom Y-axis scale if provided
  ylim_custom <- rangesAlsSp$y
  if (!is.null(input$alsSpYmin) && !is.null(input$alsSpYmax)) {
    if (is.finite(input$alsSpYmin) && is.finite(input$alsSpYmax) && 
        input$alsSpYmin < input$alsSpYmax) {
      ylim_custom <- c(input$alsSpYmin, input$alsSpYmax)
    }
  }
  
  plotAlsVec(alsOut,
             type = "Sp",
             xlim = xlim_custom,
             ylim = ylim_custom,
             nonnegS = input$nonnegS,
             delayTrans = Inputs$delayTrans
  )
},
height = plotHeight
)

observeEvent(input$alsSp_dblclick, {
  brush <- input$alsSp_brush
  if (!is.null(brush)) {
    rangesAlsSp$x <- c(brush$xmin, brush$xmax)
    rangesAlsSp$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesAlsSp$x <- NULL
    rangesAlsSp$y <- NULL
  }
})

#### Copy buttons ####
# observe({
#   # req(input$copybtn_ALS_Kin)
#   req(alsOut <- resALS$results)
#   C <- cbind(Inputs$delaySave, reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))$C)
#   colnames(C) <- c("delay", colnames(alsOut$C))
#   txt = readr::format_tsv(as.data.frame(C), eol = "\r\n")
#   shinyCopy2clipboard::CopyButtonUpdate(
#     session,
#     id    = "copybtn_ALS_Kin",
#     label = "Copy Kinetics",
#     icon  = icon("copy"),
#     text  = txt 
#   )
# })
# observe({
#   # req(input$copybtn_ALS_Sp)
#   req(alsOut <- resALS$results)
#   S <- cbind(Inputs$wavl, reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))$S)
#   colnames(S) <- c("wavl", colnames(alsOut$S))
#   txt = readr::format_tsv(as.data.frame(S), eol = "\r\n")
#   shinyCopy2clipboard::CopyButtonUpdate(
#     session,
#     id    = "copybtn_ALS_Sp",
#     label = "Copy Spectra",
#     icon  = icon("copy"),
#     text  = txt 
#   )
# })

observeEvent(
  input$alsSpKinSave,
  isolate({
    req(alsOut <- resALS$results)
    
    CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
    
    S <- cbind(Inputs$wavl, CS$S)
    colnames(S) <- c("wavl", colnames(alsOut$S))
    write.csv(
      S,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_alsSpectra_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    # C = cbind(alsOut$xC,alsOut$C)
    C <- cbind(Inputs$delaySave, CS$C)
    colnames(C) <- c("delay", colnames(alsOut$C))
    write.csv(
      C,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_alsKinets_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    # Save G parameters if broadening is enabled
    if (!is.null(alsOut$G) && ncol(alsOut$G) > 0) {
      G <- cbind(Inputs$delaySave, alsOut$G)
      colnames(G) <- c("delay", colnames(alsOut$G))
      write.csv(
        G,
        file = file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_alsBroadening_",
            input$nALS, "sp",
            ".csv"
          )
        ),
        row.names = FALSE
      )
    }
    
    showNotification(
      "ALS results saved to outputDir folder",
      type = "message",
      duration = 3
    )
  })
)

output$alsSpKinDownload <- downloadHandler(
  filename = function() {
    paste0(input$projectTag, "_ALS_", input$nALS, "sp_results.zip")
  },
  content = function(fname) {
    req(alsOut <- resALS$results)
    
    # Create temporary directory
    tmpdir <- tempdir()
    
    CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
    
    # Save Spectra
    S <- cbind(Inputs$wavl, CS$S)
    colnames(S) <- c("wavl", colnames(alsOut$S))
    spectra_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_alsSpectra_",
      input$nALS, "sp.csv"
    ))
    write.csv(S, file = spectra_file, row.names = FALSE)
    
    # Save Kinetics
    C <- cbind(Inputs$delaySave, CS$C)
    colnames(C) <- c("delay", colnames(alsOut$C))
    kinets_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_alsKinets_",
      input$nALS, "sp.csv"
    ))
    write.csv(C, file = kinets_file, row.names = FALSE)
    
    # Save G parameters if broadening is enabled
    files_to_zip <- c(spectra_file, kinets_file)
    if (!is.null(alsOut$G) && ncol(alsOut$G) > 0) {
      G <- cbind(Inputs$delaySave, alsOut$G)
      colnames(G) <- c("delay", colnames(alsOut$G))
      broadening_file <- file.path(tmpdir, paste0(
        input$projectTag,
        "_alsBroadening_",
        input$nALS, "sp.csv"
      ))
      write.csv(G, file = broadening_file, row.names = FALSE)
      files_to_zip <- c(files_to_zip, broadening_file)
    }
    
    # Zip the files
    zip(zipfile = fname, files = files_to_zip, flags = "-j")
    
    # Handle .zip extension issue
    if(file.exists(paste0(fname, ".zip"))) {
      file.rename(paste0(fname, ".zip"), fname)
    }
  },
  contentType = "application/zip"
)

output$alsContribs <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  plotConbtribs(
    Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight
)

## Ambiguity ####
### UI ####
output$selAmbParams <- renderUI({
  req(alsOut <- resALS$results)
  
  # Init ambRot vector selection
  isolate({
    # TBD manage externalSpectra...
    nS0 = 0
    if(!input$softS0) {
      S0 <- NULL
      if (length(externalSpectraALS) != 0)
        for(sp in names(externalSpectraALS)) {
          pname = paste0('fixALS_',sp)
          if( input[[pname]] )
            S0 = cbind(S0, externalSpectraALS[[sp]])
        }
      if(!is.null(S0))
        nS0 = ncol(S0) 
    }
    
    if(nS0 > ncol(alsOut$S)-2) {
      id = showNotification(
        "No ambiguity: too many spectra fixed",
        type = "warning",
        duration = 10
      )
      req(NULL)
    }
  })

  lv <- list()
  for (i in (nS0+1):input$nALS)
    lv[[i-nS0]] <- i
  
  if(length(lv) > 2)
    label = "Select 2 (or 3) vectors"
  else
    label = "Select 2 vectors"
  
  list(
    fluidRow(
      column(
        4,
        checkboxGroupInput("vecsToRotate",
                           label = label,
                           choices = lv,
                           selected = c(lv[[1]],lv[[2]])
        )
      ),
      column(
        4,
        sliderInput("alsRotAmbEps",
                    label = "Relative positivity threshold",
                    min = signif(-0.1),
                    max = signif(0.1),
                    value = -0.01,
                    step = 0.01,
                    sep = ""
        )
      ),
      column(
        4,
        sliderInput("alsRotAmbDens",
                    label = "Exploration Step",
                    min = signif(0.01),
                    max = signif(0.1),
                    value = 0.05,
                    step = 0.01,
                    sep = ""
        )
      )
    )
  )
})

### Asynchronous Process ####
bgAmbpx = NULL
resAmb  = reactiveValues(results = NULL)
bgAmb   = reactiveValues(status = process_status(bgAmbpx))
obsAmbStatus = observe(
  {
    invalidateLater(millis = 500)
    bgAmb$status = process_status(bgAmbpx)
  },
  suspended = TRUE
)

### Kill current process ####
observeEvent(
  input$killALSAmb,
  isolate({
    if(!is.null(bgAmbpx)) {
      if(!is.null(bgAmb$status$running)) {
        if(bgAmb$status$running) {
          bgAmbpx$kill()
          obsAmbStatus$suspend()
          id = showNotification(
            "Ambiguity explorer stopped !",
            type = "warning",
            duration = 5
          )
          resAmb$results = NULL
        }
      }
    } 
  })
)

### Manage results ####
obsAmb = observeEvent(
  bgAmb$status$result, 
  {
    obsAmbStatus$suspend()
    sol = bgAmb$status$result
    log_info(paste("Ambiguity explorer completed:", length(sol$solutions), "solutions found"))
    if (length(sol$solutions) == 0) {
      if (sol$finished) {
        log_warning("No solutions found - finished")
        showModal(modalDialog(
          title = ">>>> No Solution Found <<<< ",
          paste0(
            "Try to decrease the exploration step ",
            "and/or the relative positivity threshold!"
          ),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "s"
        ))
      } else {
        log_warning("No solutions found - algorithm incomplete")
        showModal(modalDialog(
          title = ">>>> No Solution Found <<<< ",
          paste0("Try to let the algorithm run for a longer time!"),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "s"
        ))
      }
    } else {
      log_info(paste("Solutions available for display"))
    }
    resAmb$results = sol
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)

### Run ####
doAmbRot <- observeEvent(
  input$runALSAmb, {
    log_info("===== AMBIGUITY EXPLORER START =====")
    tryCatch({
    req(alsOut <- resALS$results)
    log_info("ALS results available")

    isolate({
      # TBD manage externalSpectra...
      nS0 = 0
      if(!input$softS0) {
        S0 <- NULL
        if (length(externalSpectraALS) != 0)
          for(sp in names(externalSpectraALS)) {
            pname = paste0('fixALS_',sp)
            if( input[[pname]] )
              S0 = cbind(S0, externalSpectraALS[[sp]])
          }
        if(!is.null(S0))
          nS0 = ncol(S0) 
      }
      
      if(nS0 > ncol(alsOut$S)-2) {
        id = showNotification(
          "No ambiguity: too many spectra fixed",
          type = "warning",
          duration = 10
        )
        req(NULL)
      }
    })
    
    isolate({
      rotVec <- as.numeric(unlist(input$vecsToRotate))
      eps <- input$alsRotAmbEps
      dens <- input$alsRotAmbDens
      log_debug(paste("Selected vectors:", paste(rotVec, collapse=",")))
      log_debug(paste("Eps:", eps, "Dens:", dens))
    })
    
    if (length(rotVec) > 3) {
      log_error("Too many vectors selected (max 3)")
      showModal(modalDialog(
        title = ">>>> Too Many Vectors Selected <<<< ",
        paste0("Please choose 3 vectors max."),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "s"
      ))
      return(NULL)
    }
    
    C0 <- alsOut$C
    S0 <- alsOut$S
    nullC <- alsOut$nullC
    
    id = showNotification(
      "Running ambiguity explorer...",
      type = "message",
      duration = 5
    )
    
    fun = get(paste0('rotAmb',length(rotVec)))
    log_debug(paste("Using function:", paste0('rotAmb',length(rotVec))))

    # Same Windows Rterm.exe handling as ALS run
    if (identical(.Platform$OS.type, "windows")) {
      arch <- Sys.getenv("R_ARCH", "")
      rterm_candidate <- file.path(R.home("bin"), arch, "Rterm.exe")
      if (!file.exists(rterm_candidate)) {
        rbgenv <- character(0)
        rbgenv["R_ARCH"] <- ""
        
        rx <- callr::r_bg(
          fun,
          args = list(
            C0 = C0, 
            S0 = S0,
            data = Inputs$mat, 
            nullC = nullC,
            rotVec = rotVec, 
            dens = dens, 
            eps = eps,
            updateProgress = NULL
          ),
          package = TRUE,
          env = rbgenv
        )
      } else {
        rx <- callr::r_bg(
          fun,
          args = list(
            C0 = C0, 
            S0 = S0,
            data = Inputs$mat, 
            nullC = nullC,
            rotVec = rotVec, 
            dens = dens, 
            eps = eps,
            updateProgress = NULL
          ),
          package = TRUE
        )
      }
    } else {
      # Linux/Docker - no special environment needed
      rx <- callr::r_bg(
        fun,
        args = list(
          C0 = C0, 
          S0 = S0,
          data = Inputs$mat, 
          nullC = nullC,
          rotVec = rotVec, 
          dens = dens, 
          eps = eps,
          updateProgress = NULL
        ),
        package = TRUE
      )
    }
    resAmb$results = NULL
    obsAmbStatus$resume()
    bgAmbpx <<- rx
    
    id = showNotification(
      "Running ambiguity explorer...",
      type = "message",
      duration = 5
    )
    log_info("Ambiguity explorer process started")
    }, error = function(e) {
      log_error(paste("Ambiguity explorer error:", e$message))
      showNotification(
        paste("Error in ambiguity explorer:", e$message),
        type = "error",
        duration = 10
      )
    })
  }
)
rangesAmbSp <- reactiveValues(x = NULL, y = NULL)

# Conditional UI: show status or plot for Spectra
output$ambSpPlotOrStatus <- renderUI({
  # Check if calculation is running
  if (!is.null(bgAmb$status$running) && bgAmb$status$running) {
    # Show running status
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border: 2px dashed #007bff; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("cog", class = "fa-spin fa-3x", style = "color: #007bff; margin-bottom: 20px;"),
      h4("Ambiguity Calculation Running...", style = "color: #007bff; margin-bottom: 10px;"),
      p("Exploring rotational/scaling ambiguity", style = "color: #6c757d;")
    )
  } else if (!is.null(resAmb$results) && length(resAmb$results$solutions) > 0) {
    # Show plot when results available
    plotOutput(
      "ambSpVectors",
      height = "450px",
      dblclick = "ambSp_dblclick",
      brush = brushOpts(
        id = "ambSp_brush",
        resetOnNew = TRUE
      )
    )
  } else {
    # Show instructions
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("chart-line", class = "fa-3x", style = "color: #6c757d; margin-bottom: 20px;"),
      h4("Ambiguity Spectra", style = "color: #6c757d; margin-bottom: 10px;"),
      p("Click 'Start' to explore ambiguity", style = "color: #adb5bd;")
    )
  }
})

output$ambSpVectors <- renderPlot({
  req(resAmb$results)
  req(length(resAmb$results$solutions) > 0)
  req(alsOut <- resALS$results)
  
  plotAmbVec(
    alsOut, 
    resAmb$results$solutions,
    type = "Sp",
    displayLines = input$ambDisplayLines,
    xlim = rangesAmbSp$x,
    ylim = rangesAmbSp$y,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight - 100
)

observeEvent(input$ambSp_dblclick, {
  brush <- input$ambSp_brush
  if (!is.null(brush)) {
    rangesAmbSp$x <- c(brush$xmin, brush$xmax)
    rangesAmbSp$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesAmbSp$x <- NULL
    rangesAmbSp$y <- NULL
  }
})

rangesAmbKin <- reactiveValues(x = NULL, y = NULL)

# Conditional UI: show status or plot for Kinetics
output$ambKinPlotOrStatus <- renderUI({
  # Check if calculation is running
  if (!is.null(bgAmb$status$running) && bgAmb$status$running) {
    # Show running status
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border: 2px dashed #007bff; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("cog", class = "fa-spin fa-3x", style = "color: #007bff; margin-bottom: 20px;"),
      h4("Ambiguity Calculation Running...", style = "color: #007bff; margin-bottom: 10px;"),
      p("Exploring rotational/scaling ambiguity", style = "color: #6c757d;")
    )
  } else if (!is.null(resAmb$results) && length(resAmb$results$solutions) > 0) {
    # Show plot when results available
    plotOutput(
      "ambKinVectors",
      height = "450px",
      dblclick = "ambKin_dblclick",
      brush = brushOpts(
        id = "ambKin_brush",
        resetOnNew = TRUE
      )
    )
  } else {
    # Show instructions
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("chart-line", class = "fa-3x", style = "color: #6c757d; margin-bottom: 20px;"),
      h4("Ambiguity Kinetics", style = "color: #6c757d; margin-bottom: 10px;"),
      p("Click 'Start' to explore ambiguity", style = "color: #adb5bd;")
    )
  }
})

output$ambKinVectors <- renderPlot({
  req(resAmb$results)
  req(length(resAmb$results$solutions) > 0)
  req(alsOut <- resALS$results)
  
  plotAmbVec(
    alsOut, 
    resAmb$results$solutions,
    type = "Kin",
    displayLines = input$ambDisplayLines,
    xlim = rangesAmbKin$x,
    ylim = rangesAmbKin$y,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight - 100
)

observeEvent(input$ambKin_dblclick, {
  brush <- input$ambKin_brush
  if (!is.null(brush)) {
    rangesAmbKin$x <- c(brush$xmin, brush$xmax)
    rangesAmbKin$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesAmbKin$x <- NULL
    rangesAmbKin$y <- NULL
  }
})

### Save ####
observeEvent(
  input$rotAmbVecSave,
  isolate({
    req(alsOut <- resALS$results)
    req(ambOut <- resAmb$results)
    
    solutions = ambOut$solutions
    
    CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
    
    C <- alsOut$C
    xC <- alsOut$xC
    nC <- ncol(C)
    S <- alsOut$S
    xS <- alsOut$xS
    nS <- ncol(S)
    for (i in 1:ncol(C)) {
      nn <- sum(S[, i])
      S[, i] <- S[, i] / nn
      C[, i] <- C[, i] * nn
    }
    
    nkeep <- length(solutions) - 2
    allVec <- 1:nC
    rotVec <- solutions$rotVec
    sel <- allVec %in% rotVec
    nvec <- length(rotVec)
    eps <- solutions$eps
    
    # Estimate ranges of S
    S1 <- S[, sel]
    Smax <- matrix(eps, nrow = nrow(S1), ncol = ncol(S1))
    Smin <- matrix(1e30, nrow = nrow(S1), ncol = ncol(S1))
    for (i in 1:nkeep) {
      for (j in 1:nvec) {
        vec <- solutions[[i]]$S1[, j]
        nn <- sum(vec)
        for (k in 1:nrow(S1)) {
          val <- vec[k] / nn
          Smin[k, j] <- min(Smin[k, j], val, na.rm = TRUE)
          Smax[k, j] <- max(Smax[k, j], val, na.rm = TRUE)
        }
      }
    }
    S <- cbind(Inputs$wavl, Smin, Smax)
    colnames(S) <- c(
      'wavl', 
      paste0(colnames(S1),'_min'),
      paste0(colnames(S1),'_max')
    )
    write.csv(
      S,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_ambSpectra_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    
    # Estimate ranges of C
    C1 <- C[, sel]
    Cmax <- matrix(eps, nrow = nrow(C1), ncol = ncol(C1))
    Cmin <- matrix(1e30, nrow = nrow(C1), ncol = ncol(C1))
    for (i in 1:nkeep) {
      for (j in 1:nvec) {
        vec <- solutions[[i]]$S1[, j]
        nn <- sum(vec)
        for (k in 1:nrow(C1)) {
          val <- solutions[[i]]$C1[k, j] * nn # Normalize
          Cmin[k, j] <- min(Cmin[k, j], val, na.rm = TRUE)
          Cmax[k, j] <- max(Cmax[k, j], val, na.rm = TRUE)
        }
      }
    }
    
    C <- cbind(Inputs$delaySave, Cmin, Cmax)
    colnames(C) <- c(
      'delay', 
      paste0(colnames(C1),'_min'),
      paste0(colnames(C1),'_max')
    )
    write.csv(
      C,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_ambKinets_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    showNotification(
      "Ambiguity results saved to outputDir folder",
      type = "message",
      duration = 3
    )
  })
)

output$rotAmbVecDownload <- downloadHandler(
  filename = function() {
    paste0(input$projectTag, "_ALS_", input$nALS, "sp_ambiguity.zip")
  },
  content = function(fname) {
    req(alsOut <- resALS$results)
    req(ambOut <- resAmb$results)
    
    # Create temporary directory
    tmpdir <- tempdir()
    
    solutions = ambOut$solutions
    
    C <- alsOut$C
    xC <- alsOut$xC
    nC <- ncol(C)
    S <- alsOut$S
    xS <- alsOut$xS
    nS <- ncol(S)
    for (i in 1:ncol(C)) {
      nn <- sum(S[, i])
      S[, i] <- S[, i] / nn
      C[, i] <- C[, i] * nn
    }
    
    nkeep <- length(solutions) - 2
    allVec <- 1:nC
    rotVec <- solutions$rotVec
    sel <- allVec %in% rotVec
    nvec <- length(rotVec)
    eps <- solutions$eps
    
    # Estimate ranges of S
    S1 <- S[, sel]
    Smax <- matrix(eps, nrow = nrow(S1), ncol = ncol(S1))
    Smin <- matrix(1e30, nrow = nrow(S1), ncol = ncol(S1))
    for (i in 1:nkeep) {
      for (j in 1:nvec) {
        vec <- solutions[[i]]$S1[, j]
        nn <- sum(vec)
        for (k in 1:nrow(S1)) {
          val <- vec[k] / nn
          Smin[k, j] <- min(Smin[k, j], val, na.rm = TRUE)
          Smax[k, j] <- max(Smax[k, j], val, na.rm = TRUE)
        }
      }
    }
    S_out <- cbind(Inputs$wavl, Smin, Smax)
    colnames(S_out) <- c(
      'wavl', 
      paste0(colnames(S1),'_min'),
      paste0(colnames(S1),'_max')
    )
    
    # Save Spectra
    spectra_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_ambSpectra_",
      input$nALS, "sp.csv"
    ))
    write.csv(S_out, file = spectra_file, row.names = FALSE)
    
    # Estimate ranges of C
    C1 <- C[, sel]
    Cmax <- matrix(eps, nrow = nrow(C1), ncol = ncol(C1))
    Cmin <- matrix(1e30, nrow = nrow(C1), ncol = ncol(C1))
    for (i in 1:nkeep) {
      for (j in 1:nvec) {
        vec <- solutions[[i]]$S1[, j]
        nn <- sum(vec)
        for (k in 1:nrow(C1)) {
          val <- solutions[[i]]$C1[k, j] * nn # Normalize
          Cmin[k, j] <- min(Cmin[k, j], val, na.rm = TRUE)
          Cmax[k, j] <- max(Cmax[k, j], val, na.rm = TRUE)
        }
      }
    }
    
    C_out <- cbind(Inputs$delaySave, Cmin, Cmax)
    colnames(C_out) <- c(
      'delay', 
      paste0(colnames(C1),'_min'),
      paste0(colnames(C1),'_max')
    )
    
    # Save Kinetics
    kinets_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_ambKinets_",
      input$nALS, "sp.csv"
    ))
    write.csv(C_out, file = kinets_file, row.names = FALSE)
    
    # Zip the files
    files_to_zip <- c(spectra_file, kinets_file)
    zip(zipfile = fname, files = files_to_zip, flags = "-j")
    
    # Handle .zip extension issue
    if(file.exists(paste0(fname, ".zip"))) {
      file.rename(paste0(fname, ".zip"), fname)
    }
  },
  contentType = "application/zip"
)
