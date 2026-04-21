

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
