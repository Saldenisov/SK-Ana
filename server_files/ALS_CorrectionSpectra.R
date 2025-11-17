# Correction Spectra Extension for MCR-ALS
# ==========================================
# This module implements the "correction spectra" approach:
# - k = 2n + a dimensions (n fixed, n corrections, a free)
# - Each correction spectrum is paired with a fixed spectrum
# - Corrections are orthogonal to their fixed counterparts
#
# Usage: Enable when fixed spectra (S0) are provided
#        and "Correction Spectra" option is checked

# Get C with pairwise coupling constraint ####
getC_Coupled <- safely(function(S, data, C, nonnegC = TRUE,
                         nullC = NA, closeC = FALSE, wCloseC = 0,
                         nFixed = 0) {
  # Adapted from getC() with pairwise coupling for corrections
  # Concentrations of correction spectra are projected onto fixed spectrum direction
  
  S[which(is.nan(S))] <- 1
  
  # Soft closure constraint
  if (closeC) {
    if (wCloseC != 0) {
      S <- rbind(S, rep(wCloseC, ncol(S)))
      data <- cbind(data, rep(wCloseC, nrow(data)))
    }
  }
  
  for (i in 1:nrow(data)) {
    if (nonnegC) {
      cc <- try(nnls::nnls(S, data[i, ]))
    } else {
      cc <- try(qr.coef(qr(S), data[i, ]))
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
  
  # Pairwise coupling: project correction columns onto fixed spectrum direction
  # This ensures: C_corr is proportional to C_fix (same time dependence)
  if (nFixed > 0) {
    for (i in 1:nFixed) {
      corr_idx <- nFixed + i
      if (corr_idx <= ncol(C)) {
        c_fix <- C[, i]
        c_corr <- C[, corr_idx]
        
        # Project correction onto fixed direction
        norm_sq <- sum(c_fix^2)
        if (norm_sq > 1e-12) {
          alpha <- sum(c_corr * c_fix) / norm_sq
          C[, corr_idx] <- alpha * c_fix
        } else {
          C[, corr_idx] <- 0
        }
      }
    }
  }
  
  if (!anyNA(nullC)) {
    if (ncol(nullC) == ncol(C)) {
      C <- C * nullC
    }
  }
  
  return(C)
}, return_on_error = NULL)

# Get S with orthogonality constraint for corrections ####
getS_Coupled <- safely(function(C, data, S, xS, nonnegS, uniS,
                         S0, normS, smooth, SumS, hardS0,
                         wHardS0, nFixed = 0, lambdaCorr = 0,
                         normMode = "intensity") {
  # Adapted from getS() with orthogonality enforcement for correction spectra
  # - Correction spectra are orthogonal to fixed spectra
  # - Optional: Tikhonov regularization on correction spectra
  
  C[which(is.nan(C))] <- 1
  
  if (!is.null(S0)) {
    nS0 <- ncol(S0)
    if (hardS0) {
      
      if (ncol(S) == nS0)
        return(S0)
      
      # Enforce equality to external spectrum by direct elimination
      C0 <- matrix(C[, 1:nS0], ncol = nS0)
      C <- matrix(C[, (nS0 + 1):ncol(C)], ncol = ncol(C) - nS0)
      S <- matrix(S[, (nS0 + 1):ncol(S)], ncol = ncol(S) - nS0)
      
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
  
  for (i in 1:ncol(data)) {
    if (nonnegS) {
      s <- try(nnls::nnls(C, data[, i]))
    } else {
      s <- try(qr.coef(qr(C), data[, i]))
    }
    
    if (class(s) == "try-error") {
      S[i, ] <- rep(1, ncol(C))
    } else {
      S[i, ] <- if (nonnegS) {
        s$x
      } else {
        s
      }
    }
  }
  
  if (uniS) {
    # Enforce unimodality
    for (i in 1:ncol(S))
      S[, i] <- Iso::ufit(y = S[, i], x = xS)$y
  }
  
  # Orthogonalize correction spectra to fixed spectra
  # s_corr = s_corr - proj(s_corr onto s_fix) - mean(s_corr)
  if (nFixed > 0) {
    for (i in 1:nFixed) {
      corr_idx <- nFixed + i
      if (corr_idx <= ncol(S)) {
        s_fix <- S[, i]
        s_corr <- S[, corr_idx]
        
        # Remove component parallel to fixed spectrum
        s_fix_norm_sq <- sum(s_fix^2)
        if (s_fix_norm_sq > 1e-12) {
          proj_coeff <- sum(s_corr * s_fix) / s_fix_norm_sq
          s_corr <- s_corr - proj_coeff * s_fix
        }
        
        # Zero-mean correction (prevents it from becoming a baseline)
        s_corr <- s_corr - mean(s_corr)
        
        # Apply smallness penalty if lambda > 0 (soft constraint)
        # This is a post-hoc scaling; for true Tikhonov, modify the solver
        if (lambdaCorr > 0) {
          penalty <- 1 / (1 + lambdaCorr * sqrt(sum(s_corr^2)))
          s_corr <- s_corr * penalty
        }
        
        S[, corr_idx] <- s_corr
      }
    }
  }
  
  if (smooth != 0) {
    # Smooth spectra (skip correction spectra if strong corrections)
    for (i in 1:ncol(S)) {
      # Skip smoothing for correction spectra (small signals)
      if (nFixed > 0 && i > nFixed && i <= (2 * nFixed)) {
        next  # Don't smooth corrections
      }
      y <- S[, i]
      x <- 1:length(y)
      mod <- loess(y ~ x, span = smooth)
      y <- predict(mod)
      y[y < 0] <- 0
      S[, i] <- y
    }
  }
  
  if (normS) {
    # Spectra normalization (separate for fixed, corrections, and free)
    # Fixed spectra: always normalize
    # Correction spectra: normalize separately if present
    # Free spectra: normalize normally
    
    if (nFixed > 0) {
      # Normalize fixed spectra only
      for (i in 1:nFixed) {
        if (SumS) {
          norm_factor <- sum(abs(S[, i]))
          S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
        } else {
          if (normMode == "l1") {
            norm_factor <- sum(abs(S[, i]))
            S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          } else {
            norm_factor <- max(abs(S[, i]))
            S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          }
        }
      }
      
      # Correction spectra: keep as-is (they are already zero-mean, small)
      # They should not be renormalized to 1 to maintain their "correction" nature
      
      # Free spectra: normalize normally
      if (ncol(S) > 2 * nFixed) {
        for (i in (2 * nFixed + 1):ncol(S)) {
          if (SumS) {
            norm_factor <- sum(abs(S[, i]))
            S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          } else {
            if (normMode == "l1") {
              norm_factor <- sum(abs(S[, i]))
              S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
            } else {
              norm_factor <- max(abs(S[, i]))
              S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
            }
          }
        }
      }
    } else {
      # No fixed spectra: use original normalization
      if (SumS) {
        for (i in 1:ncol(S)) {
          norm_factor <- sum(abs(S[, i]))
          S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
        }
      } else {
        if (normMode == "l1") {
          for (i in 1:ncol(S)) {
            norm_factor <- sum(abs(S[, i]))
            S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          }
        } else {
          for (i in 1:ncol(S)) {
            norm_factor <- max(abs(S[, i]))
            S[, i] <- S[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          }
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
}

# Main coupled ALS iteration ####
myals_Coupled <- function(C, Psi, S,
                          thresh = 0.001,
                          maxiter = 100,
                          xC = 1:nrow(C),
                          xS = 1:nrow(S),
                          nonnegC = TRUE, nonnegS = TRUE, optS1st = TRUE,
                          normS = TRUE, uniS = FALSE, S0 = NULL, smooth = 0,
                          silent = TRUE, SumS = FALSE, hardS0 = TRUE,
                          wHardS0 = 1.0,
                          nullC = NA, closeC = FALSE, wCloseC = 0,
                          nFixed = 0, lambdaCorr = 0,
                          normMode = "intensity",
                          state_file = NULL, update_interval = 10) {
  # Main ALS loop with correction spectra coupling
  # - nFixed: number of fixed spectra
  # - lambdaCorr: Tikhonov regularization parameter for corrections
  
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
        for (i in 1:ncol(S0)) {
          norm_factor <- sum(abs(S0[, i]))
          S0[, i] <- S0[, i] / ifelse(norm_factor > 0, norm_factor, 1)
        }
      } else {
        if (normMode == "l1") {
          for (i in 1:ncol(S0)) {
            norm_factor <- sum(abs(S0[, i]))
            S0[, i] <- S0[, i] / ifelse(norm_factor > 0, norm_factor, 1)
          }
        } else {
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
  
  b <- ifelse(optS1st, 1, 0)
  iter <- 0
  oneMore <- TRUE
  while ((abs(RD) > thresh && maxiter >= iter) || oneMore) {
    iter <- iter + 1
    
    if (iter %% 2 == b) {
      S <- getS_Coupled(
        C, Psi, S, xS, nonnegS, uniS,
        S0, normS, smooth, SumS, hardS0, wHardS0,
        nFixed = nFixed, lambdaCorr = lambdaCorr, normMode = normMode
      )
    } else {
      C <- getC_Coupled(S, Psi, C, nonnegC, nullC, closeC, wCloseC,
                        nFixed = nFixed)
    }
    
    for (i in 1:nrow(Psi)) {
      mod[i, ] <- C[i, ] %*% t(S)
      resid[i, ] <- Psi[i, ] - mod[i, ]
    }
    
    rss <- sum(resid^2) / sum(Psi^2)
    RD <- ((oldrss - rss) / oldrss)
    if (!is.finite(RD))
      return(NULL)
    oldrss <- rss
    
    if (!silent && (iter %% update_interval == 1)) {
      lof_iter <- 100 * sqrt(rss)
      msg <- paste0(
        "Iter. (opt. ",
        ifelse(iter %% 2 == b, "S", "C"), "): ", iter,
        ", |RD| : ", signif(abs(RD), 3), " > ", thresh,
        ", LOF(%) : ", signif(lof_iter, 3)
      )
      cat(msg, "<br/>")
      if (!is.null(state_file)) {
        vlof_snap <- try(100 * sqrt(rss), silent = TRUE)
        snap <- list(
          C = C, S = S, xC = xC, xS = xS, Psi = Psi,
          rss = rss, resid = resid, iter = iter,
          msg = msg,
          lof = ifelse(class(vlof_snap) == "try-error", NA, vlof_snap),
          nullC = nullC
        )
        colnames(snap$S) <- paste0("S_", 1:ncol(snap$S))
        colnames(snap$C) <- paste0("C_", 1:ncol(snap$C))
        try(saveRDS(snap, state_file), silent = TRUE)
      }
    }
    
    oneMore <- ((iter %% 2 != b) && maxiter != 1 && iter <= 2)
  }
  
  vlof <- lof(model = mod, data = Psi)
  msg <- shiny::HTML(paste0(
    "Dimension: ", ncol(S), " (", nFixed, " fixed + ", nFixed,
    " corrections + ", ncol(S) - 2 * nFixed, " free)<br/>",
    "|RD| : ", signif(abs(RD), 3), " <= ", thresh,
    "<br/>Lack-of-fit (%) : ", signif(vlof, 3)
  ))
  
  return(
    list(
      C = C, S = S, xC = xC, xS = xS, Psi = Psi,
      rss = rss, resid = resid, iter = iter,
      msg = msg, lof = vlof,
      nFixed = nFixed, lambdaCorr = lambdaCorr
    )
  )
}

# Wrapper for sequential dimension growth with coupled spectra ####
als_Coupled <- function(
  delay, delayId, wavl, mat, nullC, S, C,
  nAls = 2, 
  nStart = 2,
  S0 = NULL,
  nFixed = 0,
  useCorrectionSpectra = FALSE,
  maxiter = 100,
  uniS = NA,
  nonnegS = NA,
  nonnegC = NA,
  thresh = NA,
  normS = NA,
  hardS0 = NA,
  wHardS0 = NA,
  optS1st = NA,
  smooth = FALSE,
  SumS = NA,
  closeC = NA,
  wCloseC = NA,
  lambdaCorr = 0,
  normMode = "intensity",
  state_file = NULL,
  update_interval = 10) {
  
  # Load required packages in subprocess
  required_packages <- c('nnls', 'Iso', 'mvtnorm', 'fields', 'Rsolnp', 'deSolve', 'msm', 'changepoint', 'outliers', 'rgenoud', 'NMFN')
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }
  
  # If correction spectra not enabled, fall back to standard ALS
  if (!useCorrectionSpectra || nFixed == 0) {
    source("server_files/ALS.R")
    return(als(
      delay, delayId, wavl, mat, nullC, S, C,
      nAls = nAls, nStart = nStart, S0 = S0,
      maxiter = maxiter, uniS = uniS, nonnegS = nonnegS,
      nonnegC = nonnegC, thresh = thresh, normS = normS,
      hardS0 = hardS0, wHardS0 = wHardS0, optS1st = optS1st,
      smooth = smooth, SumS = SumS, closeC = closeC,
      wCloseC = wCloseC, normMode = normMode,
      state_file = state_file, update_interval = update_interval
    ))
  }
  
  # Coupled correction spectra approach
  res <- list()
  for (n in nStart:nAls) { 
    
    cat(paste0('Running ALS (dim = ', n, ', coupled)<br/>'))
    
    res[[n]] <- myals_Coupled(
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
      nFixed = nFixed,
      lambdaCorr = lambdaCorr,
      normMode = normMode,
      silent = FALSE,
      state_file = state_file,
      update_interval = update_interval
    )
    
    if (is.null(res[[n]]))
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
}
