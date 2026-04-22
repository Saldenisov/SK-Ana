
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
externalSpectraALS <- reactiveVal(list())

selectedExternalSpectraALS <- function(input_prefix = "fixALS_") {
  extSpectra <- externalSpectraALS()
  if (length(extSpectra) == 0) {
    return(NULL)
  }

  selected <- NULL
  for (sp in names(extSpectra)) {
    pname <- paste0(input_prefix, sp)
    if (isTRUE(input[[pname]])) {
      selected <- cbind(selected, extSpectra[[sp]])
    }
  }

  selected
}

als_fixed_spectra_count <- safely(function(softS0 = isTRUE(input$softS0)) {
  if (softS0) {
    return(0L)
  }

  selected <- selectedExternalSpectraALS()
  if (is.null(selected)) {
    return(0L)
  }

  as.integer(ncol(selected))
}, return_on_error = 0L)

output$extSpectraALS <- renderUI({
  req(input$S0File)
  
  wavl = Inputs$wavl[!is.na(Inputs$wavlMask)]
  ui   = list()
  res  = getExternalSpectra(
    ui         = ui,
    inputFile  = input$S0File, 
    wavl       = wavl,
    tag        = 'fixALS_S_')

  externalSpectraALS(res$extSpectra)
  res$ui
  
})
