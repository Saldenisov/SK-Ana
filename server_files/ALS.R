

# Functions ####

getC <- function(S, data, C, nonnegC = TRUE,
                 nullC = NA, closeC = FALSE, wCloseC = 0) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  
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
  
  if (!anyNA(nullC)) {
    if (ncol(nullC) == ncol(C)) {
      C <- C * nullC
    }
  }
  
  # Hard closure constrained (replaced by soft)
  # if(closeC)
  #   C = C / rowSums(C, na.rm = TRUE)
  
  return(C)
}
getS <- function(C, data, S, xS, nonnegS, uniS,
                 S0, normS, smooth, SumS, hardS0,
                 wHardS0, normMode = "intensity") {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  # 2017-12-07 : replaced direct substitution of S0 by direct elimination
  
  C[which(is.nan(C))] <- 1
  
  if (!is.null(S0)) {
    nS0 <- ncol(S0)
    if (hardS0) {
      
      if(ncol(S) == nS0)
        return(S0) # Nothing left to optimize...
      
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
}
als <- function(
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
  update_interval = 10) {
  
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
      update_interval = update_interval
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
  
}
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
                  state_file = NULL, update_interval = 10) {
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
  
  b <- ifelse(optS1st, 1, 0)
  iter <- 0
  oneMore <- TRUE
  while ((abs(RD) > thresh && maxiter >= iter) || oneMore) {
    iter <- iter + 1
    
    if (iter %% 2 == b) {
      S <- getS(
        C, Psi, S, xS, nonnegS, uniS,
        S0, normS, smooth, SumS, hardS0, wHardS0, normMode
      )
    } else {
      C <- getC(S, Psi, C, nonnegC, nullC, closeC, wCloseC)
    }
    
    for (i in 1:nrow(Psi)) {
      mod[i, ] <- C[i, ] %*% t(S)
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
      msg <- paste0(
        "Iter. (opt. ",
        ifelse(iter %% 2 == b, "S", "C"), "): ", iter,
        ", |RD| : ", signif(abs(RD), 3), " > ", thresh,
        ", LOF(%) : ", signif(lof_iter, 3)
      )
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
  
  return(
    list(
      C = C, S = S, xC = xC, xS = xS, Psi = Psi,
      rss = rss, resid = resid, iter = iter,
      msg = msg, lof = vlof
    )
  )
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
            nonnegS = input$nonnegS,
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
            update_interval = 10
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
            nonnegS = input$nonnegS,
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
            update_interval = 10
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
          nonnegS = input$nonnegS,
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
          update_interval = 10
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
  plotDatavsMod(Inputs$delay, Inputs$wavl, mat,
                CS$C, CS$S,
                main = main,
                delayTrans = Inputs$delayTrans)
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
  
  plotAlsVec(alsOut,
             type = "Kin",
             xlim = xlim_custom,
             ylim = ylim_custom,
             delayTrans = Inputs$delayTrans
  )
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
