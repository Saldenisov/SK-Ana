plotAmbVec <- function(alsOut, solutions,
                       type = "Kin",
                       displayLines = FALSE,
                       xlim = NULL, ylim = NULL, 
                       delayTrans = '',
                       ...) {
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  
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
  
  cols <- lineColors[1:nC]
  col0 <- cols[!sel]
  colF <- cols[sel]
  colR <- colo_tr2[sel]
  
  if (type == "Sp") {
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
    if (is.null(xlim)) {
      xlim <- range(xS, na.rm = TRUE)
    }
    if (is.null(ylim)) {
      ylim <- range(c(S, Smin, Smax), na.rm = TRUE)
    }
    
    matplot(
      xS, S,
      type = "n",
      xlim = xlim, ylim = ylim,
      xaxs = "i", yaxs = "i",
      main = "Area Normalized Spectra",
      xlab = "Wavelength",
      col = cols
    )
    grid()
    abline(h = 0, lty = 2)
    if (sum(!sel) != 0) {
      S1 <- S[, !sel]
      matplot(xS, S1,
              type = "p", pch = 16, cex = 0.5,
              col = col0, add = TRUE
      )
    }
    for (j in 1:nvec) {
      polygon(
        c(xS, rev(xS)), c(Smin[, j], rev(Smax[, j])),
        col = colR[j], border = NA
      )
      if (displayLines)
        for (i in 1:nkeep) {
          vec <- solutions[[i]]$S1[, j]
          nn <- sum(vec)
          vec = vec / nn
          lines(xS,vec,lty=1,col=colF[j],lwd=1)
        }
    }
    colorizeMask1D(axis = "wavl", ylim = ylim)
    box()
  } else {
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
    if (is.null(xlim)) {
      xlim <- range(xC, na.rm = TRUE)
    }
    if (is.null(ylim)) {
      ylim <- range(c(C, Cmin, Cmax), na.rm = TRUE)
    }
    
    matplot(
      xC, C,
      type = "n",
      xlim = xlim, ylim = ylim,
      xaxs = "i", yaxs = "i",
      main = "Kinetics", 
      xlab = paste0("Delay ",delayTrans),
      col = cols
    )
    grid()
    abline(h = 0, lty = 2)
    if (sum(!sel) != 0) {
      C1 <- C[, !sel]
      matplot(
        xC, C1,
        type = "p", pch = 16, cex = 0.5,
        col = col0, add = TRUE
      )
    }
    for (j in 1:nvec) {
      polygon(
        c(xC, rev(xC)), c(Cmin[, j], rev(Cmax[, j])),
        col = colR[j], border = NA
      )
      if (displayLines)
        for (i in 1:nkeep) {
          vec <- solutions[[i]]$S1[, j]
          nn <- sum(vec)
          vec = solutions[[i]]$C1[, j] * nn
          lines(xC,vec,lty=1,col=colF[j],lwd=1)
        }    
    }
    colorizeMask1D(axis = "delay", ylim = ylim)
    
    box()
  }
}

rotAmb2 <- function(C0, S0, data, rotVec = 1:2,
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
          
          # httpuv::service()
          # if (input$killALSAmb) { # Get out of here
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
showMSE <- function(a, b, c) {
  if (is.null(a)) {
    return(FALSE)
  }
  if (a != "tileDel") {
    return(FALSE)
  }
  if (length(b) <= 1) {
    return(FALSE)
  }
  if (c <= 1) {
    return(FALSE)
  }
  return(TRUE)
}
getExternalSpectra <- function(ui, inputFile, wavl, tag) {
  # Get spectra on file(s), interpolate them on wavl grid
  # and generate selection ui

  offset = length(ui)
  isp = 0
  extSpectra = list()
  for (i in seq_along(inputFile$datapath)) {
    fname = inputFile[i,'name']
    fN    = inputFile[i,'datapath']
    tmp   = try(
      read.table(
        file   = fN,
        header = TRUE,
        dec    = inputStyle$dec,
        sep    = inputStyle$sep,
        colClasses = "numeric",
        stringsAsFactors = FALSE
      ),
      silent = TRUE
    )
    if(class(tmp) == 'try-error') {
      id = showNotification(
        paste0('Error while reading file: ',fname),
        type = "error",
        duration = NULL
      )
    } else {
      for (k in 2:ncol(tmp)) {
        isp = isp + 1
        sp = colnames(tmp)[k]
        
        # Interpolate on wavl grid
        S0 = spline(tmp[, 1], tmp[, k], xout = wavl)$y
        
        # Normalize
        S0 = S0 / max(S0)
        
        # Store in global list
        extSpectra[[paste0("S_",sp)]] = S0
        
        # Generate selection control
        ui[[isp + offset]] <-
          checkboxInput(
            inputId = paste0(tag,sp),
            label   = paste0(sp,' (orig: ',fname,')'),
            value   = FALSE
          )
      }
    }
  }
  return(list(ui = ui, extSpectra = extSpectra))
}
process_id = function(px) {
  if (is.null(px)) return(NULL)
  px$get_pid()
}
process_running = function(px) {
  if (is.null(px)) return(NULL)
  px$is_alive()
}
process_exit_status = function(px) {
  if (is.null(px)) return(NULL)
  px$get_exit_status()
}
process_result = function(px) {
  if (is.null(px)) return(NULL)
  if (is.null(process_running(px))) return(NULL)
  if (process_running(px)) return(NULL)
  if (is.null(process_exit_status(px))) return(NULL)
  if (process_exit_status(px) != 0) return(NULL)
  px$get_result()
}
process_status = function(px) {
  list(
    pid         = process_id(px), 
    running     = process_running(px),
    exit_status = process_exit_status(px),
    result      = process_result(px) 
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
