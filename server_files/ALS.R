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
      cc <- try(nnls(S, data[i, ]))
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
                 wHardS0) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  # 2017-12-07 : replaced direct substitution of S0 by direct elimination
  
  C[which(is.nan(C))] <- 1
  
  if (!is.null(S0)) {
    nS0 <- ncol(S0)
    if (hardS0) {
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
      # Area
      for (i in 1:ncol(S))
        S[, i] <- S[, i] / sum(S[, i])
    } else {
      # Amplitude
      for (i in 1:ncol(S))
        S[, i] <- S[, i] / ifelse(max(S[, i] > 0), max(S[, i]), 1)
    }
  }
  
  if (!is.null(S0) & hardS0) {
    # Combine optimized spectra with constrained ones
    S <- cbind(S0, S)
    C <- cbind(C0, C)
  }
  
  return(S)
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
                  updateProgress = NULL) {
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
      # Spectra normalization
      if (SumS) {
        # Area
        for (i in 1:ncol(S0))
          S0[, i] <- S0[, i] / sum(S0[, i])
      } else {
        # Amplitude
        for (i in 1:ncol(S0))
          S0[, i] <- S0[, i] / ifelse(max(S0[, i] > 0), max(S0[, i]), 1)
      }
    }
  }
  
  if (!silent) {
    cat("Initial RSS", initialrss, "\n")
  }
  b <- ifelse(optS1st, 1, 0)
  iter <- 0
  oneMore <- TRUE
  while ((abs(RD) > thresh && maxiter >= iter) || oneMore) {
    iter <- iter + 1
    
    if (iter %% 2 == b) {
      S <- getS(
        C, Psi, S, xS, nonnegS, uniS,
        S0, normS, smooth, SumS, hardS0, wHardS0
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
    oldrss <- rss
    
    msg <- paste0(
      "Iter. (opt. ",
      ifelse(iter %% 2 == b, "S", "C"), "): ", iter,
      ", |RD| : ", signif(abs(RD), 3), " > ", thresh
    )
    
    if (!silent) {
      cat(msg, "\n")
    }
    if (is.function(updateProgress)) {
      updateProgress(
        value = iter / maxiter,
        detail = msg
      )
    }
    
    oneMore <- ((iter %% 2 != b) && maxiter != 1 && iter <= 2)
  }
  vlof <- lof(model = mod, data = Psi)
  msg <- HTML(paste0(
    "|RD| : ", signif(abs(RD), 3), " <= ", thresh,
    "<br/> Lack-of-fit (%) : ", signif(vlof, 3)
  ))
  
  if (!silent) {
    cat(msg)
  }
  
  return(
    list(
      C = C, S = S, xC = xC, xS = xS, Psi = Psi,
      rss = rss, resid = resid, iter = iter,
      msg = msg, lof = vlof
    )
  )
}

plotAlsVec <- function(alsOut, type = "Kin",
                       xlim = NULL, ylim = NULL,
                       plotUQ = FALSE, nMC = 100, ...) {
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  
  nvec <- ncol(alsOut$S)
  
  plotBands <- FALSE
  if (is.finite(alsOut$hessian) && plotUQ) {
    # Generate sample of curves
    Sigma <- try(solve(alsOut$hessian), silent = TRUE)
    if (class(Sigma) != "try-error" && alsOut$cnv == 0) {
      plotBands <- TRUE
      eps <- 0.0
      S <- alsOut$S
      Smax <- matrix(eps, nrow = nrow(S), ncol = nvec)
      Smin <- matrix(1e30, nrow = nrow(S), ncol = nvec)
      C <- alsOut$C
      Cmax <- matrix(eps, nrow = nrow(C), ncol = nvec)
      Cmin <- matrix(1e30, nrow = nrow(C), ncol = nvec)
      for (iMC in 1:nMC) {
        pmc <- mvtnorm::rmvnorm(
          n = 1,
          mean = alsOut$map,
          sigma = Sigma
        )
        map <- parExpand(pmc, alsOut$paropt)
        sel <- alsOut$parms[["active"]]
        C <- kinet(map, alsOut$parms)[, sel]
        S <- spectra(C, map, alsOut$parms)
        for (j in 1:nvec) {
          for (k in 1:nrow(C)) {
            Cmin[k, j] <- min(Cmin[k, j], C[k, j], na.rm = TRUE)
            Cmax[k, j] <- max(Cmax[k, j], C[k, j], na.rm = TRUE)
          }
          for (k in 1:nrow(S)) {
            Smin[k, j] <- min(Smin[k, j], S[k, j], na.rm = TRUE)
            Smax[k, j] <- max(Smax[k, j], S[k, j], na.rm = TRUE)
          }
        }
      }
    }
  }
  
  colF <- lineColors
  colR <- colo_tr2
  
  if (type == "Kin") {
    if (is.null(ylim)) {
      ylim <- c(0, 1.1 * max(alsOut$C))
    }
    x <- alsOut$xC
    matplot(
      x, alsOut$C,
      type = ifelse(length(x) > 20, "p", "b"),
      pch = 16, cex = 0.5, lwd = 2, lty = 3,
      col = colF,
      xlab = "Delay", ylab = "C",
      xlim = xlim,
      ylim = ylim,
      main = paste0("Kinetics"),
      xaxs = "i", yaxs = "i"
    )
    grid()
    if (plotBands) {
      for (j in 1:nvec)
        polygon(
          c(x, rev(x)), c(Cmin[, j], rev(Cmax[, j])),
          col = colR[j], border = NA
        )
    }
    legend(
      "topright",
      legend = colnames(alsOut$C),
      lty = 3, lwd = 3, col = colF
    )
    colorizeMask1D(axis = "delay", ylim = ylim)
    box()
  } else {
    if (is.null(ylim)) {
      ylim <- c(0, 1.1 * max(alsOut$S))
    }
    x <- alsOut$xS
    matplot(
      x, alsOut$S,
      type = ifelse(length(x) > 20, "p", "b"),
      pch = 16, cex = 0.5, lwd = 2, lty = 3,
      col = colF,
      xlab = "Wavelength", ylab = "S",
      xlim = xlim,
      ylim = ylim,
      main = paste0(
        "Spectra / Lack-of-fit (%) : ",
        signif(alsOut$lof, 3)
      ),
      xaxs = "i", yaxs = "i"
    )
    grid()
    if (plotBands) {
      for (j in 1:nvec)
        polygon(
          c(x, rev(x)), c(Smin[, j], rev(Smax[, j])),
          col = colR[j], border = NA
        )
    }
    colorizeMask1D(axis = "wavl", ylim = ylim)
    box()
  }
}
plotResidAna <- function(delay, wavl, mat, C, S,
                         d = rep(1, ncol(C)),
                         main = "Data", ...) {
  # Compound plot with
  # - map od weighted residuals
  # - 2 vectors of SVD decomposition of residuals
  # - Normal QQ-plot of residuals
  
  # Build model matrix
  matAls <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:ncol(S))
    matAls <- matAls + C[, i] %o% S[, i] * d[i]
  resid <- matAls - mat
  resid[!is.finite(resid)] <- 0
  rm(matAls)
  
  wres <- resid / sd(resid)
  sv <- svd(wres, nu = 2, nv = 2)
  
  par(
    mfrow = c(2, 2),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = "s"
  )
  
  plotImage(
    delay, wavl, wres,
    col = resColors,
    main = "Weighted Residuals",
    zlim = c(-3, 3),
    colorBar = TRUE
  )
  
  matplot(
    sv$v, wavl,
    type = "l", lwd = 2,
    xlab = "Sing. Vec.",
    ylab = "Wavelength",
    main = "SVD of Residuals",
    col = lineColors[c(6, 3)]
  )
  colorizeMask1D(
    axis = "wavl", dir = "h",
    ylim = range(sv$v, na.rm = TRUE)
  )
  abline(v = 0)
  box()
  
  matplot(
    delay, sv$u,
    type = "l", lwd = 2,
    xlab = "Delay",
    ylab = "Sing. Vec.",
    main = "SVD of Residuals",
    col = lineColors[c(6, 3)]
  )
  colorizeMask1D(
    axis = "delay",
    ylim = range(sv$u, na.rm = TRUE)
  )
  abline(h = 0)
  box()
  
  qqnorm(wres, col = lineColors[3])
  abline(a = 0, b = 1, col = lineColors[6])
  grid()
  box()
}
plotAmbVec <- function(alsOut, solutions,
                       type = "Kin",
                       displayLines = FALSE,
                       xlim = NULL, ylim = NULL, ...) {
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
      main = "Kinetics", xlab = "Delay",
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
  
  ttry <- function(i) dens * i
  
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
          # if(!is.null(updateProgress))
          #   updateProgress(value = iter / 100)
          
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
          
          httpuv::service()
          if (input$killALSAmb) { # Get out of here
            if (length(solutions) != 0) {
              solutions$rotVec <- rotVec
              solutions$eps <- eps
            }
            return(
              list(
                solutions = solutions,
                finished = FALSE
              )
            )
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
  
  ttry <- function(i) dens * i
  
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
                          httpuv::service()
                          if (input$killALSAmb) { # Get out of here
                            # print(length(solutions))
                            if (length(solutions) != 0) {
                              solutions$rotVec <- rotVec
                              solutions$eps <- eps
                            }
                            return(
                              list(
                                solutions = solutions,
                                finished = FALSE
                              )
                            )
                          }
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

getS0 <- eventReactive(
  input$S0File, {
    isolate({
      
      # Get all shapes
      S0_in <- list()
      i <- 0
      for (fN in input$S0File$datapath) {
        tmp <- read.table(
          file = fN,
          header = FALSE,
          dec = input$dec,
          sep = input$sep,
          colClasses = "numeric",
          stringsAsFactors = FALSE
        )
        i <- i + 1
        S0_in[[i]] <- tmp
      }
      return(S0_in)
    })
  }
)

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
output$showMSE <- reactive({
  showMSE(
    input$procMult,
    input$rawData_rows_selected,
    input$nALS
  )
})
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

als <- function() {
  nAls <- input$nALS
  
  # (Re)-init output
  lapply(1:10, function(n) {
    output[[paste0("iter", n)]] <<- renderUI({
      list()
    })
  })
  
  # Reinit ambRot vector selection
  updateCheckboxGroupInput(
    session,
    inputId = "vecsToRotate",
    selected = c(1, 2)
  )
  
  # Suppress masked areas from coordinates
  delay <- Inputs$delay[!is.na(Inputs$delayMask)]
  delayId <- Inputs$delayId[!is.na(Inputs$delayMask)]
  wavl <- Inputs$wavl[!is.na(Inputs$wavlMask)]
  
  
  
  if (input$useFiltered) {
    # Choose SVD filtered matrix
    s <- doSVD()
    mat <- matrix(0, nrow = length(delay), ncol = length(wavl))
    for (ic in 1:input$nSV)
      mat <- mat + s$u[, ic] %o% s$v[, ic] * s$d[ic]
  } else {
    mat <- Inputs$mat
    mat <- mat[!is.na(Inputs$delayMask), ]
    mat <- mat[, !is.na(Inputs$wavlMask) ]
  }
  
  # External spectrum shapes
  S0 <- NULL
  if (input$shapeS) {
    if (is.null(S0_in)) {
      S0_in <- getS0()
    }
    ii <- 0
    S0 <- c()
    for (i in 1:length(S0_in)) {
      tmp <- S0_in[[i]]
      for (k in 2:ncol(tmp))
        S0 <- cbind(S0, spline(tmp[, 1], tmp[, k], xout = wavl)$y)
    }
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
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  updateProgress <- function(value = NULL, detail = NULL) {
    progress$set(value = value, detail = detail)
  }
  
  if (input$initALS == "seq") {
    nStart <- 2
  } else {
    nStart <- nAls
  }
  
  if (input$initALS == "SVD" || input$initALS == "seq") {
    # initialize with abs(SVD)
    if (is.null(s <- doSVD())) {
      return(NULL)
    }
    
    S <- matrix(abs(s$v[, 1:nStart]), ncol = nStart)
    C <- matrix(abs(s$u[, 1:nStart]), ncol = nStart)
  } else if (input$initALS == "NMF") {
    # initialize with SVD + NMF
    if (is.null(s <- doSVD())) {
      return(NULL)
    }
    
    progress$set(message = "Running NMF ", value = 0)
    
    # 1/ filter matrix to avoid negative values (noise)
    fMat <- rep(0, nrow = nrow(data), ncol = ncol(data))
    for (i in 1:nStart)
      fMat <- fMat + s$u[, i] %o% s$v[, i] * s$d[i]
    # 2/ NMF
    # res  = NMF::nmf(abs(fMat), rank=nStart, method='lee')
    # C = NMF::basis(res)
    # S = t(NMF::coef(res))
    res <- NMFN::nnmf(abs(fMat),
                      k = nStart,
                      method = "nnmf_als",
                      eps = 1e-8)
    C <- res$W
    S <- t(res$H)
  } else {
    # restart from existing solution
    if (!exists("RES")) {
      return(NULL)
    }
    S <- RES$S[, 1:nStart]
    C <- RES$C[, 1:nStart]
  }
  
  # Progress bar
  progress$set(message = "Running ALS ", value = 0)
  
  # Run
  res <- list()
  for (n in nStart:nAls) {
    progress$set(message = paste0("Running ALS ", n), value = 0)
    res[[n]] <- myals(
      C = C, Psi = mat, S = S, xC = delay, xS = wavl,
      maxiter = input$maxiter,
      uniS = input$uniS,
      nonnegS = input$nonnegS,
      nonnegC = input$nonnegC,
      thresh = 10^input$alsThresh,
      normS = input$normS,
      S0 = S0,
      hardS0 = !input$softS0,
      wHardS0 = 10^input$wSoftS0,
      optS1st = input$optS1st,
      smooth = input$smooth,
      SumS = input$SumS,
      updateProgress = updateProgress,
      nullC = nullC,
      closeC = input$closeC,
      wCloseC = 10^input$wCloseC
    )
    res[[n]]$hessian <- NA # Compatibility with Kinet
    
    if (n < nAls) {
      # Prepare next iteration
      S <- res[[n]]$S
      C <- res[[n]]$C
      S <- cbind(S, 1)
      C <- cbind(C, 1)
    }
    RES <<- res[[n]]
  }
  
  lapply(nStart:nAls, function(n) {
    output[[paste0("iter", n)]] <<- renderUI({
      list(
        h4(n, " species"),
        h5("Terminated in ", res[[n]]$iter, " iterations"),
        res[[n]]$msg,
        br()
      )
    })
  })
  
  colnames(res[[nAls]]$S) <- paste0("S_", 1:nAls)
  colnames(res[[nAls]]$C) <- paste0("C_", 1:nAls)
  
  # Update Reporting
  updateCheckboxGroupInput(session,
                           inputId = "inReport",
                           selected = c("SVD", "ALS")
  )
  
  res[[nAls]]$nullC <- nullC
  
  return(res[[nAls]])
}

doALS <- eventReactive(
  input$runALS, {
    if (isolate(!checkInputsSanity())) {
      return(NULL)
    }
    return(als())
  }
)

output$alsOpt <- renderUI({
  if (input$runALS == 0) {
    h5('Select ALS options and press "Run"\n')
  } else {
    alsOut <- doALS()
    h5("ALS done")
    isolate({
      if (alsOut$iter >= input$maxiter) {
        strong("Warning : maxiter limit reached !!!")
      }
    })
  }
})

output$alsResid1 <- renderPlot({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
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
                main = main)
},
height = plotHeight)

output$alsResid3 <- renderPlot({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
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
            main = main)
},
height = plotHeight)

output$alsResid2 <- renderPlot({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
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
               main = main)
},
height = plotHeight)

rangesAlsKin <- reactiveValues(x = NULL, y = NULL)

output$alsKinVectors <- renderPlot(
  {
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  plotAlsVec(alsOut,
             type = "Kin",
             xlim = rangesAlsKin$x,
             ylim = rangesAlsKin$y
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

output$alsSpVectors <- renderPlot(
  {
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  plotAlsVec(alsOut,
             type = "Sp",
             xlim = rangesAlsSp$x,
             ylim = rangesAlsSp$y
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

observeEvent(
  input$alsSpKinSave,
  isolate({
    if (is.null(alsOut <- doALS())) {
      return(NULL)
    }
    
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
  })
)

output$alsContribs <- renderPlot(
  {
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  plotConbtribs(
    Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S
  )
},
height = plotHeight
)

output$selAmbParams <- renderUI({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
  lv <- list()
  for (i in 1:input$nALS)
    lv[[i]] <- i
  
  list(
    fluidRow(
      column(
        4,
        checkboxGroupInput("vecsToRotate",
                           label = "Pick 2 (or 3) vectors",
                           choices = lv,
                           selected = c(1, 2)
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



doAmbRot <- eventReactive(
  input$runALSAmb, {
    if (is.null(alsOut <- doALS())) {
      return(NULL)
    }
    
    updateButton(session, "killALSAmb", value = FALSE)
    
    isolate({
      rotVec <- as.numeric(unlist(input$vecsToRotate))
      eps <- input$alsRotAmbEps
      dens <- input$alsRotAmbDens
    })
    
    if (length(rotVec) > 3) {
      showModal(modalDialog(
        title = ">>>> Too Many Vectors Selected <<<< ",
        paste0("Please choose 3 vectors max."),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "s"
      ))
    }
    
    C0 <- alsOut$C
    S0 <- alsOut$S
    nullC <- alsOut$nullC
    
    solutions <- NULL
    finished <- FALSE
    if (length(rotVec) == 2) {
      sol <- rotAmb2(C0, S0,
                     data = Inputs$mat, nullC = nullC,
                     rotVec = rotVec, dens = dens, eps = eps,
                     updateProgress = NULL
      )
      solutions <- sol$solutions
      finished <- sol$finished
    }
    else if (length(rotVec) == 3) {
      sol <- rotAmb3(C0, S0,
                     data = Inputs$mat, nullC = nullC,
                     rotVec = rotVec, dens = dens, eps = eps,
                     updateProgress = updateProgress
      )
      solutions <- sol$solutions
      finished <- sol$finished
    }
    
    if (length(solutions) == 0) {
      if (finished) {
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
        showModal(modalDialog(
          title = ">>>> No Solution Found <<<< ",
          paste0("Try to let the algorithm run for a longer time!"),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "s"
        ))
      }
    }
    
    return(solutions)
  }
)


rangesAmbSp <- reactiveValues(x = NULL, y = NULL)

output$ambSpVectors <- renderPlot(
  {
    if (is.null(alsOut <- doALS())) {
      return(NULL)
    }
    if (!is.list(solutions <- doAmbRot())) {
      cat(paste0("No solutions found \n"))
    } else {
      plotAmbVec(alsOut, solutions,
                 type = "Sp",
                 displayLines = input$ambDisplayLines,
                 xlim = rangesAmbSp$x,
                 ylim = rangesAmbSp$y
      )
    }
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

output$ambKinVectors <- renderPlot(
  {
    if (is.null(alsOut <- doALS())) {
      return(NULL)
    }
    
    if (!is.list(solutions <- doAmbRot())) {
      cat(paste0("No solutions found \n"))
    } else {
      plotAmbVec(alsOut, solutions,
                 type = "Kin",
                 displayLines = input$ambDisplayLines,
                 xlim = rangesAmbKin$x,
                 ylim = rangesAmbKin$y
      )
    }
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
