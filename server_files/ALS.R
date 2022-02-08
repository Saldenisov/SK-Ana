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
  wCloseC = NA) {
  
  # Interface to the core ALS code (myals)
  # Implements sequential dimension growth...
  
  # Run
  res <- list()
  for (n in nStart:nAls) { 
    
    cat(paste0('Running ALS (dim = ', n,')<br/>'))
    
    res[[n]] <- myals(
      C = C, Psi = mat, S = S, xC = delay, xS = wavl,
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
      silent = FALSE
    )
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
                  nullC = NA, closeC = FALSE, wCloseC = 0) {
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
    
    if ( !silent & iter%%10 == 1 ) {
      msg <- paste0(
        "Iter. (opt. ",
        ifelse(iter %% 2 == b, "S", "C"), "): ", iter,
        ", |RD| : ", signif(abs(RD), 3), " > ", thresh
      )
      cat(msg, "<br/>")
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
plotAlsVec <- function(alsOut, type = "Kin",
                       xlim = NULL, ylim = NULL,
                       plotUQ = FALSE, nMC = 100, 
                       nonnegS = TRUE, 
                       cols = NULL,
                       activeOnly = FALSE,
                       delayTrans = '',
                       ...) {
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  
  plotBands <- FALSE
  if (is.finite(alsOut$hessian) && plotUQ) {
    # Generate sample of curves
    Sigma <- try(solve(alsOut$hessian), silent = TRUE)
    if (class(Sigma) != "try-error" && alsOut$cnv == 0) {
      plotBands <- TRUE
      eps <- 0.0
      S <- alsOut$S
      epsS = ifelse(nonnegS,eps,-1e30)
      Smax <- matrix(epsS, nrow = nrow(S), ncol = ncol(S))
      Smin <- matrix(1e30, nrow = nrow(S), ncol = ncol(S))
      colnames(Smax) = colnames(Smin) = colnames(S)
      C <- alsOut$C
      Cmax <- matrix(eps, nrow = nrow(C), ncol = ncol(C))
      Cmin <- matrix(1e30, nrow = nrow(C), ncol = ncol(C))
      colnames(Cmax) = colnames(Cmin) = colnames(C)
      for (iMC in 1:nMC) {
        pmc <- mvtnorm::rmvnorm(
          n = 1,
          mean = alsOut$map,
          sigma = Sigma
        )
        map <- parExpand(pmc, alsOut$paropt)
        C <- kinet(map, alsOut$parms)
        Cmin = pmin(C,Cmin)
        Cmax = pmax(C,Cmax)
        Ca <- C[, alsOut$active]
        S <- spectra(Ca, map, alsOut$parms)
        Smin = pmin(S,Smin)
        Smax = pmax(S,Smax)
      }
    }
  }
  
  colF <- lineColors # Full colors
  colR <- colo_tr2   # Transparent colors
  if(!is.null(cols)) {
    names(colF) = Scheme$species
    names(colR) = Scheme$species
  }
  
  if (type == "Kin") {
    x <- alsOut$xC
    y <- alsOut$C
    if(activeOnly & !is.null(alsOut$active))
      y <- y[,alsOut$active]
    if (is.null(ylim)) {
      if(plotBands){
        if(activeOnly & !is.null(alsOut$active))
          ylim <- c(0, 1.1 * max(Cmax[,alsOut$active]))
        else
          ylim <- c(0, 1.1 * max(Cmax))
      } else {
        ylim <- c(0, 1.1 * max(y))
      }
    }
    sp = colnames(y)
    matplot(
      x, y,
      type = ifelse(length(x) > 20, "p", "b"),
      pch = 16, cex = 0.5, lwd = 2, lty = 3,
      col = if(is.null(cols)) colF else colF[sp],
      xlab = paste0("Delay ",delayTrans), 
      ylab = "C",
      xlim = xlim,
      ylim = ylim,
      main = paste0("Kinetics"),
      xaxs = "i", yaxs = "i"
    )
    grid()
    if (plotBands) {
      sel = 1:ncol(Cmin)
      if(activeOnly & !is.null(alsOut$active))
        sel = sel[alsOut$active]
      for (j in sel)
        polygon(
          c(x, rev(x)), c(Cmin[, j], rev(Cmax[, j])),
          col = if(is.null(cols)) colR[j] else colR[colnames(Cmin)[j]], 
          border = NA
        )
    }
    legend(
      "topright",
      legend = sp,
      lty = 3, lwd = 3, 
      col = if(is.null(cols)) colF else colF[sp],
    )
    colorizeMask1D(axis = "delay", ylim = ylim)
    box()
    
  } else {
    if (is.null(ylim)) {
      if(nonnegS)
        ylim <- c(0, 1.1 * max(alsOut$S))
      else
        ylim <- 1.1 * range(alsOut$S)
    }
    x <- alsOut$xS
    y <- alsOut$S
    sp = colnames(y)
    matplot(
      x, y,
      type = ifelse(length(x) > 20, "p", "b"),
      pch = 16, cex = 0.5, lwd = 2, lty = 3,
      col = if(is.null(cols)) colF else colF[sp],
      xlab = "Wavelength", ylab = "S",
      xlim = xlim,
      ylim = ylim,
      main = paste0(
        "Spectra / Lack-of-fit (%) : ",
        signif(alsOut$lof, 3)
      ),
      xaxs = "i", yaxs = "i"
    )
    if(!nonnegS)
      abline(h=0, lty=2)
    grid()
    if (plotBands) {
      for (j in 1:ncol(Smin))
        polygon(
          c(x, rev(x)), c(Smin[, j], rev(Smax[, j])),
          col = if(is.null(cols)) colR[j] else colR[colnames(Smin)[j]],
          border = NA
        )
    }
    colorizeMask1D(axis = "wavl", ylim = ylim)
    box()
  }
}
plotResidAna <- function(delay, wavl, mat, C, S,
                         d = rep(1, ncol(C)),
                         main = "Data", 
                         delayTrans = '',
                         ...) {
  # Compound plot with
  # - map of weighted residuals
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
    colorBar = TRUE,
    delayTrans = delayTrans
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
    xlab = paste0("Delay ",delayTrans),
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
alsStdOut = tempfile(tmpdir = '/tmp',fileext = '_als.stdout')
file.create(alsStdOut, showWarnings = FALSE)
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

nclicks <- reactiveVal(0)

doALS <- observeEvent(
  input$runALS, {
    if (isolate(!checkInputsSanity())) {
      return(NULL)
    }
    if(nclicks() != 0){
      showNotification("Already running ALS")
      return(NULL)
    }

    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)

    
    # Reinit ambRot vector selection
    updateCheckboxGroupInput(
      session,
      inputId = "vecsToRotate",
      selected = c(1, 2)
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
    
    id = showNotification(
      "Running ALS...",
      type = "message",
      duration = 5
    )
    
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
        wCloseC = 10^input$wCloseC
      ),
      package = TRUE,
      stdout = alsStdOut,
      stderr = alsStdOut
    )
    resALS$results = NULL
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
  req(alsOut <- resALS$results)
  
  plotAlsVec(alsOut,
             type = "Kin",
             xlim = rangesAlsKin$x,
             ylim = rangesAlsKin$y,
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
  req(alsOut <- resALS$results)
  
  plotAlsVec(alsOut,
             type = "Sp",
             xlim = rangesAlsSp$x,
             ylim = rangesAlsSp$y,
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
  })
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
    if (length(sol$solutions) == 0) {
      if (sol$finished) {
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
    resAmb$results = sol
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)

### Run ####
doAmbRot <- observeEvent(
  input$runALSAmb, {
    req(alsOut <- resALS$results)

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
    
    id = showNotification(
      "Running ambiguity explorer...",
      type = "message",
      duration = 5
    )
    
    fun = get(paste0('rotAmb',length(rotVec)))
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
    resAmb$results = NULL
    obsAmbStatus$resume()
    bgAmbpx <<- rx
    
    

  }
)


rangesAmbSp <- reactiveValues(x = NULL, y = NULL)

output$ambSpVectors <- renderPlot({
  req(resAmb$results)    
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
