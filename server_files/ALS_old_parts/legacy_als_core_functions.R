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
