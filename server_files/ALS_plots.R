# ALS plotting utilities

plotAlsVec <- safely(function(alsOut, type = "Kin",
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
      lty = 1, lwd = 3, 
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
}, return_on_error = NULL)

plotResidAna <- safely(function(delay, wavl, mat, C, S,
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
}, return_on_error = NULL)

plotAmbVec <- safely(function(alsOut, solutions,
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
    if (is.null(xlim)) xlim <- range(xS, na.rm = TRUE)
    if (is.null(ylim)) ylim <- range(c(S, Smin, Smax), na.rm = TRUE)
    
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
    if (is.null(xlim)) xlim <- range(xC, na.rm = TRUE)
    if (is.null(ylim)) ylim <- range(c(C, Cmin, Cmax), na.rm = TRUE)
    
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
    legend(
      "topright",
      legend = colnames(C),
      lty = 1, lwd = 3, 
      col = cols
    )
    box()
  }
}, return_on_error = NULL)
