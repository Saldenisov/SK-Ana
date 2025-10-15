# Debugging version of ALS functions with explicit package loading

getC <- function(S, data, C, nonnegC = TRUE,
                 nullC = NA, closeC = FALSE, wCloseC = 0) {
  # EXPLICIT PACKAGE LOADING FOR DOCKER
  if (!requireNamespace("nnls", quietly = TRUE)) {
    stop("nnls package not available in subprocess")
  }
  library(nnls)
  
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
  
  return(C)
}

getS <- function(C, data, S, xS, nonnegS, uniS,
                 S0, normS, smooth, SumS, hardS0,
                 wHardS0) {
  # EXPLICIT PACKAGE LOADING FOR DOCKER
  if (!requireNamespace("nnls", quietly = TRUE)) {
    stop("nnls package not available in subprocess")
  }
  library(nnls)
  
  if (!requireNamespace("Iso", quietly = TRUE)) {
    stop("Iso package not available in subprocess")
  }
  library(Iso)
  
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

als_debug <- function(
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
  
  # EXPLICIT PACKAGE LOADING FOR DOCKER
  cat("DEBUG: Loading required packages in subprocess\n")
  required_packages <- c('nnls', 'Iso', 'mvtnorm', 'fields', 'Rsolnp', 'deSolve', 'msm', 'changepoint', 'outliers', 'rgenoud', 'NMFN')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("DEBUG: Package", pkg, "not found, attempting to load\n")
    } else {
      library(pkg, character.only = TRUE)
      cat("DEBUG: Package", pkg, "loaded successfully\n")
    }
  }
  
  # Test nnls availability
  tryCatch({
    test_result <- nnls::nnls(matrix(c(1,1,2,3), nrow=2), c(6,14))
    cat("DEBUG: nnls test successful\n")
  }, error = function(e) {
    cat("DEBUG: nnls test failed:", e$message, "\n")
    stop("NNLS package not working in subprocess")
  })
  
  # Interface to the core ALS code (myals)
  # Implements sequential dimension growth...
  
  # Run
  res <- list()
  for (n in nStart:nAls) { 
    
    cat(paste0('Running ALS (dim = ', n,')<br/>'))
    
    res[[n]] <- myals_debug(
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
      silent = FALSE
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

myals_debug <- function(C, Psi, S,
                 thresh = 0.001,
                 maxiter = 100,
                 xC = 1:nrow(C),
                 xS = 1:nrow(S),
                 nonnegC = TRUE, nonnegS = TRUE, optS1st = TRUE,
                 normS = TRUE, uniS = FALSE, S0 = NULL, smooth = 0,
                 silent = TRUE, SumS = FALSE, hardS0 = TRUE,
                 wHardS0 = 1.0,
                 nullC = NA, closeC = FALSE, wCloseC = 0) {
  
  cat("DEBUG: Starting myals_debug function\n")
  
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
    if(!is.finite(RD))
      return(NULL)
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
  msg <- paste0(
    "Dimension :", ncol(S),
    "<br/>|RD| : ", signif(abs(RD), 3), " <= ", thresh,
    "<br/> Lack-of-fit (%) : ", signif(vlof, 3)
  )
  
  return(
    list(
      C = C, S = S, xC = xC, xS = xS, Psi = Psi,
      rss = rss, resid = resid, iter = iter,
      msg = msg, lof = vlof
    )
  )
}

# Helper function (from your existing codebase)
lof <- function(model, data) {
  # Calculate lack of fit
  residuals <- data - model
  rss <- sum(residuals^2, na.rm = TRUE)
  tss <- sum((data - mean(data, na.rm = TRUE))^2, na.rm = TRUE)
  lof_percent <- 100 * sqrt(rss / tss)
  return(lof_percent)
}