# enableBookmarking(store = "server")

options(shiny.maxRequestSize = 20 * 1024 ^ 2)

# options(shiny.json.digits=32)

# Libraries ####
## CRAN
libs <- c(
  "outliers", "nnls", "Iso", "viridis", "httpuv",
  "changepoint", "shiny", "shinyBS", "DT", "Rsolnp",
  "fields", "NMF", "tools", "shinycssloaders",
  "rgenoud", "mvtnorm", "deSolve", "msm"
)
for (lib in libs) {
  if (!require(lib, character.only = TRUE, quietly = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE, quietly = TRUE)
}

# Colors ####
cols <- viridis::viridis(128)
col2tr <- function(col, alpha)
  rgb(unlist(t(col2rgb(col))), alpha = alpha, maxColorValue = 255)
cyan_tr <- col2tr("cyan", 120)
pink_tr <- col2tr("pink", 120)
mask_tr <- "gray95"
colWR <- fields::two.colors(17, start = "blue", middle = "white", end = "red")

# Global graphical params ####
cex <- 1
mar <- c(4.5, 5, 2, 1)
mgp <- c(2, .75, 0)
pty <- "m"
tcl <- -0.5

# Functions ####
string2Expr <- function(string) {
  dat <- try(parse(text = string), silent = TRUE)
  if (!is(dat, "try-error")) {
    return(dat)
  } else {
    return(NULL)
  }
}
string2Num <- function(x)
  as.numeric(eval(parse(text = eval(string2Expr(x)))))

downsizeMatrix <- function(delay, wavl, mat, fwD=1, fwW=NULL) {
  # Downsize matrix by factors fwD (delay) and fwW (wavl)

  if (is.null(fwW)) {
    fwW <- fwD
  }

  # pad matrix with Nas
  newNrow <- ceiling(nrow(mat) / fwD) * fwD
  newNcol <- ceiling(ncol(mat) / fwW) * fwW
  lmat <- matrix(NA, nrow = newNrow, ncol = newNcol)
  lmat[1:nrow(mat), 1:ncol(mat)] <- mat

  # Block average
  nRowBloc <- newNrow / fwD
  nColBloc <- newNcol / fwW
  amat <- matrix(NA, nrow = nRowBloc, ncol = nColBloc)
  for (i in 1:nRowBloc)
    for (j in 1:nColBloc)
      amat[i, j] <- mean(
        lmat[
          ((i - 1) * fwD + 1):(i * fwD),
          ((j - 1) * fwW + 1):(j * fwW)
        ],
        na.rm = TRUE
      )
  delay[newNrow] <- NA
  adelay <- c()
  for (i in 1:nRowBloc)
    adelay[i] <- mean(
      delay[((i - 1) * fwD + 1):(i * fwD)],
      na.rm = TRUE
    )
  wavl[newNcol] <- NA
  awavl <- c()
  for (i in 1:nColBloc)
    awavl[i] <- mean(
      wavl[((i - 1) * fwW + 1):(i * fwW)],
      na.rm = TRUE
    )
  return(
    list(
      mat = amat,
      delay = adelay,
      wavl = awavl
    )
  )
}

getC <- function(S, data, C, nonnegC=TRUE,
                 nullC = NA, closeC=FALSE, wCloseC = 0) {
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
      mod <- loess(y~x, span = smooth)
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
                  normS = TRUE, uniS = FALSE, S0 = NULL, smooth=0,
                  silent = TRUE, SumS = FALSE, hardS0 = TRUE,
                  wHardS0 = 1.0,
                  nullC = NA, closeC=FALSE, wCloseC = 0,
                  updateProgress = NULL) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS

  RD <- 10 ^ 20

  resid <- matrix(0, nrow(Psi), ncol(Psi))
  for (i in 1:nrow(Psi))
    resid[i, ] <- Psi[i, ] - C[i, ] %*% t(S)

  initialrss <- oldrss <- sum((resid) ^ 2) / sum(Psi ^ 2)

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

    for (i in 1:nrow(Psi))
      resid[i, ] <- Psi[i, ] - C[i, ] %*% t(S)

    rss <- sum(resid ^ 2) / sum(Psi ^ 2)
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
  lof <- signif(100 * (sum(resid ^ 2) / sum(Psi ^ 2)) ^ 0.5, 4)
  msg <- HTML(paste0(
    "|RD| : ", signif(abs(RD), 3), " <= ", thresh,
    "<br/> L.O.F. = ", lof
  ))

  # "Initial RSS / Final RSS = ", signif(initialrss,3),
  # "/", signif(rss,3), " = ", signif(initialrss / rss,3),

  if (!silent) {
    cat(msg)
  }

  return(list(
    C = C, S = S, xC = xC, xS = xS, Psi = Psi,
    rss = rss, resid = resid, iter = iter,
    msg = msg, lof = lof
  ))
}

indxCuts <- function(xCut, coords, minx=50) {
  delta <- 0
  # Select indices around cut
  if (xCut == coords[1]) {
    # First point
    indx <- c(1)
  } else {
    if (xCut == coords[length(coords)]) {
      # Last point
      indx <- c(length(coords))
    } else {
      if (length(coords) > 2 * minx) {
        # Select points around cut
        delta <- diff(range(coords)) / minx
        indx <- which(coords > xCut - delta / 2 &
          coords < xCut + delta / 2)
      } else {
        # Select point nearest cut
        indx <- c(which.min(abs(coords - xCut)))
      }
    }
  }
  return(list(indx = indx, delta = delta))
}



autoDlMask <- function(mat, nmat) {
  # Locate empty delay areas (experimental)

  # Integrate on wavl
  trace <- rowSums(mat)

  # Special treatment for nmat=1
  # (SegNeigh fails with Q=2 !!!)
  ans <- changepoint::cpt.var(
    diff(trace), penalty = "BIC",
    method = "SegNeigh",
    Q = 2 + max(1, 2 * (nmat - 1))
  )
  if (nmat == 1) {
    chp <- cpts(ans)[2]
  } else {
    chp <- cpts(ans)
  }

  return(chp)
}

autoWlMask <- function(mat, nmat) {
  # Locate useless wavl areas (experimental)

  # Integrate on wavl
  trace <- colSums(mat)

  # Special treatment for nmat=1
  # (SegNeigh fails with Q=2 !!!)
  ans <- changepoint::cpt.var(
    diff(trace), penalty = "BIC",
    method = "SegNeigh",
    Q = 2 + max(1, 2 * (nmat - 1))
  )
  chp <- sort(cpts(ans))

  return(chp)
}
cleanUp <- function(delayMask, wavlMask, mat, level) {
  # Remove glitches from matrix by detecting outliers in SVD vectors...

  # Replace masks/NAs by 0 (do not eliminate to facilitate indexing)
  mat0 <- mat
  mat0[is.na(delayMask), ] <- 0
  mat0[, is.na(wavlMask)] <- 0

  # SVD
  s <- svd(mat0, nu = level, nv = level)

  # Use SV vectors
  vec <- abs(s$u[, level])
  out <- which.max(vec)

  return(out)
}
