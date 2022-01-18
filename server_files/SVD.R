# Functions ####
lof <- function(model, data) {
  100 * (
    sum((data - model)^2, na.rm = TRUE) /
      sum(data^2, na.rm = TRUE)
  )^0.5
}
getGlitch <- function(delayMask, wavlMask, mat, level) {
  # Detect points with largest amplitude in  SVD vector (#level)

  ## Replace masks/NAs by 0 (do not eliminate to facilitate indexing)
  mat0 <- mat
  mat0[is.na(delayMask), ] <- 0
  mat0[, is.na(wavlMask)] <- 0

  ## SVD
  s <- svd(mat0, nu = level, nv = level)

  ## Detect max.
  out <- which.max(abs(s$u[, level]))

  return(out)
}
plotImage <- function(x, y, z, main = "Data", col= imgColors,
                      xlim = NULL, ylim = NULL, zlim = NULL,
                      colorBar = FALSE, cont = FALSE, 
                      delayTrans = '') {
  # Plot image with masks

  if (is.null(xlim)) {
    xlim <- range(x, na.rm = TRUE)
  }
  if (is.null(ylim)) {
    ylim <- range(y, na.rm = TRUE)
  }
  if (is.null(zlim)) {
    zlim <- range(z, na.rm = TRUE)
  }

  image(
    x, y, z,
    xlim = xlim, ylim = ylim, zlim = zlim,
    xlab = paste0("Delay ",delayTrans),
    ylab = "Wavelength",
    main = main,
    col  = col
  )
  if (cont) {
    contour(x, y, z,
      nlevels = 10,
      col = lineColors[3],
      lwd = 1, add = TRUE
    )
  }
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)
  box()
  
  # Not robust...
  if(colorBar)
    image.plot(
      x, y, z,
      zlim = zlim,
      col = col,
      add = TRUE,
      legend.mar = 5,
      legend.shrink = 0.8,
      xlab = paste0("Delay ",delayTrans),
      ylab = "Wavelength"
    )
  
}
plotResid <- function(delay, wavl, mat, C, S,
                      d = rep(1, ncol(C)),
                      main = "",
                      delayTrans = '', ...) {
  # Build model matrix
  matAls <- rep(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:ncol(S))
    matAls <- matAls + C[, i] %o% S[, i] * d[i]
  resid <- matAls - mat
  vlof <- lof(model = matAls, data = mat)

  par(
    mfrow = c(1,2),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )

  plotImage(
    delay, wavl, resid,
    main = paste0(
      "Residuals / Lack-of-fit (%) : ",
      signif(vlof, 3)
    ),
    delayTrans = delayTrans
  )

  hist(
    resid[!is.na(resid)],
    col = cyan_tr, border = NA,
    xlim = range(c(resid, mat), na.rm=TRUE),
    xlab = "O.D.", main = "Residuals vs. data"
  )
  hist(
    mat,
    col = pink_tr, border = NA,
    add = TRUE
  )
  legend(
    "topright",
    bty = "n",
    c("Data", "Residuals"),
    pch = 15,
    col = c(pink_tr2, cyan_tr2)
  )
}
plotConbtribs <- function(delay, wavl, mat, C, S,
                          d = rep(1, ncol(C)),
                          type = "als", 
                          delayTrans = '',
                          ...) {
  # Plot image contribution of indivudual vectors

  # Estimate weight of species
  cont <- c()
  for (ic in 1:min(6, ncol(S))) {
    matSvd <- C[, ic] %o% S[, ic] * d[ic]
    if (type == "als") {
      cont[ic] <- sum(abs(matSvd), na.rm = TRUE)
    } else {
      cont[ic] <- d[ic]
    }
  }
  cont <- cont / sum(cont) * 100

  par(
    mfrow = c(2, 3),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )

  for (ic in 1:min(6, ncol(S))) {
    matSvd <- C[, ic] %o% S[, ic] * d[ic]
    plotImage(
      delay, wavl, matSvd,
      main = paste0(
        colnames(S)[ic], " / wgt. (%) = ",
        signif(cont[ic], 3)
      ),
      delayTrans = delayTrans
    )
  }
}
plotDatavsMod <- function(delay, wavl, mat, C, S,
                          d = rep(1, ncol(C)),
                          main = "Data",
                          cont = FALSE, 
                          delayTrans = '', ...) {
  # Build model matrix
  matAls <- rep(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:ncol(S))
    matAls <- matAls + C[, i] %o% S[, i] * d[i]

  par(
    mfrow = c(1, 2),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )

  # Data
  plotImage(
    delay, wavl, mat,
    main = "Data",
    cont = cont,
    delayTrans = delayTrans
  )

  # Model
  plotImage(
    delay, wavl, matAls,
    main = paste0("Model ", ncol(S), " species"),
    cont = cont,
    delayTrans = delayTrans  )
}


plotSvdLof <- function(s, mat, ...) {
  par(
    mfrow = c(1, 2),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )

  # S.V.
  plot(s$d[1:ncol(s$u)],
    type = "n",
    ylab = " ", log = "y",
    xlab = "Nb. of vectors", xlim = c(1, 10),
    main = "Singular Values"
  )
  grid()

  # Trend line for noise
  x <- 10:20
  y <- s$d[x]
  reg <- lm(y ~ x)
  xpred <- 1:20
  ypred <- predict(reg, newdata = data.frame(x = xpred))
  lines(xpred, ypred, lwd = 3, lty = 2, col = lineColors[7])

  # Singular values
  lines(s$d, col = lineColors[6], lwd = 3, lty = 3)
  points(s$d[1:ncol(s$u)], pch = 16, cex = 1.5, col = lineColors[3])
  text(1:ncol(s$u), s$d[1:ncol(s$u)],
    labels = 1:ncol(s$u),
    col = lineColors[5], pos = 3, offset = 0.5
  )
  box()

  # Lack_of-fit
  loft <- c()
  mat1 <- rep(0, nrow = nrow(s$u), ncol = ncol(s$v))
  for (i in 1:10) {
    mat1 <- mat1 + s$u[, i] %o% s$v[, i] * s$d[i]
    loft[i] <- lof(model = mat1, data = mat)
  }

  plot(
    loft,
    type = "n",
    xlab = "Nb. of vectors",
    ylab = " ", log = "y",
    main = "Lack-of-fit (%)"
  )
  grid()
  lines(loft, col = lineColors[6], lwd = 3, lty = 3)
  points(loft, pch = 16, cex = 1.5, col = lineColors[3])
  text(1:length(loft), loft,
    labels = signif(loft, 3),
    col = lineColors[5], pos = 3, offset = 0.5
  )
  box()
}
plotSVDVecBloc <- function(C, S, axisC, axisS, delayTrans = '', ...) {
  par(cex = cex, cex.main = cex)
  nco <- 2
  n <- min(floor(ncol(C) / 2), 5)
  fh <- 0.18

  for (icol in 1:nco) {
    ylim <- range(c(
      C[, ((icol - 1) * n + 1):(icol * n)],
      S[, ((icol - 1) * n + 1):(icol * n)]
    ),
    na.rm = TRUE
    )
    for (i in 1:n) {
      if (i == n) {
        xlab1 <- "Wavelength"
        xlab2 <- paste0("Delay ",delayTrans)
        xaxt <- "s"
        mar1 <- c(4.2, 4, 0, 0)
        mar2 <- c(4.2, 0, 0, 1.2)
      } else {
        xlab1 <- ""
        xlab2 <- ""
        xaxt <- "n"
        mar1 <- c(0, 4, 0, 0)
        mar2 <- c(0, 0, 0, 1.2)
      }
      par(
        fig = c(
          (icol - 1) * 0.5,
          (icol - 1) * 0.5 + 0.27,
          max(0, 1 - fh * i - ifelse(i == n, 0.11, 0)),
          1 - fh * (i - 1)
        ),
        new = ifelse(i * icol == 1, FALSE, TRUE),
        mar = mar1
      )
      plot(
        axisS, S[, (icol - 1) * n + i],
        type = "l", col = lineColors[6],
        xlab = xlab1, ylab = "Arb. units",
        xaxt = xaxt, ylim = ylim, lwd = 1
      )
      abline(h = 0, lty = 2)
      colorizeMask1D(axis = "wavl", ylim = ylim)
      grid()
      legend("topleft", legend = paste0((icol - 1) * n + i), bty = "n")

      par(
        fig = c(
          (icol - 1) * 0.5 + 0.27,
          (icol - 1) * 0.5 + 0.5,
          max(0, 1 - fh * i - ifelse(i == n, 0.11, 0)),
          1 - fh * (i - 1)
        ),
        new = TRUE,
        mar = mar2
      )
      plot(
        axisC, C[, (icol - 1) * n + i],
        type = "l", col = lineColors[3],
        xlab = xlab2, ylab = "", lwd = 1,
        xaxt = xaxt, yaxt = "n", ylim = ylim
      )
      abline(h = 0, lty = 2)
      colorizeMask1D(axis = "delay", ylim = ylim)
      grid()
    }
  }
}

# Interactive ####
doSVD <- reactive({
  # Perform SVD
  if (!checkInputsSanity()) {
    return(NULL)
  }
  
  # Suppress masked areas
  mat <- Inputs$mat
  mat <- mat[!is.na(Inputs$delayMask), ]
  mat <- mat[, !is.na(Inputs$wavlMask) ]
  
  validate(
    need(is.finite(diff(range(mat,na.rm = TRUE))),FALSE)
  )
  
  # Set max. nb. of SVD vectors
  nsvMax <- min(10, length(Inputs$delay), length(Inputs$wavl))

  svd(mat, nu = nsvMax, nv = nsvMax)
})

observeEvent(
  input$clean, {
    # Build vector of glitches
    gl <- getGlitch(
      Inputs$delayMask,
      Inputs$wavlMask,
      Inputs$mat,
      isolate(input$cleanLevel)
    )
    dlgl <- Inputs$delay[gl]
    if (anyNA(Inputs$delayGlitch)) {
      Inputs$delayGlitch <<- dlgl
    } else {
      Inputs$delayGlitch <<- unique(c(Inputs$delayGlitch, dlgl))
    }
  }
)
observeEvent(
  input$cleanCancel, {
    # Remove last glitch
    if (!anyNA(Inputs$delayGlitch)) {
      Inputs$delayGlitch <<-
        Inputs$delayGlitch[-length(Inputs$delayGlitch)]
    }
  }
)

output$svdSV <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  mat <- Inputs$mat
  # Suppress masked areas
  mat <- mat[!is.na(Inputs$delayMask), ]
  mat <- mat[, !is.na(Inputs$wavlMask) ]

  plotSvdLof(s, mat)
},
height = plotHeight, width = 2*plotHeight
)

output$svdVec <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, ncol(s$u))
  plotSVDVecBloc(CS$C, CS$S, Inputs$delay, Inputs$wavl, 
                 delayTrans = Inputs$delayTrans)
},
height = plotHeight, width = 2*plotHeight
)

output$svdResid <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, input$nSV)
  plotDatavsMod(Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    d = s$d,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight, width = 2*plotHeight
)

output$svdResid1 <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, input$nSV)
  plotResid(
    Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    d = s$d,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight, width = 2*plotHeight
)

output$svdContribs <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, input$nSV)
  colnames(CS$S) <- paste0("Vector ", 1:ncol(CS$S))
  plotConbtribs(
    Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    d = s$d, type = "svd",
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight, width = 2*plotHeight
)

output$svdStat <- DT::renderDataTable({
  # Build table of SVD indicators 
  # (S.V., Lack-of-fit, Sd(resid))
  if (is.null(s <- doSVD())) {
    return(NULL)
  }

  # Max number of lines in table
  nsvMax <- min(
    10,
    length(Inputs$delay),
    length(Inputs$wavl)
  )

  # Suppress masked areas to conform with SVD results
  mat <- Inputs$mat
  mat <- mat[!is.na(Inputs$delayMask), ]
  mat <- mat[, !is.na(Inputs$wavlMask)]

  mat1 <- rep(0, nrow = nrow(s$u), ncol = ncol(s$v))
  sv <- loft <- sig <- c()
  for (i in 1:nsvMax) {
    mat1 <- mat1 + s$u[, i] %o% s$v[, i] * s$d[i]
    sig[i] <- sd(mat - mat1)
    loft[i] <- lof(model = mat1, data = mat)
    sv[i] <- s$d[i]
  }

  data <- cbind(
    id = 1:nsvMax,
    signif(sv, 3),
    signif(loft, 3),
    signif(sig, 3)
  )
  colnames(data) <- c(
    "Rank",
    "Sing. Val.",
    "Lack-of-fit (%)",
    "Sd(resid)"
  )
  DT::datatable(
    data,
    class = "cell-border stripe",
    options = list(
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      dom = "t"
    ),
    escape = FALSE
  )
})


outputOptions(output, "svdSV", suspendWhenHidden = FALSE)
outputOptions(output, "svdVec", suspendWhenHidden = FALSE)
