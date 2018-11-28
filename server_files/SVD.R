# Functione ####
plotConbtribs <- function(delay, wavl, mat, C, S,
                          d = rep(1, ncol(C)),
                          type = "als", ...) {
  xlim <- range(delay, na.rm = TRUE)
  ylim <- range(wavl, na.rm = TRUE)

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

  dummy <- split.screen(c(2, 3))
  for (ic in 1:min(6, ncol(S))) {
    screen(ic)
    par(
      cex = cex, cex.main = cex, mar = mar,
      mgp = mgp, tcl = tcl, pty = pty
    )
    matSvd <- C[, ic] %o% S[, ic] * d[ic]
    image(
      delay, wavl, matSvd,
      col = imgColors,
      xlim = xlim, ylim = ylim,
      xlab = "Delay", ylab = "Wavelength",
      main = paste0(
        colnames(S)[ic], " / wgt. (%) = ",
        signif(cont[ic], 3)
      )
    )
    colorizeMask1D(axis = "delay", ylim = ylim)
    colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)
  }
  dummy <- close.screen(all.screens = TRUE)
}
plotSVDVecBloc <- function(C, S, axisC, axisS, ...) {
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
        xlab2 <- "Delay"
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
plotDatavsMod <- function(delay, wavl, mat, C, S,
                          d = rep(1, ncol(C)),
                          main = "Data",
                          cont = FALSE, ...) {
  # Build model matrix
  matAls <- rep(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:ncol(S))
    matAls <- matAls + C[, i] %o% S[, i] * d[i]
  zlim <- range(mat, na.rm = TRUE)

  xlim <- range(delay, na.rm = TRUE)
  ylim <- range(wavl, na.rm = TRUE)

  par(mfrow = c(1, 2))
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )

  image(
    delay, wavl, mat,
    xlim = xlim, ylim = ylim,
    xlab = "Delay", ylab = "Wavelength",
    main = main, col = imgColors, zlim = zlim
  )
  if (cont) {
    contour(delay, wavl, mat, 10, col = "gold", lwd = 2, add = TRUE)
  }
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)

  image(
    delay, wavl, matAls,
    xlim = xlim, ylim = ylim,
    xlab = "Delay", ylab = "Wavelength",
    main = paste0("Model ", ncol(S), " species"),
    col = imgColors, zlim = zlim
  )
  if (cont) {
    contour(delay, wavl, matAls, 10, col = "gold", lwd = 2, add = TRUE)
  }
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)
}

plotResidOnly <- function(delay, wavl, mat, C, S,
                          d = rep(1, ncol(C)),
                          main = "Data", ...) {
  # Build model matrix
  matAls <- rep(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:ncol(S))
    matAls <- matAls + C[, i] %o% S[, i] * d[i]
  zlim <- range(mat, na.rm = TRUE)

  xlim <- range(delay, na.rm = TRUE)
  ylim <- range(wavl, na.rm = TRUE)

  resid <- matAls - mat
  lof <- 100 * (sum(resid^2, na.rm = TRUE) /
    sum(mat^2, na.rm = TRUE)
  )^0.5

  par(mfrow = c(1, 2))
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )

  image(
    delay, wavl, resid,
    xlim = xlim, ylim = ylim,
    xlab = "Delay", ylab = "Wavelength",
    main = paste0("Residuals; Lack-of-fit = ", signif(lof, 3), "%"),
    col = imgColors
  )
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)

  res <- resid[!is.na(resid)]
  hist(
    res,
    col = cyan_tr, border = NA,
    xlim = range(c(res, zlim)),
    xlab = "O.D.", main = "Residuals vs. signal"
  )
  hist(mat, col = pink_tr, border = NA, add = TRUE)
  legend(
    "topright",
    bty = "n",
    c("Signal", "Residuals"),
    pch = 15,
    col = c(pink_tr2, cyan_tr2)
  )
}

plotResid <- function(delay, wavl, mat, C, S,
                      d = rep(1, ncol(C)),
                      main = "Data", ...) {
  # Build model matrix
  matAls <- rep(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:ncol(S))
    matAls <- matAls + C[, i] %o% S[, i] * d[i]
  zlim <- range(mat, na.rm = TRUE)

  xlim <- range(delay, na.rm = TRUE)
  ylim <- range(wavl, na.rm = TRUE)

  dummy <- split.screen(c(2, 3))
  screen(1)
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  image(
    delay, wavl, mat,
    xlim = xlim, ylim = ylim,
    xlab = "Delay", ylab = "Wavelength",
    main = main, col = imgColors, zlim = zlim
  )
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)

  screen(2)
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  image(
    delay, wavl, matAls,
    xlim = xlim, ylim = ylim,
    xlab = "Delay", ylab = "Wavelength",
    main = paste0("Model ", ncol(S), " species"),
    col = imgColors, zlim = zlim
  )
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)

  screen(4)
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  resid <- matAls - mat
  lof <- 100 * (sum(resid^2, na.rm = TRUE) /
    sum(mat^2, na.rm = TRUE)
  )^0.5
  image(
    delay, wavl, resid,
    xlim = xlim, ylim = ylim,
    xlab = "Delay", ylab = "Wavelength",
    main = paste0("Residuals; Lack-of-fit = ", signif(lof, 3), "%"),
    col = imgColors
  )
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)

  screen(5)
  par(
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  res <- resid[!is.na(resid)]
  hist(
    res,
    col = cyan_tr, border = NA,
    xlim = range(c(res, zlim)),
    xlab = "O.D.", main = "Residuals vs. signal"
  )
  hist(mat, col = pink_tr, border = NA, add = TRUE)
  legend(
    "topright",
    bty = "n",
    c("Signal", "Residuals"),
    pch = 15,
    col = c(pink_tr2, cyan_tr2)
  )
  dummy <- close.screen(all.screens = TRUE)
}

# Interactive ####
doSVD <- reactive({
  if (!checkInputsSanity()) {
    return(NULL)
  }

  # Suppress masked areas
  mat <- Inputs$mat
  mat <- mat[!is.na(Inputs$delayMask), ]
  mat <- mat[, !is.na(Inputs$wavlMask) ]

  nsvMax <- min(
    10,
    length(Inputs$delay),
    length(Inputs$wavl)
  )
  svd(mat, nu = nsvMax, nv = nsvMax)
})

observeEvent(
  input$clean, {
    gl <- cleanUp(
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
  points(s$d[1:ncol(s$u)], pch = 19, cex = 1.5, col = lineColors[3])
  text(1:ncol(s$u), s$d[1:ncol(s$u)],
    labels = 1:ncol(s$u),
    col = lineColors[5], pos = 3, offset = 0.5
  )
  box()

  # Lack_of-fit
  lof <- c()
  mat <- Inputs$mat
  # Suppress masked areas
  mat <- mat[!is.na(Inputs$delayMask), ]
  mat <- mat[, !is.na(Inputs$wavlMask) ]
  sMat <- sum(mat^2)
  mat1 <- rep(0, nrow = nrow(s$u), ncol = ncol(s$v))
  for (i in 1:10) {
    mat1 <- mat1 + s$u[, i] %o% s$v[, i] * s$d[i]
    resid <- mat - mat1
    lof[i] <- 100 * (sum(resid^2) / sMat)^0.5
  }

  plot(
    lof,
    type = "n",
    xlab = "Nb. of vectors",
    ylab = " ", log = "y",
    main = "Lack-of-fit (%)"
  )
  grid()
  lines(lof, col = lineColors[6], lwd = 3, lty = 3)
  points(lof, pch = 19, cex = 1.5, col = lineColors[3])
  text(1:length(lof), lof,
    labels = signif(lof, 3),
    col = lineColors[5], pos = 3, offset = 0.5
  )
  box()
}, height = 550)
outputOptions(output, "svdSV",
  suspendWhenHidden = FALSE
)

output$svdVec <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, ncol(s$u))
  plotSVDVecBloc(CS$C, CS$S, Inputs$delay, Inputs$wavl)
}, height = 550)
outputOptions(output, "svdVec",
  suspendWhenHidden = FALSE
)

output$svdResid <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, input$nSV)
  plotDatavsMod(Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    d = s$d
  )
}, height = 550)

output$svdResid1 <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, input$nSV)
  plotResidOnly(Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    d = s$d
  )
}, height = 550)


output$svdContribs <- renderPlot({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }
  CS <- reshapeCS(s$u, s$v, input$nSV)
  colnames(CS$S) <- paste0("Vector ", 1:ncol(CS$S))
  plotConbtribs(Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    d = s$d, type = "svd"
  )
}, height = 550)

output$svdStat <- DT::renderDataTable({
  if (is.null(s <- doSVD())) {
    return(NULL)
  }

  nsvMax <- min(
    10,
    length(Inputs$delay),
    length(Inputs$wavl)
  )

  mat <- Inputs$mat
  # Suppress masked areas
  mat <- mat[!is.na(Inputs$delayMask), ]
  mat <- mat[, !is.na(Inputs$wavlMask) ]
  sMat <- sum(mat^2)
  mat1 <- rep(0, nrow = nrow(s$u), ncol = ncol(s$v))
  sv <- lof <- sig <- c()
  for (i in 1:nsvMax) {
    mat1 <- mat1 + s$u[, i] %o% s$v[, i] * s$d[i]
    resid <- mat - mat1
    sig[i] <- sd(resid)
    lof[i] <- 100 * (sum(resid^2) / sMat)^0.5
    sv[i] <- s$d[i]
  }

  data <- cbind(
    id = 1:nsvMax,
    signif(sv, 3),
    signif(lof, 3),
    signif(sig, 3)
  )
  colnames(data) <- c(
    "Rank",
    "S.V.",
    "Lack-of-fit (%)",
    "Std. dev. resid."
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
