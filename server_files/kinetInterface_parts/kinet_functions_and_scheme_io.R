# Functions ####
panel.hist <- safely(function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  grid(ny = 0)
  rect(breaks[-nB], 0, breaks[-1], y, col = lineColors[3], ...)
}, return_on_error = NULL)

panel.cor <- safely(function(x, y, digits = 2, prefix = "", cex.cor) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, method = "spearman")
  ra <- abs(r)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt) * ra
  text(0.5, 0.5, txt, cex = cex.cor, col = ifelse(r >= 0, 4, 2))
}, return_on_error = NULL)

panel.smooth <- safely(function(x, y, cex = 1.5, col.smooth = "red",
                         span = 2 / 3, iter = 3, ...) {
  maxPoints <- 500
  nP <- min(maxPoints, length(x))
  iSamp <- seq.int(1, length(x), length.out = nP)
  x1 <- x[iSamp]
  y1 <- y[iSamp]
  grid()
  points(x1, y1, pch = 19, col = cyan_tr, lwd = 0, cex = cex)
}, return_on_error = NULL)

SAPlot <- safely(function(X, cex = 1) {
  sdX <- apply(X, 2, sd)
  par(cex = cex, cex.axis = 1.5 * cex)
  pairs(X[, sdX != 0],
    gap = 0,
    upper.panel = panel.cor,
    diag.panel = panel.hist,
    lower.panel = panel.smooth
  )
}, return_on_error = NULL)

plotLofVsSvd <- safely(function(s, opt, lmax = 10,...) {
  par(
    mfrow = c(1, 2),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = "s"
  )

  loft <- c()
  mat1 <- rep(0, nrow = nrow(s$u), ncol = ncol(s$v))
  for (i in 1:lmax) {
    mat1 <- mat1 + s$u[, i] %o% s$v[, i] * s$d[i]
    loft[i] <- lof(model = mat1, data = opt$mat)
  }
  lof0 <- opt$lof
  lmin <- max(1, which(loft < lof0)[1] - 1)
  loft <- loft[lmin:lmax]

  plot(
    lmin:lmax, loft,
    type = "n",
    xlab = "Nb. species",
    ylab = "Lack-of-fit (%)", log = "y",
    ylim = c(min(loft), 1.02 * max(c(loft, opt$lof))),
    main = "Lack-of-fit analysis"
  )
  grid()
  lines(
    lmin:lmax, loft,
    col = lineColors[6],
    lwd = 3,
    lty = 3
  )
  points(
    lmin:lmax, loft,
    pch = 16,
    cex = 1.5,
    col = lineColors[3]
  )
  abline(
    h = lof0,
    col = lineColors[5],
    lwd = 1.5
  )
  text(
    lmax - 1.5, lof0,
    labels = paste0(
      "Achieved : ",
      signif(lof0, 3),
      "%"
    ),
    col = lineColors[5],
    pos = 3
  )
  nsp <- ncol(opt$S)
  abline(
    h = loft[nsp - lmin + 1],
    col = lineColors[2],
    lty = 2,
    lwd = 1.5
  )
  text(
    lmax - 1.5, loft[nsp - lmin + 1],
    labels = paste0(
      "Target  : ",
      signif(loft[nsp - lmin + 1], 3),
      "%"
    ),
    col = lineColors[2],
    pos = 1
  )
  box()
}, return_on_error = NULL)

plotIntKin <- function(opt, delayTrans = '',...) {
  # Plot wavelength-integrated kinetic traces

  times <- opt$times
  mat <- opt$mat
  mod <- opt$mod
  nExp <- opt$nExp

  # ncol = ceiling(sqrt(nExp))
  ncol <- max(2, min(5, nExp))
  nrow <- floor(nExp / ncol)
  if (nrow * ncol != nExp) nrow <- nrow + 1

  par(
    mfrow = c(nrow, ncol),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = "s"
  )

  startd <- opt$parms[["startd"]]
  i0 <- 0
  for (iExp in 1:nExp) {
    sel <- (i0 + 1):startd[iExp]
    tinteg <- opt$xC[sel]
    i0 <- startd[iExp]
    dd <- rowSums(mat[sel, ], na.rm = TRUE)
    plot(
      tinteg, dd,
      xlab = paste0("Delay ",delayTrans),
      ylab = "Integrated O.D.",
      pch = 16,
      col = lineColors[3],
      main = "Integrated kinetic traces"
    )
    grid()
    lines(
      tinteg,
      rowSums(mod[sel, ]),
      col = lineColors[6],
      lwd = 2
    )
    box()
  }
}
plotPriPost <- function(opt, cex = 1, ...) {
  # PLot marginal prior and posterior distributions
  # with Laplace approximation

  map <- parExpand(opt$map, opt$paropt)

  # ncol = ceiling(sqrt(length(map)))
  ncol <- min(
    ceiling(sqrt(length(map))),
    min(5, length(map))
  )
  nrow <- floor(length(map) / ncol)
  if (nrow * ncol != length(map)) nrow <- nrow + 1

  par(
    mfrow = c(nrow, ncol),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = "s"
  )

  Sd <- rep(NA, length(map))
  Sigma <- try(solve(opt$hessian), silent = TRUE)
  if (class(Sigma) != "try-error" && opt$cnv == 0) {
    Sd <- diag(Sigma)^0.5
  }
  lSd <- unlist(sdExpand(Sd, opt$paropt))
  names(Sd) <- names(lSd) <- names(map)

  for (p in names(opt$paropt)) {
    # Marginal prior density
    C <- priorDensity(p, opt$paropt)
    pname = p
    if (!input$logPriPost) {
      C[,1] = 10^C[,1]
      pname = substr(p,4,nchar(p))      
    }
    plot(
      C,
      ylim = c(0, 1.1),
      type = "l",
      col = lineColors[6],
      xlab = pname, 
      ylab = "PDF", 
      yaxs = "i"
    )
    grid()
    x.poly <- c(C[, 1], C[nrow(C), 1], C[1, 1])
    y.poly <- c(C[, 2], 0, 0)
    polygon(x.poly, y.poly, col = cyan_tr, border = NA)

    # Marginal posterior
    m <- map[[p]]
    s <- lSd[p]
    if (is.finite(s)) {
      x = seq(from = m - 6 * s, to = m + 6 * s, length.out = 500)
      y = exp(-0.5 * (x - m)^2 / s^2)
      if (!input$logPriPost)
        x = 10^x
      lines(x,y, col = lineColors[3])
      x.poly <- c(x, x[length(x)], x[1])
      y.poly <- c(y, 0, 0)
      # C <- curve(
      #   exp(-0.5 * (x - m)^2 / s^2),
      #   from = m - 6 * s, to = m + 6 * s, n = 500,
      #   add = TRUE, col = lineColors[3]
      # )
      # x.poly <- c(C$x, C$x[length(C$x)], C$x[1])
      # y.poly <- c(C$y, 0, 0)
    } else {
      abline(v = m, col = lineColors[2])
      x.poly <- c(C[1, 1], C[nrow(C), 1], C[nrow(C), 1], C[1, 1])
      y.poly <- c(m, m, 0, 0)
    }
    polygon(x.poly, y.poly, col = pink_tr, border = NA)
    box()
  }
}

setSchemeField <- function(name, value) {
  Scheme[[name]] <- value
  invisible(value)
}

setSchemeFields <- function(values) {
  for (name in names(values)) {
    Scheme[[name]] <- values[[name]]
  }
  invisible(values)
}

parseScheme <- function(scheme) {
  # Parse Scheme
  
  kinList = try(kinParse(scheme),silent = TRUE)
  if(class(kinList) == 'try-error') {
    gotKin = FALSE
  } else {
    setSchemeFields(list(
      scheme = scheme,
      nbReac = kinList$nbReac,
      nbSpecies = kinList$nbSpecies,
      D = kinList$D,
      L = kinList$L,
      kReac = kinList$kReac,
      kReacF = kinList$kReacF,
      species = kinList$species,
      reactants = kinList$reactants,
      products = kinList$products,
      tags = kinList$tags
    ))
    gotKin = TRUE
  }
  
  c0List = try(c0Parse(scheme),silent = TRUE)
  if(class(c0List) == 'try-error') {
    gotC0 = FALSE
  } else {
    setSchemeFields(list(
      c0 = c0List$c0,
      c0F = c0List$c0F
    ))
    gotC0 = TRUE
  }
  
  epsList = try(epsParse(scheme),silent = TRUE)
  if(class(epsList) == 'try-error') {
    gotEps = FALSE
  } else {
    setSchemeFields(list(
      eps = epsList$eps,
      epsF = epsList$epsF
    ))
    gotEps = TRUE
  }
  
  gotData = gotKin & gotC0 & gotEps
  
  if(!gotData) {
    msg = c()
    if(!gotKin) msg = c(msg,'reac.') 
    if(!gotC0)  msg = c(msg,'conc.') 
    if(!gotEps) msg = c(msg,'eps.') 
    id = showNotification(
      paste0(
        'Problem with scheme file: ',
        paste(msg,collapse = ', ')
      ),
      type = 'error',
      duration = NULL
    )
  }
  
  setSchemeField("gotData", gotData)
  
  
}
