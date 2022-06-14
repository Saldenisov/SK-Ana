# Functions ####
panel.hist <- function(x, ...) {
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
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, method = "spearman")
  ra <- abs(r)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt) * ra
  text(0.5, 0.5, txt, cex = cex.cor, col = ifelse(r >= 0, 4, 2))
}
panel.smooth <- function(x, y, cex = 1.5, col.smooth = "red",
                         span = 2 / 3, iter = 3, ...) {
  maxPoints <- 500
  nP <- min(maxPoints, length(x))
  iSamp <- seq.int(1, length(x), length.out = nP)
  x1 <- x[iSamp]
  y1 <- y[iSamp]
  grid()
  points(x1, y1, pch = 19, col = cyan_tr, lwd = 0, cex = cex)
}
SAPlot <- function(X, cex = 1) {
  sdX <- apply(X, 2, sd)
  par(cex = cex, cex.axis = 1.5 * cex)
  pairs(X[, sdX != 0],
    gap = 0,
    upper.panel = panel.cor,
    diag.panel = panel.hist,
    lower.panel = panel.smooth
  )
}
plotLofVsSvd <- function(s, opt, lmax = 10,...) {
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
}
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

parseScheme <- function(scheme) {
  # Parse Scheme
  
  kinList = try(kinParse(scheme),silent = TRUE)
  if(class(kinList) == 'try-error') {
    gotKin = FALSE
  } else {
    Scheme[["scheme"]] <<- scheme
    Scheme[["nbReac"]] <<- kinList$nbReac
    Scheme[["nbSpecies"]] <<- kinList$nbSpecies
    Scheme[["D"]] <<- kinList$D
    Scheme[["L"]] <<- kinList$L
    Scheme[["kReac"]] <<- kinList$kReac
    Scheme[["kReacF"]] <<- kinList$kReacF
    Scheme[["species"]] <<- kinList$species
    Scheme[["reactants"]] <<- kinList$reactants
    Scheme[["products"]] <<- kinList$products
    Scheme[["tags"]] <<- kinList$tags
    gotKin = TRUE
  }
  
  c0List = try(c0Parse(scheme),silent = TRUE)
  if(class(c0List) == 'try-error') {
    gotC0 = FALSE
  } else {
    Scheme[["c0"]] <<- c0List$c0
    Scheme[["c0F"]] <<- c0List$c0F
    gotC0 = TRUE
  }
  
  epsList = try(epsParse(scheme),silent = TRUE)
  if(class(epsList) == 'try-error') {
    gotEps = FALSE
  } else {
    Scheme[["eps"]] <<- epsList$eps
    Scheme[["epsF"]] <<- epsList$epsF
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
  
  Scheme$gotData <<- gotData
  
  
}
setOptPars  = function() {
  
  # Suppress masked areas
  mat <- Inputs$mat
  mat <- mat[!is.na(Inputs$delayMask), ]
  mat <- mat[, !is.na(Inputs$wavlMask) ]
  times   <- Inputs$delaySave[!is.na(Inputs$delayMask)]
  delay   <- Inputs$delay[!is.na(Inputs$delayMask)]
  wavl    <- Inputs$wavl[!is.na(Inputs$wavlMask)]
  delayId <- Inputs$delayId[!is.na(Inputs$delayMask)]
  
  # Number of experiments
  nExp <- 1
  if (!is.null(input$procMult) &&
      input$procMult == "tileDel") {
    nExp <- length(input$rawData_rows_selected)
  }
  
  deltaStart <- rep(0, nExp)
  startd     <- rep(NA, nExp)
  for (iExp in 1:(nExp - 1))
    startd[iExp] <- which(delayId == (iExp + 1))[1] - 1
  startd[nExp] <- length(delay)
  
  parOpt <- list()
  
  species <- Scheme$species
  nbSpecies <- length(species)
  nbReac <- Scheme$nbReac
  
  # Spectral constraints
  active <- rep(TRUE, nbSpecies)
  names(active) <- species
  eps <- rep(0, nbSpecies)
  names(eps) <- species
  for (sp in species) {
    param <- paste0("eps_", sp)
    eps0 <- input[[param]]
    eps[sp] <- eps0
    
    param <- paste0("epsF_", sp)
    epsF <- input[[param]]
    
    if (eps0 == 0) {
      active[sp] <- FALSE
    } else
      if (epsF > 1) {
        parOpt[[paste0("logeps_", sp)]] <-
          paste0("tnorm(", log(eps0), ",", log(epsF), ")")
      }
  }
  
  # Initial concentrations
  state <- matrix(0, nrow = nbSpecies, ncol = nExp)
  rownames(state) <- species
  for (iExp in 1:nExp)
    for (sp in species) {
      param <- paste0("c0_", sp, "_", iExp)
      c0 <- input[[param]]
      state[sp, iExp] <- c0 # Nominal value
      
      param <- paste0("c0F_", sp, "_", iExp)
      c0F <- input[[param]]
      if (c0F > 1) { # Optimize concentration
        parOpt[[paste0("logc0_", sp, "_", iExp)]] <-
          paste0("tnorm(", log(c0), ",", log(c0F), ")")
      }
    }
  
  # Reaction rates
  kReac <- c()
  for (i in 1:nbReac) {
    param <- paste0("k_", i)
    k <- input[[param]]
    # Nominal value
    kReac[i] <- k
    param <- paste0("kF_", i)
    kF <- input[[param]]
    if (kF > 1) {
      parOpt[[paste0("logk_", i)]] <-
        paste0("tnorm(", log(k), ",", log(kF), ")")
    }
  }
  
  # Gather parameters in list
  kinParms <- list(
    times = times,
    delay = delay,
    wavl = wavl,
    mat = mat,
    nExp = nExp,
    kReac = kReac,
    L = Scheme$L,
    D = Scheme$D,
    state = state,
    active = active,
    startd = startd,
    deltaStart = deltaStart,
    sigma   = input$kinSigma,
    reactants = Scheme$reactants,
    eps     = eps,
    uniS    = FALSE,
    nonnegS = input$nonnegSKinet,
    smooth  = input$kinSmooth,
    logPri  = genPriorPDF(parOpt)
  )
  
  # External spectrum shapes
  if (length(externalSpectra) != 0)
    for(sp in names(externalSpectra)) {
      pname = paste0('fix_',sp)
      if( input[[pname]] )
        kinParms[[sp]] = externalSpectra[[sp]]
    }

  map = NULL
  if (!is.null(RestartKin$map) &
      input$kinRestart) {
      map = RestartKin$map
  } else if (!is.null(RefineKin$map)) {
      map = RefineKin$map
  } 
  startp = startpInit(map, parOpt = parOpt)

  mc <- FALSE
  niter  <- input$kinGlobNit
  global <- input$kinGlobFac * length(startp)
  
  return(
    list(
      parOpt   = parOpt,
      kinParms = kinParms,
      mc       = mc,
      global   = global,
      niter    = niter,
      startp   = startp
    )
  )
}
doKinGlob   = function() {
  bgGlobpx <<- NULL
  
  id = showNotification(
    "Running global optimizer...",
    type = "message",
    duration = 10
  )
  
  with(
    setOptPars(),
    bgGlobpx <<- bmc_glob(
      paropt   = parOpt,
      parms    = kinParms,
      global   = global,
      niter    = niter,
      mc       = mc,
      startp   = startp,
      tol      = 10^input$kinThresh,
      weighted = input$kinWeighted
    )
  )
  
  return()
}
doKinLoc    = function() {
  bgLocpx <<- NULL
  
  id = showNotification(
    "Running local optimizer...",
    type = 'message',
    duration = 10
  )

  with(
    setOptPars(),
    bgLocpx <<- bmc_loc(
      paropt   = parOpt,
      parms    = kinParms,
      startp   = startp,
      tol      = 10^input$kinThresh,
      weighted = input$kinWeighted
    )
  )
  
  return()
  
}
doKinFinish = function (opt0) {
  
  # Global variable for restart
  RestartKin$map <<- opt0$map
  
  updateSlider("keepWlCutKin", range(Inputs$wavl), mean(Inputs$wavl), 500)
  
  # Update Reporting
  if(! 'KIN' %in% includeInReport()) {
    todo = c(includeInReport(),'KIN')
    includeInReport(todo)
    updateCheckboxGroupInput(session,
                             inputId = "inReport",
                             choices = todo,
                             selected = todo
    )
  }
  
  # Finalize optimizer outputs
  with(
    setOptPars(),
    {
      map  = parExpand(opt0$map, parOpt)
      mod  = model(map, kinParms)
      C    = kinet(map, kinParms)
      Ca   = C[, kinParms$active]
      S    = spectra(Ca, map, kinParms)
      vlof = signif( lof(model = mod, 
                         data = kinParms$mat), 
                     3)
      
      return(
        list(
          map      = opt0$map,
          hessian  = opt0$hessian,
          parms    = kinParms,
          paropt   = parOpt,
          mat      = kinParms$mat,
          model    = mod,
          weighted = input$kinWeighted,
          sigma    = input$kinSigma,
          nExp     = kinParms$nExp,
          times    = kinParms$times,
          xC       = kinParms$delay,
          xS       = kinParms$wavl,
          active   = kinParms$active,
          C        = C,
          S        = S,
          lof      = vlof,
          glOut    = opt0$glOut,
          optOut   = opt0$optOut,
          values   = opt0$values,
          cnv      = opt0$cnv
        )
      )
    }
  )
}

# Interactive ####
kinPrint        <- reactiveValues(glOut = NULL, optOut = NULL)
Scheme          <- reactiveValues(gotData = FALSE)
rangesKinSp     <- reactiveValues(x = NULL, y = NULL)
rangesKinKin    <- reactiveValues(x = NULL, y = NULL)
externalSpectra <- list()

observeEvent(input$kinSp_dblclick, {
  brush <- input$kinSp_brush
  if (!is.null(brush)) {
    rangesKinSp$x <- c(brush$xmin, brush$xmax)
    rangesKinSp$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesKinSp$x <- NULL
    rangesKinSp$y <- NULL
  }
})
observeEvent(input$kinKin_dblclick, {
  brush <- input$kinKin_brush
  if (!is.null(brush)) {
    rangesKinKin$x <- c(brush$xmin, brush$xmax)
    rangesKinKin$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesKinKin$x <- NULL
    rangesKinKin$y <- NULL
  }
})
observeEvent(input$schemeFile, {
  if (is.null(inFile <- input$schemeFile)) {
    return(NULL)
  }
  scheme = try(
    scan(
      file = inFile$datapath,
      what = "character",
      sep = "%",
      comment.char = "#",
      blank.lines.skip = TRUE,
      quiet = TRUE
    ),
    silent = TRUE
  )
  if(class(scheme) == 'try-error') {
    id = showNotification(
      paste0('Error while reading file :',inFile$name),
      type = "error",
      duration = NULL
    )
  } else {
    parseScheme(scheme)
    updateTextAreaInput(
      session,
      inputId = "schemeScript",
      value = paste(scheme, collapse = "\n")
    )
  }
})
observeEvent(
  input$update_tS, 
  isolate({
    if (is.null(scheme <- input$schemeScript)) {
      return(NULL)
    } else {
      parseScheme(scan(text = scheme, what = "character", sep = "\n"))
    }
  })
)
observeEvent(input$clear_tS, {
    updateTextAreaInput(session,
                        inputId = "schemeScript",
                        value = ""
    )
    Scheme$gotData <<- FALSE
  })
observeEvent(input$kinSigmaIndex, {
  isolate({
    if (input$kinSigmaIndex == 0) {
      updateNumericInput(session,
                         inputId = "kinSigma",
                         value = 1
      )
    } else {
      if (!is.null(s <- doSVD())) {
        mat <- Inputs$mat
        mat <- mat[!is.na(Inputs$delayMask), ]
        mat <- mat[, !is.na(Inputs$wavlMask)]
        mat1 <- rep(0, nrow = nrow(s$u), ncol = ncol(s$v))
        for (i in 1:input$kinSigmaIndex)
          mat1 <- mat1 + s$u[, i] %o% s$v[, i] * s$d[i]
        resid <- mat - mat1
        val <- signif(sd(resid), 2)
        updateNumericInput(session,
                           inputId = "kinSigma",
                           value = val
        )
      }
    }
  })
})

# Get external spectra ####
output$extSpectra <- renderUI({
  req(input$S0KinFile)

  wavl    <- Inputs$wavl[!is.na(Inputs$wavlMask)]
  ui <- list(
    h4('Fix spectra shapes'),
    hr(style = "border-color: #666;")
  )
  res = getExternalSpectra(
    ui         = ui,
    inputFile  = input$S0KinFile, 
    wavl       = wavl,
    tag        = 'fix_S_')
  
  externalSpectra <<- res$extSpectra
  res$ui
})


# Manage async optimization processes ####

RestartKin = reactiveValues(map=NULL) 
RefineKin  = reactiveValues(map=NULL) 

## Global Opt. Process ####
bgGlobpx = NULL
glOptOut = tempfile(tmpdir = '/tmp',fileext = '_glob.stdout')
file.create(glOptOut, showWarnings = FALSE)
resGlob  = reactiveValues(results = NULL)
bgGlob   = reactiveValues(status = process_status(bgGlobpx))
obsGlobStatus = observe(
  {
    invalidateLater(millis = 500)
    bgGlob$status = process_status(bgGlobpx)
  },
  suspended = TRUE
)

## Local Opt. Process ####
bgLocpx   = NULL
locOptOut = tempfile(tmpdir = '/tmp',fileext = '_loc.stdout')
file.create(locOptOut, showWarnings = FALSE)
resLoc  = reactiveValues(results = NULL)
bgLoc   = reactiveValues(status = process_status(bgLocpx))
obsLocStatus = observe(
  {
    invalidateLater(millis = 500)
    bgLoc$status = process_status(bgLocpx)
  },
  suspended = TRUE
)

## Kill current process ####
observeEvent(
  input$killKin,
  isolate({
    # Is any process running ?
    req(!is.null(bgGlobpx) | !is.null(bgLocpx))

    # Kill the active one...
    if(!is.null(bgGlobpx)) {
      if(!is.null(bgGlob$status$running)) {
        if(bgGlob$status$running) {
          bgGlobpx$kill()
          obsGlobStatus$suspend()
          file.create(glOptOut, showWarnings = FALSE)
          file.create(locOptOut, showWarnings = FALSE)
        }
      }
    } else if(!is.null(bgLocpx)) {
      if(!is.null(bgLoc$status$running)) {
        if(bgLoc$status$running) {
          bgLocpx$kill()
          obsLocStatus$suspend()
          file.create(locOptOut, showWarnings = FALSE)
        }
      }
    }
  })
)

## Manage results ####
obsGlob = observeEvent(
  bgGlob$status$result, 
  {
    # Store optimum for refining by local optim.
    pnames        = parContract(setOptPars()$parOpt)$names
    map           = bgGlob$status$result$par 
    names(map)    = pnames
    RefineKin$map = map
    
    # Do refine
    obsGlobStatus$suspend()
    obsLocStatus$resume()
    doKinLoc()
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)
obsLoc =observeEvent(
  bgLoc$status$result, 
  {
    # Post-reat optimal parameters
    best = bgLoc$status$result
    pnames         = parContract(setOptPars()$parOpt)$names
    map            = best$pars 
    names(map)     = pnames
    
    opt0 = list(
      map     = map,
      hessian = best$hessian,
      values  = best$values,
      cnv     = best$convergence
    )
    resLoc$results = doKinFinish(opt0)
    obsLocStatus$suspend()
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)

# doKin ####
observeEvent(
  input$runKin, {
    isolate({
      if(input$kinGlobNit > 0) {
        obsGlobStatus$resume()
        doKinGlob()
      } else {
        obsLocStatus$resume()
        doKinLoc()
      }
    })
  })

# Params Outputs ####
output$schemeFileSave<- downloadHandler(
  filename = function() {
    paste0(input$schemeFileName, ".in")
  },
  content = function(file) {
    if (!Scheme$gotData) {
      return(NULL)
    }

    sink(file)

    # Reaction scheme
    for (i in 1:Scheme$nbReac)
      cat(
        paste(Scheme$reactants[[i]], collapse = " + "),
        " -> ",
        paste(Scheme$products[[i]], collapse = " + "),
        " ; ", Scheme$kReac[i], " / ", Scheme$kReacF[i],
        "\n"
      )
    cat("\n")

    # Absorbances
    species <- Scheme$species
    for (sp in species) {
      param <- paste0("eps_", sp)
      eps <- input[[param]]
      paramF <- paste0("epsF_", sp)
      epsF <- input[[paramF]]
      if (eps != 0) {
        cat(param, " = ", eps, " / ", epsF, "\n")
      }
    }

    # Initial concentrations
    nExp <- 1
    if (!is.null(input$procMult) &&
      input$procMult == "tileDel") {
      nExp <- length(input$rawData_rows_selected)
    }

    for (iExp in 1:nExp) {
      cat("\n")
      for (sp in species) {
        param <- paste0("c0_", sp, "_", iExp)
        c0 <- input[[param]]
        paramF <- paste0("c0F_", sp, "_", iExp)
        c0F <- input[[paramF]]
        if (c0 != 0) {
          cat(param, " = ", c0, " / ", c0F, "\n")
        }
      }
    }

    sink()
  })
output$scheme        <- DT::renderDataTable({
  if (!Scheme$gotData) {
    return(NULL)
  }

  reacString <- c()
  for (i in 1:Scheme$nbReac)
    reacString[i] <-
      paste(
        paste(Scheme$reactants[[i]], collapse = " + "),
        " -> ",
        paste(Scheme$products[[i]], collapse = " + ")
      )

  DT::datatable(
    data.frame(Tags = Scheme$tags, Reactions = reacString),
    rownames = FALSE,
    class = "cell-border stripe",
    fillContainer = FALSE,
    options = list(
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      dom = "t"
    ),
    escape = FALSE
  )
})
output$rates         <- renderUI({
  if (!Scheme$gotData) {
    return(NULL)
  }

  kReac <- Scheme[["kReac"]]
  kReacF <- Scheme[["kReacF"]]
  tags <- Scheme[["tags"]]

  ui <- list(
    br(),
    fluidRow(
      column(
        3,
        h4("Reaction")
      ),
      column(
        4,
        h4("Rate ct.")
      ),
      column(
        4,
        h4("F uncert.")
      )
    ),
    hr(style = "border-color: #666;")
  )
  for (i in 1:length(kReac)) {
    ui[[i + 3]] <-
      fluidRow(
        column(
          3,
          h5(tags[i])
        ),
        column(
          4,
          numericInput(
            paste0("k_", i),
            label = NULL,
            value = kReac[i]
          )
        ),
        column(
          4,
          numericInput(
            paste0("kF_", i),
            label = NULL,
            min = 1,
            max = 5,
            step = 0.1,
            value = kReacF[i]
          )
        )
      )
  }
  ui
})
output$concentrations<- renderUI({
  if (!Scheme$gotData) {
    return(NULL)
  }

  # Species in scheme
  species <- Scheme$species

  # Species with declared concentration
  c0 <- Scheme[["c0"]] # ; print(c0)
  c0F <- Scheme[["c0F"]] # ; print(c0F)
  sp0 <- rownames(c0)

  # Number of experiments
  nExp <- 1
  if (!is.null(input$procMult) &&
    input$procMult == "tileDel") {
    nExp <- length(input$rawData_rows_selected)
  }

  header <-
    fluidRow(
      column(
        3,
        h5("Species")
      ),
      column(
        4,
        h5("Init. conc.")
      ),
      column(
        4,
        h5("F uncert.")
      )
    )

  for (iExp in 1:nExp) {
    ui <- list()
    for (sp in species) {
      if (sp %in% sp0) {
        c0_sp <- ifelse(is.na(c0[sp, iExp]), 0, c0[sp, iExp])
        c0F_sp <- ifelse(is.na(c0F[sp, iExp]), 1, c0F[sp, iExp])
      } else {
        c0_sp <- 0
        c0F_sp <- 1
      }
      ui[[sp]] <-
        fluidRow(
          column(
            3,
            h5(sp)
          ),
          column(
            4,
            numericInput(
              paste0("c0_", sp, "_", iExp),
              label = NULL,
              value = c0_sp
            )
          ),
          column(
            4,
            numericInput(
              paste0("c0F_", sp, "_", iExp),
              label = NULL,
              min = 1,
              max = 5,
              value = c0F_sp,
              step = 0.1
            )
          )
        )
    }
    id <- paste0("conc_", iExp)
    tp <- tabPanel(h5(iExp), value = id, list(header, ui))
    removeTab("all_conc", target = id)
    appendTab("all_conc", tp, select = TRUE)
  }
})
output$epsilon       <- renderUI({
  if (!Scheme$gotData) {
    return(NULL)
  }
  species <- Scheme$species

  # Species with declared absorption coef
  eps <- Scheme[["eps"]]
  epsF <- Scheme[["epsF"]]
  sp0 <- names(eps)

  ui <- list(
    br(),
    fluidRow(
      column(
        3,
        h5("Species")
      ),
      column(
        4,
        h5("Eps_max.")
      ),
      column(
        4,
        h5("F uncert.")
      )
    ),
    hr(style = "border-color: #666;")
  )
  for (sp in species) {
    if (sp %in% sp0) {
      eps_sp <- ifelse(is.na(eps[sp]), 0, eps[sp])
      epsF_sp <- ifelse(is.na(epsF[sp]), 1, epsF[sp])
    } else {
      eps_sp <- 0
      epsF_sp <- 1
    }
    ui[[sp]] <-
      fluidRow(
        column(
          2,
          h5(sp)
        ),
        column(
          4,
          numericInput(
            paste0("eps_", sp),
            label = NULL,
            value = eps_sp
          )
        ),
        column(
          4,
          numericInput(
            paste0("epsF_", sp),
            label = NULL,
            min = 1,
            max = 5,
            value = epsF_sp,
            step = 0.1
          )
        )
      )
  }

  ui
})
# Best Params ####
output$kinRes <- renderUI({
  req(resLoc$results)
  
  out <- list()
  
  opt    = isolate(resLoc$results)
  paropt = opt$paropt
  
  if (opt$cnv == 0) {
    out <- list(out, h4("Optimization done!"))
  } else {
    out <- list(
      out,
      h4("WARNING: Optimization ended badly (see Trace)!")
    )
  }
  
  out <- list(
    out,
    strong("Lack-of-fit (%) :"),
    signif(opt$lof, 3),
    br(), p()
  )
  
  if (opt$weighted) {
    ndf <- length(opt$mat) - length(opt$map) - length(opt$S)
    chi2 <- sum(((opt$mat - opt$mod) / opt$sigma)^2)
    ndig <- 3
    out <- list(
      out,
      strong("*** Chi2 Analysis ***"), br(),
      "chi2_obs = ", format(chi2, digits = ndig), br(),
      "ndf      = ", ndf, br(),
      "chi2_red =", format(chi2 / ndf, digits = ndig + 1), br(),
      "P(chi2>chi2_obs)=",
      format(pchisq(chi2, df = ndf, lower.tail = FALSE), digits = ndig), br(),
      "Q05=", format(qchisq(0.05, df = ndf), digits = ndig), ", ",
      "Q95=", format(qchisq(0.95, df = ndf), digits = ndig)
    )
  }
  
  return(out)
})
output$kinOpt <- DT::renderDataTable({
  req(resLoc$results)
  
  opt    = isolate(resLoc$results)
  paropt <- opt$paropt
  
  map <- parExpand(opt$map, paropt)
  names(map) <- names(paropt)

  Sigma <- try(solve(opt$hessian), silent = TRUE)
  if (class(Sigma) != "try-error" && opt$cnv == 0) {
    EV <- Re(eigen(Sigma)$values)
    if (sum(EV < 0) > 0) 
      print("Non-positive definite Covariance matrix")
    Sd <- diag(Sigma)^0.5
    names(Sd) <- names(paropt)
  } else {
    Sd <- rep(NA, length(paropt))
    names(Sd) <- names(paropt)
  }
  lSd <- unlist(sdExpand(Sd, paropt))
  names(lSd) <- names(paropt)

  eps <- 1e-3
  parsc <- parContract(paropt)
  LB <- parsc$LB
  names(LB) <- parsc$names
  UB <- parsc$UB
  names(UB) <- parsc$names
  nPar <- length(names(map))
  alert <- rep("", nPar)
  names(alert) <- names(map)
  tags <- rep("", nPar)
  names(tags) <- names(map)
  val <- rep(NA, nPar)
  names(val) <- names(map)
  valF <- rep(NA, nPar)
  names(valF) <- names(map)

  for (item in names(map)) {
    # Detect params close to priors limits
    if (abs(opt$map[item] - LB[item]) < eps) {
      alert[item] <- " *** at min of prior"
    } else if (abs(opt$map[item] - UB[item]) < eps) {
      alert[item] <- " *** at max of prior"
    }
    
    if (grepl("log", item)) {
      tags[item] <- sub("log", "", item)
      val[item] <- signif(exp(map[[item]]), digits = 2)
      valF[item] <- ifelse(
        !is.finite(lSd[item]),
        "",
        # paste("/*", signif(exp(lSd[item]), digits = 3))
        paste(signif(100*lSd[item], digits = 3),' %')
      )
    } else {
      tags[item] <- item
      val[item] <- signif(map[item], digits = 2)
      valF[item] <- ifelse(
        !is.finite(lSd[item]),
        "",
        # paste("+/-", signif(lSd[item], digits = 3))
        signif(lSd[item], digits = 3)
      )
    }
  }

  DT::datatable(
    data.frame(
      "Parameter"  = tags,
      "Best Value" = val,
      "Std. Unc."  = valF,
      "Comment"    = alert
    ),
    class = "cell-border stripe",
    rownames = FALSE,
    options = list(
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      dom = "t"
    ),
    escape = FALSE,
    width = 200
  )
})
# Trace ####
stdGlobOut = reactiveFileReader(
  intervalMillis = 500,
  session  = session,
  filePath = glOptOut,
  readFunc = readLines,
  warn     = FALSE
)
output$kinGlPrint <- renderPrint({
  cat("### GLOBAL OPTIMIZATION ###\n")
  cat(stdGlobOut(), sep = '\n')
})
stdLocOut = reactiveFileReader(
  intervalMillis = 500,
  session  = session,
  filePath = locOptOut,
  readFunc = readLines,
  warn     = FALSE
)
output$kinOptPrint <- renderPrint({
  cat("### LOCAL OPTIMIZATION ###\n")
  cat(stdLocOut(), sep = '\n')
})

#PriPost ####
output$kinPriPost <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)
  plotPriPost(opt)
}, height = plotHeight)
output$kinParamsSamp <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)
  Sigma <- try(solve(opt$hessian), silent = TRUE)
  if (class(Sigma) == "try-error" || opt$cnv != 0) {
    return(NULL)
  }

  sample <- mvtnorm::rmvnorm(500, opt$map, Sigma)
  sample <- t(apply(
    sample, 1,
    function(x) unlist(parExpand(x, opt$paropt))
  ))
  colnames(sample) <- names(opt$map)

  SAPlot(sample, cex = cex)
}, height = plotHeight)
# Diagnostics ####
output$kinResid      <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)

  CS <- reshapeCS(opt$C, opt$S)

  if (isolate(input$useFiltered)) { # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
      nrow = length(Inputs$delay),
      ncol = length(Inputs$wavl)
    )
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]

    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotResid(Inputs$delay, Inputs$wavl, mat,
    CS$C[,opt$active], CS$S,
    main = main,
    delayTrans = Inputs$delayTrans
  )
}, height = plotHeight)
output$kinResidAna   <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)

  CS <- reshapeCS(opt$C, opt$S)

  if (isolate(input$useFiltered)) { # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
      nrow = length(Inputs$delay),
      ncol = length(Inputs$wavl)
    )
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]

    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotResidAna(Inputs$delay, Inputs$wavl, mat,
    CS$C[,opt$active], CS$S,
    main = main,
    delayTrans = Inputs$delayTrans
  )
}, height = plotHeight)
output$kinIntKin     <- renderPlot({
  # Integrated kinetics
  req(resLoc$results)
  opt    = isolate(resLoc$results)
  plotIntKin(opt, delayTrans=Inputs$delayTrans)
}, height = plotHeight)
### Transects ####
wlCutKin   = reactive({input$keepWlCutKin}) %>% debounce(debounceDelay/2)
output$transectsKin <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)
  
  times <- opt$times
  mat   <- opt$mat
  mod   <- opt$mod
  nExp  <- opt$nExp
  wavl  <- Inputs$wavl
  trans <- Inputs$delayTrans

  dCut <- wlCutKin()
  iCut <- indxCuts(dCut, wavl)
  indx <- iCut$indx
  delta <- iCut$delta
  
  # ncol <- max(2, min(5, nExp))
  # nrow <- floor(nExp / ncol)
  # if (nrow * ncol != nExp) nrow <- nrow + 1
  
  par(
    mfrow = c(1, 1),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = "s"
  )
  
  startd <- opt$parms[["startd"]]
  i0 <- 0
  for (iExp in 1:nExp) {
    sel <- (i0 + 1):startd[iExp]
    tinteg <- opt$xC[sel]
    i0 <- startd[iExp]
    if (length(indx) == 1) {
      cutMean = mat[sel, indx]
      cutMod  = mod[sel, indx]
    } else {
      cutMean = rowMeans(mat[sel, indx])
      cutMod  = rowMeans(mod[sel, indx])
    }
    if (all(is.na(cutMean))) cutMean = cutMean * 0
    if (all(is.na(cutMod)))  cutMod  = cutMod  * 0
    plot(
      tinteg, cutMean,
      xlab = paste0("Delay ",trans),
      ylab = "O.D.",
      pch = 16,
      col = lineColors[3],
      main = paste0(
        "Mean O.D. at wavl: ", signif(mean(wavl[indx]), 3),
        ifelse(delta == 0,
               "",
               paste0(" +/- ", signif(delta / 2, 2))
        )
      )
    )
    grid()
    lines(
      tinteg,
      cutMod,
      col = lineColors[6],
      lwd = 2
    )
    box()
  }
},
height = plotHeight, width = plotHeight
)
observeEvent(
  input$transKinSave,
  isolate({
    req(resLoc$results)
    opt    = isolate(resLoc$results)
    
    times <- opt$times
    mat   <- opt$mat
    mod   <- opt$mod
    nExp  <- opt$nExp
    wavl  <- Inputs$wavl
    trans <- Inputs$delayTrans
    
    dCut <- wlCutKin()
    iCut <- indxCuts(dCut, wavl)
    indx <- iCut$indx
    delta <- iCut$delta
    
    startd <- opt$parms[["startd"]]
    i0 <- 0
    for (iExp in 1:nExp) {
      sel <- (i0 + 1):startd[iExp]
      tinteg <- opt$xC[sel]
      i0 <- startd[iExp]
      if (length(indx) == 1) {
        cutMean = mat[sel, indx]
        cutMod  = mod[sel, indx]
      } else {
        cutMean = rowMeans(mat[sel, indx])
        cutMod  = rowMeans(mod[sel, indx])
      }
      if (all(is.na(cutMean))) cutMean = cutMean * 0
      if (all(is.na(cutMod)))  cutMod  = cutMod  * 0
      
      dat = cbind(tinteg, cutMean, cutMod)
      colnames(dat) <- c("delay", "Data", "Model")
      
      wv = signif(mean(wavl[indx]), 3)
      if(nExp == 1)
        fileName = paste0("_transKin_wvl=",wv,".csv")
      else
        fileName = paste0("_transKin_wvl=",wv,"_exp=",iExp,".csv")
      
      write.csv(
        dat,
        file = file.path(
          "outputDir",
          paste0(
            input$projectTag,
            fileName
          )
        ),
        row.names = FALSE
      )
    }
  })
)
## Lof ####
output$kinLofVsSvd   <- renderPlot({
  # L.o.F comparison with SVD
  req(resLoc$results)
  opt = isolate(resLoc$results)
  s   = doSVD()
  plotLofVsSvd(s, opt)
}, height = plotHeight)
## Data vs model ####
output$kinDatavsMod  <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)

  CS <- reshapeCS(opt$C, opt$S)

  if (isolate(input$useFiltered)) { # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
      nrow = length(Inputs$delay),
      ncol = length(Inputs$wavl)
    )
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]

    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotDatavsMod(Inputs$delay,
    Inputs$wavl,
    mat,
    CS$C[,opt$active], CS$S,
    main = main,
    cont = input$kinContours,
    delayTrans = Inputs$delayTrans
  )
}, height = plotHeight)
observeEvent(
  input$modelKinSave,
  isolate({
    req(resLoc$results)
    opt    = isolate(resLoc$results)
    
    mod   <- opt$mod
    nExp  <- opt$nExp
    wavl  <- Inputs$wavl

    startd <- opt$parms[["startd"]]
    i0 <- 0
    for (iExp in 1:nExp) {
      sel <- (i0 + 1):startd[iExp]
      tinteg <- opt$xC[sel]
      i0 <- startd[iExp]
      
      dat = rbind(
        c(0,wavl),
        cbind(tinteg,mod[sel,])
      )
      if(input$datStr != 'dxw') 
        dat = t(dat)
      
      if(nExp == 1)
        fileName = paste0("_model.csv")
      else
        fileName = paste0("_model_exp=",iExp,".csv")
      
      write.table(
        dat,
        file = file.path(
          "outputDir",
          paste0(
            input$projectTag,
            fileName
          )
        ),
        row.names = FALSE, 
        col.names = FALSE,
        sep = ','
      )
    }
  })
)

# Spectra & Kinetics ####
output$kinSpVectors  <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)
  plotAlsVec(opt,
    type = "Sp",
    xlim = rangesKinSp$x,
    ylim = rangesKinSp$y,
    plotUQ = input$plotCSUQ,
    nonnegS = input$nonnegSKinet,
    cols = 1,
    delayTrans = Inputs$delayTrans
  )
}, height = plotHeight - 50)
output$kinKinVectors <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)
  plotAlsVec(opt,
    type = "Kin",
    xlim = rangesKinKin$x,
    ylim = rangesKinKin$y,
    plotUQ = input$plotCSUQ,
    cols = 1,
    activeOnly = input$activeOnly,
    delayTrans = Inputs$delayTrans
  )
}, height = plotHeight - 50)

observeEvent(
  input$kinSpKinSave,
  isolate({
    req(resLoc$results)
    opt    = isolate(resLoc$results)
    
    CS <- reshapeCS(opt$C, opt$S)
    
    S <- cbind(Inputs$wavl, CS$S)
    colnames(S) <- c("wavl", colnames(opt$S))
    write.csv(
      S,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_kinetSpectra.csv"
        )
      ),
      row.names = FALSE
    )
    C <- cbind(Inputs$delaySave, CS$C)
    colnames(C) <- c("delay", colnames(opt$C))
    write.csv(
      C,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_kinetKinets.csv"
        )
      ),
      row.names = FALSE
    )
  })
)

# Contribs ####
output$kinContribs   <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)
  CS <- reshapeCS(opt$C, opt$S)
  plotConbtribs(
    Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C[,opt$active], CS$S,
    delayTrans = Inputs$delayTrans
  )
}, height = plotHeight)

outputOptions(output, "scheme", suspendWhenHidden = FALSE)
outputOptions(output, "rates", suspendWhenHidden = FALSE)
outputOptions(output, "concentrations", suspendWhenHidden = FALSE)
outputOptions(output, "epsilon", suspendWhenHidden = FALSE)
outputOptions(output, "kinGlPrint", suspendWhenHidden = FALSE)
outputOptions(output, "kinOptPrint", suspendWhenHidden = FALSE)

