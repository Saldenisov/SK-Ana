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
  extSpectra <- externalSpectra()
  if (length(extSpectra) != 0)
    for(sp in names(extSpectra)) {
      pname = paste0('fix_',sp)
      if( input[[pname]] )
        kinParms[[sp]] = extSpectra[[sp]]
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
  bgGlobpx(NULL)
  
  id = showNotification(
    "Running global optimizer...",
    type = "message",
    duration = 10
  )
  
  with(
    setOptPars(),
    {
      launch <- bmc_glob(
        paropt   = parOpt,
        parms    = kinParms,
        global   = global,
        niter    = niter,
        mc       = mc,
        startp   = startp,
        tol      = 10^input$kinThresh,
        weighted = input$kinWeighted
      )
      bgGlobpx(launch$job)
      if (is.null(bgGlobpx())) {
        showNotification(launch$error, type = "error", duration = 10)
      }
    }
  )
  
  return()
}
doKinLoc    = function() {
  bgLocpx(NULL)
  
  id = showNotification(
    "Running local optimizer...",
    type = 'message',
    duration = 10
  )

  with(
    setOptPars(),
    {
      launch <- bmc_loc(
        paropt   = parOpt,
        parms    = kinParms,
        startp   = startp,
        tol      = 10^input$kinThresh,
        weighted = input$kinWeighted
      )
      bgLocpx(launch$job)
      if (is.null(bgLocpx())) {
        showNotification(launch$error, type = "error", duration = 10)
      }
    }
  )
  
  return()
  
}
doKinFinish = function (opt0) {
  
  # Global variable for restart
  RestartKin$map <- opt0$map
  
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
externalSpectra <- reactiveVal(list())

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
    Scheme$gotData <- FALSE
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
  
  externalSpectra(res$extSpectra)
  res$ui
})


# Manage async optimization processes ####

RestartKin = reactiveValues(map=NULL) 
RefineKin  = reactiveValues(map=NULL) 

## Global Opt. Process ####
bgGlobpx = reactiveVal(NULL)
glOptOut = tempfile(tmpdir = '/tmp',fileext = '_glob.stdout')
file.create(glOptOut, showWarnings = FALSE)
resGlob  = reactiveValues(results = NULL)
bgGlob   = reactiveValues(status = process_status(NULL))
obsGlobStatus = observe(
  {
    invalidateLater(millis = 500)
    bgGlob$status = process_status(bgGlobpx())
  },
  suspended = TRUE
)

## Local Opt. Process ####
bgLocpx   = reactiveVal(NULL)
locOptOut = tempfile(tmpdir = '/tmp',fileext = '_loc.stdout')
file.create(locOptOut, showWarnings = FALSE)
resLoc  = reactiveValues(results = NULL)
bgLoc   = reactiveValues(status = process_status(NULL))
obsLocStatus = observe(
  {
    invalidateLater(millis = 500)
    bgLoc$status = process_status(bgLocpx())
  },
  suspended = TRUE
)

## Kill current process ####
observeEvent(
  input$killKin,
  isolate({
    # Is any process running ?
    req(!is.null(bgGlobpx()) | !is.null(bgLocpx()))

    # Kill the active one...
    if(!is.null(bgGlobpx())) {
      if(!is.null(bgGlob$status$running)) {
        if(bgGlob$status$running) {
          bgGlobpx()$kill()
          obsGlobStatus$suspend()
          file.create(glOptOut, showWarnings = FALSE)
          file.create(locOptOut, showWarnings = FALSE)
        }
      }
    } else if(!is.null(bgLocpx())) {
      if(!is.null(bgLoc$status$running)) {
        if(bgLoc$status$running) {
          bgLocpx()$kill()
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
