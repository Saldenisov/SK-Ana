
## Ambiguity ####
### UI ####
output$selAmbParams <- renderUI({
  req(alsOut <- resALS$results)
  
  # Init ambRot vector selection
  isolate({
    nS0 <- als_fixed_spectra_count()
    
    if(nS0 > ncol(alsOut$S)-2) {
      id = showNotification(
        "No ambiguity: too many spectra fixed",
        type = "warning",
        duration = 10
      )
      req(NULL)
    }
  })

  lv <- list()
  for (i in (nS0+1):input$nALS)
    lv[[i-nS0]] <- i
  
  if(length(lv) > 2)
    label = "Select 2 (or 3) vectors"
  else
    label = "Select 2 vectors"
  
  list(
    fluidRow(
      column(
        4,
        checkboxGroupInput("vecsToRotate",
                           label = label,
                           choices = lv,
                           selected = c(lv[[1]],lv[[2]])
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
bgAmbpx = reactiveVal(NULL)
resAmb  = reactiveValues(results = NULL)
bgAmb   = reactiveValues(status = process_status(NULL))
obsAmbStatus = observe(
  {
    invalidateLater(millis = 500)
    bgAmb$status = process_status(bgAmbpx())
  },
  suspended = TRUE
)

### Kill current process ####
observeEvent(
  input$killALSAmb,
  isolate({
    if(!is.null(bgAmbpx())) {
      if(!is.null(bgAmb$status$running)) {
        if(bgAmb$status$running) {
          bgAmbpx()$kill()
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
    log_info(paste("Ambiguity explorer completed:", length(sol$solutions), "solutions found"))
    if (length(sol$solutions) == 0) {
      if (sol$finished) {
        log_warning("No solutions found - finished")
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
        log_warning("No solutions found - algorithm incomplete")
        showModal(modalDialog(
          title = ">>>> No Solution Found <<<< ",
          paste0("Try to let the algorithm run for a longer time!"),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "s"
        ))
      }
    } else {
      log_info(paste("Solutions available for display"))
    }
    resAmb$results = sol
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)

### Run ####
doAmbRot <- observeEvent(
  input$runALSAmb, {
    log_info("===== AMBIGUITY EXPLORER START =====")
    tryCatch({
    req(alsOut <- resALS$results)
    log_info("ALS results available")

    isolate({
      nS0 <- als_fixed_spectra_count()
      
      if(nS0 > ncol(alsOut$S)-2) {
        id = showNotification(
          "No ambiguity: too many spectra fixed",
          type = "warning",
          duration = 10
        )
        req(NULL)
      }
    })
    
    isolate({
      rotVec <- as.numeric(unlist(input$vecsToRotate))
      eps <- input$alsRotAmbEps
      dens <- input$alsRotAmbDens
      log_debug(paste("Selected vectors:", paste(rotVec, collapse=",")))
      log_debug(paste("Eps:", eps, "Dens:", dens))
    })
    
    if (length(rotVec) > 3) {
      log_error("Too many vectors selected (max 3)")
      showModal(modalDialog(
        title = ">>>> Too Many Vectors Selected <<<< ",
        paste0("Please choose 3 vectors max."),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "s"
      ))
      return(NULL)
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
    log_debug(paste("Using function:", paste0('rotAmb',length(rotVec))))

    launch <- launch_background_job(
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
      job_name = "Ambiguity explorer"
    )
    rx <- launch$job
    if (is.null(rx)) {
      log_error(launch$error)
      showNotification(launch$error, type = "error", duration = 10)
      return(NULL)
    }
    resAmb$results = NULL
    obsAmbStatus$resume()
    bgAmbpx(rx)
    
    id = showNotification(
      "Running ambiguity explorer...",
      type = "message",
      duration = 5
    )
    log_info("Ambiguity explorer process started")
    }, error = function(e) {
      log_error(paste("Ambiguity explorer error:", e$message))
      showNotification(
        paste("Error in ambiguity explorer:", e$message),
        type = "error",
        duration = 10
      )
    })
  }
)
rangesAmbSp <- reactiveValues(x = NULL, y = NULL)

# Conditional UI: show status or plot for Spectra
output$ambSpPlotOrStatus <- renderUI({
  # Check if calculation is running
  if (!is.null(bgAmb$status$running) && bgAmb$status$running) {
    # Show running status
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border: 2px dashed #007bff; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("cog", class = "fa-spin fa-3x", style = "color: #007bff; margin-bottom: 20px;"),
      h4("Ambiguity Calculation Running...", style = "color: #007bff; margin-bottom: 10px;"),
      p("Exploring rotational/scaling ambiguity", style = "color: #6c757d;")
    )
  } else if (!is.null(resAmb$results) && length(resAmb$results$solutions) > 0) {
    # Show plot when results available
    plotOutput(
      "ambSpVectors",
      height = "450px",
      dblclick = "ambSp_dblclick",
      brush = brushOpts(
        id = "ambSp_brush",
        resetOnNew = TRUE
      )
    )
  } else {
    # Show instructions
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("chart-line", class = "fa-3x", style = "color: #6c757d; margin-bottom: 20px;"),
      h4("Ambiguity Spectra", style = "color: #6c757d; margin-bottom: 10px;"),
      p("Click 'Start' to explore ambiguity", style = "color: #adb5bd;")
    )
  }
})

output$ambSpVectors <- renderPlot({
  req(resAmb$results)
  req(length(resAmb$results$solutions) > 0)
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

# Conditional UI: show status or plot for Kinetics
output$ambKinPlotOrStatus <- renderUI({
  # Check if calculation is running
  if (!is.null(bgAmb$status$running) && bgAmb$status$running) {
    # Show running status
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border: 2px dashed #007bff; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("cog", class = "fa-spin fa-3x", style = "color: #007bff; margin-bottom: 20px;"),
      h4("Ambiguity Calculation Running...", style = "color: #007bff; margin-bottom: 10px;"),
      p("Exploring rotational/scaling ambiguity", style = "color: #6c757d;")
    )
  } else if (!is.null(resAmb$results) && length(resAmb$results$solutions) > 0) {
    # Show plot when results available
    plotOutput(
      "ambKinVectors",
      height = "450px",
      dblclick = "ambKin_dblclick",
      brush = brushOpts(
        id = "ambKin_brush",
        resetOnNew = TRUE
      )
    )
  } else {
    # Show instructions
    div(
      style = "text-align: center; padding: 100px 20px; background-color: #f8f9fa; border-radius: 8px; height: 450px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      icon("chart-line", class = "fa-3x", style = "color: #6c757d; margin-bottom: 20px;"),
      h4("Ambiguity Kinetics", style = "color: #6c757d; margin-bottom: 10px;"),
      p("Click 'Start' to explore ambiguity", style = "color: #adb5bd;")
    )
  }
})

output$ambKinVectors <- renderPlot({
  req(resAmb$results)
  req(length(resAmb$results$solutions) > 0)
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

### Save ####
observeEvent(
  input$rotAmbVecSave,
  isolate({
    req(alsOut <- resALS$results)
    req(ambOut <- resAmb$results)
    
    solutions = ambOut$solutions
    
    CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
    
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
    S <- cbind(Inputs$wavl, Smin, Smax)
    colnames(S) <- c(
      'wavl', 
      paste0(colnames(S1),'_min'),
      paste0(colnames(S1),'_max')
    )
    write.csv(
      S,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_ambSpectra_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    
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
    
    C <- cbind(Inputs$delaySave, Cmin, Cmax)
    colnames(C) <- c(
      'delay', 
      paste0(colnames(C1),'_min'),
      paste0(colnames(C1),'_max')
    )
    write.csv(
      C,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_ambKinets_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    showNotification(
      "Ambiguity results saved to outputDir folder",
      type = "message",
      duration = 3
    )
  })
)
