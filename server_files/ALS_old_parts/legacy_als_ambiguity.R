
## Ambiguity ####
### UI ####
output$selAmbParams <- renderUI({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
  lv <- list()
  for (i in 1:input$nALS)
    lv[[i]] <- i
  
  list(
    fluidRow(
      column(
        4,
        checkboxGroupInput("vecsToRotate",
                           label = "Pick 2 (or 3) vectors",
                           choices = lv,
                           selected = c(1, 2)
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
bgAmbpx = NULL
resAmb  = reactiveValues(results = NULL)
bgAmb   = reactiveValues(status = process_status(bgAmbpx))
obsAmbStatus = observe(
  {
    invalidateLater(millis = 500)
    bgAmb$status = process_status(bgAmbpx)
  },
  suspended = TRUE
)

### Kill current process ####
observeEvent(
  input$killALSAmb,
  isolate({
    if(!is.null(bgAmbpx)) {
      if(!is.null(bgAmb$status$running)) {
        if(bgAmb$status$running) {
          bgAmbpx$kill()
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
    sol = bgAmb$status$result
    if (length(sol$solutions) == 0) {
      if (sol$finished) {
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
        showModal(modalDialog(
          title = ">>>> No Solution Found <<<< ",
          paste0("Try to let the algorithm run for a longer time!"),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "s"
        ))
      }
    }
    resAmb$results = sol
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)

### Run ####
doAmbRot <- observeEvent(
  input$runALSAmb, {
    req(alsOut <- doALS())
    
    isolate({
      rotVec <- as.numeric(unlist(input$vecsToRotate))
      eps <- input$alsRotAmbEps
      dens <- input$alsRotAmbDens
    })
    
    if (length(rotVec) > 3) {
      showModal(modalDialog(
        title = ">>>> Too Many Vectors Selected <<<< ",
        paste0("Please choose 3 vectors max."),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "s"
      ))
    }
    
    C0 <- alsOut$C
    S0 <- alsOut$S
    nullC <- alsOut$nullC
    
    fun = get(paste0('rotAmb',length(rotVec)))
    rx <- callr::r_bg(
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
      package = TRUE
    )
    resAmb$results = NULL
    obsAmbStatus$resume()
    bgAmbpx <<- rx
    
    id = showNotification(
      "Running ambiguity explorer...",
      type = "message",
      duration = 5
    )

  }
)


rangesAmbSp <- reactiveValues(x = NULL, y = NULL)

output$ambSpVectors <- renderPlot({
  req(resAmb$results)    
  plotAmbVec(
    alsOut <- doALS(), 
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

output$ambKinVectors <- renderPlot({
  req(resAmb$results)    
  plotAmbVec(
    alsOut <- doALS(), 
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
