project_state_format_version <- 1L
project_slider_fields <- c(
  "keepDoRange",
  "keepWlRange",
  "keepWlCut",
  "keepDlRange",
  "keepDlCut",
  "keepCbl"
)

build_project_config <- safely(function(input_values) {
  cfg <- input_values[intersect(names(input_values), project_slider_fields)]
  cfg[!vapply(cfg, is.null, logical(1))]
}, return_on_error = list())

build_project_state <- safely(function(input_values, input_state) {
  list(
    version = project_state_format_version,
    metadata = list(
      project_tag = if (!is.null(input_values$projectTag)) input_values$projectTag else "",
      saved_at = as.character(Sys.time())
    ),
    config = build_project_config(input_values),
    data = input_state
  )
}, return_on_error = NULL)

write_project_state <- safely(function(path, state) {
  project_state <- state
  save(project_state, file = path)
  invisible(path)
}, return_on_error = NULL)

read_project_state <- safely(function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }

  state_env <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = state_env)

  if ("project_state" %in% loaded_names && exists("project_state", envir = state_env, inherits = FALSE)) {
    state <- get("project_state", envir = state_env, inherits = FALSE)
    if (!is.list(state) || !is.list(state$data)) {
      return(NULL)
    }
    if (is.null(state$config) || !is.list(state$config)) {
      state$config <- list()
    }
    if (is.null(state$metadata) || !is.list(state$metadata)) {
      state$metadata <- list(project_tag = "")
    }
    return(state)
  }

  legacy_data <- NULL
  if ("data" %in% loaded_names && exists("data", envir = state_env, inherits = FALSE)) {
    legacy_data <- get("data", envir = state_env, inherits = FALSE)
  } else if ("Inputs" %in% loaded_names && exists("Inputs", envir = state_env, inherits = FALSE)) {
    legacy_data <- get("Inputs", envir = state_env, inherits = FALSE)
  }
  if (!is.list(legacy_data)) {
    return(NULL)
  }

  legacy_config <- list()
  if ("config" %in% loaded_names && exists("config", envir = state_env, inherits = FALSE)) {
    loaded_config <- get("config", envir = state_env, inherits = FALSE)
    if (is.list(loaded_config)) {
      legacy_config <- loaded_config[intersect(names(loaded_config), project_slider_fields)]
    }
  }

  list(
    version = 0L,
    metadata = list(
      project_tag = if (!is.null(legacy_config$projectTag)) legacy_config$projectTag else ""
    ),
    config = legacy_config,
    data = legacy_data
  )
}, return_on_error = NULL)

projectState <- reactiveVal(NULL)
projectLoadError <- reactiveVal(NULL)

apply_project_state <- safely(function(state) {
  if (is.null(state) || !is.list(state) || !is.list(state$data)) {
    return(FALSE)
  }

  for (n in names(state$data)) {
    Inputs[[n]] <- state$data[[n]]
  }

  if (!is.null(state$metadata$project_tag) && nzchar(state$metadata$project_tag)) {
    updateTextInput(session, inputId = "projectTag", value = state$metadata$project_tag)
  }

  initSliders(if (length(state$config) == 0) NULL else state$config)
  projectState(state)
  projectLoadError(NULL)
  TRUE
}, return_on_error = FALSE)

output$ui      = renderUI({
  if( !dataLoaded() )
    return(NULL)
  if(length(input$rawData_rows_selected) == 0)
    return(NULL)
  
  if(length(input$rawData_rows_selected) == 1) {
    # Single matrix : no processing options
    Inputs$process <- TRUE
    Inputs$finish  <- FALSE
    finishMatrix()
    return(NULL)
    
  } else {
    # Several matrices: propose processing options
    ndelay  = nwavl = c()
    ii=0
    for (i in input$rawData_rows_selected) {
      ii = ii+1
      ndelay[ii] = length(RawData[[i]]$delay)
      nwavl[ii]  = length(RawData[[i]]$wavl)
    }
    ok_delay = length(unique(ndelay)) == 1
    ok_wavl  = length(unique(nwavl))  == 1
    choices = list()
    if(ok_delay & ok_wavl)
      choices[["Average"]]    = 'avrg'
    if (ok_delay)
      choices[["Tile Wavl"]]  = 'tileWav'
    if (ok_wavl)
      choices[["Tile Delay"]] = 'tileDel'
    
    Inputs$process <- FALSE
    if(length(choices) == 0 ) {
      showModal(modalDialog(
        title = ">>>> Data problem <<<< ",
        paste0("The chosen data have inconsient dimensions. ",
               "They cannot be treated simultaneously !"),
        easyClose = TRUE, 
        footer = modalButton("Close"),
        size = 's'
      ))
    } else {
      verticalLayout(
        column(
          6,
          radioButtons(
            inputId  = 'procMult', 
            label    = h4('Please choose processing option'),
            choices  = choices,
            selected = choices[length(choices)],
            inline   = TRUE),
          shinyBS::bsTooltip(
            'procMult',
            title = 'Choice based on dims of matrices')
        ),
        column(
          2,
          actionButton(
            "process",
            strong("Do it!"),
            icon=icon('cog')
          ),
          tags$style(
            type='text/css',
            "#process { width:100%; margin-top: 5px;}"
          )
        )
      )
    }
    
  } 
  
})

observeEvent(input$process,
  isolate({
    Inputs$process <- TRUE
    Inputs$finish  <- FALSE
    finishMatrix()
  })
)

observeEvent(input$projectFile, {
  projectState(NULL)
  projectLoadError(NULL)

  if (is.null(input$projectFile)) {
    return(NULL)
  }

  state <- read_project_state(input$projectFile$datapath)
  if (is.null(state)) {
    projectLoadError("The selected project file could not be read.")
    return(NULL)
  }

  if (!apply_project_state(state)) {
    projectLoadError("The selected project file has an unsupported or invalid structure.")
    return(NULL)
  }
})

output$loadErrorOpen <- renderUI({
  msg <- projectLoadError()
  if (is.null(msg) || !nzchar(msg)) {
    return(NULL)
  }

  div(
    class = "text-danger",
    strong(msg)
  )
})

output$projectInfoNew <- renderUI({
  if(!Inputs$finish)
    return(NULL)
  
  HTML(paste0(
    '<b>Global matrix</b>: ',
    length(Inputs$delay),'x', length(Inputs$wavl),'<br>',
    'O.D.  range  : ',paste0(signif(range(Inputs$mat)      ,2),
                           collapse=', '),'<br>',
    'Delay range  : ',paste0(signif(range(Inputs$delay),4),
                           collapse=', '),'<br>',
    'Delay transfo: ',Inputs$delayTrans,'<br>',
    'Wavl  range  : ',paste0(signif(range(Inputs$wavl), 4),
                           collapse=', ')
  )
  )
})
output$showPIN = reactive({
  Inputs$process &
    length(input$rawData_rows_selected) != 0
})
outputOptions(output, "showPIN", suspendWhenHidden = FALSE)
output$vignette <- renderPlot({
  if(!Inputs$finish)
    return(NULL)
  
  mat   = Inputs$mat
  wavl  = Inputs$wavl
  delay = Inputs$delay
  trans = Inputs$delayTrans
  
  validate(
    need(
      is.finite(diff(range(wavl))) &
        is.finite(diff(range(delay))) &
        is.finite(diff(range(mat, na.rm = TRUE))),
      FALSE
    )
  )

  par(
    mfrow = c(1, 1),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  image(
    delay, wavl, mat,
    xlab = paste0('Delay ',trans), 
    ylab = 'Wavelength',
    col  = imgColors, 
    zlim = quantile(mat,probs = c(0.001,0.999),na.rm = TRUE)
  )
})

output$projectInfoOpen <- renderPrint({
  validate(
    need(
      !is.null(input$projectFile),
      "Please select a project file (*.Rda)"
    )
  )
  validate(
    need(
      is.null(projectLoadError()),
      projectLoadError()
    )
  )

  state <- projectState()
  validate(
    need(
      !is.null(state),
      "Project state is not loaded."
    )
  )
  
  # Check file read
  cat(paste0('Project: ', input$projectTag, '\n'))
  cat('Format version: ', state$version, '\n\n')
  cat('Data File(s):\n')
  cat(paste0(Inputs$fileOrig,'\n'))
  cat("\n")
  cat(paste0(
    'Matrix: ',
    length(Inputs$delayOrig),'x',
    length(Inputs$wavlOrig),'\n'
  ))
  cat('Delay range: ',range(Inputs$delayOrig),'...\n')
  cat('Wavl  range: ',range(Inputs$wavlOrig),'...\n')
  
  cat("Sanity:",checkInputsSanity(),'\n')
  
})

output$saveProject <- downloadHandler(
  filename = function()    {
    paste0(input$projectTag,'.Rda')
  },
  content  = function(con) {
    state <- build_project_state(
      input_values = reactiveValuesToList(input),
      input_state = reactiveValuesToList(Inputs)
    )
    if (is.null(state) || is.null(write_project_state(con, state))) {
      stop("Failed to save project state.")
    }
  }
)

observeEvent(
  input$dataSave,
  isolate({
    mat   <- Inputs$mat
    wavl  <- Inputs$wavl
    delay <- Inputs$delaySave
    header = c("wavl",paste0(delay))
    # print(str(Inputs$delay))
    # print(str(Inputs$delaySave))
    
    write.table(
      cbind(wavl, t(mat)),
      file =
        file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_Matrix.csv"
          )
        ),
      sep = ",",
      row.names = FALSE,
      col.names = header
    )
  })
)
