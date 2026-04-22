
## Per-component constraints UI ####
output$perComponentSUI <- renderUI({
  req(input$perComponentS)
  req(input$nALS)
  
  nS <- input$nALS
  
  # Create checkboxes for each component
  checkboxes <- lapply(1:nS, function(i) {
    checkboxInput(
      inputId = paste0("nonnegS_", i),
      label = paste0("S_", i, " > 0"),
      value = TRUE
    )
  })
  
  fluidRow(
    column(
      width = 12,
      HTML("<small>Uncheck to allow negative values (e.g., for difference spectra)</small>"),
      br(), br(),
      do.call(tagList, checkboxes)
    )
  )
})

## Per-component broadening UI ####
output$perComponentBroadeningUI <- renderUI({
  req(input$perComponentBroadening)
  req(input$broadeningS)
  req(input$nALS)
  
  nS <- input$nALS
  
  # Create checkboxes for each component
  checkboxes <- lapply(1:nS, function(i) {
    checkboxInput(
      inputId = paste0("broadenComp_", i),
      label = paste0("Broaden C_", i),
      value = TRUE
    )
  })
  
  fluidRow(
    column(
      width = 12,
      HTML("<small>Uncheck to disable broadening for specific components</small>"),
      br(), br(),
      do.call(tagList, checkboxes)
    )
  )
})


output$showMSE <- reactive({
  showMSE(
    input$procMult,
    input$rawData_rows_selected,
    input$nALS
  )
})

## Masks ####
output$maskSpExp_ui <- renderUI({
  if (!showMSE(
    input$procMult,
    input$rawData_rows_selected,
    input$nALS
  )
  ) {
    return(NULL)
  }
  
  nM <- length(input$rawData_rows_selected) # Nb data matrices
  nS <- input$nALS # Nb spectra
  
  if (anyNA(Inputs$maskSpExp)) {
    Inputs$maskSpExp <- matrix(1, nrow = nM, ncol = nS)
  } else
    if (nrow(Inputs$maskSpExp) != nM ||
        ncol(Inputs$maskSpExp) != nS) {
      Inputs$maskSpExp <- matrix(1, nrow = nM, ncol = nS)
    }
  
  matInput <- list(
    h5("Presence Matrix"),
    HTML("<table cellpadding=2 border=0>")
  )
  head <- paste0(paste0("<th>Sp_", 1:nS, "</th>"), collapse = "")
  matInput <- list(
    matInput,
    HTML(paste0("<tr><td>&nbsp;</td>", head, "</tr>"))
  )
  for (i1 in 1:nM) {
    var1 <- paste0("Exp_", i1)
    matInput <- c(matInput, list(HTML(paste0(
      "<tr><th>",
      var1, "&nbsp;</th>"
    ))))
    for (i2 in 1:nS) {
      var2 <- paste0("C_", i2)
      name <- paste0("mCE_", var1, "_", var2)
      value <- Inputs$maskSpExp[i1, i2]
      locInput <- list(
        HTML("<td>"),
        tags$input(
          id = name,
          type = "number",
          value = value,
          min = 0, max = 1,
          class = "shiny-bound-input",
          style = "width: 50px;"
        ),
        HTML("</td>")
      )
      matInput <- c(matInput, locInput)
    }
    matInput <- c(matInput, list(HTML("</tr>")))
  }
  matInput <- list(matInput, HTML("</table>"))
  
  wellPanel(
    verticalLayout(
      matInput,
      br(),
      fixedRow(
        column(12,
               offset = 0,
               actionButton("clear_mCE",
                            "Reset",
                            icon = icon("eraser")
               ),
               actionButton("update_mCE",
                            "Done",
                            icon = icon("check")
               )
        )
      )
    )
  )
})
outputOptions(output, "maskSpExp_ui",
              suspendWhenHidden = FALSE
)
# Update maskSpExp
observe({
  if (is.null(input$update_mCE) ||
      input$update_mCE == 0) {
    return()
  }
  
  isolate({
    if (is.null(Inputs$maskSpExp)) {
      return()
    }
    
    nM <- length(input$rawData_rows_selected)
    nS <- input$nALS
    for (i1 in 1:nM) {
      var1 <- paste0("Exp_", i1)
      for (i2 in 1:nS) {
        var2 <- paste0("C_", i2)
        name <- paste0("mCE_", var1, "_", var2)
        Inputs$maskSpExp[i1, i2] <- input[[name]]
      }
    }
  })
})

# Reset maskSpExp
observe({
  if (is.null(input$clear_mCE) ||
      input$clear_mCE == 0) {
    return()
  }
  
  isolate({
    if (is.null(Inputs$maskSpExp)) {
      return()
    }
    
    nM <- length(input$rawData_rows_selected)
    nS <- input$nALS
    Inputs$maskSpExp <- matrix(1, nrow = nM, ncol = nS)
    for (i1 in 1:nM) {
      var1 <- paste0("Exp_", i1)
      for (i2 in 1:nS) {
        var2 <- paste0("C_", i2)
        name <- paste0("mCE_", var1, "_", var2)
        updateNumericInput(session, inputId = name, value = 1)
      }
    }
  })
})

## ALS ####

als_selected_data <- safely(function(use_filtered = isTRUE(input$useFiltered)) {
  delay <- Inputs$delay[!is.na(Inputs$delayMask)]
  delayId <- Inputs$delayId[!is.na(Inputs$delayMask)]
  wavl <- Inputs$wavl[!is.na(Inputs$wavlMask)]

  if (use_filtered) {
    mat <- svd_masked_matrix()
    if (is.null(mat)) {
      return(NULL)
    }
  } else {
    mat <- Inputs$mat
    mat <- mat[!is.na(Inputs$delayMask), ]
    mat <- mat[, !is.na(Inputs$wavlMask)]
  }

  list(
    delay = delay,
    delayId = delayId,
    wavl = wavl,
    mat = mat
  )
}, return_on_error = NULL)

als_null_constraints <- safely(function(delayId, nAls) {
  if (anyNA(Inputs$maskSpExp)) {
    return(NA)
  }

  nullC <- matrix(1, nrow = length(delayId), ncol = nAls)
  for (i in 1:nAls) {
    for (j in 1:nrow(Inputs$maskSpExp)) {
      if (Inputs$maskSpExp[j, i] == 0) {
        sel <- which(delayId == j)
        nullC[sel, i] <- 0
      }
    }
  }

  nullC
}, return_on_error = NA)

als_nstart <- safely(function(nAls, initALS = input$initALS) {
  if (identical(initALS, "seq")) 2L else as.integer(nAls)
}, return_on_error = NULL)

als_initial_factors <- safely(function(initALS, nStart, mat, current_results = resALS$results) {
  if (initALS == "SVD" || initALS == "seq") {
    svdRES <- doSVD()
    if (is.null(svdRES)) {
      return(NULL)
    }

    return(list(
      S = matrix(abs(svdRES$v[, 1:nStart]), ncol = nStart),
      C = matrix(abs(svdRES$u[, 1:nStart]), ncol = nStart)
    ))
  }

  if (initALS == "PCA") {
    mat_centered <- scale(mat, center = TRUE, scale = FALSE)
    pcaRES <- svd(mat_centered, nu = nStart, nv = nStart)

    S <- matrix(abs(pcaRES$v[, 1:nStart]), ncol = nStart)
    C <- matrix(abs(pcaRES$u[, 1:nStart]), ncol = nStart)
    for (i in 1:nStart) {
      S[, i] <- S[, i] * pcaRES$d[i] / max(S[, i])
    }

    return(list(S = S, C = C))
  }

  if (initALS == "NMF") {
    svdRES <- doSVD()
    if (is.null(svdRES)) {
      return(NULL)
    }

    fMat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
    for (i in 1:nStart) {
      fMat <- fMat + svdRES$u[, i] %o% svdRES$v[, i] * svdRES$d[i]
    }
    res <- NMFN::nnmf(
      abs(fMat),
      k = nStart,
      method = "nnmf_als",
      eps = 1e-8
    )

    return(list(S = t(res$H), C = res$W))
  }

  if (is.null(current_results)) {
    return(NULL)
  }

  list(
    S = current_results$S[, 1:nStart],
    C = current_results$C[, 1:nStart]
  )
}, return_on_error = NULL)

als_norm_mode <- safely(function() {
  if (!is.null(input$normMode)) input$normMode else "intensity"
}, return_on_error = "intensity")

als_nonnegS_constraints <- safely(function(nAls) {
  if (!is.null(input$perComponentS) && input$perComponentS) {
    return(sapply(1:nAls, function(i) {
      val <- input[[paste0("nonnegS_", i)]]
      if (is.null(val)) TRUE else val
    }))
  }

  input$nonnegS
}, return_on_error = TRUE)

als_broadening_flags <- safely(function(nAls) {
  if (!is.null(input$perComponentBroadening) &&
      input$perComponentBroadening &&
      !is.null(input$broadeningS) &&
      input$broadeningS) {
    return(sapply(1:nAls, function(i) {
      val <- input[[paste0("broadenComp_", i)]]
      if (is.null(val)) TRUE else val
    }))
  }

  if (!is.null(input$broadeningS)) input$broadeningS else FALSE
}, return_on_error = FALSE)

als_clear_live_snapshot <- safely(function() {
  try({
    if (file.exists(alsStateFile)) {
      file.remove(alsStateFile)
    }
  }, silent = TRUE)
  invisible(NULL)
}, return_on_error = NULL)

### Asynchronous Process ####
bgALSpx = reactiveVal(NULL)
resALS  = reactiveValues(results = NULL)
bgALS   = reactiveValues(status = process_status(NULL))
alsStdOut <- tempfile(pattern = "als_", tmpdir = tempdir(), fileext = ".stdout")
# Live state snapshot file (updated during ALS)
alsStateFile <- tempfile(pattern = "als_state_", tmpdir = tempdir(), fileext = ".rds")
# Ensure the stdout file exists (portable across OSes)
tryCatch({
  if (!file.exists(dirname(alsStdOut))) {
    dir.create(dirname(alsStdOut), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(alsStdOut)) {
    file.create(alsStdOut, showWarnings = FALSE)
  }
}, error = function(e) {})
obsALSStatus = observe(
  {
    invalidateLater(millis = 1000)
    bgALS$status = process_status(bgALSpx())
  },
  suspended = TRUE
)

### Kill current process ####
observeEvent(
  input$killALS,
  isolate({
    if(!is.null(bgALSpx())) {
      if(!is.null(bgALS$status$running)) {
        if(bgALS$status$running) {
          bgALSpx()$kill()
          nclicks(0)
          obsALSStatus$suspend()
          id = showNotification(
            "ALS stopped !",
            type = "warning",
            duration = 5
          )
          resALS$results = NULL
        }
      }
    } 
  })
)

### Manage results ####
obsALS = observeEvent(
  bgALS$status$result, 
  {
    obsALSStatus$suspend()
    nclicks(0)
    resALS$results = bgALS$status$result
    
    # Update Reporting
    if(! 'ALS' %in% includeInReport()) {
      todo = c(includeInReport(),'ALS')
      includeInReport(todo) 
      updateCheckboxGroupInput(session,
                               inputId = "inReport",
                               choices = todo,
                               selected = todo
      )
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)


### Run ####
nclicks <- reactiveVal(0)
doALS <- observeEvent(
  input$runALS, {
    if (isolate(!checkInputsSanity())) {
      showNotification(
        "Inputs are incomplete: please load data and define a selection before running ALS.",
        type = "warning",
        duration = 6
      )
      return(NULL)
    }
    if(nclicks() != 0){
      showNotification("Already running ALS")
      return(NULL)
    }

    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)

    id = showNotification(
      "Running ALS...",
      type = "message",
      duration = 10
    )
    
    nAls = input$nALS
    selected_data <- als_selected_data()
    if (is.null(selected_data)) {
      nclicks(0)
      return(NULL)
    }
    delay <- selected_data$delay
    delayId <- selected_data$delayId
    wavl <- selected_data$wavl
    mat <- selected_data$mat

    nullC <- als_null_constraints(delayId, nAls)
    S0 <- selectedExternalSpectraALS()

    nStart <- als_nstart(nAls)
    init_values <- als_initial_factors(input$initALS, nStart, mat)
    if (is.null(init_values)) {
      nclicks(0)
      return(NULL)
    }
    S <- init_values$S
    C <- init_values$C

    normMode <- als_norm_mode()
    nonnegS_vec <- als_nonnegS_constraints(nAls)
    broadening_vec <- als_broadening_flags(nAls)
    als_clear_live_snapshot()

    launch <- launch_background_job(
      als,
      args = list(
        delay, delayId, wavl, mat, nullC, S, C,
        nAls    = nAls,
        nStart  = nStart,
        S0      = S0,
        maxiter = input$maxiter,
        uniS    = input$uniS,
        nonnegS = nonnegS_vec,
        nonnegC = input$nonnegC,
        thresh  = 10^input$alsThresh,
        normS   = input$normS,
        hardS0  = !input$softS0,
        wHardS0 = 10^input$wSoftS0,
        optS1st = input$optS1st,
        smooth  = input$smooth,
        SumS    = input$SumS,
        closeC  = input$closeC,
        wCloseC = 10^input$wCloseC,
        normMode = normMode,
        state_file = alsStateFile,
        update_interval = 10,
        broadening = broadening_vec,
        broadening_max_pct = if (!is.null(input$broadeningMaxPct)) input$broadeningMaxPct else 10
      ),
      stdout = alsStdOut,
      stderr = alsStdOut,
      job_name = "ALS background job"
    )
    rx <- launch$job
    if (is.null(rx)) {
      nclicks(0)
      showNotification(launch$error, type = "error", duration = 10)
      return(NULL)
    }
    resALS$results = NULL
    resAmb$results = NULL
    obsALSStatus$resume()
    bgALSpx(rx)
    
  }
)
