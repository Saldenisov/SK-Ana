
### Outputs ####
stdALSOut = reactiveFileReader(
  intervalMillis = 1000,
  session  = session,
  filePath = alsStdOut,
  readFunc = readLines,
  warn     = FALSE
)
# Live preview of ALS state (updated by background process)
alsPreview <- reactiveFileReader(
  intervalMillis = 1000,
  session = session,
  filePath = alsStateFile,
  readFunc = function(fp) {
    if (!file.exists(fp)) return(NULL)
    tryCatch(readRDS(fp), error = function(e) NULL)
  }
)
# Reactive value to cache last valid state
alsLastValid <- reactiveVal(NULL)
# Use final results if available, otherwise fall back to live preview
# Cache last valid state to prevent flickering during updates
alsLiveOut <- reactive({
  if (!is.null(resALS$results)) {
    alsLastValid(resALS$results)
    return(resALS$results)
  }
  preview <- alsPreview()
  if (!is.null(preview)) {
    alsLastValid(preview)
    return(preview)
  }
  # Return cached value if current read failed
  alsLastValid()
})

output$alsPrint <- renderPrint({
  cat(stdALSOut(), sep = '\n')
})
output$alsOpt <- renderUI({
  if (is.null(alsOut <- resALS$results)) {
    if(is.null(bgALSpx()))
      h5('Select ALS options and press "Run"\n')
    
  } else {
    list(
      h4(">>> ALS done <<<"),
      # h5(n, " species")
      h5("Terminated in ", alsOut$iter, " iterations"),
      if (alsOut$iter >= input$maxiter)
        strong("Warning : maxiter limit reached !!!"),
      h5(alsOut$msg)
    )
  }
})
# Dynamic slider for row selection
output$alsDataModDelayUI <- renderUI({
  sliderInput(
    "alsDataModDelay",
    "Row index (delay/time)",
    min = 1,
    max = max(1, length(Inputs$delay)),
    value = max(1, floor(length(Inputs$delay) / 2)),
    step = 1,
    sep = "",
    width = '100%'
  )
})

output$alsResid1 <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  display <- selected_plot_matrix()
  mat <- display$mat
  
  # Build model matrix
  mod <- component_matrix(CS$C, CS$S)
  
  # Get row index from slider - use directly as index
  rowIdx <- input$alsDataModDelay
  if (is.null(rowIdx)) rowIdx <- 1
  rowIdx <- max(1, min(rowIdx, nrow(mat)))  # Clamp to valid range
  
  delay <- Inputs$delay
  
  # Extract row data (signal vs wavelength at specific delay)
  dataRow = mat[rowIdx, ]
  modRow  = mod[rowIdx, ]
  if (all(is.na(dataRow))) dataRow = dataRow * 0
  if (all(is.na(modRow)))  modRow  = modRow  * 0
  
  # Create 3-panel comparison plot
  par(
    mfrow = c(1, 3),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  
  # Left panel: Row comparison plot (signal vs wavelength)
  plot(
    Inputs$wavl, dataRow,
    type = "l", col = lineColors[3], lwd = 2,
    xlab = "Wavelength (q)",
    ylab = "O.D.",
    main = paste0(
      "Row ", rowIdx, " at delay: ", signif(delay[rowIdx], 3)
    )
  )
  grid()
  lines(Inputs$wavl, modRow, col = lineColors[6], lwd = 2)
  legend(
    "topright",
    legend = c("Data", "Model"),
    col = lineColors[c(3, 6)],
    lwd = 2,
    bty = "n"
  )
  box()
  
  # Middle panel: Data image
  plotImage(
    Inputs$delay, Inputs$wavl, mat,
    main = "Data",
    cont = if (!is.null(input$alsContours)) input$alsContours else FALSE,
    delayTrans = Inputs$delayTrans
  )
  
  # Right panel: Model image
  plotImage(
    Inputs$delay, Inputs$wavl, mod,
    main = paste0("Model ", ncol(CS$S), " species"),
    cont = if (!is.null(input$alsContours)) input$alsContours else FALSE,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight)
output$alsResid3 <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  display <- selected_plot_matrix()
  mat <- display$mat
  plotResid(Inputs$delay, Inputs$wavl, mat,
            CS$C, CS$S,
            main = display$main,
            delayTrans = Inputs$delayTrans)
},
height = plotHeight)
output$alsResid2 <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  display <- selected_plot_matrix()
  mat <- display$mat
  plotResidAna(Inputs$delay, Inputs$wavl, mat,
               CS$C, CS$S,
               main = display$main,
               delayTrans = Inputs$delayTrans)
},
height = plotHeight)

rangesAlsKin <- reactiveValues(x = NULL, y = NULL)

output$alsKinVectors <- renderPlot({
  alsOut <- alsLiveOut()
  req(alsOut)  # Now safe - alsLiveOut caches last valid state
  
  # Use custom X-axis scale if provided
  xlim_custom <- rangesAlsKin$x
  if (!is.null(input$alsKinXmin) && !is.null(input$alsKinXmax)) {
    if (is.finite(input$alsKinXmin) && is.finite(input$alsKinXmax) && 
        input$alsKinXmin < input$alsKinXmax) {
      xlim_custom <- c(input$alsKinXmin, input$alsKinXmax)
    }
  }
  
  # Use custom Y-axis scale if provided
  ylim_custom <- rangesAlsKin$y
  if (!is.null(input$alsKinYmin) && !is.null(input$alsKinYmax)) {
    if (is.finite(input$alsKinYmin) && is.finite(input$alsKinYmax) && 
        input$alsKinYmin < input$alsKinYmax) {
      ylim_custom <- c(input$alsKinYmin, input$alsKinYmax)
    }
  }
  
  # Apply Savitzky-Golay smoothing if enabled
  if (!is.null(input$alsKinDisplaySmoothed) && input$alsKinDisplaySmoothed) {
    # Get smoothing parameters
    window <- input$alsKinSGWindow
    order <- input$alsKinSGOrder
    
    # Validate and adjust parameters
    if (window %% 2 == 0) window <- window + 1
    if (order >= window) order <- window - 2
    
    tryCatch({
      # Apply SG filter to kinetics data
      C_smoothed <- apply_sg_filter(alsOut$C, window, order)
      
      # Create modified alsOut with smoothed kinetics
      alsOut_smooth <- alsOut
      
      if (!is.null(input$alsKinDisplayBoth) && input$alsKinDisplayBoth) {
        # Plot both raw and smoothed
        # First plot raw kinetics
        plotAlsVec(alsOut,
                   type = "Kin",
                   xlim = xlim_custom,
                   ylim = ylim_custom,
                   delayTrans = Inputs$delayTrans
        )
        
        # Overlay smoothed kinetics with transparency
        par(new = TRUE)
        alsOut_smooth$C <- C_smoothed
        x <- alsOut_smooth$xC
        y <- alsOut_smooth$C
        
        matlines(x, y,
                type = "l", lwd = 3, lty = 1,
                col = adjustcolor(lineColors[1:ncol(y)], alpha.f = 0.7)
        )
      } else {
        # Plot only smoothed
        alsOut_smooth$C <- C_smoothed
        plotAlsVec(alsOut_smooth,
                   type = "Kin",
                   xlim = xlim_custom,
                   ylim = ylim_custom,
                   delayTrans = Inputs$delayTrans
        )
      }
    }, error = function(e) {
      log_warning(paste("Error applying SG filter to ALS kinetics:", e$message))
      # Fall back to unsmoothed plot
      plotAlsVec(alsOut,
                 type = "Kin",
                 xlim = xlim_custom,
                 ylim = ylim_custom,
                 delayTrans = Inputs$delayTrans
      )
    })
  } else {
    # No smoothing - plot raw kinetics
    plotAlsVec(alsOut,
               type = "Kin",
               xlim = xlim_custom,
               ylim = ylim_custom,
               delayTrans = Inputs$delayTrans
    )
  }
},
height = plotHeight
)

observeEvent(input$alsKin_dblclick, {
  brush <- input$alsKin_brush
  if (!is.null(brush)) {
    rangesAlsKin$x <- c(brush$xmin, brush$xmax)
    rangesAlsKin$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesAlsKin$x <- NULL
    rangesAlsKin$y <- NULL
  }
})

rangesAlsSp <- reactiveValues(x = NULL, y = NULL)

output$alsSpVectors <- renderPlot({
  alsOut <- alsLiveOut()
  req(alsOut)  # Now safe - alsLiveOut caches last valid state
  
  # Use custom X-axis scale if provided
  xlim_custom <- rangesAlsSp$x
  if (!is.null(input$alsSpXmin) && !is.null(input$alsSpXmax)) {
    if (is.finite(input$alsSpXmin) && is.finite(input$alsSpXmax) && 
        input$alsSpXmin < input$alsSpXmax) {
      xlim_custom <- c(input$alsSpXmin, input$alsSpXmax)
    }
  }
  
  # Use custom Y-axis scale if provided
  ylim_custom <- rangesAlsSp$y
  if (!is.null(input$alsSpYmin) && !is.null(input$alsSpYmax)) {
    if (is.finite(input$alsSpYmin) && is.finite(input$alsSpYmax) && 
        input$alsSpYmin < input$alsSpYmax) {
      ylim_custom <- c(input$alsSpYmin, input$alsSpYmax)
    }
  }
  
  plotAlsVec(alsOut,
             type = "Sp",
             xlim = xlim_custom,
             ylim = ylim_custom,
             nonnegS = input$nonnegS,
             delayTrans = Inputs$delayTrans
  )
},
height = plotHeight
)

observeEvent(input$alsSp_dblclick, {
  brush <- input$alsSp_brush
  if (!is.null(brush)) {
    rangesAlsSp$x <- c(brush$xmin, brush$xmax)
    rangesAlsSp$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesAlsSp$x <- NULL
    rangesAlsSp$y <- NULL
  }
})

#### Copy buttons ####
# observe({
#   # req(input$copybtn_ALS_Kin)
#   req(alsOut <- resALS$results)
#   C <- cbind(Inputs$delaySave, reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))$C)
#   colnames(C) <- c("delay", colnames(alsOut$C))
#   txt = readr::format_tsv(as.data.frame(C), eol = "\r\n")
#   shinyCopy2clipboard::CopyButtonUpdate(
#     session,
#     id    = "copybtn_ALS_Kin",
#     label = "Copy Kinetics",
#     icon  = icon("copy"),
#     text  = txt 
#   )
# })
# observe({
#   # req(input$copybtn_ALS_Sp)
#   req(alsOut <- resALS$results)
#   S <- cbind(Inputs$wavl, reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))$S)
#   colnames(S) <- c("wavl", colnames(alsOut$S))
#   txt = readr::format_tsv(as.data.frame(S), eol = "\r\n")
#   shinyCopy2clipboard::CopyButtonUpdate(
#     session,
#     id    = "copybtn_ALS_Sp",
#     label = "Copy Spectra",
#     icon  = icon("copy"),
#     text  = txt 
#   )
# })

observeEvent(
  input$alsSpKinSave,
  isolate({
    req(alsOut <- resALS$results)
    
    CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
    
    S <- cbind(Inputs$wavl, CS$S)
    colnames(S) <- c("wavl", colnames(alsOut$S))
    write.csv(
      S,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_alsSpectra_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    # C = cbind(alsOut$xC,alsOut$C)
    C <- cbind(Inputs$delaySave, CS$C)
    colnames(C) <- c("delay", colnames(alsOut$C))
    write.csv(
      C,
      file = file.path(
        "outputDir",
        paste0(
          input$projectTag,
          "_alsKinets_",
          input$nALS, "sp",
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    # Save G parameters if broadening is enabled
    if (!is.null(alsOut$G) && ncol(alsOut$G) > 0) {
      G <- cbind(Inputs$delaySave, alsOut$G)
      colnames(G) <- c("delay", colnames(alsOut$G))
      write.csv(
        G,
        file = file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_alsBroadening_",
            input$nALS, "sp",
            ".csv"
          )
        ),
        row.names = FALSE
      )
    }
    
    showNotification(
      "ALS results saved to outputDir folder",
      type = "message",
      duration = 3
    )
  })
)

output$alsSpKinDownload <- downloadHandler(
  filename = function() {
    paste0(input$projectTag, "_ALS_", input$nALS, "sp_results.zip")
  },
  content = function(fname) {
    req(alsOut <- resALS$results)
    
    # Create temporary directory
    tmpdir <- tempdir()
    
    CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
    
    # Save Spectra
    S <- cbind(Inputs$wavl, CS$S)
    colnames(S) <- c("wavl", colnames(alsOut$S))
    spectra_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_alsSpectra_",
      input$nALS, "sp.csv"
    ))
    write.csv(S, file = spectra_file, row.names = FALSE)
    
    # Save Kinetics
    C <- cbind(Inputs$delaySave, CS$C)
    colnames(C) <- c("delay", colnames(alsOut$C))
    kinets_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_alsKinets_",
      input$nALS, "sp.csv"
    ))
    write.csv(C, file = kinets_file, row.names = FALSE)
    
    # Save G parameters if broadening is enabled
    files_to_zip <- c(spectra_file, kinets_file)
    if (!is.null(alsOut$G) && ncol(alsOut$G) > 0) {
      G <- cbind(Inputs$delaySave, alsOut$G)
      colnames(G) <- c("delay", colnames(alsOut$G))
      broadening_file <- file.path(tmpdir, paste0(
        input$projectTag,
        "_alsBroadening_",
        input$nALS, "sp.csv"
      ))
      write.csv(G, file = broadening_file, row.names = FALSE)
      files_to_zip <- c(files_to_zip, broadening_file)
    }
    
    # Zip the files
    zip(zipfile = fname, files = files_to_zip, flags = "-j")
    
    # Handle .zip extension issue
    if(file.exists(paste0(fname, ".zip"))) {
      file.rename(paste0(fname, ".zip"), fname)
    }
  },
  contentType = "application/zip"
)

output$alsContribs <- renderPlot({
  req(alsOut <- resALS$results)
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  plotConbtribs(
    Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight
)
