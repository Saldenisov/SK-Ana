
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
als <- function() {
  nAls <- input$nALS
  
  # (Re)-init output
  lapply(1:10, function(n) {
    output[[paste0("iter", n)]] <<- renderUI({
      list()
    })
  })
  
  # Reinit ambRot vector selection
  updateCheckboxGroupInput(
    session,
    inputId = "vecsToRotate",
    selected = c(1, 2)
  )
  
  # Suppress masked areas from coordinates
  delay <- Inputs$delay[!is.na(Inputs$delayMask)]
  delayId <- Inputs$delayId[!is.na(Inputs$delayMask)]
  wavl <- Inputs$wavl[!is.na(Inputs$wavlMask)]
  
  if (input$useFiltered) {
    # Choose SVD filtered matrix
    s <- doSVD()
    mat <- matrix(0, nrow = length(delay), ncol = length(wavl))
    for (ic in 1:input$nSV)
      mat <- mat + s$u[, ic] %o% s$v[, ic] * s$d[ic]
  } else {
    mat <- Inputs$mat
    mat <- mat[!is.na(Inputs$delayMask), ]
    mat <- mat[, !is.na(Inputs$wavlMask) ]
  }
  
  # External spectrum shapes
  S0 <- NULL
  if (length(externalSpectraALS) != 0)
    for(sp in names(externalSpectraALS)) {
      pname = paste0('fixALS_',sp)
      if( input[[pname]] )
        S0 = cbind(S0, externalSpectraALS[[sp]])
    }
  
  # Null C constraints
  nullC <- NA
  if (!anyNA(Inputs$maskSpExp)) {
    nullC <- matrix(1, nrow = length(delayId), ncol = nAls)
    for (i in 1:nAls) {
      for (j in 1:nrow(Inputs$maskSpExp)) {
        if (Inputs$maskSpExp[j, i] == 0) {
          sel <- which(delayId == j)
          nullC[sel, i] <- 0
        }
      }
    }
  }
  
  progress <- shiny::Progress$new(style = "notification")
  on.exit(progress$close())
  updateProgress <- function(value = NULL, detail = NULL) {
    progress$set(value = value, detail = detail)
  }
  
  if (input$initALS == "seq") {
    nStart <- 2
  } else {
    nStart <- nAls
  }
  
  if (input$initALS == "SVD" || input$initALS == "seq") {
    # initialize with abs(SVD)
    if (is.null(s <- doSVD())) {
      return(NULL)
    }
    
    S <- matrix(abs(s$v[, 1:nStart]), ncol = nStart)
    C <- matrix(abs(s$u[, 1:nStart]), ncol = nStart)
  } else if (input$initALS == "NMF") {
    # initialize with SVD + NMF
    if (is.null(s <- doSVD())) {
      return(NULL)
    }
    
    progress$set(message = "Running NMF ", value = 0)
    
    # 1/ filter matrix to avoid negative values (noise)
    fMat <- rep(0, nrow = nrow(data), ncol = ncol(data))
    for (i in 1:nStart)
      fMat <- fMat + s$u[, i] %o% s$v[, i] * s$d[i]
    # 2/ NMF
    # res  = NMF::nmf(abs(fMat), rank=nStart, method='lee')
    # C = NMF::basis(res)
    # S = t(NMF::coef(res))
    res <- NMFN::nnmf(abs(fMat),
                      k = nStart,
                      method = "nnmf_als",
                      eps = 1e-8)
    C <- res$W
    S <- t(res$H)
  } else {
    # restart from existing solution
    if (!exists("RES")) {
      return(NULL)
    }
    S <- RES$S[, 1:nStart]
    C <- RES$C[, 1:nStart]
  }
  
  # Progress bar
  progress$set(message = "Running ALS ", value = 0)
  
  # Run
  res <- list()
  for (n in nStart:nAls) {
    progress$set(message = paste0("Running ALS ", n), value = 0)
    res[[n]] <- myals(
      C = C, Psi = mat, S = S, xC = delay, xS = wavl,
      maxiter = input$maxiter,
      uniS = input$uniS,
      nonnegS = input$nonnegS,
      nonnegC = input$nonnegC,
      thresh = 10^input$alsThresh,
      normS = input$normS,
      S0 = S0,
      hardS0 = !input$softS0,
      wHardS0 = 10^input$wSoftS0,
      optS1st = input$optS1st,
      smooth = input$smooth,
      SumS = input$SumS,
      updateProgress = updateProgress,
      nullC = nullC,
      closeC = input$closeC,
      wCloseC = 10^input$wCloseC
    )
    res[[n]]$hessian <- NA # Compatibility with Kinet
    
    if (n < nAls) {
      # Prepare next iteration
      S <- res[[n]]$S
      C <- res[[n]]$C
      S <- cbind(S, 1)
      C <- cbind(C, 1)
    }
    RES <<- res[[n]]
  }
  
  lapply(nStart:nAls, function(n) {
    output[[paste0("iter", n)]] <<- renderUI({
      list(
        h4(n, " species"),
        h5("Terminated in ", res[[n]]$iter, " iterations"),
        res[[n]]$msg,
        br()
      )
    })
  })
  
  colnames(res[[nAls]]$S) <- paste0("S_", 1:nAls)
  colnames(res[[nAls]]$C) <- paste0("C_", 1:nAls)
  
  # Update Reporting
  todo = c(includeInReport(),'ALS')
  includeInReport(todo) 
  updateCheckboxGroupInput(session,
                           inputId = "inReport",
                           choices = todo,
                           selected = todo
  )
  
  res[[nAls]]$nullC <- nullC
  
  return(res[[nAls]])
}

doALS <- eventReactive(
  input$runALS, {
    if (isolate(!checkInputsSanity())) {
      return(NULL)
    }
    return(als())
  }
)

## Outputs ####
output$alsOpt <- renderUI({
  if (input$runALS == 0) {
    h5('Select ALS options and press "Run"\n')
  } else {
    alsOut <- doALS()
    h5("ALS done")
    isolate({
      if (alsOut$iter >= input$maxiter) {
        strong("Warning : maxiter limit reached !!!")
      }
    })
  }
})
output$alsResid1 <- renderPlot({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  
  if (isolate(input$useFiltered)) {
    # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
                  nrow = length(Inputs$delay),
                  ncol = length(Inputs$wavl))
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]
    
    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotDatavsMod(Inputs$delay, Inputs$wavl, mat,
                CS$C, CS$S,
                main = main,
                delayTrans = Inputs$delayTrans)
},
height = plotHeight)
output$alsResid3 <- renderPlot({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  
  if (isolate(input$useFiltered)) {
    # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
                  nrow = length(Inputs$delay),
                  ncol = length(Inputs$wavl))
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]
    
    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotResid(Inputs$delay, Inputs$wavl, mat,
            CS$C, CS$S,
            main = main,
            delayTrans = Inputs$delayTrans)
},
height = plotHeight)
output$alsResid2 <- renderPlot({
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  
  if (isolate(input$useFiltered)) {
    # Choose SVD filtered matrix
    s <- doSVD()
    CS1 <- reshapeCS(s$u, s$v, input$nSV)
    mat <- matrix(0,
                  nrow = length(Inputs$delay),
                  ncol = length(Inputs$wavl))
    for (ic in 1:input$nSV)
      mat <- mat + CS1$C[, ic] %o% CS1$S[, ic] * s$d[ic]
    
    main <- "SVD-filtered data"
  } else {
    mat <- Inputs$mat
    main <- "Raw data"
  }
  plotResidAna(Inputs$delay, Inputs$wavl, mat,
               CS$C, CS$S,
               main = main,
               delayTrans = Inputs$delayTrans)
},
height = plotHeight)

rangesAlsKin <- reactiveValues(x = NULL, y = NULL)

output$alsKinVectors <- renderPlot(
  {
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  plotAlsVec(alsOut,
             type = "Kin",
             xlim = rangesAlsKin$x,
             ylim = rangesAlsKin$y,
             delayTrans = Inputs$delayTrans
  )
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

output$alsSpVectors <- renderPlot(
  {
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  plotAlsVec(alsOut,
             type = "Sp",
             xlim = rangesAlsSp$x,
             ylim = rangesAlsSp$y,
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

observeEvent(
  input$alsSpKinSave,
  isolate({
    if (is.null(alsOut <- doALS())) {
      return(NULL)
    }
    
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
  })
)

output$alsContribs <- renderPlot(
  {
  if (is.null(alsOut <- doALS())) {
    return(NULL)
  }
  CS <- reshapeCS(alsOut$C, alsOut$S, ncol(alsOut$C))
  plotConbtribs(
    Inputs$delay, Inputs$wavl, Inputs$mat,
    CS$C, CS$S,
    delayTrans = Inputs$delayTrans
  )
},
height = plotHeight
)
