### Transects ####
output$transects <- renderPlot({
  if (is.null(selectArea())) {
    return(NULL)
  }

  mat <- Inputs$mat
  wavl <- Inputs$wavl
  delay <- Inputs$delay
  trans <- Inputs$delayTrans
  
  validate(
    need(
      is.finite(diff(range(wavl))) &
        is.finite(diff(range(delay))) &
        is.finite(diff(range(mat, na.rm = TRUE))) &
        is.finite(diff(doRange())),
      'Data not ready !'
    )
  )
  
  if (is.null(rangesImage1$x)) {
    xlim <- range(delay)
  } else {
    xlim <- rangesImage1$x
  }
  
  if (is.null(rangesImage1$y)) {
    ylim <- range(wavl)
  } else {
    ylim <- rangesImage1$y
  }
  
  zlim = doRange()
  
  par(
    mfrow = c(2, 1),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  
  # Locally Averaged Spectrum
  dCut <- dlCut() * Inputs$dlScaleFacOrig
  iCut <- indxCuts(dCut, delay)
  indx <- iCut$indx
  delta <- iCut$delta
  if (length(indx) == 1) {
    cutMean <- mat[indx, ]
  } else {
    cutMean <- colMeans(mat[indx, ])
  }
  if (all(is.na(cutMean))) cutMean <- cutMean * 0
  matplot(
    wavl, cutMean,
    type = "l", col = lineColors[6], lwd = 2,
    xlab = "Wavelength", ylab = "O.D.",
    xlim = ylim,
    ylim = zlim, 
    yaxs = "i",
    main = paste0(
      "Mean O.D. at delay: ", signif(mean(delay[indx]), 3),
      ifelse(delta == 0,
             "",
             paste0(" +/- ", signif(delta / 2, 2))
      )
    )
  )
  grid()
  abline(h = 0, lty = 2)
  colorizeMask1D(axis = "wavl", ylim = zlim)
  box()
  
  # Locally Averaged Kinetics
  dCut <- wlCut()
  iCut <- indxCuts(dCut, wavl)
  indx <- iCut$indx
  delta <- iCut$delta
  if (length(indx) == 1) {
    cutMean <- mat[, indx]
  } else {
    cutMean <- rowMeans(mat[, indx])
  }
  if (all(is.na(cutMean))) cutMean <- cutMean * 0
  matplot(
    delay, cutMean,
    type = "l", col = lineColors[6], lwd = 2,
    xlab = paste0("Delay ",trans), 
    ylab = "O.D.",
    xlim = xlim,
    ylim = zlim, 
    yaxs = "i",
    main = paste0(
      "Mean O.D. at wavl: ", signif(mean(wavl[indx]), 3),
      ifelse(delta == 0,
             "",
             paste0(" +/- ", signif(delta / 2, 2))
      )
    )
  )
  grid()
  abline(h = 0, lty = 2)
  colorizeBaselineMask(ylim = zlim)
  colorizeMask1D(axis = "delay", ylim = zlim)
  box()
  
},
height = plotHeight, width = plotHeight
)
outputOptions(output, "transects",
              suspendWhenHidden = FALSE
)


rangesDl <- reactiveValues(x = NULL, y = NULL)

output$cutsDl <- renderPlot({
  if (is.null(selectArea())) {
    return(NULL)
  }
  
  mat <- Inputs$mat
  wavl <- Inputs$wavl
  delay <- Inputs$delay
  
  wCut <- seq(1, length(delay),
    by = max(1, input$stepDlCut)
  )

  if (is.null(rangesDl$y)) {
    ylim <- doRange()
  } else {
    ylim <- rangesDl$y
  }

  par(
    mfrow = c(1, 1),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  colset = seq(1,length(cutColors),length.out = length(wCut))
  matplot(
    wavl, t(mat[wCut, ]),
    type = "l", lty=1,
    xlim = rangesDl$x,
    ylim = ylim,
    xlab = "Wavelength", ylab = "DO",
    xaxs = "i", yaxs = "i",
    col  = cutColors[colset]
  )
  grid()
  colorizeMask1D(axis = "wavl", ylim = ylim)
  box()
})

observeEvent(input$cutsDl_dblclick, {
  brush <- input$cutsDl_brush
  if (!is.null(brush)) {
    rangesDl$x <- c(brush$xmin, brush$xmax)
    rangesDl$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesDl$x <- NULL
    rangesDl$y <- NULL
  }
})

rangesWl <- reactiveValues(x = NULL, y = NULL)

output$cutsWl <- renderPlot({
  if (is.null(selectArea())) {
    return(NULL)
  }

  mat <- Inputs$mat
  wavl <- Inputs$wavl
  delay <- Inputs$delay
  trans <- Inputs$delayTrans
  
  dCut <- seq(1, length(wavl),
    # by = ifelse(length(wavl ) >= 50 , 10, 1)
    by = max(1, input$stepWlCut)
  )

  if (is.null(rangesWl$y)) {
    ylim <- doRange()
  } else {
    ylim <- rangesWl$y
  }

  par(
    mfrow = c(1, 1),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  colset = seq(1,length(cutColors),length.out = length(dCut))
  matplot(
    delay, mat[, dCut],
    type = "l",
    xlim = rangesWl$x,
    ylim = ylim,
    xlab = paste0("Delay ",trans), 
    ylab = "DO",
    xaxs = "i", yaxs = "i",
    col  = cutColors[colset]
  )
  grid()
  colorizeBaselineMask(ylim=ylim)
  colorizeMask1D(axis = "delay", ylim = ylim)
  box()
})

observeEvent(input$cutsWl_dblclick, {
  brush <- input$cutsWl_brush
  if (!is.null(brush)) {
    rangesWl$x <- c(brush$xmin, brush$xmax)
    rangesWl$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesWl$x <- NULL
    rangesWl$y <- NULL
  }
})

observeEvent(
  input$wavlCutSave,
  isolate({
    mat <- Inputs$mat
    wavl <- Inputs$wavl
    delay <- Inputs$delay
    dCut <- wlCut()
    indx <- indxCuts(dCut, wavl)$indx
    if (length(indx) == 1) {
      cutMean <- mat[, indx]
    } else {
      cutMean <- rowMeans(mat[, indx])
    }
    write.csv(
      cbind(delay, cutMean),
      file =
        file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_wavlCut_",
            signif(mean(wavl[indx]), 3),
            ".csv"
          )
        ),
      row.names = FALSE
    )
  })
)

observeEvent(
  input$delayCutSave,
  isolate({
    mat <- Inputs$mat
    wavl <- Inputs$wavl
    delay <- Inputs$delay
    dCut <- dlCut() * Inputs$dlScaleFacOrig
    indx <- indxCuts(dCut, delay)$indx
    if (length(indx) == 1) {
      cutMean <- mat[indx, ]
    } else {
      cutMean <- colMeans(mat[indx, ])
    }
    write.csv(
      cbind(wavl, cutMean),
      file =
        file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_delayCut_",
            signif(mean(delay[indx]), 3),
            ".csv"
          )
        ),
      row.names = FALSE
    )
  })
)

#### Copy buttons ####
# observe({
#   req(Inputs$mat)
#   req(input$stepWlCut)
#   mat <- Inputs$mat
#   wavl <- Inputs$wavl
#   delay <- Inputs$delaySave
#   indx <- seq(1, length(wavl),
#               by = max(1, input$stepWlCut)
#   )
#   mat = mat[,indx]
#   wavl = c("delay",paste0(wavl[indx]))
#   C = cbind(delay, mat)
#   txt = readr::format_tsv(as.data.frame(C), eol = "\r\n")
#   shinyCopy2clipboard::CopyButtonUpdate(
#     session,
#     id    = "copybtn_dlCut",
#     label = "Copy",
#     icon  = icon("copy"),
#     text  = txt 
#   )
# })
# observe({
#   req(Inputs$mat)
#   req(input$stepDlCut)
#   mat   <- Inputs$mat
#   wavl  <- Inputs$wavl
#   delay <- Inputs$delaySave
#   indx  <- seq(1, length(delay),
#                by = max(1, input$stepDlCut)
#   )
#   mat = mat[indx,]
#   delay = c("wavl",paste0(delay[indx]))
#   S = cbind(wavl, t(mat))
#   txt = readr::format_tsv(as.data.frame(S), eol = "\r\n")
#   shinyCopy2clipboard::CopyButtonUpdate(
#     session,
#     id    = "copybtn_wlCut",
#     label = "Copy",
#     icon  = icon("copy"),
#     text  = txt 
#   )
# })

observeEvent(
  input$wavlStepCutSave,
  isolate({
    mat <- Inputs$mat
    wavl <- Inputs$wavl
    delay <- Inputs$delaySave
    indx <- seq(1, length(wavl),
                by = max(1, input$stepWlCut)
    )
    mat = mat[,indx]
    wavl = c("delay",paste0(wavl[indx]))
    
    write.table(
     cbind(delay, mat),
      file =
        file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_wavlCuts.csv"
          )
        ),
      sep = ",",
      row.names = FALSE,
      col.names = wavl
    )
  })
)

observeEvent(
  input$delayStepCutSave,
  isolate({
    mat <- Inputs$mat
    wavl <- Inputs$wavl
    delay <- Inputs$delaySave
    indx <- seq(1, length(delay),
                by = max(1, input$stepDlCut)
    )
    mat = mat[indx,]
    delay = c("wavl",paste0(delay[indx]))
    
    write.table(
      cbind(wavl, t(mat)),
      file =
        file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_delayCuts.csv"
          )
        ),
      sep = ",",
      row.names = FALSE,
      col.names = delay
    )
  })
)
