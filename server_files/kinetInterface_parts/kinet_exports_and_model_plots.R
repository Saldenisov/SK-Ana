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
# Dynamic slider for row selection
output$kinDataModDelayUI <- renderUI({
  sliderInput(
    "kinDataModDelay",
    "Row index (delay/time)",
    min = 1,
    max = max(1, length(Inputs$delay)),
    value = max(1, floor(length(Inputs$delay) / 2)),
    step = 1,
    sep = "",
    width = '100%'
  )
})

output$kinDatavsMod  <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)

  CS <- reshapeCS(opt$C, opt$S)
  display <- selected_plot_matrix()
  mat <- display$mat
  
  # Build model matrix
  mod <- component_matrix(CS$C[, opt$active, drop = FALSE], CS$S)
  
  # Get row index from slider - use directly as index
  rowIdx <- input$kinDataModDelay
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
    cont = input$kinContours,
    delayTrans = Inputs$delayTrans
  )
  
  # Right panel: Model image
  plotImage(
    Inputs$delay, Inputs$wavl, mod,
    main = paste0("Model ", ncol(CS$S), " species"),
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
