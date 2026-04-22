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
  intervalMillis = 1000,
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
  intervalMillis = 1000,
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
  display <- selected_plot_matrix()
  mat <- display$mat
  plotResid(Inputs$delay, Inputs$wavl, mat,
    CS$C[,opt$active], CS$S,
    main = display$main,
    delayTrans = Inputs$delayTrans
  )
}, height = plotHeight)
output$kinResidAna   <- renderPlot({
  req(resLoc$results)
  opt    = isolate(resLoc$results)

  CS <- reshapeCS(opt$C, opt$S)
  display <- selected_plot_matrix()
  mat <- display$mat
  plotResidAna(Inputs$delay, Inputs$wavl, mat,
    CS$C[,opt$active], CS$S,
    main = display$main,
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
