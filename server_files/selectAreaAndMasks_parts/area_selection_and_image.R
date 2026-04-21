
selectArea <- reactive({
  if (!checkInputsSanity()) {
    return(NULL)
  }

  delay     <- Inputs$delayOrig
  wavl      <- Inputs$wavlOrig
  mat       <- Inputs$matOrig
  delayId   <- Inputs$delayIdOrig
  delaySave <- Inputs$delaySaveOrig
    
  if (input$nMasksBaseline != 0) {
    for (mask in 1:input$nMasksBaseline) {
      xlim <- Masks[[paste0('blm', mask)]]() * Inputs$dlScaleFacOrig
      if (length(xlim) != 0) {
        if (diff(xlim) != 0) {
          sel <- delay >= xlim[1] & delay <= xlim[2]
          if (sum(sel) != 0) {
            # Baseline-mean correction per wavelength
            blCor = unlist(apply(mat[sel, ], 2, mean, na.rm = TRUE))
            # Apply up to next mask or end of matrix
            selt = NULL
            if (mask == input$nMasksBaseline) {
              selt = delay >= xlim[1]
            } else {
              xlimNext = Masks[[paste0('blm', mask + 1)]]() *
                Inputs$dlScaleFacOrig
              if (length(xlimNext) != 0)
                selt = delay >= xlim[1] & delay < xlimNext[1]
            }
            if (!is.null(selt))
              if (sum(selt) > 0) {
                mblCor = matrix(
                  blCor,
                  nrow = sum(selt),
                  ncol = ncol(mat),
                  byrow = TRUE
                )
                mat[selt, ] = mat[selt, ] - mblCor
              }
          }
        }
      }
    }
  }
  
  # Select work area
  # xlim <- input$keepDlRange * Inputs$dlScaleFacOrig
  xlim <- dlRange() * Inputs$dlScaleFacOrig
  ylim <- wlRange()

  subX <- delay >= xlim[1] & delay <= xlim[2]
  subY <- wavl  >= ylim[1] & wavl  <= ylim[2]

  delay     <- delay[subX]
  delayId   <- delayId[subX]
  wavl      <- wavl[subY]
  mat       <- mat[subX, subY]
  delaySave <- delaySave[subX]
  
  setInputValues(list(
    delay = delay,
    delayId = delayId,
    delaySave = delaySave,
    wavl = wavl
  ))

  # Aggregate and apply masks
  delayMask <- rep(0, length(delay))
  if (input$nMasksDl != 0) {
    for (mask in 1:input$nMasksDl) {
      xlim <- Masks[[paste0('dlm', mask)]]() * Inputs$dlScaleFacOrig
      if (length(xlim) != 0) {
        if (diff(xlim) != 0) {
          sel <- delay >= xlim[1] & delay <= xlim[2]
          if (sum(sel) != 0) delayMask[sel] = NA
        }
      }
    }
  }
  
  wavlMask <- rep(0, length(wavl))
  if (input$nMasksWl != 0) {
    for (mask in 1:input$nMasksWl) {
      ylim <- Masks[[paste0('wlm', mask)]]()
      if (length(ylim) != 0) {
        if (diff(ylim) != 0) {
          sel <- wavl >= ylim[1] & wavl <= ylim[2]
          if (sum(sel) != 0) wavlMask[sel] = NA
        }
      }
    }
  }
  
  if (!anyNA(Inputs$delayGlitch)) {
    for (i in 1:length(Inputs$delayGlitch))
      delayMask[which(delay == Inputs$delayGlitch[i])] = NA
  }

  # Track of baseline masks for plots
  baselineMask <- rep(0, length(delay))
  if (input$nMasksBaseline != 0) {
    for (mask in 1:input$nMasksBaseline) {
      xlim <- Masks[[paste0('blm', mask)]]() * Inputs$dlScaleFacOrig
      if (length(xlim) != 0) {
        if (diff(xlim) != 0) {
          sel <- delay >= xlim[1] & delay <= xlim[2]
          if (sum(sel) != 0) {
            baselineMask[sel] <- NA
          } 
        }
      }
    }
  }
  
  setInputValues(list(
    baselineMask = baselineMask,
    delayMask = delayMask,
    wavlMask = wavlMask
  ))

  mat[is.na(delayMask), ] = NA
  mat[, is.na(wavlMask)]  = NA

  setInputValue("mat", mat)

  # Automatic ajustment of DO range
  updateSlider(
    "keepDoRange",
    # signif(range(Inputs$matOrig, na.rm = TRUE), 3),
    signif(range(mat, na.rm = TRUE), 3),
    signif(range(mat, na.rm = TRUE), 3),
    200
  )
})

reshapeCS <- safely(function(U, V, n = NULL) {
  # Expand vectors wrt masks
  nC = ifelse(is.null(n),ncol(U),n)
  C <- matrix(NA, nrow = length(Inputs$delay), ncol = nC)
  colnames(C) <- colnames(U)
  nS = ifelse(is.null(n),ncol(V),n)
  S <- matrix(NA, nrow = length(Inputs$wavl), ncol = nS)
  colnames(S) <- colnames(V)
  i <- 0
  for (j in 1:nrow(C)) {
    if (!is.na(Inputs$delayMask[j])) {
      i <- i + 1
      C[j, ] <- U[i, 1:nC]
    }
  }
  i <- 0
  for (j in 1:nrow(S)) {
    if (!is.na(Inputs$wavlMask[j])) {
      i <- i + 1
      S[j, ] <- V[i, 1:nS]
    }
  }
  return(list(C = C, S = S))
}, return_on_error = NULL)

## Manage masksDl ####
observeEvent(
  input$nMasksDl,
  isolate({
    nsteps <- min(length(Inputs$delayOrig), 500)
    dlRange <- signif(range(Inputs$delayOrig / Inputs$dlScaleFacOrig), 3)

    if (input$nMasksDl != 0) {
      # Add new slider(s) if required
      for (mask in 1:input$nMasksDl) {
        maskName <- paste0("keepDlMask", mask)
        if (!hasMask("dl", maskName)) {
          insertUI(
            selector = "#masksC",
            where = "beforeEnd",
            ui = tags$div(
              id = maskName,
              sliderInput(
                inputId = maskName,
                label = NULL,
                min = dlRange[1],
                max = dlRange[2],
                value = c(dlRange[1], dlRange[1]),
                step = signif(diff(dlRange) / nsteps, 3),
                sep = ""
              )
            )
          )
          registerMask("dl", maskName)
        }
      }
    }
    # Remove extra sliders
    for (mask in (input$nMasksDl + 1):15) {
      maskName <- paste0("keepDlMask", mask)
      if (hasMask("dl", maskName)) {
        removeUI(
          selector = paste0("#", maskName),
          immediate = TRUE
        )
        unregisterMask("dl", maskName)
      }
    }
  })
)

## AutoDlMAsk ####
observeEvent(
  input$autoDlMask,
  isolate({

    # Mask 1 area per input dataset
    nmat <- 1
    if (!is.null(input$procMult)) {
      if (input$procMult == "tileDel") {
        nmat <- length(input$rawData_rows_selected)
      }
    }

    # Get changepoints
    chgp <- autoDlMask(Inputs$matOrig, nmat)
    if (!is.null(chgp)) {
      chgp <- c(1, chgp)

      # Remove all sliders
      for (mask in 1:15) {
        maskName <- paste0("keepDlMask", mask)
        if (hasMask("dl", maskName)) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          unregisterMask("dl", maskName)
        }
      }

      # Generate sliders
      nsteps <- min(length(Inputs$delayOrig), 500)
      dlRange <- signif(range(Inputs$delayOrig / Inputs$dlScaleFacOrig), 3)
      for (mask in 1:nmat) {
        maskName <- paste0("keepDlMask", mask)
        sel <- c(chgp[2 * (mask - 1) + 1], chgp[2 * (mask - 1) + 2])
        value <- Inputs$delayOrig[sel] / Inputs$dlScaleFacOrig
        insertUI(
          selector = "#masksC",
          where = "beforeEnd",
          ui = tags$div(
            id = maskName,
            sliderInput(
              inputId = maskName,
              label = NULL,
              min = dlRange[1],
              max = dlRange[2],
              value = value,
              step = signif(diff(dlRange) / nsteps, 3),
              sep = ""
            )
          )
        )
        registerMask("dl", maskName)
      }
    }

    if (nmat != input$nMasksDl) {
      updateNumericInput(
        session = session,
        inputId = "nMasksDl",
        value = nmat
      )
    }
  })
)

## Manage MasksWl ####
observeEvent(
  input$nMasksWl,
  isolate({
    nsteps <- min(length(Inputs$wavlOrig), 200)
    wlRange <- signif(range(Inputs$wavlOrig), 3)

    if (input$nMasksWl != 0) {
      # Add new slider(s) if required
      for (mask in 1:input$nMasksWl) {
        maskName <- paste0("keepWlMask", mask)
        if (!hasMask("wl", maskName)) {
          insertUI(
            selector = "#masksS",
            where = "beforeEnd",
            ui = tags$div(
              id = maskName,
              sliderInput(
                inputId = maskName,
                label = NULL,
                min = wlRange[1],
                max = wlRange[2],
                value = c(wlRange[1], wlRange[1]),
                step = signif(diff(wlRange) / nsteps, 3),
                sep = ""
              )
            )
          )
          registerMask("wl", maskName)
        }
      }
    }
    # Remove extra sliders
    for (mask in (input$nMasksWl + 1):15) {
      maskName <- paste0("keepWlMask", mask)
      if (hasMask("wl", maskName)) {
        removeUI(
          selector = paste0("#", maskName),
          immediate = TRUE
        )
        unregisterMask("wl", maskName)
      }
    }
  })
)

## AutoWlMAsk ####
observeEvent(
  input$autoWlMask,
  isolate({

    # TO BE UPDATED IF Wavl tiling...
    nmat <- 1
    nmasks <- 0

    # Get changepoints
    chgp <- autoWlMask(Inputs$matOrig, nmat)
    if (!is.null(chgp)) {
      chgp <- c(1, chgp, nrow(Inputs$matOrig)) # valid if no tiling ???

      # Remove all sliders
      for (mask in 1:15) {
        maskName <- paste0("keepWlMask", mask)
        if (hasMask("wl", maskName)) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          unregisterMask("wl", maskName)
        }
      }

      # Generate sliders
      nmasks <- length(chgp) - 2

      nsteps <- min(length(Inputs$wavlOrig), 200)
      wlRange <- range(Inputs$wavlOrig)
      for (mask in 1:nmasks) {
        maskName <- paste0("keepWlMask", mask)
        sel <- c(chgp[2 * (mask - 1) + 1], chgp[2 * (mask - 1) + 2])
        value <- Inputs$wavlOrig[sel]
        insertUI(
          selector = "#masksS",
          where = "beforeEnd",
          ui = tags$div(
            id = maskName,
            sliderInput(
              inputId = maskName,
              label = NULL,
              min = signif(wlRange[1], 3),
              max = signif(wlRange[2], 3),
              value = signif(value, 3),
              step = signif(diff(wlRange) / nsteps, 3),
              sep = ""
            )
          )
        )
        registerMask("wl", maskName)
      }
    }

    if (nmasks != input$nMasksWl) {
      updateNumericInput(
        session = session,
        inputId = "nMasksWl",
        value = nmasks
      )
    }
  })
)

## Image ####
colorizeBaselineMask <- safely(function(ylim = NULL, eps = 1e-4) {
  mask   <- Inputs$baselineMask
  values <- Inputs$delay
  masked <- which(is.na(mask))

  # Remove extremities
  sel <- masked != 1 & masked != length(values)
  masked <- sort(masked[sel], decreasing = FALSE)

  for (i in masked) {
    if (is.na(mask[i - 1])) {
      x1 <- values[i]
    } # Prevent overlap of rectangles
    else {
      x1 <- values[i - 1]
    }
    x2 <- values[i + 1]
    rect(x1 + eps, ylim[1], x2 - eps, ylim[2],
         border = NA, col = pink_tr2
    )
  }
}, return_on_error = NULL)

colorizeMask1D <- safely(function(axis = "delay", dir = "v",
                           ylim = NULL, eps = 1e-4) {
  if (axis == "delay") {
    mask   <- Inputs$delayMask
    values <- Inputs$delay
  } else {
    mask   <- Inputs$wavlMask
    values <- Inputs$wavl
  }
  masked <- which(is.na(mask))
  
  # Remove extremities
  sel <- masked != 1 & masked != length(values)
  masked <- sort(masked[sel], decreasing = FALSE)
  
  for (i in masked) {
    if (is.na(mask[i - 1])) {
      x1 <- values[i]
    } # Prevent overlap of rectangles
    else {
      x1 <- values[i - 1]
    }
    
    x2 <- values[i + 1]
    
    if (dir == "v") {
      rect(x1 + eps, ylim[1], x2 - eps, ylim[2],
           border = NA, col = mask_tr2
      )
    } else {
      rect(ylim[1], x1 + eps, ylim[2], x2 - eps,
           border = NA, col = mask_tr2
      )
    }
  }
}, return_on_error = NULL)

rangesImage1 <- reactiveValues(x = NULL, y = NULL)

output$image1 <- renderPlot({
  if (is.null(selectArea()) |
      is.null(Inputs$mat)) {
    return(NULL)
  }
  
  mat   <- Inputs$mat
  wavl  <- Inputs$wavl
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
  
  par(
    mfrow = c(1, 1),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  
  image(
    delay, wavl, mat,
    xlab = paste0("Delay ",trans), 
    ylab = "Wavelength",
    col  = imgColors,
    xlim = xlim,
    ylim = ylim,
    zlim = doRange(),
    main = 'Data'
  )
  
  abline(
    v = dlCut() * Inputs$dlScaleFacOrig,
    lwd = 2, col = lineColors[7], lty = 2
  )
  abline(
    h = wlCut(),
    lwd = 2, col = lineColors[7], lty = 2
  )

  # Mask for baseline treatment
  colorizeBaselineMask(ylim = ylim)
  
  # if (input$keepCbl != 0) {
  #   rect(Inputs$delayOrig[1],
  #        Inputs$wavlOrig[1],
  #        Inputs$delayOrig[input$keepCbl],
  #        Inputs$wavlOrig[length(Inputs$wavlOrig)],
  #        border = NA,
  #        col = pink_tr
  #   )
  # }
  
  # Show data masks
  colorizeMask1D(axis = "delay", ylim = ylim)
  colorizeMask1D(axis = "wavl", dir = "h", ylim = xlim)
  
  box()
},
height = plotHeight, width = plotHeight
)
outputOptions(output, "image1",
  suspendWhenHidden = FALSE
)

observeEvent(input$image1_dblclick, {
  brush <- input$image1_brush
  if (!is.null(brush)) {
    rangesImage1$x <- c(brush$xmin, brush$xmax)
    rangesImage1$y <- c(brush$ymin, brush$ymax)
  } else {
    rangesImage1$x <- NULL
    rangesImage1$y <- NULL
  }
})
