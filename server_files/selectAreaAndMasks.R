# Functions ####

indxCuts <- function(xCut, coords, minx = 50) {
  delta <- 0
  # Select indices around cut
  if (xCut == coords[1]) {
    # First point
    indx <- c(1)
  } else {
    if (xCut == coords[length(coords)]) {
      # Last point
      indx <- c(length(coords))
    } else {
      if (length(coords) > 2 * minx) {
        # Select points around cut
        delta <- diff(range(coords)) / minx
        indx <- which(coords > xCut - delta / 2 &
          coords < xCut + delta / 2)
      } else {
        # Select point nearest cut
        indx <- c(which.min(abs(coords - xCut)))
      }
    }
  }
  return(list(indx = indx, delta = delta))
}

autoDlMask <- function(mat, nmat) {
  # Locate empty delay areas (experimental)

  # Integrate on wavl
  trace <- rowSums(mat)

  # Special treatment for nmat=1
  # (SegNeigh fails with Q=2 !!!)
  ans <- changepoint::cpt.var(
    diff(trace),
    penalty = "BIC",
    method = "SegNeigh",
    Q = 2 + max(1, 2 * (nmat - 1))
  )
  if (nmat == 1) {
    chp <- cpts(ans)[2]
  } else {
    chp <- cpts(ans)
  }

  return(chp)
}

autoWlMask <- function(mat, nmat) {
  # Locate useless wavl areas (experimental)

  # Integrate on wavl
  trace <- colSums(mat)

  # Special treatment for nmat=1
  # (SegNeigh fails with Q=2 !!!)
  ans <- changepoint::cpt.var(
    diff(trace),
    penalty = "BIC",
    method = "SegNeigh",
    Q = 2 + max(1, 2 * (nmat - 1))
  )
  chp <- sort(cpts(ans))

  return(chp)
}


# Interactive ####

observeEvent(
  input$reset,
  initSliders()
)

observeEvent(
  input$saveSelectors,
  isolate({
    ll <- list()
    # ID
    ll$projectTag <- input$projectTag
    ll$matDims <- dim(Inputs$matOrig)
    # Selectors
    ll$keepCbl <- input$keepCbl
    ll$keepDlRange <- input$keepDlRange
    ll$keepWlRange <- input$keepWlRange
    ll$dlScaleFacOrig <- Inputs$dlScaleFacOrig
    # Masks
    ll$nMasksBaseline <- input$nMasksBaseline
    if (input$nMasksBaseline != 0) {
      for (mask in 1:input$nMasksBaseline) {
        maskName <- paste0("keepBaselineMask", mask)
        ll[[maskName]] <- input[[maskName]]
      }
    }
    ll$nMasksWl <- input$nMasksWl
    if (input$nMasksWl != 0) {
      for (mask in 1:input$nMasksWl) {
        maskName <- paste0("keepWlMask", mask)
        ll[[maskName]] <- input[[maskName]]
      }
    }
    ll$nMasksDl <- input$nMasksDl
    if (input$nMasksDl != 0) {
      for (mask in 1:input$nMasksDl) {
        maskName <- paste0("keepDlMask", mask)
        ll[[maskName]] <- input[[maskName]]
      }
    }
    ll$delayGlitch <- Inputs$delayGlitch
    # Save
    file <- file.path(
      "outputDir",
      paste0(input$projectTag, "_Selections.Rda")
    )
    save(ll, file = file)
    showModal(modalDialog(
      title = ">>>> Selections Saved <<<< ",
      paste0("File:", file),
      footer = modalButton("Close"),
      easyClose = TRUE, fade = TRUE, size = "m"
    ))
  })
)

observeEvent(
  input$selectorFile,
  isolate({
    # Get data
    load(input$selectorFile$datapath)

    # Check project name and matrix dims
    if (ll$projectTag != input$projectTag ||
      ll$matDims != dim(Inputs$matOrig)) {
      showModal(modalDialog(
        title = ">>>> Incorrect File <<<< ",
        paste0(
          "The chosen selections file does not match ",
          "the current project!"
        ),
        footer = modalButton("Close"),
        easyClose = TRUE, fade = TRUE, size = "m"
      ))
    } else {
      Inputs$dlScaleFacOrig <<- ll$dlScaleFacOrig
      Inputs$delayGlitch <<- ll$delayGlitch

      # Ranges of sliders
      wavl  <- Inputs$wavlOrig
      delay <- Inputs$delayOrig / Inputs$dlScaleFacOrig
      mat   <- Inputs$matOrig
      wlRange  <- signif(range(wavl), 3)
      dlRange  <- signif(range(delay), 3)
      cblRange <- c(0, length(delay))

      nsteps <- min(length(wavl), 200)
      wlRangeSel <- ll$keepWlRange
      updateSlider("keepWlRange", wlRange, wlRangeSel, nsteps)

      nsteps <- min(length(delay), 500)
      dlRangeSel <- ll$keepDlRange
      updateSlider("keepDlRange", dlRange, dlRangeSel, nsteps)

      nsteps <- round(diff(cblRange) / 10)
      cblSel <- ll$keepCbl
      updateSlider("keepCbl", cblRange, cblSel, nsteps)

      # Wavl masks
      ## Remove existing Masks sliders
      for (mask in 1:20) {
        maskName <- paste0("keepWlMask", mask)
        if (maskName %in% masksWl) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksWl <<- masksWl[-which(masksWl == maskName)]
        }
      }

      ## Generate Masks sliders if necessary
      nMasks <- ll$nMasksWl
      if (nMasks != 0) {
        nsteps <- min(length(wavl), 200)
        for (mask in 1:nMasks) {
          maskName <- paste0("keepWlMask", mask)
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
                value = ll[[maskName]],
                step = signif(diff(wlRange) / nsteps, 3),
                sep = ""
              )
            )
          )
          masksWl <<- unique(c(masksWl, maskName))
        }
      }
      # if(nMasks != input$nMasksWl)
      updateNumericInput(
        session = session,
        inputId = "nMasksWl",
        value = nMasks
      )

      # Delay masks
      ## Remove existing Masks sliders
      for (mask in 1:20) {
        maskName <- paste0("keepDlMask", mask)
        if (maskName %in% masksDl) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksDl <<- masksDl[-which(masksDl == maskName)]
        }
      }

      ## Generate Masks sliders if necessary
      nMasks <- ll$nMasksDl
      if (nMasks != 0) {
        nsteps <- min(length(delay), 500)
        for (mask in 1:nMasks) {
          maskName <- paste0("keepDlMask", mask)
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
                value = ll[[maskName]],
                step = signif(diff(dlRange) / nsteps, 3),
                sep = ""
              )
            )
          )
          masksDl <<- unique(c(masksDl, maskName))
        }
      }
      # if(nMasks != input$nMasksDl)
      updateNumericInput(
        session = session,
        inputId = "nMasksDl",
        value = nMasks
      )
      
      # Baseline masks
      ## Remove existing Masks sliders
      for (mask in 1:20) {
        maskName <- paste0("keepBaselineMask", mask)
        if (maskName %in% masksBaseline) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksBaseline <<- 
            masksBaseline[-which(masksBaseline == maskName)]
        }
      }
      
      ## Generate Masks sliders if necessary
      nMasks <- ll$nMasksBaseline
      if (nMasks != 0) {
        nsteps <- min(length(delay), 500)
        for (mask in 1:nMasks) {
          maskName <- paste0("keepBaselineMask", mask)
          insertUI(
            selector = "#masksBl",
            where = "beforeEnd",
            ui = tags$div(
              id = maskName,
              sliderInput(
                inputId = maskName,
                label = NULL,
                min = dlRange[1],
                max = dlRange[2],
                value = ll[[maskName]],
                step = signif(diff(dlRange) / nsteps, 3),
                sep = ""
              )
            )
          )
          masksBaseline <<- unique(c(masksBaseline, maskName))
        }
      }
      # if(nMasks != input$nMasksDl)
      updateNumericInput(
        session = session,
        inputId = "nMasksBaseline",
        value = nMasks
      )
    }
  })
)

## Manage masksBaseline ####
observeEvent(
  input$nMasksBaseline,
  isolate({
    nsteps <- min(length(Inputs$delayOrig), 500)
    dlRange <- signif(range(Inputs$delayOrig / Inputs$dlScaleFacOrig), 3)
    
    if (input$nMasksBaseline != 0) {
      # Add new slider(s) if required
      for (mask in 1:input$nMasksBaseline) {
        maskName <- paste0("keepBaselineMask", mask)
        if (!(maskName %in% masksBaseline)) {
          insertUI(
            selector = "#masksBl",
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
          masksBaseline <<- unique(c(masksBaseline, maskName))
        }
      }
    }
    # Remove extra sliders
    for (mask in (input$nMasksBaseline + 1):15) {
      maskName <- paste0("keepBaselineMask", mask)
      if (maskName %in% masksBaseline) {
        removeUI(
          selector = paste0("#", maskName),
          immediate = TRUE
        )
        masksBaseline <<- 
          masksBaseline[-which(masksBaseline == maskName)]
      }
    }
  })
)

## AutoBaselineMAsk
observeEvent(
  input$autoBaselineMask,
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
        maskName <- paste0("keepBaselineMask", mask)
        if (maskName %in% masksBaseline) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksBaseline <<- 
            masksBaseline[-which(masksBaseline == maskName)]
        }
      }
      
      # Generate sliders
      nsteps <- min(length(Inputs$delayOrig), 500)
      dlRange <- signif(range(Inputs$delayOrig / Inputs$dlScaleFacOrig), 3)
      for (mask in 1:nmat) {
        maskName <- paste0("keepBaselineMask", mask)
        sel <- c(chgp[2 * (mask - 1) + 1], chgp[2 * (mask - 1) + 2])
        value <- Inputs$delayOrig[sel] / Inputs$dlScaleFacOrig
        insertUI(
          selector = "#masksBl",
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
        masksBaseline <<- unique(c(masksBaseline, maskName))
      }
    }
    
    if (nmat != input$nMasksBaseline) {
      updateNumericInput(
        session = session,
        inputId = "nMasksBaseline",
        value = nmat
      )
    }
  })
)

## selectArea ####

### Temper excessive reactivity of sliders
## TBD: add a control to let the user choose debounceDelay ?

dlRange = reactive({input$keepDlRange}) %>% debounce(debounceDelay)
wlRange = reactive({input$keepWlRange}) %>% debounce(debounceDelay)
doRange = reactive({input$keepDoRange}) %>% debounce(debounceDelay)
dlCut   = reactive({input$keepDlCut})   %>% debounce(debounceDelay)
wlCut   = reactive({input$keepWlCut})   %>% debounce(debounceDelay)

Masks <- reactiveValues()
observeEvent(
  input$nMasksBaseline,
    if (input$nMasksBaseline != 0)
      for (mask in 1:input$nMasksBaseline) {
        f = function() 
          NULL
        body(f) = substitute(
          input[[mName]],
          list(
            mName = paste0("keepBaselineMask", mask)
          )
        )
        Masks[[paste0('blm',mask)]] = debounce(f, debounceDelay)
      }
)
observeEvent(
  input$nMasksDl,
  if (input$nMasksDl != 0)
    for (mask in 1:input$nMasksDl) {
      f = function() 
        NULL
      body(f) = substitute(
        input[[mName]],
        list(
          mName = paste0("keepDlMask", mask)
        )
      )
      Masks[[paste0('dlm',mask)]] = debounce(f, debounceDelay)
    }
)
observeEvent(
  input$nMasksWl,
  if (input$nMasksWl != 0)
    for (mask in 1:input$nMasksWl) {
      f = function() 
        NULL
      body(f) = substitute(
        input[[mName]],
        list(
          mName = paste0("keepWlMask", mask)
        )
      )
      Masks[[paste0('wlm',mask)]] = debounce(f, debounceDelay)
    }
)

selectArea <- reactive({
  if (!checkInputsSanity()) {
    return(NULL)
  }

  delay <- Inputs$delayOrig
  wavl <- Inputs$wavlOrig
  mat <- Inputs$matOrig
  delayId <- Inputs$delayIdOrig
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

  delay   <- delay[subX]
  delayId <- delayId[subX]
  wavl    <- wavl[subY]
  mat     <- mat[subX, subY]
  delaySave <- delaySave[subX]

  Inputs$delay <<- delay
  Inputs$delayId <<- delayId
  Inputs$delaySave <<- delaySave
  Inputs$wavl <<- wavl

  # Aggregate and apply masks
  delayMask <- rep(0, length(delay))
  if (input$nMasksDl != 0) {
    for (mask in 1:input$nMasksDl) {
      xlim <- Masks[[paste0('dlm', mask)]]() * Inputs$dlScaleFacOrig
      if (length(xlim) != 0) {
        if (diff(xlim) != 0) {
          sel <- delay >= xlim[1] & delay <= xlim[2]
          if (sum(sel) != 0) delayMask[sel] <- NA
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
          if (sum(sel) != 0) wavlMask[sel] <- NA
        }
      }
    }
  }
  
  if (!anyNA(Inputs$delayGlitch)) {
    for (i in 1:length(Inputs$delayGlitch))
      delayMask[which(delay == Inputs$delayGlitch[i])] <- NA
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
  
  Inputs$baselineMask <<- baselineMask
  Inputs$delayMask <<- delayMask
  Inputs$wavlMask  <<- wavlMask

  mat[is.na(delayMask), ] <- NA
  mat[, is.na(wavlMask)]  <- NA

  Inputs$mat <<- mat

  # Automatic ajustment of DO range
  updateSlider(
    "keepDoRange",
    # signif(range(Inputs$matOrig, na.rm = TRUE), 3),
    signif(range(mat, na.rm = TRUE), 3),
    signif(range(mat, na.rm = TRUE), 3),
    200
  )
})

reshapeCSOld <- function(U, V, n) {
  # Expand vectors wrt masks
  C <- matrix(NA, nrow = length(Inputs$delay), ncol = n)
  colnames(C) <- colnames(U)
  S <- matrix(NA, nrow = length(Inputs$wavl), ncol = n)
  colnames(S) <- colnames(V)
  i <- 0
  for (j in 1:nrow(C)) {
    if (!is.na(Inputs$delayMask[j])) {
      i <- i + 1
      C[j, ] <- U[i, 1:n]
    }
  }
  i <- 0
  for (j in 1:nrow(S)) {
    if (!is.na(Inputs$wavlMask[j])) {
      i <- i + 1
      S[j, ] <- V[i, 1:n]
    }
  }
  return(list(C = C, S = S))
}

reshapeCS <- function(U, V, n = NULL) {
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
}

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
        if (!(maskName %in% masksDl)) {
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
          masksDl <<- unique(c(masksDl, maskName))
        }
      }
    }
    # Remove extra sliders
    for (mask in (input$nMasksDl + 1):15) {
      maskName <- paste0("keepDlMask", mask)
      if (maskName %in% masksDl) {
        removeUI(
          selector = paste0("#", maskName),
          immediate = TRUE
        )
        masksDl <<- masksDl[-which(masksDl == maskName)]
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
        if (maskName %in% masksDl) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksDl <<- masksDl[-which(masksDl == maskName)]
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
        masksDl <<- unique(c(masksDl, maskName))
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
        if (!(maskName %in% masksWl)) {
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
          masksWl <<- unique(c(masksWl, maskName))
        }
      }
    }
    # Remove extra sliders
    for (mask in (input$nMasksWl + 1):15) {
      maskName <- paste0("keepWlMask", mask)
      if (maskName %in% masksWl) {
        removeUI(
          selector = paste0("#", maskName),
          immediate = TRUE
        )
        masksWl <<- masksWl[-which(masksWl == maskName)]
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
        if (maskName %in% masksWl) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksWl <<- masksWl[-which(masksWl == maskName)]
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
        masksWl <<- unique(c(masksWl, maskName))
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
colorizeBaselineMask <- function(ylim = NULL, eps = 1e-4) {
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
}

colorizeMask1D <- function(axis = "delay", dir = "v",
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
}

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
