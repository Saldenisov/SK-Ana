# Functions ####

indxCuts <- safely(function(xCut, coords, minx = 50) {
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
}, return_on_error = NULL)

autoDlMask <- safely(function(mat, nmat) {
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
    chp = cpts(ans)[2]
  } else {
    chp = cpts(ans)
  }
  
  return(chp)
}, return_on_error = NULL)

autoWlMask <- safely(function(mat, nmat) {
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
}, return_on_error = NULL)

selection_state_format_version <- 1L

buildSelectionState <- safely(function(payload, project_tag = NULL) {
  list(
    version = selection_state_format_version,
    metadata = list(
      project_tag = project_tag,
      saved_at = as.character(Sys.time())
    ),
    data = payload
  )
}, return_on_error = NULL)

writeSelectionState <- safely(function(path, state) {
  selection_state <- state
  save(selection_state, file = path)
  invisible(path)
}, return_on_error = NULL)

maskRegistry <- safely(function(axis) {
  switch(
    axis,
    wl = masksWl,
    dl = masksDl,
    baseline = masksBaseline,
    character(0)
  )
}, return_on_error = character(0))

setMaskRegistry <- safely(function(axis, values) {
  updated <- unique(values)
  switch(
    axis,
    wl = { masksWl <<- updated },
    dl = { masksDl <<- updated },
    baseline = { masksBaseline <<- updated },
    character(0)
  )
  invisible(updated)
}, return_on_error = character(0))

hasMask <- safely(function(axis, mask_name) {
  mask_name %in% maskRegistry(axis)
}, return_on_error = FALSE)

registerMask <- safely(function(axis, mask_name) {
  updated <- unique(c(maskRegistry(axis), mask_name))
  setMaskRegistry(axis, updated)
  invisible(updated)
}, return_on_error = character(0))

unregisterMask <- safely(function(axis, mask_name) {
  updated <- setdiff(maskRegistry(axis), mask_name)
  setMaskRegistry(axis, updated)
  invisible(updated)
}, return_on_error = character(0))

loadSelectionState <- safely(function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }

  # Load in an isolated environment to prevent variable overwrite in session env.
  sel_env <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = sel_env)

  if ("selection_state" %in% loaded_names &&
      exists("selection_state", envir = sel_env, inherits = FALSE)) {
    state <- get("selection_state", envir = sel_env, inherits = FALSE)
    if (is.list(state) && is.list(state$data)) {
      return(state$data)
    }
    return(NULL)
  }

  if (!("ll" %in% loaded_names) || !exists("ll", envir = sel_env, inherits = FALSE)) {
    return(NULL)
  }

  ll <- get("ll", envir = sel_env, inherits = FALSE)
  if (!is.list(ll)) {
    return(NULL)
  }
  ll
}, return_on_error = NULL)

selectionFileMatchesProject <- safely(function(saved, project_tag, mat_orig) {
  if (is.null(saved) || !is.list(saved)) return(FALSE)
  if (is.null(saved$projectTag) || !identical(saved$projectTag, project_tag)) return(FALSE)

  saved_dims <- suppressWarnings(as.integer(saved$matDims))
  current_dims <- suppressWarnings(as.integer(dim(mat_orig)))
  if (length(saved_dims) != 2 || length(current_dims) != 2) return(FALSE)

  identical(saved_dims, current_dims)
}, return_on_error = FALSE)


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
    ll$matDims    <- dim(Inputs$matOrig)
    # Selectors
    ll$keepCbl        <- input$keepCbl
    ll$keepDlRange    <- input$keepDlRange
    ll$keepWlRange    <- input$keepWlRange
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
    state <- buildSelectionState(ll, project_tag = input$projectTag)
    if (is.null(state) || is.null(writeSelectionState(file, state))) {
      showModal(modalDialog(
        title = ">>>> Save Failed <<<< ",
        "The selections file could not be saved.",
        footer = modalButton("Close"),
        easyClose = TRUE, fade = TRUE, size = "m"
      ))
      return(NULL)
    }
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
    ll <- loadSelectionState(input$selectorFile$datapath)
    if (is.null(ll)) {
      showModal(modalDialog(
        title = ">>>> Incorrect File <<<< ",
        paste0(
          "The chosen selections file is invalid ",
          "or does not contain saved selections."
        ),
        footer = modalButton("Close"),
        easyClose = TRUE, fade = TRUE, size = "m"
      ))
      return(NULL)
    }

    # Check project name and matrix dims
    if (!selectionFileMatchesProject(ll, input$projectTag, Inputs$matOrig)) {
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
      setInputValues(list(
        dlScaleFacOrig = ll$dlScaleFacOrig,
        delayGlitch = ll$delayGlitch
      ))

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
        if (hasMask("wl", maskName)) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          unregisterMask("wl", maskName)
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
          registerMask("wl", maskName)
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
        if (hasMask("dl", maskName)) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          unregisterMask("dl", maskName)
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
          registerMask("dl", maskName)
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
        if (hasMask("baseline", maskName)) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          unregisterMask("baseline", maskName)
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
          registerMask("baseline", maskName)
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
        if (!hasMask("baseline", maskName)) {
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
          registerMask("baseline", maskName)
        }
      }
    }
    # Remove extra sliders
    for (mask in (input$nMasksBaseline + 1):15) {
      maskName <- paste0("keepBaselineMask", mask)
      if (hasMask("baseline", maskName)) {
        removeUI(
          selector = paste0("#", maskName),
          immediate = TRUE
        )
        unregisterMask("baseline", maskName)
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
        if (hasMask("baseline", maskName)) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          unregisterMask("baseline", maskName)
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
        registerMask("baseline", maskName)
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
