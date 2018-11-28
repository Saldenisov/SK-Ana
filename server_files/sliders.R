updateSlider <- function (inputId, range, value, nsteps) {
  # Wrapper for generic function
  updateSliderInput(
    session,
    inputId = inputId,
    min     = range[1],
    max     = range[2],
    value   = value,
    step    = signif(diff(range)/nsteps, 3)
  )
}

initSliders <- function(config=NULL) {
  
  wavl  = Inputs$wavlOrig
  delay = Inputs$delayOrig/Inputs$dlScaleFacOrig
  mat   = Inputs$matOrig
  
  # Range of sliders
  doRange = signif(range(mat,na.rm=TRUE),2)
  wlRange = signif(range(wavl),3)
  wlMask  = signif(range(wavl),3)
  wlCut   = signif(range(wavl),3)
  dlRange = signif(range(delay),3)
  dlMask  = signif(range(delay),3)
  dlCut   = signif(range(delay),3)
  cblRange= c(0,length(delay))
  
  # Values of sliders
  if(!is.null(config)) {
    # Restore from project
    doRangeSel = config$keepDoRange
    wlRangeSel = config$keepWlRange
    # wlMaskSel1 = config$keepWlMask1
    wlCutSel   = config$keepWlCut
    dlRangeSel = config$keepDlRange
    # dlMaskSel1 = config$keepDlMask1
    dlCutSel   = config$keepDlCut
    cblSel     = config$keepCbl
  } else {
    # Initialize
    doRangeSel = as.vector(quantile(mat,probs = c(0.001,0.999),
                                    na.rm = TRUE))
    wlRangeSel = wlRange
    wlCutSel   = signif(mean(wlCut),3)
    dlRangeSel = dlRange
    dlCutSel   = signif(mean(dlCut),3)
    cblSel     = cblRange[1]
  }
  
  # DO slider
  updateSlider("keepDoRange", doRange, doRangeSel, 200)
  
  # Wavelength sliders
  nsteps = min(length(wavl),200)
  updateSlider("keepWlRange", wlRange, wlRangeSel, nsteps)
  updateSlider("keepWlCut"  , wlCut  , wlCutSel  , nsteps)
  updateNumericInput(session = session,
                     inputId = "nMasksWl",
                     value   = 0)
  
  # Delay sliders
  nsteps = min(length(delay),500)
  updateSlider("keepDlRange", dlRange, dlRangeSel, nsteps)
  updateSlider("keepDlCut"  , dlCut  , dlCutSel  , nsteps)
  updateNumericInput(session = session,
                     inputId = "nMasksDl",
                     value   = 0)
  
  # Baseline correction slider
  nsteps = round(diff(cblRange)/10)
  updateSlider("keepCbl"    , cblRange, cblSel   , nsteps)
  
  # Update Reporting
  updateCheckboxGroupInput(session,
                           inputId = 'inReport',
                           selected = c('SVD'))
  # Empty glitches
  Inputs$delayGlitch  <<- NA
  
}
