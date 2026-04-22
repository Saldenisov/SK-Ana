# Server-side logic for Correction Spectra ALS
# =============================================
# This extension module integrates correction spectra functionality
# into the existing ALS workflow by routing the run logic to either
# the standard als() or als_Coupled() function based on user settings.

# Override: Modify the ALS run logic to support correction spectra
# This should be sourced AFTER ALS.R to override the doALS observer

# Store original doALS observer ID
originalDoALS <- NULL
doALSOriginalDisabled <- FALSE
if (exists("doALS", inherits = FALSE)) {
  originalDoALS <- doALS
  # Avoid double-triggering on input$runALS by disabling the original observer.
  try({
    if (!is.null(originalDoALS$destroy) && is.function(originalDoALS$destroy)) {
      originalDoALS$destroy()
      doALSOriginalDisabled <- TRUE
    }
  }, silent = TRUE)
}

# Enhanced ALS Run with Correction Spectra Support ####
doALS_Enhanced <- observeEvent(
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

    # Check if correction spectra are enabled
    useCorrectionSpectra <- isolate(input$useCorrectionSpectra %||% FALSE)

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

    nFixed <- 0
    S0 <- selectedExternalSpectraALS()
    
    # Calculate nFixed from S0 if correction spectra are enabled
    if (useCorrectionSpectra) {
      nFixed <- als_fixed_spectra_count(softS0 = FALSE)
    }
     
    nStart <- als_nstart(nAls)
    init_values <- als_initial_factors(input$initALS, nStart, mat)
    if (is.null(init_values)) {
      nclicks(0)
      return(NULL)
    }
    S <- init_values$S
    C <- init_values$C

    normMode <- als_norm_mode()
    
    # Get lambda for correction spectra (if enabled)
    lambdaCorr <- 0
    if (useCorrectionSpectra && !is.null(input$lambdaCorrectionSpectra)) {
      lambdaCorr <- 10^input$lambdaCorrectionSpectra
    }
    
    als_clear_live_snapshot()

    # Determine which function to call
    usingCoupledALS <- useCorrectionSpectra && nFixed > 0
    alsFun <- if (usingCoupledALS) {
      # Load correction spectra functions
      source("server_files/ALS_CorrectionSpectra.R")
      als_Coupled
    } else {
      als
    }

    alsArgs <- list(
      delay, delayId, wavl, mat, nullC, S, C,
      nAls    = nAls,
      nStart  = nStart,
      S0      = S0,
      maxiter = input$maxiter,
      uniS    = input$uniS,
      nonnegS = input$nonnegS,
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
      update_interval = 10
    )

    if (usingCoupledALS) {
      alsArgs$nFixed <- nFixed
      alsArgs$useCorrectionSpectra <- useCorrectionSpectra
      alsArgs$lambdaCorr <- lambdaCorr
    }

    launch <- launch_background_job(
      alsFun,
      args = alsArgs,
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

# Add output for extSpectraALS availability (to show correction spectra UI) ####
output$extSpectraALS_Ready <- reactive({
  length(externalSpectraALS()) > 0
})
outputOptions(output, "extSpectraALS_Ready", suspendWhenHidden = FALSE)
