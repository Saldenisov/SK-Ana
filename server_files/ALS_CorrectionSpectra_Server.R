# Server-side logic for Correction Spectra ALS
# =============================================
# This extension module integrates correction spectra functionality
# into the existing ALS workflow by routing the run logic to either
# the standard als() or als_Coupled() function based on user settings.

# Override: Modify the ALS run logic to support correction spectra
# This should be sourced AFTER ALS.R to override the doALS observer

# Store original doALS observer ID
originalDoALS <- NULL

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
    
    # Suppress masked areas from coordinates
    delay <- Inputs$delay[!is.na(Inputs$delayMask)]
    delayId <- Inputs$delayId[!is.na(Inputs$delayMask)]
    wavl <- Inputs$wavl[!is.na(Inputs$wavlMask)]
    
    if (input$useFiltered) {
      # Choose SVD filtered matrix
      svdRES = doSVD()
      mat <- matrix(0, nrow = length(delay), ncol = length(wavl))
      for (ic in 1:input$nSV)
        mat <- mat + svdRES$u[, ic] %o% svdRES$v[, ic] * svdRES$d[ic]
    } else {
      mat <- Inputs$mat
      mat <- mat[!is.na(Inputs$delayMask), ]
      mat <- mat[, !is.na(Inputs$wavlMask) ]
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
    
    # External spectrum shapes
    S0 <- NULL
    nFixed <- 0
    if (length(externalSpectraALS) != 0)
      for(sp in names(externalSpectraALS)) {
        pname = paste0('fixALS_',sp)
        if( input[[pname]] )
          S0 = cbind(S0, externalSpectraALS[[sp]])
      }
    
    # Calculate nFixed from S0 if correction spectra are enabled
    if (useCorrectionSpectra && !is.null(S0)) {
      nFixed <- ncol(S0)
    }
     
    # S-C Vectors init values
    initALS = input$initALS
    if (initALS == "seq") {
      nStart = 2
    } else {
      nStart = nAls
    }

    if (initALS == "SVD" | initALS == "seq") {
      # Initialize with abs(SVD)
      if (is.null(svdRES <- doSVD()))
        return(NULL)

      S = matrix(abs(svdRES$v[, 1:nStart]), ncol = nStart)
      C = matrix(abs(svdRES$u[, 1:nStart]), ncol = nStart)

    } else if (initALS == "NMF") {
      # initialize with SVD + NMF
      if (is.null(svdRES <- doSVD()))
        return(NULL)

      # 1/ filter matrix to avoid negative values (noise)
      fMat = rep(0, nrow = nrow(data), ncol = ncol(data))
      for (i in 1:nStart)
        fMat = fMat + svdRES$u[, i] %o% svdRES$v[, i] * svdRES$d[i]
      # 2/ NMF
      res = NMFN::nnmf(
        abs(fMat),
        k = nStart,
        method = "nnmf_als",
        eps = 1e-8)
      C = res$W
      S = t(res$H)

    } else {

      RES = resALS$results
      # restart from existing solution
      if (is.null(RES))
        return(NULL)

      S <- RES$S[, 1:nStart]
      C <- RES$C[, 1:nStart]
    }
    
    # Get normMode with fallback for backward compatibility
    normMode <- if (!is.null(input$normMode)) input$normMode else "intensity"
    
    # Get lambda for correction spectra (if enabled)
    lambdaCorr <- 0
    if (useCorrectionSpectra && !is.null(input$lambdaCorrectionSpectra)) {
      lambdaCorr <- 10^input$lambdaCorrectionSpectra
    }
    
    # Remove any previous live snapshot
    try({ if (file.exists(alsStateFile)) file.remove(alsStateFile) }, silent = TRUE)

    # Determine which function to call
    alsFun <- if (useCorrectionSpectra && nFixed > 0) {
      # Load correction spectra functions
      source("server_files/ALS_CorrectionSpectra.R")
      als_Coupled
    } else {
      als
    }

    # On Windows, Conda R often lacks the bin/x64 layout. If Rterm.exe is not found
    # under the current R_ARCH, drop R_ARCH for the child process so callr uses bin\\Rterm.exe.
    # For Docker/Linux, we don't need any special environment variables.
    if (identical(.Platform$OS.type, "windows")) {
      arch <- Sys.getenv("R_ARCH", "")
      rterm_candidate <- file.path(R.home("bin"), arch, "Rterm.exe")
      if (!file.exists(rterm_candidate)) {
        rbgenv <- character(0)
        rbgenv["R_ARCH"] <- ""
        
        rx = callr::r_bg(
          alsFun,
          args = list(
            delay, delayId, wavl, mat, nullC, S, C,
            nAls    = nAls,
            nStart  = nStart,
            S0      = S0,
            nFixed  = nFixed,
            useCorrectionSpectra = useCorrectionSpectra,
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
            lambdaCorr = lambdaCorr,
            normMode = normMode,
            state_file = alsStateFile,
            update_interval = 10
          ),
          package = TRUE,
          stdout = alsStdOut,
          stderr = alsStdOut,
          env = rbgenv
        )
      } else {
        rx = callr::r_bg(
          alsFun,
          args = list(
            delay, delayId, wavl, mat, nullC, S, C,
            nAls    = nAls,
            nStart  = nStart,
            S0      = S0,
            nFixed  = nFixed,
            useCorrectionSpectra = useCorrectionSpectra,
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
            lambdaCorr = lambdaCorr,
            normMode = normMode,
            state_file = alsStateFile,
            update_interval = 10
          ),
          package = TRUE,
          stdout = alsStdOut,
          stderr = alsStdOut
        )
      }
    } else {
      # Linux/Docker - no special environment needed
      rx = callr::r_bg(
        alsFun,
        args = list(
          delay, delayId, wavl, mat, nullC, S, C,
          nAls    = nAls,
          nStart  = nStart,
          S0      = S0,
          nFixed  = nFixed,
          useCorrectionSpectra = useCorrectionSpectra,
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
          lambdaCorr = lambdaCorr,
          normMode = normMode,
          state_file = alsStateFile,
          update_interval = 10
        ),
        package = TRUE,
        stdout = alsStdOut,
        stderr = alsStdOut
      )
    }
    resALS$results = NULL
    resAmb$results = NULL
    obsALSStatus$resume()
    bgALSpx <<- rx
    
  }
)

# Add output for extSpectraALS availability (to show correction spectra UI) ####
output$extSpectraALS_Ready <- reactive({
  !is.null(externalSpectraALS) && length(externalSpectraALS) > 0
})
outputOptions(output, "extSpectraALS_Ready", suspendWhenHidden = FALSE)
