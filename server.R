
function(input, output, session) {
  
  # Attempt at bookmarking: failed !!!
  # setBookmarkExclude("bookmark")
  # observeEvent(input$bookmark, {
  #   session$doBookmark()
  # })
  
  # session$onSessionEnded(stopApp)
  
  # Initialize ####
  if (!dir.exists("outputDir")) {
    dir.create("outputDir", showWarnings = FALSE)
  }
  
  projConfig <- NULL
  S0_in <- NULL
  RawData <- NULL
  masksBaseline <- c()
  masksDl <- c()
  masksWl <- c()
  Inputs <- reactiveValues()
  
  initInputs = safely(function() {
    Inputs$gotData        <<- FALSE
    Inputs$process        <<- FALSE
    Inputs$finish         <<- FALSE
    Inputs$validData      <<- TRUE
    Inputs$fileOrig       <<- NULL
    Inputs$matOrig        <<- NULL
    Inputs$wavlOrig       <<- NULL
    Inputs$delayOrig      <<- NULL
    Inputs$dlScaleFacOrig <<- NULL
    Inputs$baselineMask   <<- NA
    Inputs$delayMask      <<- NA
    Inputs$wavlMask       <<- NA
    Inputs$maskSpExp      <<- NA
    Inputs$mat            <<- NULL
    Inputs$wavl           <<- NULL
    Inputs$delay          <<- NULL
    Inputs$delaySave      <<- NULL # True delays used in saved kinetics
    Inputs$delayId        <<- NA   # Pointer to original matrices 
    Inputs$delayGlitch    <<- NA   # List of glitches to mask
  }, return_on_error = NULL)
  
  initInputs()
  
  checkInputsSanity <- safely(function() {
    listIn <- reactiveValuesToList(Inputs)
    nulVec <- unlist(lapply(listIn, is.null))
    # print(listIn)
    noNull <- !any(nulVec)
    return(noNull)
  }, return_on_error = FALSE)
  
  # Load Server files ####
  files <- c(
    "helpers.R",
    "process_utils.R",
    "getData.R",
    "sliders.R",
    "project.R",
    "selectAreaAndMasks.R",
    "SVD.R",
    "ALS_plots.R",
    "ALS.R",
    "ALS_CorrectionSpectra_Server.R",  # Correction spectra extension (must be after ALS.R)
    "debug_console.R",  # Debug console and logging (must be after ALS.R)
    "kinetHypercubeTransfo.R",
    "kinetParsers.R",
    "kinetSpectrokineticModel.R",
    "kinetBayesian.R",
    "kinetInterface.R",
    "kinetics_smoothing.R",  # Kinetics smoothing with Savitzky-Golay filter
    "report.R"
  )
  
  for (f in files)
    source(
      file.path("server_files", f),
      local = TRUE
    )
}

