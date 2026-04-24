
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

  setInputValue <- safely(function(name, value) {
    Inputs[[name]] <- value
    invisible(value)
  }, return_on_error = NULL)

  setInputValues <- safely(function(values) {
    for (name in names(values)) {
      Inputs[[name]] <- values[[name]]
    }
    invisible(values)
  }, return_on_error = NULL)

  setProjectConfig <- safely(function(value) {
    projConfig <<- value
    invisible(value)
  }, return_on_error = NULL)

  clearProjectConfig <- safely(function() {
    setProjectConfig(NULL)
  }, return_on_error = NULL)

  projectConfigValue <- safely(function() {
    projConfig
  }, return_on_error = NULL)

  replaceRawData <- safely(function(value) {
    RawData <<- value
    invisible(value)
  }, return_on_error = NULL)

  resetDelayGlitch <- safely(function() {
    setInputValue("delayGlitch", NA)
  }, return_on_error = NULL)

  addDelayGlitch <- safely(function(values) {
    if (length(values) == 0 || all(is.na(values))) {
      return(Inputs$delayGlitch)
    }

    values <- unique(values)

    if (anyNA(Inputs$delayGlitch)) {
      updated <- values
    } else {
      updated <- unique(c(Inputs$delayGlitch, values))
    }

    setInputValue("delayGlitch", updated)
  }, return_on_error = NA)

  removeLastDelayGlitch <- safely(function() {
    current <- Inputs$delayGlitch
    if (anyNA(current) || length(current) == 0) {
      return(current)
    }

    updated <- current[-length(current)]
    if (length(updated) == 0) {
      resetDelayGlitch()
    } else {
      setInputValue("delayGlitch", updated)
    }
  }, return_on_error = NA)
  
  initInputs = safely(function() {
    setInputValues(list(
      gotData = FALSE,
      process = FALSE,
      finish = FALSE,
      validData = TRUE,
      fileOrig = NULL,
      matOrig = NULL,
      wavlOrig = NULL,
      delayOrig = NULL,
      dlScaleFacOrig = NULL,
      baselineMask = NA,
      delayMask = NA,
      wavlMask = NA,
      maskSpExp = NA,
      mat = NULL,
      wavl = NULL,
      delay = NULL,
      dataDatStr = NULL,
      delaySave = NULL, # True delays used in saved kinetics
      delayId = NA, # Pointer to original matrices
      delayGlitch = NA # List of glitches to mask
    ))
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
