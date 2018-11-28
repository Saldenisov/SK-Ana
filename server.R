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
  masksDl <- c()
  masksWl <- c()

  Inputs <- reactiveValues(
    gotData = FALSE,
    process = FALSE,
    finish = FALSE,
    validData = TRUE,
    fileOrig = NULL,
    matOrig = NULL,
    wavlOrig = NULL,
    delayOrig = NULL,
    dlScaleFacOrig = NULL,
    delayMask = NA,
    wavlMask = NA,
    maskSpExp = NA,
    mat = NULL,
    wavl = NULL,
    delay = NULL,
    delaySave = NULL, # True delays used in saved kinetics
    delayId = NA, # Reference to original matrices when tiled
    delayGlitch = NA # List of glitches to mask
  )

  checkInputsSanity <- function() {
    listIn <- reactiveValuesToList(Inputs)
    nulVec <- unlist(lapply(listIn, is.null))
    noNull <- !any(nulVec)
    return(noNull)
  }

  # Load Server files ####
  files <- c(
    "getData.R",

    "sliders.R",

    "project.R",

    "selectAreaAndMasks.R",

    "SVD.R",

    "ALS.R",

    "kinetHypercubeTransfo.R",
    "kinetParsers.R",
    "kinetSpectrokineticModel.R",
    "kinetBayesian.R",
    "kinetInterface.R",

    "report.R"
  )

  for (f in files)
    source(
      file.path("server_files", f),
      local = TRUE
    )
}
