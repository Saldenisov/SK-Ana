fluidRow(
  column(
    width = 6,
    uiOutput("ambSpPlotOrStatus")
  ),
  column(
    width = 6,
    uiOutput("ambKinPlotOrStatus")
  ),
  br(),
  column(
    width = 12,
    h4("Explore Rotational/Scaling Ambiguity"),
    fluidRow(
      column(
        width = 10,
        uiOutput("selAmbParams"),
        checkboxInput(
          "ambDisplayLines", 
          label = "Display Lines",
          value = FALSE)
      ),
      column(
        width = 2,
        actionButton(
          "runALSAmb",
          strong("Start"),
          icon=icon("cog")
        ),
        tags$style(
          type='text/css',
          "#runALSAmb { width:100%; margin-top: 0px;}"
        ),
        actionButton(
          "killALSAmb",
          strong("Stop"),
          icon=icon("ban")
        ),
        tags$style(
          type='text/css',
          "#killALSAmb { width:100%; margin-top: 5px;}"
        ),
        actionButton(
          "rotAmbVecSave",
          "Save",
          icon     = icon('save')
        ),
        tags$style(
          type='text/css',
          "#rotAmbVecSave { width:100%; margin-top: 5px;}"
        ),
        downloadButton(
          "rotAmbVecDownload",
          "Download"
        ),
        tags$style(
          type='text/css',
          "#rotAmbVecDownload { width:100%; margin-top: 5px;}"
        )
      )
    )
  )
)

