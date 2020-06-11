fluidRow(
  column(
    width = 6,
    # withSpinner(
      plotOutput(
        "ambSpVectors",
        height = "450px",
        dblclick = "ambSp_dblclick",
        brush = brushOpts(
          id = "ambSp_brush",
          resetOnNew = TRUE
        )
      # ),
      # type=4
    )
  ),
  column(
    width = 6,
    # withSpinner(
      plotOutput(
        "ambKinVectors",
        height = "450px",
        dblclick = "ambKin_dblclick",
        brush = brushOpts(
          id = "ambKin_brush",
          resetOnNew = TRUE
        )
      # ),
      # type=4
    )
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
        shinyBS::bsButton(
          "killALSAmb", 
          label = "Stop",
          type  = "toggle", 
          value = FALSE, 
          size  = "default", 
          icon  = icon("ban")
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
        )
      )
    )
  )
)

