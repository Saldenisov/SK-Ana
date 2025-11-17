fluidRow(
  column(6,
         # withSpinner(
           plotOutput("alsSpVectors", height=500,
                      dblclick = "alsSp_dblclick",
                      brush = brushOpts(
                        id = "alsSp_brush",
                        resetOnNew = TRUE
                      )
           # ),
           # type=4
         ),
         fluidRow(
           column(3,
                  numericInput("alsSpXmin", "X min:", value = NULL, step = 1)
           ),
           column(3,
                  numericInput("alsSpXmax", "X max:", value = NULL, step = 1)
           ),
           column(3,
                  numericInput("alsSpYmin", "Y min:", value = NULL, step = 0.01)
           ),
           column(3,
                  numericInput("alsSpYmax", "Y max:", value = NULL, step = 0.01)
           )
         )
  ),
  column(6,
         # withSpinner(
           plotOutput("alsKinVectors", height=500,
                      dblclick = "alsKin_dblclick",
                      brush = brushOpts(
                        id = "alsKin_brush",
                        resetOnNew = TRUE
                      )
           # ),
           # type=4
         ),
         fluidRow(
           column(3,
                  numericInput("alsKinXmin", "X min:", value = NULL, step = 0.1)
           ),
           column(3,
                  numericInput("alsKinXmax", "X max:", value = NULL, step = 0.1)
           ),
           column(3,
                  numericInput("alsKinYmin", "Y min:", value = NULL, step = 0.01)
           ),
           column(3,
                  numericInput("alsKinYmax", "Y max:", value = NULL, step = 0.01)
           )
         )
  ),
  br(),
  column(
    width=12,
    wellPanel(
      h4("ALS Kinetics Smoothing Options"),
      hr(),
      fluidRow(
        column(
          6,
          checkboxInput(
            "alsKinDisplaySmoothed",
            label = HTML("Show smoothed kinetics <span style='font-size: 0.9em; color: #888;'>(Savitzky-Golay)</span>"),
            value = FALSE
          ),
          helpText(
            "Enable to display Savitzky-Golay smoothed kinetics.",
            "Useful for identifying trends and removing noise."
          )
        ),
        column(
          6,
          checkboxInput(
            "alsKinDisplayBoth",
            label = "Show both raw & smoothed",
            value = FALSE
          ),
          helpText(
            "Display original kinetics and smoothed version together for comparison."
          )
        )
      ),
      conditionalPanel(
        condition = "input.alsKinDisplaySmoothed",
        hr(),
        fluidRow(
          column(
            6,
            numericInput(
              "alsKinSGWindow",
              label = "Window length (data points)",
              value = 5,
              min = 3,
              max = 51,
              step = 2
            ),
            helpText(
              "Savitzky-Golay window size.",
              "Odd number required. Larger = more smoothing.",
              "Min: 3, Max: 51"
            )
          ),
          column(
            6,
            numericInput(
              "alsKinSGOrder",
              label = "Polynomial order",
              value = 2,
              min = 1,
              max = 6,
              step = 1
            ),
            helpText(
              "Polynomial degree for SG filter.",
              "Higher = follows data better.",
              "Must be < window length"
            )
          )
        )
      )
    )
  ),
  br(),
  column(
    width=12,
    h5('Save ALS spectra and kinetics'),
    actionButton("alsSpKinSave","Save",
                 icon     = icon('save'),
                 class = "btn-primary",
                 width = "150px"),
    downloadButton("alsSpKinDownload", "Download",
                   class = "btn-success",
                   style = "margin-left: 10px;")#,
   
    # shinyCopy2clipboard::CopyButton("copybtn_ALS_Sp", 
    #                                 label = "Copy Spectra",
    #                                 icon  = icon("pause"),
    #                                 text  = "Empty",
    #                                 class = "btn-primary",
    #                                 width = "150px"),
    # 
    # shinyCopy2clipboard::CopyButton("copybtn_ALS_Kin", 
    #                                 label = "Copy Kinetics",
    #                                 icon  = icon("pause"),
    #                                 text  = "Empty",
    #                                 class = "btn-primary",
    #                                 width = "150px")
  )
)
