sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    tabsetPanel(
      tabPanel(
        value = "dataSel",
        title = h4("Selection"),
        br(),
        # sliderInput(
        #   "keepCbl",
        #   "Nb pixels for Baseline Correction",
        #   min = 0,
        #   max = 1,
        #   value = 0,
        #   sep = ""
        # ),
        sliderInput(
          "keepDoRange",
          "DO Range",
          min = 0,
          max = 1,
          value = c(0, 1),
          sep = "",
          width = "400px"
        ),
        sliderInput(
          "keepWlRange",
          "Wavelength Range",
          min = 0,
          max = 1,
          value = c(0, 1),
          sep = ""
        ),
        sliderInput(
          "keepDlRange",
          "Delay Range",
          min = 0,
          max = 1,
          value = c(0, 1),
          sep = ""
        )
      ),
      tabPanel(
        value = "dataMasksBl",
        title = h4("Baseline"),
        br(),
        fluidRow(
          column(
            4,
            numericInput(
              'nMasksBaseline',
              'Nb of masks',
              width = "100px",
              value = 0,
              min = 0,
              max = 15,
              step = 1
            )
          ),
          column(4,
                 style = "margin-top: 25px;",
                 actionButton(
                   'autoBaselineMask',
                   strong("Auto"),
                   icon = icon("gear")
                 ))
        ),
        tags$div(id = "masksBl")
      ),
      tabPanel(
        value = "dataMasksS",
        title = h4("Wavl Mask"),
        br(),
        fluidRow(
          column(
            4,
            numericInput(
              'nMasksWl',
              'Nb of masks',
              width = "100px",
              value = 0,
              min = 0,
              max = 15,
              step = 1
            )
          ),
          column(4,
                 style = "margin-top: 25px;",
                 actionButton(
                   'autoWlMask',
                   strong("Auto"),
                   icon = icon("gear")
                 ))
        ),
        tags$div(id = "masksS")
      ),
      tabPanel(
        value = "dataMasksC",
        title = h4("Delay Mask"),
        br(),
        fluidRow(
          column(
            4,
            numericInput(
              'nMasksDl',
              'Nb of masks',
              width = "100px",
              value = 0,
              min = 0,
              max = 15,
              step = 1
            )
          ),
          column(4,
                 style = "margin-top: 25px;",
                 actionButton(
                   'autoDlMask',
                   strong("Auto"),
                   icon = icon("gear")
                 ))
        ),
        tags$div(id = "masksC")
      ),
      id = "selTabset"
    ),
    hr(style = "border-color: #666;"),
    fluidRow(column(
      5,
      actionButton("reset",
                   strong("Reset Selections"),
                   icon = icon("eraser"))
    ),
    column(
      7,
      actionButton('saveSelectors',
                   strong('Save Selections'),
                   icon = icon("save"))
    )),
    fluidRow(
      column(5,
             br()),
      column(
        7,
        br(),
        fileInput(
          inputId = 'selectorFile',
          label   = 'Load Saved Selections',
          multiple = FALSE,
          accept  = c('.Rda')
        )
      )
    )
  ),
  mainPanel(
    width = mainWidth,
    wellPanel(
      tabsetPanel(
        tabPanel(
          value = "dataImg",
          title = h4("Data"),
          br(),
          fluidRow(
            column(
              6,
              # withSpinner(
              plotOutput(
                "image1",
                height = 450,
                dblclick = "image1_dblclick",
                brush = brushOpts(id = "image1_brush",
                                  resetOnNew = TRUE)
              ),
              # type=4
              # ),
              br(),
              wellPanel(
                fluidRow(
                  column(
                    9,
                    sliderInput(
                      "keepWlCut",
                      "Reference wavl",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      sep = ""
                    )
                  ),
                  column(
                    3,
                    actionButton(
                      "delayCutSave", "Save",
                      icon = icon('save')
                    ),
                    tags$style(
                      type = 'text/css',
                      "#delayCutSave { width:100%; margin-top: 30px;}"
                    )
                  )
                )
              )
            ),
            column(
              6,
              # withSpinner(
              plotOutput("transects", height = 450),
              # type=4
              # ),
              br(),
              wellPanel(
                fluidRow(
                  column(
                    9,
                    sliderInput(
                      "keepDlCut",
                      "Reference delay",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      sep = ""
                    )
                  ),
                  column(
                    3,
                    actionButton(
                      "wavlCutSave", "Save",
                      icon = icon('save')
                    ),
                    tags$style(
                      type = 'text/css',
                      "#wavlCutSave { width:100%; margin-top: 30px;}"
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          value = "dataCuts",
          title = h4("Cuts"),
          br(),
          fluidRow(
            column(
              6,
              # withSpinner(
              plotOutput(
                "cutsDl",
                height = 450,
                dblclick = "cutsDl_dblclick",
                brush = brushOpts(id = "cutsDl_brush",
                                  resetOnNew = TRUE)
              ),
              # type=4
              # ),
              br(),
              wellPanel(
                fluidRow(
                  column(
                    9,
                    sliderInput(
                      "stepDlCut",
                      "Cut freq.",
                      min = 0,
                      max = 100,
                      value = 10,
                      sep = ""
                    )
                  ),
                  column(
                    3,
                    actionButton(
                      "delayStepCutSave", "Save",
                      icon = icon('save')
                    ),
                    tags$style(
                      type = 'text/css',
                      "#delayStepCutSave { width:100%; margin-top: 30px;}"
                    )
                  )
                )
              )
            ),
            column(
              6,
              # withSpinner(
              plotOutput(
                "cutsWl",
                height = 450,
                dblclick = "cutsWl_dblclick",
                brush = brushOpts(id = "cutsWl_brush",
                                  resetOnNew = TRUE)
              ),
              # type=4
              # ),
              br(),
              wellPanel(
                fluidRow(
                  column(
                    9,
                    sliderInput(
                      "stepWlCut",
                      "Cut freq.",
                      min = 0,
                      max = 100,
                      value = 10,
                      sep = ""
                    )
                  ),
                  column(
                    3,
                    actionButton(
                      "wavlStepCutSave", "Save",
                      icon = icon('save')
                    ),
                    tags$style(
                      type = 'text/css',
                      "#wavlStepCutSave { width:100%; margin-top: 30px;}"
                    )
                  )
                )
              )
            )
          )
        ),
        id = "svdTabset"
      )
    )
  )
)
