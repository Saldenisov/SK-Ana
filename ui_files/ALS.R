sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    source_ui("ALSInputRun.R"),
    fluidRow(
      column(
        width = 8,
        sliderInput(
          "alsThresh", 
          "Log convergence threshold",
          min   =  -10, 
          max   =   -2, 
          value =   -6,
          sep   =   ""
        )
      )
    ),
    br(),
    tabsetPanel(
      tabPanel(
        title=h4("Options"),
        br(),
        source_ui("ALSInputOptions.R"),
        value='ALSparams'
      ),
      tabPanel(
        title=h4("C const."),
        br(),
        source_ui("ALSInputConstraintsKinet.R"),
        value='ALSCconstraints'
      ),
      tabPanel(
        title=h4("S const."),
        br(),
        source_ui("ALSInputConstraintsSpectra.R"),
        value='ALSSconstraints'
      )
    )
  ),
  mainPanel(
    width = mainWidth,
    wellPanel(
      tabsetPanel(
        tabPanel(
          value="alsOptTab",
          title=h4("Alternated Least Squares"),
          style = "overflow-y:scroll; max-height: 600px",
          br(),
          lapply(1:10, function(i) {
            uiOutput(paste0('iter', i))
          }),
          htmlOutput("alsOpt")
        ),
        tabPanel(
          title=h4("Diagnostics"),
          br(),
          source_ui("ALSOutputDiagnostics.R"),
          value="alsResid"
        ),
        tabPanel(
          title=h4("Kinetics & Spectra"),
          br(),
          source_ui("ALSOutputVectors.R"),
          value="alsVectorsTab"
        ),
        tabPanel(
          title=h4("Contributions"),
          br(),
          # withSpinner(
            plotOutput("alsContribs", height=550),
          #   type=4
          # ),
          value="alsContribTab"
        ),
        tabPanel(
          title=h4("Ambiguity"),
          br(),
          source_ui("ALSOutputAmbiguity.R"),
          value="alsRotAmbTab"
        ),
        id="alsTabset"
      )
    )
  )
)
