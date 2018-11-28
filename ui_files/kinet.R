sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    tabsetPanel(
      tabPanel(
        h4("Model"),
        source_ui("kinetInputModel.R")
      ),
      tabPanel(
        h4("Run"),
        source_ui("kinetInputRun.R")
      )
    )
  ),
  mainPanel(
    width = mainWidth,
    wellPanel(
      tabsetPanel(
        tabPanel(
          h4("Best params"),
          source_ui("kinetOutputBestParams.R")
        ),
        tabPanel(
          h4("Trace"),
          source_ui("kinetOutputTrace.R")
        ),
        tabPanel(
          h4("Identifiability"),
          br(),
          source_ui("kinetOutputIdentifiability.R")
        ),
        tabPanel(
          title=h4("Diagnostics"),
          br(),
          source_ui("kinetOutputDiagnostics.R"),
          value="kinResid"
        ),
        tabPanel(
          title=h4("Kinetics & Spectra"),
          br(),
          source_ui("kinetOutputVectors.R"),
          value="kinVectorsTab"
        ),
        tabPanel(
          title=h4("Contributions"),
          br(),
          withSpinner(
            plotOutput("kinContribs", height=550),
            type=4
          ),
          value="kinContribTab"
        )
      )
    )
  )
)

