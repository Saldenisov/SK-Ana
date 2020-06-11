sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    h4("SVD parameters"),
    fluidRow(
      column(
        width = 3,
        numericInput(
          "nSV", 
          label = "Dimension", 
          value =  2, 
          min   =  1, 
          max   =  10, 
          step  =  1,
          width = '100px'
        )
      )
    ),
    hr( style="border-color: #666;"),
    h4("Glitch removal in kinetics"),
    source_ui("SVDInputGlitch.R")
  ),
  mainPanel(
    width = mainWidth,
    wellPanel(                  
      tabsetPanel(
        tabPanel(
          title=h4("Singular Values"),
          br(),
          plotOutput("svdSV", height=550),
          value="singVal"
        ),
        tabPanel(
          title=h4("Vectors"),
          br(),
          # withSpinner(
            plotOutput("svdVec", height=550),
          #   type=4
          # ),
          value="delaySV"
        ),
        tabPanel(
          title=h4("Data vs. Model"),
          br(),
          # withSpinner(
            plotOutput("svdResid", height=550),
          #   type=4
          # ),
          value="residSVD"
        ),
        tabPanel(
          title=h4("Residuals"),
          br(),
          # withSpinner(
            plotOutput("svdResid1", height=550),
          #   type=4
          # ),
          value="residSVD1"
        ),
        tabPanel(
          title=h4("Contributions"),
          br(),
          # withSpinner(
            plotOutput("svdContribs", height=550),
          #   type=4
          # ),
          value="recSVD"
        ),
        tabPanel(
          title=h4("Statistics"),
          br(),
          # withSpinner(
            DT::dataTableOutput('svdStat',width = "50%"),
          #   type=4
          # ),
          value="statSVD"
        )
      ),
      id="svdTabset"
    )
  )
)
