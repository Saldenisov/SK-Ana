tabsetPanel(
  tabPanel(
    value="kinPriPost",
    title=h5("Densities"), br(),
    withSpinner(
      plotOutput("kinParams", height=550) ,
      type=4
    )
  ),
  tabPanel(
    value="kinPriPostSample",
    title=h5("Sample"), br(),
    withSpinner(
      plotOutput("kinParamsSamp", width = "550px", height="550px"),
      type=4
    )
  )
)
