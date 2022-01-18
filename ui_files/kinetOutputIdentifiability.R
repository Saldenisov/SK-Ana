tabsetPanel(
  tabPanel(
    value="kinPriPost",
    title=h5("Densities"), br(),
    # withSpinner(
      plotOutput("kinPriPost", height=550),
    #   type=4
    # ),
    checkboxInput(
      'logPriPost',
      label = 'Log parameters',
      value = TRUE
    )
  ),
  tabPanel(
    value="kinPriPostSample",
    title=h5("Sample"), br(),
    # withSpinner(
      plotOutput("kinParamsSamp", width = "550px", height="550px")#,
    #   type=4
    # )
  )
)
