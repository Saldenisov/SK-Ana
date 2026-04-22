tabsetPanel(
  
  tabPanel(
    title=h5("Residuals"), 
    br(),
    # withSpinner(
      plotOutput("alsResid3", height=550),
    #   type=4
    # ),
    value="alsResid1_3"
  ),
  
  tabPanel(
    title=h5("SVD of Residuals"), 
    br(),
    # withSpinner(
      plotOutput("alsResid2", height=550),
    #   type=4
    # ),
    value="alsResid1_2"
  ),
  
  tabPanel(
    title=h5("Data vs. Model"), 
    br(),
    wellPanel(
      fluidRow(
        column(
          6,
          uiOutput("alsDataModDelayUI")
        ),
        column(
          3,
          checkboxInput(
            "alsContours",
            label = "Add contours",
            value = FALSE)
        ),
        column(
          3,
          actionButton(
            "alsDataModSave",
            "Save",
            icon = icon('save'))
        )
      )
    ),
    # withSpinner(
    plotOutput("alsResid1", height=550),
    #   type=4
    # ),
    value="alsResid1_1"
  ),
  id="alsResid1" 
)
