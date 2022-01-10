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
    # withSpinner(
    plotOutput("alsResid1", height=550),
    #   type=4
    # ),
    value="alsResid1_1"
  ),
  id="alsResid1" 
)
