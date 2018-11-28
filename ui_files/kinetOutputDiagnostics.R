tabsetPanel(
  tabPanel(
    value="kinResid1_4",
    title=h5("Lack-of-fit"), br(),
    withSpinner(
      plotOutput("kinResid4", height=550),
      type=4
    )
  ),
  tabPanel(
    value="kinResid1_3",
    title=h5("Integ. kinet."), br(),
    withSpinner(
      plotOutput("kinResid3", height=550),
      type=4
    )
  ),
  tabPanel(
    value="kinResid1_5",
    title=h5("Data vs. Model"), br(),
    withSpinner(
      plotOutput("kinResid5", height=550) ,
      type=4
    ),
    wellPanel(
      fluidRow(
        column(4,
               checkboxInput("kinContours",
                             label = "Add contours",
                             value = FALSE),
               offset = 8
        )
      )
    )
  ),
  tabPanel(
    value="kinResid1_1",
    title=h5("Residuals"), br(),
    withSpinner(
      plotOutput("kinResid1", height=550) ,
      type=4
    )
  ),
  tabPanel(
    value="kinResid1_2",
    title=h5("SVD of Residuals"), br(),
    withSpinner(
      plotOutput("kinResid2", height=550),
      type=4
    )
  ),
  id="kinResidIn" 
)
