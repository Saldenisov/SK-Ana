fluidRow(
  column(
    width = 3,
    numericInput(
      'cleanLevel',
      'Level', 
      width="100px",
      value = 2, 
      min = 2, 
      max=10, 
      step=1
    )
  ),
  column(
    width = 3,
    style = "margin-top: 25px;",
    actionButton(
      'clean',
      strong("Clean"),
      icon = icon("gear")
    )
  ),
  column(
    width = 3,
    style = "margin-top: 25px;",
    actionButton(
      'cleanCancel',
      strong("Cancel"),
      icon = icon("eraser")
    )
  )
)
