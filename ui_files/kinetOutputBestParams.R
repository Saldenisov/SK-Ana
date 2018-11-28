wellPanel(
  fluidRow(
    column(4,
           uiOutput('kinRes')
    ),
    column(6,
           h4('Best parameters (MAP)'),
           withSpinner(
             DT::dataTableOutput('kinOpt'),
             type = 4
           )
    )
  )
)
