wellPanel(
  column(
    width = 4,
    checkboxInput(
      "nonnegS", 
      label= "S > 0",
      value = TRUE
    ),
    checkboxInput(
      "uniS", 
      label= "S Unimodal",
      value = FALSE
    ),
    checkboxInput(
      "normS", 
      label= "Normalize",
      value = TRUE
    ),
    shinyBS::bsTooltip(
      "normS",
      title = "Enforces the normalization of S ( default: max(S)=1 )"
    )
  ),
  column(
    width = 4,
    numericInput(
      "smooth", 
      label = "Smooth", 
      value =    0, 
      min   =    0, 
      max   =    1, 
      step  =  0.1,
      width = '100px'
    ),
    checkboxInput(
      "SumS", 
      label= "SUM(S)=1",
      value = FALSE
    ),
    shinyBS::bsTooltip(
      "SumS", 
      title = "If Normalize is set, set norm such as sum(S)=1 "
    )
  ),
  fluidRow(
    column(
      width = 12,
      hr( style="border-color: #666;"),
      checkboxInput(
        "shapeS", 
        label= "External spectrum shape(s)",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.shapeS",
        fileInput(
          inputId = 'S0File',
          label   = 'Select file(s)',
          multiple= TRUE,
          accept  = c('.dat','.txt','.csv')
        ),
        checkboxInput(
          "softS0", 
          label= "Soft constraint",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.softS0",
          sliderInput(
            "wSoftS0", 
            "logWeight for Soft constraint",
            min   =  -3, 
            max   =   3, 
            value =   1,
            step  = 0.5,
            sep   = ""
          )
        )
      )
    )
  )
)

