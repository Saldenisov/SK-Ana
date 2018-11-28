wellPanel(
  checkboxInput(
    "nonnegC", 
    label= "C > 0",
    value = TRUE),
  checkboxInput(
    "closeC", 
    label= "Closure",
    value = FALSE),
  shinyBS::bsTooltip(
    "closeC", 
    title = "Ensures that sum(C)=1 at each delay"),
  conditionalPanel(
    condition = "input.closeC",
    sliderInput(
      "wCloseC", 
      "logWeight for Soft constraint",
      min   =  -3, 
      max   =   3, 
      value =   1,
      step  = 0.5,
      sep   = ""
    )
  ),
  uiOutput("maskSpExp_ui")
)
