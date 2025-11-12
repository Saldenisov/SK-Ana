# Kinetics Smoothing & Guidelines Options
# =======================================
# Savitzky-Golay filter options for smoothing kinetics curves

wellPanel(
  h4("Kinetics Smoothing Options"),
  hr(),
  
  fluidRow(
    column(
      6,
      checkboxInput(
        "kinDisplaySmoothed",
        label = HTML("Show smoothed kinetics <span style='font-size: 0.9em; color: #888;'>(as guidelines)</span>"),
        value = FALSE
      ),
      helpText(
        "Enable to display Savitzky-Golay smoothed kinetics as guidelines.",
        "Useful for identifying trends and removing noise."
      )
    ),
    column(
      6,
      checkboxInput(
        "kinDisplayBoth",
        label = "Show both raw & smoothed",
        value = FALSE
      ),
      helpText(
        "Display original kinetics and smoothed version together for comparison."
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.kinDisplaySmoothed",
    hr(),
    
    fluidRow(
      column(
        6,
        numericInput(
          "kinSGWindow",
          label = "Window length (data points)",
          value = 5,
          min = 3,
          max = 51,
          step = 2
        ),
        helpText(
          "Savitzky-Golay window size.",
          "Odd number required. Larger = more smoothing.",
          "Min: 3, Max: 51"
        )
      ),
      column(
        6,
        numericInput(
          "kinSGOrder",
          label = "Polynomial order",
          value = 2,
          min = 1,
          max = 6,
          step = 1
        ),
        helpText(
          "Polynomial degree for SG filter.",
          "Higher = follows data better.",
          "Must be < window length"
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        sliderInput(
          "kinSmoothAlpha",
          label = "Smoothed line transparency",
          min = 0,
          max = 1,
          value = 0.7,
          step = 0.1
        ),
        helpText(
          "Alpha value for smoothed curve (0=transparent, 1=opaque)"
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        div(
          style = "background: #f0f0f0; padding: 10px; border-left: 4px solid #0066cc; margin: 10px 0;",
          h6("Savitzky-Golay Filter Tips:"),
          tags$ul(
            tags$li("Window length should be ~5-15% of data points"),
            tags$li("Polynomial order usually 2-3 for smooth curves"),
            tags$li("Larger window = stronger smoothing"),
            tags$li("Higher order = more detail preservation")
          )
        )
      )
    )
  )
)
