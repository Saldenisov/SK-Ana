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
      title = "Enforces the normalization of S"
    ),
    conditionalPanel(
      condition = "input.normS",
      radioButtons(
        "normMode",
        label = "Norm mode",
        choices = list(
          "Intensity" = "intensity",
          "L1" = "l1"
        ),
        selected = "intensity",
        inline = TRUE
      )
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
  fixedRow(
    column(
      width = 12,
      hr(style = "border-color: #666;"),
      fileInput(
        inputId = 'S0File',
        label   = 'Fix spectral shape(s)',
        multiple= TRUE,
        accept  = c('.dat','.txt','.csv')
      ),
      uiOutput('extSpectraALS'),
      hr(style = "border-color: #666;"),
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
      ),
      # Correction spectra extension
      source('ui_files/ALSInputConstraintsCorrectionSpectra.R')
    )
  )
)

