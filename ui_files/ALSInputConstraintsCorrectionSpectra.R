# Conditional panel: shown only when S0 (fixed spectra) are loaded
conditionalPanel(
  condition = "output.extSpectraALS && output.extSpectraALS != ''",
  wellPanel(
    h5(
      span("Correction Spectra", style = "color: #0066cc; font-weight: bold;"),
      span(" (paired with fixed spectra)", style = "color: #666; font-style: italic;")
    ),
    br(),
    checkboxInput(
      "useCorrectionSpectra",
      label = HTML(
        "Enable correction spectra <span style='font-size: 0.9em; color: #888;'>(fixed + correction pairs)</span>"
      ),
      value = FALSE
    ),
    shinyBS::bsTooltip(
      "useCorrectionSpectra",
      title = paste(
        "When checked, ALS will create k = 2n+a dimensions:",
        "n fixed spectra (shape-constrained),",
        "n correction spectra (orthogonal to fixed, small magnitude),",
        "and a free spectra.",
        sep = "<br/>"
      ),
      placement = "right"
    ),
    
    # Show additional options only when correction spectra are enabled
    conditionalPanel(
      condition = "input.useCorrectionSpectra",
      
      hr(style = "border-color: #ddd;"),
      
      fluidRow(
        column(
          width = 6,
          sliderInput(
            "lambdaCorrectionSpectra",
            label = "Correction penalty (λ)",
            min = -4,
            max = 0,
            value = -2,
            step = 0.5,
            sep = "",
            ticks = FALSE
          ),
          shinyBS::bsTooltip(
            "lambdaCorrectionSpectra",
            title = paste(
              "Tikhonov regularization on correction spectra.",
              "Higher value → smaller, more regularized corrections.",
              "λ = 10^(slider value)",
              sep = "<br/>"
            ),
            placement = "left"
          )
        ),
        column(
          width = 6,
          checkboxInput(
            "correctionZeroMean",
            label = "Zero-mean corrections",
            value = TRUE
          ),
          shinyBS::bsTooltip(
            "correctionZeroMean",
            title = "Enforce zero mean for correction spectra (prevents baseline-like behavior)",
            placement = "left"
          )
        )
      ),
      
      hr(style = "border-color: #ddd;"),
      
      h6("Interpretation:"),
      p(
        "• ",
        span("Fixed (S_fix):", style = "font-weight: bold;"),
        " Known spectral shape (hard-constrained, normalized)",
        br(),
        "• ",
        span("Correction (S_corr):", style = "font-weight: bold;"),
        " Spectral deviation from fixed (orthogonal, small amplitude)",
        br(),
        "• ",
        span("Free (S_free):", style = "font-weight: bold;"),
        " Additional spectral components (standard ALS)",
        br(),
        "• ",
        span("Kinetics:", style = "font-weight: bold;"),
        " Each correction has same time profile as its fixed pair",
        style = "font-size: 0.9em; color: #555; line-height: 1.6;"
      )
    )
  )
)
