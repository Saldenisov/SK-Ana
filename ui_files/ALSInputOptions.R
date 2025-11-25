wellPanel(
  fluidRow(
    column(
      width = 4,
      radioButtons(
        "initALS", 
        label = "Initialization", 
        choices = 
          list(
            "|SVD|"      = "SVD",
            "PCA"        = "PCA",
            "NMF"        = "NMF",
            "Sequential" = "seq",
            "Restart"    = "rst"), 
        inline = FALSE)
    ),
    column(
      width = 8,
      h4(""),
      checkboxInput(
        "useFiltered", 
        label = "Use SVD-filtered matrix",
        value = FALSE),
      checkboxInput(
        "optS1st", 
        label= "Opt. S first",
        value = FALSE)
    )
  )
)
