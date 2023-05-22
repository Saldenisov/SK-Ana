sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    h5("Author      : P. Pernot"),
    h5("Affiliation : CNRS"),
    h5(paste0("Version     : ", Version)),
    h5(paste0("Date        : ", DateVersion)),
    hr( style="border-color: #666;"),
    a(href="https://github.com/ppernot/SK-Ana","How to cite..."),
    br(),
    a(href="https://github.com/ppernot/SK-Ana","code@github"),
    br(),
    a(href="https://github.com/ppernot/SK-Ana/issues",
      "Bugs report, Features request"),
    br(),
    a(href="https://ppernot.github.io/SK-Ana/","Users Manual")
  ),
  mainPanel(
    width = mainWidth,
    wellPanel(
      h4("Warning: on some systems, the app. may crash"),
      h4("when you click on the Save or Download buttons."),
      h4("To avoid this, press the Ctrl key while clicking"),
      h4("(Ctrl+Click).")
    )
  )
)
