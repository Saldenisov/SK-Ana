sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    h5("Creator          : Pascal PERNOT"),
    h5("Developer        : Sergey A. DENISOV"),
    h5("Affiliation      : CNRS ICP UMR8000"),
    h5(paste0("Version          : ", Version)),
    h5(paste0("Date             : ", DateVersion)),
    hr( style="border-color: #666;"),
    a(href="https://github.com/Saldenisov/SK-Ana","How to cite..."),
    br(),
    a(href="https://github.com/Saldenisov/SK-Ana","code@github"),
    br(),
    a(href="https://github.com/Saldenisov/SK-Ana/issues",
      "Bugs report, Features request"),
    br(),
    a(href="https://saldenisov.github.io/SK-Ana/","Users Manual")
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
