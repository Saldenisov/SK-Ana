
function(request) {
  source_ui <- function(...) {
    source(
      file.path("ui_files", ...),
      local = TRUE
    )$value
  }
  
  navbarPage(
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             font-size: 24px; 
             line-height: 24px;
             opacity: 1;
             width: 25%;
             min-width: 200px;
             }",
             ".shiny-progress {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             font-size: 24px; 
             line-height: 24px;
             opacity: 1;;
             width: 25%;
             min-width: 200px;
             }")
      )
    ),
    title = "SK-Ana",
    tabPanel(
      title = "Project",
      source_ui("project.R")
    ),
    tabPanel(
      title = "Data Selection",
      source_ui("dataSelection.R")
    ),
    tabPanel(
      title = "SVD",
      source_ui("SVD.R")
    ),
    tabPanel(
      title = "ALS",
      source_ui("ALS.R")
    ),
    tabPanel(
      title = "Kinet",
      source_ui("kinet.R")
    ),
    tabPanel(
      title = "Downloads",
      source_ui("downloads.R")
    ),
    tabPanel(
      title = "About",
      source_ui("about.R")
    ),
    tabPanel(
      title = "Debug Console",
      icon = icon("terminal"),
      style = "padding: 20px;",
      fluidPage(
        h2("Debug & Diagnostics"),
        hr(),
        p(
          "This tab shows real-time logs from all processes including ALS analysis, ambiguity explorer, and system events.",
          "Use this to diagnose issues and monitor application performance.",
          style = "color: #666; font-size: 14px;"
        ),
        uiOutput("debug_console"),
        hr(),
        h4("System Information"),
        verbatimTextOutput("system_info")
      )
    ),
    theme = shinythemes::shinytheme(
      c("cosmo", "cerulean", "spacelab", "yeti")[3]
    )
  )
}

