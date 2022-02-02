
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
    theme = shinythemes::shinytheme(
      c("cosmo", "cerulean", "spacelab", "yeti")[3]
    )
  )
}

