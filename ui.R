function(request) {
  source_ui <- function(...) {
    source(
      file.path("ui_files", ...),
      local = TRUE
    )$value
  }

  navbarPage(
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
