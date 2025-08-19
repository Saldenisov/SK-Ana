# Auto-generated run script to launch Shiny app on port 3840
try({
  setwd('C:/dev/SK-Ana')
  options(shiny.port = 3840, shiny.host = '0.0.0.0')
  if (!requireNamespace('shiny', quietly = TRUE)) {
    message('shiny package not found. Exiting with status 11.')
    quit(status = 11)
  }
  message(sprintf('Launching Shiny app in %s on port %s', getwd(), getOption('shiny.port')))
  shiny::runApp('.', launch.browser = FALSE)
}, silent = FALSE)

