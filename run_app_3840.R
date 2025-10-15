# Auto-generated run script to launch Shiny app on port 3840
try({
  setwd('C:/dev/SK-Ana')
  options(shiny.port = 3840, shiny.host = '127.0.0.1')
  if (!requireNamespace('shiny', quietly = TRUE)) {
    message('shiny package not found. Exiting with status 11.')
    quit(status = 11)
  }
  message(sprintf('Launching Shiny app in %s on http://%s:%s', getwd(), getOption('shiny.host'), getOption('shiny.port')))
  shiny::runApp('.', launch.browser = TRUE)
}, silent = FALSE)

