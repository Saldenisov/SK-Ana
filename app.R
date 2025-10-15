# Entry point for Railway deployment
source("global.R")

# Get port from environment (Railway provides this)
port <- as.integer(Sys.getenv("PORT", "3000"))
host <- "0.0.0.0"

message(sprintf("Starting SK-Ana on %s:%d", host, port))

# Source UI and Server
ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

# Create and run the Shiny app
shinyApp(ui = ui, server = server)