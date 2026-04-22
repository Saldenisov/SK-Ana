# Debug Console Tab UI
# ====================
# Adds a Debug tab to the main tab panel with real-time logging

tabPanel(
  "Debug Console",
  icon = icon("terminal"),
  style = "padding: 20px;",
  
  fluidPage(
    h2("Debug & Diagnostics"),
    hr(),
    
    fluidRow(
      column(
        12,
        p(
          "This tab shows real-time logs from all processes including ALS analysis, ambiguity explorer, and system events.",
          "Use this to diagnose issues and monitor application performance.",
          style = "color: #666; font-size: 14px;"
        )
      )
    ),
    
    fluidRow(
      column(12, uiOutput("debug_console"))
    ),
    
    hr(),
    
    fluidRow(
      column(
        12,
        h4("Quick Information"),
        tableOutput("debug_info_table")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(
        12,
        h4("System Information"),
        verbatimTextOutput("system_info")
      )
    )
  )
)
