sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    tabsetPanel(
      tabPanel(
        title =h4("New Project"),
        br(),
        # shinyCopy2clipboard::use_copy(),
        textInput(
          inputId = 'projectTag', 
          label   = 'Project Name', 
          value   = "",
          placeholder = "Please type project's name"
        ),
        hr( style="border-color: #666;"),
        selectInput(
          inputId='style', 
          label = 'Predefined File Formats', 
          choices = list(
            "Auto"    = "autoStyle",
            "ELYSE (TSV)" = 'elyseStyle',
            "CSV"     = "csvStyle",
            "Fluo"    = 'heleneStyle',
            # "Münich"  = 'munichStyle',
            "Streak"  = 'streakStyle',
            "Other..."= 'otherStyle'
          ), 
          selected = 'elyseStyle',
          multiple = FALSE,
          selectize = FALSE, 
          width = NULL, 
          size = NULL),
        conditionalPanel(
          condition = "input.style == 'otherStyle'",
          hr( style="border-color: #FFF;"),
          checkboxInput(
            inputId = 'header', 
            label   = 'Header', 
            value   = FALSE),
          radioButtons(
            inputId = 'sep', 
            label   = 'Separator',
            choices = c(Comma = ',',
                        Semicolon = ';',
                        Tab = '\t',
                        Space = ' '),
            selected = '\t',
            inline = TRUE),
          radioButtons(
            inputId = 'dec', 
            label   = 'Decimal',
            choices = c(Comma=',',
                        Dot='.'),
            selected = '.',
            inline = TRUE),
          radioButtons(
            inputId = 'datStr', 
            label   = 'Data structure',
            choices = list("Wavl on a row"   = 'dxw',
                           "Wavl on a column"= 'wxd'),
            selected= 'wxd',
            inline = TRUE)
        ),
        hr( style="border-color: #666;"),
        fluidRow(
          column(
            12,
            radioButtons(
              inputId = 'transformDelay',
              label   = 'Transform delay (for single delay scale)',
              choices = c('No'    = 0, 
                          'Index' = 1,
                          'Log10' = 2),
              inline  = TRUE 
            )
          )
        ),
        hr( style="border-color: #666;"),
        fileInput(
          inputId = 'dataFile',
          label   = 'Select data file(s)',
          multiple= TRUE,
          accept  = c('.dat','.txt','.csv')
        ),
        fluidRow(
          column(
            6,
            checkboxInput(
              inputId = 'appendFiles',
              label   = 'Append to existing files',
              value   = FALSE
            )
          ),
          column(
            6,
            actionButton(
              inputId = 'clearFiles',
              label   = 'Clear All Files',
              icon    = icon('trash')
            )
          )
        ),
        hr( style="border-color: #666;"),
        div(
          title = "Reduce data size during file loading by averaging neighboring points",
          style = "cursor: help;",
          fluidRow(
            column(
              12,
              tags$strong("Load-time compression"),
              tags$small(" (reduces data during import)")
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                inputId = 'compFacW',
                label   = 'Delay',
                value   = 1, min=1, max=20, step=1,
                width   = '80px'
              )
            ),
            column(
              6,
              numericInput(
                inputId = 'compFacD',
                label   = 'Wavl',
                value   = 1, min=1, max=20, step=1,
                width   = '80px'
              )
            )
          )
        ),
        br(),
        div(
          title = "Further reduce data size after combining/processing multiple files",
          style = "cursor: help;",
          fluidRow(
            column(
              12,
              tags$strong("Post-process compression"),
              tags$small(" (after file combination)")
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                inputId = 'postCompFacD',
                label   = 'Delay',
                value   = 1, min=1, max=20, step=1,
                width   = '80px'
              )
            ),
            column(
              6,
              numericInput(
                inputId = 'postCompFacW',
                label   = 'Wavl',
                value   = 1, min=1, max=20, step=1,
                width   = '80px'
              )
            )
          )
        )
      ),
      tabPanel(
        title =h4("Open"),
        h4("Select Existing Project (*.Rda)"),
        hr( style="border-color: #666;"),
        fileInput(
          inputId = 'projectFile',
          label   = 'Select Project',
          multiple= FALSE,
          accept  = c('.Rda')
        ),
        br(),
        uiOutput("loadErrorOpen"),
        br(),
        verbatimTextOutput("projectInfoOpen")
      ),
      tabPanel(
        title =h4("Save"),
        sidebarLayout(
          sidebarPanel(
            h4("Save Project"),
            hr( style="border-color: #666;"),
            downloadButton('saveProject','Save (Ctrl+Click)')
          ),
          mainPanel(
          )
        )
      )
    )
  ),
  mainPanel(
    width = mainWidth,
    uiOutput("loadMsg"),
    conditionalPanel(
      condition = "output.rawData !== null",
      DT::dataTableOutput('rawData')
    ),
    conditionalPanel(
      condition = "output.showsel",
      verbatimTextOutput('sel')
    ),
    br(),
    uiOutput("ui"),
    br(),
    fluidRow(
      column(
        4,
        conditionalPanel(
          condition = "output.showPIN",
          wellPanel(
            style = "background-color: #ffffff;",
            uiOutput("projectInfoNew")
          ),
          actionButton(
            "dataSave", "Save Matrix",
            icon = icon('save')
          ),
          tags$style(
            type = 'text/css',
            "#dataSave { width:40%; margin-top: 30px;}"
          )
        )
      ),
      column(
        6,
        conditionalPanel(
          condition = "output.showPIN",
          wellPanel(
            style = "background-color: #ffffff;",
            plotOutput("vignette", height = 250, width=400)
          )
        )
      )
    )
  )
) 
