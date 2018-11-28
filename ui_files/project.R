sidebarLayout(
  sidebarPanel(
    width = sideWidth,
    tabsetPanel(
      tabPanel(
        title =h4("New Project"),
        br(),
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
            "CSV"     = "csvStyle",
            "ELYSE"   = 'elyseStyle',
            "Fluo"    = 'heleneStyle',
            "Münich"  = 'munichStyle',
            "Streak"  = 'streakStyle',
            "Other..."= 'otherStyle'
          ), 
          selected = 'csvStyle', 
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
        strong('Load-time compression factors'),
        br(),br(),
        fluidRow(
          # Warning: permutation of the meanings for
          # compFacW et compFacD, to compensate for
          # default choice of matrix structure (wxd)
          column(3,
                 numericInput(
                   inputId = 'compFacW', 
                   label   = 'Delay', 
                   value   = 1, min=1, max=20, step=1,
                   width   = '100px')
          ),
          column(3,
                 numericInput(
                   inputId = 'compFacD', 
                   label   = 'Wavl', 
                   value   = 1, min=1, max=20, step=1,
                   width   = '100px')
          )
        ),
        hr( style="border-color: #666;"),
        fileInput(
          inputId = 'dataFile',
          label   = 'Select data file(s)',
          multiple= TRUE,
          accept  = c('.dat','.txt','.csv')
        ),
        hr( style="border-color: #666;"),
        strong('Post-process compression factor'),
        br(),br(),
        fluidRow(
          column(3,
                 numericInput(
                   inputId = 'postCompFacD', 
                   label   = 'Delay', 
                   value   = 1, min=1, max=20, step=1,
                   width   = '100px')
          ),
          column(3,
                 numericInput(
                   inputId = 'postCompFacW', 
                   label   = 'Wavl', 
                   value   = 1, min=1, max=20, step=1,
                   width   = '100px')
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
      column(4,
             conditionalPanel(
               condition = "output.showPIN",
               wellPanel(style = "background-color: #ffffff;", 
                         uiOutput("projectInfoNew")
               )
             )
      ),
      column(6,
             conditionalPanel(
               condition = "output.showPIN",
               wellPanel(style = "background-color: #ffffff;", 
                         plotOutput("vignette", height = 250, width=400)
               )
             )
             
      )
    )
    
  )
) 
