library(shiny)

navbarPage( 
  "SK-Ana",
  # Config ####
  theme=shinythemes::shinytheme("cerulean"),
  HTML('<style type="text/css"> 
          .progress-bar {
            background-color: #FF0000;
          }          
          .shiny-progress .progress {
            height: 10px;
          }
          hr {
           height: 2px;
           color: #123455;
           background-color: #123455;
           border: none;
          }
         </style>'), 
  
  # Project ####
  tabPanel(
    "Project",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel(
            title=h5("New"),
            wellPanel(
              h4("Define New Project"),
              hr( style="border-color: #666;"),
              # h5('Predefined File Formats'),
              selectInput(
                inputId='style', 
                label = 'Predefined File Formats', 
                choices = list(
                  "CSV"    = "csvStyle",
                  "Münich" = 'munichStyle',
                  "ELYSE"  = 'elyseStyle',
                  "Streak" = 'streakStyle',
                  "Hélène" = 'heleneStyle'
                ), 
                selected = 'csvStyle', 
                multiple = FALSE,
                selectize = FALSE, 
                width = NULL, 
                size = NULL),
              #             fluidRow(
              #               actionButton('csv',"CSV"),
              #               actionButton('munichStyle',"Münich"),
              #               actionButton('elyseStyle', "ELYSE "),
              #               actionButton('streakStyle',"Streak"),
              #               actionButton('heleneStyle',"Hélène"),
              #               align = "center" 
              #             ),
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
                selected= 'dxw',
                inline = TRUE),
              radioButtons(
                inputId = 'procMult', 
                label   = 'Multiple files processing',
                choices = list("Average"   = 'avrg',
                               "Tile Wavl" = 'tileWav',
                               "Tile Delay"= 'tileDel'),
                selected= 'avrg',
                inline = TRUE),
              numericInput(
                inputId = 'compFac', 
                label   = 'Compression factor', 
                value   = 1, min=1, max=100, step=1,
                width   = '150px'),
              hr( style="border-color: #666;"),
              fileInput(
                inputId = 'dataFile',
                label   = 'Select data file',
                multiple= TRUE,
                accept  = c('.dat','.txt','.csv')
              ),
              textInput(
                inputId = 'projectTag', 
                label   = 'Project Name', 
                value   = ""
              )
            )
          ),
          tabPanel(
            title=h5("Open"),
            wellPanel(
              h4("Select Existing Project"),
              hr( style="border-color: #666;"),
              fileInput(
                inputId = 'projectFile',
                label   = 'Select Project',
                multiple= FALSE,
                accept  = c('.ska')
              )
            )
          ),
          tabPanel(
            title=h5("Save"),
            wellPanel(
              h4("Save Project"),
              hr( style="border-color: #666;"),
              downloadButton('saveProject','Save (Ctrl+Click)')
            )
          )
        )
        
      ),
      mainPanel(
        br(),
        uiOutput("loadError"),
        br(),
        verbatimTextOutput("projectInfo")
        
      )
    )
  ),
  
  # Select Area ####
  tabPanel(
    "Data Selection",
    sidebarLayout(
      sidebarPanel(
        sliderInput("keepCbl", 
                    "Nb pixels for Baseline Correction",
                    min = 0, 
                    max = 1, 
                    value = 0,
                    sep=""
        ),
        sliderInput("keepDoRange", 
                    "DO Range",
                    min = 0, 
                    max = 1, 
                    value = c(0,1),
                    sep="",
                    width="400px"
        ),
        sliderInput("keepWlRange", 
                    "Wavelength Range",
                    min = 0, 
                    max = 1, 
                    value = c(0,1),
                    sep=""
        ),
        sliderInput("keepDlRange", 
                    "Delay Range",
                    min = 0, 
                    max = 1, 
                    value = c(0,1),
                    sep=""
        ),
        sliderInput("keepWlMask", 
                    "Wavelength Mask",
                    min = 0, 
                    max = 1, 
                    value = c(0,0),
                    sep=""
        ),
        sliderInput("keepDlMask", 
                    "Delay Mask",
                    min = 0, 
                    max = 1,
                    value = c(0,0),
                    sep=""
        ),
        actionButton("reset",
                     "Reset all")
        
      ),
      mainPanel(
        wellPanel(
          tabsetPanel(
            id="svdTabset",
            type='pills',
            tabPanel(
              value="dataImg",
              title=h4("Data"),
              br(),
              plotOutput("image1", height = 450),
              br(),
              fluidRow(
                column(4,
                       sliderInput("keepWlCut", 
                                   "Reference wavelength",
                                   min = 0, 
                                   max = 1, 
                                   value = 0.5,
                                   sep=""
                       )
                ),
                column(2,
                       actionButton("delayCutSave","Save",
                                    icon     = icon('save')),
                       tags$style(type='text/css', 
                                  "#delayCutSave { width:100%; margin-top: 30px;}")
                ),
                column(4,
                       sliderInput("keepDlCut", 
                                   "Reference delay",
                                   min = 0, 
                                   max = 1, 
                                   value = 0.5,
                                   sep=""
                       )
                ),
                column(2,
                       actionButton("wavlCutSave","Save",
                                    icon     = icon('save')),
                       tags$style(type='text/css', 
                                  "#wavlCutSave { width:100%; margin-top: 30px;}")
                )
              )
            ),
            tabPanel(
              value="dataCuts",
              title=h4("Cuts"),
              br(),
              plotOutput("cuts", height=450)
              #           ),
              #           tabPanel(
              #             value="wlCut",
              #             title=h4("Manual cuts"),
              #             br(),
              #             plotOutput("manuCut", height=450)
            )
          ) 
        )
      )
    )
  ),
  
  # SVD ####  
  tabPanel(
    "SVD",
    sidebarLayout(
      sidebarPanel(
        wellPanel( 
          h4("SVD config"),
          fluidRow(
            column(4,
                   numericInput("nSV", 
                                label = "Dimension", 
                                value =  2, 
                                min   =  1, 
                                max   =  10, 
                                step  =  1,
                                width = '100px')
            )
          )
        )
      ),
      mainPanel(
        wellPanel(                  
          tabsetPanel(
            id="svdTabset",
            type='pills',
            tabPanel(
              value="singVal",
              title=h4("Singular Values"),
              br(),
              plotOutput("svdSV", height=550)
            ),
            tabPanel(
              value="delaySV",
              title=h4("Vectors"),
              br(),
              plotOutput("svdVec", height=550)
            ),
            tabPanel(
              value="residSVD",
              title=h4("Residuals"),
              br(),
              plotOutput("svdResid", height=550)
            ),
            tabPanel(
              value="recSVD",
              title=h4("Contributions"),
              br(),
              plotOutput("svdContribs", height=550)
            )
          )
        )
      )
    )
  ),
  
  # ALS ####  
  tabPanel(
    "ALS",
    sidebarLayout(
      sidebarPanel(
        wellPanel( 
          h4("ALS config"),
          fluidRow(
            column(4,
                   numericInput("nALS", 
                                label = "Dimension", 
                                value =  2, 
                                min   =  1, 
                                max   = 10, 
                                step  =  1,
                                width = '100px')
            ),
            column(4,
                   numericInput("maxiter", 
                                label = "MaxIter", 
                                value =   100, 
                                min   =    20, 
                                max   =  1000, 
                                step  =    20,
                                width = '100px')
            ),
            column(4,
                   actionButton("runALS",strong("Run")),
                   tags$style(type='text/css', 
                              "#runALS { width:100%; margin-top: 25px;}")
            )
          ),
          fluidRow(
            h4("Options"),
            column(4,
                   radioButtons("initALS", 
                                label = "Initialization", 
                                choices = 
                                  list("SVD"        = "SVD",
                                       "Sequential" = "seq",
                                       "Restart"    = "rst"), 
                                inline = FALSE)
            ),
            column(8,
                   h4(""),
                   checkboxInput("useFiltered", 
                                 label = "Use SVD-filtered matrix",
                                 value = FALSE),
                   # checkboxInput("forcemaxiter", 
                   #               label = "Force MaxIter",
                   #               value = FALSE),
                   # tags$style(type='text/css', 
                   #            "#rforcemaxiter { width:100%; margin-top: 25px;}"),
                   checkboxInput("optS1st", 
                                 label= "Opt. S first",
                                 value = FALSE)
            )
          ),
          fluidRow(
            h4("Constraints"),
            column(4,
                   checkboxInput("nonnegS", 
                                 label= "S > 0",
                                 value = TRUE),
                   checkboxInput("uniS", 
                                 label= "S Unimodal",
                                 value = FALSE)
            ),
            column(4,
                   checkboxInput("nonnegC", 
                                 label= "C > 0",
                                 value = TRUE),
                   numericInput("smooth", 
                                label = "Smooth", 
                                value =    0, 
                                min   =    0, 
                                max   =    1, 
                                step  =  0.1,
                                width = '100px')
            )
          ),
          fluidRow(
            column(12,
                   checkboxInput("shapeS", 
                                 label= "External shape",
                                 value = FALSE),
                   fileInput(
                     inputId = 'S0File',
                     label   = 'Select spectrum file',
                     multiple= TRUE,
                     accept  = c('.dat','.txt','.csv')
                   )
            )
          )
        )
      ),
      mainPanel(
        wellPanel(
          tabsetPanel(
            id="alsTabset",
            type='pills',
            tabPanel(
              value="alsOptTab",
              title=h4("Alternated Least Squares"),
              style = "overflow-y:scroll; max-height: 600px",
              br(),
              lapply(1:10, function(i) {
                uiOutput(paste0('iter', i))
              }),
              htmlOutput("alsOpt")
            ),
            tabPanel(
              value="alsResid",
              title=h4("Residuals"),
              br(),
              plotOutput("alsResid", height=550)
            ),
            tabPanel(
              value="alsVectorsTab",
              title=h4("Spectra & Kinetics"),
              br(),
              plotOutput("alsVectors", height=450),
              wellPanel(
                fluidRow(
                  column(12,
                         h5('Save ALS spectra and kinetics'),
                         actionButton("alsSpKinSave","Save",
                                      icon     = icon('save'))
                  )
                )
              )
            ),
            tabPanel(
              value="alsContribTab",
              title=h4("Contributions"),
              br(),
              plotOutput("alsContribs", height=550)
            ),
            tabPanel(
              value="alsRotAmbTab",
              title=h4("Ambiguity"),
              br(),
              plotOutput("alsRotAmb", height=450),
              wellPanel( 
                h4("Explore Rotational/Scaling Ambiguity"),
                fluidRow(
                  column(3,
                         sliderInput("pairToRotate", 
                                     label = "Pick 2 vectors",
                                     min=1, 
                                     max=6,
                                     value = c(1,2),
                                     step  = 1,
                                     sep="")
                  ),
                  column(3,
                         sliderInput("alsRotAmbEps",
                                     label = "Positivity threshold",
                                     min=signif(-0.1),
                                     max=signif( 0.1),
                                     value = -0.01,
                                     step  = 0.01,
                                     sep="")
                  ),
                  column(3,
                         sliderInput("alsRotAmbDens",
                                     label = "Exploration Step",
                                     min=signif(0.001),
                                     max=signif(0.1),
                                     value = 0.04,
                                     step  = 0.01,
                                     sep="")
                  ),
                  column(3,
                         actionButton("runALSAmb",strong("Start")),
                         tags$style(type='text/css', 
                            "#runALSAmb { width:100%; margin-top: 25px;}")
                  ) #,
                  # column(2,
                  #        h5('Save current'),
                  #        actionButton("alsSpKinAmbSave",label="Save",
                  #                     icon = icon("save"))
                  # )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Report ####
  tabPanel(
    title="Downloads",
    sidebarPanel(
      wellPanel(
        h4("Generate Report"),
        checkboxGroupInput(
          inputId = 'inReport', 
          label   = 'Include these results', 
          choices = c(
            'SVD' = 'SVD',
            'ALS' = 'ALS'), 
          selected = c('SVD')
        ),
        # No other formats than html available on styx (install pandoc ???)
        #       radioButtons(
        #         inputId = 'format',
        #         label   = 'Document format',
        #         choices = c('html', 'pdf', 'docx'),
        #         inline  = TRUE
        #       ),
        verticalLayout(
          textInput(
            inputId = 'reportName', 
            label   = 'Report Name', 
            value   = "SK-Ana"
          ),
          downloadButton(
            outputId = 'report',
            label    = 'Download  (Ctrl+Click)'
          )
        )
      ),
      wellPanel(
        h4("Get my files"),
        h5("Download the files you saved during your session"),
        downloadButton(
          outputId = 'getMyFiles',
          label    = 'Download  (Ctrl+Click)'
        )
      )
    )
  )
  
  # END ####
)
