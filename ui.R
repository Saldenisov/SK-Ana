# library(shiny)
# library(DT)

navbarPage( 
  "SK-Ana",
  # Config ####
  theme=shinythemes::shinytheme(c("cosmo","cerulean","spacelab","yeti")[2]),
  
  # Project ####
  # navbarMenu(
  tabPanel(
    "Project",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          # type = "pills",
          tabPanel(
            title =h4("New Project"),
            # h4("Define New Project"),
            # hr( style="border-color: #666;"),
            br(),
            textInput(
              inputId = 'projectTag', 
              label   = 'Project Name', 
              value   = ""
            ),
            hr( style="border-color: #666;"),
            selectInput(
              inputId='style', 
              label = 'Predefined File Formats', 
              choices = list(
                "CSV"    = "csvStyle",
                "Münich" = 'munichStyle',
                "ELYSE"  = 'elyseStyle',
                "Streak" = 'streakStyle',
                "Fluo"   = 'heleneStyle'
              ), 
              selected = 'csvStyle', 
              multiple = FALSE,
              selectize = FALSE, 
              width = NULL, 
              size = NULL),
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
            numericInput(
              inputId = 'compFac', 
              label   = 'Compression factor', 
              value   = 1, min=1, max=100, step=1,
              width   = '150px'),
            hr( style="border-color: #666;"),
            fileInput(
              inputId = 'dataFile',
              label   = 'Select data file(s)',
              multiple= TRUE,
              accept  = c('.dat','.txt','.csv')
            )
          ),
          tabPanel(
            title =h4("Open"),
            h4("In construction...")
          ),
          
          #              h4("Select Existing Project (*.ska)"),
          #              hr( style="border-color: #666;"),
          #              fileInput(
          #                inputId = 'projectFile',
          #                label   = 'Select Project',
          #                multiple= FALSE,
          #                accept  = c('.ska')
          #                    )
          #                    
          #                  ),
          #                  mainPanel(
          #                    br(),
          #                    uiOutput("loadErrorOpen"),
          #                    br(),
          #                    verbatimTextOutput("projectInfoOpen")
          #                  )
          #                )
          #       ),
          tabPanel(
            title =h4("Save"),
            h4("In construction...")
          )
          #       tabPanel("Save",
          #                sidebarLayout(
          #                  sidebarPanel(
          #                    h4("Save Project"),
          #                    hr( style="border-color: #666;"),
          #                    downloadButton('saveProject','Save (Ctrl+Click)')
          #                  ),
          #                  mainPanel(
          #                  )
          #                )
          #       )
        )
        ),
        mainPanel(
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
  ),
  
  # Select Area ####
  tabPanel(
    "Data Selection",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel(
            value="dataSel",
            title=h4("Selection"),
            br(),
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
            )
          ),
          tabPanel(
            value="dataMasksS",
            title=h4("Wavl Masks"),
            br(),
            fluidRow(
              column(4,
                     numericInput('nMasksWl','Nb of masks', width="100px",
                                  value = 0, min = 0, max=15, step=1)),
              column(4,
                     actionButton('autoWlMask',
                                  strong("Auto"),
                                  icon = icon("gear") ))
            ),
            # uiOutput("masksS")
            tags$div(id = "masksS")
          ),
          tabPanel(
            value="dataMasksC",
            title=h4("Delay Masks"),
            br(),
            fluidRow(
              column(4,
                     numericInput('nMasksDl','Nb of masks', width="100px",
                                  value = 0, min = 0, max=15, step=1)),
              column(4,
                     actionButton('autoDlMask',
                                  strong("Auto"),
                                  icon = icon("gear")))
            ),
            # uiOutput("masksC")
            tags$div(id = "masksC")
          ),
          id="selTabset"#,
          # type='pills'
        ),
        hr(),
        actionButton("reset",
                     "Reset All Selections")
      ),
      mainPanel(
        wellPanel(
          tabsetPanel(
            # type='pills',
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
            )
          ),
          id="svdTabset"
        )
      )
    )
  ),
  
  # SVD ####  
  tabPanel(
    "SVD",
    sidebarLayout(
      sidebarPanel(
        h4("SVD parameters"),
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
      ),
      mainPanel(
        wellPanel(                  
          tabsetPanel(
            # type="pills",
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
          ),
          id="svdTabset"
        )
      )
    )
  ),
  
  # ALS ####  
  tabPanel(
    "ALS",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(4,
                 numericInput("nALS", 
                              label = "Nb. spectra", 
                              value =  2, 
                              min   =  1, 
                              max   = 10, 
                              step  =  1,
                              width = '100px')
          ),
          column(4,
                 numericInput("maxiter", 
                              label = "MaxIter", 
                              value =  1000, 
                              min   =    20, 
                              max   =  9999, 
                              step  =    20,
                              width = '100px')
          ),
          column(4,
                 actionButton("runALS",strong("Run")),
                 tags$style(type='text/css', 
                            "#runALS { width:100%; margin-top: 25px;}")
          )
        ),
        br(),
        tabsetPanel(
          # type='pills',
          tabPanel(
            value='ALSparams',
            title=h4("Options"),
            br(),
            fluidRow(
              column(4,
                     radioButtons("initALS", 
                                  label = "Initialization", 
                                  choices = 
                                    list(
                                      "|SVD|"        = "SVD",
                                      "NMF"        = "NMF",
                                      "Sequential" = "seq",
                                      "Restart"    = "rst"), 
                                  inline = FALSE)
              ),
              column(8,
                     h4(""),
                     checkboxInput("useFiltered", 
                                   label = "Use SVD-filtered matrix",
                                   value = FALSE),
                     checkboxInput("optS1st", 
                                   label= "Opt. S first",
                                   value = FALSE)
              )
            )
          ),
          tabPanel(
            value='ALSSconstraints',
            title=h4("S const."),
            br(),
            column(4,
                   checkboxInput("nonnegS", 
                                 label= "S > 0",
                                 value = TRUE),
                   checkboxInput("uniS", 
                                 label= "S Unimodal",
                                 value = FALSE),
                   checkboxInput("normS", 
                                 label= "Normalize",
                                 value = TRUE),
                   shinyBS::bsTooltip("normS",
                                      title = "Enforces the normalization of S ( default: max(S)=1 )")
            ),
            column(4,
                   numericInput("smooth", 
                                label = "Smooth", 
                                value =    0, 
                                min   =    0, 
                                max   =    1, 
                                step  =  0.1,
                                width = '100px'),
                   checkboxInput("SumS", 
                                 label= "SUM(S)=1",
                                 value = FALSE),
                   shinyBS::bsTooltip("SumS", 
                                      title = "If Normalize is set, set norm such as sum(S)=1 ")
            ),
            fluidRow(
              column(12,
                     checkboxInput("shapeS", 
                                   label= "External spectrum shape(s)",
                                   value = FALSE),
                     
                     fileInput(
                       inputId = 'S0File',
                       label   = 'Select file(s)',
                       multiple= TRUE,
                       accept  = c('.dat','.txt','.csv')
                     ),
                     checkboxInput("softS0", 
                                   label= "Soft constraint",
                                   value = FALSE),
                     conditionalPanel(
                       condition = "input.softS0",
                       sliderInput("wSoftS0", 
                                 "logWeight for Soft constraint",
                                 min = -5, 
                                 max =  2, 
                                 value = 1,
                                 sep=""
                       )
                     )
              )
            )
          ),
          tabPanel(
            value='ALSCconstraints',
            title=h4("C const."),
            br(),
            checkboxInput("nonnegC", 
                          label= "C > 0",
                          value = TRUE),
            checkboxInput("closeC", 
                          label= "Closure",
                          value = FALSE),
            shinyBS::bsTooltip("closeC", 
                               title = "Ensures that sum(C)=1 at each delay"),
            uiOutput("maskSpExp_ui")
         )
        )
      ),
      mainPanel(
        wellPanel(
          tabsetPanel(
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
              tabsetPanel(
                tabPanel(
                  value="alsResid1_1",
                  title=h5("Residuals"), br(),
                  plotOutput("alsResid1", height=550)
                ),
                tabPanel(
                  value="alsResid1_2",
                  title=h5("SVD of Residuals"), br(),
                  plotOutput("alsResid2", height=550)
                ),
                id="alsResid1" #,
                # type='pills'
              )
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
                         actionButton("alsSpKinSave","Save (Ctrl+Click)",
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
                         checkboxGroupInput("vecsToRotate", 
                                     label = "Pick 2 or 3 vectors",
                                     choices = list("1" = 1, "2" = 2, "3" = 3,
                                                    "4" = 4, "5" = 5),
                                     selected = c(1,2))
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
                  )
                )
              )
            ),
            id="alsTabset"#,
            # type='pills'
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
  ),
  
  # About ####
  tabPanel(
    title="About",
    sidebarPanel(
      h5("Author      : P. Pernot"),
      a(href="https://doi.org/10.5281/zenodo.1064370",">>> How to Cite"),
      h5("Affiliation : CNRS"),
      h5("Version     : 2.3"),
      h5("Date        : 2017/11/30"),
      hr(),
      a(href="https://github.com/ppernot/SK-Ana","Code@GitHub"),
      br(),
      a(href="https://github.com/ppernot/SK-Ana/issues","Bug report")
    )
  )
  
  # END ####
)
