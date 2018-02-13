function(request) {
  
  sideWidth = 4
  mainWidth = 12 - sideWidth
  
navbarPage( 
  "SK-Ana",
  
  # Config ####
  theme = shinythemes::shinytheme(c("cosmo","cerulean","spacelab","yeti")[2]),
  
  # Project ####
  tabPanel(
    "Project",
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
                selected= 'dxw',
                inline = TRUE)
            ),
            hr( style="border-color: #666;"),
            strong('Load-time compression factors'),
            br(),br(),
            fluidRow(
              column(3,
                      numericInput(
                        inputId = 'compFacD', 
                        label   = 'Delay', 
                        value   = 1, min=1, max=20, step=1,
                        width   = '100px')
              ),
              column(3,
                      numericInput(
                        inputId = 'compFacW', 
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
            # bookmarkButton(id = "bookmark")
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
  ),
  
  # Select Area ####
  tabPanel(
    "Data Selection",
    sidebarLayout(
      sidebarPanel(
        width = sideWidth,
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
            title=h4("Wavl Mask"),
            br(),
            fluidRow(
              column(4,
                     numericInput('nMasksWl','Nb of masks', width="100px",
                                  value = 0, min = 0, max=15, step=1)),
              column(4,
                     style = "margin-top: 25px;",
                     actionButton('autoWlMask',
                                  strong("Auto"),
                                  icon = icon("gear") ))
            ),
            tags$div(id = "masksS")
          ),
          tabPanel(
            value="dataMasksC",
            title=h4("Delay Mask"),
            br(),
            fluidRow(
              column(4,
                     numericInput('nMasksDl','Nb of masks', width="100px",
                                  value = 0, min = 0, max=15, step=1)),
              column(4,
                     style = "margin-top: 25px;",
                     actionButton('autoDlMask',
                                  strong("Auto"),
                                  icon = icon("gear")))
            ),
            tags$div(id = "masksC")
          ),
          id="selTabset"
        ),
                    hr( style="border-color: #666;"),
        fluidRow(
          column(5,
                 actionButton("reset",
                              strong("Reset Selections"),
                              icon = icon("eraser"))
          ),
          column(7,
                 actionButton('saveSelectors',
                              strong('Save Selections'),
                              icon = icon("save"))
          )
        ),
        fluidRow(
          column(5,
                 br()
          ),
          column(7, br(),
                 fileInput(
                   inputId = 'selectorFile',
                   label   = 'Load Saved Selections',
                   multiple= FALSE,
                   accept  = c('.Rda')
                 )
          )       
        )
      ),
      mainPanel(
        width = mainWidth,
        wellPanel(
          tabsetPanel(
            tabPanel(
              value="dataImg",
              title=h4("Data"),
              br(),
              fluidRow(
                column(6,
                       # withSpinner(
                         plotOutput("image1", 
                                    height = 450,
                                    dblclick = "image1_dblclick",
                                    brush = brushOpts(
                                      id = "image1_brush",
                                      resetOnNew = TRUE
                                    )
                         ),
                         # type=4
                       # ),
                       br(),
                       wellPanel(
                         fluidRow(
                           column(9,
                                  sliderInput("keepWlCut", 
                                              "Reference wavl",
                                              min = 0, 
                                              max = 1, 
                                              value = 0.5,
                                              sep=""
                                  )
                           ),
                           column(3,
                                  actionButton("delayCutSave","Save",
                                               icon     = icon('save')),
                                  tags$style(type='text/css', 
                                             "#delayCutSave { width:100%; margin-top: 30px;}")
                                  
                           )
                         )
                       )
                ),
                column(6,
                       # withSpinner(
                         plotOutput("transects", height = 450),
                         # type=4
                       # ),
                       br(),
                       wellPanel(
                         fluidRow(
                           column(9,
                                  sliderInput("keepDlCut", 
                                              "Reference delay",
                                              min = 0, 
                                              max = 1, 
                                              value = 0.5,
                                              sep=""
                                  )
                           ),
                           column(3,
                                  actionButton("wavlCutSave","Save",
                                               icon     = icon('save')),
                                  tags$style(type='text/css', 
                                             "#wavlCutSave { width:100%; margin-top: 30px;}")
                           )
                         )
                       )
                )
              )
            ),
            tabPanel(
              value="dataCuts",
              title=h4("Cuts"),
              br(),
              fluidRow(
                column(6,
                       # withSpinner(
                         plotOutput("cutsDl", 
                                    height=450,
                                    dblclick = "cutsDl_dblclick",
                                    brush = brushOpts(
                                      id = "cutsDl_brush",
                                      resetOnNew = TRUE
                                    )
                         ),
                         # type=4
                       # ),
                       br(),
                       wellPanel(
                         sliderInput("stepDlCut", 
                                     "Cut freq.",
                                     min = 0, 
                                     max = 100, 
                                     value = 10,
                                     sep=""
                         )
                       )
                ),
                column(6,
                       # withSpinner(
                         plotOutput("cutsWl", 
                                    height=450,
                                    dblclick = "cutsWl_dblclick",
                                    brush = brushOpts(
                                      id = "cutsWl_brush",
                                      resetOnNew = TRUE
                                    )
                         ),
                         # type=4
                       # ),
                       br(),
                       wellPanel(
                         sliderInput("stepWlCut", 
                                     "Cut freq.",
                                     min = 0, 
                                     max = 100, 
                                     value = 10,
                                     sep=""
                         )
                         
                       )
                )
              )
            ),
            id="svdTabset"
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
        width = sideWidth,
        h4("SVD parameters"),
        fluidRow(
          column(3,
                 numericInput("nSV", 
                              label = "Dimension", 
                              value =  2, 
                              min   =  1, 
                              max   =  10, 
                              step  =  1,
                              width = '100px')
          )
        ),
                    hr( style="border-color: #666;"),
        h4("Glitch removal in kinetics"),
        fluidRow(
          column(3,
                 numericInput('cleanLevel','Level', width="100px",
                              value = 2, min = 2, max=10, step=1)),
          column(3,
                 style = "margin-top: 25px;",
                 actionButton('clean',
                              strong("Clean"),
                              icon = icon("gear"))),
          column(3,
                 style = "margin-top: 25px;",
                 actionButton('cleanCancel',
                              strong("Cancel"),
                              icon = icon("eraser")))
        )
      ),
      mainPanel(
        width = mainWidth,
        wellPanel(                  
          tabsetPanel(
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
              withSpinner(
                plotOutput("svdVec", height=550),
                type=4
              )
            ),
            tabPanel(
              value="residSVD",
              title=h4("Data vs. Model"),
              br(),
              withSpinner(
                plotOutput("svdResid", height=550),
                type=4
              )
            ),
            tabPanel(
              value="residSVD1",
              title=h4("Residuals"),
              br(),
              withSpinner(
                plotOutput("svdResid1", height=550),
                type=4
              )
            ),
            tabPanel(
              value="recSVD",
              title=h4("Contributions"),
              br(),
              withSpinner(
                plotOutput("svdContribs", height=550),
                type=4
              )
            ),
            tabPanel(
              value="statSVD",
              title=h4("Statistics"),
              br(),
              withSpinner(
                DT::dataTableOutput('svdStat',width = "50%"),
                type=4
              )
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
        width = sideWidth,
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
                 actionButton("runALS",
                              strong("Run"),
                              icon=icon('gear')
                              ),
                 tags$style(type='text/css', 
                            "#runALS { width:100%; margin-top: 25px;}")
          )
        ),
        fluidRow(
          column(8,
                 sliderInput("alsThresh", 
                             "Log convergence threshold",
                             min   =  -10, 
                             max   =   -2, 
                             value =   -6,
                             sep   =   ""
                 )
          )
        ),
        br(),
        tabsetPanel(
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
                                 hr( style="border-color: #666;"),
                     checkboxInput("shapeS", 
                                   label= "External spectrum shape(s)",
                                   value = FALSE),
                     conditionalPanel(
                       condition = "input.shapeS",
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
                                     min   =  -3, 
                                     max   =   3, 
                                     value =   1,
                                     step  = 0.5,
                                     sep   = ""
                         )
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
            conditionalPanel(
              condition = "input.closeC",
              sliderInput("wCloseC", 
                          "logWeight for Soft constraint",
                          min   =  -3, 
                          max   =   3, 
                          value =   1,
                          step  = 0.5,
                          sep   = ""
              )
            ),
            uiOutput("maskSpExp_ui")
         )
        )
      ),
      mainPanel(
        width = mainWidth,
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
              title=h4("Diagnostics"),
              br(),
              tabsetPanel(
                tabPanel(
                  value="alsResid1_1",
                  title=h5("Data vs. Model"), br(),
                  withSpinner(
                    plotOutput("alsResid1", height=550) ,
                    type=4
                  )
                ),
                tabPanel(
                  value="alsResid1_3",
                  title=h5("Residuals"), br(),
                  withSpinner(
                    plotOutput("alsResid3", height=550) ,
                    type=4
                  )
                ),
                tabPanel(
                  value="alsResid1_2",
                  title=h5("SVD of Residuals"), br(),
                  withSpinner(
                    plotOutput("alsResid2", height=550),
                    type=4
                  )
                ),
                id="alsResid1" 
              )
            ),
            tabPanel(
              value="alsVectorsTab",
              title=h4("Kinetics & Spectra"),
              br(),
              fluidRow(
                column(6,
                       withSpinner(
                         plotOutput("alsSpVectors", height=500,
                                    dblclick = "alsSp_dblclick",
                                    brush = brushOpts(
                                      id = "alsSp_brush",
                                      resetOnNew = TRUE
                                    )),
                         type=4
                       )
                ),
                column(6,
                       withSpinner(
                         plotOutput("alsKinVectors", height=500,
                                    dblclick = "alsKin_dblclick",
                                    brush = brushOpts(
                                      id = "alsKin_brush",
                                      resetOnNew = TRUE
                                    )),
                         type=4
                       )
                )
              ),
              wellPanel(
                h5('Save ALS spectra and kinetics'),
                actionButton("alsSpKinSave","Save",
                             icon     = icon('save'))
              )
            ),
            tabPanel(
              value="alsContribTab",
              title=h4("Contributions"),
              br(),
              withSpinner(
                plotOutput("alsContribs", height=550),
                type=4
              )
            ),
            tabPanel(
              value="alsRotAmbTab",
              title=h4("Ambiguity"),
              br(),
              fluidRow(
                column(6,
                       withSpinner(
                         plotOutput("ambSpVectors", height=450,
                                    dblclick = "ambSp_dblclick",
                                    brush = brushOpts(
                                      id = "ambSp_brush",
                                      resetOnNew = TRUE
                                    )),
                         type=4
                       )
                ),
                column(6,
                       withSpinner(
                         plotOutput("ambKinVectors", height=450,
                                    dblclick = "ambKin_dblclick",
                                    brush = brushOpts(
                                      id = "ambKin_brush",
                                      resetOnNew = TRUE
                                    )),
                         type=4
                       )
                )
              ),
              wellPanel( 
                h4("Explore Rotational/Scaling Ambiguity"),
                fluidRow(
                  column(10,
                         uiOutput("selAmbParams")
                  ),
                  column(2,
                         actionButton("runALSAmb",
                                      strong("Start"),
                                      icon=icon("cog")),
                         tags$style(type='text/css',
                                    "#runALSAmb { width:100%; margin-top: 0px;}"),
                         shinyBS::bsButton("killALSAmb", 
                                           label = "Stop",
                                           type  = "toggle", 
                                           value = FALSE, 
                                           size  = "default", 
                                           icon  = icon("ban")),
                         tags$style(type='text/css',"#killALSAmb { width:100%;}")
                  )
                )
              )
            ),
            id="alsTabset"
          )
        )
      )
    )
  ),
  
  # Kinet ####
  tabPanel(
    title="Kinet",
    sidebarPanel(
      width = sideWidth,
      tabsetPanel(
        tabPanel(
          h4("Model"),
          wellPanel(
            tabsetPanel(
              tabPanel(
                h4("Type"),
                hr( style="border-color: #666;"),
                textAreaInput(
                  inputId = 'schemeScript', 
                  label   = ' Enter scheme', 
                  value   = "", 
                  width   = '100%', 
                  height  = '200px',
                  cols    = NULL, rows = NULL, 
                  placeholder = NULL, 
                  resize = "none"
                ),
                fixedRow(
                  column(12,offset=0,
                         actionButton("clear_tS" ,
                                      "Reset",
                                      icon=icon("eraser")),
                         actionButton("update_tS",
                                      "Done",
                                      icon=icon("check"))
                  )
                )
              ),
              tabPanel(
                h4("Load"),
                hr( style="border-color: #666;"),
                fileInput(
                  inputId = 'schemeFile',
                  label   = 'Select scheme file',
                  multiple= FALSE,
                  accept  = c('.txt','.in')
                )
              ),
              tabPanel(
                h4("Save"),
                hr( style="border-color: #666;"),
                verticalLayout(
                  textInput(
                    inputId = 'schemeFileName', 
                    label   = 'Model Name', 
                    value   = "model1"
                  ),
                  downloadButton(
                    outputId = 'schemeFileSave',
                    label    = 'Save (Ctrl+Click)'
                  )
                )
              )
            ),
            hr( style="border-color: #666;"),
            tabsetPanel(
              tabPanel(
                h4("Scheme"),
                DT::dataTableOutput('scheme')
              ),
              tabPanel(
                h4("Rates"),
                uiOutput('rates')
              ),
              tabPanel(
                h4("Conc."),
                tabsetPanel(id='all_conc'),
                uiOutput('concentrations')
              ),
              tabPanel(
                h4("Eps."),
                uiOutput('epsilon')
              )
            )
          )
        ),
        tabPanel(
          h4("Run"),
          
          wellPanel(
            fluidRow(
              column(8,
                     
                     sliderInput("kinGlobNit", 
                                 "Global Optimization Iterations",
                                 min   =   0, 
                                 max   =  50, 
                                 value =   0,
                                 step  =  10
                     ),
                     sliderInput("kinGlobFac", 
                                 "Global Population Factor",
                                 min   =   10, 
                                 max   =  100, 
                                 value =   30,
                                 step  =   10
                     ),
                     sliderInput("kinThresh", 
                                 "Log Convergence Threshold",
                                 min   =  -10, 
                                 max   =   -2, 
                                 value =   -8
                     ),
                     numericInput("kinSmooth", 
                                  label = "Smooth", 
                                  value =    0, 
                                  min   =    0, 
                                  max   =    1, 
                                  step  =  0.1,
                                  width = '120px'
                     ),
                     checkboxInput("kinWeighted", 
                                   label= "Weigted data",
                                   value = FALSE),
                     # shinyBS::bsTooltip("closeC", 
                     #                    title = "Ensures that sum(C)=1 at each delay"),
                     conditionalPanel(
                       condition = "input.kinWeighted",
                       fluidRow(
                         column(6,
                                numericInput("kinSigma", 
                                             label = "Sigma", 
                                             value =       1,
                                             width = '120px')
                         ),
                         column(6,
                                sliderInput("kinSigmaIndex", 
                                            label = "SVD Level",
                                            min   = 0,
                                            max   = 6,
                                            step  = 1,
                                            value = 0)
                         )
                       )
                     )
              ),
              column(4,
                     actionButton("runKin",
                                  strong("Run"),
                                  icon=icon('gear')
                     ),
                     tags$style(type='text/css', 
                                "#runKin { width:100%; margin-top: 25px;}"
                     ),
                     checkboxInput("kinRestart",
                                   label=strong(" Restart"),
                                   value=FALSE)
              )
            )
          )
        )
      )
    ),
    mainPanel(
      width = mainWidth,
      wellPanel(
        tabsetPanel(
          tabPanel(
            h4("Best params"),
            wellPanel(
              fluidRow(
                column(4,
                       uiOutput('kinRes')
                       ),
                column(6,
                       h4('Best parameters (MAP)'),
                       withSpinner(
                         DT::dataTableOutput('kinOpt'),
                         type = 4
                       )
                )
              )
            )
          ),
          tabPanel(
            h4("Trace"),
            wellPanel(
              verbatimTextOutput('kinGlPrint'),
              verbatimTextOutput('kinOptPrint')
            )
          ),
          tabPanel(
            h4("Identifiability"),
            wellPanel(
              withSpinner(
                plotOutput("kinParams", height=550) ,
                type=4
              )
            )
          ),
          tabPanel(
            value="kinResid",
            title=h4("Diagnostics"),
            br(),
            
            tabsetPanel(
              tabPanel(
                value="kinResid1_4",
                title=h5("L.O.F"), br(),
                withSpinner(
                  plotOutput("kinResid4", height=550),
                  type=4
                )
              ),
              tabPanel(
                value="kinResid1_3",
                title=h5("Integ. kinet."), br(),
                withSpinner(
                  plotOutput("kinResid3", height=550),
                  type=4
                )
              ),
              tabPanel(
                value="kinResid1_5",
                title=h5("Data vs. Model"), br(),
                withSpinner(
                  plotOutput("kinResid5", height=550) ,
                  type=4
                ),
                wellPanel(
                  fluidRow(
                    column(4,
                           checkboxInput("kinContours",
                                         label = "Add contours",
                                         value = FALSE),
                           offset = 8
                    )
                  )
                )
              ),
              tabPanel(
                value="kinResid1_1",
                title=h5("Residuals"), br(),
                withSpinner(
                  plotOutput("kinResid1", height=550) ,
                  type=4
                )
              ),
              tabPanel(
                value="kinResid1_2",
                title=h5("SVD of Residuals"), br(),
                withSpinner(
                  plotOutput("kinResid2", height=550),
                  type=4
                )
              ),
              id="kinResidIn" 
            )
          ),
          tabPanel(
            value="kinVectorsTab",
            title=h4("Kinetics & Spectra"),
            br(),
            fluidRow(
              column(6,
                     withSpinner(
                       plotOutput("kinSpVectors", height=500,
                                  dblclick = "kinSp_dblclick",
                                  brush = brushOpts(
                                    id = "kinSp_brush",
                                    resetOnNew = TRUE
                                  )),
                       type=4
                     )
              ),
              column(6,
                     withSpinner(
                       plotOutput("kinKinVectors", height=500,
                                  dblclick = "kinKin_dblclick",
                                  brush = brushOpts(
                                    id = "kinKin_brush",
                                    resetOnNew = TRUE
                                  )),
                       type=4
                     )
              )
            ),
            wellPanel(
              fluidRow(
                column(6,
                       h5('Save Kinet spectra and kinetics'),
                       actionButton("kinSpKinSave","Save",
                                    icon     = icon('save'))
                ),
                column(6,
                       checkboxInput("plotCSUQ",
                                     label = " Plot confidence bands",
                                     value = FALSE)
                )
              )
            )
          ),
          tabPanel(
            value="kinContribTab",
            title=h4("Contributions"),
            br(),
            withSpinner(
              plotOutput("kinContribs", height=550),
              type=4
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
      width = sideWidth,
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
      width = sideWidth,
      h5("Author      : P. Pernot"),
      a(href="https://doi.org/10.5281/zenodo.1064370",">>> How to Cite"),
      h5("Affiliation : CNRS"),
      h5("Version     : 3.0beta"),
      h5("Date        : 2018/01/30"),
      hr( style="border-color: #666;"),
      a(href="https://github.com/ppernot/SK-Ana","Code@GitHub"),
      br(),
      a(href="https://github.com/ppernot/SK-Ana/issues","Bug report")
    )
  )
  
  # END ####
)

}



