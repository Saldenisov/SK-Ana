wellPanel(
  tabsetPanel(
    
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
      h4("Save"),
      hr( style="border-color: #666;"),
      verticalLayout(
        textInput(
          inputId = 'schemeFileName', 
          label   = 'Model Name', 
          value   = 'model1',
          placeholder = 'model1'
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
    ),
    
    tabPanel(
      h4("Spectra"),
      hr( style="border-color: #666;"),
      # checkboxInput(
      #   "shapeSKin", 
      #   label= "External spectrum shape(s)",
      #   value = FALSE
      # ),
      # conditionalPanel(
      #   condition = "input.shapeSKin",
        fileInput(
          inputId  = 'S0KinFile',
          label    = 'Select file(s)',
          multiple = TRUE,
          accept   = c('.dat','.txt','.csv')
        )#,
        # checkboxInput(
        #   "softS0", 
        #   label= "Soft constraint",
        #   value = FALSE
        # ),
        # conditionalPanel(
        #   condition = "input.softS0",
        #   sliderInput(
        #     "wSoftS0", 
        #     "logWeight for Soft constraint",
        #     min   =  -3, 
        #     max   =   3, 
        #     value =   1,
        #     step  = 0.5,
        #     sep   = ""
        #   )
        # )
      # )
        ,
      uiOutput('extSpectra')
    )
  )
)
