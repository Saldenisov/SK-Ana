fluidRow(
  column(6,
         # withSpinner(
           plotOutput("alsSpVectors", height=500,
                      dblclick = "alsSp_dblclick",
                      brush = brushOpts(
                        id = "alsSp_brush",
                        resetOnNew = TRUE
                      )
           # ),
           # type=4
         ),
         fluidRow(
           column(3,
                  numericInput("alsSpXmin", "X min:", value = NULL, step = 1)
           ),
           column(3,
                  numericInput("alsSpXmax", "X max:", value = NULL, step = 1)
           ),
           column(3,
                  numericInput("alsSpYmin", "Y min:", value = NULL, step = 0.01)
           ),
           column(3,
                  numericInput("alsSpYmax", "Y max:", value = NULL, step = 0.01)
           )
         )
  ),
  column(6,
         # withSpinner(
           plotOutput("alsKinVectors", height=500,
                      dblclick = "alsKin_dblclick",
                      brush = brushOpts(
                        id = "alsKin_brush",
                        resetOnNew = TRUE
                      )
           # ),
           # type=4
         ),
         fluidRow(
           column(3,
                  numericInput("alsKinXmin", "X min:", value = NULL, step = 0.1)
           ),
           column(3,
                  numericInput("alsKinXmax", "X max:", value = NULL, step = 0.1)
           ),
           column(3,
                  numericInput("alsKinYmin", "Y min:", value = NULL, step = 0.01)
           ),
           column(3,
                  numericInput("alsKinYmax", "Y max:", value = NULL, step = 0.01)
           )
         )
  ),
  br(),
  column(
    width=12,
    h5('Save ALS spectra and kinetics'),
    actionButton("alsSpKinSave","Save",
                 icon     = icon('save'),
                 class = "btn-primary",
                 width = "150px"),
    downloadButton("alsSpKinDownload", "Download",
                   class = "btn-success",
                   style = "margin-left: 10px;")#,
   
    # shinyCopy2clipboard::CopyButton("copybtn_ALS_Sp", 
    #                                 label = "Copy Spectra",
    #                                 icon  = icon("pause"),
    #                                 text  = "Empty",
    #                                 class = "btn-primary",
    #                                 width = "150px"),
    # 
    # shinyCopy2clipboard::CopyButton("copybtn_ALS_Kin", 
    #                                 label = "Copy Kinetics",
    #                                 icon  = icon("pause"),
    #                                 text  = "Empty",
    #                                 class = "btn-primary",
    #                                 width = "150px")
  )
)
