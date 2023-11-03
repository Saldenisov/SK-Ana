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
         )
  ),
  br(),
  column(
    width=12,
    h5('Save ALS spectra and kinetics'),
    actionButton("alsSpKinSave","Save",
                 icon     = icon('save'),
                 class = "btn-primary",
                 width = "150px")#,
   
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
