fluidRow(
  column(6,
         # withSpinner(
           plotOutput("kinSpVectors", height=500,
                      dblclick = "kinSp_dblclick",
                      brush = brushOpts(
                        id = "kinSp_brush",
                        resetOnNew = TRUE
                      )
           # ),
         #   type=4
         )
  ),
  column(6,
         # withSpinner(
           plotOutput("kinKinVectors", height=500,
                      dblclick = "kinKin_dblclick",
                      brush = brushOpts(
                        id = "kinKin_brush",
                        resetOnNew = TRUE
                      )
           # ),
           # type=4
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
)
