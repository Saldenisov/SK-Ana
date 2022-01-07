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
           checkboxInput("nonnegSKinet", 
                         label= "S>0",
                         value = FALSE
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
                         label= "Weighted data",
                         value = FALSE
           ),
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
                        icon=icon('cog')
           ),
           tags$style(type='text/css', 
                      "#runKin { width:100%; margin-top: 25px;}"
           ),
           checkboxInput("kinRestart",
                         label=strong(" Restart"),
                         value=FALSE
           )
                        
    )
  )
)
