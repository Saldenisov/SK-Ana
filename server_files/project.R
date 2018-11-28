# Predefined input styles
observeEvent(
  input$style, isolate({
    switch( input$style,
            csvStyle = {
              header = FALSE
              sep= ","
              dec= "."
              datStr= "wxd"
            },
            otherStyle = {
              header = FALSE
              sep= ","
              dec= "."
              datStr= "wxd"
            },
            munichStyle = {
              header = FALSE
              sep= "\t"
              dec= "."
              datStr= "wxd"
            },
            elyseStyle = {
              header = FALSE
              sep= "\t"
              dec= "."
              datStr= "wxd"
            },
            heleneStyle={
              header = FALSE
              sep= ";"
              dec= "."
              datStr= "wxd"
            },
            streakStyle = {
              header = TRUE
              sep= ","
              dec= "."
              datStr= "wxd"
            }      
    )
    updateCheckboxInput(session, 
                        inputId = "header", 
                        value   = header)
    updateRadioButtons(session,
                       inputId  = "sep",
                       selected = sep)
    updateRadioButtons(session,
                       inputId  = "dec",
                       selected = dec)
    updateRadioButtons(session,
                       inputId  = "datStr",
                       selected = datStr)
  })
)

# New project
observeEvent(
  input$dataFile,
  getRawData(input$dataFile)
)
output$loadMsg <- renderUI({
  ll = list(
    h4('No data loaded'),
    h5('Please select data file(s)...')
  )
  
  if(Inputs$gotData & Inputs$validData) 
    ll = list(
      h4('Data loaded !')
    )
  
  return(ll)
})
output$rawData = DT::renderDataTable({
  if( !(Inputs$gotData && Inputs$validData) )
    return(NULL)
  
  ndelay  = nwavl = name = size = c()
  for (i in 1:length(RawData)) {
    name[i]   = RawData[[i]]$name
    ndelay[i] = length(RawData[[i]]$delay)
    nwavl[i]  = length(RawData[[i]]$wavl)
    size[i]   = format(object.size(RawData[[i]]$mat),units="Mb")
  }
  DT::datatable(cbind(id=1:length(RawData),name,ndelay,nwavl,size),
                options = list(paging    = FALSE,
                               ordering  = FALSE,
                               searching = FALSE,
                               dom       = 't'   ),
                selection=list(target='row',
                               selected=1:length(RawData)
                ), 
                escape    = FALSE
  )
  
})
output$sel     = renderPrint({
  if( !(Inputs$gotData && Inputs$validData) )
    return(NULL)
  
  cat(
    paste0('Selected file(s) :',
           ifelse(
             length(input$rawData_rows_selected) != 0 ,
             paste0(input$rawData_rows_selected,collapse=','),
             ' none'
           )
    )
  )
})
output$showsel = reactive({
  Inputs$gotData && 
    Inputs$validData 
})
outputOptions(output, "showsel", suspendWhenHidden = FALSE)
output$ui      = renderUI({
  if( !(Inputs$gotData && Inputs$validData) )
    return(NULL)
  if(length(input$rawData_rows_selected) == 0)
    return(NULL)
  
  if(length(input$rawData_rows_selected) == 1) {
    # Single matrix : no processing options
    Inputs$process <<- TRUE
    finishMatrix()
    return(NULL)
    
  } else {
    # Several matrices: propose processing options
    ndelay  = nwavl = c()
    ii=0
    for (i in input$rawData_rows_selected) {
      ii = ii+1
      ndelay[ii] = length(RawData[[i]]$delay)
      nwavl[ii]  = length(RawData[[i]]$wavl)
    }
    ok_delay = length(unique(ndelay)) == 1
    ok_wavl  = length(unique(nwavl))  == 1
    choices = list()
    if(ok_delay & ok_wavl)
      choices[["Average"]]    = 'avrg'
    if (ok_delay)
      choices[["Tile Wavl"]]  = 'tileWav'
    if (ok_wavl)
      choices[["Tile Delay"]] = 'tileDel'
    
    Inputs$process <<- FALSE
    if(length(choices) == 0 ) {
      showModal(modalDialog(
        title = ">>>> Data problem <<<< ",
        paste0("The chosen data have inconsient dimensions. ",
               "They cannot be treated simultaneously !"),
        easyClose = TRUE, 
        footer = modalButton("Close"),
        size = 's'
      ))
    } else {
      verticalLayout(
        column(
          6,
          radioButtons(
            inputId  = 'procMult', 
            label    = h4('Please choose processing option'),
            choices  = choices,
            selected = choices[length(choices)],
            inline   = TRUE),
          shinyBS::bsTooltip(
            'procMult',
            title = 'Choice based on dims of matrices')
        ),
        column(
          2,
          actionButton(
            "process",
            strong("Do it!"),
            icon=icon('gear')
          ),
          tags$style(
            type='text/css',
            "#process { width:100%; margin-top: 5px;}"
          )
        )
      )
    }
    
  } 
  
})

observeEvent(
  input$process,
  isolate({
    Inputs$process <<- TRUE
    finishMatrix()
  })
)

output$projectInfoNew <- renderUI({
  if(!Inputs$finish)
    return(NULL)
  
  HTML(paste0(
    '<b>Global matrix</b>: ',
    length(Inputs$delay),'x', length(Inputs$wavl),'<br>',
    'O.D.  range: ',paste0(signif(range(Inputs$mat)      ,2),
                           collapse=', '),'<br>',
    'Delay range: ',paste0(signif(range(Inputs$delay),4),
                           collapse=', '),'<br>',
    'Wavl  range: ',paste0(signif(range(Inputs$wavl), 4),
                           collapse=', ')
  )
  )
})
output$showPIN = reactive({
  Inputs$process &&
    length(input$rawData_rows_selected) != 0
})
outputOptions(output, "showPIN", suspendWhenHidden = FALSE)
output$vignette <- renderPlot({
  if(!Inputs$finish)
    return(NULL)
  
  mat   = Inputs$mat
  wavl  = Inputs$wavl
  delay = Inputs$delay
  
  if(!is.finite(diff(range(wavl)))  ||
     !is.finite(diff(range(delay))) ||
     !is.finite(diff(range(mat,na.rm=TRUE)))   ) {
    plot(1:10,1:10,type='n')
    text(x=5,y=5,labels='Data not ready...',col=2)
  }
  
  par(
    mfrow = c(1, 1),
    cex = cex, cex.main = cex, mar = mar,
    mgp = mgp, tcl = tcl, pty = pty
  )
  image(
    delay,wavl,mat,
    xlab = 'Delay',ylab = 'Wavelength',
    col  = imgColors, 
    zlim = quantile(mat,probs = c(0.001,0.999),
                    na.rm = TRUE)
  )
})

output$projectInfoOpen <- renderPrint({
  validate(
    need(
      !is.null(input$projectFile),
      "Please select a project file (*.Rda)"
    )
  )

  # Restore data
  for(n in names(data))
    Inputs[[n]] <<- data[[n]]
  
  # Update selectors
  for(n in names(config))
    updateSliderInput(session, inputId = n, value = config[[n]])
  
  
  # Check file read
  cat(paste0('Project: ',input$projectTag,'\n\n'))
  cat('Data File(s):\n')
  cat(paste0(Inputs$fileOrig,'\n'))
  cat("\n")
  cat(paste0(
    'Matrix: ',
    length(Inputs$delayOrig),'x',
    length(Inputs$wavlOrig),'\n'
  ))
  cat('Delay range: ',range(Inputs$delayOrig),'...\n')
  cat('Wavl  range: ',range(Inputs$wavlOrig),'...\n')
  
  cat("Sanity:",checkInputsSanity(),'\n')
  
})

output$saveProject <- downloadHandler(
  filename = function()    {
    paste0(input$projectTag,'.Rda')
  },
  content  = function(con) {
    # Collect configuration parameters by type
    ll = reactiveValuesToList(input)
    # sel = grepl('^keep',names(ll))
    config = ll #[sel]
    
    # Collect Inputs
    ll = reactiveValuesToList(Inputs)
    # sel = grepl('Orig',names(ll))
    Inputs = ll #[sel]
    
    save(sliders, buttons, Inputs,
         file     = con)
  }
)
