inputStyle = reactiveValues(
  # TBD: check reactivity to inputStyle when otherStyle is used.
  #      (does not work for datStr...)
  header = FALSE,
  sep    = ",",
  dec    = ".",
  datStr = "wxd"
)
dataLoaded = reactive({
  Inputs$gotData & Inputs$validData
})
# Functions ####
downsizeMatrix <- function(delay, wavl, mat, fwD = 1, fwW = fwD) {
  # Downsize matrix by factors fwD (delay) and fwW (wavl)
  
  # Pad matrix with Nas
  newNrow <- ceiling(nrow(mat) / fwD) * fwD
  newNcol <- ceiling(ncol(mat) / fwW) * fwW
  lmat <- matrix(NA, nrow = newNrow, ncol = newNcol)
  lmat[1:nrow(mat), 1:ncol(mat)] <- mat
  
  # Block average
  nRowBloc <- newNrow / fwD
  nColBloc <- newNcol / fwW
  
  amat <- matrix(NA, nrow = nRowBloc, ncol = nColBloc)
  for (i in 1:nRowBloc)
    for (j in 1:nColBloc)
      amat[i, j] <- mean(
        lmat[
          ((i - 1) * fwD + 1):(i * fwD),
          ((j - 1) * fwW + 1):(j * fwW)
          ],
        na.rm = TRUE
      )
  
  ldelay <- rep(NA, newNrow)
  ldelay[1:length(delay)] <- delay
  adelay <- c()
  for (i in 1:nRowBloc)
    adelay[i] <- mean(
      ldelay[((i - 1) * fwD + 1):(i * fwD)],
      na.rm = TRUE
    )
  
  lwavl <- rep(NA, newNcol)
  lwavl[1:length(wavl)] <- wavl
  awavl <- c()
  for (i in 1:nColBloc)
    awavl[i] <- mean(
      lwavl[((i - 1) * fwW + 1):(i * fwW)],
      na.rm = TRUE
    )
  
  return(
    list(
      mat = amat,
      delay = adelay,
      wavl = awavl
    )
  )
}
getOneMatrix  <- function(dataFile) {
  wavl = try(
    as.numeric(
      as.vector(
        read.table(
          dataFile,
          nrows = 1,
          header = inputStyle$header,
          sep = inputStyle$sep,
          stringsAsFactors = FALSE,
          dec = inputStyle$dec,
          fileEncoding = "ISO-8859-1",
          quote = ""
        )
      )
    )[-1],
    silent = TRUE
  )
  if(class(wavl) == 'try-error' | length(wavl) == 0) 
    return(NULL) 
  
  mat = try(
    read.table(
      dataFile, 
      header = inputStyle$header, 
      skip = 1,
      dec = inputStyle$dec, 
      sep = inputStyle$sep,
      colClasses= 'numeric',
      stringsAsFactors = FALSE
    ),
    silent = TRUE
  )
  if(class(mat) == 'try-error') 
    return(NULL) 
  
  mat = as.matrix(mat)
  delay = as.numeric(mat[,1])
  if(length(delay) == 0)
    return(NULL) 
  
  u = !duplicated(delay)
  delay = delay[u]
  mat   = mat[u,-1]
  mat[!is.finite(mat)] = 0
  
  # Ensure increasing coordinates
  iord = order(wavl,decreasing=FALSE)
  wavl=wavl[iord]
  mat = mat[,iord] 
  iord = order(delay,decreasing=FALSE)
  delay=delay[iord]
  mat = mat[iord,] 
  
  # Downsize
  if(input$compFacD >= 2 | input$compFacW >= 2) {
    dsm = downsizeMatrix(delay,wavl,mat,
                         fwD=input$compFacD, 
                         fwW=input$compFacW)
    mat   = dsm$mat
    delay = dsm$delay
    wavl  = dsm$wavl
    rm(dsm)
  }
  
  # Transpose if necessary
  # if(inputStyle$datStr != 'dxw') {
  if(input$datStr != 'dxw') {
    print('Transpose')
    mat   = t(mat)
    tmp   = delay
    delay = wavl
    wavl  = tmp
  }
  
  return(list(mat=mat, wavl=wavl, delay=delay, 
              delaySave=delay, delayId= rep(1,length(delay))))
  
}
getRawData    <- function(fileNames) {
  initInputs()  # (Re)initialize data tables
  RawData <<- list()  # Init list in upper environment
  
  # Init progress bar
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  updateProgress <- function(value = NULL, detail = NULL) {
    progress$set(value = value, detail = detail)
  }
  progress$set(message = "Reading data file(s) ", value = 0)
  
  # Load data files
  for(i in 1:nrow(fileNames)) {
    fName = fileNames[i,'name']
    updateProgress(value  = i / nrow(fileNames),detail = fName)
    O = getOneMatrix(fileNames[i,'datapath'])
    if (!is.null(O)) { 
      O$name = fName
    } else {
      Inputs$validData <<- FALSE
      showModal(modalDialog(
        title = ">>>> Data problem <<<< ",
        paste0("The chosen data type does not ",
               "correspond to the opened data file(s)!"),
        easyClose = TRUE, 
        footer = modalButton("Close"),
        size = 's'
      ))
    }
    RawData[[i]] <<- O
  }
  Inputs$gotData <<- TRUE
}

# Predefined input styles ####
observeEvent(
  input$style, 
  isolate({
    switch( input$style,
            csvStyle = {
              inputStyle$header = FALSE
              inputStyle$sep    = ","
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            otherStyle = {
              inputStyle$header = FALSE
              inputStyle$sep    = ","
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            # munichStyle = {
            #   inputStyle$header = FALSE
            #   inputStyle$sep= "\t"
            #   inputStyle$dec= "."
            #   inputStyle$datStr= "wxd"
            # },
            elyseStyle = {
              inputStyle$header = FALSE
              inputStyle$sep    = "\t"
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            heleneStyle={
              inputStyle$header = FALSE
              inputStyle$sep    = ";"
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            streakStyle = {
              inputStyle$header = TRUE
              inputStyle$sep    = ","
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            otherStyle = {
              inputStyle$header = input$header
              inputStyle$sep    = input$sep
              inputStyle$dec    = input$dec
              inputStyle$datStr = input$datStr
            }       
    )
    # updateCheckboxInput(session, 
    #                     inputId = "header", 
    #                     value   = header)
    # updateRadioButtons(session,
    #                    inputId  = "sep",
    #                    selected = sep)
    # updateRadioButtons(session,
    #                    inputId  = "dec",
    #                    selected = dec)
    # updateRadioButtons(session,
    #                    inputId  = "datStr",
    #                    selected = datStr)
  })
)

# New project ####
observeEvent(
  input$dataFile,
  getRawData(input$dataFile)
)
output$loadMsg <- renderUI({
  if(dataLoaded()) {
      ll = list(
      h4('Data loaded !')
    )
  } else {
    ll = list(
      h4('No data loaded'),
      h5('Please select data file(s)...')
    )
  }
  return(ll)
})
output$rawData = DT::renderDataTable({
  if( !dataLoaded() )
    return(NULL)
  
  ndelay  = nwavl = name = size = c()
  for (i in 1:length(RawData)) {
    name[i]   = RawData[[i]]$name
    ndelay[i] = length(RawData[[i]]$delay)
    nwavl[i]  = length(RawData[[i]]$wavl)
    size[i]   = format(
      object.size(RawData[[i]]$mat),
      units="Mb")
  }
  DT::datatable(
    cbind(id=1:length(RawData),name,ndelay,nwavl,size),
    options = list(
      paging    = FALSE,
      ordering  = FALSE,
      searching = FALSE,
      dom       = 't'   ),
    selection=list(
      target='row',
      selected=1:length(RawData)
    ), 
    escape    = FALSE
  )
})
output$sel     = renderPrint({
  if( !dataLoaded() )
    return(NULL)
  cat(
    paste0(
      'Selected file(s) :',
      ifelse(
        length(input$rawData_rows_selected) != 0 ,
        paste0(input$rawData_rows_selected,collapse=','),
        ' none'
      )
    )
  )
  Inputs$process <<- FALSE
  Inputs$finish  <<- FALSE
})
output$showsel = reactive({
  Inputs$gotData & Inputs$validData 
})
outputOptions(output, "showsel", suspendWhenHidden = FALSE)
output$ui      = renderUI({
  if( !dataLoaded() )
    return(NULL)
  if(length(input$rawData_rows_selected) == 0)
    return(NULL)
  
  if(length(input$rawData_rows_selected) == 1) {
    # Single matrix : no processing options
    Inputs$process <<- TRUE
    Inputs$finish  <<- FALSE
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
            icon=icon('cog')
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

observeEvent(input$process,
  isolate({
    Inputs$process <<- TRUE
    Inputs$finish  <<- FALSE
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
  Inputs$process &
    length(input$rawData_rows_selected) != 0
})
outputOptions(output, "showPIN", suspendWhenHidden = FALSE)
output$vignette <- renderPlot({
  if(!Inputs$finish)
    return(NULL)
  
  mat   = Inputs$mat
  wavl  = Inputs$wavl
  delay = Inputs$delay
  
  if(!is.finite(diff(range(wavl ))) |
     !is.finite(diff(range(delay))) |
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

observeEvent(
  input$dataSave,
  isolate({
    mat <- Inputs$mat
    wavl <- Inputs$wavl
    delay <- Inputs$delaySave
    delay = c("wavl",paste0(delay))
    print(str(Inputs$delay))
    print(str(Inputs$delaySave))
    
    write.table(
      cbind(wavl, t(mat)),
      file =
        file.path(
          "outputDir",
          paste0(
            input$projectTag,
            "_Matrix.csv"
          )
        ),
      sep = ",",
      row.names = FALSE,
      col.names = delay
    )
  })
)
