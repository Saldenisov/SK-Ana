function(input, output, session) {
  
  # Attempt at bookmarking: failed !!!
  # setBookmarkExclude("bookmark") 
  # observeEvent(input$bookmark, {
  #   session$doBookmark()
  # })
  
# Initialize ####
  if(!dir.exists("outputDir"))
     dir.create("outputDir",showWarnings = FALSE)
  
  projConfig = NULL
  S0_in      = NULL
  RawData    = NULL
  masksDl    = c()
  masksWl    = c()
  
  Inputs = reactiveValues(
    gotData        = FALSE,
    process        = FALSE,
    validData      = TRUE,
    fileOrig       = NULL,
    matOrig        = NULL,
    wavlOrig       = NULL,
    delayOrig      = NULL,
    dlScaleFacOrig = NULL,
    delayMask      = NA,
    wavlMask       = NA,
    maskSpExp      = NA,
    mat            = NULL,
    wavl           = NULL,
    delay          = NULL,
    delaySave      = NULL,  # True delays used in saved kinetics
    delayId        = NA,    # Reference to original matrices when tiled
    delayGlitch    = NA     # List of glitches to mask
  )
  
  
  checkInputsSanity = function() {
    listIn = reactiveValuesToList(Inputs)
    nulVec = unlist(lapply(listIn, is.null))
    noNull = !any(nulVec)
    return(noNull)
  }
  
  reshapeCS <- function(U,V,n) {
    # Expand vectors wrt masks
    C = matrix(NA,nrow=length(Inputs$delay),ncol=n)
    S = matrix(NA,nrow=length(Inputs$wavl) ,ncol=n)
    i=0
    for(j in 1:nrow(C)) {
      if(!is.na(Inputs$delayMask[j])) {
        i = i+1
        C[j,] = U[i,1:n]
      } 
    }
    i=0
    for(j in 1:nrow(S)) {
      if(!is.na(Inputs$wavlMask[j])) {
        i = i+1
        S[j,] = V[i,1:n]
      } 
    }
    return(list(C=C,S=S))
  }

  updateSlider <- function (inputId, range, value, nsteps) {
    # Wrapper for generic function
    updateSliderInput(
      session,
      inputId = inputId,
      min     = range[1],
      max     = range[2],
      value   = value,
      step    = signif(diff(range)/nsteps, 3)
    )
  }
  
  initSliders <- function(config=NULL) {
    
    wavl  = Inputs$wavlOrig
    delay = Inputs$delayOrig/Inputs$dlScaleFacOrig
    mat   = Inputs$matOrig

    # Range of sliders
    doRange = signif(range(mat,na.rm=TRUE),2)
    wlRange = signif(range(wavl),3)
    wlMask  = signif(range(wavl),3)
    wlCut   = signif(range(wavl),3)
    dlRange = signif(range(delay),3)
    dlMask  = signif(range(delay),3)
    dlCut   = signif(range(delay),3)
    cblRange= c(0,length(delay))
    
    # Values of sliders
    if(!is.null(config)) {
      # Restore from project
      doRangeSel = config$keepDoRange
      wlRangeSel = config$keepWlRange
      # wlMaskSel1 = config$keepWlMask1
      wlCutSel   = config$keepWlCut
      dlRangeSel = config$keepDlRange
      # dlMaskSel1 = config$keepDlMask1
      dlCutSel   = config$keepDlCut
      cblSel     = config$keepCbl
    } else {
      # Initialize
      doRangeSel = as.vector(quantile(mat,probs = c(0.001,0.999),
                                      na.rm = TRUE))
      wlRangeSel = wlRange
      wlCutSel   = signif(mean(wlCut),3)
      dlRangeSel = dlRange
      dlCutSel   = signif(mean(dlCut),3)
      cblSel     = cblRange[1]
    }

    # DO slider
    updateSlider("keepDoRange", doRange, doRangeSel, 200)

    # Wavelength sliders
    nsteps = min(length(wavl),200)
    updateSlider("keepWlRange", wlRange, wlRangeSel, nsteps)
    updateSlider("keepWlCut"  , wlCut  , wlCutSel  , nsteps)
    updateNumericInput(session = session,
                       inputId = "nMasksWl",
                       value   = 0)
    
    # Delay sliders
    nsteps = min(length(delay),500)
    updateSlider("keepDlRange", dlRange, dlRangeSel, nsteps)
    updateSlider("keepDlCut"  , dlCut  , dlCutSel  , nsteps)
    updateNumericInput(session = session,
                       inputId = "nMasksDl",
                       value   = 0)
    
    # Baseline correction slider
    nsteps = round(diff(cblRange)/10)
    updateSlider("keepCbl"    , cblRange, cblSel   , nsteps)

    # Update Reporting
    updateCheckboxGroupInput(session,
                             inputId = 'inReport',
                             selected = c('SVD'))
    # Empty glitches
    Inputs$delayGlitch  <<- NA
    
  }
  getOneMatrix <- function(dataFile) {
    wavl = try(
      as.numeric(
        as.vector(
          read.table(
            dataFile, 
            nrows = 1,
            header = input$header, 
            sep = input$sep,
            stringsAsFactors = FALSE,
            dec = input$dec,
            fileEncoding = "ISO-8859-1",
            quote=""
          )
        )
      )[-1],
      silent = TRUE
    )
    if(class(wavl) == 'try-error' || length(wavl) == 0) 
      return(NULL) 
    
    mat = try(
      read.table(
        dataFile, 
        header = input$header, 
        skip = 1,
        dec = input$dec, 
        sep = input$sep,
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
    if(input$compFac >= 2) {
      dsm = downsizeMatrix(delay,wavl,mat,fw=input$compFac)
      mat   = dsm$mat
      delay = dsm$delay
      wavl  = dsm$wavl
      rm(dsm)
    }
    
    # Transpose if necessary
    if(input$datStr != 'dxw') {
      mat   = t(mat)
      tmp   = delay
      delay = wavl
      wavl  = tmp
    }
    
    return(list(mat=mat, wavl=wavl, delay=delay, 
                delaySave=delay, delayId= rep(1,length(delay))))
    
  }
  getRawData <- function (fileNames) {
    
    # (Re)initialize data tables
    Inputs$gotData  <<- FALSE
    Inputs$validData<<- TRUE    # Data type assumed correct
    RawData         <<- list()  # Init list in upper environment
    Inputs$fileOrig <<- NULL    # Invalidate earlier data
    Inputs$process  <<- FALSE   # Invalidate earlier processing
    
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
      if (!is.null(O)) 
        O$name = fName
      else {
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
  doMeanMatrix  <- function(sel) {
    
    # Assume all matrices are on same grid
    delay = RawData[[1]]$delay
    wavl  = RawData[[1]]$wavl
  
    # Load all matrices, recast them in the full coords
    matTab=array(NA,dim=c(length(sel),length(delay),length(wavl)))
    for (i in 1:length(sel)) {
      j = sel[i]
      matm = RawData[[j]]$mat
      wav  = RawData[[j]]$wavl
      del  = RawData[[j]]$delay
      matTab[i,,] =  matm
    }
    matTab[!is.finite(matTab)] = NA
    
    # Take the mean 
    matm = sigma = matrix(NA,ncol=length(wavl),nrow=length(delay))
    for (i in 1:dim(matm)[1]) {
      for (j in 1:dim(matm)[2]) {
        v = matTab[,i,j]
        v = v[!is.na(v)]
        if(length(v) >=1) {
          effData    = outliers::rm.outlier(v)
          matm[i,j]  = mean(effData,na.rm = TRUE)
        }
      }
    } 
    matm[!is.finite(matm)] = 0
    delay = 1:length(delay) # Replace by ordinal scale
    
    return(list(mat=matm, wavl=wavl, delay=delay, 
                delaySave=delay, delayId= rep(1,length(delay))))
  }
  doTileMatrix  <- function(sel, tileDel=TRUE) {
    nbFiles = length(sel)
    for (i in 1:nbFiles) {
      j     = sel[i]
      mat1  = RawData[[j]]$mat
      wav1  = RawData[[j]]$wavl
      del1  = RawData[[j]]$delay
      delS1 = RawData[[j]]$delaySave
      
      if(i==1) {    
        mat       = mat1
        delay     = del1
        wavl      = wav1
        delaySave = delS1
        delayId   = rep(i,length(del1))
        
      } else {
        if(tileDel) {
          # Tile matrices by delay (row)
          delay     = c(delay,del1)
          delayId   = c(delayId,rep(i,length(del1)))
          mat       = rbind(mat,mat1)      
          delaySave = c(delaySave,delS1)
        } else {
          # Tile matrices by wavl (col)
          wavl = c(wavl,wav1)
          # wavl = 1:length(wavl) # Replace by ordinal scale
          mat  = cbind(mat,mat1)      
        }
      }
      delay = 1:length(delay) # Replace by ordinal scale
    }
    # Order wavl by increasing value
    sel  = order(wavl)
    wavl = wavl[sel]
    mat  = mat[,sel]
    
    return(list(mat=mat, wavl=wavl, delay=delay, 
                delaySave=delaySave, delayId = delayId))
  }
  combineMatrix <- function(sel){
    if(is.null(sel)) 
      return(NULL)
    
    if(length(sel) == 1) {
      if(is.null(RawData[[sel]]))
        return(NULL)
      list(
        mat       = RawData[[sel]]$mat, 
        delay     = 1:length(RawData[[sel]]$delay), 
        wavl      = RawData[[sel]]$wavl, 
        delaySave = RawData[[sel]]$delay,
        delayId   = rep(1,length(RawData[[sel]]$delay))
      )
    } else {
      switch( input$procMult,
              avrg    = doMeanMatrix(sel),
              tileWav = doTileMatrix(sel,tileDel=FALSE),
              tileDel = doTileMatrix(sel,tileDel=TRUE)
      )
    }
  }
## Finish matrix ####
  finishMatrix  <- reactive({
    if(!Inputs$process)
      return(NULL)
    
    data = combineMatrix(input$rawData_rows_selected)

    validate(
      need(!is.null(data),"--> Bad data type")
    )
    
    fw = input$postCompFac
    if(fw >= 2) {
      dls = data$delay
      dId = data$delayId
      data = downsizeMatrix(data$delaySave,data$wavl,data$mat,fw=fw)
      data$delaySave = data$delay
      data$delay     = 1:length(data$delay)
      data$delayId   = dId[seq(1,length(dls),by=fw)]
      # If necessary, adjust delayId
      if(length(data$delayId) < length(data$delay)) 
        data$delayId[(length(data$delayId)+1):length(data$delay)] =
        data$delayId[length(data$delayId)]
      else if(length(data$delayId) > length(data$delay))
        data$delayId = data$delayId[1:length(data$delay)]
    }
    
    if(is.null(data)) {
      Inputs$fileOrig       <<- NULL
      
    } else {
      isolate({
        # ckeck for load errors
        loadError  = FALSE
        loadErrMsg = ""
        # if(is.null(data)) {
        #   loadError  = TRUE
        #   loadErrMsg = "Please select data file(s)..."  
        # }
        if ( length(data$wavl) == 0 || 
             !is.finite(diff(range(data$wavl))) ||
             is.na(data$wavl)){
          loadError  = TRUE
          loadErrMsg = "wavl"  
        }
        if ( length(data$delay) == 0 || 
             !is.finite(diff(range(data$delay))) ){
          loadError  = TRUE
          loadErrMsg = paste0(loadErrMsg,", delay")
        }
        if (!is.numeric(data$mat) ||
            !is.matrix(data$mat)    ){
          loadError  = TRUE
          loadErrMsg = paste0(loadErrMsg,", matrix")
        }
        
        if(loadError) {
          output$loadErrorNew <- renderUI({
            msg1 = paste0("--> Improperly formatted ", loadErrMsg)
            msg2 = "--> Check the header, delimiter or decimal marker"
            list(
              div(strong('Error in loading data'), style = "color:red"),
              div(msg1, style = "color:red"),
              div(msg2, style = "color:red")
            )
          })
          Inputs$fileOrig  <<- NULL
          
        } else {
          if(input$projectTag == '') {
            if(nrow(input$dataFile) == 1) {
              projName = substr(
                tools::file_path_sans_ext(
                  input$dataFile$name
                ),
                1,14)
            } else {
              prefix = 'Mean_'
              if( input$procMult != 'avrg') prefix = 'Tile_'
              projName = paste0(prefix,
                                substr(tools::file_path_sans_ext(
                                  input$dataFile$name[1]
                                ),1,8))
            }
            updateTextInput(session,
                            inputId = "projectTag",
                            value   = projName)
          }
          
          # Scale factor for neater delay selectors
          dlScaleFac = 1 #10^(floor(log10(diff(range(data$delay)))-1))

          # Install data
          Inputs$fileOrig       <<- input$dataFile$name
          Inputs$dlScaleFacOrig <<- dlScaleFac

          Inputs$matOrig        <<- data$mat
          Inputs$wavlOrig       <<- data$wavl
          Inputs$delayOrig      <<- data$delay
          Inputs$delayIdOrig    <<- data$delayId
          Inputs$delaySaveOrig  <<- data$delaySave
          
          Inputs$mat            <<- data$mat
          Inputs$wavl           <<- data$wavl
          Inputs$delay          <<- data$delay
          Inputs$delayId        <<- data$delayId
          Inputs$delaySave      <<- data$delaySave

          # Initialize config
          initSliders()
          projConfig <<- NULL
        }
        
      })
    }
  })
  
  # observeEvent(
  #   input$postCompFac,
  #   isolate({
  #     Inputs$process <<- TRUE
  #     finishMatrix()
  #   })
  # )
  
  
# Project ####
  
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
                datStr= "dxw"
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
      ok_wavl  = length(unique(nwavl)) == 1
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
  



  # # Open saved project
  # observeEvent(
  #   input$projectFile,
  #   isolate({
  #     load(input$projectFile$datapath)
  #     
  #     updateTextInput(session,
  #                     inputId = "projectTag",
  #                     value = strsplit(
  #                       input$projectFile$name,
  #                       ".",fixed=TRUE)[[1]][1]
  #                     )
  # 
  #     # Install data
  #     Inputs$fileOrig       <<- data$fileOrig
  #     Inputs$matOrig        <<- data$matOrig
  #     Inputs$wavlOrig       <<- data$wavlOrig
  #     Inputs$delayOrig      <<- data$delayOrig
  #     Inputs$delayIdOrig    <<- data$delayIdOrig
  #     Inputs$delaySaveOrig  <<- data$delaySaveOrig
  #     Inputs$dlScaleFacOrig <<- data$dlScaleFacOrig
  #     Inputs$mat            <<- data$matOrig
  #     Inputs$wavl           <<- data$wavlOrig
  #     Inputs$delay          <<- data$delayOrig
  #     Inputs$delaySave      <<- data$delaySaveOrig
  #     Inputs$delayId        <<- data$delayIdOrig
  #     
  #     # Restore project config
  #     initSliders(config)
  #     projConfig <<- config
  #     
  #   })
  # )
  
  output$projectInfoNew <- renderUI({
    if(!Inputs$process)
      return(NULL)
    HTML(paste0(
      '<b>Global matrix</b>: ',
      length(Inputs$delayOrig),'x', length(Inputs$wavlOrig),'<br>',
      'O.D.  range: ',paste0(signif(range(Inputs$mat)      ,2),collapse=', '),'<br>',
      'Delay range: ',paste0(signif(range(Inputs$delayOrig),4),collapse=', '),'<br>',
      'Wavl  range: ',paste0(signif(range(Inputs$wavlOrig), 4),collapse=', ')
      )
    )
  })
  output$showPIN = reactive({
      Inputs$process &&
      length(input$rawData_rows_selected) != 0
  })
  outputOptions(output, "showPIN", suspendWhenHidden = FALSE)
  output$vignette <- renderPlot({
    if(!Inputs$process)
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
    par(cex = cex, mar = mar)
    image(
      delay,wavl,mat,
      xlab = 'Delay',ylab = 'Wavelength',
      col = cols, 
      zlim = quantile(mat,probs = c(0.001,0.999),
                      na.rm = TRUE)
    )
  })
  

#   output$projectInfoOpen <- renderPrint({
# # BETTER EXTENSION: Rda ????
#     validate(
#       need(
#         !is.null(input$projectFile), 
#         "Please select a project file (*.ska)"
#       )
#     )
#     
# # TO BE DONE PROPERLY....
#     # Check file read
#     cat(paste0('Project: ',input$projectTag,'\n\n'))
#     cat('Data File(s):\n')
#     cat(paste0(Inputs$fileOrig,'\n'))
#     cat("\n")
#     cat(paste0(
#       'Matrix: ',
#       length(Inputs$delayOrig),'x',
#       length(Inputs$wavlOrig),'\n'
#     ))
#     cat('Delay range: ',range(Inputs$delayOrig),'...\n')
#     cat('Wavl  range: ',range(Inputs$wavlOrig),'...\n')
#     
#     cat("Sanity:",checkInputsSanity(),'\n')
#     
#   })
#   
#   output$saveProject <- downloadHandler(
# # BETTER EXTENSION: Rda ????
#     filename = function()    {
#       paste0(input$projectTag,'.ska')
#     },
#     content  = function(con) {
#       # Collect congigutation parameters
#       ll = reactiveValuesToList(input)
#       sel = grepl('^keep',names(ll))
#       config = ll[sel]
#       # Collect data
#       ll = reactiveValuesToList(Inputs)
#       sel = grepl('Orig',names(ll))
#       data = ll[sel]
#       
#       save(config, data, 
#            file     = con, 
#            compress = 'gzip')
#     }
#   )
  
# Select Area ####

  observeEvent(
    input$reset,
    initSliders()
  )
  
  observeEvent(
    input$saveSelectors,
    isolate({
      ll = list()
      # ID
      ll$projectTag     = input$projectTag
      ll$matDims        = dim(Inputs$matOrig)
      # Selectors
      ll$keepCbl        = input$keepCbl
      ll$keepDlRange    = input$keepDlRange
      ll$keepWlRange    = input$keepWlRange
      ll$dlScaleFacOrig = Inputs$dlScaleFacOrig
      # Masks
      ll$nMasksWl       = input$nMasksWl
      if(input$nMasksWl!=0)
        for (mask in 1:input$nMasksWl) {
          maskName = paste0("keepWlMask",mask)
          ll[[maskName]] = input[[maskName]]
        }
      ll$nMasksDl       = input$nMasksDl
      if(input$nMasksDl!=0)
        for (mask in 1:input$nMasksDl) {
          maskName = paste0("keepDlMask",mask)
          ll[[maskName]] = input[[maskName]]
        }
      ll$delayGlitch   = Inputs$delayGlitch
      # Save
      file = file.path(
        "outputDir",
        paste0(input$projectTag,'_Selections.Rda')
      )
      save(ll,file = file)
      showModal(modalDialog(
        title = ">>>> Selections Saved <<<< ",
        paste0("File:",file), 
        footer = modalButton("Close"),
        easyClose = TRUE, fade = TRUE, size = 'm'
      ))
    })
  )
  
  observeEvent(
    input$selectorFile,
    isolate({
      # Get data
      load(input$selectorFile$datapath)
      
      # Check project name and matrix dims
      if(ll$projectTag != input$projectTag    ||
        ll$matDims    != dim(Inputs$matOrig) ) {
        showModal(modalDialog(
          title = ">>>> Incorrect File <<<< ",
          paste0("The chosen selections file does not match ",
                 "the current project!"), 
          footer = modalButton("Close"),
          easyClose = TRUE, fade = TRUE, size = 'm'
        ))
        
      } else {
        Inputs$dlScaleFacOrig <<- ll$dlScaleFacOrig
        Inputs$delayGlitch    <<- ll$delayGlitch
        
        # Ranges of sliders
        wavl  = Inputs$wavlOrig
        delay = Inputs$delayOrig/Inputs$dlScaleFacOrig
        mat   = Inputs$matOrig
        wlRange = signif(range(wavl),3)
        dlRange = signif(range(delay),3)
        cblRange= c(0,length(delay))
        
        nsteps = min(length(wavl),200)
        wlRangeSel = ll$keepWlRange
        updateSlider("keepWlRange", wlRange, wlRangeSel, nsteps)
        
        nsteps = min(length(delay),500)
        dlRangeSel = ll$keepDlRange
        updateSlider("keepDlRange", dlRange, dlRangeSel, nsteps)
        
        nsteps = round(diff(cblRange)/10)
        cblSel = ll$keepCbl
        updateSlider("keepCbl"    , cblRange, cblSel   , nsteps)      
        
        # Wavl masks
        ## Remove existing Masks sliders
        for (mask in 1:20) {
          maskName = paste0("keepWlMask",mask)
          if( maskName %in% masksWl ) {
            removeUI(
              selector = paste0("#",maskName),
              immediate = TRUE
            )
            masksWl <<- masksWl[-which(masksWl == maskName)]
          }
        }  
        
        ## Generate Masks sliders if necessary
        nMasks = ll$nMasksWl
        if(nMasks!=0) {
          nsteps  = min(length(wavl),200)
          for (mask in 1:nMasks) {
            maskName = paste0("keepWlMask",mask)
            insertUI(
              selector = "#masksS",
              where    = "beforeEnd",
              ui = tags$div(
                id = maskName,
                sliderInput(
                  inputId = maskName, 
                  label   = NULL,
                  min     = wlRange[1],
                  max     = wlRange[2],
                  value   = ll[[maskName]],
                  step    = signif(diff(wlRange)/nsteps, 3),
                  sep     = "")
              )
            )
            masksWl <<- unique(c(masksWl,maskName))
          }
        }
        # if(nMasks != input$nMasksWl)
          updateNumericInput(session = session,
                             inputId = "nMasksWl",
                             value   = nMasks)
        
        # Delay masks
        ## Remove existing Masks sliders
        for (mask in 1:20) {
          maskName = paste0("keepDlMask",mask)
          if( maskName %in% masksDl ) {
            removeUI(
              selector = paste0("#",maskName),
              immediate = TRUE
            )
            masksDl <<- masksDl[-which(masksDl == maskName)]
          }
        }  
        
        ## Generate Masks sliders if necessary
        nMasks = ll$nMasksDl
        if(nMasks!=0) {
          nsteps  = min(length(delay),500)
          for (mask in 1:nMasks) {
            maskName = paste0("keepDlMask",mask)
            insertUI(
              selector = "#masksC",
              where    = "beforeEnd",
              ui = tags$div(
                id = maskName,
                sliderInput(
                  inputId = maskName, 
                  label   = NULL,
                  min     = dlRange[1],
                  max     = dlRange[2],
                  value   = ll[[maskName]],
                  step    = signif(diff(dlRange)/nsteps, 3),
                  sep     = "")
              )
            )
            masksDl <<- unique(c(masksDl,maskName))
          }
        }
        # if(nMasks != input$nMasksDl)
          updateNumericInput(session = session,
                             inputId = "nMasksDl",
                             value   = nMasks)
        
      }


      
    })
  )
  
  selectArea <- reactive({
    if (!checkInputsSanity())
      return(NULL)
  
    delay     = Inputs$delayOrig
    wavl      = Inputs$wavlOrig
    mat       = Inputs$matOrig
    delayId   = Inputs$delayIdOrig
    delaySave = Inputs$delaySaveOrig
    
    # Correct baseline
    if (input$keepCbl > 2) {
      mat =
        matrix(
          unlist(
            apply(
              mat,
              2,
              function(x) {
                x - mean(x[1:input$keepCbl],na.rm = TRUE)
              }
            )
          ),
          ncol = ncol(mat),byrow = FALSE
        )
    }
  
    # Select work area
    xlim = input$keepDlRange * Inputs$dlScaleFacOrig
    ylim = input$keepWlRange
    
    subX = delay >= xlim[1] & delay <= xlim[2]
    subY = wavl  >= ylim[1] & wavl  <= ylim[2]
    
    delay     = delay[subX]
    delayId   = delayId[subX]
    wavl      = wavl[subY]
    mat       = mat[subX,subY]
    delaySave = delaySave[subX]
    
    Inputs$delay     <<- delay
    Inputs$delayId   <<- delayId
    Inputs$delaySave <<- delaySave
    Inputs$wavl      <<- wavl
 
    # Aggregate and apply masks
    delayMask = rep(0,length(delay))
    wavlMask  = rep(0,length(wavl))
    if(input$nMasksDl!=0)
      for (mask in 1:input$nMasksDl) {
        maskName = paste0("keepDlMask",mask)
        xlim = input[[maskName]] * Inputs$dlScaleFacOrig
        if(length(xlim)!=0)
          if (diff(xlim) != 0) {
            sel = delay >= xlim[1] & delay <= xlim[2]
            if(sum(sel)!=0) delayMask[sel]  = NA
          }
      }
    if(input$nMasksWl!=0)
      for (mask in 1:input$nMasksWl) {
        maskName = paste0("keepWlMask",mask)
        ylim = input[[maskName]]
        if(length(ylim)!=0)
          if (diff(ylim) != 0) {
            sel = wavl >= ylim[1] & wavl <= ylim[2]
            if(sum(sel)!=0) wavlMask[sel]  = NA
          }
      }
    if(!anyNA(Inputs$delayGlitch)) 
      for (i in 1:length(Inputs$delayGlitch))
        delayMask[which(delay == Inputs$delayGlitch[i])] = NA
    
    Inputs$delayMask <<- delayMask
    Inputs$wavlMask  <<- wavlMask
    
    mat[is.na(delayMask),] = NA
    mat[,is.na(wavlMask)]  = NA
    
    Inputs$mat   <<- mat
    
    # Automatic ajustment of DO range
    updateSlider("keepDoRange", 
                 signif(range(Inputs$matOrig,na.rm=TRUE),3), 
                 signif(range(mat,na.rm = TRUE),3), 
                 200)
    
  })
  
  ## Manage masksDl ####
  observeEvent(
    input$nMasksDl,
    isolate({
      nsteps  = min(length(Inputs$delayOrig),500)
      dlRange = signif(range(Inputs$delayOrig/Inputs$dlScaleFacOrig),3)

      if(input$nMasksDl!=0) {
        # Add new slider(s) if required
        for (mask in 1:input$nMasksDl) {
          maskName = paste0("keepDlMask",mask)
          if( !(maskName %in% masksDl) ) {
            insertUI(
              selector = "#masksC",
              where    = "beforeEnd",
              ui = tags$div(
                id = maskName,
                sliderInput(
                  inputId = maskName, 
                  label   = NULL,
                  min     = dlRange[1],
                  max     = dlRange[2],
                  value   = c(dlRange[1],dlRange[1]),
                  step    = signif(diff(dlRange)/nsteps, 3),
                  sep     = "")
              )
            )
            masksDl <<- unique(c(masksDl,maskName))
          }
        }
      }
      # Remove extra sliders
      for (mask in (input$nMasksDl+1):15) {
        maskName = paste0("keepDlMask",mask)
        if( maskName %in% masksDl ) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksDl <<- masksDl[-which(masksDl == maskName)]
        }
      } 
    })
  )
  
  ## AutoDlMAsk ####
  observeEvent(
    input$autoDlMask,
    isolate({
      
      # Mask 1 area per input dataset
      nmat = 1
      if(!is.null(input$procMult))
        if(input$procMult == 'tileDel')
          nmat = length(input$rawData_rows_selected)
      
      # Get changepoints
      chgp = autoDlMask(Inputs$matOrig,nmat)
      if(!is.null(chgp)) {
        chgp = c(1,chgp)

        # Remove all sliders
        for (mask in 1:15) {
          maskName = paste0("keepDlMask",mask)
          if( maskName %in% masksDl ) {
            removeUI(
              selector = paste0("#",maskName),
              immediate = TRUE
            )
            masksDl <<- masksDl[-which(masksDl == maskName)]
          }
        }  
        
        # Generate sliders
        nsteps  = min(length(Inputs$delayOrig),500)
        dlRange = signif(range(Inputs$delayOrig/Inputs$dlScaleFacOrig),3)
        for (mask in 1:nmat) {
          maskName = paste0("keepDlMask",mask)
          sel   = c(chgp[2*(mask-1)+1],chgp[2*(mask-1)+2])
          value = Inputs$delayOrig[sel]/Inputs$dlScaleFacOrig
          insertUI(
            selector = "#masksC",
            where    = "beforeEnd",
            ui = tags$div(
              id = maskName,
              sliderInput(
                inputId = maskName, 
                label   = NULL,
                min     = dlRange[1],
                max     = dlRange[2],
                value   = value,
                step    = signif(diff(dlRange)/nsteps, 3),
                sep     = "")
            )
          )
          masksDl <<- unique(c(masksDl,maskName))
        }
      }
      
      if(nmat != input$nMasksDl)
        updateNumericInput(session = session,
                           inputId = "nMasksDl",
                           value=nmat)
      
      
      
    })
  )

  ## Manage MasksWl ####
  observeEvent(
    input$nMasksWl,
    isolate({
      nsteps  = min(length(Inputs$wavlOrig),200)
      wlRange = signif(range(Inputs$wavlOrig),3)
      
      if(input$nMasksWl!=0) {
        # Add new slider(s) if required
        for (mask in 1:input$nMasksWl) {
          maskName = paste0("keepWlMask",mask)
          if( !(maskName %in% masksWl) ) {
            insertUI(
              selector = "#masksS",
              where    = "beforeEnd",
              ui = tags$div(
                id = maskName,
                sliderInput(
                  inputId = maskName, 
                  label   = NULL,
                  min     = wlRange[1],
                  max     = wlRange[2],
                  value   = c(wlRange[1],wlRange[1]),
                  step    = signif(diff(wlRange)/nsteps, 3),
                  sep     = "")
              )
            )
            masksWl <<- unique(c(masksWl,maskName))
          }
        }
      }
      # Remove extra sliders
      for (mask in (input$nMasksWl+1):15) {
        maskName = paste0("keepWlMask",mask)
        if( maskName %in% masksWl ) {
          removeUI(
            selector = paste0("#", maskName),
            immediate = TRUE
          )
          masksWl <<- masksWl[-which(masksWl == maskName)]
        }
      } 
    })
  )
  
  ## AutoWlMAsk ####
  observeEvent(
    input$autoWlMask,
    isolate({
      
      # TO BE UPDATED IF Wavl tiling...
      nmat = 1
      nmasks = 0
      
      # Get changepoints
      chgp = autoWlMask(Inputs$matOrig,nmat)
      if(!is.null(chgp)) {
        
        chgp = c(1,chgp,nrow(Inputs$matOrig)) # valid if no tiling ???

        # Remove all sliders
        for (mask in 1:15) {
          maskName = paste0("keepWlMask",mask)
          if( maskName %in% masksWl ) {
            removeUI(
              selector = paste0("#",maskName),
              immediate = TRUE
            )
            masksWl <<- masksWl[-which(masksWl == maskName)]
          }
        }  
        
        # Generate sliders
        nmasks = length(chgp)-2
                    
        nsteps  = min(length(Inputs$wavlOrig),200)
        wlRange = range(Inputs$wavlOrig)
        for (mask in 1:nmasks ) {
          maskName = paste0("keepWlMask",mask)
          sel   = c(chgp[2*(mask-1)+1],chgp[2*(mask-1)+2])
          value = Inputs$wavlOrig[sel]
          insertUI(
            selector = "#masksS",
            where    = "beforeEnd",
            ui = tags$div(
              id = maskName,
              sliderInput(
                inputId = maskName, 
                label   = NULL,
                min     = signif(wlRange[1],3),
                max     = signif(wlRange[2],3),
                value   = signif(value,3),
                step    = signif(diff(wlRange)/nsteps, 3),
                sep     = "")
            )
          )
          masksWl <<- unique(c(masksWl,maskName))
        }
      }
      
      if(nmasks != input$nMasksWl)
        updateNumericInput(session = session,
                           inputId = "nMasksWl",
                           value   = nmasks)
      
    })
  )
  
  colorizeMask1D = function(axis='delay', dir='v', 
                            ylim=NULL, eps=1e-4) {
    
    if(axis == 'delay') {
      mask   = Inputs$delayMask
      values = Inputs$delay
    } else {
      mask   = Inputs$wavlMask
      values = Inputs$wavl
    }
    masked = which(is.na(mask))
    
    # Remove extremities
    sel = masked != 1 & masked != length(values)
    masked = sort(masked[sel],decreasing = FALSE)
    
    for(i in masked) {
      if(is.na(mask[i-1])) 
        x1 = values[i] # Prevent overlap of rectangles
      else
        x1 = values[i-1]
      
      x2 = values[i+1]
      
      if(dir == 'v')
        rect(x1+eps,ylim[1],x2-eps,ylim[2],
             border=NA,col=mask_tr)
      else
        rect(ylim[1],x1+eps,ylim[2],x2-eps,
             border=NA,col=mask_tr)
    }
  }

  rangesImage1 <- reactiveValues(x = NULL, y = NULL)
  
  output$image1 <- renderPlot({
    if(is.null(selectArea())) 
      return(NULL)

    mat   = Inputs$mat
    wavl  = Inputs$wavl
    delay = Inputs$delay
    
    if(!is.finite(diff(range(wavl)))  ||
       !is.finite(diff(range(delay))) ||
       !is.finite(diff(range(mat,na.rm=TRUE)))   ) {
      plot(1:10,1:10,type='n')
      text(x=5,y=5,labels='Data not ready...',col=2)
      
    } else {

      if(is.null(rangesImage1$x))
        xlim = range(delay)
      else
        xlim = rangesImage1$x
      
      if(is.null(rangesImage1$y))
        ylim = range(wavl)
      else
        ylim = rangesImage1$y
      
      par(cex = cex, mar = mar)
      image(
        delay,wavl,mat,
        xlab = 'Delay',ylab = 'Wavelength',
        col = cols,
        xlim = xlim,
        ylim = ylim,
        zlim = input$keepDoRange
      )
      
      abline(
        v = input$keepDlCut * Inputs$dlScaleFacOrig,
        lwd = 2,col = 'orange',lty = 2
      )
      abline(
        h = input$keepWlCut,
        lwd = 2,col = 'orange',lty = 2
      )
      
      if(input$keepCbl !=0) {
        rect(Inputs$delayOrig[1],
             Inputs$wavlOrig[1],
             Inputs$delayOrig[input$keepCbl],
             Inputs$wavlOrig[length(Inputs$wavlOrig)],
             border=NA,
             col=pink_tr
        )
      }
      
      # Show masks
      colorizeMask1D(axis="delay",ylim=ylim)
      colorizeMask1D(axis="wavl", dir='h', ylim=xlim)
    }
  })
  outputOptions(output, "image1",
                suspendWhenHidden = FALSE)
  
  observeEvent(input$image1_dblclick, {
    brush <- input$image1_brush
    if (!is.null(brush)) {
      rangesImage1$x <- c(brush$xmin, brush$xmax)
      rangesImage1$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesImage1$x <- NULL
      rangesImage1$y <- NULL
    }
  })

  output$transects <- renderPlot({
    if(is.null(selectArea())) 
      return(NULL)
    
    mat   = Inputs$mat
    wavl  = Inputs$wavl
    delay = Inputs$delay
    
    if(!is.finite(diff(range(wavl)))  ||
       !is.finite(diff(range(delay))) ||
       !is.finite(diff(range(mat,na.rm=TRUE)))   ) {
      plot(1:10,1:10,type='n')
      text(x=5,y=5,labels='Data not ready...',col=2)
      
    } else {
      
      if(is.null(rangesImage1$x))
        xlim = range(delay)
      else
        xlim = rangesImage1$x
      
      if(is.null(rangesImage1$y))
        ylim = range(wavl)
      else
        ylim = rangesImage1$y
      
      par(mfrow=c(2,1))
      
      par(cex = cex, mar = mar)
      # Locally Averaged Spectrum
      dCut = input$keepDlCut * Inputs$dlScaleFacOrig
      iCut = indxCuts(dCut,delay)
      indx = iCut$indx
      delta= iCut$delta
      if(length(indx)==1) {
        cutMean = mat[indx,]
      } else {
        cutMean = colMeans(mat[indx,])
      }
      if(all(is.na(cutMean))) cutMean=cutMean*0
      matplot(
        wavl,cutMean,type = 'l',col = 'orange', lwd=2,
        xlab = 'Wavelength', ylab = 'O.D.',
        xlim = ylim,
        ylim = input$keepDoRange, yaxs='i',
        main = paste0('Mean O.D. at delay: ',signif(mean(delay[indx]),3),
                      ifelse(delta==0,
                             '',
                             paste0(' +/- ',signif(delta / 2,2))
                      )
        )
      )
      abline(h = 0,lty = 2)
      colorizeMask1D(axis="wavl",ylim=input$keepDoRange)
      grid();box()
      
      par(cex = cex, mar = mar)
      # Locally Averaged Kinetics
      dCut = input$keepWlCut
      iCut = indxCuts(dCut,wavl)
      indx = iCut$indx
      delta= iCut$delta
      if(length(indx)==1) {
        cutMean = mat[,indx]
      } else {
        cutMean = rowMeans(mat[,indx])
      }
      if(all(is.na(cutMean))) cutMean=cutMean*0
      matplot(
        delay,cutMean,type = 'l', col = 'orange', lwd=2,
        xlab = 'Delay', ylab = 'O.D.',
        xlim = xlim,
        ylim = input$keepDoRange, yaxs='i',
        main = paste0('Mean O.D. at wavl: ',signif(mean(wavl[indx]),3),
                      ifelse(delta==0,
                             '',
                             paste0(' +/- ',signif(delta / 2,2))
                      )
        )
      )
      abline(h = 0,lty = 2)
      colorizeMask1D(axis="delay",ylim=input$keepDoRange)
      grid();box()
          }
  })
  outputOptions(output, "transects",
                suspendWhenHidden = FALSE)
  
  rangesDl <- reactiveValues(x = NULL, y = NULL)
  
  output$cutsDl <- renderPlot({
    if(is.null(selectArea())) 
      return(NULL)

    mat   = Inputs$mat
    wavl  = Inputs$wavl
    delay = Inputs$delay
    
    wCut = seq(1,length(delay),
               by = max(1,input$stepDlCut)
               )

    if(is.null(rangesDl$y))
      ylim = input$keepDoRange
    else
      ylim = rangesDl$y
    
    par(cex = cex, mar = mar)
    matplot(
      wavl,t(mat[wCut,]),type = 'l', 
      xlim = rangesDl$x,
      ylim = ylim,
      xlab = 'Wavelength', ylab = 'DO',
      xaxs='i', yaxs='i'
    )
    colorizeMask1D(axis="wavl",ylim=ylim)
    grid();box()
  })

  observeEvent(input$cutsDl_dblclick, {
    brush <- input$cutsDl_brush
    if (!is.null(brush)) {
      rangesDl$x <- c(brush$xmin, brush$xmax)
      rangesDl$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesDl$x <- NULL
      rangesDl$y <- NULL
    }
  })

  rangesWl <- reactiveValues(x = NULL, y = NULL)
  
  output$cutsWl <- renderPlot({
    if(is.null(selectArea())) 
      return(NULL)
 
    mat   = Inputs$mat
    wavl  = Inputs$wavl
    delay = Inputs$delay
    
    dCut = seq(1,length(wavl),
               # by = ifelse(length(wavl ) >= 50 , 10, 1)
               by = max(1,input$stepWlCut)
    )
    
    if(is.null(rangesWl$y))
      ylim = input$keepDoRange
    else
      ylim = rangesWl$y
    
    par(cex = cex, mar = mar)
    matplot(
      delay,mat[,dCut],type = 'l', 
      xlim = rangesWl$x,
      ylim = ylim,
      xlab = 'Delay', ylab = 'DO',
      xaxs='i', yaxs='i'
    )
    colorizeMask1D(axis="delay",ylim=ylim)
    grid();box()
    if(input$keepCbl !=0) {
      rect(Inputs$delayOrig[1],
           ylim[1],
           Inputs$delayOrig[input$keepCbl],
           ylim[2],
           border=NA,
           col=pink_tr
      )
    }
    
  })

  observeEvent(input$cutsWl_dblclick, {
    brush <- input$cutsWl_brush
    if (!is.null(brush)) {
      rangesWl$x <- c(brush$xmin, brush$xmax)
      rangesWl$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesWl$x <- NULL
      rangesWl$y <- NULL
    }
  })
  
  observeEvent(input$wavlCutSave,
               isolate({
                 mat   = Inputs$mat
                 wavl  = Inputs$wavl
                 delay = Inputs$delay
                 dCut  = input$keepWlCut
                 indx  = indxCuts(dCut,wavl)$indx
                 if(length(indx)==1) {
                   cutMean = mat[,indx]
                 } else {
                   cutMean = rowMeans(mat[,indx])
                 }
                 write.csv(
                   cbind(delay,cutMean),
                   file =  
                     file.path("outputDir",
                               paste0(
                                 input$projectTag,
                                 '_wavlCut_',
                                 signif(mean(wavl[indx]),3),
                                 '.csv'
                               )),
                   row.names = FALSE
                 )
               }))
  
  observeEvent(input$delayCutSave ,
               isolate({
                 mat   = Inputs$mat
                 wavl  = Inputs$wavl
                 delay = Inputs$delay
                 dCut  = input$keepDlCut * Inputs$dlScaleFacOrig
                 indx  = indxCuts(dCut,delay)$indx
                 if(length(indx)==1) {
                   cutMean = mat[indx,]
                 } else {
                   cutMean = colMeans(mat[indx,])
                 }
                 write.csv(
                   cbind(wavl,cutMean),
                   file =  
                     file.path("outputDir",
                               paste0(
                                 input$projectTag,
                                 '_delayCut_',
                                 signif(mean(delay[indx]),3),
                                 '.csv'
                               )),
                   row.names = FALSE
                 )
                 
               }))
  
# SVD ####
  doSVD <- reactive({
    if (!checkInputsSanity())
      return(NULL)
    
    # Suppress masked areas
    mat = Inputs$mat 
    mat = mat[!is.na(Inputs$delayMask),]
    mat = mat[,!is.na(Inputs$wavlMask) ]
    
    nsvMax = min(10,
                 length(Inputs$delay),
                 length(Inputs$wavl)
                 )
    svd(mat,nu = nsvMax,nv = nsvMax)
  })

  ## Clean Up ####
  observeEvent(
    input$clean,
    {
      gl   = cleanUp(Inputs$delayMask,
                     Inputs$wavlMask,
                     Inputs$mat,
                     isolate(input$cleanLevel)
                     )
      dlgl = Inputs$delay[gl]
      if(anyNA(Inputs$delayGlitch))
        Inputs$delayGlitch  <<- dlgl
      else
        Inputs$delayGlitch  <<- unique(c(Inputs$delayGlitch,dlgl))
    }
  )
  observeEvent(
    input$cleanCancel,
    {
      if(!anyNA(Inputs$delayGlitch))
        Inputs$delayGlitch  <<- 
          Inputs$delayGlitch[-length(Inputs$delayGlitch)] 
    }
  )
  
  output$svdSV <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    
    par(mfrow=c(1,2),cex = cex, mar = mar, cex.lab=1.5)
    # S.V.
    plot(s$d[1:ncol(s$u)],ylab = "Singular Values",log = "y",
         pch=19,cex=1.5,col='orange',xlim=c(1,10))
    
    # Trend line for noise
    x = 10:20
    y = s$d[x]
    reg = lm(y~x)
    xpred = 1:20
    ypred = predict(reg,newdata=data.frame(x=xpred))
    lines(xpred,ypred,lwd=3,lty=2,col='gray60')
    
    lines(s$d,col = "blue",lwd=3,lty=3)
    points(s$d[1:ncol(s$u)],pch=19,cex=1.5,col='orange')
    grid();box()
    
    #L.o.F
    lof=c()
    mat  = Inputs$mat
    # Suppress masked areas
    mat = mat[!is.na(Inputs$delayMask),]
    mat = mat[,!is.na(Inputs$wavlMask) ]
    sMat = sum(mat^2)
    mat1 = rep(0,nrow=nrow(s$u),ncol=ncol(s$v))
    for (i in 1:10) {
      mat1  = mat1 + s$u[,i] %o% s$v[,i] * s$d[i]
      resid = mat - mat1
      lof[i] = 100*(sum(resid^2)/sMat)^0.5
    }
    
    plot(lof,ylab = "Lack of Fit (%)",log = "y",
         pch=19,cex=1.5,col='orchid')
    lines(lof,col = "darkgreen",lwd=3,lty=3)
    text(1:length(lof),lof,labels = signif(lof,3),col=2,pos=3)
    points(lof,pch=19,cex=1.5,col='orchid')
    grid();box()
  
  },height = 450)
  outputOptions(output, "svdSV",
                suspendWhenHidden = FALSE)

  plotSVDVecBloc <- function (C,S,axisC,axisS,...) {
    par(cex = cex,cex.main=cex)
    nco = 2
    n   = min(floor(ncol(C)/2),5)
    fh = 0.18
    
    for(icol in 1:nco) {
      ylim=range(c(C[,((icol-1)*n+1):(icol*n)],
                   S[,((icol-1)*n+1):(icol*n)]),
                 na.rm=TRUE)
      for (i in 1:n) {
        if(i==n) {
          xlab1 = 'Wavelength'
          xlab2 = 'Delay'
          xaxt  = 's'
          mar1  = c(4.2,4,0,0)
          mar2  = c(4.2,0,0,1.2)
        } else {
          xlab1 = ''
          xlab2 = ''
          xaxt  = 'n'
          mar1  = c(0,4,0,0)
          mar2  = c(0,0,0,1.2)
        }
        par( fig = c((icol-1)*0.5,
                     (icol-1)*0.5+0.27,
                     max(0,1-fh*i - ifelse(i==n,0.11,0)),
                     1-fh*(i-1)), 
             new=ifelse(i*icol==1,FALSE,TRUE),
             mar=mar1)
        plot(
          axisS,S[,(icol-1)*n+i],type = "l",col = 'darkgreen',
          xlab = xlab1, ylab = 'Arb. units',
          xaxt=xaxt, ylim=ylim, lwd=1
        )
        abline(h = 0,lty=2)
        colorizeMask1D(axis="wavl",ylim=ylim)
        grid()
        legend('topleft',legend=paste0((icol-1)*n+i),bty='n')
        
        par( fig = c((icol-1)*0.5+0.27,
                     (icol-1)*0.5+0.5,                     
                     max(0,1-fh*i - ifelse(i==n,0.11,0)),
                     1-fh*(i-1)), 
             new=TRUE,
             mar=mar2)
        plot(
          axisC,C[,(icol-1)*n+i],type = "l",col = 'red',
          xlab = xlab2, ylab = '', lwd=1,
          xaxt=xaxt, yaxt='n', ylim=ylim
        )
        abline(h = 0,lty=2)
        colorizeMask1D(axis="delay",ylim=ylim)
        grid()
      }
    }
  }  
  
  output$svdVec <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    CS = reshapeCS(s$u,s$v,ncol(s$u))
    plotSVDVecBloc(CS$C,CS$S,Inputs$delay,Inputs$wavl)    
  },height = 500)
  outputOptions(output, "svdVec",
                suspendWhenHidden = FALSE)
  
  plotResid <- function (delay,wavl,mat,C,S,
                         d = rep(1,ncol(C)),
                         main = 'Data',...) {
    # Build model matrix
    matAls = rep(0,nrow=nrow(mat),ncol=ncol(mat))
    for (i in 1:ncol(S))
      matAls = matAls + C[,i] %o% S[,i] * d[i]
    zlim = range(mat,na.rm = TRUE)
    
    xlim=range(delay,na.rm=TRUE)
    ylim=range(wavl,na.rm=TRUE)
    
    dummy = split.screen(c(2,3))
    screen(1)
    par(cex = cex,cex.main=cex, mar = mar, 
        mgp = mgp, tcl = tcl, pty=pty)
    image(
      delay,wavl,mat,xlim=xlim,ylim=ylim,
      xlab = 'Delay',ylab = 'Wavelength',
      main = main, col = cols, zlim = zlim
    )
    colorizeMask1D(axis="delay",ylim=ylim)
    colorizeMask1D(axis="wavl", dir='h', ylim=xlim)
    
    screen(2)
    par(cex = cex,cex.main=cex, mar = mar, 
        mgp = mgp, tcl = tcl, pty=pty)
    image(
      delay,wavl,matAls,xlim=xlim,ylim=ylim,
      xlab = 'Delay',ylab = 'Wavelength',
      main = paste0('Model ',ncol(S),' species'),
      col = cols,zlim = zlim
    )
    colorizeMask1D(axis="delay",ylim=ylim)
    colorizeMask1D(axis="wavl", dir='h', ylim=xlim)
    
    screen(4)
    par(cex = cex,cex.main=cex, mar = mar, 
        mgp = mgp, tcl = tcl, pty=pty)
    resid = matAls - mat
    lof = 100*(sum(resid^2,na.rm = TRUE)/
                 sum(mat^2,na.rm = TRUE)
    )^0.5
    image(
      delay,wavl,resid,xlim=xlim,ylim=ylim,
      xlab = 'Delay',ylab = 'Wavelength',
      main = paste0('Residuals; L.o.f.=',signif(lof,3),'%'),
      col = cols
    )
    colorizeMask1D(axis="delay",ylim=ylim)
    colorizeMask1D(axis="wavl", dir='h', ylim=xlim)
    
    screen(5)
    par(cex = cex,cex.main=cex, mar = mar, 
        mgp = mgp, tcl = tcl, pty=pty)
    res = resid[!is.na(resid)]
    hist(
      res,col = cyan_tr, border=NA,
      xlim = range(c(res,zlim)),
      xlab = '',main = 'Residuals vs. signal'
    )
    hist(mat,col = pink_tr,border=NA,add = TRUE)
    legend(
      'topright',c('Signal','Residuals'),
      pch = 15,col = c(pink_tr,cyan_tr),cex = 0.75
    )
    dummy = close.screen(all.screens = TRUE)
  }
  
  output$svdResid <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    CS = reshapeCS(s$u,s$v,input$nSV)
    plotResid(Inputs$delay,Inputs$wavl,Inputs$mat,
              CS$C,CS$S,d = s$d)
  },height = 450)
  
  plotConbtribs <- function (delay,wavl,mat,C,S,
                             d = rep(1,ncol(C)),
                             type = 'als',...) {
    
    xlim=range(delay,na.rm=TRUE)
    ylim=range(wavl,na.rm=TRUE)
    
    # Estimate weight of species
    cont = c()
    for (ic in 1:min(6,ncol(S))) {
      matSvd = C[,ic] %o% S[,ic] * d[ic]
      
      if (type == 'als')
        cont[ic] = sum(abs(matSvd),na.rm = TRUE)
      else
        cont[ic] = d[ic]
    }    
    cont = cont / sum(cont) * 100
    
    dummy = split.screen(c(2,3))
    for (ic in 1:min(6,ncol(S))) {
      screen(ic)
      par(cex = cex,cex.main=cex, mar = mar, 
          mgp = mgp, tcl = tcl, pty=pty)
      matSvd = C[,ic] %o% S[,ic] * d[ic]
      image(
        delay,wavl,matSvd,col = cols, 
        xlim=xlim,ylim=ylim,
        xlab = 'Delay',ylab = 'Wavelength',
        main = paste0('Sp. ',ic,' / Wgt. (%) = ',
                      signif(cont[ic],3))
      )
      colorizeMask1D(axis="delay",ylim=ylim)
      colorizeMask1D(axis="wavl", dir='h', ylim=xlim)
    }
    dummy = close.screen(all.screens = TRUE)
  }
  
  output$svdContribs <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    CS = reshapeCS(s$u,s$v,input$nSV)
    plotConbtribs(Inputs$delay,Inputs$wavl,Inputs$mat,
                  CS$C,CS$S,d = s$d, type ='svd')
  },height = 450)
  
# ALS ####
  getS0 <- eventReactive(
    input$S0File, {
      isolate({

        # Get all shapes
        S0_in = list(); i=0
        for (fN in input$S0File$datapath) {
          tmp   = read.table(
            file = fN,
            header = FALSE,
            dec = input$dec,
            sep = input$sep,
            colClasses= 'numeric',
            stringsAsFactors = FALSE
          )
          i=i+1
          S0_in[[i]]=tmp
        }
        return(S0_in)
      })
    }
  )
  
  showMSE = function(a,b,c) {
    if(is.null(a))
      return(FALSE)
    if(a != 'tileDel')
      return(FALSE)
    if(length(b) <= 1)
      return(FALSE)
    if(c <= 1)
      return(FALSE)
    return(TRUE)
  }
  output$showMSE = reactive({
    showMSE(input$procMult,
            input$rawData_rows_selected,
            input$nALS)
  })
  output$maskSpExp_ui = renderUI({
    if(!showMSE(input$procMult,
                input$rawData_rows_selected,
                input$nALS)
      )
      return(NULL)
    
    nM = length(input$rawData_rows_selected) # Nb data matrices
    nS = input$nALS # Nb spectra
    
    if(anyNA(Inputs$maskSpExp))
      Inputs$maskSpExp = matrix(1,nrow=nM,ncol=nS)
    else 
      if (nrow(Inputs$maskSpExp) != nM ||
          ncol(Inputs$maskSpExp) != nS   )
        Inputs$maskSpExp = matrix(1,nrow=nM,ncol=nS)     
    
    matInput=list(
      h5("Presence Matrix"),
      HTML('<table cellpadding=2 border=0>')
      )
    head = paste0(paste0('<th>Sp_',1:nS,'</th>'),collapse = '')
    matInput=list(
      matInput,
      HTML(paste0('<tr><td>&nbsp;</td>',head,'</tr>'))
    )
    for (i1 in 1:nM) {
      var1 = paste0('Exp_',i1)
      matInput=c(matInput,list(HTML(paste0('<tr><th>',
                                           var1,'&nbsp;</th>'))))
      for (i2 in 1:nS) {
        var2  = paste0('C_',i2)
        name  = paste0('mCE_',var1,'_',var2)
        value = Inputs$maskSpExp[i1,i2]
        locInput = list(HTML('<td>'),
                        tags$input(id = name,
                                   type = 'number',
                                   value = value,
                                   min=0, max=1,
                                   class='shiny-bound-input',
                                   style='width: 50px;'),
                        HTML('</td>'))
        matInput=c(matInput,locInput)
      }
      matInput=c(matInput,list(HTML('</tr>')))
    }
    matInput=list(matInput,HTML('</table>'))

    wellPanel(
      verticalLayout(
        matInput,
        br(),
        fixedRow(
          column(12,offset=0,
                 actionButton("clear_mCE" ,
                              "Reset",
                              icon=icon("eraser")),
                 actionButton("update_mCE",
                              "Done",
                              icon=icon("check"))
          )
        )
      )
    )
    
  })
  outputOptions(output, "maskSpExp_ui",
               suspendWhenHidden = FALSE)
  # Update maskSpExp
  observe({
    if (is.null(input$update_mCE) || 
        input$update_mCE == 0 ) return()
    
    isolate({
      
      if(is.null(Inputs$maskSpExp)) 
        return()
      
      nM = length(input$rawData_rows_selected)
      nS = input$nALS 
      for (i1 in 1:nM) {
        var1 = paste0('Exp_',i1)
        for (i2 in 1:nS) {
          var2  = paste0('C_',i2)
          name  = paste0('mCE_',var1,'_',var2)
          Inputs$maskSpExp[i1,i2] = input[[name]]
        }
      }
    })
  })
  
  # Reset maskSpExp
  observe({
    if (is.null(input$clear_mCE) || 
        input$clear_mCE == 0 ) return()
    
    isolate({
      
      if(is.null(Inputs$maskSpExp)) 
        return()
      
      nM = length(input$rawData_rows_selected) 
      nS = input$nALS
      Inputs$maskSpExp = matrix(1,nrow=nM,ncol=nS)
      for (i1 in 1:nM) {
        var1 = paste0('Exp_',i1)
        for (i2 in 1:nS) {
          var2  = paste0('C_',i2)
          name  = paste0('mCE_',var1,'_',var2)
          updateNumericInput(session, inputId=name, value=1)
        }
      }
    })
  })
  

  doALS <- eventReactive(
    input$runALS, {
    isolate({
      if (!checkInputsSanity())
        return(NULL)
      
      nAls = input$nALS
      updateCheckboxGroupInput(
        session,
        inputId = "vecsToRotate",
        selected = c(1,2)
      )
      
      # Suppress masked areas
      delay   = Inputs$delay[!is.na(Inputs$delayMask)]
      delayId = Inputs$delayId[!is.na(Inputs$delayMask)]
      wavl    = Inputs$wavl[!is.na(Inputs$wavlMask)]
      
      if(input$useFiltered) { # Choose SVD filtered matrix  
        s <- doSVD()
        mat = matrix(0,nrow=length(delay),ncol=length(wavl))
        for (ic in 1:input$nSV) 
          mat = mat + s$u[,ic] %o% s$v[,ic] * s$d[ic]
        
      } else {
        mat = Inputs$mat 
        mat = mat[!is.na(Inputs$delayMask),]
        mat = mat[,!is.na(Inputs$wavlMask) ]
      }

      # External spectrum shapes      
      S0 = NULL
      if(input$shapeS) {
        if(is.null(S0_in))
          S0_in <- getS0()
        ii = 0; S0 = c()
        for (i in 1:length(S0_in)) {
          tmp = S0_in[[i]]
          for (k in 2:ncol(tmp))
            S0 = cbind(S0,spline(tmp[,1],tmp[,k],xout=wavl)$y)
        }
      }
      
      # Null C constraints
      nullC = NA
      if(!anyNA(Inputs$maskSpExp)) {
        nullC = matrix(1, nrow=length(delayId),ncol=nAls)
        for(i in 1:nAls) {
          for (j in 1:nrow(Inputs$maskSpExp)) {
            if(Inputs$maskSpExp[j,i] == 0) {
              sel = which(delayId == j)
              nullC[sel,i] = 0
            } 
          }
        }
      } 

      progress <- shiny::Progress$new()
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      msg = list()

      if (input$initALS != 'seq') {
        if (input$initALS == 'SVD') {
          # initialize with abs(SVD)
          if (is.null(s <- doSVD()))
            return(NULL)
          
          S = matrix(abs(s$v[,1:nAls]),ncol=nAls)
          C = matrix(abs(s$u[,1:nAls]),ncol=nAls)
          
        } else if (input$initALS == 'NMF') {
          # initialize with SVD + NMF
          if (is.null(s <- doSVD()))
            return(NULL)
          
          progress$set(message = "Running NMF ", value = 0)
          
          # 1/ filter matrix to avoid negative values (noise)
          fMat = rep(0,nrow=nrow(data),ncol=ncol(data))
          for (i in 1:nAls)
            fMat = fMat + s$u[,i] %o% s$v[,i] * s$d[i]
          # 2/ NMF
          res  = NMF::nmf(abs(fMat), rank=nAls, method='lee')
          C = NMF::basis(res)
          S = t(NMF::coef(res))
          
        } else {
          # restart from existing solution
          if (!exists('RES'))
            return(NULL)
          S = RES$S
          C = RES$C
        }
        # Progress bar
        progress$set(message = "Running ALS ", value = 0)
        #Run
        res = myals(
          C = C, Psi = mat, S = S, xC = delay, xS = wavl,
          maxiter = input$maxiter,
          uniS    = input$uniS,
          nonnegS = input$nonnegS,
          nonnegC = input$nonnegC,
          thresh  = 10^input$alsThresh,
          normS   = input$normS,
          S0      = S0,
          hardS0  = !input$softS0,
          wHardS0 = 10^input$wSoftS0,
          optS1st = input$optS1st,
          SumS    = input$SumS,
          smooth  = input$smooth,
          updateProgress = updateProgress,
          nullC   = nullC,
          closeC  = input$closeC,
          wCloseC = 10^input$wCloseC
        )
        RES <<- res
        msg = list(msg,
                   h4('Single step: ',nAls,' species'),
                   h5('Spectra constrained: ',ifelse(!is.null(S0),ncol(S0),0)),
                   h5('Results after ',res$iter,' iterations'),
                   h5(res$msg),br()
        )
      } else {
        # Sequential update
        # 1 - Start from first SVD
        if (is.null(s <- doSVD()))
          return(NULL)
        S = matrix(abs(s$v[,1]),ncol=1)
        C = matrix(abs(s$u[,1]),ncol=1)

        for (n in 2:nAls) {
          S = cbind(S,1)
          C = cbind(C,1)
          progress$set(message = paste0("Running ALS ",n), value = 0)
          res = myals(
            C = C, Psi = mat, S = S, xC = delay, xS = wavl,
            maxiter = input$maxiter,
            uniS    = input$uniS,
            nonnegS = input$nonnegS,
            nonnegC = input$nonnegC,
            thresh  = 10^input$alsThresh,
            normS   = input$normS,
            S0      = S0,
            hardS0  = !input$softS0,
            wHardS0 = 10^input$wSoftS0,
            optS1st = input$optS1st,
            smooth  = input$smooth,
            SumS    = input$SumS,
            updateProgress = updateProgress,
            nullC   = nullC,
            closeC  = input$closeC,
            wCloseC = 10^input$wCloseC
          )
          S = res$S
          C = res$C
          RES <<- res
          msg = list(msg,
              h4('Step ',n-1,': ',n,' species'),
              h5('Results after ',res$iter,' iterations'),
              h5(res$msg),br()
            )
        }
      }
      
      # Sort contributions by decreasing amplitude
      # cont = c()
      # for (ic in 1:nAls)
      #   cont[ic] = sum(abs(res$C[,ic] %o% res$S[,ic]), na.rm = TRUE)
      # perm  = order(cont, decreasing = TRUE)
      # res$C = matrix(res$C[,perm],ncol=nAls)
      # res$S = matrix(res$S[,perm],ncol=nAls)
      
      colnames(res$S) = paste0('S_',1:nAls)
      colnames(res$C) = paste0('C_',1:nAls)
      
      # Update Reporting
      updateCheckboxGroupInput(session,
                               inputId = 'inReport',
                               selected = c('SVD','ALS'))
      
      output[[paste0('iter', 1)]] <- renderUI({
        msg
      })
      
      res$nullC = nullC
      
      return(res)
      
    })
  })
  
  output$alsOpt <- renderUI({
    if (input$runALS == 0) {
      h5('Select ALS options and press "Run"\n')
    } else {
      alsOut <- doALS()
      h5('ALS done')
      isolate({
        if (alsOut$iter >= input$maxiter)
          strong('Warning : maxiter limit reached !!!')
      })
    }
  })
  
  output$alsResid1 <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)

    CS = reshapeCS(alsOut$C,alsOut$S,ncol(alsOut$C))    

    if(isolate(input$useFiltered)) { # Choose SVD filtered matrix  
      s <- doSVD()
      CS1 = reshapeCS(s$u,s$v,input$nSV)
      mat = matrix(0,nrow=length(Inputs$delay),
                   ncol=length(Inputs$wavl))
      for (ic in 1:input$nSV) 
        mat = mat + CS1$C[,ic] %o% CS1$S[,ic] * s$d[ic]

      main = "SVD-filtered data"
    
    } else {
      mat = Inputs$mat
      main = 'Raw data'
    }
    plotResid(Inputs$delay,Inputs$wavl,mat,
              CS$C,CS$S,main=main)
    
  },height = 450)
  
  plotResidAna = function(delay,wavl,mat,C,S,
                          d = rep(1,ncol(C)),
                          main = 'Data',...) {
    
    # Build model matrix
    matAls = matrix(0,nrow=nrow(mat),ncol=ncol(mat))
    for (i in 1:ncol(S))
      matAls = matAls + C[,i] %o% S[,i] * d[i]
    resid = matAls - mat
    resid[!is.finite(resid)] = 0
    rm(matAls)
    
    wres = resid/sd(resid)
    sv   = svd(wres,nu=2,nv=2)  
    
    par(mfrow=c(2,2), cex = cex,cex.main=cex, mar = mar, 
        mgp = mgp, tcl = tcl, pty='s')
    
    image(delay,wavl,wres,col=colWR,main="Weighted Residuals",
          xlab='Delay',ylab='Wavelength')
    colorizeMask1D(axis="delay",
                   ylim=range(wavl,na.rm=TRUE))
    colorizeMask1D(axis="wavl", dir='h', 
                   ylim=range(delay,na.rm=TRUE))
    
    image.plot(delay,wavl,wres,zlim=c(-3,3),col=colWR,add=TRUE,
               legend.mar=5, legend.shrink=0.8,
               xlab='Delay',ylab='Wavelength')
    
    matplot(sv$v,wavl,type="l",lwd=2,
            xlab="Sing. Vec.",ylab='Wavelength',
            main="SVD of Residuals")
    colorizeMask1D(axis="wavl", dir='h', 
                   ylim=range(sv$v,na.rm=TRUE))
    abline(v=0)
    box()
    
    matplot(delay,sv$u,type="l",lwd=2,
            xlab='Delay',ylab="Sing. Vec.",
            main="SVD of Residuals")
    colorizeMask1D(axis="delay",
                   ylim=range(sv$u,na.rm=TRUE))
    abline(h=0)
    box()
    
    qqnorm(wres);abline(a=0,b=1,col="blue");grid(col="darkgray")
    box()
    
  }
  output$alsResid2 <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    CS = reshapeCS(alsOut$C,alsOut$S,ncol(alsOut$C))    
    
    if(isolate(input$useFiltered)) { # Choose SVD filtered matrix  
      s <- doSVD()
      CS1 = reshapeCS(s$u,s$v,input$nSV)
      mat = matrix(0,nrow=length(Inputs$delay),
                   ncol=length(Inputs$wavl))
      for (ic in 1:input$nSV) 
        mat = mat + CS1$C[,ic] %o% CS1$S[,ic] * s$d[ic]
      
      main = "SVD-filtered data"
      
    } else {
      mat = Inputs$mat
      main = 'Raw data'
    }
    plotResidAna(Inputs$delay,Inputs$wavl,mat,
              CS$C,CS$S,main=main)
    
  },height = 450)

  plotAlsVec <- function (alsOut,type="Kin",xlim=NULL,ylim=NULL,...) {
    par(cex = cex,cex.main=cex, mar = mar, 
        mgp = mgp, tcl = tcl, pty=pty)
    
    if(type == "Kin") {
      if(is.null(ylim))
        ylim = range(alsOut$C)
      matplot(
        alsOut$xC,alsOut$C,
        type = ifelse(length(alsOut$xC)>20,'p','b'),
        pch = 19, cex = 0.5, lwd=2, lty=3,
        xlab = 'Delay', ylab = 'C',
        xlim = xlim,
        ylim = ylim,
        main = paste0('ALS Kinetics'),
        xaxs = 'i', yaxs='i'
      )
      n=ncol(alsOut$C)
      legend('topright',legend=1:n,lty=3,lwd=3,col=1:n)
      colorizeMask1D(axis="delay",ylim=ylim)
      grid();box()
      
    } else {
      if(is.null(ylim))
        ylim = range(alsOut$S)
      matplot(
        alsOut$xS,alsOut$S,
        type = ifelse(length(alsOut$xC)>20,'p','b'),
        pch = 19, cex = 0.5, lwd=2, lty=3,
        xlab = 'Wavelength',ylab = 'S',
        xlim = xlim,
        ylim = ylim,
        main = paste0('ALS Spectra; L.o.f. =',alsOut$lof,'%'), 
        xaxs = 'i', yaxs='i'
      )
      colorizeMask1D(axis="wavl",ylim=ylim)
      grid();box()
      
    }
  }
  rangesAlsKin <- reactiveValues(x = NULL, y = NULL)
  
  output$alsKinVectors <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    plotAlsVec(alsOut,type = "Kin",
               xlim = rangesAlsKin$x,
               ylim = rangesAlsKin$y)
  },height = 400)
  
  observeEvent(input$alsKin_dblclick, {
    brush <- input$alsKin_brush
    if (!is.null(brush)) {
      rangesAlsKin$x <- c(brush$xmin, brush$xmax)
      rangesAlsKin$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesAlsKin$x <- NULL
      rangesAlsKin$y <- NULL
    }
  })

  rangesAlsSp <- reactiveValues(x = NULL, y = NULL)
  
  output$alsSpVectors <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    plotAlsVec(alsOut,type = "Sp",
               xlim = rangesAlsSp$x,
               ylim = rangesAlsSp$y)
  },height = 400)

  observeEvent(input$alsSp_dblclick, {
    brush <- input$alsSp_brush
    if (!is.null(brush)) {
      rangesAlsSp$x <- c(brush$xmin, brush$xmax)
      rangesAlsSp$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesAlsSp$x <- NULL
      rangesAlsSp$y <- NULL
    }
  })
  
  observeEvent(
    input$alsSpKinSave,
    isolate({
      if (is.null(alsOut <- doALS()))
        return(NULL)
      
      CS = reshapeCS(alsOut$C,alsOut$S,ncol(alsOut$C))
      
      S = cbind(Inputs$wavl,CS$S)
      colnames(S) = c('wavl',colnames(alsOut$S))
      write.csv(
        S,
        file = file.path("outputDir",
                         paste0(input$projectTag,
                                '_alsSpectra_',
                                input$nALS,'sp',
                                '.csv')),
        row.names = FALSE
      )
      # C = cbind(alsOut$xC,alsOut$C)
      C = cbind(Inputs$delaySave,CS$C)
      colnames(C) = c('delay',colnames(alsOut$C))
      write.csv(
        C,
        file = file.path("outputDir",
                         paste0(input$projectTag,
                                '_alsKinets_',
                                input$nALS,'sp',
                                '.csv')),
        row.names = FALSE
      )
    })
  )

  output$alsContribs <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    CS = reshapeCS(alsOut$C,alsOut$S,ncol(alsOut$C))    
    plotConbtribs(Inputs$delay,Inputs$wavl,Inputs$mat,
                  CS$C,CS$S)
  },height = 450)
  
  output$selAmbParams <- renderUI({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    lv = list()
    for (i in 1:input$nALS)
      lv[[i]] = i
    
    list(
      fluidRow(
        column(4,
               checkboxGroupInput("vecsToRotate", 
                                  label = "Pick 2 (or 3) vectors",
                                  choices = lv,
                                  selected = c(1,2))
        ),
        column(4,
               sliderInput("alsRotAmbEps",
                           label = "Relative positivity threshold",
                           min   = signif(-0.1),
                           max   = signif( 0.1),
                           value = -0.01,
                           step  =  0.01,
                           sep   = "")
        ),
        column(4,
               sliderInput("alsRotAmbDens",
                           label = "Exploration Step",
                           min=signif(0.01),
                           max=signif(0.1),
                           value = 0.05,
                           step  = 0.01,
                           sep="")
        )
      )
    )
  })
  
  rotAmb2 = function(C0,S0,data,rotVec=1:2,
                     dens=0.05,eps=-0.01,
                     updateProgress=NULL,
                     nullC=NA) {
    
    S = S0[,rotVec]
    C = C0[,rotVec]
    
    ttry = function(i) dens*i
    
    ikeep = 0
    solutions = list() 
    ntry = 0; iter=0
    
    for(s12 in c(0,-1,1)) {
      i12 = 0
      OK1 = TRUE
      while(OK1) {
        i12 = i12 + s12
        t12 = ttry(i12)
        OK1 = FALSE
        
        for(s21 in c(0,-1,1)) {
          i21 = 0
          OK2  = TRUE
          while(OK2) {
            i21 = i21 + s21
            t21 = ttry(i21)
            OK2 = FALSE
            
            iter = iter+1
            # if(!is.null(updateProgress))
            #   updateProgress(value = iter / 100)
            
            # Transformation matrix
            R = matrix(c(1  , t12,
                         t21,   1),
                       nrow = 2,ncol = 2,
                       byrow = TRUE)
            Ri = try(solve(R),silent=TRUE)
            
            if(class(Ri) !='try-error') {
              ntry = ntry +1
              
              # Transform spectra and kinetics
              S1 = t(R %*% t(S))
              C1 = C %*% Ri
              
              # Renormalize spectra
              for(i in 1:2) {
                n = max(S1[,i],na.rm=TRUE)
                S1[,i] = S1[,i] / n
                C1[,i] = C1[,i] * n
              }
              
              
              if(!anyNA(nullC))
                C1 = C1 * nullC[,rotVec]
              
              # Test for positivity
              if(min(S1,na.rm=TRUE) >= eps*max(S1,na.rm=TRUE)  &
                 min(C1,na.rm=TRUE) >= eps*max(C1,na.rm=TRUE)
              ) {
                ikeep = ikeep+1
                solutions[[ikeep]] = list(S1=S1, C1=C1, 
                                          t12=t12, t21=t21)
                OK1 = OK2 = TRUE
                
                if(s21 == 0) OK2 = FALSE
              }
            }
            
            httpuv::service()    
            if(input$killALSAmb) {# Get out of here
              if(length(solutions)!=0) {
                solutions$rotVec = rotVec
                solutions$eps    = eps
              }
              return(
                list(solutions = solutions,
                     finished  = FALSE)
              )
            }
          
          }
          if(s12 == 0) OK1 = FALSE
        }
      }
    }
    
    if(length(solutions)!=0) {
      solutions$rotVec = rotVec
      solutions$eps    = eps
    }
    
    return(
      list(solutions = solutions,
           finished  = TRUE)
    )
  }
  rotAmb3 = function(C0,S0,data,rotVec=1:3,
                     dens=0.05,eps=-0.01,
                     updateProgress=NULL,
                     nullC=NA) {
    
    S = S0[,rotVec]
    C = C0[,rotVec]
    
    ttry = function(i) dens*i
    
    ikeep = 0
    solutions = list() 
    ntry = 0; iter=0
    
    for(s12 in c(0,-1,1)) {
      i12 = 0
      OK1 = TRUE
      while(OK1) {
        i12 = i12 + s12
        t12 = ttry(i12)
        OK1 = FALSE
        
        for(s21 in c(0,-1,1)) {
          i21 = 0
          OK2  = TRUE
          while(OK2) {
            i21 = i21 + s21
            t21 = ttry(i21)
            OK2 = FALSE
            
            for(s23 in c(0,-1,1)) {
              i23 = 0
              OK3 = TRUE
              while(OK3) {
                i23 = i23 + s23
                t23 = ttry(i23)
                OK3 = FALSE
                
                for(s32 in c(0,-1,1)) {
                  i32 = 0
                  OK4 = TRUE
                  while(OK4) {
                    i32 = i32 + s32
                    t32 = ttry(i32)
                    OK4 = FALSE
                    
                    for(s13 in c(0,-1,1)) {
                      i13 = 0
                      OK5 = TRUE
                      while(OK5) {
                        i13 = i13 + s13
                        t13 = ttry(i13)
                        OK5 = FALSE
                        
                        for(s31 in c(0,-1,1)) {
                          i31 = 0
                          OK6 = TRUE
                          while(OK6) {
                            i31 = i31 + s31
                            t31 = ttry(i31)
                            OK6 = FALSE
                            
                            iter = iter+1
                            # if(!is.null(updateProgress))
                            #   updateProgress(value = iter / 100)
                            
                            # Transformation matrix
                            R = matrix(c(1  , t12,  t13,
                                         t21,   1,  t23,
                                         t31, t32,    1),
                                       nrow = 3,ncol = 3,
                                       byrow = TRUE)
                            Ri = try(solve(R),silent=TRUE)
                            
                            if(class(Ri) !='try-error') {
                              ntry = ntry +1
                              
                              # Transform spectra and kinetics
                              S1 = t(R %*% t(S))
                              C1 = C %*% Ri
                              
                              # Renormalize spectra
                              for(i in 1:3) {
                                n = max(S1[,i],na.rm=TRUE)
                                S1[,i] = S1[,i] / n
                                C1[,i] = C1[,i] * n
                              }
                              
                              
                              if(!anyNA(nullC))
                                C1 = C1 * nullC[,rotVec]
                              
                              # Test for positivity
                              if(min(S1,na.rm=TRUE) >= eps*max(S1,na.rm=TRUE)  &
                                 min(C1,na.rm=TRUE) >= eps*max(C1,na.rm=TRUE)
                              ) {
                                ikeep = ikeep+1
                                solutions[[ikeep]] = list(S1=S1, C1=C1, 
                                                          t12=t12, t21=t21,
                                                          t23=t23, t32=t32,
                                                          t13=t13, t31=t31)
                                
                                OK1 = OK2 = OK3 = OK4 = OK5 = OK6 = TRUE
                                
                                if(s31 == 0) OK6 = FALSE
                              }

                            }
                            httpuv::service()                           
                            if(input$killALSAmb) {# Get out of here
                              # print(length(solutions))
                              if(length(solutions)!=0) {
                                solutions$rotVec = rotVec
                                solutions$eps    = eps
                              }
                              return(
                                list(solutions = solutions,
                                     finished  = FALSE)
                              )
                            }
                          }
                          if(s13 == 0) OK5 = FALSE
                        }
                      }
                      if(s32 == 0) OK4 = FALSE
                    }
                  }
                  if(s23 == 0) OK3 = FALSE
                }
              }
              if(s21 == 0) OK2 = FALSE
            }
          }
          if(s12 == 0) OK1 = FALSE
        }
      }
    }
    
    if(length(solutions)!=0) {
      solutions$rotVec = rotVec
      solutions$eps    = eps
    }
    
    return(
      list(solutions = solutions,
           finished  = TRUE)
    )
  }
  
  doAmbRot <- eventReactive(
    input$runALSAmb,
    {
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    updateButton(session,"killALSAmb",value=FALSE)
    
    isolate({
      rotVec = as.numeric(unlist(input$vecsToRotate))
      eps    = input$alsRotAmbEps
      dens   = input$alsRotAmbDens
    })
    
    if(length(rotVec) > 3)
      showModal(modalDialog(
        title = ">>>> Too Many Vectors Selected <<<< ",
        paste0("Please choose 3 vectors max."),
        easyClose = TRUE, 
        footer = modalButton("Close"),
        size = 's'
      ))
    
    C0    = alsOut$C
    S0    = alsOut$S
    nullC = alsOut$nullC
    
    solutions = NULL
    
    if(length(rotVec)==2) {
      sol = rotAmb2(C0, S0, data=Inputs$mat,nullC=nullC,
                    rotVec=rotVec,dens=dens,eps=eps,
                    updateProgress=NULL)
    } 
    else if(length(rotVec)==3) {
      sol = rotAmb3(C0,S0,data=Inputs$mat,nullC=nullC,
                    rotVec=rotVec,dens=dens,eps=eps,
                    updateProgress=updateProgress)
    } 
    solutions = sol$solutions
    
    if(length(solutions)==0)
      if(sol$finished)
        showModal(modalDialog(
          title = ">>>> No Solution Found <<<< ",
          paste0("Try to decrease the exploration step ",
                 "and/or the relative positivity threshold!"),
          easyClose = TRUE, 
          footer = modalButton("Close"),
          size = 's'
        ))
    else
      showModal(modalDialog(
        title = ">>>> No Solution Found <<<< ",
        paste0("Try to let the algorithm run for a longer time!"),
        easyClose = TRUE, 
        footer = modalButton("Close"),
        size = 's'
      ))
    
    return(solutions)
  })
  
  plotAmbVec <- function (alsOut, solutions, 
                          type="Kin",xlim=NULL,ylim=NULL,...) {
    
    par(cex = cex,cex.main=cex, mar = mar, 
        mgp = mgp, tcl = tcl, pty=pty)
    
    C = alsOut$C;   xC = alsOut$xC; nC = ncol(C)
    S = alsOut$S;   xS = alsOut$xS; nS = ncol(S)
    for (i in 1:ncol(C)) {
      nn = sum(S[,i])
      S[,i] = S[,i] / nn
      C[,i] = C[,i] * nn
    }
    
    nkeep = length(solutions)-2
    allVec = 1:nC
    rotVec = solutions$rotVec
    sel    = allVec %in% rotVec
    nvec   = length(rotVec)
    eps    = solutions$eps
    
    col0  = (1:nC)[!sel]
    colR  = col2tr(1:nC,120)[sel]
    
    if(type == 'Sp') {
      # Estimate ranges of S
      S1   = S[,sel] 
      Smax = matrix( eps,nrow=nrow(S1),ncol=ncol(S1))
      Smin = matrix(1e30,nrow=nrow(S1),ncol=ncol(S1))
      for (i in 1:nkeep) {
        for(j in 1:nvec) {
          vec = solutions[[i]]$S1[,j]
          nn = sum(vec)
          for(k in 1:nrow(S1)){
            val = vec[k]/nn
            Smin[k,j] = min(Smin[k,j],val,na.rm=TRUE)
            Smax[k,j] = max(Smax[k,j],val,na.rm=TRUE)
          }
        }
      }
      if(is.null(xlim))
        xlim=range(xS,na.rm=TRUE)
      if(is.null(ylim))
        ylim=range(c(S,Smin,Smax),na.rm=TRUE)
      
      matplot(xS,S,type='n',
              xlim=xlim, ylim=ylim, 
              xaxs='i', yaxs='i',
              main = 'Area Normalized Spectra', 
              xlab = 'Wavelength')
      abline(h = 0,lty = 2)
      if(sum(!sel) != 0) {
        S1 = S[,!sel]  
        matplot(xS,S1,type='p', pch=19, cex=0.5, 
                col= col0, add=TRUE)
      }
      for (j in 1:nvec)
        polygon(c(xS,rev(xS)),c(Smin[,j],rev(Smax[,j])),
                col= colR[j],border = j)
      colorizeMask1D(axis="wavl",ylim=ylim)
      grid(); box()
      
    } else {
      # Estimate ranges of C
      C1 = C[,sel]  
      Cmax = matrix( eps,nrow=nrow(C1),ncol=ncol(C1))
      Cmin = matrix(1e30,nrow=nrow(C1),ncol=ncol(C1))
      for (i in 1:nkeep) {
        for(j in 1:nvec) {
          vec = solutions[[i]]$S1[,j]
          nn = sum(vec) 
          for(k in 1:nrow(C1)){
            val = solutions[[i]]$C1[k,j]*nn # Normalize
            Cmin[k,j] = min(Cmin[k,j],val,na.rm=TRUE)
            Cmax[k,j] = max(Cmax[k,j],val,na.rm=TRUE)
          }
        }
      }
      if(is.null(xlim))
        xlim=range(xC,na.rm=TRUE)
      if(is.null(ylim))
        ylim=range(c(C,Cmin,Cmax),na.rm=TRUE)
      
      matplot(xC,C,type='n',
              xlim=xlim,ylim=ylim, 
              xaxs='i', yaxs='i',
              main = 'Kinetics', xlab = 'Delay')
      abline(h = 0,lty = 2)
      if(sum(!sel) != 0) {
        C1 = C[,!sel]  
        matplot(xC,C1,type='p', pch=19, cex=0.5, 
                col= col0, add=TRUE)
      }
      for (j in 1:nvec)
        polygon(c(xC,rev(xC)),c(Cmin[,j],rev(Cmax[,j])),
                col= colR[j],border = j)
      colorizeMask1D(axis="delay",ylim=ylim)
      grid(); box()
    }

  }
  
  rangesAmbSp <- reactiveValues(x = NULL, y = NULL)
  
  output$ambSpVectors <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
  
    if (!is.list(solutions <- doAmbRot())) 
      cat(paste0("No solutions found \n"))
    else 
      plotAmbVec(alsOut, solutions,
                 type = "Sp",
                 xlim = rangesAmbSp$x,
                 ylim = rangesAmbSp$y)

  },height = 400)
  
  observeEvent(input$ambSp_dblclick, {
    brush <- input$ambSp_brush
    if (!is.null(brush)) {
      rangesAmbSp$x <- c(brush$xmin, brush$xmax)
      rangesAmbSp$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesAmbSp$x <- NULL
      rangesAmbSp$y <- NULL
    }
  })
  
  rangesAmbKin <- reactiveValues(x = NULL, y = NULL)
  
  output$ambKinVectors <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    if (!is.list(solutions <- doAmbRot())) 
      cat(paste0("No solutions found \n"))
    else 
      plotAmbVec(alsOut, solutions,
                 type = "Kin",
                 xlim = rangesAmbKin$x,
                 ylim = rangesAmbKin$y)
  },height = 400)
  
  observeEvent(input$ambKin_dblclick, {
    brush <- input$ambKin_brush
    if (!is.null(brush)) {
      rangesAmbKin$x <- c(brush$xmin, brush$xmax)
      rangesAmbKin$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesAmbKin$x <- NULL
      rangesAmbKin$y <- NULL
    }
  })
  
# Report ####
  observe(updateTextInput(
    session,
    inputId = "reportName",
    value = paste0(input$projectTag,
                   '_Report')
  ))
  
  output$report = downloadHandler(
    filename = function() {
      paste0(input$reportName, '.html') # cf. below for format choice
    },
    content = function(file) {
      src <- normalizePath('reportTemplate.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reportTemplate.Rmd')
      out <- rmarkdown::render('reportTemplate.Rmd',"html_document")

      # No other formats available on styx (install pandoc ???)
      #       out <- rmarkdown::render('reportTemplate.Rmd',
      #                                switch(
      #                                  input$format,
      #                                  html = "html_document",
      #                                  pdf  = "pdf_document",
      #                                  docx = "word_document"
      #                                ))
      file.rename(out, file)
    }
  )
  
  output$getMyFiles <- downloadHandler(
    filename = function ()
      paste0(input$projectTag,'_files.zip'),
    content = function(fname) {
      wd = getwd()
      setwd("outputDir")
      fs= list.files(pattern=input$projectTag)
      # Zip'em
      zip(zipfile=fname, files=fs)
      # cf. https://groups.google.com/d/msg/shiny-discuss/D5F2nqrIhiM/JDEX0b6we1cJ
      if(file.exists(paste0(fname, ".zip"))) 
        file.rename(paste0(fname, ".zip"), fname)
      setwd(wd)
    },
    contentType = "application/zip"
  )
  
}

