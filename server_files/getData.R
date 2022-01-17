# Functions ####
transformDelay <- function(delay,trans = 0) {
  # trans = 0: do nothing
  # trans = 1: return index
  # trans = 2: return log10 with management of neg or null values
  
  del = delay
  Inputs$delayTrans <<- ''
  
  if(trans == 1) {
    del = 1:length(del)
    Inputs$delayTrans <<- 'index'
    
  } else if(trans == 2) {
    minLoc = which(del>0)[1]
    if(minLoc > 1) {
      # Compress all points below minLoc 
      # into ]0,del(minLoc)[ 
      step   = del[minLoc]/minLoc
      for(i in 1:minLoc-1) 
        del[i] = del[minLoc] - (minLoc-i)*step
    }
    del = log10(del)
    Inputs$delayTrans <<- 'log10'
    
  }
  
  return(del)
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
        # effData    = outliers::rm.outlier(v)
        # matm[i,j]  = mean(effData,na.rm = TRUE)
        matm[i,j]    = mean(v,na.rm = TRUE)
      }
    }
  } 
  matm[!is.finite(matm)] = 0
  
  # Delay transforms for single delay scale
  if(!is.null(input$transformDelay))
    delay = transformDelay(delay,input$transformDelay)
  # delay = 1:length(delay) # Replace by ordinal scale
  
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
  }
  
  if(tileDel) {
    delay = transformDelay(delay,1) # Convert to index
  
  }else{
    # Delay transforms for single delay scale
    if(!is.null(input$transformDelay))
      delay = transformDelay(delay,input$transformDelay)
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
    
    del = RawData[[sel]]$delay
    
    # Delay transforms for single matrix
    if(!is.null(input$transformDelay))
      del = transformDelay(del,input$transformDelay)
    
    list(
      mat       = RawData[[sel]]$mat, 
      delay     = del, 
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

# Interactive ####
finishMatrix  <- reactive({
  if(!Inputs$process)
    return(NULL)
  
  Inputs$finish <<- FALSE
  data = combineMatrix(input$rawData_rows_selected)
  validate(need(!is.null(data),"--> Bad data type"))
  
  fwD = input$postCompFacD
  fwW = input$postCompFacW
  if( fwD >= 2 | fwW >=2 ) {
    dls  = data$delay
    dId  = data$delayId
    data = downsizeMatrix(data$delaySave,data$wavl,data$mat,
                          fwD=fwD, fwW=fwW)
    data$delaySave = data$delay
    data$delay     = 1:length(data$delay)
    data$delayId   = dId[seq(1,length(dls),by=fwD)]
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
      
      if ( length(data$wavl) == 0 ){
        loadError  = TRUE
        loadErrMsg = "wavl"  
      }
      
      if ( length(data$delay) == 0 ){
        loadError  = TRUE
        loadErrMsg = paste0(loadErrMsg,", delay")
      }
      
      if (!is.numeric(data$mat) ||
          !is.matrix(data$mat)    ){
        loadError  = TRUE
        loadErrMsg = paste0(loadErrMsg,", matrix")
      }
      
      if(loadError) {
        showModal(modalDialog(
          title = ">>>> Data problem <<<< ",
          paste0("Improperly formatted ", loadErrMsg,
                 "Check the header, delimiter or decimal marker"),
          easyClose = TRUE, 
          footer = modalButton("Close"),
          size = 's'
        ))
        Inputs$fileOrig  <<- NULL
        
      } else {
        
        # Auto Project Tag
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
        
        # (Re)-Install data
        if(is.null(Inputs$fileOrig) | 
           is.null(Inputs$matOrig) |
           length(Inputs$fileOrig) != 
              length(input$rawData_rows_selected) |
           any(Inputs$fileOrig != 
               input$dataFile$name[input$rawData_rows_selected])
           ) {
          # Scale factor for neater delay selectors
          dlScaleFac = 1 #10^(floor(log10(diff(range(data$delay)))-1))
          Inputs$fileOrig       <<- 
            input$dataFile$name[input$rawData_rows_selected]
          Inputs$dlScaleFacOrig <<- dlScaleFac
          Inputs$matOrig        <<- data$mat
          Inputs$wavlOrig       <<- data$wavl
          Inputs$delayOrig      <<- data$delay
          Inputs$delayIdOrig    <<- data$delayId
          Inputs$delaySaveOrig  <<- data$delaySave
        } 
        
        # Update
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
    # browser()
    Inputs$finish <<- TRUE
  }
})
