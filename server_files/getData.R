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
  if(input$datStr != 'dxw') {
    print('Permute')
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

# Interactive ####
finishMatrix  <- reactive({
  if(!Inputs$process)
    return(NULL)
  
  Inputs$finish <<- FALSE
  
  data = combineMatrix(input$rawData_rows_selected)
  validate(need(!is.null(data),"--> Bad data type"))
  
  fwD = input$postCompFacD
  fwW = input$postCompFacW
  if( fwD >= 2 || fwW >=2 ) {
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
        
        # Install data
        if(is.null(Inputs$matOrig)) {
          # Scale factor for neater delay selectors
          dlScaleFac = 1 #10^(floor(log10(diff(range(data$delay)))-1))
          Inputs$fileOrig       <<- input$dataFile$name
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
    Inputs$finish <<- TRUE
  }
})
