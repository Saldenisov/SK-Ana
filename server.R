options(shiny.maxRequestSize=20*1024^2)
# options(shiny.json.digits=32)

# Libraries ####
library(outliers)
library(nnls)
library(Iso)
library(viridis)
library(shiny)
# library(NMF)
 
# Colors ####
cols = viridis(128)
col2tr = function(col,alpha)
  rgb(unlist(t(col2rgb(col))),alpha = alpha,maxColorValue = 255)
cyan_tr = col2tr("cyan",120)
pink_tr = col2tr("pink",120)

# Global graphical params ####
cex = 1
mar = c(4.5,5,2,1)
mgp = c(2,.75,0)
pty = 's'
tcl = -0.5
 
# Functions ####
getC  = function (S, Psi, C, nonnegC) {
  S[which(is.nan(S))] = 1
  for (i in 1:nrow(Psi)) {
    if (nonnegC)
      cc = try(nnls(S ,Psi[i,]))
    else
      cc = try(qr.coef(qr(S) , Psi[i,]))
    if (class(cc) == "try-error")
      sol = rep(1, ncol(S))
    else
      sol = if (nonnegC)
        cc$x
    else
      cc
    
    cc1 = rep(NA, ncol(C))
    cc1[is.na(cc1)] <- sol
    C[i,] <- cc1
  }
  return(C)
}
getS  = function (C, Psi, S, xS, nonnegS, uniS, S0, normS, smooth) {
  C[which(is.nan(C))] = 1
  for (i in 1:ncol(Psi)) {
    if (nonnegS) {
      s <- try(nnls(C, Psi[,i]))
    }
    else
      s <- try(qr.coef(qr(C), Psi[,i]))
    
    if (class(s) == "try-error") {
      S[i,] = rep(1, ncol(C))
    } else {
      S[i,] = if (nonnegS)
        s$x
      else
        s
    }
  }
  
  if (uniS) {
    for (i in 1:ncol(S))
      S[,i] = ufit(y = S[,i], x = xS)$y
  }
   
  if(smooth != 0) {
    for (i in 1:ncol(S)) {
      y=S[,i]
      x = 1:length(y)
      mod = loess(y~x,span=smooth)
      y=predict(mod)
      y[y<0]=0
      S[,i]=y      
    }
  } 

  if (!is.null(S0)) {
    S[,1:ncol(S0)] = S0
  }
  
  if (normS != 0) {
    for (i in 1:ncol(S))
      S[,i] = S[,i] / ifelse(max(S[,i] > 0),max(S[,i]),1)
  }
  
  return(S)
}
myals = function (C, Psi, S, 
                  thresh = 0.001, 
                  maxiter = 100,
                  xC = 1:nrow(C), 
                  xS = 1:nrow(S),
                  nonnegC = TRUE, nonnegS = TRUE, optS1st = TRUE,
                  normS = 1, uniS = FALSE, S0 = NULL, smooth=0,
                  silent = TRUE,
                  updateProgress = NULL) {
  
  RD <- 10 ^ 20
  
  resid = matrix(0, nrow(Psi), ncol(Psi))
  for (i in 1:nrow(Psi))
    resid[i,] = Psi[i,] - C[i,] %*% t(S)
  
  initialrss <- oldrss <- sum((resid) ^ 2) / sum(Psi ^ 2)
  
  if (!silent)
    cat("Initial RSS", initialrss, "\n")
  
  b <- ifelse(optS1st,1,0)
  iter <- 0
  oneMore <- TRUE
  while ((abs(RD) > thresh  && maxiter >= iter) || oneMore) {
    
    iter <- iter + 1
    
    if (iter %% 2 == b)
      S = getS(C, Psi, S, xS, nonnegS, uniS, S0, normS, smooth)
    else
      C = getC(S, Psi, C, nonnegC)
    
    for (i in 1:nrow(Psi))
      resid[i,] = Psi[i,] - C[i,] %*% t(S)
    
    rss <- sum(resid ^ 2) / sum(Psi ^ 2)
    RD <- ((oldrss - rss) / oldrss)
    oldrss <- rss
    
    msg = paste0("Iter. (opt. ", 
                 ifelse (iter %% 2 == b, 'S', 'C'), "): ", iter, 
                 ", |RD| : ", signif(abs(RD),3)," > ",thresh)
    
    if (!silent)
      cat(msg,'\n')
    if (is.function(updateProgress))
      updateProgress(value = iter / maxiter,
                     detail = msg)
    
    oneMore <- ( (iter %% 2 != b) && maxiter != 1 && iter <=2)
  }
  
  msg = HTML(paste0(
    "Initial RSS / Final RSS = ", signif(initialrss,3),
    "/", signif(rss,3), " = ", signif(initialrss / rss,3),
    "<br/> |RD| : ", signif(abs(RD),3)," <= ",thresh,
    "<br/> L.O.F. = ",signif(100*(sum(resid^2)/sum(Psi^2))^0.5,4)  
    ))
  
  if (!silent)
    cat(msg)
  
  return(list(
    C = C, S = S, xC = xC, xS = xS, Psi = Psi,
    rss = rss, resid = resid, iter = iter,
    msg = msg
  ))
}
unMask  <- function (X, axis = axis, mask = c(1,1), dim = 1) {
  if (diff(mask) == 0)
    return(X)
  
  # Select masked values
  sel = axis  > mask[1] & axis  < mask[2]
  if (sum(sel)==0)
    return(X)
  
  # Expand object by inserting NAs at masked values
  if (is.vector(X)) {
    X0 = rep(NA,length(axis))
    X0[!sel] = X
    
  } else {
    if (is.matrix(X)) {
      if (dim == 1) {
        X0 = matrix(NA,ncol = ncol(X),nrow = length(axis))
        X0[!sel,] = X
      } else {
        X0 = matrix(NA,ncol = length(axis),nrow = nrow(X))
        X0[,!sel] = X
      }
      
    } else {
      stop('Unknown object type for unMask(): use vector or matrix')
    }
  }
  return(X0)
}
delMask <- function (X, axis = axis, mask = c(1,1), dim = 1) {
  if (diff(mask) == 0)
    return(X)
  
  # Select masked values
  sel = axis  > mask[1] & axis  < mask[2]
  if (sum(sel)==0)
    return(X)
  
  # Suppress masked values
  if (is.vector(X)) {
    X0 = X[!sel]
    
  } else {
    if (is.matrix(X)) {
      if (dim == 1) {
        X0 = X[!sel,]
      } else {
        X0 = X[,!sel]
      }
      
    } else {
      stop('Unknown object type for delMask(): use vector or matrix')
    }
  }
  return(X0)
}
plotResid <- function (delay,wavl,mat,C,S,
                       d = rep(1,ncol(C)),
                       main = 'Data',...) {
  # Buid model matrix
  matAls = mat * 0
  for (i in 1:ncol(S))
    matAls = matAls + C[,i] %o% S[,i] * d[i]
  
  zlim = range(mat,na.rm = TRUE)
  
  dummy = split.screen(c(2,3))
  screen(1)
  par(cex = cex,cex.main=cex, mar = mar, 
      mgp = mgp, tcl = tcl, pty=pty)
  image(
    delay,wavl,mat,
    xlab = 'Delay',ylab = 'Wavelength',
    main = main, col = cols, zlim = zlim
  )
  screen(2)
  par(cex = cex,cex.main=cex, mar = mar, 
      mgp = mgp, tcl = tcl, pty=pty)
  image(
    delay,wavl,matAls,
    xlab = 'Delay',ylab = 'Wavelength',
    main = paste0('Model ',ncol(S),' species'),
    col = cols,zlim = zlim
  )
  screen(4)
  par(cex = cex,cex.main=cex, mar = mar, 
      mgp = mgp, tcl = tcl, pty=pty)
  resid = matAls - mat
  image(
    delay,wavl,resid,
    xlab = 'Delay',ylab = 'Wavelength',
    main = 'Residuals',col = cols
  )
  screen(5)
  par(cex = cex,cex.main=cex, mar = mar, 
      mgp = mgp, tcl = tcl, pty=pty)
  res = resid[!is.na(resid)]
  hist(
    res,col = cyan_tr,xlim = range(c(res,zlim)),
    xlab = '',main = 'Residuals vs. signal'
  )
  hist(mat,col = pink_tr,add = TRUE)
  legend(
    'topright',c('Signal','Residuals'),
    pch = 15,col = c(pink_tr,cyan_tr),cex = 0.75
  )
  dummy = close.screen(all.screens = TRUE)
}
plotConbtribs <- function (delay,wavl,mat,C,S,
                           d = rep(1,ncol(C)),
                           type = 'als',...) {
  
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
      xlab = 'Delay',ylab = 'Wavelength',
      main = paste0('Sp. ',ic,' / Wgt. (%) = ',signif(cont[ic],3))
    )
  }
  dummy = close.screen(all.screens = TRUE)
}
plotAlsVec <- function (alsOut,...) {
  par(mfrow = c(1,2))
  par(cex = cex,cex.main=cex, mar = mar, 
      mgp = mgp, tcl = tcl, pty=pty)
  matplot(
    alsOut$xS,alsOut$S,
    type = ifelse(length(alsOut$xC)>20,'p','b'),
    pch = 19, cex = 0.5, lwd=2, lty=3,
    xlab = 'Wavelength',ylab = 'S',
    main = 'ALS Spectra', ylim = c(0,1.1),
    xaxs = 'i',yaxs = 'i'
  )
  n=ncol(alsOut$S)
  legend('topright',legend=1:n,lty=3,lwd=3,col=1:n)
  grid();box()
  par(cex = cex,cex.main=cex, mar = mar, 
      mgp = mgp, tcl = tcl, pty=pty)
  matplot(
    alsOut$xC,alsOut$C,
    type = ifelse(length(alsOut$xC)>20,'p','b'),
    pch = 19, cex = 0.5, lwd=2, lty=3,
    xlab = 'Delay', ylab = 'C',
    main = 'ALS Kinetics',
    xaxs = 'i',yaxs = 'i'
  )
  grid();box()
}
plotSVDVec <- function (X,axis,xlab="x",col='blue',...) {
  is = 0
  dummy = split.screen(c(2,4))
  for (i in 1:8) {
    is = is + 1
    screen(is)
    par(cex = cex,cex.main=cex, mar = mar)
    plot(
      axis,X[,i],type = "l",col = col,
      xlab = xlab,ylab = 'Arb. units',
      main = paste0("vector ",i)
    )
    abline(h = 0)
  }
  dummy = close.screen(all.screens = TRUE)
}
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
        xlab1='Delay'
        xlab2='Wavelength'
        xaxt='s'
        mar1=c(4,4,0,0)
        mar2=c(4,0,0,1)
      } else {
        xlab1=''
        xlab2=''
        xaxt='n'
        mar1=c(0,4,0,0)
        mar2=c(0,0,0,1)
      }
      par( fig = c((icol-1)*0.5,
                   (icol-1)*0.5+0.27,
                   max(0,1-fh*i - ifelse(i==n,0.11,0)),
                   1-fh*(i-1)), 
           new=ifelse(i*icol==1,FALSE,TRUE),
           mar=mar1)
      plot(
        axisC,C[,(icol-1)*n+i],type = "l",col = 'darkgreen',
        xlab = xlab1,ylab = 'Arb. units',
        xaxt=xaxt, ylim=ylim, lwd=2
      )
      abline(h = 0,lty=2); grid()
      legend('topleft',legend=paste0((icol-1)*n+i),bty='n')
      par( fig = c((icol-1)*0.5+0.27,
                   (icol-1)*0.5+0.5,                     
                   max(0,1-fh*i - ifelse(i==n,0.11,0)),
                   1-fh*(i-1)), 
           new=TRUE,
           mar=mar2)
      plot(
        axisS,S[,(icol-1)*n+i],type = "l",col = 'orange',
        xlab = xlab2,ylab = '',lwd=2,
        xaxt=xaxt, yaxt='n', ylim=ylim
      )
      abline(h = 0,lty=2);grid()
    }
    
  }
}
indxCuts <- function (xCut, coords, minx=50) {
  delta=0
  # Select indices around cut
  if(xCut == coords[1]) {
    # First point
    indx = c(1)
  } else {
    if(xCut == coords[length(coords)]) {
      # Last point
      indx=c(length(coords))
    } else {
      if(length(coords) > 2*minx) {
        # Select points around cut
        delta = diff(range(coords)) / minx
        indx = which(coords > xCut - delta / 2 &
                     coords < xCut + delta / 2)
      } else {
        # Select point nearest cut
        indx = c(which.min(abs(coords-xCut)))
      }
    }
  }
  return(list(indx=indx,delta=delta))
}
 
# Server ####
shinyServer(function(input, output, session) {
   
# Initialize ####
  if(!dir.exists("outputDir"))
     dir.create("outputDir",showWarnings = FALSE)
  
  projConfig = NULL
  S0_in      = NULL
  
  Inputs = reactiveValues(
    fileOrig       = NULL,
    matOrig        = NULL,
    wavlOrig       = NULL,
    delayOrig      = NULL,
    dlScaleFacOrig = NULL,
    mat            = NULL,
    wavl           = NULL,
    delay          = NULL,
    delaySave      = NULL  # True delays used in saved kinetics
  )
  
  checkInputsSanity = function() {
    listIn = reactiveValuesToList(Inputs)
    nulVec = unlist(lapply(listIn, is.null))
    noNull = !any(nulVec)
    return(noNull)
  }

  initSliders <- function(config=NULL) {
    
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
      wlMaskSel  = config$keepWlMask
      wlCutSel   = config$keepWlCut
      dlRangeSel = config$keepDlRange
      dlMaskSel  = config$keepDlMask
      dlCutSel   = config$keepDlCut
      cblSel     = config$keepCbl
    } else {
      # Initialize
      doRangeSel = doRange
      wlRangeSel = wlRange
      wlMaskSel  = c(wlMask[1],wlMask[1])
      wlCutSel   = signif(mean(wlCut),3)
      dlRangeSel = dlRange
      dlMaskSel  = c(dlMask[1],dlMask[1])
      dlCutSel   = signif(mean(dlCut),3)
      cblSel     = cblRange[1]
    }

    # DO slider
    updateSlider("keepDoRange", doRange, doRangeSel, 200)

    # Wavelength sliders
    nsteps = min(length(wavl),200)
    updateSlider("keepWlRange", wlRange, wlRangeSel, nsteps)
    updateSlider("keepWlMask" , wlMask , wlMaskSel , nsteps)
    updateSlider("keepWlCut"  , wlCut  , wlCutSel  , nsteps)
    
    # Delay sliders
    nsteps = min(length(delay),500)
    updateSlider("keepDlRange", dlRange, dlRangeSel, nsteps)
    updateSlider("keepDlMask" , dlMask , dlMaskSel , nsteps)
    updateSlider("keepDlCut"  , dlCut  , dlCutSel  , nsteps)

    
    # Baseline correction slider
    nsteps = round(diff(cblRange)/10)
    updateSlider("keepCbl"    , cblRange, cblSel   , nsteps)

    # Update Reporting
    updateCheckboxGroupInput(session,
                             inputId = 'inReport',
                             selected = c('SVD'))
    
  }

  getMatrix <- reactive({

    fileNames = input$dataFile
    
    if(nrow(fileNames)>=2) {
      # If multiple files, average them
      switch( input$procMult,
              avrg    = getMeanMatrix(fileNames),
              tileWav = getTileMatrix(fileNames,tileDel=FALSE),
              tileDel = getTileMatrix(fileNames,tileDel=TRUE)
            ) 
      
    } else {
      getOneMatrix(fileNames$datapath)
      
    }
    
  })

  getOneMatrix <- function(dataFile) {
    # print("getOneMatrix")
    # system(paste0('iconv -f ISO-8859-1 -t UTF-8 ',fileName,' > temp.txt'))
    # cat(dataFile,'\n')
    wavl =
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
            # na.strings=c("NA","NaN", "x/y", "-Inf")
          )
        )[-1]
      )
    # print(wavl)
    # if(is.na(sum(wavl)))
    
    mat   = read.table(
      dataFile, 
      header = input$header, 
      skip = 1,
      dec = input$dec, 
      sep = input$sep,
      colClasses= 'numeric',
      stringsAsFactors = FALSE
    )
    mat = as.matrix(mat)
    # print(str(mat))
#     print(range(mat))
    
    delay = as.numeric(mat[,1])
    u = !duplicated(delay)
    delay = delay[u]
    mat   = mat[u,-1]
    mat[!is.finite(mat)] = 0
    # print(range(mat))
    
    # Ensure increasing coordinates
    iord = order(wavl,decreasing=FALSE)
    wavl=wavl[iord]
    mat = mat[,iord] 
    iord = order(delay,decreasing=FALSE)
    delay=delay[iord]
    mat = mat[iord,] 

    # Downsize
    if(input$compFac >= 2) {
      fw = input$compFac
      # pad matrix with Nas
      newNcol = ceiling(ncol(mat)/fw)*fw
      newNrow = ceiling(nrow(mat)/fw)*fw
      lmat = matrix(NA,nrow=newNrow,ncol=newNcol)
      lmat[1:nrow(mat),1:ncol(mat)]=mat
      # Block average
      nRowBloc = newNrow/fw
      nColBloc = newNcol/fw
      amat = matrix(NA, nrow=nRowBloc,ncol=nColBloc)
      for(i in 1:nRowBloc) 
        for(j in 1:nColBloc)
          amat[i,j] = mean(lmat[((i-1)*fw+1):(i*fw),
                                ((j-1)*fw+1):(j*fw)],
                           na.rm=TRUE)
      delay[newNrow]=NA
      adelay = c()
      for(i in 1:nRowBloc) 
          adelay[i] = mean(delay[((i-1)*fw+1):(i*fw)],
                           na.rm=TRUE)
      wavl[newNcol]=NA
      awavl = c()
      for(i in 1:nColBloc) 
        awavl[i] = mean(wavl[((i-1)*fw+1):(i*fw)],
                         na.rm=TRUE)
      
      mat   = amat
      delay = adelay
      wavl  = awavl
    }
    
    # Transpose if necessary
    if(input$datStr != 'dxw') {
      mat   = t(mat)
      tmp   = delay
      delay = wavl
      wavl  = tmp
    }
    
    return(list(
      mat = mat, delay = delay, wavl = wavl, delaySave = delay
    ))
  }
  
  getMeanMatrix <- function (fileNames) {

    # First pass: build full delay and wavl tables 
    # (some matrices mignt have missing rows and/or columns)
    delay=c()
    wavl=c()
    for (fN in fileNames$datapath) {
      O     = getOneMatrix(fN)    
      delay = c(delay,O$delay)
      wavl  = c(wavl,O$wavl)
    }
    delay=sort(unique(delay))
    wavl =sort(unique(wavl ))

    # Load all matrices, recast them in the full coords
    matTab=array(NA,dim=c(nrow(fileNames),length(delay),length(wavl)))
    i=0
    for (fN in fileNames$datapath) {
      i=i+1
      O=getOneMatrix(fN)
      matm=O$mat
      del=O$delay
      wav=O$wavl
      
      matTab[i,
             which ((delay %in% del) == TRUE),
             which ((wavl %in% wav) == TRUE)] = 
        matm[1:length(del),1:length(wav)]
    }
    
    # Take the average and sd rejecting outliers
    matm = sigma = matrix(NA,ncol=length(wavl),nrow=length(delay))
    for (i in 1:dim(matm)[1]) {
      for (j in 1:dim(matm)[2]) {
        effData    = rm.outlier(matTab[,i,j])
        matm[i,j]  = mean(effData,na.rm = TRUE)
        # sigma[i,j] = sd(effData)/sqrt(length(effData))
      }
    } 
    matm[!is.finite(matm)] = 0
    
    return(list(mat=matm, wavl=wavl, delay=delay, delaySave=delay))
  }
  
  getTileMatrix <- function (fileNames, tileDel=TRUE) {
    
    nbFiles = length(fileNames$datapath)
    for (i in 1:nbFiles) {
      fN = fileNames$datapath[i]
      O   = getOneMatrix(fN)
      mat1 = O$mat
      wav1 = O$wavl
      del1 = O$delay
      delS1 = O$delaySave
      
      if(i==1) {    
        mat   = mat1
        delay = del1
        wavl  = wav1
        delaySave = delS1
        
      } else {
        if(tileDel) {
          # Tile matrices by delay (row)
          delay = c(delay,del1)
          delay = 1:length(delay) # Replace by ordinal scale
          mat   = rbind(mat,mat1)      
          delaySave = c(delaySave,delS1)
          
        } else {
          # Tile matrices by wavl (col)
          wavl = c(wavl,wav1)
          # wavl = 1:length(wavl) # Replace by ordinal scale
          mat  = cbind(mat,mat1)      
          
        }
      }
    }
    # Order wavl by increasing value
    sel = order(wavl)
    wavl=wavl[sel]
    mat = mat[,sel]
    
    return(list(mat=mat, wavl=wavl, delay=delay, delaySave=delaySave))
  }
    
  
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
    isolate({
      # print("observeData")
      
      data = getMatrix()

      # ckeck for load errors
      loadError  = FALSE
      loadErrMsg = ""
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
        output$loadError <- renderUI({
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
        output$loadError <- renderUI({
          h4('Data loaded')
        })
        if(nrow(input$dataFile) == 1) {
          projName = substr(input$dataFile$name,1,14)
        } else {
          prefix = 'Mean_'
          if( input$procMult != 'avrg') prefix = 'Tile_'
          projName = paste0(prefix,
                            substr(input$dataFile$name[1],1,8))
        }
          
        updateTextInput(session,
                        inputId = "projectTag",
                        value   = projName)
        
        # Scale factor for neater delay selectors
        dlScaleFac = 10^(floor(log10(diff(range(data$delay)))-1))
        # cat(dlScaleFac,'\n')
        
        # Install data
        Inputs$fileOrig       <<- input$dataFile$name
        Inputs$matOrig        <<- data$mat
        Inputs$wavlOrig       <<- data$wavl
        Inputs$delayOrig      <<- data$delay
        Inputs$delaySaveOrig  <<- data$delaySave
        Inputs$dlScaleFacOrig <<- dlScaleFac
        Inputs$mat            <<- data$mat
        Inputs$wavl           <<- data$wavl
        Inputs$delay          <<- data$delay
        Inputs$delaySave      <<- data$delaySave
        
        # Initialize config
        initSliders()
        projConfig <<- NULL
      }
      
      
    })
  )
  
  # Open saved project
  observeEvent(
    input$projectFile,
    isolate({
      # print("observeProject")
      load(input$projectFile$datapath)
      
      updateTextInput(session,
                      inputId = "projectTag",
                      value = strsplit(
                        input$projectFile$name,
                        ".",fixed=TRUE)[[1]][1]
                      )

      # Install data
      Inputs$fileOrig       <<- data$fileOrig
      Inputs$matOrig        <<- data$matOrig
      Inputs$wavlOrig       <<- data$wavlOrig
      Inputs$delayOrig      <<- data$delayOrig
      Inputs$delaySaveOrig  <<- data$delaySaveOrig
      Inputs$dlScaleFacOrig <<- data$dlScaleFacOrig
      Inputs$mat            <<- data$matOrig
      Inputs$wavl           <<- data$wavlOrig
      Inputs$delay          <<- data$delayOrig
      Inputs$delaySave      <<- data$delaySave
      
      # Restore project config
      initSliders(config)
      projConfig <<- config
      
    })
  )
 
  output$projectInfo <- renderPrint({

    validate(
      need(
        checkInputsSanity(), 
        "Please select a Project or Data file"
      )
    )
 
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
      paste0(input$projectTag,'.ska')
    },
    content  = function(con) {
      # Collect congigutation parameters
      ll = reactiveValuesToList(input)
      sel = grepl('^keep',names(ll))
      config = ll[sel]
      # Collect data
      ll = reactiveValuesToList(Inputs)
      sel = grepl('Orig',names(ll))
      data = ll[sel]
      
      save(config, data, 
           file     = con, 
           compress = 'gzip')
    }
  )
  
# Select Area ####

  observeEvent(input$reset,
               initSliders()
               )
  
  selectArea <- reactive({
    # print('Select')
    if (!checkInputsSanity())
      return(NULL)
  
    #     if(is.null(projConfig))
    #       initSliders()
    #     else
    #       initSliders(projConfig)
    
    delay = Inputs$delayOrig
    wavl  = Inputs$wavlOrig
    mat   = Inputs$matOrig
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
    
    # Mask area
    xlim = input$keepDlMask * Inputs$dlScaleFacOrig
    if (xlim[2] != xlim[1]) {
      subX = delay > xlim[1] & delay < xlim[2]
      if(sum(subX)!=0)
        mat[subX,]  = NA
    }
    ylim = input$keepWlMask
    if (ylim[2] != ylim[1]) {
      subY = wavl  > ylim[1] & wavl  < ylim[2]
      if(sum(subY)!=0)
        mat[,subY] = NA
    }
    
    # Select work area
    xlim = input$keepDlRange * Inputs$dlScaleFacOrig
    ylim = input$keepWlRange
    
    subX = delay >= xlim[1] & delay <= xlim[2]
    subY = wavl  >= ylim[1] & wavl  <= ylim[2]
    
    delay = delay[subX]
    wavl  = wavl[subY]
    mat   = mat[subX,subY]
    delaySave = delaySave[subX]
    
    Inputs$delay <<- delay
    Inputs$delaySave <<- delaySave
    Inputs$wavl  <<- wavl
    Inputs$mat   <<- mat
  })
  
  output$image1 <- renderPlot({
    if(is.null(selectArea())) 
      return(NULL)
    # print('Image')
    
    mat   = Inputs$mat
    wavl  = Inputs$wavl
    delay = Inputs$delay
    
    if(!is.finite(diff(range(wavl)))  ||
       !is.finite(diff(range(delay))) ||
       !is.finite(diff(range(mat,na.rm=TRUE)))   ) {
      plot(1:10,1:10,type='n')
      text(x=5,y=5,labels='Data not ready...',col=2)
    }
    
    split.screen(c(1, 2))
    split.screen(c(2, 1),screen=2)
    screen(1)
    par(cex = cex, mar = mar)
    image(
      delay,wavl,mat,
      xlab = 'Delay',ylab = 'Wavelength',
      col = cols, zlim = input$keepDoRange
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
      abline(
        v = Inputs$delayOrig[input$keepCbl],lwd = 2,col = 'red',lty = 2
      )
      rect(Inputs$delayOrig[1],
           Inputs$wavlOrig[1],
           Inputs$delayOrig[input$keepCbl],
           Inputs$wavlOrig[length(Inputs$wavlOrig)],
           border=NA,
           col=pink_tr #'gray70'
      )
    }
 
    screen(3)
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
      xlab = 'Wavelength', ylab = 'DO', 
      ylim = range(mat[!is.na(mat)]),
      main = paste0('Mean DO at delay: ',signif(mean(delay[indx]),3),
                    ifelse(delta==0,
                           '',
                           paste0(' +/- ',signif(delta / 2,2))
                           )
                   )
            )
    abline(h = 0,lty = 2)

    screen(4)
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
      xlab = 'Delay', ylab = 'DO', 
      ylim = range(mat[!is.na(mat)]),
      main = paste0('Mean DO at wavl: ',signif(mean(wavl[indx]),3),
                    ifelse(delta==0,
                           '',
                           paste0(' +/- ',signif(delta / 2,2))
                    )
      )
    )
    abline(h = 0,lty = 2)
    close.screen(all.screens = TRUE)
    
  })
  
  output$cuts <- renderPlot({
    if(is.null(selectArea())) 
      return(NULL)
    # print('Cuts')
    mat   = Inputs$mat
    wavl  = Inputs$wavl
    delay = Inputs$delay
    
    wCut = seq(1,length(delay),
               by = ifelse(length(delay) >= 50, 10, 1)
               )
    dCut = seq(1,length(wavl),
               by = ifelse(length(wavl ) >= 50 , 10, 1)
               )

    ylim = input$keepDoRange
    
    par(mfrow = c(1,2))
    par(cex = cex, mar = mar)
    matplot(
      wavl,t(mat[wCut,]),type = 'l', ylim = ylim,
      xlab = 'Wavelength', ylab = 'DO',
      xaxs='i', yaxs='i'
    )
    # mtext(signif(delay[wCut],3),side=3,at=mat[wCut,length(wavl)])

    par(cex = cex, mar = mar)
    matplot(
      delay,mat[,dCut],type = 'l', ylim = ylim,
      xlab = 'Delay', ylab = 'DO',
      xaxs='i', yaxs='i'
    )
    if(input$keepCbl !=0) {
      abline(
        v = Inputs$delayOrig[input$keepCbl],lwd = 2,col = 'red',lty = 2
      )
      rect(Inputs$delayOrig[1],
           ylim[1],
           Inputs$delayOrig[input$keepCbl],
           ylim[2],
           border=NA,
           col=pink_tr #'gray70'
      )
    }

  }, height = 400)
  
  observeEvent(input$wavlCutSave,
               isolate({
                 mat   = Inputs$mat
                 wavl  = Inputs$wavl
                 delay = Inputs$delay
                 dCut = input$keepWlCut
                 indx = indxCuts(dCut,wavl)$indx
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
                 dCut = input$keepDlCut * Inputs$dlScaleFacOrig
                 indx = indxCuts(dCut,delay)$indx
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
    mat   = delMask(
      Inputs$mat  , 
      axis = Inputs$delay, 
      mask = input$keepDlMask * Inputs$dlScaleFacOrig, 
      dim = 1
    )
    mat   = delMask(
      mat  , 
      axis = Inputs$wavl , 
      mask = input$keepWlMask, 
      dim = 2)
    
    nsvMax = min(10,
                 length(Inputs$delay),
                 length(Inputs$wavl)
                 )
    svd(mat,nu = nsvMax,nv = nsvMax)
    
  })
  
  output$svdSV <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    
    par(cex = cex, mar = mar)
    plot(s$d[1:ncol(s$u)],ylab = "S. V.",log = "y")
    lines(s$d,col = "blue")
    grid()
    
  },height = 450)
  
  output$svdVec <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    
    # Reshape vectors
    C = unMask(s$u, axis = Inputs$delay, 
               mask = input$keepDlMask * Inputs$dlScaleFacOrig)
    S = unMask(s$v, axis = Inputs$wavl , 
               mask = input$keepWlMask)
 
    plotSVDVecBloc(C,S,Inputs$delay,Inputs$wavl)    

  },height = 500)
  
  output$svdResid <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    
    # Reshape vectors
    C = matrix(
      unMask(s$u[,1:input$nSV], axis = Inputs$delay, 
             mask = input$keepDlMask * Inputs$dlScaleFacOrig),
      ncol= input$nSV
    )
    S = matrix(
      unMask(s$v[,1:input$nSV], axis = Inputs$wavl,  
             mask = input$keepWlMask),
      ncol= input$nSV
    )
    
    plotResid(Inputs$delay,Inputs$wavl,Inputs$mat,C,S,
              d = s$d)
    
  },height = 450)
  
  output$svdContribs <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    
    # Reshape vectors
    C = matrix(
      unMask(s$u[,1:input$nSV], axis = Inputs$delay, 
             mask = input$keepDlMask * Inputs$dlScaleFacOrig),
      ncol= input$nSV
    )
    S = matrix(
      unMask(s$v[,1:input$nSV], axis = Inputs$wavl,  
             mask = input$keepWlMask),
      ncol= input$nSV
    )
    
    plotConbtribs(Inputs$delay,Inputs$wavl,Inputs$mat,C,S,
                  d = s$d, type ='svd')
    
  },height = 450)
  
# ALS ####
  getS0 <- eventReactive(
    input$S0File, {
      isolate({
        # tmp   = read.table(
        #   file = input$S0File$datapath, 
        #   header = FALSE, 
        #   dec = input$dec, 
        #   sep = input$sep,
        #   colClasses= 'numeric',
        #   stringsAsFactors = FALSE
        # )
        # S0_in = as.matrix(tmp)
        
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

  doALS <- eventReactive(
    input$runALS, {
    isolate({
      if (!checkInputsSanity())
        return(NULL)
      
      nAls = input$nALS
      updateSliderInput(
        session,
        inputId = "pairToRotate",
        min = 1,
        max = nAls,
        value = c(1:2),
        step = 1
      )
      
      # Suppress masked areas
      delay = delMask(
        Inputs$delay, 
        axis = Inputs$delay, 
        mask = input$keepDlMask*Inputs$dlScaleFacOrig
      )
      wavl  = delMask(
        Inputs$wavl , 
        axis = Inputs$wavl , 
        mask = input$keepWlMask
      )
      
      if(input$useFiltered) { # Choose SVD filtered matrix  
        s <- doSVD()
        mat = matrix(0,nrow=length(delay),ncol=length(wavl))
        for (ic in 1:input$nSV) 
          mat = mat + s$u[,ic] %o% s$v[,ic] * s$d[ic]
        
      } else {
        mat = Inputs$mat
        mat   = delMask(
          mat, 
          axis = Inputs$delay, 
          mask = input$keepDlMask * Inputs$dlScaleFacOrig, 
          dim = 1
        )
        mat   = delMask(
          mat, 
          axis = Inputs$wavl , 
          mask = input$keepWlMask, 
          dim = 2
        )
      }
      
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

      progress <- shiny::Progress$new()
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      msg = list()

      if (input$initALS != 'seq') {
        if (input$initALS == 'SVD') {
          # initialize with SVD
          if (is.null(s <- doSVD()))
            return(NULL)
          S = matrix(abs(s$v[,1:nAls]),ncol=nAls)
          C = matrix(abs(s$u[,1:nAls]),ncol=nAls)
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
          uniS = input$uniS,
          nonnegS = input$nonnegS,
          nonnegC = input$nonnegC,
          thresh = 1e-4,
          normS = 1,
          S0 =S0,
          optS1st = input$optS1st,
          smooth = input$smooth,
          updateProgress = updateProgress
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
            uniS = input$uniS,
            nonnegS = input$nonnegS,
            nonnegC = input$nonnegC,
            thresh = 1e-4,
            normS = 1,
            S0 = S0,
            optS1st = input$optS1st,
            smooth = input$smooth,
            updateProgress = updateProgress
          )
          S = res$S
          C = res$C
          msg = list(msg,
              h4('Step ',n-1,': ',n,' species'),
              h5('Results after ',res$iter,' iterations'),
              h5(res$msg),br()
            )
        }
      }
      
      # M = mat
      # M[M<0] = 0
      # res1 = nmf(M, nAls)
      # S = t(coef(res1))  
      # C = basis(res1)    
      # res=list(
      #   C = C, S = S, xC = delay, xS = wavl, Psi = mat,
      #   rss = 0, resid = 0, iter = 0,
      #   msg = summary(res1)
      # )
      
      # Sort contributions by decreasing amplitude
      cont = c()
      for (ic in 1:nAls)
        cont[ic] = sum(abs(res$C[,ic] %o% res$S[,ic]), na.rm = TRUE)
      perm  = order(cont, decreasing = TRUE)
      res$C = matrix(res$C[,perm],ncol=nAls)
      res$S = matrix(res$S[,perm],ncol=nAls)
      
      colnames(res$S) = paste0('S_',1:nAls)
      colnames(res$C) = paste0('C_',1:nAls)
      
      # Update Reporting
      updateCheckboxGroupInput(session,
                               inputId = 'inReport',
                               selected = c('SVD','ALS'))
      
      output[[paste0('iter', 1)]] <- renderUI({
        msg
      })
      
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
  
  output$alsResid <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    # Reshape vectors
    C = matrix(
      unMask(alsOut$C, axis = Inputs$delay, 
             mask = input$keepDlMask * Inputs$dlScaleFacOrig),
      ncol=input$nALS
    )
    S = matrix(
      unMask(alsOut$S, axis = Inputs$wavl,  
             mask = input$keepWlMask),
      ncol=input$nALS
    )

    if(input$useFiltered) { # Choose SVD filtered matrix  
      s <- doSVD()
      mat = matrix(0,nrow=nrow(s$u),ncol=nrow(s$v))
      for (ic in 1:input$nSV) 
        mat = mat + s$u[,ic] %o% s$v[,ic] * s$d[ic]
      mat = unMask(mat, axis = Inputs$delay, 
                 mask = input$keepDlMask * Inputs$dlScaleFacOrig,
                 dim=1)
      mat = unMask(mat, axis = Inputs$wavl,  
                 mask = input$keepWlMask,
                 dim=2)
      main = "SVD-filtered data"
    } else {
      mat = Inputs$mat
      main = 'Raw data'
    }
    plotResid(Inputs$delay,Inputs$wavl,mat,C,S,main=main)
    
  },height = 450)
  
  output$alsVectors <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    plotAlsVec(alsOut)

  },height = 400)
  
  observeEvent(
    input$alsSpKinSave,
    isolate({
      if (is.null(alsOut <- doALS()))
        return(NULL)
      S = cbind(alsOut$xS,alsOut$S)
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
      C = cbind(Inputs$delaySave,alsOut$C)
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
    
    # Reshape vectors
    C = matrix(
      unMask(alsOut$C, axis = Inputs$delay, 
             mask = input$keepDlMask * Inputs$dlScaleFacOrig),
      ncol=input$nALS
    )
    S = matrix(
      unMask(alsOut$S, axis = Inputs$wavl,  
               mask = input$keepWlMask),
      ncol=input$nALS)
    
    plotConbtribs(Inputs$delay,Inputs$wavl,Inputs$mat,C,S)
    
  },height = 450)
  
  doAmbRot <- eventReactive(
    input$runALSAmb, {
      isolate({
        twoVec = input$pairToRotate
        eps = input$alsRotAmbEps
        dens = input$alsRotAmbDens
      })
      
      alsOut <- doALS()
      nAls = ncol(alsOut$S)
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      msg = list()
      # Progress bar
      progress$set(message = "Running Ambiguity Analysis ", value = 0)
      
      S = alsOut$S[,twoVec]
      C = alsOut$C[,twoVec]
      
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
              updateProgress(value = iter / 100)
              
              # Transformation matrix
              R = matrix(c(1,t12,t21,1),
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
                
                # Test for positivity
                if(min(S1,na.rm=TRUE) >= eps &
                   min(C1,na.rm=TRUE) >= eps*max(Inputs$mat,na.rm=TRUE)) {
                  ikeep = ikeep+1
                  solutions[[ikeep]] = list(S1=S1, C1=C1,
                                            t12=t12, t21=t21)
                  OK1 = TRUE
                  OK2 = TRUE
                  if(s21 == 0) OK2 = FALSE
                  
                }
              }
            }
            if(s12 == 0) OK1 = FALSE
          }
        }
      }
      
      
      if(ikeep == 0) {
        cat(paste0("No solutions found over ",ntry," trials\n"))
        
      } else {
        alsOutAmb <<- solutions
        cols=1:nAls
        for (i in twoVec)
          cols[i]=col2tr(palette()[i],5)

        # # Check solutions
        # sdr = c()
        # for (i in 1:ikeep)
        #   sdr[i] = solutions[[i]]$resd
        # print(range(sdr))
        
        par(mfrow = c(1,3))
        
        par(cex=cex,mar=mar)
        S = alsOut$S
        for (i in 1:ikeep) {
          S[,twoVec] = solutions[[i]]$S1
          # S = unMask(S, axis = Inputs$wavl,  
          #            mask = input$keepWlMask)
          matplot(
            alsOut$xS, S, type = 'p', pch = 19, col = cols, cex=0.5,
            ylim=c(eps,1-eps),
            main = 'Spectra', xlab = 'Wavelength',
            add = i > 1
          )
        }
        abline(h = 0,lty = 2)
        grid()

        par(cex=cex,mar=mar)
        ylim= c(eps, max(Inputs$mat,na.rm=TRUE))
        C = alsOut$C
        for (i in 1:ikeep) {
          C[,twoVec] = solutions[[i]]$C1
          matplot(
            alsOut$xC, C, type = 'p', pch = 19, col = cols, cex=0.5,
            ylim=ylim,
            main = 'Kinetics', xlab = 'Delay',
            add = i > 1
          )
        }
        abline(h = 0,lty = 2)
        grid()
        
        par(cex=cex,mar=mar)
        x = y = vector("numeric",length = ikeep)
        for (i in 1:ikeep) {
          x[i] = solutions[[i]]$t12
          y[i] = solutions[[i]]$t21
        }
        xlim = c(-1.1,1.1)*max(abs(c(x,y)))
        matplot(
          x,y,type = 'p', pch=0, cex=1, col = 4,
          xlim = xlim, ylim = xlim,
          main = 'Transformation Coefs', 
          xlab = 't12', ylab='t21'
        )
        grid()
        
        # par(cex=cex,mar=mar)
        # sdr = c()
        # for (i in 1:ikeep) 
        #   sdr[i] = solutions[[i]]$resd
        # hist(
        #   sdr,col = pink_tr,
        #   xlab = '',main = 'Residuals'
        # )
        # grid()
        
      }
    }
  )
  
  output$alsRotAmb <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    doAmbRot()
  },height = 400)
  

# Report ####
  observe(updateTextInput(
    session,
    inputId = "reportName",
    value = paste0(input$projectTag,
                   '_Report')
  ))
  
  output$report = downloadHandler(
    filename = function() {
#       paste(input$reportName, sep = '.', switch(
#         input$format, html = 'html', pdf = 'pdf', docx = 'docx'
#       ))
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
  
# END ####
  
})
