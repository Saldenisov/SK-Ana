options(shiny.maxRequestSize=20*1024^2)
# options(shiny.json.digits=32)

# Libraries ####
library(outliers)
library(nnls)
library(Iso)
library(viridis)
library(shiny)
library(DT)
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
plotResid <- function (delay,wavl,mat,C,S,
                       d = rep(1,ncol(C)),
                       main = 'Data',...) {
  # Buid model matrix
  matAls = rep(0,nrow=nrow(mat),ncol=ncol(mat))
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

plotAlsAmbRot = function(alsOut,solutions,twoVec,eps,ylim){
  
  ikeep = length(solutions)
  
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
    matplot(
      alsOut$xS, S, type = 'p', pch = 19, cex=0.5,
      ylim=c(eps,1-eps),
      main = 'Spectra', xlab = 'Wavelength',
      add = i > 1
    )
  }
  abline(h = 0,lty = 2)
  grid()
  
  par(cex=cex,mar=mar)
  C = alsOut$C
  for (i in 1:ikeep) {
    C[,twoVec] = solutions[[i]]$C1
    matplot(
      alsOut$xC, C, type = 'p', pch = 19, cex=0.5,
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
}


# Server ####
shinyServer(function(input, output, session) {
   
# Initialize ####
  if(!dir.exists("outputDir"))
     dir.create("outputDir",showWarnings = FALSE)
  
  nMasks = 4 # Max number of masks in each dimension
  
  projConfig = NULL
  S0_in      = NULL
  RawData    = NULL
  
  Inputs = reactiveValues(
    gotData        = FALSE,
    process        = FALSE,
    fileOrig       = NULL,
    matOrig        = NULL,
    wavlOrig       = NULL,
    delayOrig      = NULL,
    dlScaleFacOrig = NULL,
    delayMask      = NA,
    wavlMask       = NA,
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
      wlMaskSel1 = config$keepWlMask1
      wlMaskSel2 = config$keepWlMask2
      wlMaskSel3 = config$keepWlMask3
      wlMaskSel4 = config$keepWlMask4
      wlCutSel   = config$keepWlCut
      dlRangeSel = config$keepDlRange
      dlMaskSel1 = config$keepDlMask1
      dlMaskSel2 = config$keepDlMask2
      dlMaskSel3 = config$keepDlMask3
      dlMaskSel4 = config$keepDlMask4
      dlCutSel   = config$keepDlCut
      cblSel     = config$keepCbl
    } else {
      # Initialize
      doRangeSel = as.vector(quantile(mat,probs = c(0.01,0.99),
                                      na.rm = TRUE))
      wlRangeSel = wlRange
      wlMaskSel1  = c(wlMask[1],wlMask[1])
      wlMaskSel2  = c(wlMask[1],wlMask[1])
      wlMaskSel3  = c(wlMask[1],wlMask[1])
      wlMaskSel4  = c(wlMask[1],wlMask[1])
      wlCutSel   = signif(mean(wlCut),3)
      dlRangeSel = dlRange
      dlMaskSel1  = c(dlMask[1],dlMask[1])
      dlMaskSel2  = c(dlMask[1],dlMask[1])
      dlMaskSel3  = c(dlMask[1],dlMask[1])
      dlMaskSel4  = c(dlMask[1],dlMask[1])
      dlCutSel   = signif(mean(dlCut),3)
      cblSel     = cblRange[1]
    }

    # DO slider
    updateSlider("keepDoRange", doRange, doRangeSel, 200)

    # Wavelength sliders
    nsteps = min(length(wavl),200)
    updateSlider("keepWlRange", wlRange, wlRangeSel, nsteps)
    updateSlider("keepWlMask1", wlMask , wlMaskSel1, nsteps)
    updateSlider("keepWlMask2", wlMask , wlMaskSel2, nsteps)
    updateSlider("keepWlMask3", wlMask , wlMaskSel3, nsteps)
    updateSlider("keepWlMask4", wlMask , wlMaskSel4, nsteps)
    updateSlider("keepWlCut"  , wlCut  , wlCutSel  , nsteps)
    
    # Delay sliders
    nsteps = min(length(delay),500)
    updateSlider("keepDlRange", dlRange, dlRangeSel, nsteps)
    updateSlider("keepDlMask1", dlMask , dlMaskSel1, nsteps)
    updateSlider("keepDlMask2", dlMask , dlMaskSel2, nsteps)
    updateSlider("keepDlMask3", dlMask , dlMaskSel3, nsteps)
    updateSlider("keepDlMask4", dlMask , dlMaskSel4, nsteps)
    updateSlider("keepDlCut"  , dlCut  , dlCutSel  , nsteps)

    
    # Baseline correction slider
    nsteps = round(diff(cblRange)/10)
    updateSlider("keepCbl"    , cblRange, cblSel   , nsteps)

    # Update Reporting
    updateCheckboxGroupInput(session,
                             inputId = 'inReport',
                             selected = c('SVD'))
    
  }
  getRawData <- function (fileNames) {
    Inputs$gotData <<- FALSE
    RawData <<- list()  # Init list in upper environment
    Inputs$fileOrig <<- NULL # Invalid earlier data
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    progress$set(message = "Reading data file(s) ", value = 0)
    for(i in 1:nrow(fileNames)) {
      fName = fileNames[i,'name']
      updateProgress(value  = i / nrow(fileNames),
                     detail = fName)
      fPath = fileNames[i,'datapath']
      O = getOneMatrix(fPath)
      O$name = fName
      RawData[[i]] <<- O
    }
    Inputs$gotData <<- TRUE
    output$loadErrorNew <- renderUI({
      h4('Loaded data')
    })
  }
  getOneMatrix <- function(dataFile) {
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
  getMeanMatrix <- function(fileNames) {
    
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
  doMeanMatrix  <- function(sel) {
    
    # First pass: build full delay and wavl tables 
    # (some matrices mignt have missing rows and/or columns)
    delay=c()
    wavl=c()
    for (i in 1:length(sel)) {
      j = sel[i]
      delay = c(delay,RawData[[j]]$delay)
      wavl  = c(wavl, RawData[[j]]$wavl )
    }
    delay=sort(unique(delay))
    wavl =sort(unique(wavl ))
  
    # Load all matrices, recast them in the full coords
    matTab=array(NA,dim=c(length(sel),length(delay),length(wavl)))
    for (i in 1:length(sel)) {
      j = sel[i]
      matm = RawData[[j]]$mat
      wav  = RawData[[j]]$wavl
      del  = RawData[[j]]$delay

      matTab[i,
             which ((delay %in% del) == TRUE),
             which ((wavl %in% wav) == TRUE)] = 
        matm[1:length(del),1:length(wav)]
    }
    matTab[!is.finite(matTab)] = NA
    
    # Take the mean 
    matm = sigma = matrix(NA,ncol=length(wavl),nrow=length(delay))
    for (i in 1:dim(matm)[1]) {
      for (j in 1:dim(matm)[2]) {
        v = matTab[,i,j]
        v = v[!is.na(v)]
        if(length(v) >=1) {
          effData    = rm.outlier(v)
          matm[i,j]  = mean(effData,na.rm = TRUE)
        }
      }
    } 
    matm[!is.finite(matm)] = 0
    
    return(list(mat=matm, wavl=wavl, delay=delay, delaySave=delay))
  }
  getTileMatrix <- function(fileNames, tileDel=TRUE) {
    
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
  doTileMatrix  <- function(sel, tileDel=TRUE) {
    nbFiles = length(sel)
    for (i in 1:nbFiles) {
      j = sel[i]
      mat1 = RawData[[j]]$mat
      wav1 = RawData[[j]]$wavl
      del1 = RawData[[j]]$delay
      delS1 = RawData[[j]]$delaySave
      
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
  combineMatrix <- function(sel){
    if(is.null(sel)) 
      return(NULL)
    
    if(length(sel) == 1) {
      list(
        mat       = RawData[[sel]]$mat, 
        delay     = RawData[[sel]]$delay, 
        wavl      = RawData[[sel]]$wavl, 
        delaySave = RawData[[sel]]$delay
      )
    } else {
      switch( input$procMult,
              avrg    = doMeanMatrix(sel),
              tileWav = doTileMatrix(sel,tileDel=FALSE),
              tileDel = doTileMatrix(sel,tileDel=TRUE)
      )
    }
  }
  finishMatrix  <- reactive({
    if(!Inputs$process)
      return(NULL)
    
    data = combineMatrix(input$rawData_rows_selected)

    if(is.null(data)) {
      Inputs$fileOrig       <<- NULL
      
    } else {
      isolate({
        # ckeck for load errors
        loadError  = FALSE
        loadErrMsg = ""
        if(is.null(data)) {
          loadError  = TRUE
          loadErrMsg = "Please select data file(s)..."  
        }
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
          }
          
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
    }
  })
  
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
    getRawData(input$dataFile)
  )
  output$rawData = DT::renderDataTable({
    if(!Inputs$gotData) 
      return(NULL)
    
    ndelay  = nwavl = name = size = c()
    for (i in 1:length(RawData)) {
      name[i]   = RawData[[i]]$name
      ndelay[i] = length(RawData[[i]]$delay)
      nwavl[i]  = length(RawData[[i]]$wavl)
      size[i]   = format(object.size(RawData[[i]]$mat),units="Mb")
    }
    datatable(cbind(id=1:length(RawData),name,ndelay,nwavl,size),
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
    validate(
      need(
        Inputs$gotData, 
        "Please select data file(s)..."
      )
    )
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
  output$ui      = renderUI({
    if(!Inputs$gotData)
      return(NULL)
    if(length(input$rawData_rows_selected) <= 1) {
      Inputs$process <<- TRUE
      finishMatrix()
      return(NULL)
    } 
    
    fluidRow(
      column(6,
      radioButtons(
        inputId = 'procMult', 
        label   = 'Multiple files processing',
        choices = list("Average"   = 'avrg',
                       "Tile Wavl" = 'tileWav',
                       "Tile Delay"= 'tileDel'),
        selected= 'avrg',
        inline = TRUE)
      ),
      column(2,
      actionButton("process",strong("Do it!")),
      tags$style(type='text/css',
                 "#process { width:100%; margin-top: 5px;}")
      )
    )
    
  })

  observeEvent(
    input$process,
    isolate({
      Inputs$process <<-TRUE
      finishMatrix()
    })
  )

  # Open saved project
  observeEvent(
    input$projectFile,
    isolate({
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
  
  output$projectInfoNew <- renderPrint({
    if(!Inputs$gotData)
      return(NULL)
    if(is.null(input$rawData_rows_selected))
      return(NULL)
    validate(
      need(
        checkInputsSanity(), 
        "Please choose processing option"
      )
    )
    
    ## Check file read
    cat(paste0(
      'Processed matrix: ',
      length(Inputs$delayOrig),'x',
      length(Inputs$wavlOrig),'\n'
    ))
    cat('O.D.  range: ',range(Inputs$mat),'...\n')
    cat('Delay range: ',range(Inputs$delayOrig),'...\n')
    cat('Wavl  range: ',range(Inputs$wavlOrig),'...\n')

  })

  output$projectInfoOpen <- renderPrint({
# BETTER EXTENSION: Rda ????
    validate(
      need(
        !is.null(input$projectFile), 
        "Please select a project file (*.ska)"
      )
    )
    
# TO BE DONE PROPERLY....
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
# BETTER EXTENSION: Rda ????
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
 
    
    # Aggregate and apply masks
    delayMask = rep(0,length(delay))
    wavlMask  = rep(0,length(wavl))
    for (mask in 1:nMasks) {
      maskName = paste0("keepDlMask",mask)
      xlim = input[[maskName]] * Inputs$dlScaleFacOrig
      if (diff(xlim) != 0) {
        sel = delay >= xlim[1] & delay <= xlim[2]
        if(sum(sel)!=0) delayMask[sel]  = NA
      }
      maskName = paste0("keepWlMask",mask)
      ylim = input[[maskName]]
      if (diff(ylim) != 0) {
        sel = wavl >= ylim[1] & wavl <= ylim[2]
        if(sum(sel)!=0) wavlMask[sel]  = NA
      }
    }
    Inputs$delayMask <<- delayMask
    Inputs$wavlMask  <<- wavlMask
    
    mat[is.na(delayMask),] = NA
    mat[,is.na(wavlMask)]  = NA
    
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
      xlab = 'Wavelength', ylab = 'O.D.', 
      ylim = input$keepDoRange,
      main = paste0('Mean O.D. at delay: ',signif(mean(delay[indx]),3),
                    ifelse(delta==0,
                           '',
                           paste0(' +/- ',signif(delta / 2,2))
                           )
                   )
            )
    abline(h = 0,lty = 2)
    grid();box()

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
      xlab = 'Delay', ylab = 'O.D.', 
      ylim = input$keepDoRange,
      main = paste0('Mean O.D. at wavl: ',signif(mean(wavl[indx]),3),
                    ifelse(delta==0,
                           '',
                           paste0(' +/- ',signif(delta / 2,2))
                    )
      )
    )
    abline(h = 0,lty = 2)
    grid();box()
    
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
    grid();box()
    # mtext(signif(delay[wCut],3),side=3,at=mat[wCut,length(wavl)])

    par(cex = cex, mar = mar)
    matplot(
      delay,mat[,dCut],type = 'l', ylim = ylim,
      xlab = 'Delay', ylab = 'DO',
      xaxs='i', yaxs='i'
    )
    grid();box()
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
    CS = reshapeCS(s$u,s$v,ncol(s$u))
    plotSVDVecBloc(CS$C,CS$S,Inputs$delay,Inputs$wavl)    
  },height = 500)
  
  output$svdResid <- renderPlot({
    if (is.null(s <- doSVD()))
      return(NULL)
    CS = reshapeCS(s$u,s$v,input$nSV)
    plotResid(Inputs$delay,Inputs$wavl,Inputs$mat,
              CS$C,CS$S,d = s$d)
  },height = 450)
  
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
      delay = Inputs$delay[!is.na(Inputs$delayMask)]
      wavl  = Inputs$wavl[!is.na(Inputs$wavlMask)]
      
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
  
  doAmbRot <- eventReactive(
    input$runALSAmb, {
      if (is.null(alsOut <- doALS()))
        return(NULL)
      
      isolate({
        twoVec = input$pairToRotate
        eps = input$alsRotAmbEps
        dens = input$alsRotAmbDens
      })
      
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
      
      return(solutions)
    }
  )
  
  output$alsRotAmb <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    if (!is.list(solutions <- doAmbRot())) {
      cat(paste0("No solutions found over ",ntry," trials\n"))
      
    } else {
      isolate({
        twoVec = input$pairToRotate
        eps = input$alsRotAmbEps
        dens = input$alsRotAmbDens
        ylim= c(eps,1.1*max(Inputs$mat[is.finite(Inputs$mat)]))
        # ylim= c(eps,1.1*max(Inputs$mat))
      })
      plotAlsAmbRot(alsOut,solutions,twoVec,eps,ylim)
    }
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
