options(shiny.maxRequestSize=20*1024^2)
# options(shiny.json.digits=32)

# Libraries ####
libs = c('outliers', 'nnls', 'Iso', 'viridis', 'changepoint',
         'shiny', 'shinyBS','DT', 'fields', 'NMF')
for (lib in libs ) {
  if(!require(lib,character.only = TRUE,quietly=TRUE))
    install.packages(lib,dependencies=TRUE)
  library(lib,character.only = TRUE,quietly=TRUE)
}

# Colors ####
cols = viridis(128)
col2tr = function(col,alpha)
  rgb(unlist(t(col2rgb(col))),alpha = alpha,maxColorValue = 255)
cyan_tr = col2tr("cyan",120)
pink_tr = col2tr("pink",120)
colWR=fields::two.colors(17,start="blue",middle="white",end="red")

# Global graphical params ####
cex = 1
mar = c(4.5,5,2,1)
mgp = c(2,.75,0)
pty = 's'
tcl = -0.5
 
# Functions ####
string2Expr = function(string) {
  dat <- try(parse(text = string),silent = TRUE)
  if(!is(dat, 'try-error')) 
    return(dat)
  else
    return(NULL)
} 
string2Num = function(x) 
  as.numeric(eval(parse(text=eval(string2Expr(x)))))

getC  = function (S, Psi, C, nonnegC=TRUE, 
                  nullC = NA, closeC=FALSE) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS

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
  
  if(!anyNA(nullC) & 
     ncol(nullC) == ncol(C) )
    C = C * nullC
  
  if(closeC)
    C = C / rowSums(C, na.rm = TRUE)
  
  return(C)
}
getS  = function (C, Psi, S, xS, nonnegS, uniS, 
                  S0, normS, smooth, SumS) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  
  C[which(is.nan(C))] = 1
  
  for (i in 1:ncol(Psi)) {
    if (nonnegS) {
      # Non-negative least quares
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
    # Enforce unimodality
    for (i in 1:ncol(S))
      S[,i] = Iso::ufit(y = S[,i], x = xS)$y
  }
   
  if(smooth != 0) {
    # Smooth spectra
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
    # Enforce equality to external spectrum
    S[,1:ncol(S0)] = S0
  }
  
  if (normS) { 
    # Spectra normalization
    if (SumS) {
      # Area 
      for (i in 1:ncol(S)) 
        S[,i] = S[,i] / sum(S[,i])                          
    } else {
      # Amplitude 
      for (i in 1:ncol(S)) 
        S[,i] = S[,i] / ifelse(max(S[,i] > 0),max(S[,i]),1) 
    }
  }
  
  return(S)
}
myals = function (C, Psi, S, 
                  thresh = 0.001, 
                  maxiter = 100,
                  xC = 1:nrow(C), 
                  xS = 1:nrow(S),
                  nonnegC = TRUE, nonnegS = TRUE, optS1st = TRUE,
                  normS = TRUE, uniS = FALSE, S0 = NULL, smooth=0,
                  silent = TRUE, SumS = FALSE,
                  nullC = NA, closeC=FALSE,
                  updateProgress = NULL) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  
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
      S = getS(C, Psi, S, xS, nonnegS, uniS, S0, normS, smooth, SumS)
    else
      C = getC(S, Psi, C, nonnegC, nullC, closeC)
    
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
  # Build model matrix
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
      mgp = mgp, tcl = tcl, pty=pty)
  image(delay,wavl,wres,col=colWR,main="Weighted Residuals",
        xlab='Delay',ylab='Wavelength')
  image.plot(delay,wavl,wres,zlim=c(-3,3),col=colWR,add=TRUE,
             legend.mar=5, legend.shrink=0.8,
             xlab='Delay',ylab='Wavelength')
  
  matplot(sv$v,wavl,type="l",lwd=2,
          xlab="Sing. Vec.",ylab='Wavelength',
          main="SVD of Residuals")
  abline(v=0)
  
  matplot(delay,sv$u,type="l",lwd=2,
          xlab='Delay',ylab="Sing. Vec.",
          main="SVD of Residuals")
  abline(h=0)
  
  qqnorm(wres);abline(a=0,b=1,col="blue");grid(col="darkgray")
  
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
    alsOut$xC,alsOut$C,
    type = ifelse(length(alsOut$xC)>20,'p','b'),
    pch = 19, cex = 0.5, lwd=2, lty=3,
    xlab = 'Delay', ylab = 'C',
    main = 'ALS Kinetics',
    xaxs = 'i'
  )
  n=ncol(alsOut$C)
  legend('topright',legend=1:n,lty=3,lwd=3,col=1:n)
  grid();box()

  matplot(
    alsOut$xS,alsOut$S,
    type = ifelse(length(alsOut$xC)>20,'p','b'),
    pch = 19, cex = 0.5, lwd=2, lty=3,
    xlab = 'Wavelength',ylab = 'S',
    main = 'Area Normalized ALS Spectra', 
    xaxs = 'i'
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
          updateProgress(value = iter / 100)
          
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
            if(min(S1,na.rm=TRUE) >= eps &
               min(C1,na.rm=TRUE) >= eps*max(data,na.rm=TRUE)) {
              ikeep = ikeep+1
              solutions[[ikeep]] = list(S1=S1, C1=C1, 
                                        t12=t12, t21=t21)
              OK1 = OK2 = TRUE
              
              if(s21 == 0) OK2 = FALSE
            }
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
  
  return(solutions)
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
                          updateProgress(value = iter / 100)
                          
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
                            if(min(S1,na.rm=TRUE) >= eps &
                               min(C1,na.rm=TRUE) >= eps*max(data,na.rm=TRUE)) {
                              ikeep = ikeep+1
                              solutions[[ikeep]] = list(S1=S1, C1=C1, 
                                                        t12=t12, t21=t21,
                                                        t23=t23, t32=t32,
                                                        t13=t13, t31=t31)
                              
                              OK1 = OK2 = OK3 = OK4 = OK5 = OK6 = TRUE
                              
                              if(s31 == 0) OK6 = FALSE
                            }
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
  
  return(solutions)
}
plotRotAmb = function(alsOut, solutions){

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
  
  par(mfrow = c(1,2))
  par(cex = cex,cex.main=cex, mar = mar, 
      mgp = mgp, tcl = tcl, pty=pty)
  
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
  
  matplot(xC,C,type='n',
          ylim=range(c(C,Cmin,Cmax)),xaxs='i',
          main = 'Kinetics', xlab = 'Delay')
  abline(h = 0,lty = 2)
  grid(); box()
  if(sum(!sel) != 0) {
    C1 = C[,!sel]  
    matplot(xC,C1,type='p', pch=19, cex=0.5, col= col0, add=TRUE)
  }
  for (j in 1:nvec)
    polygon(c(xC,rev(xC)),c(Cmin[,j],rev(Cmax[,j])),
            col= colR[j],border = NA)
  
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
  
  matplot(xS,S,type='n',
          ylim=range(c(S,Smin,Smax)), xaxs='i',
          main = 'Area Normalized Spectra', xlab = 'Wavelength')
  abline(h = 0,lty = 2)
  grid(); box()
  if(sum(!sel) != 0) {
    S1 = S[,!sel]  
    matplot(xS,S1,type='p', pch=19, cex=0.5, col= col0, add=TRUE)
  }
  for (j in 1:nvec)
    polygon(c(xS,rev(xS)),c(Smin[,j],rev(Smax[,j])),
            col= colR[j],border = NA)
  
  # x = y = vector("numeric",length = nkeep)
  # for (i in 1:nkeep) {
  #   x[i] = solutions[[i]]$t12
  #   y[i] = solutions[[i]]$t21
  # }
  # xlim = c(-1.1,1.1)*max(abs(c(x,y)))
  # matplot(
  #   x,y,type = 'p', pch=0, cex=1, col = 4,
  #   xlim = xlim, ylim = xlim,
  #   main = 'Transformation Coefs',
  #   xlab = 't12', ylab='t21'
  # )
  # grid()
}

autoDlMask <- function (mat,nmat) {
# Locate empty delay areas (experimental)
  
  # Integrate on wavl
  trace = rowSums(mat)

  # Special treatment for nmat=1 
  # (SegNeigh fails with Q=2 !!!)
  ans = cpt.var(diff(trace), penalty='BIC', 
                method = 'SegNeigh', 
                Q = 2 + max(1, 2*(nmat-1))
                )
  if(nmat==1) 
    chp = cpts(ans)[2]
  else
    chp = cpts(ans)
  
  return(chp)  
}

autoWlMask <- function (mat,nmat) {
  # Locate useless wavl areas (experimental)
  
  # Integrate on wavl
  trace = colSums(mat)
  
  # Special treatment for nmat=1 
  # (SegNeigh fails with Q=2 !!!)
  ans = cpt.var(diff(trace), penalty='BIC', 
                method = 'SegNeigh', 
                Q = 2 + max(1, 2*(nmat-1))
  )
  chp = sort(cpts(ans))

  return(chp)  
}

# Server ####
shinyServer(function(input, output, session) {
   
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
    delayId        = NA
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
      dlMaskSel5 = config$keepDlMask5
      dlMaskSel6 = config$keepDlMask6
      dlMaskSel7 = config$keepDlMask7
      dlMaskSel8 = config$keepDlMask8
      dlCutSel   = config$keepDlCut
      cblSel     = config$keepCbl
    } else {
      # Initialize
      doRangeSel = as.vector(quantile(mat,probs = c(0.001,0.999),
                                      na.rm = TRUE))
      wlRangeSel = wlRange
      # wlMaskSel = list()
      # if(input$nMasksWl!=0)
      #   for (mask in 1:isolate(input$nMasksWl)) {
      #     wlMaskSel[[mask]]  = c(wlMask[1],wlMask[1])
      #   }
      wlCutSel   = signif(mean(wlCut),3)
      dlRangeSel = dlRange
      # dlMaskSel = list()
      # if(input$nMasksDl!=0)
      #   for (mask in 1:isolate(input$nMasksDl)) {
      #     dlMaskSel[[mask]]  = c(dlMask[1],dlMask[1])
      #   }
      dlCutSel   = signif(mean(dlCut),3)
      cblSel     = cblRange[1]
    }

    # DO slider
    updateSlider("keepDoRange", doRange, doRangeSel, 200)

    # Wavelength sliders
    nsteps = min(length(wavl),200)
    updateSlider("keepWlRange", wlRange, wlRangeSel, nsteps)
    updateSlider("keepWlCut"  , wlCut  , wlCutSel  , nsteps)
    # if(input$nMasksWl!=0)
    #   for (mask in 1:isolate(input$nMasksWl)) {
    #     maskName = paste0("keepWlMask",mask)
    #     updateSlider(maskName, wlMask , wlMaskSel[[mask]], nsteps)
    #   }
    # Delay sliders
    nsteps = min(length(delay),500)
    updateSlider("keepDlRange", dlRange, dlRangeSel, nsteps)
    updateSlider("keepDlCut"  , dlCut  , dlCutSel  , nsteps)
    # if(input$nMasksDl!=0)
    #   for (mask in 1:isolate(input$nMasksDl)) {
    #     maskName = paste0("keepDlMask",mask)
    #     updateSlider(maskName, dlMask , dlMaskSel[[mask]], nsteps)
    #   }
    updateNumericInput(session = session,
                       inputId = "nMasksDl",
                       value   = 0)
    updateNumericInput(session = session,
                       inputId = "nMasksWl",
                       value   = 0)
    
    # Baseline correction slider
    nsteps = round(diff(cblRange)/10)
    updateSlider("keepCbl"    , cblRange, cblSel   , nsteps)

    # Update Reporting
    updateCheckboxGroupInput(session,
                             inputId = 'inReport',
                             selected = c('SVD'))
    
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
    
    return(list(mat=mat, wavl=wavl, delay=delay, 
                delaySave=delay, delayId= rep(1,length(delay))))
    
  }
  getRawData <- function (fileNames) {
    
    # (Re)initialize data tables
    Inputs$gotData  <<- FALSE
    Inputs$validData<<- TRUE    # Data type assumed correct
    RawData         <<- list()  # Init list in upper environment
    Inputs$fileOrig <<- NULL    # Invalidate earlier data
    
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
          size = 's'
        ))
      }
      RawData[[i]] <<- O
    }
    Inputs$gotData <<- TRUE
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
    
    return(list(mat=matm, wavl=wavl, delay=delay, 
                delaySave=delay, delayId= rep(1,length(delay))))
    
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
    delay = 1:length(delay) # Replace by ordinal scale
    
    return(list(mat=matm, wavl=wavl, delay=delay, 
                delaySave=delay, delayId= rep(1,length(delay))))
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
        delayId = rep(i,length(del1))
        
      } else {
        if(tileDel) {
          # Tile matrices by delay (row)
          delay = c(delay,del1)
          delayId = c(delayId,rep(i,length(del1)))
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
    
    return(list(mat=mat, wavl=wavl, delay=delay, 
                delaySave=delaySave, delayId = delayId))
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
        delayId = rep(i,length(del1))
        
      } else {
        if(tileDel) {
          # Tile matrices by delay (row)
          delay = c(delay,del1)
          delayId = c(delayId,rep(i,length(del1)))
          mat   = rbind(mat,mat1)      
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
    sel = order(wavl)
    wavl=wavl[sel]
    mat = mat[,sel]
    
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
        delay     = RawData[[sel]]$delay, 
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
    
    # print(data)

    validate(
      need(!is.null(data),"--> Bad data type")
    )
    
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
          dlScaleFac = 1 #10^(floor(log10(diff(range(data$delay)))-1))

          # Install data
          Inputs$fileOrig       <<- input$dataFile$name
          Inputs$matOrig        <<- data$mat
          Inputs$wavlOrig       <<- data$wavl
          Inputs$delayOrig      <<- data$delay
          Inputs$delayIdOrig    <<- data$delayId
          Inputs$delaySaveOrig  <<- data$delaySave
          Inputs$dlScaleFacOrig <<- dlScaleFac
          Inputs$mat            <<- data$mat
          Inputs$wavl           <<- data$wavl
          Inputs$delay          <<- data$delay
          Inputs$delaySave      <<- data$delaySave
          Inputs$delayId        <<- data$delayId
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
      for (i in input$rawData_rows_selected) {
        ndelay[i] = length(RawData[[i]]$delay)
        nwavl[i]  = length(RawData[[i]]$wavl)
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
          size = 's'
        ))
      # } else if(length(choices) == 1 ) {
      #   h4('1 choice')
      #   Inputs$process <<- TRUE
      #   finishMatrix()
        
      } else {
        verticalLayout(
          column(6,
                 radioButtons(
                   inputId = 'procMult', 
                   label   = list(h4('Please choose processing option'),
                                  h5('Choice based on matrices dimensions')
                                  ),
                   # choices = list("Average"   = 'avrg',
                   #                "Tile Wavl" = 'tileWav',
                   #                "Tile Delay"= 'tileDel'),
                   choices = choices,
                   selected= choices[length(choices)],
                   inline = TRUE)
          ),
          column(2,
                 actionButton("process",strong("Do it!")),
                 tags$style(type='text/css',
                            "#process { width:100%; margin-top: 5px;}")
          )
        )
      }
      
    } 
    
  })
  # output$showui = reactive({
  #   Inputs$gotData && 
  #     Inputs$validData && 
  #     length(input$rawData_rows_selected) != 0
  # })
  # outputOptions(output, "showui", suspendWhenHidden = FALSE)
  
  observeEvent(
    input$process,
    isolate({
      Inputs$process <<- TRUE
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
      Inputs$delayIdOrig    <<- data$delayIdOrig
      Inputs$delaySaveOrig  <<- data$delaySaveOrig
      Inputs$dlScaleFacOrig <<- data$dlScaleFacOrig
      Inputs$mat            <<- data$matOrig
      Inputs$wavl           <<- data$wavlOrig
      Inputs$delay          <<- data$delayOrig
      Inputs$delaySave      <<- data$delaySaveOrig
      Inputs$delayId        <<- data$delayIdOrig
      
      # Restore project config
      initSliders(config)
      projConfig <<- config
      
    })
  )
  
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
    Inputs$gotData &&
      Inputs$validData &&
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
        if (diff(xlim) != 0) {
          sel = delay >= xlim[1] & delay <= xlim[2]
          if(sum(sel)!=0) delayMask[sel]  = NA
        }
      }
    if(input$nMasksWl!=0)
      for (mask in 1:input$nMasksWl) {
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
      if(input$procMult == 'tileDel')
        nmat = length(input$rawData_rows_selected)
      else
        nmat = 1
      
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
    }
    

    
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
          
          # 1/ filter matrix to avoid negative values (noise)
          fMat = rep(0,nrow=nrow(data),ncol=ncol(data))
          for (i in 1:nAls)
            fMat = fMat + s$u[,i] %o% s$v[,i] * s$d[i]
          # 2/ NMF
          res  = nmf(abs(fMat), rank=nAls, method='lee')
          C = basis(res)
          S = t(coef(res))
          
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
          normS = input$normS,
          S0 =S0,
          optS1st = input$optS1st,
          SumS = input$SumS,
          smooth = input$smooth,
          updateProgress = updateProgress,
          nullC = nullC,
          closeC = input$closeC
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
            normS = input$normS,
            S0 = S0,
            optS1st = input$optS1st,
            smooth = input$smooth,
            SumS = input$SumS,
            updateProgress = updateProgress,
            nullC = nullC,
            closeC = input$closeC
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
        rotVec = as.numeric(unlist(input$vecsToRotate))
        eps    = input$alsRotAmbEps
        dens   = input$alsRotAmbDens
      })
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      msg = list()
      # Progress bar
      progress$set(message = "Running Ambiguity Analysis ", value = 0)
      
      C0    = alsOut$C
      S0    = alsOut$S
      nullC = alsOut$nullC
      
      solutions = NULL
      if(length(rotVec)==2) {
        solutions = rotAmb2(C0, S0, data=Inputs$mat,nullC=nullC,
                            rotVec=rotVec,dens=dens,eps=eps,
                            updateProgress=updateProgress)
      } 
      else if(length(rotVec)==3) {
        solutions = rotAmb3(C0,S0,data=Inputs$mat,nullC=nullC,
                            rotVec=rotVec,dens=dens,eps=eps,
                            updateProgress=updateProgress)
      } 
      else {
        cat('Length of rotVec should be 2 or 3\n')  
      }
      return(solutions)
    }
  )
  
  output$alsRotAmb <- renderPlot({
    if (is.null(alsOut <- doALS()))
      return(NULL)
    
    if (!is.list(solutions <- doAmbRot())) 
      cat(paste0("No solutions found \n"))
    else 
      plotRotAmb(alsOut,solutions)
    
  },height = 400)
  
  # Define null concentrations constraints
  output$maskSpExp_ui = renderUI({
    nM = length(input$rawData_rows_selected) # Nb data matrices
    nS = input$nALS # Nb spectra
    if(nM*nS == 0)
      return(NULL)
    
    if(anyNA(Inputs$maskSpExp))
      Inputs$maskSpExp = matrix(1,nrow=nM,ncol=nS)
    else 
      if (nrow(Inputs$maskSpExp) != nM ||
          ncol(Inputs$maskSpExp) != nS   )
        Inputs$maskSpExp = matrix(1,nrow=nM,ncol=nS)     

    matInput=list(HTML('<table cellpadding=2 border=0>'))
    head = paste0(paste0('<th>Sp_',1:nS,'</th>'),collapse = '')
    matInput=list(matInput,HTML(paste0('<tr><td>&nbsp;</td>',head,'</tr>')))
    for (i1 in 1:nM) {
      var1 = paste0('Exp_',i1)
      matInput=c(matInput,list(HTML(paste0('<tr><th>',var1,'&nbsp;</th>'))))
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
