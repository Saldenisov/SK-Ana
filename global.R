# enableBookmarking(store = "server")

options(shiny.maxRequestSize=20*1024^2)
# options(shiny.json.digits=32)

# Libraries ####
libs = c('outliers', 'nnls', 'Iso', 'viridis', 
         'changepoint', 'shiny', 'shinyBS','DT', 
         'fields', 'NMF','shinycssloaders')
for (lib in libs ) {
  if(!require(lib,character.only = TRUE,quietly=TRUE))
    install.packages(lib,dependencies=TRUE)
  library(lib,character.only = TRUE,quietly=TRUE)
}

# Colors ####
cols    = viridis::viridis(128)
col2tr  = function(col,alpha)
  rgb(unlist(t(col2rgb(col))),alpha = alpha,maxColorValue = 255)
cyan_tr = col2tr("cyan",120)
pink_tr = col2tr("pink",120)
mask_tr = "gray95"
colWR   = fields::two.colors(17,start="blue",middle="white",end="red")

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

getC  = function (S, data, C, nonnegC=TRUE, 
                  nullC = NA, closeC=FALSE, wCloseC = 0) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  
  S[which(is.nan(S))] = 1
  
  # Soft closure constraint
  if (closeC)
    if (wCloseC !=0 ) {
      S    = rbind(S   ,rep(wCloseC,ncol(S))   )
      data = cbind(data,rep(wCloseC,nrow(data)))
    }
  
  for (i in 1:nrow(data)) {
    if (nonnegC)
      cc = try(nnls(S ,data[i,]))
    else
      cc = try(qr.coef(qr(S) , data[i,]))
    
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
  
  if(!anyNA(nullC))
    if(ncol(nullC) == ncol(C))
      C = C * nullC
    
    # Hard closure constrained (replaced by soft) 
    # if(closeC)
    #   C = C / rowSums(C, na.rm = TRUE)
    
    return(C)
}
getS  = function (C, data, S, xS, nonnegS, uniS, 
                  S0, normS, smooth, SumS, hardS0,
                  wHardS0) {
  # Adapted from ALS package (KM Muellen)
  #   Katharine M. Mullen (2015). ALS: Multivariate Curve Resolution
  #   Alternating Least Squares (MCR-ALS). R package version 0.0.6.
  #   https://CRAN.R-project.org/package=ALS
  # 2017-12-07 : replaced direct substitution of S0 by direct elimination 
  
  C[which(is.nan(C))] = 1
  
  if (!is.null(S0)) {
    nS0 = ncol(S0)
    if(hardS0) {
      # Enforce equality to external spectrum by direct elimination
      C0 = matrix(C[,1:nS0],ncol=nS0)
      C  = matrix(C[,(nS0+1):ncol(C)],ncol=ncol(C)-nS0)
      S  = matrix(S[,(nS0+1):ncol(S)],ncol=ncol(S)-nS0)
      
      contrib = matrix(0,nrow=nrow(data),ncol=ncol(data))
      for (i in 1:nS0)
        contrib = contrib + C0[,i] %o% S0[,i]
      
      data = data - contrib
      data[!is.finite(data)] = 0
    } else {
      # Augment equations system by weighted constraint
      C    = rbind(C,matrix(0,nrow=nS0,ncol=ncol(C)))
      for (i in 1:nS0)
        C[nrow(C)-nS0+i,i] = wHardS0
      data = rbind(data,wHardS0*t(S0))
    }
  }
  
  for (i in 1:ncol(data)) {
    
    if (nonnegS)
      s <- try(nnls::nnls(C, data[,i]))
    else
      s <- try(qr.coef(qr(C), data[,i]))
    
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
  
  if (!is.null(S0) & hardS0) {
    # Combine optimized spectra with constrained ones
    S = cbind(S0,S)
    C = cbind(C0,C) 
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
                  silent = TRUE, SumS = FALSE, hardS0 = TRUE,
                  wHardS0 = 1.0,
                  nullC = NA, closeC=FALSE, wCloseC = 0,
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
  
  if(!is.null(S0)) {
    if(!is.matrix(S0))
      S0 = matrix(S0,ncol=1,nrow=length(S0))
    if (normS) { 
      # Spectra normalization
      if (SumS) {
        # Area 
        for (i in 1:ncol(S0)) 
          S0[,i] = S0[,i] / sum(S0[,i])                          
      } else {
        # Amplitude 
        for (i in 1:ncol(S0)) 
          S0[,i] = S0[,i] / ifelse(max(S0[,i] > 0),max(S0[,i]),1) 
      }
    }
  }
  
  if (!silent)
    cat("Initial RSS", initialrss, "\n")
  b <- ifelse(optS1st,1,0)
  iter <- 0
  oneMore <- TRUE
  while ((abs(RD) > thresh  && maxiter >= iter) || oneMore) {
    
    iter <- iter + 1
    
    if (iter %% 2 == b)
      S = getS(C, Psi, S, xS, nonnegS, uniS, 
               S0, normS, smooth, SumS, hardS0, wHardS0)
    else
      C = getC(S, Psi, C, nonnegC, nullC, closeC, wCloseC)
    
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
  lof = signif(100*(sum(resid^2)/sum(Psi^2))^0.5,4)
  msg = HTML(paste0(
    "Initial RSS / Final RSS = ", signif(initialrss,3),
    "/", signif(rss,3), " = ", signif(initialrss / rss,3),
    "<br/> |RD| : ", signif(abs(RD),3)," <= ",thresh,
    "<br/> L.O.F. = ", lof 
  ))
  
  if (!silent)
    cat(msg)
  
  return(list(
    C = C, S = S, xC = xC, xS = xS, Psi = Psi,
    rss = rss, resid = resid, iter = iter,
    msg = msg, lof = lof
  ))
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
          if(!is.null(updateProgress))
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
                          if(!is.null(updateProgress))
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


autoDlMask <- function (mat,nmat) {
  # Locate empty delay areas (experimental)
  
  # Integrate on wavl
  trace = rowSums(mat)
  
  # Special treatment for nmat=1 
  # (SegNeigh fails with Q=2 !!!)
  ans = changepoint::cpt.var(diff(trace), penalty='BIC', 
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
  ans = changepoint::cpt.var(diff(trace), penalty='BIC', 
                             method = 'SegNeigh', 
                             Q = 2 + max(1, 2*(nmat-1))
  )
  chp = sort(cpts(ans))
  
  return(chp)  
}
cleanUp <- function (mat,level) {
  # Remove glitches from matrix by detecting outliers in SVD residuals...
  
  # Replace NAs by mean
  mat0 = mat
  mat0[is.na(mat)] = mean(mat,na.rm=TRUE)
  
  # Get SVD residuals at specified level
  nsvMax = 10
  s = svd(mat0,nu = nsvMax,nv = nsvMax)
  for (i in 1:(level-1))
    mat0 = mat0 - s$u[,i] %o% s$v[,i] * s$d[i]
  
  # Detect the problematic matrix column and return its index
  out = which.max(rowSums(abs(mat0)))
  
  return(out)
}

