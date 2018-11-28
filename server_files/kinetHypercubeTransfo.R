# Hypercube transfo of NLopt parameters #############################
parExpand = function (popt,params) {
  p=params
  iopt=0
  for (item in names(params)) {
    if(is.numeric(params[[item]])) {
      p[[item]] = params[[item]]
    } else {
      iopt=iopt+1
      parts     = unlist(strsplit(params[[item]], split="[(,)]" ))
      priorPDF  = parts[1]
      paramPDF  = as.numeric(parts[2:3])
      p[[item]] =
        switch(priorPDF,
               unif  = paramPDF[1]+popt[iopt]*(paramPDF[2]-paramPDF[1]),
               norm  = paramPDF[1]+popt[iopt]*paramPDF[2], 
               tnorm = paramPDF[1]+popt[iopt]*paramPDF[2],
               lnorm = exp(paramPDF[1]+popt[iopt]*paramPDF[2])
        )              
    }  
  }
  return(p)
}
sdExpand = function (popt,params) {
  p=params
  iopt=0
  for (item in names(params)) {
    if(is.numeric(params[[item]])) {
      p[[item]]=params[[item]]
    } else {
      iopt=iopt+1
      parts = unlist(strsplit(params[[item]], split="[(,)]" ))
      priorPDF=parts[1]
      paramPDF=as.numeric(parts[2:3])
      p[[item]]=
        switch(priorPDF,
               unif  = popt[iopt]*(paramPDF[2]-paramPDF[1]),
               norm  = popt[iopt]*paramPDF[2], 
               tnorm = popt[iopt]*paramPDF[2], 
               lnorm = 1
        )              
    }  
  }
  return(p)
}
parContract = function (params) {
  p0=c(); LB=c(); UB=c(); names=c(); priorPDF=c()
  paramPDF=matrix(0,ncol=2,nrow=length(params))
  iopt=0
  for (item in names(params)) {
    if(!is.numeric(params[[item]])) {
      iopt=iopt+1
      names[iopt]=item
      parts = unlist(strsplit(params[[item]], split="[(,)]" ))
      priorPDF[iopt]=parts[1]
      paramPDF[iopt,1:2]=as.numeric(parts[2:3])
      if(priorPDF[iopt]=="unif") {
        p0[iopt] = 0.5
        LB[iopt] = 0
        UB[iopt] = 1       
      } else {
        p0[iopt] = 0
        LB[iopt] =-3
        UB[iopt] = 3
      }
    } 
  }
  paramPDF=paramPDF[-(iopt+1:length(params)),]
  
  return(list(p0=p0,LB=LB,UB=UB,names=names,priorPDF=priorPDF,paramPDF=paramPDF))
}
sampleContract = function(sample,paropt) {
  # Contract sample to variable parameters
  psample=sample
  for (item in names(paropt)) {
    if(is.numeric(paropt[[item]]))
      psample=psample[,-which(colnames(psample) == item)]
  }
  psample
}
genPriorPDF = function(paropt) {
  priorPDF=parContract(paropt)$priorPDF
  bodyFunc="{logpri = 0\n" 
  for (ip in 1:length(priorPDF)) {
    add = switch(priorPDF[ip],
                 # Special arguments for tnorm...
                 tnorm  = paste0("logpri = logpri + d",priorPDF[ip],
                                 "(x=x[",ip,"],0,1,lower=-3,upper=3,log=TRUE)\n"),
                 paste0("logpri = logpri + d",priorPDF[ip],
                        "(x=x[",ip,"],0,1,log=TRUE)\n")
    )  
    bodyFunc = paste0(bodyFunc,add)
  }
  bodyFunc=paste0(bodyFunc,"return(logpri)\n}")
  f = function(x) {}
  body(f) = parse(text =bodyFunc) 
  return(f)
}
c2w = function (x,ip,pars) {
  priorPDF= pars$priorPDF[ip]
  paramPDF= pars$paramPDF[ip,]
  xw=switch(priorPDF,
            unif  = paramPDF[1]+x*(paramPDF[2]-paramPDF[1]),
            norm  = paramPDF[1]+x*paramPDF[2], 
            tnorm = paramPDF[1]+x*paramPDF[2], 
            lnorm = exp(paramPDF[1]+x*paramPDF[2])
  )  
  return(xw)
}
w2c = function (x,ip,pars) {
  priorPDF= pars$priorPDF[ip]
  paramPDF= pars$paramPDF[ip,]  
  xw=switch(priorPDF,
            unif  = (x-paramPDF[1])/(paramPDF[2]-paramPDF[1]),
            norm  = (x-paramPDF[1])/paramPDF[2], 
            tnorm = (x-paramPDF[1])/paramPDF[2], 
            lnorm = (log(x)-paramPDF[1])/paramPDF[2]
  )  
  return(xw)
}
w2cVec = function (vec,pars) {
  vw=c()
  for (i in 1:length(vec))  vw[i]=w2c(vec[i],i,pars)
  return(vw)
}
priorDensity = function(item,paropt) {
  pars=parContract(paropt)
  priorPDF= pars$priorPDF
  pnames  = pars$names
  LB      = pars$LB
  UB      = pars$UB
  ip= which(pnames == item)
  nout=500
  xtab=seq(LB[ip],UB[ip],length.out=nout)
  ytab=c();xw=c()
  for (i in 1:nout) {
    x=xtab[i]
    xw[i] = c2w(x,ip,pars)
    ytab [i] = switch(priorPDF[ip],
                      # Special arguments for tnorm...
                      tnorm  = eval(call(paste("d",priorPDF[ip],sep=""),
                                         x,0,1,lower=-3,upper=3,log=FALSE)),
                      eval(call(paste("d",priorPDF[ip],sep=""),x,0,1,log=FALSE))
    )
  }
  xw=c(xw[1],xw,xw[nout])
  ytab=c(0,ytab,0)
  return(cbind(xw,ytab/max(ytab)))
}
priorSampler = function(paropt) {
  pars=parContract(paropt)
  priorPDF= pars$priorPDF
  pnames  = pars$names
  samp=c()
  for (ip in 1:length(pnames)) {
    samp[ip] = switch(priorPDF[ip],
                      # Special arguments for tnorm...
                      tnorm  = eval(call(paste("r",priorPDF[ip],sep=""),1,0,1,
                                         lower=-3,upper=3)),
                      eval(call(paste("r",priorPDF[ip],sep=""),1,0,1))
    )
  }
  return(samp)
}
startpInit = function(map, parOpt) {
  # Transfer MAP values to startp with 
  # overlapping set of parameters
  
  # 1- Initialize to center of prior
  pC = parContract(parOpt)
  startp = pC$p0
  names(startp) = pC$names
  
  # 2- Recycle values of previous MAP, if any
  if(!is.null(map)) {
    for (item in pC$names) {
      if(item %in% names(map))
        startp[item] = map[item]
    }
  }
  
  return(startp)
}
SAPlot = function(X,cex=1) {
  sdX=apply(X,2,sd) 
  par(cex=cex,cex.axis=1.5*cex)
  pairs(X[,sdX != 0], gap=0,
        upper.panel=panel.cor,
        diag.panel =panel.hist,
        lower.panel=panel.smooth )
}
panel.hist <- function(x,...) {
  usr <- par("usr"); on.exit(par(usr))     
  par(usr = c(usr[1:2], 0, 1.5))     
  h <- hist(x, plot = FALSE)     
  breaks <- h$breaks;
  nB <- length(breaks)   
  y <- h$counts; y <- y/max(y)
  grid(col='brown',ny=0)
  rect(breaks[-nB],0,breaks[-1],y,col="orange",...)
}   
panel.cor <- function(x,y,digits=2,prefix="",cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,method="spearman")
  ra = abs(r)
  txt <- format(c(r,0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)*ra
  text(0.5,0.5,txt,cex = cex.cor,col=ifelse(r>=0,4,2))
}
panel.smooth <- function (x, y, cex = 1.5, col.smooth = "red", 
                          span = 2/3, iter = 3, ...) {
  maxPoints=500
  nP=min(maxPoints,length(x))
  iSamp = seq.int(1,length(x),length.out=nP)
  x1=x[iSamp]
  y1=y[iSamp]
  green_tr=rgb(unlist(t(col2rgb("darkgreen"))),
               alpha=30,maxColorValue = 255)
  grid(col='brown')
  points(x1, y1, pch = 19, col = green_tr, lwd=0, cex = cex)
}
