# Spectrokinetic model ##############################################
C.model = safely(function(t,y,parms) {
  dC = t(parms$kReac) %*% apply(parms$L,1,function(x) prod(y^x))
  return(list(dy=dC))
}, return_on_error = NULL)

kinet   = safely(function(pars,parms) {
  with(parms,{
    
    nExp = length(startd)
    
    # Update parameters with optimized values
    for (iExp in 1:nExp) {  
      for (spec in rownames(state)) {
        pName=paste0("logc0_",spec,'_',iExp)
        if(pName %in% names(pars)) state[spec,iExp] = exp(pars[[pName]])
      }
    }
    for (iReac in 1:length(kReac)) {
      pName=paste0("logk_",iReac)
      if(pName %in% names(pars)) kReac[iReac] = exp(pars[[pName]])
    } 
    
    ## Ionic force correction
    # A = 0.509 
    # # A = 2^0.5*Faraday^2*elec / (8*pi*(eps*R*Temp)^1.5)
    # B = 3.28 # for radius en nm 
    # # B = (2*Faraday^2/(eps*R*Temp))^0.5
    # kReacFI = c()
    # for (iExp in 1:nExp) {     
    #   # Ionic strength
    #   I = 0.5*sum(state[,iExp]*charge^2)
    #   for (iReac in 1:length(kReac)) {
    #     gamma = 0
    #     if (length(reactants[[iReac]]) == 2) {
    #       z1 = charge[reactants[[iReac]][1]]
    #       z2 = charge[reactants[[iReac]][2]]
    #       if(z1*z2 != 0) {
    #         r1 = radius[reactants[[iReac]][1]]
    #         r2 = radius[reactants[[iReac]][2]]
    #         gamma = -A*I^0.5*(
    #           z1^2 / (1+r1*B*I^0.5) +
    #             z2^2 / (1+r2*B*I^0.5) -
    #             (z1+z2)^2 / (1+(r1+r2)*B*I^0.5) 
    #         )
    #       }
    #     } 
    #     kReacFI[iReac] = kReac[iReac] * 10^gamma
    #   }
    
    parms$kReac = parms$D * kReac # kReacFI

    C = c()
    i0 = 0
    for (iExp in 1:nExp) {
      # Time grid for current experiment
      tExp = parms$times[(i0 + 1):startd[iExp]]
      i0 = startd[iExp]
      # tExp = tExp - tExp[1] + 10e-12 #+ parms$deltaStart[iExp]
      tc = c(0.0, tExp)
      conc = deSolve::ode(
        y = state[, iExp],
        times = tc,
        func = C.model,
        parms = parms,
        method = "lsoda",
        rtol = 1e-6,
        atol = 1e-8,
        verbose = FALSE
      )
      
      C = rbind(C, conc[-1, -1])
    }
    return(C)
  })      
}
spectra = function (C,pars,parms) {
  
  S = matrix(0,nrow=length(parms$wavl),ncol=ncol(C))
  colnames(S) = names(parms$active)[parms$active]
  colnames(C) = colnames(S)
  
  # Constraints
  freeS = rep(TRUE,ncol(S))
  names(freeS) = colnames(S)
  eps = parms$eps
  
  M = parms$mat
  for (spec in colnames(S)) {
    # Amplitude constraint (soft: applied at end)
    pName = paste0("logeps_",spec)
    if(pName %in% names(pars)) 
      eps[spec] = exp(pars[[pName]]) 
    
    # Spectral constraint (hard: remove from data)
    pName=paste0("S_",spec)
    if(pName %in% names(parms)) { 
      if(!is.null(parms[[pName]])) {
        freeS[[spec]]=FALSE
        S[,spec]= ifelse(
          !is.na(eps[spec]),
          eps[spec],
          1) * parms[[pName]]  
        M = M - C[,spec] %o% S[,spec]
      }
    }
  }
  
  if(sum(freeS)==1){
    # 1 free component: analytic expression
    spec=names(freeS)[freeS]
    x=C[,spec]
    xm=mean(x)
    for (i in 1:ncol(M)) {
      y=M[,i]
      ym=mean(y)
      S[i,spec]=max(0,sum((y-ym)*(x-xm))/sum((x-xm)*(x-xm)))
    }
    
  } else {
    for (i in 1:ncol(M)) {
      if (parms$nonnegS) {
        s <- try(nnls::nnls(C[,freeS],M[,i]))
      }
      else
        s <- try(qr.coef(qr(C[,freeS]), M[,i]))
      
      if (class(s) == "try-error") 
        S[i,freeS] = rep(0,sum(parms$active))
      else {
        S[i,freeS] = if (parms$nonnegS) s$x else s
      }
    }
  }
  
  if (parms$uniS) {
    for (i in 1:ncol(S))
      S[,i] = ufit(y = S[,i], x = y)$y
  }
  
  if(parms$smooth != 0) {
    for (i in 1:ncol(S)) {
      y=S[,i]
      x = 1:length(y)
      mod = try(loess(y~x,span=parms$smooth),
                silent=TRUE)
      if (class(mod) != "try-error") {
        y=predict(mod)
        if (parms$nonnegS)
          y[y<0] = 0
        S[,i]=y      
      } 
    }
  } 
  
  # Amplitude constraints    
  for (spec in colnames(S))
    S[,spec] = (S[,spec]/max(abs(S[,spec]))) * eps[spec]
  
  return(S)
}
model   = function(pars,parms) {
  
  C = kinet(pars,parms)
  Ca=matrix(nrow=nrow(C),ncol=sum(parms$active))
  Ca[,1:sum(parms$active)] = C[,parms$active]
  
  S = spectra(Ca,pars,parms)
  
  nx = length(parms$delay)
  ny = length(parms$wavl)  
  mod=matrix(0,nrow=nx,ncol=ny)
  for (i in 1:ncol(Ca)) {
    mod = mod + Ca[,i] %o% S[,i]
  }
  return(mod)
}
