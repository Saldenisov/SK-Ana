uresid  = function (pars,parms) {
  parms$mat - model(pars,parms)
}
uchisq  = function (pars,parms) {
  sum(uresid(pars,parms)^2)
}
ulogL   = function (pars,parms) {
  -length(parms$mat)/2*log(uchisq(pars,parms))
}
ulogP   = function (pars,paropt,parms) {
  logL =  ulogL(parExpand(pars,paropt),parms)  
  logP  = logL + logPri(pars)
  if ( is.nan(logP) || is.infinite(logP) ) logP=-1e30
  return(logP)
} 
mulogP  = function (pars,paropt,parms) {
  -ulogP(pars,paropt,parms)
}
wresid  = function (pars,parms) {
  (parms$mat - model(pars,parms))/parms$sigma
}
wchisq  = function (pars,parms) {
  sum(wresid(pars,parms)^2)
}
wlogL   = function (pars,parms) {
  -0.5*wchisq(pars,parms)
}
wlogP   = function (pars,paropt,parms) {
  logL =  wlogL(parExpand(pars,paropt),parms)  
  logP  = logL + logPri(pars)
  if ( is.nan(logP) || is.infinite(logP) ) logP=-1e30
  return(logP)
} 
mwlogP  = function (pars,paropt,parms) {
  -wlogP(pars,paropt,parms)
}
bmc_hyb = function (paropt, parms, 
                    mc=FALSE, global=30, startp=NULL, 
                    niter=0, tune=0.8, tol=1e-8,
                    weighted = FALSE) {   
  
  pars=parContract(paropt)
  np=length(pars$p0) 
  
  if (niter != 0) {
    kinPrint$glOut <<- capture.output(
      xopt <- rgenoud::genoud(
        fn = ifelse(weighted,mwlogP,mulogP), 
        parms = parms, 
        paropt = paropt,
        starting.values = startp,
        nvars = np,
        BFGS=FALSE,
        print.level=1,
        max.generations=niter, 
        wait.generations=5,
        gradient.check=FALSE,
        pop.size = global,
        Domains=cbind(pars$LB,pars$UB),
        boundary.enforcement=2
      )
    )
    best1 = xopt$par    
  } else {
    if(!is.null(startp))
      best1 = startp
    else
      best1 = pars$p0
  } 
  
  best = NA
  kinPrint$optOut <<- capture.output(
    best <- Rsolnp::solnp(best1, 
                          fun = ifelse(weighted,mwlogP,mulogP), 
                          LB=pars$LB, UB=pars$UB, 
                          control=list(tol=tol,trace=1), 
                          parms=parms,paropt=paropt)
  )
  p1=best$pars
  names(p1) = names(paropt)
  
  out = list(
    map     = p1, 
    hessian = best$hessian,
    values  = best$values,
    cnv     = best$convergence
  )
  
  return( out )
}
