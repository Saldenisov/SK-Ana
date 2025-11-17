kinParse = safely(function(scheme) {
  
  # Extract relevant parts
  parts=c()
  for (reac in scheme) {
    m = regexec("(.+?)\\s*->\\s*(.+?)\\s*(?:;\\s*(.*))?$",reac)
    if(m[[1]][1] != -1)
      parts = rbind(parts,unlist(regmatches(reac,m))[2:5])
  }
  parts = data.frame(parts, stringsAsFactors = FALSE)
  names(parts) = c("reactants", "products", "rateConstant")
  nbReac = nrow(parts)
  
  reactants = products = list()
  kReac = kReacF = tag = c()
  for (i in 1:nbReac) {
    tag[i] = paste0('R',i)
    reactants[[i]] = gsub(" ","",unlist(strsplit(parts[i,"reactants"],
                                                 split="\\+")))
    products [[i]] = gsub(" ","",unlist(strsplit(parts[i,"products" ],
                                                 split="\\+")))   
    cRate=as.numeric(unlist(strsplit(parts[i,"rateConstant"],split="/")))
    kReac[i] =cRate[1]
    if (length(cRate)>1)
      kReacF[i]=cRate[2]
    else
      kReacF[i]=1.0
  }
  
  species=levels(as.factor(unlist(c(reactants,products))))
  nbSpecies=length(species)
  
  # R, L, D matrices 
  L = R = matrix(0,ncol=nbSpecies,nrow=nbReac)
  for (m in 1:nbReac) {
    for (n in 1:nbSpecies) {
      search=species[n]
      L[m,n] = length(which( search == unlist(reactants[m]) )) # Loss
      R[m,n] = length(which( search == unlist(products[m] ) )) # Prod
    }
  }
  D = R - L # Step change matrix
  
  return(list(nbSpecies=nbSpecies,nbReac=nbReac,species=species,
              D=D,L=L,kReac=kReac,kReacF=kReacF,reactants=reactants,
              products=products, tags = tag))  
}
c0Parse  = function(scheme) {
  # Extract relevant parts
  parts=c()
  for (line in scheme) {
    m = regexec("c0_(.*?)_+(.*?)?\\s*=\\s*(.*)?$",line)
    if(m[[1]][1] != -1)
      parts = rbind(parts,unlist(regmatches(line,m))[2:4])
  }
  parts=data.frame(parts,stringsAsFactors=FALSE)
  
  if(nrow(parts)==0) 
    return(list(c0=NULL,c0F=NULL))
  
  names(parts)=c("spec","exp","conc")
  
  species = unique(parts[,"spec"])
  nSpec   = length(species)
  exps    = unique(parts[,"exp"])
  nExp    = max(as.numeric(exps))
  state = stateF = matrix(NA,nrow=nSpec,ncol=nExp)
  rownames(state)=rownames(stateF)=species
  for (i in 1:nrow(parts)) {
    conc=as.numeric(unlist(strsplit(parts[i,"conc"],split="/")))
    c0 =conc[1]
    if (length(conc)>1)
      c0F=conc[2]
    else
      c0F=1.0
    
    state[parts[i,"spec"],as.numeric(parts[i,"exp"])] = c0 
    stateF[parts[i,"spec"],as.numeric(parts[i,"exp"])] = c0F
  }
  return(list(c0=state,c0F=stateF))  
}
epsParse  = function(scheme) {
  # Extract relevant parts
  parts=c()
  for (line in scheme) {
    m = regexec("eps_(.*?)\\s*=\\s*(.*)?$",line) 
    if(m[[1]][1] != -1)
      parts = rbind(parts,unlist(regmatches(line,m))[2:3])
  }
  parts=data.frame(parts,stringsAsFactors=FALSE)
  names(parts)=c("spec","eps")
  
  spec = unique(parts[,"spec"])
  nSpec = length(spec)
  eps = epsF = rep(NA,nSpec)
  names(eps) = names(epsF) = spec
  for (i in 1:nrow(parts)) {
    conc=as.numeric(unlist(strsplit(parts[i,"eps"],split="/")))
    c0 =conc[1]
    if (length(conc)>1)
      c0F=conc[2]
    else
      c0F=1.0
    
    eps[parts[i,"spec"]]  = c0 
    epsF[parts[i,"spec"]] = c0F
  }
  return(list(eps=eps,epsF=epsF))  
}

