require("VGAM",quietly=TRUE)      #erf

expirf = function (x,tau,mu,del) {
  0.5 * exp(-tau*x) * exp( tau*(mu+0.5*tau*del^2) ) *
    ( 1 + VGAM::erf( (x -(mu+tau*del^2)) / (sqrt(2)*del) ) )
}

time = seq(-1 , 10, length.out = 200)
wavl = seq(300,600, length.out = 200)

Ntrue = 3

Strue = matrix(NA,ncol=3,nrow=length(wavl)) 
for (i in 1:Ntrue) {
  Strue[,i] = i^0.5*dnorm(wavl,mean=300+80*i,sd=100)
}

k1 = 1.0; k2 = 0.5
Ctrue = matrix(NA,ncol=3,nrow=length(time)) 

# Introduce IRF
kInf=min(c(k1,k2))/100
c1=expirf(time,k1  ,0.,0.1)
c2=expirf(time,k2  ,0.,0.1)
c3=expirf(time,kInf,0.,0.1)

Ctrue[,1] = c1
Ctrue[,2] = (c2-c1)/(k1-k2)
Ctrue[,3] = c3 - Ctrue[,1] - Ctrue[,2]

matplot(time,cbind(c1,c2,c3),type='l',lty=1)
matplot(time,Ctrue,type='l',lty=1)

Dtrue = Ctrue %*% t(Strue)
sig = 0.03*max(Dtrue)
Utrue = matrix(rnorm(prod(dim(Dtrue)),mean=0,sd=sig),
               ncol=ncol(Dtrue),nrow=nrow(Dtrue))

data = Dtrue + Utrue

write.table(t(data),file='data_ABC_IRF.csv',
            col.names = time, row.names=wavl, 
            quote=FALSE, sep=',')
