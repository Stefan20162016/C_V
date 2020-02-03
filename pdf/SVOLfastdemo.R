rm(list=ls())
densigamma=function(x,shape,scale){ log(scale^shape / gamma(shape)*x^(-1-shape)*exp(-scale/x)) }
densigamma2=function(x,shape,scale){   (scale^shape / gamma(shape)*x^(-1-shape)*exp(-scale/x)) }

set.seed(45678)
n     = 300
alpha=-0.0084
beta  =  0.98
tau=0.2
ptr=c(alpha,beta,tau^2,0)
y     = rep(0,n)
x     = rep(0,n)
x[1] = rnorm(1,alpha,tau/sqrt(1-beta^2) )
y[1]  = rnorm(1,0,exp(x[1]/2))
for (t in 2:n){
  x[t] = rnorm(1,alpha+beta*(x[t-1]),tau)
  y[t]= rnorm(1,0,1)*exp(x[t]/2)
}

N=300
sisr=function(mu,phi,sigma){
x=rnorm(N,0,2.2);ytot=0;meds=NULL
for(t in 1:n){
 x1 = mu + phi*(x) + rnorm(N,0,sigma)
 w  = dnorm(y[t],0,exp(x/2))
 x  = sample(x1,size=N,replace=T,prob=w)
 ytot=ytot+log(mean(w))
 }
ret=list(1,ytot)
return(ret)
}

propose=function(a,b,c){
 # check after proposal if PHI is element of (-1,1)
 v1=0.02; v2=0.02; v3=0.11
#c(rnorm(1,a,v1), rnorm(1,b,v2), exp(rnorm(1,log(c),v3)) )
c(rnorm(1,a,v1), rnorm(1,b,v2), abs(rnorm(1,c,v3)) )
}

dprior=function(a,b,c){
#c( log(dnorm(a,0,tau^2/30)*dnorm(b,0.95,0.1*tau^2)*densigamma(c^2,8,7*(tau^2+0.01))  ))
# c( log(dnorm(a,0,c^2/30)*dnorm(b,0.95,0.1*c^2)*densigamma(c^2,8,7*(tau^2+0.01))  ))
# c( log(dnorm(a,0,c/10)*dnorm(b,0.95,0.1*c)*densigamma(c^2,8,7*(tau^2+0.01))  ))
# c( (dnorm(a,0,1,log=T)) +
#	(dnorm(b,0.95,c^2,log=T)) +
#	(densigamma(c^2,0.01,0.01))   )

c( (dnorm(a,0,1,log=T)) +
	(densigamma(c^2,0.01,0.01))   )	# "loose" prior
#c( (dnorm(a,0,1,log=T)) +
#	(densigamma(c^2,8,0.28))   )	# mean=0.04 better for tau=0.2
#c( (dnorm(a,0,1,log=T)) +
#	(densigamma(c^2,11,3.6))   )	# mean=0.36 better for tau=0.6
}

muold=alpha
phiold=beta
sigmaold=tau
out=sisr(muold,phiold,sigmaold)
loglikold=out[[2]]

niter=30000
tracepars=NULL
nsteps=1
k=1
# for iterations in PMMH
for ( i in 1:niter ){
if(i == k*100){
 cat(k*100,Sys.time(),nsteps,"\n");k=k+1
}
parsnew=NULL
parsnew=propose(muold,phiold,sigmaold)
 
 if ( (parsnew[[2]] < -1) || (parsnew[[2]] > 1) ){
 	phinew=phiold
 }
 else { phinew=parsnew[[2]];  }

 munew=parsnew[[1]]; sigmanew=parsnew[[3]]
 
 out=sisr(munew,phinew,sigmanew)
 logliknew=out[[2]]
 num=logliknew + dprior(munew,phinew,sigmanew) 
 den=loglikold + dprior(muold,phiold,sigmaold) 

if(num == -Inf){ a= -1e308; cat("num -INF!!!\n") }
else {a=min(0,num-den) }
 if ( log(runif(1)) < a ){
  #accepted
cat("using: ", munew, phinew, sigmanew,"\n")
  nsteps=nsteps+1
  tracepars=rbind(tracepars,c(munew,phinew,sigmanew,logliknew))
  muold=munew; phiold=phinew; sigmaold=sigmanew
  loglikold=logliknew
 }else{
 tracepars=rbind(tracepars,c(muold,phiold,sigmaold,loglikold))
 }
}
str=c(paste("partsN:",N,"iterat:",niter),
paste("accept:",nsteps/niter,"time:",n),
paste("paras:",ptr[1],ptr[2],ptr[3]))

tracepars[,3]=tracepars[,3]^2


pdf(file="svol_N300_04_bm.pdf",width=10,height=7)

par(mfrow=c(3,4))
names=c("mu","phi","sigma","estlogLike")
index=c(1:niter)
for (i in 1:4){
  plot(index,tracepars[index,i],type="l",xlab="iteration",ylab="",main=names[i])
  abline(h=ptr[i],col=2,lwd=4)
  abline(h=median(tracepars[,i]),col="blue",lwd=2)
}

for (i in 1:4)
  acf(tracepars[,i],main="",lag.max=2000)

for (i in 1:4){
  hist(tracepars[,i],,xlab="",ylab="",main="",nclass=100,freq=F)
  if(i == 3){
  	curve(densigamma2(x,0.01,0.01), col = 2, lty = 2, lwd = 2, add = TRUE) }
  abline(v=(ptr[i]),col=2,lwd=4)
  abline(v=median(tracepars[,i]),col="blue",lwd=2)
  mtext(str[i],side=1,line=2,col="blue",cex=0.9)
}
dev.off()

cat("N: ", N, " niter: ", niter, " accepted: ", nsteps/niter, " time: ",n, "pars: ", ptr[1:3],"\n")




