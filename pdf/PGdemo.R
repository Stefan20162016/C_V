rm(list=ls(all=TRUE))

g2 = function(arg){c(0.5*arg[1],25*(arg[1]/(1+arg[1]^2)),8*(cos(1.2*arg[2])))}
g = function(arg){c(arg[1],arg[1]/(1+arg[1]^2),cos(1.2*arg[2]))}

# Simulating the data
set.seed(6665464)
n         = 200
sig2      = 1
tau2      = 10
sig=sqrt(sig2);tau=sqrt(tau2);theta=c(0.5,25,8);y=rep(0,n);x=rep(0,n);x0=0.1
Z=g(c(x0,0));x[1]=rnorm(1,sum(Z*theta),tau);y[1]=rnorm(1,x[1]^2/20,sig)
for (t in 2:n){
  Z=g(c(x[t-1],t-1))
  x[t]=rnorm(1,sum(Z*theta),tau)
  y[t]=rnorm(1,x[t]^2/20,sig)
}

set.seed(55145)
#set.seed(85145)
N=200
sisr=function (tau,sig){
tau=sqrt(tau);sig=sqrt(sig)
x=rnorm(N,x[1],tau);ytot=0
INDs=NULL; xs=NULL
for (t in 1:n){
  X  = cbind(x,x/(1+x^2),cos(1.2*(t-1)))
  mx = crossprod(t(X),theta)
  x1 = rnorm(N,mx,tau)
  w  = dnorm(y[t],x1^2/20,sig)
  ytot=ytot+log(mean(w))
  xIND=sample(1:N,size=N,replace=T,prob=w)
  x=x1[xIND]
  INDs=rbind(INDs,xIND)
  xs=rbind(xs,x1)
 }
 ret=list(xs,ytot,1,INDs,w)
 return(ret)
}

sisrCOND=function(tau2,sigma2,XVECT){
tau=sqrt(tau2); sig=sqrt(sig2)
x=rnorm(N,0,1.1); ytot=0
INDs=NULL; xs=NULL
for(t in 1:n){
 X  = cbind(x,x/(1+x^2),cos(1.2*(t-1)))
 mx = crossprod(t(X),theta)
 x1 = rnorm(N,mx,tau)
 x1[N]= XVECT[t]
 w  = dnorm(y[t],x1^2/20,sig)
 ytot=ytot+log(mean(w))
 xIND  = sample(1:N,size=N-1,replace=T,prob=w)
 xIND[N]=N
 x=x1[xIND]
 INDs=rbind(INDs,xIND)
 xs=rbind(xs,x1)
 }
ret=list(xs,ytot,1,INDs,w)
return(ret)
}

wr=sisr(5,5)



taunew=10
sigmanew=1

# CALL SMC_conditional on "good" starting proposal
out=sisr(taunew,sigmanew)
xvector=NULL
# select "start-index" according to weights at last stage
index=sample(1:N,size=1,replace=T,prob=out[[5]])
for(t in n:2){
	xs_current=out[[1]][t,]
	INDS_current=out[[4]][(t-1),]
	xvector=rbind(xvector,xs_current[index])
	index=INDS_current[index]
}
xvector=rbind(xvector,out[[1]][1,index])
XV=rev(xvector)

tracepars=NULL; nsteps=0
# START LOOP

k=1
for (i in 1:10000){
if(i == k*100){
 cat(k*100,Sys.time(),nsteps,"\n");k=k+1
}
# now use XV for conditional SMC
xx=XV
# sample tau and sigma conditioned on PATH xx
# xx <- NEW PATH
W     = cbind(c(x0,xx[1:(n-1)]),0:(n-1))
Z     = t(apply(W,1,g2))
xxx=(Z)%*%c(1,1,1)
s=sum((xx-xxx)^2)
#cat(1/rgamma(1,25,0.5*s),"\n")
n0=6
tau02=20/3
sig02=2/3
par1= (n + n0)/2
par2= (n0*tau02 + s)/2
taunew=1/rgamma(1,par1,par2)
par2_1= (n0+n)/2
par2_2= (4 + sum( (y-xx^2/20)^2  ))/2
sigmanew=1/rgamma(1,par2_1,par2_2)
cat (i,nsteps," new tau^2: ",taunew," new sigma^2: ",sigmanew,"\n")
tracepars=rbind(tracepars,c(taunew,sigmanew))

out=sisrCOND(taunew,sigmanew,XV)

##LOOP## loop over here, to get "best" X-PATH out of loop-count steps
# use one PIMH step, get new X-path and lineage or NOT
outnew=sisrCOND(taunew,sigmanew,XV)
if(log(runif(1)) < min(0,(outnew[[2]]-out[[2]])) ){
 nsteps=nsteps+1
 out=outnew
}
##LOOP##

xvector=NULL
 index=sample(1:N,size=1,replace=T,prob=out[[5]])
  for(t in n:2){
	xs_current=out[[1]][t,]
	INDS_current=out[[4]][(t-1),]
	xvector=rbind(xvector,xs_current[index])
	index=INDS_current[index]
 }
xvector=rbind(xvector,out[[1]][1,index])
XV=rev(xvector)


	if( i > 4 && i < 7){
		cat("in if\n")
		xvector=NULL
		index=sample(1:N,size=1,replace=T,prob=wr[[5]])
		for(t in n:2){
			xs_current=wr[[1]][t,]
			INDS_current=wr[[4]][(t-1),]
			xvector=rbind(xvector,xs_current[index])
			index=INDS_current[index]
		}
		xvector=rbind(xvector,wr[[1]][1,index])
		XV=rev(xvector)
	}

####
#if( i > 666635 && i < 40){
#		cat("in if2222\n")
#		wr2=sisr(taunew,1)
#		xvector=NULL
#		index=sample(1:N,size=1,replace=T,prob=wr2[[5]])
#		for(t in n:2){
#			xs_current=wr2[[1]][t,]
#			INDS_current=wr2[[4]][(t-1),]
#			xvector=rbind(xvector,xs_current[index])
#			index=INDS_current[index]
#		}
#		xvector=rbind(xvector,wr2[[1]][1,index])
#		XV=rev(xvector)
#	}
####

}



summary(tracepars[,1])
summary(tracepars[,2])

par(mfrow=c(2,2))
hist(tracepars[,1],nclass=400)
abline(v=tau2,col="red",lwd=4)
abline(v=median(tracepars[,1]),col="blue",lwd=3)
hist(tracepars[,2],nclass=400)
abline(v=sig2,col="red",lwd=4)
abline(v=median(tracepars[,2]),col="blue",lwd=3)
plot(1:length(tracepars[,1]), tracepars[,1],lwd=1,type="l")
plot(1:length(tracepars[,1]), tracepars[,2],lwd=1,type="l")

#plot(sqrt(tracepars[,1]),sqrt(tracepars[,2]),type="p",pch=".",cex=2,xlim=range(c(2.5,3.5)),ylim=range(c(0.8,1.2)))

cat("is n350  N400   \n")


