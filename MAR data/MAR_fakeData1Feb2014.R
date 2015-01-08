#MAR test

#Lotka-Volterra competition model

LV<-function(N,alpha,rs,K){ #function for LV competition model
	N1t1<-N[1]+rs[1]*N[1]*(1-alpha[1,1]*N[1]-alpha[1,2]*N[2])
	N2t2<-N[2]+rs[2]*N[2]*(1-alpha[2,1]*N[1]-alpha[2,2]*N[2])
	c(N1t1,N2t2)
}
t=100 #use 100 time steps
N<-matrix(NA,nrow=t+1,ncol=2) #empty matrix to fill
N[1,]<-c(10,10) #start at an N of 10 for both pops
alphas<-matrix(c(0.01,0.006,0.008,0.02),ncol=2,byrow=T) #find alphas that work
rs=c(0.8,0.5) #growth rates

for(i in 1:t){ #for loop to create the data
	N[i+1,]<-LV(N[i,],alphas,rs,K)
}

Ns<-matrix(NA,nrow=nrow(N),ncol=2) #add variation to the data
for(i in 1:nrow(N)){
	Ns[i,]<-c(rnorm(1,mean=N[i,1],sd=1.5),rnorm(1,mean=N[i,2],sd=1.9))
}

plot(1:nrow(Ns),log(Ns[,1]),type='l',lwd=2)
lines(1:nrow(Ns),log(Ns[,2]),type='l',lwd=2,lty=2)


#linear model - xt = B(xt-1)+a+wt

paramEst<-function(p,t,init.N,obs){
	print(p)
	sigma=exp(p[length(p)])
	
	yhat=matrix(NA,nrow=t+1,ncol=2,byrow=T)
	yhat[1,]=init.N
	a11=p[1]
	a12=p[2]
	a21=p[3]
	a22=p[4]
	r1=p[5]
	r2=p[6]
	w=c(p[7],p[8])
	
	for(i in 1:t){
		yhat[i+1,]=timestep(y=yhat[i,],a11,a12,a21,a22,r1,r2,w)
	}
	
	nll=-sum(dnorm(obs,yhat,sigma,log=TRUE))
	return(nll)
}

guess=c(1,1,1,1,1,1,1,1)
obs=log(Ns)
t=nrow(obs)-1

fit=optim(par=guess,paramEst,t=t,init.N=obs[1,],obs=obs,control=list(maxit=7000))


#CLS paramenter estimation
X=log(Ns[1:100,])
Y=log(Ns[2:101,])

one=rep(1,100)

Z<-cbind(one,X)

D<-solve(t(Z)%*%Z)%*%t(Z)%*%Y
