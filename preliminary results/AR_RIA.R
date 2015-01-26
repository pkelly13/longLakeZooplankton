#Script that runs autoregressive intervention analysis using Powers et al. (2013) methods
#Patrick Kelly 13 January 2014

#load zooplankton data
setwd('~/Documents/Notre Dame/long lake data/MAR data')

zoops<-read.csv('zoopDataMatrix2011-2014.csv')

#need to match reference and treatment basin taxa and date
#two data frames of East and West, then match East with west and subtract in the form Y=Co-CI, were Co is tretment, CI is reference

zoops.el<-zoops[zoops$lakeID=='EL',]
zoops.wl<-zoops[zoops$lakeID=='WL',]

#add daphnia, cyclopoid, and holopedium data frames from WL to EL
wlZoops<-c()
for(i in 1:nrow(zoops.el)){
	rowi<-match(zoops.el$dateSample[i],zoops.wl$dateSample)
	x<-zoops.wl[rowi,c(6,7,8)]
	wlZoops<-rbind(wlZoops,x)
}
colnames(wlZoops)<-c('daph.wl','cyc.wl','holo.wl')

tot.zoops<-cbind(zoops.el,wlZoops)

#get Y for each taxa by subtracting reference from treatment
daph.Y<-tot.zoops$daphnia.gm2-tot.zoops$daph.wl
cyclo.Y<-tot.zoops$cyclopoid.gm2-tot.zoops$cyc.wl
holo.Y<-tot.zoops$holhopedium.gm2-tot.zoops$holo.wl

#add date to get final Y data frame
Y<-data.frame(dateSample=tot.zoops$dateSample,daphnia=daph.Y,cyclopoid=cyclo.Y,holopedium=holo.Y)

#order Y by date
Y<-Y[order(Y$dateSample),]

#add year to Y data frame
Y$year<-format(as.Date(Y$dateSample,'%Y-%m-%d'),'%Y')

#make Yt and Yt+1
Yt<-Y[1:nrow(Y)-1,]
Ytplus1<-Y[2:nrow(Y),]

#remove NAs -> need to do all of this for each taxa
Yt.daphnia<-Yt[!is.na(Yt$daphnia),c(1,2,5)]
Yt.cyclopoid<-Yt[!is.na(Yt$cyclopoid),c(1,3,5)]
Yt.holopedium<-Yt[!is.na(Yt$holopedium),c(1,4,5)]

Ytplus1.daphnia<-Ytplus1[!is.na(Ytplus1$daphnia),c(1,2,5)]
Ytplus1.cyclopoid<-Ytplus1[!is.na(Ytplus1$cyclopoid),c(1,3,5)]
Ytplus1.holopedium<-Ytplus1[!is.na(Ytplus1$holopedium),c(1,4,5)]

Mi.daphnia<-rep(0,nrow(Yt.daphnia))
Mi.daphnia[Yt.daphnia$year==2013 | Yt.daphnia$year==2014]=1
Mi.cyclopoid<-rep(0,nrow(Yt.cyclopoid))
Mi.cyclopoid[Yt.cyclopoid$year==2013 | Yt.cyclopoid$year==2014]=1
Mi.holopedium<-rep(0,nrow(Yt.holopedium))
Mi.holopedium[Yt.holopedium$year==2013|Yt.holopedium$year==2014]=1

#remove transition year
Yt.year<-Yt.daphnia$year
Ytplus1.year<-Ytplus1.daphnia$year
Yt.daphnia<-as.matrix(Yt.daphnia[Yt.year==Ytplus1.year,2])
Ytplus1.daphnia<-as.matrix(Ytplus1.daphnia[Yt.year==Ytplus1.year,2])
Mi.daphnia<-Mi.daphnia[Yt.year==Ytplus1.year]

Yt.year<-Yt.cyclopoid$year
Ytplus1.year<-Ytplus1.cyclopoid$year
Yt.cyclopoid<-as.matrix(Yt.cyclopoid[Yt.year==Ytplus1.year,2])
Ytplus1.cyclopoid<-as.matrix(Ytplus1.cyclopoid[Yt.year==Ytplus1.year,2])
Mi.cyclopoid<-Mi.cyclopoid[Yt.year==Ytplus1.year]

Yt.year<-Yt.holopedium$year
Ytplus1.year<-Ytplus1.holopedium$year
Yt.holopedium<-as.matrix(Yt.holopedium[Yt.year==Ytplus1.year,2])
Ytplus1.holopedium<-as.matrix(Ytplus1.holopedium[Yt.year==Ytplus1.year,2])
Mi.holopedium<-Mi.holopedium[Yt.year==Ytplus1.year]


#Start with simple model for each taxa ->use only the intercept
#make vector of 1s for intercept
B0<-rep(1, nrow(Yt.daphnia))

#Try model with just the intercept and the autrogressive parameter
Z.simple.daphnia<-cbind(B0,Yt.daphnia)

#estimate parameters using CLS (copied this from MAR)
D.simple.daphnia<-solve(t(Z.simple.daphnia)%*%Z.simple.daphnia)%*%t(Z.simple.daphnia)%*%Ytplus1.daphnia

#calculate predicted
predict.simple.daphnia<-Z.simple.daphnia%*%D.simple.daphnia

Q<-nrow(Yt.daphnia) #length of time series
P<-1 #number of species
R<-0 #number of covariates

#AIC for Daphnia
E<-predict.simple.daphnia-Ytplus1.daphnia
sigma<-t(E)%*%E/Q
lnlike<--Q*(P/2)*log(2*pi)-(Q/2)*log(det(sigma))-Q*P/2

AIC.daphnia<-(2*2)-(2*lnlike)

#cyclopoid
B0<-rep(1, nrow(Yt.cyclopoid))

#Try model with just the intercept and the autrogressive parameter
Z.simple.cyclopoid<-cbind(B0,Yt.cyclopoid)

#estimate parameters using CLS (copied this from MAR)
D.simple.cyclopoid<-solve(t(Z.simple.cyclopoid)%*%Z.simple.cyclopoid)%*%t(Z.simple.cyclopoid)%*%Ytplus1.cyclopoid

#calculate predicted
predict.simple.cyclopoid<-Z.simple.cyclopoid%*%D.simple.cyclopoid

Q<-nrow(Yt.cyclopoid) #length of time series
P<-1 #number of species
R<-0 #number of covariates

#AIC for Cyclopoid
E<-predict.simple.cyclopoid-Ytplus1.cyclopoid
sigma<-t(E)%*%E/Q
lnlike<--Q*(P/2)*log(2*pi)-(Q/2)*log(det(sigma))-Q*P/2

AIC.cyclopoid<-(2*2)-(2*lnlike)

#holopedium
B0<-rep(1, nrow(Yt.holopedium))

#Try model with just the intercept and the autrogressive parameter
Z.simple.holopedium<-cbind(B0,Yt.holopedium)

#estimate parameters using CLS (copied this from MAR)
D.simple.holopedium<-solve(t(Z.simple.holopedium)%*%Z.simple.holopedium)%*%t(Z.simple.holopedium)%*%Ytplus1.holopedium

#calculate predicted
predict.simple.holopedium<-Z.simple.holopedium%*%D.simple.holopedium

Q<-nrow(Yt.holopedium) #length of time series
P<-1 #number of species
R<-0 #number of covariates

#AIC for Holopedium
E<-predict.simple.holopedium-Ytplus1.holopedium
sigma<-t(E)%*%E/Q
lnlike<--Q*(P/2)*log(2*pi)-(Q/2)*log(det(sigma))-Q*P/2

AIC.holopedium<-(2*2)-(2*lnlike)

#Now try model with dummy variable for manipulation
B0<-rep(1,nrow(Yt.daphnia))

#model with dummy variables
Z.mi.daphnia<-cbind(B0,Yt.daphnia,Mi.daphnia)

D.mi.daphnia<-solve(t(Z.mi.daphnia)%*%Z.mi.daphnia)%*%t(Z.mi.daphnia)%*%Ytplus1.daphnia

#calculate predicted
predict.mi.daphnia<-Z.mi.daphnia%*%D.mi.daphnia

Q<-nrow(Yt.daphnia) #length of time series
P<-1 #number of species
R<-1 #number of covariates

#AIC for Daphnia
E<-predict.mi.daphnia-Ytplus1.daphnia
sigma<-t(E)%*%E/Q
lnlike<--Q*(P/2)*log(2*pi)-(Q/2)*log(det(sigma))-Q*P/2

AIC.mi.daphnia<-(2*3)-(2*lnlike)

#Cyclopoid
B0<-rep(1,nrow(Yt.cyclopoid))

#model with dummy variables
Z.mi.cyclopoid<-cbind(B0,Yt.cyclopoid,Mi.cyclopoid)

D.mi.cyclopoid<-solve(t(Z.mi.cyclopoid)%*%Z.mi.cyclopoid)%*%t(Z.mi.cyclopoid)%*%Ytplus1.cyclopoid

#calculate predicted
predict.mi.cyclopoid<-Z.mi.cyclopoid%*%D.mi.cyclopoid

Q<-nrow(Yt.cyclopoid) #length of time series
P<-1 #number of species
R<-0 #number of covariates

#AIC for Cyclopoid
E<-predict.mi.cyclopoid-Ytplus1.cyclopoid
sigma<-t(E)%*%E/Q
lnlike<--Q*(P/2)*log(2*pi)-(Q/2)*log(det(sigma))-Q*P/2

AIC.mi.cyclopoid<-(2*3)-(2*lnlike)

#Holopedium
B0<-rep(1,nrow(Yt.holopedium))

#model with dummy variables
Z.mi.holopedium<-cbind(B0,Yt.holopedium,Mi.holopedium)

D.mi.holopedium<-solve(t(Z.mi.holopedium)%*%Z.mi.holopedium)%*%t(Z.mi.holopedium)%*%Ytplus1.holopedium

#calculate predicted
predict.mi.holopedium<-Z.mi.holopedium%*%D.mi.holopedium

Q<-nrow(Yt.holopedium) #length of time series
P<-1 #number of species
R<-0 #number of covariates

#AIC for Cyclopoid
E<-predict.mi.holopedium-Ytplus1.holopedium
sigma<-t(E)%*%E/Q
lnlike<--Q*(P/2)*log(2*pi)-(Q/2)*log(det(sigma))-Q*P/2

AIC.mi.holopedium<-(2*3)-(2*lnlike)