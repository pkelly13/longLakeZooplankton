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
Mi.daphnia.2<-rep(0,nrow(Yt.daphnia))
Mi.daphnia.2[Yt.daphnia$year==2014]=1
Mi.daphnia.2<-Mi.daphnia.2+Mi.daphnia

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
Mi.daphnia.2<-Mi.daphnia.2[Yt.year==Ytplus1.year]

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

#run glm's to get autoregressive RIA
#Daphnia
daphnia.intercept<-glm(Ytplus1.daphnia~1)
daphnia.auto<-glm(Ytplus1.daphnia~Yt.daphnia)
daphnia.treatment<-glm(Ytplus1.daphnia~Yt.daphnia+Mi.daphnia)