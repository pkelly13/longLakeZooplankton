#Look at average size of each taxa throughout the experiment - can look at absolute average size and how it changes through the experiment, but also look at the ratio of average weekly sizes
#Patrick Kelly 30 January 2015

#load zooplankton data
setwd('~/Documents/Notre Dame/long lake data/FINAL_data')

zoops<-read.csv('zoopData2011_2014.csv')

#use only EL and WL data
zoops<-zoops[zoops$lakeID=='EL'|zoops$lakeID=='WL',]

#add year to data frame
zoops$year<-format(as.Date(zoops$dateSample,'%m/%d/%y'),'%Y')

#matchup by taxa and look at ratios of east long:west long biomass
#make uniqueID for taxa and date
zoops$uniqueID<-paste(zoops$taxa,zoops$dateSample,sep='.')

#make data frame of east long and west long
el.zoops<-zoops[zoops$lakeID=='EL',]
wl.zoops<-zoops[zoops$lakeID=='WL',]

#match east and west long to make ratio of biomass
wl.meanMass_ug<-c()
for(i in 1:nrow(el.zoops)){
	rowi<-match(el.zoops$uniqueID[i],wl.zoops$uniqueID)
	wl.meanMass_ug[i]<-wl.zoops$meanMass_ug[rowi]
}
el.zoops$wl.meanMass_ug<-wl.meanMass_ug
#calculate ratio
el.zoops$ratio<-el.zoops$meanMass_ug/el.zoops$wl.meanMass_ug

#add year to data frame
el.zoops$year<-format(as.Date(el.zoops$dateSample,'%m/%d/%y'),'%Y')

#get mean mass of all taxa
daphnia.el.mean<-tapply(el.zoops$meanMass_ug[el.zoops$taxa=='daphnia'],el.zoops$year[el.zoops$taxa=='daphnia'],mean,na.rm=T)
daphnia.wl.mean<-tapply(el.zoops$wl.meanMass_ug[el.zoops$taxa=='daphnia'],el.zoops$year[el.zoops$taxa=='daphnia'],mean,na.rm=T)
daphnia.ratio.mean<-daphnia.el.mean/daphnia.wl.mean

cyclopoid.el.mean<-tapply(el.zoops$meanMass_ug[el.zoops$taxa=='cyclopoid'],el.zoops$year[el.zoops$taxa=='cyclopoid'],mean,na.rm=T)
cyclopoid.wl.mean<-tapply(el.zoops$wl.meanMass_ug[el.zoops$taxa=='cyclopoid'],el.zoops$year[el.zoops$taxa=='cyclopoid'],mean,na.rm=T)
cyclopoid.ratio.mean<-cyclopoid.el.mean/cyclopoid.wl.mean

holo.el.mean<-tapply(el.zoops$meanMass_ug[el.zoops$taxa=='holopedium'],el.zoops$year[el.zoops$taxa=='holopedium'],mean,na.rm=T)
holo.wl.mean<-tapply(el.zoops$wl.meanMass_ug[el.zoops$taxa=='holopedium'],el.zoops$year[el.zoops$taxa=='holopedium'],mean,na.rm=T)
holo.ratio.mean<-holo.el.mean/holo.wl.mean

#boxplot of mean mass ratio by year
boxplot(el.zoops$ratio~el.zoops$year,xlab='Year',ylab='EL:WL mean biomass',boxwex=0.5,cex.lab=1.2,cex.axis=1.2,outline=F)
abline(h=1,lty=2,col='grey',lwd=2)
points(c(1,2,3,4),daphnia.ratio.mean,pch=21,cex=2.5,col='black',bg='grey')
points(c(1,2,3,4),cyclopoid.ratio.mean,pch=22,cex=2.5,col='black',bg='grey')
points(c(1,2,3,4),holo.ratio.mean,pch=23,cex=2.5,col='black',bg='grey')
legend('topleft',legend=c('Daphnia','cyclopoid','Holopedium'),pch=c(21,22,23))