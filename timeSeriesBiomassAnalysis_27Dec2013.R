#data analysis for LO zooplankton production data and biomass data
#PTK 27 Dec 2013

#load zoop data in long lake data folder by loading R scripts from each year; from
#2011:
source(file.choose())

#2012
source(file.choose())

#2013
source(file.choose())

#data should be in object 'zoopData' and year (i.e. zoopData2011)

#order 2013 data
zoopData2013=zoopData2013[order(zoopData2013$Jdate),]


#make a plot for Daphnia biomass between the 3 years - y axis NOT standardized
#make biomass table
biomassData<-data.frame(lakeID=c(zoopData2011$lakeID,as.character(zoopData2012$lake),as.character(zoopData2013$Lake.ID)),dateSample=c(format(as.Date(as.character(zoopData2011$dateSample),'%Y-%m-%d'),'%m/%d/%y'),as.character(zoopData2012$date),as.character(zoopData2013$Sample.date)),taxa=c(zoopData2011$taxa,as.character(zoopData2012$taxa),as.character(zoopData2013$Taxa)),biomass=c(zoopData2011$mass_ug_m2*100,zoopData2012$mass_ugPerM2,zoopData2013$mass_ug_m2))
biomassData$dateSample=format(as.Date(biomassData$dateSample,'%m/%d/%y'),format='%Y-%m-%d')

biomassData$taxa[grep('cyclopoids',biomassData$taxa)]='cyclopoid'

#write biomass data to MAR data folder
setwd('~/Documents/Notre Dame/long lake data/MAR data')
write.csv(biomassData,'biomassTimeSeriesData_1Feb2014.csv')

#biomass data across time for daphnia
plot(biomassData$biomass[biomassData$lakeID=='WL' & biomassData$taxa=='daphnia']~as.Date(biomassData$dateSample[biomassData$lakeID=='WL' & biomassData$taxa=='daphnia'],'%Y-%m-%d'),type='b',xlab='Date',ylab=expression(paste('Daphnia biomass ',mu,'g m'^-2)))
lines(biomassData$biomass[biomassData$lakeID=='EL' & biomassData$taxa=='daphnia']~as.Date(biomassData$dateSample[biomassData$lakeID=='EL' & biomassData$taxa=='daphnia'],'%Y-%m-%d'),type='b',col='red')
legend('topright',col=c('black','red'),legend=c('WL','EL'),pch=c(1,1))

#biomass data across time for holopeiumd
plot(biomassData$biomass[biomassData$lakeID=='EL' & biomassData$taxa=='holopedium']~as.Date(biomassData$dateSample[biomassData$lakeID=='EL' & biomassData$taxa=='holopedium'],'%Y-%m-%d'),type='b',xlab='Date',ylab=expression(paste('Holopedium biomass ',mu,'g m'^-2)),col='red')
lines(biomassData$biomass[biomassData$lakeID=='WL' & biomassData$taxa=='holopedium']~as.Date(biomassData$dateSample[biomassData$lakeID=='WL' & biomassData$taxa=='holopedium'],'%Y-%m-%d'),type='b',col='black')

#biomass data across time for cyclopoids
plot(biomassData$biomass[biomassData$lakeID=='WL' & biomassData$taxa=='cyclopoid']~as.Date(biomassData$dateSample[biomassData$lakeID=='WL' & biomassData$taxa=='cyclopoid'],'%Y-%m-%d'),type='b',xlab='Date',ylab=expression(paste('Daphnia biomass ',mu,'g m'^-2)))
lines(biomassData$biomass[biomassData$lakeID=='EL' & biomassData$taxa=='cyclopoid']~as.Date(biomassData$dateSample[biomassData$lakeID=='EL' & biomassData$taxa=='cyclopoid'],'%Y-%m-%d'),type='b',col='red')

#Add chaoborus to data
#Daphnia
plot(biomassData$biomass[biomassData$lakeID=='WL' & biomassData$taxa=='daphnia']~as.Date(biomassData$dateSample[biomassData$lakeID=='WL' & biomassData$taxa=='daphnia'],'%Y-%m-%d'),type='b',xlab='Date',ylab=expression(paste('Daphnia biomass ',mu,'g m'^-2)))
par(new=T)
plot(chaobs$count[chaobs$lakeID=='WL']~as.Date(chaobs$dateSample[chaobs$lakeID=='WL'],'%m/%d/%Y'),pch=19,yaxt='n',xaxt='n',ylab='',xlab='')


plot(biomassData$biomass[biomassData$lakeID=='EL' & biomassData$taxa=='daphnia']~as.Date(biomassData$dateSample[biomassData$lakeID=='EL' & biomassData$taxa=='daphnia'],'%Y-%m-%d'),type='b',col='red',xlab='Date',ylab=expression(paste('Daphnia biomass ',mu,'g m'^-2)))
par(new=T)
plot(chaobs$count[chaobs$lakeID=='EL']~as.Date(chaobs$dateSample[chaobs$lakeID=='EL'],'%m/%d/%Y'),pch=19,yaxt='n',xaxt='n',ylab='',xlab='',col='red')


#Holopedium
plot(biomassData$biomass[biomassData$lakeID=='WL' & biomassData$taxa=='holopedium']~as.Date(biomassData$dateSample[biomassData$lakeID=='WL' & biomassData$taxa=='holopedium'],'%Y-%m-%d'),type='b',xlab='Date',ylab=expression(paste('Holopedium biomass ',mu,'g m'^-2)))
par(new=T)
plot(chaobs$count[chaobs$lakeID=='WL']~as.Date(chaobs$dateSample[chaobs$lakeID=='WL'],'%m/%d/%Y'),pch=19,yaxt='n',xaxt='n',ylab='',xlab='')


plot(biomassData$biomass[biomassData$lakeID=='EL' & biomassData$taxa=='holopedium']~as.Date(biomassData$dateSample[biomassData$lakeID=='EL' & biomassData$taxa=='holopedium'],'%Y-%m-%d'),type='b',col='red',xlab='Date',ylab=expression(paste('Holopedium biomass ',mu,'g m'^-2)))
par(new=T)
plot(chaobs$count[chaobs$lakeID=='EL']~as.Date(chaobs$dateSample[chaobs$lakeID=='EL'],'%m/%d/%Y'),pch=19,yaxt='n',xaxt='n',ylab='',xlab='',col='red')

#cyclopoids
plot(biomassData$biomass[biomassData$lakeID=='WL' & biomassData$taxa=='cyclopoid']~as.Date(biomassData$dateSample[biomassData$lakeID=='WL' & biomassData$taxa=='cyclopoid'],'%Y-%m-%d'),type='b',xlab='Date',ylab=expression(paste('biomass ',mu,'g m'^-2)))
par(new=T)
plot(chaobs$count[chaobs$lakeID=='WL']~as.Date(chaobs$dateSample[chaobs$lakeID=='WL'],'%m/%d/%Y'),pch=19,yaxt='n',xaxt='n',ylab='',xlab='')


plot(biomassData$biomass[biomassData$lakeID=='EL' & biomassData$taxa=='cyclopoid']~as.Date(biomassData$dateSample[biomassData$lakeID=='EL' & biomassData$taxa=='cyclopoid'],'%Y-%m-%d'),type='b',col='red',xlab='Date',ylab=expression(paste('biomass ',mu,'g m'^-2)))
par(new=T)
plot(chaobs$count[chaobs$lakeID=='EL']~as.Date(chaobs$dateSample[chaobs$lakeID=='EL'],'%m/%d/%Y'),pch=19,yaxt='n',xaxt='n',ylab='',xlab='',col='red')


par(mfrow=c(1,3))
plot(zoopData2011$Jdate[zoopData2011$lakeID=='WL' & zoopData2011$taxa=='daphnia'],zoopData2011$mass_ug_m2[zoopData2011$lakeID=='WL' & zoopData2011$taxa=='daphnia']*100,type='l',xlab='',ylab=expression(paste('biomass ',mu,'g m'^-2)),main='2011')
lines(zoopData2011$Jdate[zoopData2011$lakeID=='EL' & zoopData2011$taxa=='daphnia'],zoopData2011$mass_ug_m2[zoopData2011$lakeID=='EL' & zoopData2011$taxa=='daphnia']*100,col='red')

plot(zoopData2012$Jdate[zoopData2012$lake=='WL' & zoopData2012$taxa=='daphnia'],zoopData2012$mass_ugPerM2[zoopData2012$lake=='WL' & zoopData2012$taxa=='daphnia'],type='l',xlab='DOY',ylab='',main='2012')
lines(zoopData2012$Jdate[zoopData2012$lake=='EL' & zoopData2012$taxa=='daphnia'],zoopData2012$mass_ugPerM2[zoopData2012$lake=='EL' & zoopData2012$taxa=='daphnia'],col='red')
legend('top',legend=c('WL','EL'),col=c('black','red'),lty=c(1,1),ncol=2,bty='n')

plot(zoopData2013$Jdate[order(zoopData2013$Jdate)][zoopData2013$Lake.ID=='WL' & zoopData2013$Taxa=='daphnia'],zoopData2013$mass_ug_m2[order(zoopData2013$Jdate)][zoopData2013$Lake.ID=='WL' & zoopData2013$Taxa=='daphnia'],type='l',xlab='',ylab='',main='2013',ylim=c(0,270000))
lines(zoopData2013$Jdate[order(zoopData2013$Jdate)][zoopData2013$Lake.ID=='EL' & zoopData2013$Taxa=='daphnia'],zoopData2013$mass_ug_m2[order(zoopData2013$Jdate)][zoopData2013$Lake.ID=='EL' & zoopData2013$Taxa=='daphnia'],col='red')




#make a plot for Holopedium biomass between the 3 years - y axis NOT standardized
par(mfrow=c(1,3))
plot(zoopData2011$Jdate[zoopData2011$lakeID=='WL' & zoopData2011$taxa=='holopedium'],zoopData2011$mass_ug_m2[zoopData2011$lakeID=='WL' & zoopData2011$taxa=='holopedium']*100,type='l',xlab='',ylab=expression(paste('biomass ',mu,'g m'^-2)),main='2011',lwd=2,cex=1.2,cex.axis=1.2,cex.lab=1.2)
lines(zoopData2011$Jdate[zoopData2011$lakeID=='EL' & zoopData2011$taxa=='holopedium'],zoopData2011$mass_ug_m2[zoopData2011$lakeID=='EL' & zoopData2011$taxa=='holopedium']*100,col='red',lwd=2)

plot(zoopData2012$Jdate[zoopData2012$lake=='WL' & zoopData2012$taxa=='holopedium'],zoopData2012$mass_ugPerM2[zoopData2012$lake=='WL' & zoopData2012$taxa=='holopedium'],type='l',xlab='DOY',ylab='',main='2012',lwd=2,cex=1.2,cex.axis=1.2,cex.lab=1.2)
lines(zoopData2012$Jdate[zoopData2012$lake=='EL' & zoopData2012$taxa=='holopedium'],zoopData2012$mass_ugPerM2[zoopData2012$lake=='EL' & zoopData2012$taxa=='holopedium'],col='red')
legend('top',legend=c('WL','EL'),col=c('black','red'),lty=c(1,1),ncol=2,bty='n',lwd=2)

plot(zoopData2013$Jdate[zoopData2013$Lake.ID=='WL' & zoopData2013$Taxa=='holopedium'],zoopData2013$mass_ug_m2[zoopData2013$Lake.ID=='WL' & zoopData2013$Taxa=='holopedium'],type='l',xlab='',ylab='',main='2013',lwd=2,cex=1.2,cex.axis=1.2,cex.lab=1.2)
lines(zoopData2013$Jdate[zoopData2013$Lake.ID=='EL' & zoopData2013$Taxa=='holopedium'],zoopData2013$mass_ug_m2[zoopData2013$Lake.ID=='EL' & zoopData2013$Taxa=='holopedium'],col='red',lwd=2)


#make a plot for Cyclopoid biomass between the 3 years - y axis NOT standardized
par(mfrow=c(1,3))
plot(zoopData2011$Jdate[zoopData2011$lakeID=='EL' & zoopData2011$taxa=='cyclopoid'],zoopData2011$mass_ug_m2[zoopData2011$lakeID=='EL' & zoopData2011$taxa=='cyclopoid']*100,type='l',xlab='',ylab=expression(paste('biomass ',mu,'g m'^-2)),main='2011')
lines(zoopData2011$Jdate[zoopData2011$lakeID=='WL' & zoopData2011$taxa=='cyclopoid'],zoopData2011$mass_ug_m2[zoopData2011$lakeID=='WL' & zoopData2011$taxa=='cyclopoid']*100,col='red')

plot(zoopData2012$Jdate[zoopData2012$lake=='WL' & zoopData2012$taxa=='cyclopoid'],zoopData2012$mass_ugPerM2[zoopData2012$lake=='WL' & zoopData2012$taxa=='cyclopoid'],type='l',xlab='DOY',ylab='',main='2012')
lines(zoopData2012$Jdate[zoopData2012$lake=='EL' & zoopData2012$taxa=='cyclopoid'],zoopData2012$mass_ugPerM2[zoopData2012$lake=='EL' & zoopData2012$taxa=='cyclopoid'],col='red')
legend('top',legend=c('WL','EL'),col=c('black','red'),lty=c(1,1),ncol=2,bty='n')

plot(zoopData2013$Jdate[zoopData2013$Lake.ID=='EL' & zoopData2013$Taxa=='cyclopoids'],zoopData2013$mass_ug_m2[zoopData2013$Lake.ID=='EL' & zoopData2013$Taxa=='cyclopoids'],type='l',xlab='',ylab='',main='2013')
lines(zoopData2013$Jdate[zoopData2013$Lake.ID=='WL' & zoopData2013$Taxa=='cyclopoids'],zoopData2013$mass_ug_m2[zoopData2013$Lake.ID=='WL' & zoopData2013$Taxa=='cyclopoids'],col='red')


#calculate % biomass for each taxa for each year
#convert weird dateSample format into normal format
zoopData2011$dateSample<-format(as.Date(zoopData2011$dateSample,'%Y-%m-%d %H:%M:%S'),format='%m/%d/%y')
zoopData2011$uniqueID<-paste(zoopData2011$lakeID,zoopData2011$dateSample,sep='.') #make uniqueIDs to sum biomass by

sumBiomass2011<-tapply(zoopData2011$mass_ug_m2,zoopData2011$uniqueID,sum) #sum all biomass by lake/date/taxa
sumBiomass2011<-data.frame(uniqueID=rownames(sumBiomass2011),biomassSum=sumBiomass2011)
rownames(sumBiomass2011)<-NULL #clean up data

#calculate % of total biomass for each taxa in 2011
percentBiomass=c() #for loop will match data with the summed biomass for that date and then divide each taxa's biomass by that number
for(i in 1:nrow(zoopData2011)){
	rowi<-match(zoopData2011$uniqueID[i],sumBiomass2011$uniqueID)
	percentBiomass[i]<-zoopData2011$mass_ug_m2[i]/sumBiomass2011$biomassSum[rowi]
}
zoopData2011$percentBiomass<-percentBiomass


#2012
zoopData2012$uniqueID<-paste(zoopData2012$lake,zoopData2012$date,sep='.') #make uniqueIDs to sum biomass by

sumBiomass2012<-tapply(zoopData2012$mass_ugPerM2,zoopData2012$uniqueID,sum,na.rm=T) #sum all biomass by lake/date/taxa
sumBiomass2012<-data.frame(uniqueID=rownames(sumBiomass2012),biomassSum=sumBiomass2012)
rownames(sumBiomass2012)<-NULL #clean up data

#calculate % of total biomass for each taxa in 2012
percentBiomass=c() #for loop will match data with the summed biomass for that date and then divide each taxa's biomass by that number
for(i in 1:nrow(zoopData2012)){
	rowi<-match(zoopData2012$uniqueID[i],sumBiomass2012$uniqueID)
	percentBiomass[i]<-zoopData2012$mass_ugPerM2[i]/sumBiomass2012$biomassSum[rowi]
}
zoopData2012$percentBiomass<-percentBiomass


#2013
zoopData2013$uniqueID<-paste(zoopData2013$Lake.ID,zoopData2013$Sample.date,sep='.') #make uniqueIDs to sum biomass by

sumBiomass2013<-tapply(zoopData2013$mass_ug_m2,zoopData2013$uniqueID,sum) #sum all biomass by lake/date/taxa
sumBiomass2013<-data.frame(uniqueID=rownames(sumBiomass2013),biomassSum=sumBiomass2013)
rownames(sumBiomass2013)<-NULL #clean up data

#calculate % of total biomass for each taxa in 2013
percentBiomass=c() #for loop will match data with the summed biomass for that date and then divide each taxa's biomass by that number
for(i in 1:nrow(zoopData2013)){
	rowi<-match(zoopData2013$uniqueID[i],sumBiomass2013$uniqueID)
	percentBiomass[i]<-zoopData2013$mass_ug_m2[i]/sumBiomass2013$biomassSum[rowi]
}
zoopData2013$percentBiomass<-percentBiomass

percentBiomassData<-data.frame(lakeID=c(zoopData2011$lakeID,as.character(zoopData2012$lake),as.character(zoopData2013$Lake.ID)),dateSample=c(zoopData2011$dateSample,as.character(zoopData2012$date),as.character(zoopData2013$Sample.date)),taxa=c(zoopData2011$taxa,as.character(zoopData2012$taxa),as.character(zoopData2013$Taxa)),percentBiomass=c(zoopData2011$percentBiomass,zoopData2012$percentBiomass,zoopData2013$percentBiomass))

percentBiomassData$dateSample=format(as.Date(percentBiomassData$dateSample,'%m/%d/%y'),format='%Y-%m-%d')

#percent biomass differences among taxa
#Daphnia
plot(percentBiomassData$percentBiomass[percentBiomassData$lakeID=='EL' & percentBiomassData$taxa=='daphnia']~as.Date(percentBiomassData$dateSample[percentBiomassData$lakeID=='EL' & percentBiomassData$taxa=='daphnia'],'%Y-%m-%d'),type='b',ylim=c(0,1),xlab='Date',ylab='% biomass Daphnia',cex=1.2,cex.lab=1.5,cex.axis=1.2)
lines(percentBiomassData$percentBiomass[percentBiomassData$lakeID=='WL' & percentBiomassData$taxa=='daphnia']~as.Date(percentBiomassData$dateSample[percentBiomassData$lakeID=='WL' & percentBiomassData$taxa=='daphnia'],'%Y-%m-%d'),type='b',col='red',pch=19)
legend('topright',pch=c(1,19),col=c('black','red'),legend=c('EL','WL'))

#Holopedium
plot(percentBiomassData$percentBiomass[percentBiomassData$lakeID=='EL' & percentBiomassData$taxa=='holopedium']~as.Date(percentBiomassData$dateSample[percentBiomassData$lakeID=='EL' & percentBiomassData$taxa=='holopedium'],'%Y-%m-%d'),type='b',ylim=c(0,1),xlab='Date',ylab='% biomass Holopedium',cex=1.2,cex.lab=1.5,cex.axis=1.2)
lines(percentBiomassData$percentBiomass[percentBiomassData$lakeID=='WL' & percentBiomassData$taxa=='holopedium']~as.Date(percentBiomassData$dateSample[percentBiomassData$lakeID=='WL' & percentBiomassData$taxa=='holopedium'],'%Y-%m-%d'),type='b',col='red',pch=19)
legend('topright',pch=c(1,19),col=c('black','red'),legend=c('EL','WL'))

#fix cyclopoid data
percentBiomassData$taxa[grep('cyclopoids',percentBiomassData$taxa)]='cyclopoid'

#Cyclopoid
plot(percentBiomassData$percentBiomass[percentBiomassData$lakeID=='EL' & percentBiomassData$taxa=='cyclopoid']~as.Date(percentBiomassData$dateSample[percentBiomassData$lakeID=='EL' & percentBiomassData$taxa=='cyclopoid'],'%Y-%m-%d'),type='b',ylim=c(0,1),xlab='Date',ylab='% biomass Cyclopoid',cex=1.2,cex.lab=1.5,cex.axis=1.2)
lines(percentBiomassData$percentBiomass[percentBiomassData$lakeID=='WL' & percentBiomassData$taxa=='cyclopoid']~as.Date(percentBiomassData$dateSample[percentBiomassData$lakeID=='WL' & percentBiomassData$taxa=='cyclopoid'],'%Y-%m-%d'),type='b',col='red',pch=19)
legend('topright',pch=c(1,19),col=c('black','red'),legend=c('EL','WL'))
