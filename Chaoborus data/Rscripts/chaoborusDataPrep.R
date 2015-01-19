#Chaoborus data - find annual average Chaoborus abundance and biomass, as well as time series of Chaoborus abundance and biomass throughout the entire Long Lake experiment

#Load data
setwd('~/Documents/Notre Dame/Long Lake Data/Chaoborus data')

#load 2011 data
count.2011<-read.csv('2011ChaoborusCounts.csv')
length.2011<-read.csv('2011ChaoborusLengths.csv')

#load 2012 data
count.2012<-read.csv('2012ChaoborusCounts.csv')
length.2012<-read.csv('2012ChaoborusLengths.csv')

#load 2013 data
count.2013<-read.csv('2013ChaoborusCounts.csv')
length.2013<-read.csv('2013ChaoborusLengths.csv')

#load 2014 data
count.2014<-read.csv('2014ChaoborusCounts.csv')
length.2014<-read.csv('2014ChaoborusLengths.csv')

#get data into the same format and into one big datasheet with abundance in ind m-2 and biomass
#2011 data
#make lakeID column
lakeID<-sub('East','EL',length.2011$Lake.Side)
lakeID<-sub('West','WL',lakeID)
length.2011$lakeID<-lakeID

#make unique ID for count.2011 that is lake-date
count.2011$uniqueID<-paste(count.2011$lake,count.2011$date,sep='.')

#calculate dry weight (mg)
length.2011$dryWeight.mg<-(1.425*length.2011$bodyLength^3.599)/1000

#make a table of average weight by uniqueID
ag.weight<-aggregate(length.2011$dryWeight.mg,by=list(length.2011$lakeID,length.2011$Date),FUN=mean,na.rm=T)
colnames(ag.weight)<-c('lakeID','dateSample','dryWeight.mg')

#make uniqueID for ag.weight to match to count data
ag.weight$uniqueID<-paste(ag.weight$lakeID,ag.weight$dateSample,sep='.')

dryWeight.mg<-c()
for(i in 1:nrow(count.2011)){
	rowi<-match(count.2011$uniqueID[i],ag.weight$uniqueID)
	dryWeight.mg[i]<-ag.weight$dryWeight.mg[rowi]
}

count.2011$avgWeight.mg<-dryWeight.mg

#fix NAs - use average dry weight from the whole season
EL.avg<-mean(count.2011$avgWeight.mg[count.2011$lake=='EL'],na.rm=T)
WL.avg<-mean(count.2011$avgWeight.mg[count.2011$lake=='WL'],na.rm=T)

count.2011$avgWeight.mg[is.na(count.2011$avgWeight.mg) &count.2011$lake=='EL']=EL.avg
count.2011$avgWeight.mg[is.na(count.2011$avgWeight.mg) &count.2011$lake=='WL']=WL.avg

#2012 data
#use only Chaoborus from EL and WL in length data
length.2012<-length.2012[length.2012$lakeID=='EL'|length.2012$lakeID=='WL',]
length.2012<-length.2012[length.2012$taxa=='chaoborus',]

#convert to weight
length.2012$dryWeight.mg<-(1.425*length.2012$length_mm^3.599)/1000

#make a table of average weight by lake and date
ag.weight<-aggregate(length.2012$dryWeight.mg,by=list(length.2012$lakeID,length.2012$dateSample),FUN=mean,na.rm=T)
colnames(ag.weight)<-c('lakeID','dateSample','avgWeight.mg')

#make unique ID to match to count.2012
ag.weight$uniqueID<-paste(ag.weight$lakeID,ag.weight$dateSample,sep='.')
count.2012$uniqueID<-paste(count.2012$lakeID,count.2012$dateSample,sep='.')

avgWeight.mg<-c()
for(i in 1:nrow(count.2012)){
	rowi<-match(count.2012$uniqueID[i],ag.weight$uniqueID)
	avgWeight.mg[i]<-ag.weight$avgWeight.mg[rowi]
}
count.2012$avgWeight.mg<-avgWeight.mg

#fix NAs
el.avg<-mean(count.2012$avgWeight.mg[count.2012$lakeID=='EL'],na.rm=T)
wl.avg<-mean(count.2012$avgWeight.mg[count.2012$lakeID=='WL'],na.rm=T)

count.2012$avgWeight.mg[is.na(count.2012$avgWeight.mg) &count.2012$lakeID=='EL']=el.avg
count.2012$avgWeight.mg[is.na(count.2012$avgWeight.mg) &count.2012$lakeID=='WL']=wl.avg

#2013
#use only EL and WL chaborus data - for both length and counts
length.2013<-length.2013[length.2013$lakeID=='EL' | length.2013$lakeID=='WL',]
length.2013<-length.2013[length.2013$taxa=='chaoborus',]

count.2013<-count.2013[count.2013$Lake.ID=='EL' | count.2013$Lake.ID=='WL',]
count.2013<-count.2013[count.2013$Depth.Top..m.==0.0 & count.2013$Depth.Bottom..m.>5,]
count.2013<-count.2013[!is.na(count.2013$chaob.counts),]

#calculate dry mass
length.2013$dryWeight.mg<-(1.425*length.2013$length.mm^3.599)/1000

#aggregate by lake and date
ag.weight<-aggregate(length.2013$dryWeight.mg,by=list(length.2013$lakeID,length.2013$dateSample),mean,na.rm=T)
colnames(ag.weight)<-c('lakeID','dateSample','avgWeight.mg')

#make unique ID for both ag.weight and count
ag.weight$uniqueID<-paste(ag.weight$lakeID,ag.weight$dateSample,sep='.')
count.2013$uniqueID<-uniqueID<-paste(count.2013$Lake.ID,count.2013$Sample.date,sep='.')

avgWeight.mg<-c()
for(i in 1:nrow(count.2013)){
	rowi<-match(count.2013$uniqueID[i],ag.weight$uniqueID)
	avgWeight.mg[i]<-ag.weight$avgWeight.mg[rowi]
}
count.2013$avgWeight.mg<-avgWeight.mg

#fix NAs with average seasonal values
el.avg<-mean(count.2013$avgWeight.mg[count.2013$Lake.ID=='EL'],na.rm=T)
wl.avg<-mean(count.2013$avgWeight.mg[count.2013$Lake.ID=='WL'],na.rm=T)

count.2013$avgWeight.mg[is.na(count.2013$avgWeight.mg) &count.2013$Lake.ID=='EL']=el.avg
count.2013$avgWeight.mg[is.na(count.2013$avgWeight.mg) &count.2013$Lake.ID=='WL']=wl.avg

#get rid of unnecessary rows to be able to match to the rest of the data
count.2013<-count.2013[,c(6,3,10,11,12)]

#2014 data
#use only EL and WL and chaborus for both lengths and counts
length.2014<-length.2014[length.2014$lakeID=='EL' | length.2014$lakeID=='WL',]
length.2014<-length.2014[length.2014$taxa=='chaoborus',]

count.2014<-count.2014[count.2014$Lake.ID=='EL' | count.2014$Lake.ID=='WL',]
count.2014<-count.2014[count.2014$Taxa=='chaoborus',]

#calculate dry mass
length.2014$dryWeight.mg<-(1.425*length.2014$length.mm^3.599)/1000

#aggregate by lake and date
ag.weight<-aggregate(length.2014$dryWeight.mg,by=list(length.2014$lakeID,length.2014$dateSample),mean,na.rm=T)
colnames(ag.weight)<-c('lakeID','dateSample','avgWeight.mg')

#make unique ID for both ag.weight and counts to match data together
ag.weight$uniqueID<-paste(ag.weight$lakeID,ag.weight$dateSample,sep='.')
count.2014$uniqueID<-paste(count.2014$Lake.ID,count.2014$Sample.date,sep='.')

#make vector of avg weights to attach to count data
avgWeight.mg<-c()
for(i in 1:nrow(count.2014)){
	rowi<-match(count.2014$uniqueID[i],ag.weight$uniqueID)
	avgWeight.mg[i]<-ag.weight$avgWeight.mg[rowi]
}

count.2014$avgWeight.mg<-avgWeight.mg

#Fix NAs with seasonal average data from each basin
el.avg<-mean(count.2014$avgWeight.mg[count.2014$Lake.ID=='EL'],na.rm=T)
wl.avg<-mean(count.2014$avgWeight.mg[count.2014$Lake.ID=='WL'],na.rm=T)

count.2014$avgWeight.mg[is.na(count.2014$avgWeight.mg) &count.2014$Lake.ID=='EL']=el.avg
count.2014$avgWeight.mg[is.na(count.2014$avgWeight.mg) &count.2014$Lake.ID=='WL']=wl.avg

#get rid of unnecessary columns to match with the rest of the data
count.2014<-count.2014[,c(6,3,11,13,14)]

#rename columns so they all match
colnames(count.2011)<-c('lakeID','dateSample','count','uniqueID','avgWeight.mg')
colnames(count.2012)<-c('lakeID','dateSample','count','uniqueID','avgWeight.mg')
colnames(count.2013)<-c('lakeID','dateSample','count','uniqueID','avgWeight.mg')
colnames(count.2014)<-c('lakeID','dateSample','count','uniqueID','avgWeight.mg')

#combine all data frames
total.count<-rbind(count.2011,count.2012,count.2013,count.2014)

#convert count to abundance m-2
total.count$abundance.m2<-((total.count$count/0.25)/2)/0.07297

#make column of biomass m-2 (in grams)
total.count$g.m2<-(total.count$avgWeight.mg/1000)*total.count$abundance.m2

#Write data to folder
write.csv(total.count,'chaoborusDataLongLake2011-2014.csv')