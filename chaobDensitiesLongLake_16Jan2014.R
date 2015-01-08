#Script that will look at Chaoborus densities for the Long Lake project - calculated in average # ind/m2 over the coarse of the sampling season
#PTK 16 Jan 2014

#first need to load data for 2011 using Katies data - much more detailed and accurate than from the survey - located in Katie Baglini spring data > R files >2011 ChaoborusCounts
chaob2011<-file.choose()
chaob2011<-read.csv(chaob2011)

avgChaob2011<-tapply(chaob2011$chaoborus_count,chaob2011$lake,mean,na.rm=T)
avgChaob2011<-(((avgChaob2011/2)/0.07297))

#load data from 2012 - located in long lake data>2012 Production Calc>LOchaobCounts csv file
chaob2012<-file.choose()
chaob2012<-read.csv(chaob2012)

avgChaob2012<-tapply(chaob2012$count,chaob2012$lakeID,mean,na.rm=T)
avgChaob2012<-((avgChaob2012/2)/0.07297)

#load data from 2013 - both zooplankton log and chaob data
zoopLog<-file.choose()
zoopLog<-read.csv(zoopLog)

chaob2013<-file.choose()
chaob2013<-read.csv(chaob2013)

#make new data frame with data from zoop log
towType<-c()
depthTop<-c()
depthBottom<-c()
date<-c()
for(i in 1:nrow(chaob2013)){
	rowi=match(chaob2013$Zoop.ID[i],zoopLog$Zoop.ID)
	towType[i]=toString(zoopLog$tow.type[rowi])
	depthTop[i]=zoopLog$Depth.Top..m.[rowi]
	depthBottom[i]=zoopLog$Depth.Bot..m.[rowi]
	date[i]=toString(zoopLog$Date[rowi])
}

chaob<-data.frame(zoopID=chaob2013$Zoop.ID,lakeID=chaob2013$Lake.ID,dateSample=date,depthTop,depthBottom,towType=towType,counts=chaob2013$chaob.counts)

#use only vertical tows
chaob2013<-chaob[chaob$towType=='vertical',]

#use only east and west long
chaob2013<-chaob2013[chaob2013$lakeID=='EL' | chaob2013$lakeID=='WL',]

avgChaob2013<-tapply(chaob2013$counts,chaob2013$lakeID,mean,na.rm=T)[c(8,17)]
avgChaob2013<-((avgChaob2013/2)/0.07297)

#make a new data frame combining all data to look at chaoborus density through time
chaobs<-data.frame(lakeID=c(as.character(chaob2011$lake),as.character(chaob2012$lakeID),as.character(chaob2013$lakeID)),dateSample=c(as.character(chaob2011$date),as.character(chaob2012$dateSample),as.character(chaob2013$dateSample)),count=c(chaob2011$chaoborus_count,chaob2012$count,chaob2013$counts))

chaobs$dateSample<-format(as.Date(chaobs$dateSample,'%m/%d/%y'),format='%m/%d/%Y')
chaobs$count_indM2<-((chaobs$count)/2)/0.07297

#write data file to MAR model data folder
setwd('~/Documents/Notre Dame/long lake Data/MAR data')
write.csv(chaobs,'LOchaobTimeSeriesData_1Feb2014.csv')