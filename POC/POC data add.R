#Script that matches sample information from the isotope log to POC data for Long Lake POC in 2013 and 2014
#Patrick Kelly 18 December 2014

#load isotope log data
setwd('~/Documents/Notre Dame/long lake data/POC')
log<-read.csv('IsotopeLog2013.csv')

#load data
data.2013<-read.xlsx('LO2013_POC.xlsx',sheetIndex=1)

#cant work the merge function for some reason, so will match the data with the match function in a for loop
ProjectID<-c()
Date.time<-c()
lakeID<-c()
Site<-c()
depthClass<-c()
depth<-c()
volFiltered<-c()
comments<-c()
for(i in 1:nrow(data.2013)){
	rowi<-match(data.2013$ID1[i],log$Isotope.ID)
	ProjectID[i]<-log$Project.ID[rowi]
	Date.time[i]<-log$Date.time[rowi]
	lakeID[i]<-log$Lake.ID[rowi]
	Site[i]<-log$Site[rowi]
	depthClass[i]<-log$Depth.class[rowi]
	depth[i]<-log$Depth..m.[rowi]
	volFiltered[i]<-log$Volume.Filtered..mL.[rowi]
	comments[i]<-log$comments[rowi]
}
data.2013$ProjectID<-ProjectID
data.2013$Date.time<-Date.time
data.2013$lakeID<-lakeID
data.2013$Site<-Site
data.2013$depthClass<-depthClass
data.2013$depth<-depth
data.2013$volFiltered<-volFiltered
data.2013$comments<-comments

#write data to csv file
write.csv(data.2013,'2013UNDERC_POCwSampleInfo.csv')

#fix dates
data.2013<-read.csv('2013UNDERC_POCwSampleInfo.csv')

dateTimeSample<-format(as.Date(data.2013$Date.time,'%m/%d/%y %H:%M:%S'),'%Y-%m-%d %H:%M:%S')

#add sample information to POP data
#first, load 2014 data in addition to 2013 data
data.2014<-read.csv('IsotopeLog2014.csv')

#load POP data
setwd('~/Documents/Notre Dame/UNDERC 2014/water chemistry')
pop<-read.xlsx('UNDERC2014_particulateP.xlsx',sheetIndex=1)
num<-substr(pop$sampleID,3,20)
num<-as.numeric(num)