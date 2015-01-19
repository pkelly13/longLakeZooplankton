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
log<-read.csv('IsotopeLog2014.csv')

#load POC data
setwd('~/Documents/Notre Dame/long lake data/POC')
data.2014<-read.xlsx('LO2014_POC.xlsx',sheetIndex=1)

#add accompanying info from POC log
ProjectID<-c()
Date.sample<-c()
lakeID<-c()
Site<-c()
depthClass<-c()
depth<-c()
volFiltered<-c()
comments<-c()
for(i in 1:nrow(data.2014)){
	rowi<-match(data.2014$ID1[i],log$Isotope.ID)
	ProjectID[i]<-log$Proj.ID[rowi]
	Date.sample[i]<-log$Date.Sample[rowi]
	lakeID[i]<-log$Lake.ID[rowi]
	Site[i]<-log$Site[rowi]
	depthClass[i]<-log$Depth.class[rowi]
	depth[i]<-log$Depth.Bot..m.[rowi]
	volFiltered[i]<-log$Vol..Filt...mL.[rowi]
	comments[i]<-log$comments[rowi]
}
data.2014$ProjectID<-ProjectID
data.2014$dateSample<-Date.sample
data.2014$lakeID<-lakeID
data.2014$Site<-Site
data.2014$depthClass<-depthClass
data.2014$depth<-depth
data.2014$volFiltered<-volFiltered
data.2014$comments<-comments

#fix the NAs
setwd('~/Documents/Notre Dame/long lake data/POC')
log<-read.csv('IsotopeLog2013.csv')

missing<-log[log$Isotope.ID=='P-0957' | log$Isotope.ID=='P-0958',]
data.2014$ProjectID[c(1,2)]=missing$Project.ID
data.2014$dateSample[c(1,2)]<-format(as.Date(missing$Date.time,'%m/%d/%y %H:%M'),'%m/%d/%Y')
data.2014$lakeID[c(1,2)]=missing$Lake.ID
data.2014$Site[c(1,2)]=missing$Site
data.2014$depthClass[c(1,2)]=missing$Depth.class
data.2014$depth[c(1,2)]<-missing$Depth..m.
data.2014$volFiltered[c(1,2)]<-missing$Volume.Filtered..mL.
data.2014$comments[c(1,2)]<-missing$comments

#fix the dates
data.2014$dateSample<-format(as.Date(data.2014$dateSample,'%m/%d/%Y'),'%m/%d/%Y')

#write data to csv file
write.csv(data.2014,'2014UNDERC_POCwSampleInfo.csv')