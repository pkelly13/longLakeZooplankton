#make the covarite matrix to use in the MAR model to determine drivers of zooplankton biomass through time
#Patrick Kelly - 8 Jan 2014

#load chorophyll data
setwd('~/Documents/Notre Dame/long lake data/covariate data')

chl<-read.csv('Chl2014.csv')

#use only unflagged data
chl<-chl[chl$flag==0,]

#use only east and west long and PML
chl<-chl[chl$lakeID=='EL' | chl$lakeID=='WL',]

#aggregate by lake, date, and depth class to get a data frame of chlorophyll with the average of replicates
chl<-aggregate(chl$chl,by=list(chl$lakeID,chl$dateSample,chl$depthClass),mean,na.rm=T)
colnames(chl)<-c('lakeID','dateSample','depthClass','chl')
chl<-chl[chl$depthClass=='PML',]

#do the same for chl 2011-2013 data and combine data frames
chl.past<-dbGetQuery(con,'SELECT chl.lakeID,chl.dateSample,chl.depthClass,chl.chl,chl.replicate,chl.flag FROM CHLOROPHYLL AS chl')

#get rid of flagged data
chl.past<-chl.past[chl.past$flag==0,]

#use only east and west PML data
chl.past<-chl.past[chl.past$lakeID=='EL' | chl.past$lakeID=='WL',]
chl.past<-chl.past[chl.past$depthClass=='PML',]

#aggregate the data
chl.past<-aggregate(chl.past$chl,by=list(chl.past$lakeID,chl.past$dateSample,chl.past$depthClass),mean,na.rm=T)
colnames(chl.past)<-c('lakeID','dateSample','depthClass','chl')

#fix date of chl.past data frame to match chl.past
chl.past$dateSample<-format(as.Date(chl.past$dateSample,'%Y-%m-%d %H:%M:%S'),'%m/%d/%Y')
chl$dateSample<-format(as.Date(chl$dateSample,'%m/%d/%Y'),'%m/%d/%Y')

chl.final<-rbind(chl.past,chl)
write.csv(chl.final,'chl_2011-2014FINAL.csv')

#load DOC data
doc<-read.csv('DOC2014.csv')

#use unflagged data
doc<-doc[doc$flag==0,]

#use east and west long and PML
doc<-doc[doc$Lake.ID=='EL' | doc$Lake.ID=='WL',]
doc<-doc[doc$Depth.Class=='PML',]

#make data frame by averaging between replicates
doc<-aggregate(doc$DOC_mgL,by=list(doc$Lake.ID,doc$Date.Sample,doc$Depth.Class),mean,na.rm=T)
colnames(doc)<-c('lakeID','dateSample','depthClass','DOC')

#load 2011-2013 data
doc.past<-dbGetQuery(con,'SELECT doc.lakeID,doc.dateSample,doc.depthClass,doc.DOC,doc.replicate,doc.flag FROM DOC AS doc')

#remove flagged data
doc.past<-doc.past[doc.past$flag==0,]

#aggregate the data
doc.past<-aggregate(doc.past$DOC,by=list(doc.past$lakeID,doc.past$dateSample,doc.past$depthClass),mean,na.rm=T)
colnames(doc.past)<-c('lakeID','dateSample','depthClass','DOC')

#us only east and west PML
doc.past<-doc.past[doc.past$lakeID=='EL' | doc.past$lakeID=='WL',]
doc.past<-doc.past[doc.past$depthClass=='PML',]

#fix dates
doc.past$dateSample<-format(as.Date(doc.past$dateSample,'%Y-%m-%d %H:%M:%S'),'%m/%d/%Y')
doc$dateSample<-format(as.Date(doc$dateSample,'%m/%d/%Y'),'%m/%d/%Y')

doc.final<-rbind(doc.past,doc)

#write data to covariate folder
write.csv(doc.final,'doc_2011-2014FINAL.csv')

#Total phosphorus data
#load 2014 TP data
tp.2014<-read.xlsx('UNDERC2014_TP.xlsx',sheetIndex=1)

#load TP log
tp.log<-read.xlsx('Unfiltered Water Log 2014.xlsx',sheetIndex=1)

#match TP data Conc.ugL to TP log
tp<-c()
for(i in 1:nrow(tp.log)){
	rowi<-match(tp.log$Unfiltered.ID[i],tp.2014$sampleID)
	tp[i]<-tp.2014$Conc.ugL[rowi]
}
tp.log$TP<-tp

#use only filled out data
tp.log<-tp.log[1:405,]

#use only east and west long PML
tp<-tp.log[tp.log$Lake.ID=='EL' | tp.log$Lake.ID=='WL',]
tp<-tp[tp$Depth.Class=='PML',]

#load 2011-2013 data
tp.past<-dbGetQuery(con, 'SELECT tp.lakeID, tp.dateSample, tp.depthClass, tp.TP, tp.flag FROM NUTRIENTS AS tp')

#use only east and west long and PML
tp.past<-tp.past[tp.past$lakeID=='EL' | tp.past$lakeID=='WL',]
tp.past<-tp.past[tp.past$depthClass=='PML',]

#use unflagged data
tp.past<-tp.past[tp.past$flag==0,]
tp.past<-tp.past[,-ncol(tp.past)]

#get rid of unnecessary columns from tp data frame to combine with 2014 data
tp<-tp[,c(3,5,7,12)]
colnames(tp)<-c('lakeID','dateSample','depthClass','TP')


#fix dates and combine
tp.past$dateSample<-format(as.Date(tp.past$dateSample,'%Y-%m-%d %H:%M:%S'),'%m/%d/%Y')
tp$dateSample<-format(as.Date(tp$dateSample,'%Y-%m-%d'),'%m/%d/%Y')

#combine the two data frames
tp.final<-rbind(tp.past,tp)

#write data to folder
write.csv(tp.final,'tp_2011-2014FINAL.csv')

#Get stochiometry data into usable format
poc<-read.csv('pocStoich_2011-2012.csv')
poc.2013<-read.csv('pocStoich2013.csv')

#add year to both data frame
poc$year<-format(as.Date(poc$dateSample,'%Y-%m-%d %H:%M:%S'),'%Y')
poc.2013$year<-format(as.Date(poc.2013$dateSample,'%Y-%m-%d %H:%M:%S'),'%Y')

#use only columns that are needed (lake ID, date, depthClass, POC, particulateP.ugL, CP)
poc<-poc[,c(2,3,4,8,12,16,19)]
poc.2013<-poc.2013[,c(23,32,25,30,33,37,40)]
#fix column names to match poc
colnames(poc.2013)<-c('lakeID','dateSample','depthClass','POC','particulateP.ugL','CP','year')

#combine data frames
poc.tot<-rbind(poc,poc.2013)

write.csv(poc.tot,'pocStoic2011-2013.csv')


#temperature data for epilimnetic temperature as a covariate
temp<-dbGetQuery(con,'SELECT prof.lakeID,prof.dateSample,prof.depthBottom,prof.temp FROM LIMNO_PROFILES AS prof')

#use only east and west long from 0.5m
temp<-temp[temp$lakeID=='EL' | temp$lakeID=='WL',]
temp<-temp[temp$depthBottom==0.5,]

#load 2014 data
temp.2014<-read.csv('Limno Profiles Log 2014.csv')

#use only east and west long from 0.5m
temp.2014<-temp.2014[temp.2014$Lake.ID=='EL' | temp.2014$Lake.ID=='WL',]
temp.2014$Depth..m.<-as.numeric(temp.2014$Depth..m.)
temp.2014<-temp.2014[temp.2014$Depth..m.==0.50,]

#use only necessary columns
temp.2014<-temp.2014[,c(2,4,6,7)]

#fix the dates
temp$dateSample<-format(as.Date(temp$dateSample,'%Y-%m-%d %H:%M:%S'),'%Y-%m-%d')
temp.2014$Date.Sample<-format(as.Date(temp.2014$Date.Sample,'%m/%d/%y'),'%Y-%m-%d')

#change 2014 temp column headings
colnames(temp.2014)<-c('lakeID','dateSample','depthBottom','temp')

#combine the two data frames
temp.final<-rbind(temp,temp.2014)

#write to folder
write.csv(temp.final,'epiTemp2011-2014.csv')