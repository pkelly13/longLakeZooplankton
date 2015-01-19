#Look at C:P stoichiometry for East and West Long 
#Patrick Kelly 6 Jan 2015

#load 2011 and 2012 data from the database
poc<-dbGetQuery(con,'SELECT poc.lakeID,poc.dateSample,poc.depthClass,poc.filterVol,poc.sampleType,poc.sampleAmount,poc.POC,poc.PON,poc.flag FROM POC as poc')

#get ride of flagged data
poc<-poc[poc$flag==0,]

#use only east an west long and filter sample type
poc<-poc[poc$lakeID=='EL' | poc$lakeID=='WL',]
poc<-poc[poc$sampleType=='filter',]

#make uniqueID to match poc data to POP data
poc$uniqueID<-paste(poc$lakeID,poc$dateSample,poc$depthClass,sep='.')

#load particulate P data
pop<-dbGetQuery(con,'SELECT pop.lakeID,pop.dateSample,pop.depthClass,pop.particulateP,pop.flag FROM NUTRIENTS AS pop')

#use only east and west data
pop<-pop[pop$lakeID=='EL' | pop$lakeID=='WL',]

#make uniqueID
pop$uniqueID<-paste(pop$lakeID,pop$dateSample,pop$depthClass,sep='.')

#match particulate P with the particulate carbon data
particulateP.ugL<-c()
for(i in 1:nrow(poc)){
	rowi<-match(poc$uniqueID[i],pop$uniqueID)
	particulateP.ugL[i]<-pop$particulateP[rowi]
}
poc$particulateP.ugL<-particulateP.ugL

#calculate C, N, and P in mols
poc$Cmol<-(poc$filterVol/1000*(poc$POC/1000))/12.011
poc$Nmol<-(poc$filterVol/1000*(poc$PON/1000))/14.007
poc$Pmol<-(poc$filterVol/1000*(poc$particulateP.ugL/1000000))/30.973

#calculate C:N, C:P, and N:P
poc$CP<-poc$Cmol/poc$Pmol
poc$CN<-poc$Cmol/poc$Nmol
poc$NP<-poc$Nmol/poc$Pmol

#2013-2014 data
#load poc data
setwd('~/Documents/Notre Dame/long lake data/POC')
poc.2013<-read.csv('2013UNDERC_POCwSampleInfo.csv')
poc.2014<-read.csv('2014UNDERC_POCwSampleInfo.csv')

#calculate POC and PON for poc.2014
poc.2014$PON<-poc.2014$mgN/(poc.2014$volFiltered/1000)
poc.2014$POC<-poc.2014$mgC/(poc.2014$volFiltered/1000)
poc.2014$flag<-rep(0,nrow(poc.2014))

#reformat 2014 data to match 2013 data
poc.2014<-poc.2014[,c(1:16,18,17,29,19,20,21,22:28)] #shuffle columns around to match 2013 data
poc.2013<-poc.2013[,-c(ncol(poc.2013),ncol(poc.2013)-1)]
poc.2013$Date.time<-format(as.Date(poc.2013$Date.time,'%m/%d/%y %H:%M'),'%m/%d/%Y') #reformat dates to match
colnames(poc.2013)=colnames(poc.2014) #change the column names in poc.2013 to the same as poc.2014

#combine the two data frames
poc.2013<-rbind(poc.2013,poc.2014)

#save for use in isotopes
setwd('~/Documents/Notre Dame/long lake data/isotopes')
write.csv(poc.2013,'POCisotopes_2013-2014.csv')

#load POP data
setwd('~/Documents/Notre Dame/UNDERC 2014/Water chemistry')
pop.2013<-read.xlsx('UNDERC2014_particulateP.xlsx',sheetIndex=1)

#match POP data to POC data
particulateP.ug<-c()
for(i in 1:nrow(poc.2013)){
	rowi<-match(poc.2013$ID1[i],pop.2013$sampleID)
	particulateP.ug[i]<-pop.2013$Pug[rowi]
}
poc.2013$particulateP.ug<-particulateP.ug

#calculate C, N, and P in mols
poc.2013$Cmol<-(poc.2013$mgC/1000)/12.011
poc.2013$Nmol<-(poc.2013$mgN/1000)/14.007
poc.2013$Pmol<-(poc.2013$particulateP.ug/1000000)/30.973

#calculate C:P, C:N, and N:P
poc.2013$CP<-poc.2013$Cmol/poc.2013$Pmol
poc.2013$CN<-poc.2013$Cmol/poc.2013$Nmol
poc.2013$NP<-poc.2013$Nmol/poc.2013$Pmol

#clean up data
poc<-poc[,-c(5,6,10)]
poc$dateSample<-format(as.Date(poc$dateSample,'%Y-%m-%d %H:%M:%S'),'%m/%d/%Y')
cu.poc<-poc.2013[,c(22,21,24,26,29,28,19,30,31:36)]
colnames(poc)=colnames(cu.poc)

poc<-rbind(poc,cu.poc)


#write to covariate data folder
setwd('~/Documents/Notre Dame/Long lake data/covariate data')
write.csv(poc,'pocStoich_2011-2014.csv')
