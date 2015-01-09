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

#2013 data
#load poc data
setwd('~/Documents/Notre Dame/long lake data/POC')
poc.2013<-read.csv('2013UNDERC_POCwSampleInfo.csv')

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
poc.2013$Cmol<-(poc.2013$amountC.mg/1000)/12.011
poc.2013$Nmol<-(poc.2013$amountN.mg/1000)/14.007
poc.2013$Pmol<-(poc.2013$particulateP.ug/1000000)/30.973

#calculate C:P, C:N, and N:P
poc.2013$CP<-poc.2013$Cmol/poc.2013$Pmol
poc.2013$CN<-poc.2013$Cmol/poc.2013$Nmol
poc.2013$NP<-poc.2013$Nmol/poc.2013$Pmol

#clean up data
poc.2013<-poc.2013[,c(22,31,23,24,26,18,17,28,29,32,33,34,35,36,37,38,19)]

#write to covariate data folder
setwd('~/Documents/Notre Dame/Long lake data/covariate data')
write.csv(poc,'pocStoich_2011-2012.csv')
write.csv(poc.2013,'pocStoich2013.csv')