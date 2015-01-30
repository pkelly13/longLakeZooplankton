#MAR data prep, except using GPP instead of chlorophyll.  This script has abit of background work needed to get GPP in the right order and format to use in the covariate matrix
#Patrick Kelly 29 January 2015

#load zooplankton data matrix
setwd('~/Documents/Notre Dame/long lake data/MAR data')
zoop.data<-read.csv('zoopDataMatrix2011-2014.csv')

#load chaoborus data
setwd('~/Documents/Notre Dame/long lake data/chaoborus data')
chaob<-read.csv('chaoborusDataLongLake2011-2014.csv')

#fix date to match zooplankton data
chaob$dateSample<-format(as.Date(chaob$dateSample,'%m/%d/%y'),'%Y-%m-%d')

#make new uniqueID
chaob$uniqueID<-paste(chaob$lakeID,chaob$dateSample,sep='.')

#load water chemistry data
setwd('~/Documents/Notre Dame/long lake data/covariate data')
doc<-read.csv('doc_2011-2014FINAL.csv')
tp<-read.csv('tp_2011-2014FINAL.csv')
stoich<-read.csv('pocStoich_2011-2014.csv')

#fix dates on all of them
doc$dateSample<-format(as.Date(doc$dateSample,'%m/%d/%Y'),'%Y-%m-%d')
doc$uniqueID<-paste(doc$lakeID,doc$dateSample,sep='.')

tp$dateSample<-format(as.Date(tp$dateSample,'%m/%d/%Y'),'%Y-%m-%d')
tp$uniqueID<-paste(tp$lakeID,tp$dateSample,sep='.')

stoich$dateSample<-format(as.Date(stoich$dateSample,'%m/%d/%Y'),'%Y-%m-%d')
stoich$uniqueID<-paste(stoich$lakeID,stoich$dateSample,sep='.')

#Get temperature data
temp<-read.csv('epiTemp2011-2014.csv')
temp$uniqueID<-paste(temp$lakeID,temp$dateSample,sep='.')


cov.mat<-c()
for(i in 1:nrow(zoop.data)){
	row.doc<-match(zoop.data$uniqueID[i],doc$uniqueID)
	row.tp<-match(zoop.data$uniqueID[i],tp$uniqueID)
	row.stoich<-match(zoop.data$uniqueID[i],stoich$uniqueID)
	row.temp<-match(zoop.data$uniqueID[i],temp$uniqueID)
	row.chaob<-match(zoop.data$uniqueID[i],chaob$uniqueID)
	x<-data.frame(DOC=doc$DOC[row.doc],TP=tp$TP[row.tp],CP=stoich$CP[row.stoich],temp=temp$temp[row.temp],chaob.gm2=chaob$g.m2[row.chaob])
	cov.mat<-rbind(cov.mat,x)
}

#fix non-matching data in the covariate matrix by using the value from the closest date
for (i in 1:nrow(cov.mat)){
	lake<-zoop.data$lakeID[i]
	date<-zoop.data$dateSample[i]
	if (is.na(cov.mat$DOC[i])){
		lakei=doc[doc$lakeID==lake,]
		cov.mat$DOC[i]=lakei$DOC[which(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d')) == min(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d'))))][1]
	}
	if (is.na(cov.mat$TP[i])){
		lakei=tp[tp$lakeID==lake,]
		cov.mat$TP[i]=lakei$TP[which(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d')) == min(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d'))))][1]
	}
	if (is.na(cov.mat$CP[i])){
		lakei<-stoich[stoich$lakeID==lake & stoich$depthClass=='PML',]
		cov.mat$CP[i]=lakei$CP[which(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d')) == min(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d'))))][1]
	}
	if(is.na(cov.mat$temp[i])){
		lakei<-temp[temp$lakeID==lake,]
		cov.mat$temp[i]=lakei$temp[which(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d')) == min(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d'))))][1]
	}
	if (is.na(cov.mat$chaob.gm2[i])){
		lakei<-chaob[chaob$lakeID==lake,]
		cov.mat$chaob.gm2[i]=lakei$g.m2[which(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d')) == min(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d'))))][1]
	}
}

#still some NAs, fix with average values
cov.mat$TP[is.na(cov.mat$TP)]=mean(cov.mat$TP,na.rm=T)
cov.mat$CP[is.na(cov.mat$CP)]=mean(cov.mat$CP,na.rm=T)


#load GPP data
setwd('~/Documents/Notre Dame/long lake data/covariate data')
metab<-read.csv('metabolism.csv')

#reformat metab date to match zoop.data
metab$datetime<-format(as.Date(metab$datetime,'%m/%d/%y'),'%Y-%m-%d')

#add uniqueID with week to metab data to average by lake.year.week
metab$uniqueID<-paste(metab$lakeID,format(as.Date(metab$datetime,'%Y-%m-%d'),'%Y'),format(as.Date(metab$datetime,'%Y-%m-%d'),'%U'),sep='.')

#make the same unique ID for zooplankton data to match to
zoop.data$uniqueID<-paste(zoop.data$lakeID,format(as.Date(zoop.data$dateSample,'%Y-%m-%d'),'%Y'),format(as.Date(zoop.data$dateSample,'%Y-%m-%d'),'%U'),sep='.')

#aggregate metab data by uniqueID with GPP.weighted as the response variable
gpp.avg<-aggregate(metab$GPP.weighted,by=list(metab$uniqueID),mean,na.rm=T)
colnames(gpp.avg)<-c('uniqueID','avgGPP')

#match data to zooplankton data
gpp<-c()
for(i in 1:nrow(zoop.data)){
	rowi<-match(zoop.data$uniqueID[i],gpp.avg$uniqueID)
	gpp[i]<-gpp.avg$avgGPP[rowi]
}

gpp.nona<-metab[!is.na(metab$GPP.weighted),]

#replace missing values with value of the closest date from non-NA GPP.weighted's
for(i in 1:length(gpp)){
	if(!is.na(gpp[i])){
		i=i+1
	}
	if(is.na(gpp[i])){
		gpp[i]<-gpp.nona$GPP.weighted[which(abs(as.Date(zoop.data$dateSample[i],'%Y-%m-%d')-as.Date(gpp.nona$datetime,'%Y-%m-%d'))==min(abs(as.Date(zoop.data$dateSample[i],'%Y-%m-%d')-as.Date(gpp.nona$datetime,'%Y-%m-%d'))))][1]
	}
}
gpp<-gpp[-length(gpp)]

#add to cov.mat
cov.mat<-cbind(cov.mat,gpp)

#save covariate data to MAR data folder
setwd('~/Documents/Notre Dame/long lake data/MAR data')
write.csv(cov.mat,'covariateData_wGPP_2011-2014.csv')