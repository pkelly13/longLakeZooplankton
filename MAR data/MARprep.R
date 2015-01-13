#MAR (multivariate autoregressive) model prep for Long Lake zooplankton experiment.  This script uses data frame of zooplankton biomass and puts it in matrix format, also matches environmental covariate data and makes covariate matrix for the MAR

#load zooplankton data
setwd('~/Documents/Notre Dame/long lake data/FINAL_data')

zoops<-read.csv('zoopData2011_2014.csv')

#use only east and west long data
zoops<-zoops[zoops$lakeID=='EL' | zoops$lakeID=='WL',]

#make areal biomass column
zoops$biomass_gDryMass_m2<-(zoops$abundance_num_m3*zoops$depthBottom)*(zoops$meanMass_ug/1000000)

#make uniqueID to match samples together
zoops$uniqueID<-paste(zoops$lakeID,zoops$dateSample,sep='.')

#make frame of daphnia
daphnia<-zoops[zoops$taxa=='daphnia',]

#make frame of cyclopoids
cyclopoid<-zoops[zoops$taxa=='cyclopoid',]

#make frame of holopedium
holopedium<-zoops[zoops$taxa=='holopedium',]

zoop.data<-c()
for(i in 1:nrow(daphnia)){
	rowi<-match(daphnia$uniqueID[i],cyclopoid$uniqueID)
	cyc<-cyclopoid$biomass_gDryMass_m2[rowi]
	rowj<-match(daphnia$uniqueID[i],holopedium$uniqueID)
	holo<-holopedium$biomass_gDryMass_m2[rowj]
	x<-data.frame(lakeID=daphnia$lakeID[i],dateSample=daphnia$dateSample[i],depthBottom=daphnia$depthBottom[i],uniqueID=daphnia$uniqueID[i],daphnia.gm2=daphnia$biomass_gDryMass_m2[i],cyclopoid.gm2=cyc,holhopedium.gm2=holo)
	zoop.data<-rbind(x,zoop.data)
}


#########################################################
#														#
#			Now get covariate data in a matrix			#
#														#
#########################################################

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
chl<-read.csv('chl_2011-2014FINAL.csv')
stoich<-read.csv('pocStoic2011-2013.csv')

#fix dates on all of them
doc$dateSample<-format(as.Date(doc$dateSample,'%m/%d/%Y'),'%Y-%m-%d')
doc$uniqueID<-paste(doc$lakeID,doc$dateSample,sep='.')

tp$dateSample<-format(as.Date(tp$dateSample,'%m/%d/%Y'),'%Y-%m-%d')
tp$uniqueID<-paste(tp$lakeID,tp$dateSample,sep='.')

chl$dateSample<-format(as.Date(chl$dateSample,'%m/%d/%Y'),'%Y-%m-%d')
chl$uniqueID<-paste(chl$lakeID,chl$dateSample,sep='.')

stoich$dateSample<-format(as.Date(stoich$dateSample,'%Y-%m-%d %H:%M:%S'),'%Y-%m-%d')
stoich$uniqueID<-paste(stoich$lakeID,stoich$dateSample,sep='.')

#Get temperature data
temp<-read.csv('epiTemp2011-2014.csv')
temp$uniqueID<-paste(temp$lakeID,temp$dateSample,sep='.')

cov.mat<-c()
for(i in 1:nrow(zoop.data)){
	row.doc<-match(zoop.data$uniqueID[i],doc$uniqueID)
	row.tp<-match(zoop.data$uniqueID[i],tp$uniqueID)
	row.chl<-match(zoop.data$uniqueID[i],chl$uniqueID)
	row.stoich<-match(zoop.data$uniqueID[i],stoich$uniqueID)
	row.temp<-match(zoop.data$uniqueID[i],temp$uniqueID)
	row.chaob<-match(zoop.data$uniqueID[i],chaob$uniqueID)
	x<-data.frame(DOC=doc$DOC[row.doc],TP=tp$TP[row.tp],chl=chl$chl[row.chl],CP=stoich$CP[row.stoich],temp=temp$temp[row.temp],chaob.gm2=chaob$g.m2[row.chaob])
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
	if (is.na(cov.mat$chl[i])){
		lakei=chl[chl$lakeID==lake,]
		cov.mat$chl[i]=lakei$chl[which(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d')) == min(abs(as.Date(lakei$dateSample,'%Y-%m-%d')-as.Date(date,'%Y-%m-%d'))))][1]
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

#save covariate data to MAR data folder
setwd('~/Documents/Notre Dame/long lake data/MAR data')
write.csv(cov.mat,'covariateData2011-2014.csv')