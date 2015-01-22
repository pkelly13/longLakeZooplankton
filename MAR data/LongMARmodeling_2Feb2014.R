#Data manipulation for input into the MAR model
#Need to get data in a time series based on groups with colmns being different groups (Daphnia,cyclopoids,holopedium,chaoborus)

#load data from the MAR data folder in Documents>ND>long lake data
setwd('~/Documents/Notre Dame/long lake data/')

#need zooplankton data in abundance through time, need to source zooplankton time series analysis to get zoopData data frames

source('timeSeriesBiomassAnalysis_27Dec2013.R')

#TRY THIS WITH BIOMASS TIME SERIES DATA
setwd('~/Documents/Notre Dame/long lake data/MAR data')

mass<-read.csv('biomassTimeSeriesData_1Feb2014.csv')

#make matching unique ID in biomass data to match to chaob data to
mass$uniqueID<-paste(mass$lakeID,mass$dateSample,sep='')

#make taxa specific data frames
daphnia<-mass[mass$taxa=='daphnia',]
cyclopoids<-mass[mass$taxa=='cyclopoid',]
holopedium<-mass[mass$taxa=='holopedium',]

#load chaoborus data
setwd('~/Documents/Notre Dame/long lake data/MAR data')
chaobs<-read.csv('LOchaobTimeSeriesData_1Feb2014.csv')

#fix date
chaobs$dateSample<-format(as.Date(chaobs$dateSample,'%m/%d/%Y'),'%Y-%m-%d')

#make unique IDs (lakedate) for chaob data to match biomass data to
chaobs$uniqueID<-paste(chaobs$lakeID,chaobs$dateSample,sep='')

#match taxa to chaobs
daph<-c()
for(i in 1:nrow(chaobs)){
	rowi=match(chaobs$uniqueID[i],daphnia$uniqueID)
	daph[i]=daphnia$biomass[rowi]
}
chaobs$daphnia=daph

cyc<-c()
for(i in 1:nrow(chaobs)){
	rowi=match(chaobs$uniqueID[i],cyclopoids$uniqueID)
	cyc[i]=cyclopoids$biomass[rowi]
}
chaobs$cyclopoid=cyc

holo<-c()
for(i in 1:nrow(chaobs)){
	rowi=match(chaobs$uniqueID[i],holopedium$uniqueID)
	holo[i]=holopedium$biomass[rowi]
}
chaobs$holopedium=holo

#sort data
chaobs<-chaobs[order(chaobs$dateSample),]

#add year
chaobs$year<-format(as.Date(chaobs$dateSample,'%Y-%m-%d'),'%Y')

#get rid of NAs
chaobs<-chaobs[!is.na(chaobs$cyclopoid),]

#make lake-year ID
chaobs$lakeYear<-paste(chaobs$lakeID,chaobs$year,sep='')
lakeYear=c(chaobs$lakeYear[chaobs$lakeID=='WL'],chaobs$lakeYear[chaobs$lakeID=='EL'])

#build matrix of data with 4 taxa
tsMat=matrix(c(c(chaobs$daphnia[chaobs$lakeID=='WL'],chaobs$daphnia[chaobs$lakeID=='EL']),c(chaobs$cyclopoid[chaobs$lakeID=='WL'],chaobs$cyclopoid[chaobs$lakeID=='EL']),c(chaobs$holopedium[chaobs$lakeID=='WL'],chaobs$holopedium[chaobs$lakeID=='EL']),c(chaobs$count_indM2[chaobs$lakeID=='WL'],chaobs$count_indM2[chaobs$lakeID=='EL'])),ncol=4)

colnames(tsMat)=c('daph','cyc','holo','chaob') #to keep track of what is what

X=log(tsMat[1:nrow(tsMat)-1,])
Y=log(tsMat[2:nrow(tsMat),])

#make indicator matrix for treatment vs reference where treatment is 1 and reference is 0
phi<-c()
for(i in 1:length(lakeYear)){
	if(lakeYear[i]=='EL2013'){
		phi[i]<-1
	}
	else{
		phi[i]<-0
	}
}
phi<-matrix(c(phi,phi,phi,phi),ncol=4)
phi=phi[1:nrow(phi)-1,]


#remove transition year data points in both X,Y, and phi
WLyearX<-chaobs$year[chaobs$lakeID=='WL'][1:length(chaobs$year[chaobs$lakeID=='WL'])-1] #offset years to find transition years
WLyearY<-chaobs$year[chaobs$lakeID=='WL'][2:length(chaobs$year[chaobs$lakeID=='WL'])]

X=X[WLyearX==WLyearY,]
Y=Y[WLyearX==WLyearY,]
phi=phi[WLyearX==WLyearY,]
phi<-phi[,1]

one=rep(1,nrow(X)) #1 in the model

Z<-cbind(one,phi,X) #combining 1, X, u if we had it

#estimate parameters using CLS
D<-solve(t(Z)%*%Z)%*%t(Z)%*%Y

#calculate predicted 
predict<-Z%*%D
#Q=length of time series
#P=number of species
#R=number of coviates
#E=errors (predict-X)
#varY=variance in Y=obs-mean
#root mean squared error sd(E)

Q<-nrow(X)
P<-ncol(X)
R<-0
E<-predict-Y
rmse<-sd(E)
varY=matrix(c(Y[,1]-mean(Y[,1]),Y[,2]-mean(Y[,2]),Y[,3]-mean(Y[,3]),Y[,4]-mean(Y[,4])),ncol=4)

sigma<-t(E)%*%E/Q
lnlike<--Q*(P/2)*log(2*pi)-(Q/2)*log(det(sigma))-Q*P/2

#calculate total r2
varMatrix<-t(varY)%*%varY/Q
R2<-1-diag(sigma)/diag(varMatrix)


#calculate r^2 based on dY's
varDY=(Y-X)[-1,]
DYhat=c()
for(i in 2:nrow(predict)){
	x=predict[i,]-predict[i-1,]
	DYhat=rbind(DYhat,x)
}

E_D<-DYhat-DY

QD<-nrow(DY)
varMatrix_D<-t(varDY)%*%varDY/Q
R2_D<-diag(sigma)/diag(varMatrix_D)