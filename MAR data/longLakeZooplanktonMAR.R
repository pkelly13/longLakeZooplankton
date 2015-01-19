#Script that runs MAR (multivariate autoregressive) model for Long Lake zooplankton from 2011-2014 (browning experiment)
#Objective: to determine potential drivers of zooplankton biomass across the experiments in the two basins
#Using effect sizes of a model of hypothesized drivers, rather than running exhaustive search of potential drivers
#Patrick Kelly 13 January 2014

#load zooplankton data
setwd('~/Documents/Notre Dame/long lake data/MAR data')

zoops<-read.csv('zoopDataMatrix2011-2014.csv')

#load covariate data
cov.mat<-read.csv('covariateData2011-2014.csv')

#combine the two to sort
tot.data<-cbind(zoops,cov.mat[2:ncol(cov.mat)])

#sort by date
tot.data<-tot.data[order(tot.data$lakeID,tot.data$dateSample),]


#make lake-year ID
tot.data$year<-format(as.Date(tot.data$dateSample,'%Y-%m-%d'),'%Y')
tot.data$lake.year<-paste(tot.data$lakeID,format(as.Date(tot.data$dateSample,'%Y-%m-%d'),'%Y'),sep='.')
lake.year<-c(tot.data$lake.year[tot.data$lakeID=='EL'],tot.data$lake.year[tot.data$lakeID=='WL'])

#remove NAs
tot.data<-tot.data[!is.na(tot.data$cyclopoid.gm2),]
tot.data<-tot.data[!is.na(tot.data$holhopedium.gm2),]

#build a matrix of data with 3 taxa
tsMat<-as.matrix(tot.data[,6:8],ncol=3)

#make X and Y
X=tsMat[1:nrow(tsMat)-1,] # this is Xt
Y=tsMat[2:nrow(tsMat),] #this is Xt+1

#make covariate matrix
covMat<-as.matrix(tot.data[,9:14],ncol=6)
covMat<-covMat[1:nrow(covMat)-1,]

#remove transition year data points in both X, Y, and covariate data
WLyearX<-tot.data$year[tot.data$lakeID=='WL'][1:length(tot.data$year[tot.data$lakeID=='WL'])-1]
WLyearY<-tot.data$year[tot.data$lakeID=='WL'][2:length(tot.data$year[tot.data$lakeID=='WL'])]

X<-X[WLyearX==WLyearY,]
Y<-Y[WLyearX==WLyearY,]
covMat<-covMat[WLyearX==WLyearY,]

#make z-scored matrix to find effect sizes
col.mean<-colMeans(covMat) #get means for columns
col.sd<-apply(covMat,2,sd)

z.covMat<-c()
for(i in 1:nrow(covMat)){
	x<-(covMat[i,]-col.mean)/col.sd
	z.covMat<-rbind(z.covMat,x)
}

#z-score X and Y
col.mean<-colMeans(X)
col.sd<-apply(X,2,sd)

z.X<-c()
for(i in 1:nrow(X)){
	x<-(X[i,]-col.mean)/col.sd
	z.X<-rbind(z.X,x)
}

col.mean<-colMeans(Y)
col.sd<-apply(Y,2,sd)

z.Y<-c()
for(i in 1:nrow(X)){
	x<-(Y[i,]-col.mean)/col.sd
	z.Y<-rbind(z.Y,x)
}

one<-rep(1,nrow(X)) #1 in the model

Z<-cbind(one,z.X,z.covMat) #combining 1, X, u <---can change z.covMat to covMat if you don't want z-scored covariate matrix

#estimate parameters using CLS
D<-solve(t(Z)%*%Z)%*%t(Z)%*%z.Y

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
varY=matrix(c(Y[,1]-mean(Y[,1]),Y[,2]-mean(Y[,2]),Y[,3]-mean(Y[,3])),ncol=3)

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

E_D<-DYhat-varDY

QD<-nrow(Y)
varMatrix_D<-t(varDY)%*%varDY/Q
R2_D<-diag(sigma)/diag(varMatrix_D)