### Randomized Intervention Analysis (RIA) test for Long Experiment
### 5-4-13

rm(list=ls()) #clear all

setwd("/Files/NDongoing/TerrestrialCarbon/M2Mdatabase") #set this to the location of the database

library(RSQLite)

drv=SQLite() #create driver object

con=dbConnect(drv,dbname="M2Mdb_050313.db") #open database connection

units=dbGetQuery(con,'SELECT * FROM UNITS')


# RIA function
RIA<-function(pre1,pre2,post1,post2,n.iter=1000){
	# test statistic calculation
	statistic=abs(mean(pre1-pre2,na.rm=T)-mean(post1-post2,na.rm=T))
	non.abs.statistic=mean(pre1-pre2,na.rm=T)-mean(post1-post2,na.rm=T)
	# permutation
	rand_statistic=numeric(n.iter)
	for(i in 1:n.iter){
		tmp1=sample(c(pre1,post1),length(pre1)+length(post1),replace=FALSE)
		tmp2=sample(c(pre1,post1),length(pre1)+length(post1),replace=FALSE)
		rand_statistic[i]=abs(mean(tmp1[1:length(pre1)]-tmp2[1:length(pre2)])-mean(tmp1[(length(pre1)+1):length(tmp1)]-tmp2[(length(pre2)+1):length(tmp2)]))
	}
	
	return(list(statistic=statistic,non.abs.statistic=non.abs.statistic,randomizations=rand_statistic,p=sum(rand_statistic>statistic)/n.iter))
}



# organizing Long 2011 zoop data from database --> USING STUFF FROM SURVEY DOWN BELOW
#zoopDB=dbGetQuery(con,'SELECT * FROM ZOOPS_ABUND_BIOMASS')
#zoopLong=zoopDB[zoopDB$lakeID%in%c("WL","EL"),]	# only Long Lake (East and West)
#zoopLong=zoopLong[zoopLong$metadataID!="ZoopCounts.20110517",]	#No Katie counts
#zoopLongYear=strftime(strptime(zoopLong$dateSample,format="%Y-%m-%d %H:%M:%S"),format="%Y")
#zoopLong2011=zoopLong[zoopLongYear=="2011",]
#zoopLong2011$abundance=zoopLong2011$abundance/0.25	# adjust for net efficiency
#zoopLong2011$biomass=zoopLong2011$abundance*zoopLong2011$meanMass/1000000	# g dry mass m-2

#dates=sort(unique(zoopLong2011$dateSample))
#dates=dates[-3]		# WL is missing 6/15/2011???
#taxa=sort(unique(zoopLong2011$taxa))

#wl2011=matrix(0,length(dates),length(taxa))
#rownames(wl2011)=dates
#colnames(wl2011)=taxa
#el2011=wl2011

#for(i in 1:nrow(zoopLong2011)){
#	if(zoopLong2011$lakeID[i]=="WL"){
#		wl2011[dates==zoopLong2011$dateSample[i],taxa==zoopLong2011$taxa[i]]=zoopLong2011$biomass[i]
#	}else{
#		el2011[dates==zoopLong2011$dateSample[i],taxa==zoopLong2011$taxa[i]]=zoopLong2011$biomass[i]
#	}
#}

# total biomass per m2
#elTotal2011=rowSums(el2011)*1000*0.5	# mg C m-2
#wlTotal2011=rowSums(wl2011)*1000*0.5	# mg C m-2


# Total Biomass
#quartz()
#plot(elTotal2011,type='o',ylim=c(0,max(c(elTotal2011,wlTotal2011))),ylab="Total Zoop Biomass (mg C m-2)",xlab="Time point 2011")
#points(wlTotal2011,pch=15)
#lines(wlTotal2011,lty=2)

#quartz()
#par(mfrow=c(2,1))
#acf(elTotal2011)
#acf(wlTotal2011)
# appears no significant autocorrelation


# use log transformed to allow multivariate normal
#library(mvtnorm)

#LelTotal2011=log(elTotal2011)
#LwlTotal2011=log(wlTotal2011)

#quartz()
#plot(LelTotal2011,type='o',ylim=range(c(LelTotal2011,LwlTotal2011)),ylab="LN(Total Zoop Biomass (mg C m-2))",xlab="Time point 2011")
#points(LwlTotal2011,pch=15)
#lines(LwlTotal2011,lty=2)

#muLel=mean(LelTotal2011)
#sigmaLel=sd(LelTotal2011)
#muLwl=mean(LwlTotal2011)
#sigmaLwl=sd(LwlTotal2011)

#covMat=matrix(c(sigmaLel^2,2*sigmaLel*sigmaLwl,2*sigmaLel*sigmaLwl,sigmaLwl^2),2,2,byrow=TRUE)

# Testing RIA w/ distribution from 2011 zoop data
#impact=1.25#0.9	#fold change resulting from intervention
#length.pre=22
#length.post=22

#pre=rmvnorm(length.pre,c(muLel,muLwl),sigma=covMat,method="svd")
#post=rmvnorm(length.post,c(muLel*impact,muLwl),sigma=covMat,method="svd")
#colnames(pre)=colnames(post)=c('T','R')

#quartz()
#plot(c(pre[,2],post[,2]),type='o',ylim=range(rbind(pre,post)))
#lines(c(pre[,1],post[,1]),lty=2)
#points(c(pre[,1],post[,1]),pch=15)

#ria=RIA(pre[,2],pre[,1],post[,2],post[,1])


# wilcoxon rank sum test --> tends to find differences the majority of the time, even with impact=1
wilcox<-function(pre,post){
	R=c(pre[,2],post[,2])
	T=c(pre[,1],post[,1])
	abs_diff=abs(T-R)
	sign_diff=sign(T-R)
	diffs=cbind(abs_diff,sign_diff)
	diffs=diffs[diffs[,1]!=0,]
	diffs=diffs[order(diffs[,1]),]
	diffs=cbind(diffs,rank(diffs[,1],ties.method="average"))
	W=abs(sum(diffs[,2]*diffs[,3]))
	N=nrow(diffs)
	sigmaW=sqrt(N*(N+1)*(2*N+1)/6)
	z=(W-0.5)/sigmaW
	p=pnorm(z,lower.tail=FALSE)
	return(list(W=W,z=z,p=p))
}


# does covariance change post manipulation
#	-look at covariance amongst survey lakes to see if DOC or distance predicts this; use to inform change in covariance across manip.
netEfficiency=0.25

zoopDB=dbGetQuery(con,'SELECT * FROM ZOOPS_ABUND_BIOMASS')

underc=zoopDB[zoopDB$lakeID%in%c('BA','BE','BR','EL','WL','CR','HB','IN','MO','RB','RE'),]
undercYear=strftime(strptime(underc$dateSample,format="%Y-%m-%d %H:%M:%S"),format="%Y")
underc2011=underc[undercYear=="2011",]
underc2011=underc2011[underc2011$metadataID!="ZoopCounts.20110517",]
underc2011$abundance=underc2011$abundance/netEfficiency
underc2011$biomass=underc2011$abundance*underc2011$meanMass/1000000	#g dry mass m-2

lakes=sort(unique(underc2011$lakeID))
dates=sort(unique(underc2011$dateSample))

lakeXdate=matrix(NA,length(lakes),length(dates))
colnames(lakeXdate)=dates
rownames(lakeXdate)=lakes

for(i in 1:length(lakes)){
	cur=underc2011[underc2011$lakeID==lakes[i],]
	for(j in 1:length(dates)){	
		lakeXdate[i,j]=sum(cur$biomass[cur$dateSample==dates[j]])*1000	#mg dry mass m-2
	}
}

# aggregate sample dates as best as can be done
biomass=cbind(rowSums(lakeXdate[,1:3]),lakeXdate[,4],rowSums(lakeXdate[,5:7]),rowSums(lakeXdate[,8:9]),rowSums(lakeXdate[,10:13]),rowSums(lakeXdate[,14:15]),rowSums(lakeXdate[,16:18]),rowSums(lakeXdate[,19:21]),rowSums(lakeXdate[,22:23]),rowSums(lakeXdate[,24:25]),rowSums(lakeXdate[,26:27]),rowSums(lakeXdate[,28:29]),rowSums(lakeXdate[,30:32]))
# fill in some gaps with interpolation or otherwise
biomass[3,3]=70
biomass[5,2]=155
biomass[8,5]=125
biomass[9,3]=332
biomass[10,1:2]=10
biomass[11,2]=270
biomass[11,4]=185

LOGbiomass=log(biomass)

boxplot(t(LOGbiomass))

annMeans=rowMeans(LOGbiomass)

DOC=c(5.9,11.8,9.3,5.4,8,25.9,11.4,17.4,6.4,22,8)

plot(DOC,annMeans)
summary(lm(annMeans~DOC))
lines(5:25,(5:25)*-0.12327+7.5164,lwd=2)

at7=7*-0.12327+7.5164
at14=14*-0.12327+7.5164
ZOOPpercentChange=(at7-at14)/at7


DOCdiff=matrix(NA,nrow(biomass),nrow(biomass))
covMat=DOCdiff

for(i in 1:nrow(biomass)){
	for(j in 1:nrow(biomass)){
		DOCdiff[i,j]=abs(DOC[i]-DOC[j])
		covMat[i,j]=cov(LOGbiomass[i,],LOGbiomass[j,])
	}
}

summary(lm(covMat[lower.tri(covMat)]~DOCdiff[lower.tri(DOCdiff)]))	#no decrease of covariance with difference in DOC

plot(DOCdiff,covMat)
abline(h=covMat[11,5],lwd=2,lty=2,col='red')
abline(h=mean(covMat[lower.tri(covMat)]),lwd=2)
legend('topright',c('overall mean covar','long basin covar'),col=c('red','black'),lwd=2,lty=c(2,1),box.lty=0)

# WEST VS. EAST COVARIANCE IS SLIGHT LESS THAN THE MEAN PAIRWISE COVARIANCE OF SURVEY LAKES (0.56 VS. 0.63)


## SIMULATING DATA FROM DISTRIBUTIONS REPRESENTATIVE OF LONG DATA

# use log transformed to allow multivariate normal
library(mvtnorm)


muEL=mean(LOGbiomass[5,])
#sigmaEL=sd(LOGbiomass[5,])
muWL=mean(LOGbiomass[11,])
#sigmaWL=sd(LOGbiomass[11,])

#preCM=matrix(c(sigmaEL^2,sigmaEL*sigmaWL,sigmaEL*sigmaWL,sigmaWL^2),2,2,byrow=TRUE)
preCM=cov(cbind(LOGbiomass[5,],LOGbiomass[11,]))
postCM=preCM
postCM[1,2]=postCM[2,1]=0.1696
#postCM=matrix(c(sigmaEL^2,0.63,0.63,sigmaWL^2),2,2,byrow=TRUE)

# Testing RIA w/ distribution from 2011 zoop data
impact=0.9	#fold change resulting from intervention
length.pre=26
length.post=26

pre=rmvnorm(length.pre,c(muEL,muWL),sigma=preCM,method="svd")
post=rmvnorm(length.post,c(muEL*impact,muWL),sigma=postCM,method="svd")
colnames(pre)=colnames(post)=c('T','R')

quartz()
plot(c(pre[,2],post[,2]),type='o',ylim=range(rbind(pre,post)))
lines(c(pre[,1],post[,1]),lty=2)
points(c(pre[,1],post[,1]),pch=15)
legend('topleft',c("Reference","Treatment"),pch=c(21,15),lty=c(1,2),box.lty=0)

ria=RIA(pre[,2],pre[,1],post[,2],post[,1]); ria

# iterative data simulation and tests
Is=seq(0.75,0.95,0.05)
reps=500
storeRIA_P=matrix(NA,reps,length(Is))
colnames(storeRIA_P)=Is
for(i in 1:length(Is)){
	impact=Is[i]	#fold change resulting from intervention
	length.pre=26
	length.post=39

	for(j in 1:reps){
		pre=rmvnorm(length.pre,c(muEL,muWL),sigma=preCM,method="svd")
		post=rmvnorm(length.post,c(muEL*impact,muWL),sigma=postCM,method="svd")
		colnames(pre)=colnames(post)=c('T','R')
	
		pre[pre<0]=0.01
		post[post<0]=0.01
	
		storeRIA_P[j,i]=RIA(pre[,2],pre[,1],post[,2],post[,1])$p
	}
}

colSums(storeRIA_P<0.05)/reps

# w/ 26 and 26 observations we get: 1, 0.998, 0.958, 0.598, 0.102 for (0.75,0.8,0.85,0.9,0.95)
# w/ 26 and 39 observations we get: 1, 0.998, 0.98, 0.734, 0.136
# w/ 30 and 30 observations we get: 1, 1, 0.986, 0.72, 0.154
# w/ 30 and 60 observations we get: 1, 1, 1, 0.844, 0.224
# w/ 30 and 90 observations we get: 1, 1, 1, 0.9, 0.258


# TRY more resolved impacts betwen 0.85 and 0.9



# bugs
setwd("/Files/NDongoing/TerrestrialCarbon/statsForLongProposal_5-4-13/RIA")
bugs=read.table("LongBUGSdata_5-4-13.txt",header=TRUE,sep="\t")

m1=as.matrix(bugs[,c(1,4)])
m3=bugs[,c(2,5)]
m8=bugs[,c(3,6)]

bugsDepth=m1

meansDepth=colMeans(bugsDepth)
sigmaDepth=cov(bugsDepth)

impact=0.75#0.9	#fold change resulting from intervention
length.pre=12
length.post=12

#pre=rmvnorm(length.pre,meansDepth,sigma=sigmaDepth)
pre=bugsDepth
post=rmvnorm(length.post,c(meansDepth[1]*impact,meansDepth[2]),sigma=sigmaDepth)
colnames(pre)=colnames(post)=c('T','R')

pre[pre<0]=0.01
post[post<0]=0.01

ria=RIA(pre[,2],pre[,1],post[,2],post[,1]); ria


# iterative data simulation and tests
m1=as.matrix(bugs[,c(1,4)])
m3=bugs[,c(2,5)]
m8=bugs[,c(3,6)]

bugsDepth=m1

meansDepth=colMeans(bugsDepth)
sigmaDepth=cov(bugsDepth)

Is=seq(0.5,0.9,0.1)
reps=500
storeRIA_P=matrix(NA,reps,length(Is))
colnames(storeRIA_P)=Is

for(i in 1:length(Is)){
	impact=Is[i]	#fold change resulting from intervention
	length.pre=12
	length.post=12

	for(j in 1:reps){
		#pre=rmvnorm(length.pre,meansDepth,sigma=sigmaDepth,method="svd")
		pre=bugsDepth
		post=rmvnorm(length.post,c(meansDepth[1]*impact,meansDepth[2]),sigma=sigmaDepth,method="svd")
		colnames(pre)=colnames(post)=c('T','R')
	
		pre[pre<0]=0.01
		post[post<0]=0.01
	
		storeRIA_P[j,i]=RIA(pre[,2],pre[,1],post[,2],post[,1])$p
	}
}

colSums(storeRIA_P<0.05)/reps
#### depth: 1 m
# w/ 12 and 12 observations we get: 0.66, 0.57, 0.4, 0.34, 0.31 for (0.5,0.6,0.7,0.8,0.9)
# w/ 12 and 18 observations we get: 0.69, 0.55, 0.45, 0.28, 0.27
# w/ 12 and 24 observations we get: 0.77, 0.6, 0.45, 0.27, 0.2

#### depth: 3 m
# w/ 12 and 12 observations we get: 0.7, 0.41, 0.12, 0.02, 0.003 for (0.5,0.6,0.7,0.8,0.9)
# w/ 12 and 18 observations we get: 0.84, 0.50, 0.14, 0.016, 0
# w/ 12 and 24 observations we get: 0.92, 0.58, 0.15, 0.02, 0

#### depth: 8 m
# w/ 12 and 12 observations we get: 1,1,0.997,0.91,0.41 for (0.5,0.6,0.7,0.8,0.9)
# w/ 12 and 18 observations we get: 1, 1, 1, 0.96, 0.47
# w/ 12 and 24 observations we get: 1, 1, 1, 0.98, 0.47



####### hard to figure out how to do bivariate gamma with covariance: theory exists, but not straightforward R implementation

# estimate distribution to describe 2011 zoop data
quartz()
par(mfrow=c(1,2))
hist(elTotal2011)
scaleEL=var(elTotal2011)/mean(elTotal2011)
shapeEL=mean(elTotal2011)/scaleEL
lines(0:600,dgamma(0:600,shape=shapeEL,scale=scaleEL)*1000)
hist(wlTotal2011)
scaleWL=var(wlTotal2011)/mean(wlTotal2011)
shapeWL=mean(wlTotal2011)/scaleWL
lines(0:600,dgamma(0:600,shape=shapeWL,scale=scaleWL)*1000)

# Testing RIA w/ distribution from 2011 zoop data
impact=0.5	#fold change resulting from intervention
length.pre=22
length.post=22

preR=rgamma(length.pre,shape=shapeWL,scale=scaleWL)
postR=rgamma(length.post,shape=shapeWL,scale=scaleWL)
preT=rgamma(length.pre,shape=shapeEL,scale=scaleEL)
postT=rgamma(length.pre,shape=shapeEL*impact,scale=scaleEL)

quartz()
plot(c(preR,postR),type='o',ylim=c(0,max(c(preR,preT,postR,postT))))
lines(c(preT,postT),lty=2)
points(c(preT,postT),pch=15)

ria=RIA(preR,preT,postR,postT)