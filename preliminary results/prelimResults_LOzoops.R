#Preliminary results for Long Lake, plot biomass through time, average biomass and abundance
#Patrick Kelly 26 October 2014

#load data
setwd('~/Documents/Notre Dame/Long Lake Data/preliminary results')

zoop.data<-read.csv('zoopData2011_2014.csv')
zoop.data$year<-format(as.Date(zoop.data$dateSample,'%Y-%m-%d'),'%Y')
#use only Long data
zoop.data<-zoop.data[zoop.data$lakeID=='EL' | zoop.data$lakeID=='WL',]

#plot areal abundance
zoop.data$abundance_m2<-zoop.data$abundance_num_m3*zoop.data$depthBottom
zoop.data$biomass_g_m2<-(zoop.data$abundance_m2*zoop.data$meanMass_ug)/1000000

#Daphnia
plot(as.Date(zoop.data$dateSample[zoop.data$lakeID=='WL' &zoop.data$taxa=='daphnia'],'%Y-%m-%d'),zoop.data$abundance_m2[zoop.data$lakeID=='WL' &zoop.data$taxa=='daphnia'],xlab='',ylab=expression(paste('Daphnia m'^-2)))
points(as.Date(zoop.data$dateSample[zoop.data$lakeID=='EL' &zoop.data$taxa=='daphnia'],'%Y-%m-%d'),zoop.data$abundance_m2[zoop.data$lakeID=='EL' &zoop.data$taxa=='daphnia'],pch=19)
legend('topright',pch=c(1,19),legend=c('WL','EL'))

#Cyclopoids
plot(as.Date(zoop.data$dateSample[zoop.data$lakeID=='WL' &zoop.data$taxa=='cyclopoid'],'%Y-%m-%d'),zoop.data$abundance_m2[zoop.data$lakeID=='WL' &zoop.data$taxa=='cyclopoid'],xlab='',ylab=expression(paste('Cyclopoids m'^-2)))
points(as.Date(zoop.data$dateSample[zoop.data$lakeID=='EL' &zoop.data$taxa=='cyclopoid'],'%Y-%m-%d'),zoop.data$abundance_m2[zoop.data$lakeID=='EL' &zoop.data$taxa=='cyclopoid'],pch=19)
legend('topright',pch=c(1,19),legend=c('WL','EL'))

#Holopedium
plot(as.Date(zoop.data$dateSample[zoop.data$lakeID=='WL' &zoop.data$taxa=='holopedium'],'%Y-%m-%d'),zoop.data$abundance_m2[zoop.data$lakeID=='WL' &zoop.data$taxa=='holopedium'],xlab='',ylab=expression(paste('Holopedium m'^-2)))
points(as.Date(zoop.data$dateSample[zoop.data$lakeID=='EL' &zoop.data$taxa=='holopedium'],'%Y-%m-%d'),zoop.data$abundance_m2[zoop.data$lakeID=='EL' &zoop.data$taxa=='holopedium'],pch=19)
legend('topright',pch=c(1,19),legend=c('WL','EL'))

#plot average biomass by date and basin
plot(as.Date(zoop.data$dateSample[zoop.data$lakeID=='WL' &zoop.data$taxa=='daphnia'],'%Y-%m-%d'),zoop.data$meanMass_ug[zoop.data$lakeID=='WL' &zoop.data$taxa=='daphnia'],xlab='',ylab=expression(paste('Daphnia m'^-2)))
points(as.Date(zoop.data$dateSample[zoop.data$lakeID=='EL' &zoop.data$taxa=='daphnia'],'%Y-%m-%d'),zoop.data$meanMass_ug[zoop.data$lakeID=='EL' &zoop.data$taxa=='daphnia'],pch=19)
legend('topright',pch=c(1,19),legend=c('WL','EL'))

#Cyclopoids
plot(as.Date(zoop.data$dateSample[zoop.data$lakeID=='WL' &zoop.data$taxa=='cyclopoid'],'%Y-%m-%d'),zoop.data$meanMass_ug[zoop.data$lakeID=='WL' &zoop.data$taxa=='cyclopoid'],xlab='',ylab=expression(paste('Cyclopoids m'^-2)))
points(as.Date(zoop.data$dateSample[zoop.data$lakeID=='EL' &zoop.data$taxa=='cyclopoid'],'%Y-%m-%d'),zoop.data$meanMass_ug[zoop.data$lakeID=='EL' &zoop.data$taxa=='cyclopoid'],pch=19)
legend('topright',pch=c(1,19),legend=c('WL','EL'))

#Holopedium
plot(as.Date(zoop.data$dateSample[zoop.data$lakeID=='WL' &zoop.data$taxa=='holopedium'],'%Y-%m-%d'),zoop.data$meanMass_ug[zoop.data$lakeID=='WL' &zoop.data$taxa=='holopedium'],xlab='',ylab=expression(paste('Holopedium m'^-2)),ylim=c(0,60))
points(as.Date(zoop.data$dateSample[zoop.data$lakeID=='EL' &zoop.data$taxa=='holopedium'],'%Y-%m-%d'),zoop.data$meanMass_ug[zoop.data$lakeID=='EL' &zoop.data$taxa=='holopedium'],pch=19)
legend('topright',pch=c(1,19),legend=c('WL','EL'))

boxplot(zoop.data$meanMass_ug[zoop.data$taxa=='daphnia' & zoop.data$lakeID=='WL'],zoop.data$meanMass_ug[zoop.data$taxa=='daphnia' & zoop.data$lakeID=='EL'])


#match samples by date east and west
#make unique ID that is date-taxa
zoop.data$uniqueID<-paste(zoop.data$dateSample,zoop.data$taxa,sep='_')
EL.data<-zoop.data[zoop.data$lakeID=='EL',]
WL.data<-zoop.data[zoop.data$lakeID=='WL',]

WL.biomass<-c()
for(i in 1:nrow(EL.data)){
	rowi<-match(EL.data$uniqueID[i],WL.data$uniqueID)
	WL.biomass[i]<-WL.data$biomass_g_m2[rowi]
}
EL.data$WL.biomass<-WL.biomass

#plots of differences in biomass (g m-2) though time
plot(as.Date(EL.data$dateSample[EL.data$taxa=='cyclopoid'],'%Y-%m-%d'),EL.data$biomass_g_m2[EL.data$taxa=='cyclopoid']-EL.data$WL.biomass[EL.data$taxa=='cyclopoid'],ylab='EL cyclopoid biomass - WL cyclopoid biomass',xlab='Date')
abline(lm((EL.data$biomass_g_m2[EL.data$taxa=='cyclopoid']-EL.data$WL.biomass[EL.data$taxa=='cyclopoid'])~as.Date(EL.data$dateSample[EL.data$taxa=='cyclopoid'],'%Y-%m-%d')))

quartz()
plot(as.Date(EL.data$dateSample[EL.data$taxa=='daphnia'],'%Y-%m-%d'),EL.data$biomass_g_m2[EL.data$taxa=='daphnia']-EL.data$WL.biomass[EL.data$taxa=='daphnia'],ylab='EL daphnia biomass - WL daphnia biomass',xlab='Date')
abline(lm((EL.data$biomass_g_m2[EL.data$taxa=='daphnia']-EL.data$WL.biomass[EL.data$taxa=='daphnia'])~as.Date(EL.data$dateSample[EL.data$taxa=='daphnia'],'%Y-%m-%d')))

quartz()
plot(as.Date(EL.data$dateSample[EL.data$taxa=='holopedium'],'%Y-%m-%d'),EL.data$biomass_g_m2[EL.data$taxa=='holopedium']-EL.data$WL.biomass[EL.data$taxa=='holopedium'],ylab='EL holopedium biomass - WL holopedium biomass',xlab='Date')
abline(lm((EL.data$biomass_g_m2[EL.data$taxa=='holopedium']-EL.data$WL.biomass[EL.data$taxa=='holopedium'])~as.Date(EL.data$dateSample[EL.data$taxa=='holopedium'],'%Y-%m-%d')))

#RIA
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

#Cyclopoids
RIA(pre1=EL.data$biomass_g_m2[EL.data$taxa=='cyclopoid' & EL.data$year==2011 | EL.data$year==2012],pre2=EL.data$WL.biomass[EL.data$taxa=='cyclopoid' & EL.data$year==2011 | EL.data$year==2012],post1=EL.data$biomass_g_m2[EL.data$taxa=='cyclopoid' & EL.data$year==2013 | EL.data$year==2014], post2=EL.data$WL.biomass[EL.data$taxa=='cyclopoid' & EL.data$year==2013 | EL.data$year==2014])

#Daphnia
RIA(pre1=EL.data$biomass_g_m2[EL.data$taxa=='daphnia' & EL.data$year==2011 | EL.data$year==2012],pre2=EL.data$WL.biomass[EL.data$taxa=='daphnia' & EL.data$year==2011 | EL.data$year==2012],post1=EL.data$biomass_g_m2[EL.data$taxa=='daphnia' & EL.data$year==2013 | EL.data$year==2014], post2=EL.data$WL.biomass[EL.data$taxa=='daphnia' & EL.data$year==2013 | EL.data$year==2014])

#Holopedium
RIA(pre1=EL.data$biomass_g_m2[EL.data$taxa=='holopedium' & EL.data$year==2011 | EL.data$year==2012],pre2=EL.data$WL.biomass[EL.data$taxa=='holopedium' & EL.data$year==2011 | EL.data$year==2012],post1=EL.data$biomass_g_m2[EL.data$taxa=='holopedium' & EL.data$year==2013 | EL.data$year==2014], post2=EL.data$WL.biomass[EL.data$taxa=='holopedium' & EL.data$year==2013 | EL.data$year==2014])