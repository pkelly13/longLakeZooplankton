#Preliminary results for Long Lake, plot biomass through time, average biomass and abundance
#Patrick Kelly 26 October 2014

#load data
setwd('~/Documents/Notre Dame/Long Lake Data/preliminary results')

zoop.data<-read.csv('zoopData2011_2014.csv')
zoop.data$year<-format(as.Date(zoop.data$dateSample,'%Y-%m-%d'),'%Y')
#use only Long data
zoop.data<-zoop.data[zoop.data$lakeID=='EL' | zoop.data$lakeID=='WL',]

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


#Calculate zooplankton production using Plante and Downing equation
#load zooplankton biomass
setwd('~/Documents/Notre Dame/long lake data/FINAL_data')

zoops<-read.csv('zoopData2011_2014.csv')
zoops$biomass.gm2<-zoops$biomass_gDryMass_m3*zoops$depthBottom
zoops$year<-format(as.Date(zoops$dateSample,'%Y-%m-%d'),'%Y')
#only EL and WL
zoops<-zoops[zoops$lakeID=='EL' | zoops$lakeID=='WL',]

#calculate zooplankton production
#load zooplankton lengths
lengths.past<-dbGetQuery(con,'SELECT length.lakeID,length.dateSample,length.taxa,length.length,length.mass FROM ZOOPS_LENGTHS AS length')
#Use only East and West Long
lengths.past<-lengths.past[lengths.past$lakeID=='EL' | lengths.past$lakeID=='WL',]
#add year
lengths.past<-addYear(lengths.past)

#load 2014 data
setwd('~/Documents/Notre Dame/long lake data/2014 long lake data/csv files')
lengths.2014<-read.csv('LLlengths2014.csv')
lw.regression<-read.csv('LWregressions.csv')

#calculate mass by taxa
mass_ug=c()
for(i in 1:nrow(lengths.2014)){
	rowi=match(lengths.2014$taxa[i],lw.regression$taxa)
	mass_ug[i]=exp((lw.regression$b[rowi]+(lw.regression$m[rowi]*log(lengths.2014$length.mm[i]))))
}
lengths.2014$mass_mg=(mass_ug/1000)
lengths.2014$year<-format(as.Date(lengths.2014$dateSample,'%m/%d/%y'),'%Y')

#get temperature data
temp.past<-dbGetQuery(con,'SELECT temp.lakeID,temp.dateSample,temp.depthBottom,temp.temp FROM LIMNO_PROFILES AS temp')
#add year
temp.past<-addYear(temp.past)

#use only East and West Long data
temp.past<-temp.past[temp.past$lakeID=='EL' | temp.past$lakeID=='WL',]
temp.past<-temp.past[temp.past$depthBottom==0,]

#get 2014 temperature data
setwd('~/Documents/Notre Dame/long lake data/covariate data')
temp.2014<-read.csv('Limno Profiles Log 2014.csv')
#use only East and West Long data
temp.2014<-temp.2014[temp.2014$lakeID=='EL' | temp.2014$lakeID=='WL',]
temp.2014<-temp.2014[temp.2014$Depth..m.==0,]

#calculate production by taxa for East and West Long and for each year
#2011
#East Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2011 & zoops$lakeID=='EL'],zoops$taxa[zoops$year==2011 & zoops$lakeID=='EL'],mean,na.rm=T)
max.mass.mg<-tapply(lengths.past$mass[lengths.past$year==2011 & lengths.past$lakeID=='EL'],lengths.past$taxa[lengths.past$year==2011 & lengths.past$lakeID=='EL'],max,na.rm=T)/1000
max.mass.mg<-max.mass.mg[-c(3,5)]
temp<-mean(temp.past$temp[temp.past$year==2011 & temp.past$lakeID=='EL'])
production.EL.2011<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.EL.2011<-10^production.EL.2011
#West Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2011 & zoops$lakeID=='WL'],zoops$taxa[zoops$year==2011 & zoops$lakeID=='WL'],mean,na.rm=T)
max.mass.mg<-tapply(lengths.past$mass[lengths.past$year==2011 & lengths.past$lakeID=='WL'],lengths.past$taxa[lengths.past$year==2011 & lengths.past$lakeID=='WL'],max,na.rm=T)/1000
max.mass.mg<-max.mass.mg[-c(1,2)]
temp<-mean(temp.past$temp[temp.past$year==2011 & temp.past$lakeID=='WL'])
production.WL.2011<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.WL.2011<-10^production.WL.2011

#2012
#East Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2012 & zoops$lakeID=='EL' & zoops$projectID==3],zoops$taxa[zoops$year==2012 & zoops$lakeID=='EL' & zoops$projectID==3],mean,na.rm=T)
max.mass.mg<-tapply(lengths.past$mass[lengths.past$year==2012 & lengths.past$lakeID=='EL'],lengths.past$taxa[lengths.past$year==2012 & lengths.past$lakeID=='EL'],max,na.rm=T)/1000
temp<-mean(temp.past$temp[temp.past$year==2012 & temp.past$lakeID=='EL'])
production.EL.2012<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.EL.2012<-10^production.EL.2012
#West Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2012 & zoops$lakeID=='WL' & zoops$projectID==3],zoops$taxa[zoops$year==2012 & zoops$lakeID=='WL' & zoops$projectID==3],mean,na.rm=T)
max.mass.mg<-tapply(lengths.past$mass[lengths.past$year==2012 & lengths.past$lakeID=='WL'],lengths.past$taxa[lengths.past$year==2012 & lengths.past$lakeID=='WL'],max,na.rm=T)/1000
temp<-mean(temp.past$temp[temp.past$year==2012 & temp.past$lakeID=='WL'])
production.WL.2012<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.WL.2012<-10^production.WL.2012

#2013
#East Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2013 & zoops$lakeID=='EL'],zoops$taxa[zoops$year==2013 & zoops$lakeID=='EL'],mean,na.rm=T)
max.mass.mg<-tapply(lengths.past$mass[lengths.past$year==2013 & lengths.past$lakeID=='EL'],lengths.past$taxa[lengths.past$year==2013 & lengths.past$lakeID=='EL'],max,na.rm=T)/1000
temp<-mean(temp.past$temp[temp.past$year==2013 & temp.past$lakeID=='EL'])
production.EL.2013<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.EL.2013<-10^production.EL.2013
#West Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2013 & zoops$lakeID=='WL'],zoops$taxa[zoops$year==2013 & zoops$lakeID=='WL'],mean,na.rm=T)
max.mass.mg<-tapply(lengths.past$mass[lengths.past$year==2013 & lengths.past$lakeID=='WL'],lengths.past$taxa[lengths.past$year==2013 & lengths.past$lakeID=='WL'],max,na.rm=T)/1000
temp<-mean(temp.past$temp[temp.past$year==2013 & temp.past$lakeID=='WL'])
production.WL.2013<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.WL.2013<-10^production.WL.2013

#2014
#East Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2014 & zoops$lakeID=='EL'],zoops$taxa[zoops$year==2014 & zoops$lakeID=='EL'],mean,na.rm=T)
max.mass.mg<-tapply(lengths.2014$mass_mg[lengths.2014$lakeID=='EL'],lengths.2014$taxa[lengths.2014$lakeID=='EL'],mean,na.rm=T)
max.mass.mg<-max.mass.mg[-3]
temp<-mean(temp.2014$temp[temp.2014$Lake.ID=='EL'])
production.EL.2014<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.EL.2014<-10^production.EL.2014
#West Long
avg.mass.gm2<-tapply(zoops$biomass.gm2[zoops$year==2014 & zoops$lakeID=='WL'],zoops$taxa[zoops$year==2014 & zoops$lakeID=='WL'],mean,na.rm=T)
max.mass.mg<-tapply(lengths.2014$mass_mg[lengths.2014$lakeID=='WL'],lengths.2014$taxa[lengths.2014$lakeID=='WL'],mean,na.rm=T)
max.mass.mg<-max.mass.mg[-1]
temp<-mean(temp.2014$temp[temp.2014$Lake.ID=='WL'])
production.WL.2014<-0.06+(0.79*log(avg.mass.gm2,10))-(0.16*log(max.mass.mg,10))+(0.05*temp)
production.WL.2014<-10^production.WL.2014

#make comprehensive data frame of zooplankton production data
production.2011<-data.frame(lakeID=c(rep('EL',length(production.EL.2011)),rep('WL',length(production.WL.2011))),taxa=c(rownames(production.EL.2011),rownames(production.WL.2011)),production.g.m2.yr=c(production.EL.2011,production.WL.2011),year=rep(2011,length(c(production.EL.2011,production.WL.2011))))
production.2012<-data.frame(lakeID=c(rep('EL',length(production.EL.2012)),rep('WL',length(production.WL.2012))),taxa=c(rownames(production.EL.2012),rownames(production.WL.2012)),production.g.m2.yr=c(production.EL.2012,production.WL.2012),year=rep(2012,length(c(production.EL.2012,production.WL.2012))))
production.2013<-data.frame(lakeID=c(rep('EL',length(production.EL.2013)),rep('WL',length(production.WL.2013))),taxa=c(rownames(production.EL.2013),rownames(production.WL.2013)),production.g.m2.yr=c(production.EL.2013,production.WL.2013),year=rep(2013,length(c(production.EL.2013,production.WL.2013))))
production.2014<-data.frame(lakeID=c(rep('EL',length(production.EL.2014)),rep('WL',length(production.WL.2014))),taxa=c(rownames(production.EL.2014),rownames(production.WL.2014)),production.g.m2.yr=c(production.EL.2014,production.WL.2014),year=rep(2014,length(c(production.EL.2014,production.WL.2014))))
production<-rbind(production.2011,production.2012,production.2013,production.2014)

#write this to a csv file in final data
setwd('~/Documents/Notre Dame/long lake data/FINAL_data')
write.csv(production,'productionByTaxa_2011-2014.csv')

#Taxon-specific production
#figure out %copepod production
#make uniqueID from lake-year
prod$uniqueID<-paste(prod$lakeID,prod$year,sep='.')
uniques<-unique(prod$uniqueID)
percent.prod<-c()
for(i in 1:length(uniques)){
	lakei<-prod[prod$uniqueID==uniques[i],]
	tot<-sum(lakei$production.g.m2.yr)
	x<-c()
	for(j in 1:nrow(lakei)){
		x[j]<-lakei$production.g.m2.yr[j]/tot
	}
	percent.prod<-c(percent.prod,x)
}
prod$percent.prod<-percent.prod


#plots of taxa-specific production by year
cyclopoid.data<-prod[prod$taxa=='cyclopoid',]
ggplot(data=cyclopoid.data,aes(x=year,y=percent.prod,fill=lakeID))+geom_bar(stat='identity',position=position_dodge())+scale_fill_manual(values=c('EL'='blue','WL'='brown'))

daphnia.data<-prod[prod$taxa=='daphnia',]
ggplot(data=daphnia.data, aes(x=year,y=percent.prod,fill=lakeID))+geom_bar(stat='identity',position=position_dodge())+scale_fill_manual(values=c('EL'='blue','WL'='brown'))

holopedium.data<-prod[prod$taxa=='holopedium',]
ggplot(data=holopedium.data, aes(x=year,y=percent.prod,fill=lakeID))+geom_bar(stat='identity',position=position_dodge())+scale_fill_manual(values=c('EL'='blue','WL'='brown'))

el.data<-prod[prod$lakeID=='EL',]
ggplot(data=el.data, aes(x=year,y=percent.prod,fill=taxa))+geom_bar(stat='identity')

plot(prod$year[prod$lakeID=='EL' & prod$taxa=='cyclopoid'],prod$percent.prod[prod$lakeID=='EL' & prod$taxa=='cyclopoid'],ylim=c(0.1,0.6),pch=19,cex=1.3,col='brown',ylab='% production',xlab='year',xaxt='n')
points(prod$year[prod$lakeID=='WL' & prod$taxa=='cyclopoid'],prod$percent.prod[prod$lakeID=='WL' & prod$taxa=='cyclopoid'],ylim=c(0,1),pch=1,cex=1.3,col='blue')
points(jitter(prod$year[prod$lakeID=='EL' & prod$taxa=='daphnia']),prod$percent.prod[prod$lakeID=='EL' & prod$taxa=='daphnia'],ylim=c(0,1),pch=15,cex=1.3,col='brown')
points(prod$year[prod$lakeID=='WL' & prod$taxa=='daphnia'],prod$percent.prod[prod$lakeID=='WL' & prod$taxa=='daphnia'],ylim=c(0,1),pch=0,cex=1.3,col='blue')
points(prod$year[prod$lakeID=='EL' & prod$taxa=='holopedium'],prod$percent.prod[prod$lakeID=='EL' & prod$taxa=='holopedium'],ylim=c(0,1),pch=17,cex=1.3,col='brown')
points(prod$year[prod$lakeID=='WL' & prod$taxa=='holopedium'],prod$percent.prod[prod$lakeID=='WL' & prod$taxa=='holopedium'],ylim=c(0,1),pch=2,cex=1.3,col='blue')
axis(1,at=c(2011,2012,2013,2014))


#RIA on chaobs
#load chaoborus data
setwd('~/Documents/Notre Dame//long lake data/Chaoborus data')
chaobs<-read.csv('chaoborusDataLongLake2011-2014.csv')

#add year
chaobs$year<-format(as.Date(chaobs$dateSample,'%m/%d/%y'),'%Y')
boxplot(chaobs$g.m2~chaobs$lakeID*chaobs$year,col=rep(c('brown','blue'),4))
