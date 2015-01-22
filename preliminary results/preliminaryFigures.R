#Early figures for Long Lake zooplankton
#Patrick Kelly 19 January 2014

#start wth figure for Stuart -> ratio of East:West long biomass - just using the mean, by year
#load zooplankton data
setwd('~/Documents/Notre Dame/long lake data/FINAL_data')

zoops<-read.csv('zoopData2011_2014.csv')

#use only east and west long
zoops<-zoops[zoops$lakeID=='EL' | zoops$lakeID=='WL',]

#matchup by taxa and look at ratios of east long:west long biomass
#make uniqueID for taxa and date
zoops$uniqueID<-paste(zoops$taxa,zoops$dateSample,sep='.')
#I like dealing with biomass_gm-2 rather than g m-3, add that column
zoops$biomass.gm2<-zoops$biomass_gDryMass_m3*zoops$depthBottom

#make data frame of east long and west long
el.zoops<-zoops[zoops$lakeID=='EL',]
wl.zoops<-zoops[zoops$lakeID=='WL',]

#match east and west long to make ratio of biomass
wl.biomass.gm2<-c()
for(i in 1:nrow(el.zoops)){
	rowi<-match(el.zoops$uniqueID[i],wl.zoops$uniqueID)
	wl.biomass.gm2[i]<-wl.zoops$biomass.gm2[rowi]
}
el.zoops$wl.biomass.gm2<-wl.biomass.gm2
#calculate ratio
el.zoops$ratio<-el.zoops$biomass.gm2/el.zoops$wl.biomass.gm2

#add year to data frame
el.zoops$year<-format(as.Date(el.zoops$dateSample,'%Y-%m-%d'),'%Y')

#Make boxplots for taxa-specific biomass differences, also do total zooplankton biomass
#Daphnia
boxplot(el.zoops$ratio[el.zoops$taxa=='daphnia']~el.zoops$year[el.zoops$taxa=='daphnia'],log='y',main='Daphnia',xlab='year',ylab='ratio of EL:WL biomass')
boxplot(el.zoops$ratio[el.zoops$taxa=='daphnia']~el.zoops$year[el.zoops$taxa=='daphnia'],main='Daphnia',xlab='year',ylab='ratio of EL:WL biomass') #non-logged y axis - shows outlier
boxplot(el.zoops$ratio[el.zoops$taxa=='daphnia']~el.zoops$year[el.zoops$taxa=='daphnia'],main='Daphnia',xlab='year',ylab='ratio of EL:WL biomass',ylim=c(0,20)) #zoom in

#Cyclopoids
boxplot(el.zoops$ratio[el.zoops$taxa=='cyclopoid']~el.zoops$year[el.zoops$taxa=='cyclopoid'],main='Cyclopoid',xlab='year',ylab='ratio of EL:WL biomass')

#Holopedium
boxplot(el.zoops$ratio[el.zoops$taxa=='holopedium']~el.zoops$year[el.zoops$taxa=='holopedium'],main='Holopedium',xlab='year',ylab='ratio of EL:WL biomass')

#try for total zooplankton biomass
#aggregate data frame by lake and date
tot.zoops<-aggregate(cbind(el.zoops$biomass.gm2,el.zoops$wl.biomass.gm2)~el.zoops$dateSample,FUN=sum,na.rm=T)
colnames(tot.zoops)<-c('dateSample','el.biomass.gm2','wl.biomass.gm2')
#calculate ratio of EL:WL
tot.zoops$ratio<-tot.zoops$el.biomass.gm2/tot.zoops$wl.biomass.gm2
#add year to make the graph
tot.zoops$year<-format(as.Date(tot.zoops$dateSample,'%Y-%m-%d'),'%Y')

#make a boxplot of total zooplankton biomass
boxplot(tot.zoops$ratio~tot.zoops$year,xlab='year',ylab='EL:WL biomass',main='total zooplankton')
abline(h=1,lty=2,col='red') #<-add red line at 1 to visualize where zooplankton biomass is greater in East than in West

#Plante and Downing Production
prod<-read.csv('productionByTaxa_2011-2014.csv')

prod.daphnia<-prod[prod$taxa=='daphnia',]
ggplot(data=prod.daphnia, aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge())+scale_fill_manual(values=c('EL'='brown','WL'='blue'))+ylab(expression(paste('production (g m'^-2,' yr'^-1,')')))

prod.cyclopoid<-prod[prod$taxa=='cyclopoid',]
ggplot(data=prod.cyclopoid, aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge())+scale_fill_manual(values=c('EL'='brown','WL'='blue'))+ylab(expression(paste('production (g m'^-2,' yr'^-1,')')))

prod.holopedium<-prod[prod$taxa=='holopedium',]
ggplot(data=prod.holopedium, aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge())+scale_fill_manual(values=c('EL'='brown','WL'='blue'))+ylab(expression(paste('production (g m'^-2,' yr'^-1,')')))

#Total production
tot.prod<-aggregate(prod$production.g.m2.yr,by=list(prod$lakeID,prod$year),sum,na.rm=T) 
colnames(tot.prod)<-c('lakeID','year','production.g.m2.yr')

#cyclopoid
cyclopoid.data<-prod[prod$taxa=='cyclopoid',]
ggplot(data=tot.prod,aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge())+scale_fill_manual(values=c('EL'='brown','WL'='blue'))+ylab(expression(paste('production (g m'^-2,' yr'^-1,')')))