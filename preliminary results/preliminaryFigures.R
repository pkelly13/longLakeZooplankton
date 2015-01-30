#Early figures for Long Lake zooplankton
#Patrick Kelly 19 January 2014

library(ggplot2)
library(gridExtra)
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
el.zoops$year<-format(as.Date(el.zoops$dateSample,'%m/%d/%y'),'%Y')

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
tot.zoops$year<-format(as.Date(tot.zoops$dateSample,'%m/%d/%y'),'%Y')

#make a boxplot of total zooplankton biomass
boxplot(tot.zoops$ratio~tot.zoops$year,xlab='Year',ylab='EL:WL biomass',boxwex=0.5,cex.lab=1.2,cex.axis=1.2)
abline(h=1,lty=2,col='grey',lwd=2) #<-add red line at 1 to visualize where zooplankton biomass is greater in East than in West
#ratio of mean biomass to boxplot
el.mean<-tapply(tot.zoops$el.biomass.gm2,tot.zoops$year,mean)
wl.mean<-tapply(tot.zoops$wl.biomass.gm2,tot.zoops$year,mean)
ratio.mean<-el.mean/wl.mean
points(c(1,2,3,4),ratio.mean,pch=21,cex=2.5)


#Plante and Downing Production
prod<-read.csv('productionByTaxa_2011-2014.csv')

prod.daphnia<-prod[prod$taxa=='daphnia',]
daph.plot<-ggplot(data=prod.daphnia, aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge(),colour='black')+scale_fill_manual(values=c('EL'='black','WL'='white'))+theme_bw(base_size=14)+ylab(expression(paste('Production (g m'^-2,' yr'^-1,')')))+xlab('Year')+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+geom_hline(yintercept=0)+theme(legend.position='none')

prod.cyclopoid<-prod[prod$taxa=='cyclopoid',]
cyc.plot<-ggplot(data=prod.cyclopoid, aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge(),colour='black')+scale_fill_manual(values=c('EL'='black','WL'='white'))+theme_bw(base_size=14)+ylab(expression(paste('Production (g m'^-2,' yr'^-1,')')))+xlab('Year')+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+geom_hline(yintercept=0)+theme(legend.position='none')

prod.holopedium<-prod[prod$taxa=='holopedium',]
holo.plot<-ggplot(data=prod.holopedium, aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge(),colour='black')+scale_fill_manual(values=c('EL'='black','WL'='white'))+theme_bw(base_size=14)+ylab(expression(paste('Production (g m'^-2,' yr'^-1,')')))+xlab('Year')+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+geom_hline(yintercept=0)+theme(legend.position='none')

#Total production
tot.prod<-aggregate(prod$production.g.m2.yr,by=list(prod$lakeID,prod$year),sum,na.rm=T) 
colnames(tot.prod)<-c('lakeID','year','production.g.m2.yr')

tot.plot<-ggplot(data=tot.prod, aes(x=year,y=production.g.m2.yr,fill=lakeID))+geom_bar(stat='identity',position=position_dodge(),colour='black')+scale_fill_manual(values=c('EL'='black','WL'='white'))+theme_bw(base_size=14)+ylab(expression(paste('Production (g m'^-2,' yr'^-1,')')))+xlab('Year')+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+geom_hline(yintercept=0)+theme(legend.position='bottom')+annotate('text',x=2014,y=12,label='Total',size=7)


#combined plot of everything
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
myLegend<-g_legend(tot.plot)
test<-grid.arrange(arrangeGrob(daph.plot,cyc.plot,holo.plot,tot.plot+theme(legend.position='none'),nrow=2),myLegend,nrow=2,heights=c(10,1))