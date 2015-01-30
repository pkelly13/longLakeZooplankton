#Figure of zooplankton production by taxa and total

library(ggplot2)
library(gridExtra)
#start wth figure for Stuart -> ratio of East:West long biomass - just using the mean, by year
#load zooplankton data
setwd('~/Documents/Notre Dame/long lake data/FINAL_data')

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