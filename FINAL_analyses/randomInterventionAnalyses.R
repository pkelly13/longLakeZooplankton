#Random intervention analysis on zooplankton data - both taxa specific as well as total zooplankton biomass - using FINAL data
#Patrick Kelly 26 January 2015

#load RIA script
setwd('~/Documents/useful R scripts')
source('RIA.R')

#load zooplantkton data
setwd('~/Documents/Notre Dame/long lake data/FINAL_data')

zoop.data<-read.csv('zoopData2011_2014.csv')

#use only East and west Long data
zoop.data<-zoop.data[zoop.data$lakeID=='EL' | zoop.data$lakeID=='WL',]

#make biomass in g m-2
zoop.data$biomass_g_m2<-(zoop.data$abundance_num_m3*zoop.data$depthBottom)*(zoop.data$meanMass_ug/1000000)

#add year
zoop.data$year<-format(as.Date(zoop.data$dateSample,'%Y-%m-%d'),'%Y')

#make a new data frame of matching data
#make uniqueID of date.taxa
zoop.data$uniqueID<-paste(zoop.data$dateSample,zoop.data$taxa,sep='.')

#seperate data frame into east and west frames
el.data<-zoop.data[zoop.data$lakeID=='EL',]
wl.data<-zoop.data[zoop.data$lakeID=='WL',]

#match unique IDs 
wl.biomass<-c()
for(i in 1:nrow(el.data)){
	rowi<-match(el.data$uniqueID[i],wl.data$uniqueID)
	wl.biomass[i]<-wl.data$biomass_g_m2[rowi]
}
el.data$wl.biomass<-wl.biomass
zoop.data<-el.data[,c(4,6,10,11,18,21)]
colnames(zoop.data)[c(5,6)]<-c('el.biomass','wl.biomass')

zoop.data<-zoop.data[zoop.data$dateSample!='2014-06-25',]
zoop.data<-zoop.data[zoop.data$dateSample!='2011-06-15',]
zoop.data$wl.biomass[is.na(zoop.data$wl.biomass)]=0
#add year
zoop.data$year<-format(as.Date(zoop.data$dateSample,'%Y-%m-%d'),'%Y')

#run RIA on specific zooplankton taxa
#Daphnia
RIA(pre1=zoop.data$el.biomass[zoop.data$taxa=='daphnia' & (zoop.data$year==2011 | zoop.data$year==2012)],pre2=zoop.data$wl.biomass[zoop.data$taxa=='daphnia' & (zoop.data$year==2011 | zoop.data$year==2012)],post1=zoop.data$el.biomass[zoop.data$taxa=='daphnia' & (zoop.data$year==2013 | zoop.data$year==2014)],post2=zoop.data$wl.biomass[zoop.data$taxa=='daphnia' & (zoop.data$year==2013 | zoop.data$year==2014)],n.iter=5000) #p=0.0418

#Cyclopoid
RIA(pre1=zoop.data$el.biomass[zoop.data$taxa=='cyclopoid' & (zoop.data$year==2011 | zoop.data$year==2012)],pre2=zoop.data$wl.biomass[zoop.data$taxa=='cyclopoid' & (zoop.data$year==2011 | zoop.data$year==2012)],post1=zoop.data$el.biomass[zoop.data$taxa=='cyclopoid' & (zoop.data$year==2013 | zoop.data$year==2014)],post2=zoop.data$wl.biomass[zoop.data$taxa=='cyclopoid' & (zoop.data$year==2013 | zoop.data$year==2014)],n.iter=5000) #p=0.177

#Holopedium
RIA(pre1=zoop.data$el.biomass[zoop.data$taxa=='holopedium' & (zoop.data$year==2011 | zoop.data$year==2012)],pre2=zoop.data$wl.biomass[zoop.data$taxa=='holopedium' & (zoop.data$year==2011 | zoop.data$year==2012)],post1=zoop.data$el.biomass[zoop.data$taxa=='holopedium' & (zoop.data$year==2013 | zoop.data$year==2014)],post2=zoop.data$wl.biomass[zoop.data$taxa=='holopedium' & (zoop.data$year==2013 | zoop.data$year==2014)],n.iter=5000) #p=0.0372

#Make aggregated data frame for total zooplankton biomass
ag.data<-aggregate(cbind(el.biomass,wl.biomass)~year+dateSample,data=zoop.data,sum,na.rm=T)

RIA(pre1=ag.data$el.biomass[ag.data$year==2011 | ag.data$year==2012],pre2=ag.data$wl.biomass[ag.data$year==2011 | ag.data$year==2012],post1=ag.data$el.biomass[ag.data$year==2013 | ag.data$year==2014],post2=ag.data$wl.biomass[ag.data$year==2013 | ag.data$year==2014],n.iter=5000) #p=0.0382

#Perform RIA on Chaoborus as well
#load chaob data
setwd('~/Documents/Notre Dame/long lake data/chaoborus data')
chaobs<-read.csv('chaoborusDataLongLake2011-2014.csv') 

#make seperate EL and WL data frames
el.chaobs<-chaobs[chaobs$lakeID=='EL',]
wl.chaobs<-chaobs[chaobs$lakeID=='WL',]

#match wl to el data
wl<-c()
for(i in 1:nrow(el.chaobs)){
	rowi<-match(el.chaobs$dateSample[i],wl.chaobs$dateSample)
	wl[i]<-wl.chaobs$g.m2[rowi]
}
el.chaobs$wl.biomass<-wl

#replace NA with 0
el.chaobs$wl.biomass[is.na(el.chaobs$wl.biomass)]=0

#add year
el.chaobs$year<-format(as.Date(el.chaobs$dateSample,'%m/%d/%y'),'%Y')

RIA(pre1=el.chaobs$g.m2[el.chaobs$year==2011 | el.chaobs$year==2012],pre2=el.chaobs$wl.biomass[el.chaobs$year==2011 | el.chaobs$year==2012],post1=el.chaobs$g.m2[el.chaobs$year==2013 | el.chaobs$year==2014],post2=el.chaobs$wl.biomass[el.chaobs$year==2013 | el.chaobs$year==2014],n.iter=5000) #p=0.2846