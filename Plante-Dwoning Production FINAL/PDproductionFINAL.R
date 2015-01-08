#Plant and Downing production model for Long Lake data using data from the database
#Patrick Kelly 27 October 2014

####Need to change this so it is all data####
#get long lake data from database
zoops.2011.2013<-dbGetQuery(con,'SELECT zoops.lakeID, zoops.dateSample, zoops.depthBottom, zoops.taxa, zoops.biomass_gDryMass_m3 FROM ZOOPS_ABUND_BIOMASS AS zoops')
zoops.lengths.2011.2013<-dbGetQuery(con,'SELECT lengths.lakeID, lengths.dateSample, lengths.taxa, lengths.mass FROM ZOOPS_LENGTHS AS lengths')

#add year to data
zoops.2011.2013<-addYear(zoops.2011.2013)
lengths.2011.2013<-addYear(zoops.lengths.2011.2013)

#fix "copepod" taxa names
lengths.2011.2013$taxa<-sub('copepod','cyclopoid',lengths.2011.2013$taxa)

#get temp data
temp.data<-dbGetQuery(con, 'SELECT profs.lakeID, profs.dateSample, profs.depthTop, profs.depthBottom, profs.temp FROM LIMNO_PROFILES AS profs')
temp.data<-addYear(temp.data)

#average biomass by taxa for each year, find maximum biomass for each taxa for each year, and get average water temperature to input into PD model
#2011
EL.avg.biomass.2011<-tapply(zoops.2011.2013$biomass_gDryMass_m3[zoops.2011.2013$lakeID=='EL' & zoops.2011.2013$year==2011]*zoops.2011.2013$depthBottom[zoops.2011.2013$lakeID=='EL' & zoops.2011.2013$year==2011],zoops.2011.2013$taxa[zoops.2011.2013$lakeID=='EL' & zoops.2011.2013$year==2011],mean,na.rm=T)
WL.avg.biomass.2011<-tapply(zoops.2011.2013$biomass_gDryMass_m3[zoops.2011.2013$lakeID=='WL' & zoops.2011.2013$year==2011]*zoops.2011.2013$depthBottom[zoops.2011.2013$lakeID=='WL' & zoops.2011.2013$year==2011],zoops.2011.2013$taxa[zoops.2011.2013$lakeID=='WL' & zoops.2011.2013$year==2011],mean,na.rm=T)

EL.max.biomass.2011<-tapply(lengths.2011.2013$mass[lengths.2011.2013$lakeID=='EL' & lengths.2011.2013$year==2011],lengths.2011.2013$taxa[lengths.2011.2013$lakeID=='EL' & lengths.2011.2013$year==2011],mean,na.rm=T)[-3]/1000
WL.max.biomass.2011<-tapply(lengths.2011.2013$mass[lengths.2011.2013$lakeID=='WL' & lengths.2011.2013$year==2011],lengths.2011.2013$taxa[lengths.2011.2013$lakeID=='WL' & lengths.2011.2013$year==2011],mean,na.rm=T)[-1]/1000 #remove chaoborus

EL.surfaceTemp.2011<-mean(temp.data$temp[temp.data$lakeID=='EL' & temp.data$year==2011 & temp.data$depthBottom==0])
WL.surfaceTemp.2011<-mean(temp.data$temp[temp.data$lakeID=='WL' & temp.data$year==2011 & temp.data$depthBottom==0])

ELprod.2011<-10^(0.06+(0.79*log(EL.avg.biomass.2011,10))-(0.16*log(EL.max.biomass.2011,10))+(0.05*EL.surfaceTemp.2011))
WLprod.2011<-10^(0.06+(0.79*log(WL.avg.biomass.2011,10))-(0.16*log(WL.max.biomass.2011,10))+(0.05*WL.surfaceTemp.2011))

#2012
EL.avg.biomass.2012<-tapply(zoops.2011.2013$biomass_gDryMass_m3[zoops.2011.2013$lakeID=='EL' & zoops.2011.2013$year==2012]*zoops.2011.2013$depthBottom[zoops.2011.2013$lakeID=='EL' & zoops.2011.2013$year==2012],zoops.2011.2013$taxa[zoops.2011.2013$lakeID=='EL' & zoops.2011.2013$year==2012],mean,na.rm=T)
WL.avg.biomass.2012<-tapply(zoops.2011.2013$biomass_gDryMass_m3[zoops.2011.2013$lakeID=='WL' & zoops.2011.2013$year==2012]*zoops.2011.2013$depthBottom[zoops.2011.2013$lakeID=='WL' & zoops.2011.2013$year==2012],zoops.2011.2013$taxa[zoops.2011.2013$lakeID=='WL' & zoops.2011.2013$year==2012],mean,na.rm=T)

EL.max.biomass.2012<-tapply(lengths.2011.2013$mass[lengths.2011.2013$lakeID=='EL' & lengths.2011.2013$year==2012],lengths.2011.2013$taxa[lengths.2011.2013$lakeID=='EL' & lengths.2011.2013$year==2012],mean,na.rm=T)/1000
WL.max.biomass.2012<-tapply(lengths.2011.2013$mass[lengths.2011.2013$lakeID=='WL' & lengths.2011.2013$year==2012],lengths.2011.2013$taxa[lengths.2011.2013$lakeID=='WL' & lengths.2011.2013$year==2012],mean,na.rm=T)/1000 #remove chaoborus

EL.surfaceTemp.2012<-mean(temp.data$temp[temp.data$lakeID=='EL' & temp.data$year==2012 & temp.data$depthBottom==0])
WL.surfaceTemp.2012<-mean(temp.data$temp[temp.data$lakeID=='WL' & temp.data$year==2012 & temp.data$depthBottom==0])

ELprod.2012<-10^(0.06+(0.79*log(EL.avg.biomass.2012,10))-(0.16*log(EL.max.biomass.2012,10))+(0.05*EL.surfaceTemp.2012))
WLprod.2012<-10^(0.06+(0.79*log(WL.avg.biomass.2012,10))-(0.16*log(WL.max.biomass.2012,10))+(0.05*WL.surfaceTemp.2012))
