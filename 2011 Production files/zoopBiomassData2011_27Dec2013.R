#data prep for Long Lake biomass data analysis
#PTK 27 Dec 2013

#load data from database
zoopData<-dbGetQuery(con, 'SELECT zoop.lakeID,zoop.dateSample,zoop.depthBottom,zoop.taxa,zoop.count,zoop.meanMass_ug,zoop.abundance_num_m3,zoop.biomass_gDryMass_m3 FROM ZOOPS_ABUND_BIOMASS AS zoop')

#add year
zoopData$year<-format(as.Date(zoopData$dateSample,'%Y-%m-%d %H:%M:%S'),format='%Y')

#use just EL and WL 2011 data
zoopData2011<-zoopData[zoopData$lakeID=='EL' | zoopData$lakeID=='WL' & zoopData$year=='2011',]

#convert abundance from counts per m3 to counts per m2
zoopData2011$abundance_num_m2<-zoopData2011$abundance_num_m3/zoopData2011$depthBottom

#calculate biomass per m2 in ug
zoopData2011$mass_ug_m2<-zoopData2011$abundance_num_m2*zoopData2011$meanMass_ug

#add julian date to data
zoopData2011$Jdate<-strptime(zoopData2011$dateSample,'%Y-%m-%d %H:%M:%S')$yday+1