#Script to calculate taxa specific production for East and West Long
#PTK 19 Nov 2013

#loads data from the database
setwd('~/Documents/Notre Dame/database')
library(RSQLite)

drv=SQLite() #create driver object

con=dbConnect(drv,dbname="M2Mdb_081313.db") #open database connection

dbListTables(con)

dbListFields(con,'LIMNO_PROFILES')

zoopBiomass=dbGetQuery(con,'SELECT zab.projectID, zab.lakeID, zab.dateSample, zab.depthBottom, zab.taxa, zab.count, zab.meanMass_ug, zab.abundance_num_m3, zab.biomass_gDryMass_m3 FROM ZOOPS_ABUND_BIOMASS AS zab') #selects zooplankton count and average biomass data

zoopLengths=dbGetQuery(con,'SELECT zl.projectID, zl.lakeID, zl.dateSample, zl.taxa, zl.mass FROM ZOOPS_LENGTHS AS zl') #selects zooplankton mass data

temps=dbGetQuery(con, 'SELECT profs.lakeID, profs.dateSample, profs.depthTop, profs.depthBottom, profs.temp FROM LIMNO_PROFILES AS profs') #selects temperature data

zoopBiomass$year=format(as.Date(zoopBiomass$dateSample,'%Y-%m-%d %H:%M:%S'),'%Y') #makes a year column for each data table
zoopLengths$year=format(as.Date(zoopLengths$dateSample,'%Y-%m-%d %H:%M:%S'),'%Y')
temps$year=format(as.Date(temps$dateSample,'%Y-%m-%d %H:%M:%S'),'%Y')

zoopBiomass=zoopBiomass[zoopBiomass$year==2012,]
