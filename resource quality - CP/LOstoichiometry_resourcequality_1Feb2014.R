#Long Lake Resource quality data

#C:P for 2011-2013
#load POC from the database
POC<-dbGetQuery(con,'SELECT * FROM POC')

#load particulate P data from the database
nutrients<-dbGetQuery(con,'SELECT * FROM NUTRIENTS')

#get units table
units<-dbGetQuery(con,'SELECT * FROM UNITS')
units[units$tableName=='POC',] #units for POC = mgC/L
units[units$tableName=='NUTRIENTS',] #units for particulate P = ug/L POM

#use just EL and WL data
LOpoc<-POC[POC$lakeID=='EL' | POC$lakeID=='WL',]

#use non-flagged data
LOpoc<-LOpoc[LOpoc$flag==0,]

#match data to add particulate P to C data
P<-c()
for(i in 1:nrow(LOpoc)){
	rowi=match(LOpoc$sampleID[i],nutrients$sampleID)
	P[i]=nutrients$particulateP[rowi]
}

LOpoc$partP=P

#calculate mols of C and P
LOpoc$molC<-(LOpoc$POC/1000)/12
LOpoc$molP<-(LOpoc$partP/10^6)/31

#calculate C:P
LOpoc$CP<-LOpoc$molC/LOpoc$molP

#add year to data
LOpoc<-addYear(LOpoc)

#use only PML samples
LOpmlPOC<-LOpoc[LOpoc$depthClass=='PML',]

LOpmlPOC<-LOpmlPOC[LOpmlPOC$CP<1000,]
#average C:P by year
tapply(LOpmlPOC$CP[LOpmlPOC$year==2011],LOpmlPOC$lakeID[LOpmlPOC$year==2011],mean,na.rm=T)

tapply(LOpmlPOC$CP[LOpmlPOC$year==2012],LOpmlPOC$lakeID[LOpmlPOC$year==2012],mean,na.rm=T)

plot(LOpmlPOC$CP[LOpmlPOC$lakeID=='EL']~as.Date(LOpmlPOC$dateSample[LOpmlPOC$lakeID=='EL'],'%Y-%m-%d %H:%M:%S'))
points(LOpmlPOC$CP[LOpmlPOC$lakeID=='WL']~as.Date(LOpmlPOC$dateSample[LOpmlPOC$lakeID=='WL'],'%Y-%m-%d %H:%M:%S'),col='red',pch=19)