#this script determines algae 13C and 15N numbers from C:N of terrestrial vegetation, algae, and POM
#Patrick Kelly 21 January 2015

#First need to get C:N data of POM for 2011-2014
#get POM C:N data from covariate data table
setwd('~/Documents/Notre Dame/long lake data/covariate data')
POM<-read.csv('pocStoich_2011-2014.csv')

#get fracAlgae from C:N of POM, leaves, and algae....leaves and algae from Francis et al. (2011) and from Stuart's code.  Might need to look through the literature and find other ranges of these umbers and do sensitivity analysis
leavesCN<-35.5
algaeCN<-6.8
POM$fracAlgae<-(leavesCN-POM$CN)/(leavesCN-algaeCN)

#Now load isotope data from the database
pom.iso<-dbGetQuery(con,'SELECT iso.isotopeID,iso.lakeID,iso.dateSample,iso.depthClass FROM ISOTOPE_SAMPLES_POC AS iso')
results<-dbGetQuery(con,'SELECT iso.isotopeID,iso.d13C,iso.d15N FROM ISOTOPE_RESULTS AS iso')

#match samples and put d13C and d15N data into pom.iso
iso.data<-c()
for(i in 1:nrow(pom.iso)){
	rowi<-match(pom.iso$isotopeID[i],results$isotopeID)
	d13C<-results$d13C[rowi]
	d15N<-results$d15N[i]
	x<-data.frame(pom.iso[i,],d13C,d15N)
	iso.data<-rbind(iso.data,x)
}
#use only East and West long data
iso.data<-iso.data[iso.data$lakeID=='EL' | iso.data$lakeID=='WL',]

#pull in 2013 and 2014 isotope data
setwd('~/Documents/Notre Dame/long lake data/isotopes')
iso.2013.2014<-read.csv('POCisotopes_2013-2014.csv')
#make data match the old data
iso.2013.2014<-iso.2013.2014[,c(3,23,22,25,17,9)]

#fix iso.data dates to match iso.2013.2014
iso.data$dateSample<-format(as.Date(iso.data$dateSample,'%Y-%m-%d %H:%M:%S'),'%m/%d/%Y')

#combine old and new isotope data
colnames(iso.2013.2014)[1]='isotopeID'
iso.data<-rbind(iso.data,iso.2013.2014)

#get rid of samples where d15N is 0
iso.data<-iso.data[iso.data$d15N!=0,]
iso.data<-iso.data[iso.data$d13C!=0,]

#match POM fracAlage to isotope data
#make unique IDs for both first
POM$uniqueID<-paste(POM$lakeID,POM$dateSample,POM$depthClass,sep='.')
iso.data$uniqueID<-paste(iso.data$lakeID,iso.data$dateSample,iso.data$depthClass,sep='.')

fracAlgae<-c()
for(i in 1:nrow(iso.data)){
	rowi<-match(iso.data$uniqueID[i],POM$uniqueID)
	fracAlgae[i]<-POM$fracAlgae[rowi]
}
iso.data$fracAlgae<-fracAlgae

#calculate d13C and d15N of algae from linear mixing model
tPOC_C<--24.401 #Taken from IRMS sample of tPOC dog buckets by Zwart
tPOC_N<--3.303 #ditto
iso.data$d13Calgae<-(iso.data$d13C-(1-iso.data$fracAlgae)*-tPOC_C)/iso.data$fracAlgae #these all seem way too depleted, but I'll go with it
iso.data$d15Nalgae<-(iso.data$d15N-(1-iso.data$fracAlgae)*-tPOC_N)/iso.data$fracAlgae

#get zooplankton data
dbListFields(con,'ISOTOPE_SAMPLES_ZOOPS')