#preliminary look at zooplankton isotope data for Long Lake 2011-2013
#Patrick Kelly 14 Jan 2015

#load POC isotopes 2011-2012
results<-dbGetQuery(con,'SELECT iso.isotopeID,iso.d13C,iso.d15N,iso.d2H,iso.flag FROM ISOTOPE_RESULTS AS iso')

samples<-dbGetQuery(con,'SELECT iso.isotopeID,iso.lakeID,iso.dateSample,iso.depthClass,iso.sampleType FROM ISOTOPE_SAMPLES_POC AS iso')

#match isotope data with sample information
d13C<-c()
d15N<-c()
d2H<-c()
flag<-c()
for(i in 1:nrow(samples)){
	rowi<-match(samples$isotopeID[i],results$isotopeID)
	d13C[i]<-results$d13C[rowi]
	d15N[i]<-results$d15N[rowi]
	d2H[i]<-results$d2H[rowi]
	flag[i]<-results$flag[rowi]
}
samples$d13C<-d13C
samples$d15N<-d15N
samples$d2H<-d2H
samples$flag=flag

#use only East and West unflagged data
lo.samples<-samples[samples$lakeID=='EL' | samples$lakeID=='WL',]
lo.samples<-lo.samples[lo.samples$flag==0,]
#remove NAs
lo.samples<-lo.samples[!is.na(lo.samples$flag),]

#load 2013 data
setwd('~/Documents/Notre Dame/long lake data/POC')
lo.2013<-read.csv('2013UNDERC_POCwSampleInfo.csv')