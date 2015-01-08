#Data anlysis for Long Lake production for 2012
#Patrick Kelly
#20 Dec 2013

#load data
setwd('~/Documents/Notre Dame/Long Lake Data/2012 Production Calculation/csv files')

lengthsFile='LOlengths_FINAL_16Dec2013.csv'
countFile='LOcounts_FINAL_16Dec2013.csv'
subsampleFile='LOsubsamples_FINAL_16Dec2013.csv'
regressionFile='LWregressions.csv'

lengths=read.csv(lengthsFile)
counts=read.csv(countFile)
subsampleWeights=read.csv(subsampleFile)
regressionCofs=read.csv(regressionFile)


#calculate mass for each zoop
mass_ug=c()
for(i in 1:nrow(lengths)){
	rowi=match(lengths$taxa[i],regressionCofs$taxa)
	mass_ug[i]=exp((regressionCofs$b[rowi]+(regressionCofs$m[rowi]*log(lengths$length_mm[i]))))
}
lengths$mass_ug=mass_ug

#calculate subsample multipliers
subsampleMult=subsampleWeights$weightSubsample/(subsampleWeights$weightJarFull-subsampleWeights$weightJarEmpty)
subsampleWeights$subsampleMult=subsampleMult

#calculate counts per m2
#make uniqueID for both counts and subsamples data frames that consists of lake and date
uniqueCount<-paste(counts$lake,counts$date,sep='')
uniqueSub<-paste(subsampleWeights$lake,subsampleWeights$sampleDate,sep='')

counts$uniqueID<-uniqueCount
subsampleWeights$uniqueID<-uniqueSub

#divide counts by their corresponding multipliers
sampleCount<-c()
for(i in 1:nrow(counts)){
	rowi<-match(counts$uniqueID[i],subsampleWeights$uniqueID)
	sampleCount[i]<-counts$counts[i]/subsampleWeights$subsampleMult[rowi]
}
counts$sampleCount=sampleCount

#convert sample counts to counts per m2
counts$countsPerM2<-(((counts$sampleCount/2)/0.07297)/0.25)

#add average lengths to count data
#make unique IDs for both length and count data that is lakedatetaxa
lengths$uniqueID<-paste(lengths$lakeID,lengths$dateSample,lengths$taxa,sep='')
counts$uniqueID<-paste(counts$lake,counts$date,counts$taxa,sep='')

#make data frame of average mass by uniqueId's
avgMass<-tapply(lengths$mass_ug,lengths$uniqueID,mean,na.rm=T)
avgMass<-data.frame(uniqueID=rownames(avgMass),mass_ug=avgMass)
rownames(avgMass)=NULL

#calculate mass per M2 in count data
mass_ugPerM2<-c()
for(i in 1:nrow(counts)){
	rowi<-match(counts$uniqueID[i],avgMass$uniqueID)
	mass_ugPerM2[i]<-counts$countsPerM2[i]*avgMass$mass_ug[rowi]
}
counts$mass_ugPerM2<-mass_ugPerM2

#find mass in grams per m2
counts$mass_gM2<-counts$mass_ugPerM2/1E6

zoopData2012<-counts #save as this for zoop biomass data analysis
zoopData2012$Jdate<-strptime(zoopData2012$date,'%m/%d/%y')$yday+1 #add julian date

#find average biomass per taxa per lake
ELtaxaBiomass=tapply(counts$mass_gM2[counts$lake=='EL'], counts$taxa[counts$lake=='EL'],mean,na.rm=T)
WLtaxaBiomass=tapply(counts$mass_gM2[counts$lake=='WL'], counts$taxa[counts$lake=='WL'],mean,na.rm=T)

#find maximum biomass in mg per taxa for each lake
ELmaxBiomass=tapply(lengths$mass_ug[lengths$lake=='EL'],lengths$taxa[lengths$lake=='EL'],max,na.rm=T)/1000
WLmaxBiomass=tapply(lengths$mass_ug[lengths$lake=='WL'],lengths$taxa[lengths$lake=='WL'],max,na.rm=T)/1000

#get water temperature for EL and WL for 2012
temp<-dbGetQuery(con,'SELECT profs.lakeID, profs.dateSample, profs.depthTop, profs.depthBottom, profs.temp FROM LIMNO_PROFILES AS profs')
temp$year<-format(as.Date(temp$dateSample,'%Y-%m-%d %H:%M:%S'),format<-'%Y')

LOtemp<-temp[temp$lakeID=='EL' | temp$lakeID=='WL' & temp$year=='2012',] #use just East and West Long data from 2012
#average surface temps for East and West basins
ELtemp<-mean(LOtemp$temp[LOtemp$lakeID=='EL' & LOtemp$depthTop==0])
WLtemp<-mean(LOtemp$temp[LOtemp$lakeID=='WL' & LOtemp$depthTop==0])

#Calculate production using Plante and Downing 1980-whatever 
ELprod=0.06+(0.79*log(ELtaxaBiomass[-2],10))-(0.16*log(ELmaxBiomass[-2],10))+(0.05*ELtemp)
WLprod=0.06+(0.79*log(WLtaxaBiomass[-1],10))-(0.16*log(WLmaxBiomass[-c(1,2)],10))+(0.05*WLtemp)
ELprod2012=10^ELprod
WLprod2012=10^WLprod
