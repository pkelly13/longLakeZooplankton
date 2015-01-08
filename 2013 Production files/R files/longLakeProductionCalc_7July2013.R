#Early data anlysis for Long Lake production for 2013
#Patrick Kelly
#2 July 2013

#load data
setwd('~/Documents/Notre Dame/Long Lake Data/2013 Production files/R files')

lengthsFile='longLakeZoopLengths2013_2July.csv'
countFile='ZoopCounts2013_2July.csv'
subsampleFile='zoopSubsampleWeight2013_2July.csv'
regressionFile='LWregressions.csv'

lengths=read.csv(lengthsFile)
counts=read.csv(countFile)
subsampleWeights=read.csv(subsampleFile)
regressionCofs=read.csv(regressionFile)

#calculate mass for each zoop
mass_ug=c()
for(i in 1:nrow(lengths)){
	rowi=match(lengths$taxa[i],regressionCofs$taxa)
	mass_ug[i]=exp((regressionCofs$b[rowi]+(regressionCofs$m[rowi]*log(lengths$length.mm[i]))))
}
lengths$mass_ug=mass_ug

#calculate subsample multipliers
subsampleMult=subsampleWeights$weight.subsample/(subsampleWeights$weight.jar.full-subsampleWeights$weight.empty.jar)
subsampleWeights$subsampleMult=subsampleMult

#calculate counts per m2
for(i in 1:nrow(counts)){
	rowi=match(counts$Zoop.ID[i],subsampleWeights$Zoop.ID)
	counts$sampleCount[i]=counts$Counts[i]/subsampleWeights$subsampleMult[rowi]
}	

counts$countsPerM2=(((counts$sampleCount/2)/0.07297)/0.25)

#add average lengths to count data
lengths$uniqueID=paste(lengths$sampleNum,lengths$taxa,sep='')
avgBiomass=tapply(lengths$mass_ug,lengths$uniqueID,mean,na.rm=T)

#make data frame with uniqueIDs and averages
avgMass=data.frame(uniqueID=rownames(avgBiomass),avgBiomass)
rownames(avgMass)=NULL

#make unique IDs for count data
counts$uniqueID=paste(counts$Zoop.ID,counts$Taxa,sep='')

#match counts and lengths
for(i in 1:nrow(counts)){
	rowi=match(counts$uniqueID[i],avgMass$uniqueID)
	counts$avgBiomass_ug[i]=avgMass$avgBiomass[rowi]
}

#use just East and West Long
LOcounts=counts[counts$Lake.ID=='EL' | counts$Lake.ID=='WL',]

#find mass in grams per m2
LOcounts$totalBiomass_gM2=LOcounts$countsPerM2*(LOcounts$avgBiomass_ug/10^6)


#find average biomass per taxa per lake
ELtaxaBiomass=tapply(LOcounts$totalBiomass_gM2[LOcounts$Lake.ID=='EL'], LOcounts$Taxa[LOcounts$Lake.ID=='EL'],mean,na.rm=T)
WLtaxaBiomass=tapply(LOcounts$totalBiomass_gM2[LOcounts$Lake.ID=='WL'], LOcounts$Taxa[LOcounts$Lake.ID=='WL'],mean,na.rm=T)

#find maximum biomass per taxa for each lake
ELmaxBiomass=tapply(lengths$mass_ug[lengths$lakeID=='EL'],lengths$taxa[lengths$lakeID=='EL'],max,na.rm=T)
WLmaxBiomass=tapply(lengths$mass_ug[lengths$lakeID=='WL'],lengths$taxa[lengths$lakeID=='WL'],max,na.rm=T)

#Calculate production using Plante and Downing 1980-whatever 
ELprod=0.06+(0.79*log(ELtaxaBiomass[-6],10))-(0.16*log(ELmaxBiomass[-3]/1000,10))+(0.05*21)
WLprod=0.06+(0.79*log(WLtaxaBiomass[-6],10))-(0.16*log(WLmaxBiomass[-3]/1000,10))+(0.05*21)
ELprod=10^ELprod
WLprod=10^WLprod
