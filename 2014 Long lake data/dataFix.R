setwd('~/Documents/Notre Dame/Katie Baglini Data Spring 2011/R data')

lengths<-read.csv('ZoopLengths2011.csv')
LWregression<-read.csv('LWregressions2.csv')

mass<-c()
for(i in 1:nrow(lengths)){
	rowi<-match(lengths$taxa[i],LWregression$taxa)
	mass[i]<-exp(LWregression$b[rowi]+(LWregression$m[rowi]*log(lengths$length[i])))
}
lengths$mass.ug<-mass

wl<-lengths[lengths$lake=='WL' & lengths$date=='06/15/11',]

tapply(wl$mass,wl$taxa,mean)

setwd('~/Documents/Notre Dame/long lake data/2013 Production files')
lengths<-read.xlsx('longLakeZoopLengths.xlsx',1)

setwd('~/Documents/Notre Dame/long lake data/2013 Production files/R files')
LWregression<-read.csv('LWregressions.csv')

mass<-c()
for(i in 1:nrow(lengths)){
	rowi<-match(lengths$taxa[i],LWregression$taxa)
	mass[i]<-exp(LWregression$b[rowi]+(LWregression$m[rowi]*log(lengths$length[i])))
}
lengths$mass.ug<-mass

el<-lengths[lengths$lakeID=='EL' & lengths$dateSample=='2013-07-10',]

tapply(el$mass.ug,el$taxa,mean,na.rm=T)

setwd('~/Documents/Notre Dame/long lake data/2014 Long Lake data')
lengths<-read.xlsx('LLlengths2014.xlsx',1)
lengths$taxa<-tolower(lengths$taxa)

mass<-c()
for(i in 1:nrow(lengths)){
	rowi<-match(lengths$taxa[i],LWregression$taxa)
	mass[i]<-exp(LWregression$b[rowi]+(LWregression$m[rowi]*log(lengths$length[i])))
}
lengths$mass.ug<-mass

el<-lengths[lengths$lakeID=='EL',]
tapply(el$mass.ug,el$taxa,mean,na.rm=T)

wl<-lengths[lengths$lakeID=='WL',]
tapply(wl$mass.ug,wl$taxa,mean,na.rm=T)