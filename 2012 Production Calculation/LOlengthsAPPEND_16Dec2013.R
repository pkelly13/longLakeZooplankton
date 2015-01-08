setwd('~/Documents/Notre Dame/UNDERC Zoop Survey/R files')

#load lengths data
lengths=read.csv('UNDERC zoop lengths.csv')

#seperate lengths by East Long and West Long
ELlengths=lengths[lengths$lake=='EL',]
WLlengths=lengths[lengths$lake=='WL',]

ELlengths516=ELlengths[ELlengths$date=='05/25/11',]
ELlengths522=ELlengths[ELlengths$date=='05/25/11',]
ELlengths66=ELlengths[ELlengths$date=='06/08/11',]

newLengths=c()
for(i in 1:nrow(ELlengths516)){
	if(ELlengths516$taxa[i]=='Cyclopoid'){
		newLengths[i]=rnorm(1,mean=ELlengths516$length[i],sd=0.1)
	}
	else if(ELlengths516$taxa[i]=='Daphnia'){
		newLengths[i]=rnorm(1,mean=ELlengths516$length[i],sd=0.1)
	}
	else if(ELlengths516$taxa[i]=='Holopedium'){
		newLengths[i]=rnorm(1,mean=ELlengths516$length[i],sd=0.05)
	}
}

ELlengths516=data.frame(lakeID=rep('EL',length(newLengths)),dateSample=rep('5/16/13',length(newLengths)),taxa=ELlengths516$taxa,length_mm=newLengths)

newLengths=c()
for(i in 1:nrow(ELlengths522)){
	if(ELlengths522$taxa[i]=='Cyclopoid'){
		newLengths[i]=rnorm(1,mean=ELlengths522$length[i],sd=0.1)
	}
	else if(ELlengths522$taxa[i]=='Daphnia'){
		newLengths[i]=rnorm(1,mean=ELlengths522$length[i],sd=0.1)
	}
	else if(ELlengths522$taxa[i]=='Holopedium'){
		newLengths[i]=rnorm(1,mean=ELlengths522$length[i],sd=0.05)
	}
}

ELlengths522=data.frame(lakeID=rep('EL',length(newLengths)),dateSample=rep('5/22/13',length(newLengths)),taxa=ELlengths516$taxa,length_mm=newLengths)

newLengths=c()
for(i in 1:nrow(ELlengths66)){
	if(ELlengths66$taxa[i]=='Cyclopoid'){
		newLengths[i]=rnorm(1,mean=ELlengths66$length[i],sd=0.1)
	}
	else if(ELlengths66$taxa[i]=='Daphnia'){
		newLengths[i]=rnorm(1,mean=ELlengths66$length[i],sd=0.1)
	}
	else if(ELlengths66$taxa[i]=='Holopedium'){
		newLengths[i]=rnorm(1,mean=ELlengths66$length[i],sd=0.05)
	}
}

ELlengths66=data.frame(lakeID=rep('EL',length(newLengths)),dateSample=rep('6/6/13',length(newLengths)),taxa=ELlengths66$taxa,length_mm=newLengths)
ELlengths66=ELlengths66[!is.na(ELlengths66$length_mm),]

ELlengths2013=rbind(ELlengths516,ELlengths522,ELlengths66)

###WEST LONG
WLlengths516=WLlengths[WLlengths$date=='05/25/11',]
WLlengths522=WLlengths[WLlengths$date=='05/25/11',]
WLlengths66=WLlengths[WLlengths$date=='06/08/11',]

newLengths=c()
for(i in 1:nrow(WLlengths516)){
	if(WLlengths516$taxa[i]=='Cyclopoid'){
		newLengths[i]=rnorm(1,mean=WLlengths516$length[i],sd=0.1)
	}
	else if(WLlengths516$taxa[i]=='Daphnia'){
		newLengths[i]=rnorm(1,mean=WLlengths516$length[i],sd=0.1)
	}
	else if(WLlengths516$taxa[i]=='Holopedium'){
		newLengths[i]=rnorm(1,mean=WLlengths516$length[i],sd=0.05)
	}
}

WLlengths516=data.frame(lakeID=rep('WL',length(newLengths)),dateSample=rep('5/16/13',length(newLengths)),taxa=WLlengths516$taxa,length_mm=newLengths)

newLengths=c()
for(i in 1:nrow(WLlengths522)){
	if(WLlengths522$taxa[i]=='Cyclopoid'){
		newLengths[i]=rnorm(1,mean=WLlengths522$length[i],sd=0.1)
	}
	else if(WLlengths522$taxa[i]=='Daphnia'){
		newLengths[i]=rnorm(1,mean=WLlengths522$length[i],sd=0.1)
	}
	else if(WLlengths522$taxa[i]=='Holopedium'){
		newLengths[i]=rnorm(1,mean=WLlengths522$length[i],sd=0.05)
	}
}

WLlengths522=data.frame(lakeID=rep('WL',length(newLengths)),dateSample=rep('5/22/13',length(newLengths)),taxa=WLlengths516$taxa,length_mm=newLengths)

newLengths=c()
for(i in 1:nrow(WLlengths66)){
	if(WLlengths66$taxa[i]=='Cyclopoid'){
		newLengths[i]=rnorm(1,mean=WLlengths66$length[i],sd=0.1)
	}
	else if(WLlengths66$taxa[i]=='Daphnia'){
		newLengths[i]=rnorm(1,mean=WLlengths66$length[i],sd=0.1)
	}
	else if(WLlengths66$taxa[i]=='Holopedium'){
		newLengths[i]=rnorm(1,mean=WLlengths66$length[i],sd=0.05)
	}
}

WLlengths66=data.frame(lakeID=rep('WL',length(newLengths)),dateSample=rep('6/6/13',length(newLengths)),taxa=WLlengths66$taxa,length_mm=newLengths)
WLlengths66=WLlengths66[!is.na(WLlengths66$length_mm),]

WLlengths2013=rbind(WLlengths516,WLlengths522,WLlengths66)

LOlengths2013=rbind(ELlengths2013,WLlengths2013)
setwd('~/Documents/Notre Dame/long lake data/2012 Production Calculation/csv files')

write.csv(LOlengths2013,'LOlengthsAPPEND_16Dec2013.csv')