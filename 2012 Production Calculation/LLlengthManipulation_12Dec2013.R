#Script converst pixel lengths and widths to lengths and widths in mm
#then appends to measurements made at a different time where lengths were measured at mm right away
#PTK *edited - 16 Dec 2013

#loads LO length data
LLlengths<-file.choose('~/Documents/Notre Dame/long lake data/csv files')
LLlengths<-read.csv(LLlengths)

#loads magnification conversions
mag<-file.choose('~/Documents/Notre Dame/long lake data/csv files')
mag<-read.csv(mag)

length<-c() #makes empty vector of lengths for conversions
for(i in 1:nrow(LLlengths)){ #matching loop to match mag conversions with magnifications in lengths file - then calculates length in mm
	rowi<-match(LLlengths$magnification[i],mag$magnification)
	length[i]<-LLlengths$lengthPixels[i]/mag$length_pixels[rowi]
}

LLlengths$length_mm<-length #adds lengths in mm to lengths data frame

width<-c() #does the same for the width data
for(i in 1:nrow(LLlengths)){
	rowi<-match(LLlengths$magnification[i],mag$magnification)
	width[i]<-LLlengths$widthPixels[i]/mag$length_pixels[rowi]
}

LLlengths$width_mm<-width #adds to length data frame

#load other LL length data to combine the two data frames
setwd('~/Documents/Notre Dame/Long Lake data/2012 Production Calculation/csv files')
LLmeas<-read.csv('LL_Measurements.csv')

colnames(LLmeas)<-c('lakeID','dateSample','taxa','length_mm','width_mm') #renames columns to match other data frame

LLlengths<-LLlengths[,-c(4,5,6)] #gets rid of unnecessary columns 

LLlengths<-rbind(LLmeas,LLlengths) #combines two data frames

write.csv(LLlengths,'LOlengths_FINAL_16Dec2013.csv') #saves file as csv