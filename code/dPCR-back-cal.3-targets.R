##dPCR Data back calculation Pipeline 3-Plex##
#####Author: KB
#####Revision: HWu 11/17/22

## File path in, current location of the .csv file
FilePathIn <-"Desktop/test/SARS_N1N2.81922.1ato7a.raw.csv"

##File Path out, include desired file name with .csv designation
FilePathOut <-"Desktop/test/SARS-CoV-2-#1a-7a.csv"

#Experimental Parameters in uL
dPCRPrepVolume = 10
NucleicAcidVol = 5
NucleicAcidExtracVol = 80
##Sample in mL
SampleExtracVol = 35

##Enter all parameters, hit control+a to select entire window, then click Run

#####################################
#####################################
##Variable names coded for ease of use
b <-dPCRPrepVolume
c <-NucleicAcidVol
d <-NucleicAcidExtracVol
e <-SampleExtracVol
x <-FilePathIn
y <-FilePathOut

##Pull Data into environment
rawdata=read.csv(file=x)
##Create Function in environment
testfunc <- function(z,b,c,d,e) {
  testfunc <- (((z*((b*d)/(c*e)))))
  return(testfunc)
}

###Munging V2
#Load necessary library
library(tidyverse)

#Tibble and remove summary row
tibbleplay<-(as_tibble(rawdata))
notop<-tibbleplay[c(-1),]

#Create a Subset with only necessary data (none of the generated summary stat columns)
concfam<-notop[1:16,c(1:8,9:15)]
conchex<-notop[1:16,c(1:8,16:22)]
conccy5<-notop[1:16,c(1:8,23:29)]

##Must rename subsets to match mother set to allow for rbind to work##
newnames=c("Run","Date","Instrument","Plate","Group","Name","Well","Total","Dye","Target","Conc.cp.uL","SD","CV.","X95.CI","Positives")
colnames(concfam)<-newnames
colnames(conchex)<-newnames
colnames(conccy5)<-newnames

##Stack the freshly re-named subsets
stacked=rbind(concfam,conchex,conccy5)
stacked$Conc.cp.uL<-sapply(stacked$Conc.cp.uL, as.numeric)
summary(stacked)

##Munging complete, now in same format as single plex##
#######################################################

##Execute function in environment ##This is gonna get tripled##
#Results <- testfunc(rawdata$##NAME OF dPCR Output Column##,a,b,c)
results <- testfunc(stacked$Conc.cp.uL,b,c,d,e)
formattedresults<-format(results, scientific = TRUE)
"FinalConc-cp/ml"<- formattedresults
unformattedresults<-results*1000
"FinalConc-cp/L"<-format(unformattedresults,scientific=TRUE)

##Create output table
#Final<-data.frame(rawdata$SpeciemenNameColumn,rawdata$NameofOutputColumn,Results)
output=cbind(stacked,`FinalConc-cp/ml`,`FinalConc-cp/L`)

##Summary statistics##
###Need to figure out how to do paired averages without having to make a bazillion subgroups##

#Check for those will less than 3 wells (in positives column) here
QualityCheck<-ifelse(output$Well>=20000& output$Conc.cp.uL >=0.34,"Pass","Fail")
outputqc=cbind(output,QualityCheck)

##Arrange by given names to make grouping easier
finaloutput<-arrange(outputqc,Name)

##Save data to Specified out
write.csv(finaloutput,file= y,row.names=FALSE)
