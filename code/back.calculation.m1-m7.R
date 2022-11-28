# Author: KB
# Updated:HWu 11/26/22
##dPCR Data back calculation Pipeline
library(tidyverse)
library(dplyr)

## File path in, current location of the .csv file
FilePathIn <-"/Users/huiyunwu/Downloads/MS2-m1-m7-hwu-Quantification.csv"
x<-FilePathIn
##File Path out, include desired file name with .csv designation
FilePathOut <-"/Users/huiyunwu/Desktop/R/R.back.cal./NOLA sample/M1-M7.MS2.csv"
y<-FilePathOut
######Load sample volume#####
Vol.<-read.csv(file = "NOLA sample/Vol.M11toM7.csv")
summary(Vol.)
Vol.$Sampling.Date<-as.Date(Vol.$Sampling.Date, format = "%m/%d/%y")
Vol.$Vol.ml<-as.numeric(Vol.$Vol.ml)
Vol.$N1<-as.numeric(Vol.$N1)
Vol.$N2<-as.numeric(Vol.$N2)
Vol.$PMMoV<-as.numeric(Vol.$PMMoV)
Vol.$M.pox<-as.numeric(Vol.$M.pox)

# colnames(Vol.)[1]<-"Name"
# colnames(Vol.)[5]<-"Effluent.vol.ul"
# colnames(Vol.)[4]<-"Influent.vol.ml"
summary(Vol.)
colnames(Vol.)
Vol<-Vol.[,c(8:11,13)]
Vol<-na.omit(Vol)
summary(Vol)
Vol$Name<-c("m1","m2","m3","m4","m5","m6","m7")
##Enter all parameters, hit control+a to select entire window, then click Run

#####################################


##Pull Data into environment
rawdata=read.csv(file=x)
colnames(rawdata)

#####generate full dataset for back calculation###
back.cal<-left_join(rawdata,Vol)
dPCRPrepVol.ul<- 10
dPCRNAVol.ul<-5
NAExntVol.ul<- 80
back.cal$finalcon.cp.L<-(back.cal$Conc..cp.uL*dPCRPrepVol.ul)/dPCRNAVol.ul*NAExntVol.ul/back.cal$Vol.ml*1000
back.cal$finalcon.cp.L<-format(back.cal$finalcon.cp.L,scientific = TRUE)
colnames(back.cal)
# [1] "Run"           "Date"          "Instrument"    "Plate"         "Group"        
# [6] "Name"          "Well"          "Total"         "Dye"           "Target"       
# [11] "Conc..cp.uL"   "SD"            "CV."           "X95.CI"        "Positives"    
# [16] "N1"            "N2"            "PMMoV"         "Vol.ml"        "finalcon.cp.L"  
##Create and print output table
#Final<-data.frame(rawdata$SpeciemenNameColumn,rawdata$NameofOutputColumn,Results)
output=back.cal[,c("Run","Plate","Name","Well","Total","Conc..cp.uL","Positives","finalcon.cp.L")]

##Summary statistics##

#Check for those will less than 3 wells (in positives column) here
QualityCheck<-ifelse(output$Positives >=3 & output$Total >= 20000,"Pass","Fail")
outputqc=cbind(output,QualityCheck)

##Save data to Specified output
write.csv(outputqc,file= y,row.names=FALSE)
