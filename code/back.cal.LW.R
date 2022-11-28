# Author: KB
# Update:HWu 11/20/22
##dPCR Data back calculation Pipeline
library(tidyverse)
library(dplyr)

## File path in, current location of the .csv file
FilePathIn <-"Lakewater/rawdata/Crassphage56.17Nov22.Lw1toLw7.kb.raw-Quantification.csv"
x<-FilePathIn
##File Path out, include desired file name with .csv designation
FilePathOut <-"Lakewater/LW1toLW7.crassphage56.csv"
y<-FilePathOut
##Load sample volume
Vol.<-read.csv(file = "Lakewater/Vol.LW1toLW7.csv")
summary(Vol.)
Vol.$date<-as.Date(Vol.$date, format = "%m/%d/%y")
colnames(Vol.)[1]<-"Name"
colnames(Vol.)[5]<-"Effluent.vol.ul"
colnames(Vol.)[4]<-"Influent.vol.ml"
summary(Vol.)
Vol.$`Name`<-c("lw1","lw2","lw3","lw4","lw5","lw6","lw7")
Vol.$process.vol.mL<-140/Vol.$Effluent.vol.ul*Vol.$Influent.vol.ml
 dil.<-10

##Enter all parameters, hit control+a to select entire window, then click Run

#####################################


##Pull Data into environment
rawdata=read.csv(file=x)
colnames(rawdata)

#####generate full dataset for back calculation###
back.cal<-left_join(rawdata,Vol.)
back.cal$dPCRPrepVol.ul<- 10
back.cal$dPCRNAVol.ul<-5
back.cal$NAExntVol.ul<- 100
back.cal$finalcon.cp.L<-(back.cal$Conc..cp.uL*back.cal$dPCRPrepVol.ul)/back.cal$dPCRNAVol.ul*back.cal$NAExntVol.ul/back.cal$process.vol.mL*1000*dil.
back.cal$finalcon.cp.L<-format(back.cal$finalcon.cp.L,scientific = TRUE)
colnames(back.cal)
# [1] "Run"                  "Date"                 "Instrument"           "Plate"                "Group"               
# [6] "Name"                 "Well"                 "Total"                "Dye"                  "Target"              
# [11] "Conc..cp.uL"          "SD"                   "CV."                  "X95.CI"               "Positives"           
# [16] "Sample.."             "date"                 "Influent.vol.ml"      "Effluent.vol.ul"      "Concentration.Factor"
# [21] "process.vol.mL"       "dPCRPrepVol.ul"       "NAVol.ul"             "NAExntVol.ul"         "finalcon.cp.mL"      
# [26] "finalcon.cp.L"  
##Create and print output table
#Final<-data.frame(rawdata$SpeciemenNameColumn,rawdata$NameofOutputColumn,Results)
output=back.cal[,c("Run","Plate","Name","Well","Total","Conc..cp.uL","Positives","Sample..","date","finalcon.cp.L")]

##Summary statistics##

#Check for those will less than 3 wells (in positives column) here
QualityCheck<-ifelse(output$Positives >=3 & output$Total >= 20000,"Pass","Fail")
outputqc=cbind(output,QualityCheck)

##Save data to Specified output
write.csv(outputqc,file= y,row.names=FALSE)
