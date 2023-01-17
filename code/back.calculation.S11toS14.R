# Author: KB
# Updated:HWu 1/15/23
##dPCR Data back calculation Pipeline
library(tidyverse)
library(dplyr)

#####set work directory to combine the plates####
setwd("/Users/huiyunwu/Desktop/Virus_particle/processed_data/Sample11to14/rawdata")
df <-
  list.files( pattern = "*.csv") %>% 
  map_df(~read_csv(.))
df

write.csv(df,file="/Users/huiyunwu/Desktop/Virus_particle/processed_data/Sample11to14/raw_combined_plate.csv",row.names = F)

#####set work directory to the original one
setwd("/Users/huiyunwu/Desktop/Virus_particle/")


## File path in, current location of the .csv file
FilePathIn <-"processed_data/Sample11to14/combined_plate.csv"
x<-FilePathIn
##File Path out, include desired file name with .csv designation
FilePathOut <-"/Users/huiyunwu/Desktop/Virus_particle/processed_data/Sample11to14/back_cal_S11to14_combined.csv"
y<-FilePathOut
######Load sample volume#####
Vol.<-read.csv(file = "processed_data/Sample11to14/Vol.Sample.csv")
summary(Vol.)
Vol.$Sampling.Date<-as.Date(Vol.$Sampling.Date, format = "%m/%d/%y")
Vol.$Name<-as.character(Vol.$Name)
summary(Vol.)
# Vol.<-Vol.[c(1:20),]
colnames(Vol.)
# Vol<-na.omit(Vol.)
summary(Vol.)
##Enter all parameters, hit control+a to select entire window, then click Run

#####################################


##Pull Data into environment
rawdata=read.csv(file=x)
colnames(rawdata)

#####generate full dataset for back calculation###
back.cal<-left_join(rawdata,Vol.)
dPCRPrepVol.ul<- 10
dPCRNAVol.ul<-5
NAExntVol.ul<- 100
back.cal$finalcon.cp.L<-(back.cal$Conc..cp.uL*dPCRPrepVol.ul)/dPCRNAVol.ul*NAExntVol.ul/back.cal$Vol.L
back.cal$finalcon.cp.L<-format(back.cal$finalcon.cp.L,scientific = TRUE)
colnames(back.cal)
# [1] "Run"           "Date"          "Instrument"    "Plate"         "Group"         "Name"         
# [7] "Well"          "Total"         "Dye"           "Target"        "Conc..cp.uL"   "SD"           
# [13] "CV."           "X95.CI"        "Positives"     "Sample.Name"   "Sampling.Date" "Pore.Size"    
# [19] "Extraction"    "Vol.L"         "finalcon.cp.L" 
##Create and print output table
#Final<-data.frame(rawdata$SpeciemenNameColumn,rawdata$NameofOutputColumn,Results)
output=back.cal[,c("Run","Plate","Name","Sample.Name","Sampling.Date","Pore.Size","Vol.L","Well","Total","Conc..cp.uL","Positives","finalcon.cp.L")]

##Summary statistics##

#Check for those will less than 3 wells (in positives column) here
QualityCheck<-ifelse(output$Positives >=3 & output$Total >= 20000,"Pass","Fail")
outputqc=cbind(output,QualityCheck)

##Save data to Specified output
write.csv(outputqc,file= y,row.names=FALSE)
