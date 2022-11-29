# ######Part2#####
getwd()
setwd("/Users/huiyunwu/Desktop/Virus_particle/")
enterovirus<-read.csv(file = "LW1toLW7.enterovirus.csv")
norovirus<-read.csv(file = "LW1toLW7.norovirus.csv")
HAdV<-read.csv(file = "LW1toLW7.HAdV.csv")
MS2<-read.csv(file = "LW1toLW7.MS2.csv")
Stx2<-read.csv(file = "LW1toLW7.Stx2.csv")
C56<-read.csv(file = "LW1toLW7.crassphage56.csv")
salmonella<-read.csv(file = "LW1toLW7.Salmonella.csv")
# ####7 targets######
LW1toLW7.meta<-rbind(enterovirus,norovirus,HAdV,MS2,Stx2,C56,salmonella)
write.csv(LW1toLW7.meta,file= "meta.LW1toLW7.csv",row.names=FALSE)



####select QC passed data
meta.NOLA<-read.csv(file="/Users/huiyunwu/Desktop/R/R.back.cal./NOLA sample/meta.M1toM7.csv")

####omit NAs
# colnames(meta.NOLA)
# meta.nola<-meta.NOLA[,c(1,3,9,11)]
# meta.nola.1<-na.omit(meta.nola)
# write.csv(meta.nola.1,file= "meta.nola.noNA.csv",row.names=FALSE)
