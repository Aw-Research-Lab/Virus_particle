# ######Part2#####
# getwd()
# setwd("/Users/huiyunwu/Desktop/R/R.back.cal./NOLA sample/")
# m.enterovirus<-read.csv(file = "M1-M7.enterovirus.csv")
# m.norovirus<-read.csv(file = "M1-M7.norovirus.csv")
# m.HAdV<-read.csv(file = "M1-M7.HAdV.csv")
# m.MS2<-read.csv(file = "M1-M7.MS2.csv")
# m.Stx2<-read.csv(file = "M1-M7.Stx2.csv")
# m.C56<-read.csv(file = "M1-M7.C56.csv")
# m.salmonella<-read.csv(file = "M1-M7.Salmonella.csv")
# ####7 targets######
# M1to7.meta<-rbind(m.enterovirus,m.norovirus,m.HAdV,m.MS2,m.Stx2,m.C56,m.salmonella)
# write.csv(M1to7.meta,file= "meta.M1toM7.csv",row.names=FALSE)



####select QC passed data
meta.NOLA<-read.csv(file="/Users/huiyunwu/Desktop/R/R.back.cal./NOLA sample/meta.M1toM7.csv")

####omit NAs
# colnames(meta.NOLA)
# meta.nola<-meta.NOLA[,c(1,3,9,11)]
# meta.nola.1<-na.omit(meta.nola)
# write.csv(meta.nola.1,file= "meta.nola.noNA.csv",row.names=FALSE)
