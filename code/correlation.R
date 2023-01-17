install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg","NADA","visdat","naniar"))
install.packages("naniar")
install.packages(c("VIM", "mice")) 
install.packages("ggpubr")
install.packages("Hmisc")
install.packages("psych")
install.packages("vegan")


library(tidyverse)
library(tibble)
library(dplyr)
library(vegan)
library(ggplot2)
library(NADA)
library(visdat)
library(naniar)
library(dplyr)
library(naniar)
library(ggpubr)

########read one by one #####
# C56.p1<-read.csv("processed_data/Sample11to14/C56.S11.1to12.2.csv")
# C56.p2<-read.csv("processed_data/Sample11to14/C56.S12.3to13.4.csv")
# C56.p3<-read.csv("processed_data/Sample11to14/C56.S13.5to14.5.csv")
# C56_full<-rbind(C56.p1,C56.p2,C56.p3)
# C56<-na.omit(C56_full)
# dim(C56)
# 
# MS2.p1<-read.csv("processed_data/Sample11to14/MS2.S11.1to12.2.csv")
# MS2.p2<-read.csv("processed_data/Sample11to14/MS2.S12.3to13.4.csv")
# MS2.p3<-read.csv("processed_data/Sample11to14/MS2.S13.5to14.5.csv")
# MS2_full<-rbind(MS2.p1,MS2.p2,MS2.p3)
# MS2<-na.omit(MS2_full)
# dim(MS2)
# 
# HAdV.p1<-read.csv("processed_data/Sample11to14/HAdV.S11.1to12.2.csv")
# HAdV.p2<-read.csv("processed_data/Sample11to14/HAdV.S12.3to13.4.csv")
# HAdV.p3<-read.csv("processed_data/Sample11to14/HAdV.S13.5to14.5.csv")
# HAdV_full<-rbind(HAdV.p1,HAdV.p2,HAdV.p3)
# HAdV<-na.omit(HAdV_full)
# dim(HAdV)
# 
# EQ.p1<-read.csv("processed_data/Sample11to14/EQ.S11.1to12.2.csv")
# EQ.p2<-read.csv("processed_data/Sample11to14/EQ.S12.3to13.4.csv")
# EQ.p3<-read.csv("processed_data/Sample11to14/EQ.S13.5to14.5.csv")
# EQ_full<-rbind(EQ.p1,EQ.p2,EQ.p3)
# EQ<-na.omit(EQ_full)
# dim(EQ)
# 
# Noro.p1<-read.csv("processed_data/Sample11to14/Noro.S11.1to12.2.csv")
# Noro.p2<-read.csv("processed_data/Sample11to14/Noro.S12.3to13.4.csv")
# Noro.p3<-read.csv("processed_data/Sample11to14/Noro.S13.5to14.5.csv")
# Noro_full<-rbind(Noro.p1,Noro.p2,Noro.p3)
# Noro<-na.omit(Noro_full)
# dim(Noro)
# 
# S11to14<-rbind(C56,MS2,HAdV,EQ,Noro)
# write.csv(S11to14,file="processed_data/Sample11to14/back_cal_S11toS14.csv",row.names = F)
#############read in bulk######################
data1<-read.csv("processed_data/Sample11to14/back_cal_S11to14_combined.csv")
data2<-na.omit(data1)
data2$finalcon.cp.L<-as.numeric(data2$finalcon.cp.L)
summary(data2)
data2$Name<-as.character(data2$Name)
data4<-read.csv("processed_data/Sample11to14/back_cal_S11to14_clean_average.csv")
data5<-na.omit(data4)
colnames(data5)
summary(data5)
data5$average.cp.L<-as.numeric(data5$average.cp.L)
data5$average.cp.L<-data5$average.cp.L+1
data5$log.cp.L<-log10(data5$average.cp.L)
write.csv(data5,file="processed_data/Sample11to14/ReadytoRoll_S11to14.csv",row.names = F)
# data5$Pore.Size[data5$Pore.Size == '0.45um filtrate HFUF+Ceres beads'] <- 'filtrate'
colnames(data5)
# "Target"           "Name"             "Sample.Name"      "Sampling.Date"   
# [5] "Pore.Size"        "Vol.L"            "Positives"        "finalcon.cp.L"   
# [9] "finalcon.cp.L.QC" "QualityCheck"     "average.cp.L"     "log.cp.L"    
####correlation between two targets####
ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(lm(C56~HAdV, data=data5))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/Samlonella vs Stx.jpeg")



######stacked box plots#####
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
data5<-read.csv(file="processed_data/Sample11to14/ReadytoRoll_S11to14.csv")
data5$Pore.Size[data5$Pore.Size == '0.45um filtrate HFUF+Ceres beads'] <- 'filtrate'
colnames(data5)
summary(data5)
data5$Pore.Size<-factor(data5$Pore.Size,
                           levels=c('100 um','20 um','3 um','0.45um','filtrate'),ordered = TRUE)
#####stack plots by pore size####
ggplot(data5,
       aes(x = Pore.Size,
           y=log.cp.L,
           fill=Target,
           na.rm=TRUE)) +
  geom_boxplot()+
  scale_fill_manual(values=cbPalette)+
  labs(title="Sample 11 to 14",
       y= "Target Concentration (logGC/L)",
       x="Membrane pore size")+
  theme_bw()
#####stack plots by WWTP
ggplot(data5,
       aes(x = Sample.Name,
           y=log.cp.L,
           fill=Target,
           na.rm=TRUE)) +
  geom_boxplot()+
  scale_fill_manual(values=cbPalette)+
  labs(title="Particle associated viruses",
       y= "Target Concentration (logGC/L)",
       x="WWTP")+
  theme_bw()



write.csv(STAR,file="/Users/huiyunwu/Desktop/R/47mm.data.cp.L.csv",row.names = FALSE)
STAR<-read.csv(file="/Users/huiyunwu/Desktop/R/47mm.data.cp.L.csv")
