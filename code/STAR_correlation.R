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
# data1<-read.csv("processed_data/Sample11to14/back_cal_S11to14_combined.csv")
# data2<-na.omit(data1)
# data2$finalcon.cp.L<-as.numeric(data2$finalcon.cp.L)
# summary(data2)
# data2$Name<-as.character(data2$Name)
# data4<-read.csv("processed_data/Sample11to14/back_cal_S11to14_clean_average.csv")
# data5<-na.omit(data4)
# colnames(data5)
# summary(data5)
# data5$average.cp.L<-as.numeric(data5$average.cp.L)
# data5$average.cp.L<-data5$average.cp.L+1
# data5$log.cp.L<-log10(data5$average.cp.L)
# write.csv(data5,file="processed_data/Sample11to14/ReadytoRoll_S11to14.csv",row.names = F)
# # data5$Pore.Size[data5$Pore.Size == '0.45um filtrate HFUF+Ceres beads'] <- 'filtrate'
# colnames(data5)
# # "Target"           "Name"             "Sample.Name"      "Sampling.Date"   
# [5] "Pore.Size"        "Vol.L"            "Positives"        "finalcon.cp.L"   
# [9] "finalcon.cp.L.QC" "QualityCheck"     "average.cp.L"     "log.cp.L"  
data1<-read.csv(file="processed_data/Sample11to14/47mm.data.cp.L.csv")
summary(data1)
colnames(data1)
# "Name"            "WWTP"            "Collection.date" "Pore.size"       "Volume.L"       
# [6] "HAdV"            "enterovirus"     "norovirus"       "MS2"             "crAssphage56"   
# [11] "comments"   
######reformat the data for log10 #####
data1$MS2<-data1$MS2+1
data1$HAdV<-data1$HAdV+1
data1$enterovirus<-data1$enterovirus+1
data1$norovirus<-data1$norovirus+1
######prepare log10 data frame############
colnames(data1)
log_targets<-log10(data1[,c(6:10)])
colnames(log_targets)
# [1] "HAdV"         "enterovirus"  "norovirus"    "MS2"          "crAssphage56"
env <- data1[,c(1:5)]
log_data1<-cbind(env, log_targets)
log_data2<-head(log_data1,-1) ###remove bottom row
log_data2$Pore.size[log_data2$Pore.size == '0.45um filtrate HFUF+InvP'] <- 'filtrate'
log_data2$Pore.size[log_data2$Pore.size == '0.45um filtrate HFUF+ InvP'] <- 'filtrate'
str(log_data2)
dim(log_data2)
# [1] 58X10
#####one way ANOVA on pore size and WWTPs####
fit_C56 <- lm(crAssphage56 ~ Pore.size, data=log_data2)
summary(fit_C56)
fit_MS2 <- lm(MS2 ~ Pore.size, data=log_data2)
summary(fit_MS2)
fit_HAdV <- lm(HAdV ~ Pore.size, data=log_data2)
summary(fit_HAdV)
fit_enterovirus <- lm(enterovirus ~ Pore.size, data=log_data2)
summary(fit_enterovirus)
fit_norovirus <- lm(norovirus ~ Pore.size, data=log_data2)
summary(fit_norovirus)
#####POST-HOC TEST COMPARISON Pore sizes######
C56_Pore.size<-aov(log_data2$crAssphage56~log_data2$Pore.size)
summary(C56_Pore.size)
TukeyHSD(C56_Pore.size)
MS2_Pore.size<-aov(log_data2$MS2~log_data2$Pore.size)
summary(MS2_Pore.size)
TukeyHSD(MS2_Pore.size)
HAdV_Pore.size<-aov(log_data2$HAdV~log_data2$Pore.size)
summary(HAdV_Pore.size)
TukeyHSD(HAdV_Pore.size)  
enterovirus_Pore.size<-aov(log_data2$enterovirus~log_data2$Pore.size)
summary(enterovirus_Pore.size)
TukeyHSD(enterovirus_Pore.size)
norovirus_Pore.size<-aov(log_data2$norovirus~log_data2$Pore.size)
summary(norovirus_Pore.size)
TukeyHSD(norovirus_Pore.size)
######stacked box plots#####
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# data5<-read.csv(file="processed_data/Sample11to14/ReadytoRoll_S11to14.csv")
# data5$Pore.Size[data5$Pore.Size == '0.45um filtrate HFUF+Ceres beads'] <- 'filtrate'
# colnames(data5)
# summary(data5)
# data5$Pore.Size<-factor(data5$Pore.Size,
                           # levels=c('100 um','20 um','3 um','0.45um','filtrate'),ordered = TRUE)
colnames(log_data2)
# "Name"            "WWTP"            "Collection.date" "Pore.size"       "Volume.L"       
# [6] "HAdV"            "enterovirus"     "norovirus"       "MS2"             "crAssphage56"  
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
data6 <- cbind(log_data2[1:5], stack(log_data2[6:10]))
data6$Pore.size<-factor(data6$Pore.size,
  levels=c('100 um','20 um','3 um','0.45um','filtrate'),ordered = TRUE)
data6$Target<-factor(data6$Target,
                        levels=c('crAssphage56','MS2','HAdV','enterovirus','norovirus'),ordered = TRUE)
colnames(data6)
# "Name"            "WWTP"            "Collection.date" "Pore.size"       "Volume.L"       
# [6] "values"          "ind" 
colnames(data6)[6]="log.cp.L"
colnames(data6)[7]="Target"
ggplot(data6,
       aes(x = Pore.size,
           y=log.cp.L,
           fill=Target)) +
  geom_boxplot()+
  scale_fill_manual(values=cbPalette)+
  labs(title="Particle associated particles Sample 3 to 14",
       y= "Target Concentration (logGC/L)",
       x="Membrane pore size")+
  theme_bw()

ggplot(data6,
       aes(x = WWTP,
           y=log.cp.L,
           fill=Target)) +
  geom_boxplot()+
  scale_fill_manual(values=cbPalette)+
  labs(title="Particle associated particles Aug.2022 to Dec.2022",
       y= "Target Concentration (logGC/L)",
       x="WWTP")+
  theme_bw()
ggsave("/Users/huiyunwu/Desktop/********.jpeg")
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
ggplotRegression(lm(crAssphage56~HAdV, data=data1))

######multiple testing adjustment p value, spearman test ########
library(Hmisc)
##set detection limit to 1.3 log10 cp/L
log_data3<-log_data2
log_data2[is.na(log_data2)]=0.65 ###replace NA to 0.65 log10/L, half of detection limit
colnames(log_data2)
matrix1<-rcorr(as.matrix(log_data2[,c(6:10)]), type = "spearman")
print(matrix1)
capture.output(print(matrix), file = "spearman_corr.csv")
write.table(as.data.frame(matrix),file="spearman_corr.csv", quote=F,sep=",",row.names=F)
