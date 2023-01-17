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

########
C56.p1<-read.csv("processed_data/Sample11to14/C56.S11.1to12.2.csv")
C56.p2<-read.csv("processed_data/Sample11to14/C56.S12.3to13.4.csv")
C56.p3<-read.csv("processed_data/Sample11to14/C56.S13.5to14.5.csv")
C56_full<-rbind(C56.p1,C56.p2,C56.p3)
C56<-na.omit(C56_full)
dim(C56)

MS2.p1<-read.csv("processed_data/Sample11to14/MS2.S11.1to12.2.csv")
MS2.p2<-read.csv("processed_data/Sample11to14/MS2.S12.3to13.4.csv")
MS2.p3<-read.csv("processed_data/Sample11to14/MS2.S13.5to14.5.csv")
MS2_full<-rbind(MS2.p1,MS2.p2,MS2.p3)
MS2<-na.omit(MS2_full)
dim(MS2)

HAdV.p1<-read.csv("processed_data/Sample11to14/HAdV.S11.1to12.2.csv")
HAdV.p2<-read.csv("processed_data/Sample11to14/HAdV.S12.3to13.4.csv")
HAdV.p3<-read.csv("processed_data/Sample11to14/HAdV.S13.5to14.5.csv")
HAdV_full<-rbind(HAdV.p1,HAdV.p2,HAdV.p3)
HAdV<-na.omit(HAdV_full)
dim(HAdV)

EQ.p1<-read.csv("processed_data/Sample11to14/EQ.S11.1to12.2.csv")
EQ.p2<-read.csv("processed_data/Sample11to14/EQ.S12.3to13.4.csv")
EQ.p3<-read.csv("processed_data/Sample11to14/EQ.S13.5to14.5.csv")
EQ_full<-rbind(EQ.p1,EQ.p2,EQ.p3)
EQ<-na.omit(EQ_full)
dim(EQ)

Noro.p1<-read.csv("processed_data/Sample11to14/Noro.S11.1to12.2.csv")
Noro.p2<-read.csv("processed_data/Sample11to14/Noro.S12.3to13.4.csv")
Noro.p3<-read.csv("processed_data/Sample11to14/Noro.S13.5to14.5.csv")
Noro_full<-rbind(Noro.p1,Noro.p2,Noro.p3)
Noro<-na.omit(Noro_full)
dim(Noro)

S11to14<-rbind(C56,MS2,HAdV,EQ,Noro)
write.csv(S11to14,file="processed_data/Sample11to14/back_cal_S11toS14.csv",row.names = F)
###################################
SSO$HAdV<-SSO$HAdV+1
SSO$Enterovirus<-SSO$Enterovirus+1
SSO$Norovirus<-SSO$Norovirus+1
SSO$CoV2<-SSO$CoV2+1
SSO$Stx<-SSO$Stx+1
SSO$Campy<-SSO$Campy+1
SSO$Salmonella<-SSO$Salmonella+1




# weather<-merge(x=charlotte, y=rtp, by="Date")
# plot(x=rtp$Date,y=rtp$`RTP-pcp-mm`)
# plot(x=)
# bar_plot(x=weather$Date,y=weather$`RTP-pcp-mm`)
# g <- ggplot(rtp, aes(Date,RTP-PCP-mm))
# # Number of cars in each class:
# g + geom_point()

colnames(SSO)
log_targets<-log10(SSO[,c(5:9, 11,12)])#no SARS-CoV-2
log_SSO<-log_targets

colnames(log_targets)
env <- SSO[,c(2,3,13,14)]
log_SSO<-cbind(env, log_targets)
str(log_SSO)
dim(log_SSO)

# #Exploring the data
# head(log_SSO)
# summary(log_SSO)
# str(log_SSO)
# dplyr::glimpse(log_SSO)


###boxplot facet_wrap(~season)####
ggplot(log_SSO, 
       aes(x = WWTP, 
           y=C64)) + 
  geom_boxplot()+
  facet_wrap(~Season)

ggplot(log_SSO, 
       aes(x = Season, 
           y=C64)) + 
  geom_boxplot()

ggplot(log_SSO, 
       aes(x = Season, 
           y=HAdV)) + 
  geom_boxplot()

ggplot(log_SSO, 
       aes(x = Season, 
           y=Enterovirus)) + 
  geom_boxplot()


ggplot(log_SSO, 
       aes(x = Season, 
           y=Norovirus)) + 
  geom_boxplot()

ggplot(log_SSO, 
       aes(x = Season, 
           y=Stx)) + 
  geom_boxplot()

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=Campy)) + 
  geom_boxplot()

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=HAdV)) + 
  geom_boxplot()+
  facet_wrap(~Season)

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=Enterovirus)) + 
  geom_boxplot()+
  facet_wrap(~Season)

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=Norovirus)) + 
  geom_boxplot()+
  facet_wrap(~Season)

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=CoV2)) + 
  geom_boxplot()+
  facet_wrap(~Season)


ggplot(log_SSO, 
       aes(x = WWTP, 
           y=Stx)) + 
  geom_boxplot()+
  facet_wrap(~Season)

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=Stx)) + 
  geom_boxplot()

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=Campy)) + 
  geom_boxplot()+
  facet_wrap(~Season)cet_wrap(~Season)

ggplot(log_SSO, 
       aes(x = WWTP, 
           y=Salmonella)) + 
  geom_boxplot()+
  facet_wrap(~Season)

ggplot(log_SSO, 
       aes(x = Date, 
           y=Salmonella)) + 
  geom_point()+
  facet_wrap(~WWTP)


#####time variation- scatter points- colour= factor(WWTP) ####
ggplot(log_SSO, 
       aes(x = Date, 
           y = C64,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="C64", 
       y= "C64 Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/C64-time variantion.jpeg")




ggplot(log_SSO, 
       aes(x = Date, 
           y = HAdV,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="HAdV", 
       y= "HAdV Concentration (logGC/L)",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/HAdV-time variantion.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Enterovirus,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="Enterovirus", 
       y= "Enterovirus Concentration (logGC/L)",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Enterovirus-time variantion.jpeg")


ggplot(SSO, 
       aes(x = Date, 
           y = Enterovirus,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="Enterovirus", 
       y= "Enterovirus Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/Enterovirus-time variantion.jpeg")



ggplot(SSO, 
       aes(x = Date, 
           y = Norovirus,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="Norovirus", 
       y= "Norovirus Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/Norovirus-time variantion.jpeg")

ggplot(log_SSO, 
       aes(x = Date, 
           y = Norovirus,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="Norovirus", 
       y= "Norovirus Concentration (logGC/L)",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Norovirus-time variantion.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = CoV2,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="CoV2", 
       y= "CoV2 Concentration (logGC/L)",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/CoV2-time variantion.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Campy,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="Campy", 
       y= "Campy Concentration (logGC/L)",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Campy-time variantion.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Stx,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="Stx", 
       y= "Stx Concentration (logGC/L)",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Stx-time variantion.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Salmonella,
           colour=factor(WWTP))) + 
  geom_point()+
  labs(title="Salmonella", 
       y= "Salmonella Concentration (logGC/L)",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Salmonella-time variantion.jpeg")


####time variants facet_wrap(~WWTP)####
ggplot(log_SSO, 
       aes(x = Date, 
           y = C64,
           colour=factor(WWTP))) + 
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="C64", 
       y= "C64 Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/C64-time variantion-2.jpeg")

ggplot(log_SSO, 
       aes(x = Date, 
           y = HAdV,
           colour=factor(WWTP))) + 
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="HAdV", 
       y= "HAdV Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/HAdV-time variantion-2.jpeg")

ggplot(log_SSO, 
       aes(x = Date, 
           y = Enterovirus,
           colour=factor(WWTP))) +  
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="Enterovirus", 
       y= "Enterovirus Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Enterovirus-time variantion-2.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Norovirus,
           colour=factor(WWTP))) +  
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="Norovirus", 
       y= "Norovirus Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Norovirus-time variantion-2.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = CoV2,
       colour=factor(WWTP))) + 
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="SARS-CoV2", 
       y= "SARS-CoV2 Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/SARS-CoV2-time variantion-2.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Campy,
           colour=factor(WWTP))) + 
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="Campylobactor", 
       y= "Campylobactor Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Campylobactor-time variantion.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Stx,
           colour=factor(WWTP))) + 
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="Stx", 
       y= "Stx Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Stx-time variantion-2.jpeg")


ggplot(log_SSO, 
       aes(x = Date, 
           y = Salmonella,
           colour=factor(WWTP))) + 
  geom_point()+
  facet_wrap(~WWTP)+
  labs(title="Salmonella", 
       y= "Salmonella Concentration (logGC/L)",
       x="2021-2022")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Salmonella_time variantion2.jpeg")


# ###### time variation with geom_miss_point()+ by WWTP ####
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = C64)) + 
#   # geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="Crassphage", 
#        y= "Crassphage Concentration (GC/L)",
#        x="2021-2022")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
# 
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = HAdV)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="HAdV", 
#        y= "HAdV Concentration (logGC/L)",
#        x="2021-2022")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
# 
# 
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = Enterovirus)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="Enterovirus", 
#        y= "Enterovirus Concentration (GC/L)",
#        x="2021")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
# 
# 
# 
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = Norovirus)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="Norovirus", 
#        y= "Norovirus Concentration (GC/L)",
#        x="2021")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
# 
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = CoV2)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="Cov2", 
#        y= "SARS-CoV2 Concentration (logGC/L)",
#        x="2021")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
# 
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = Stx)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="Stx", 
#        y= "Stx Concentration (GC/L)",
#        x="2021")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
# 
# 
# 
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = Campy)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="Campy", 
#        y= "Campy Concentration (GC/L)",
#        x="2021")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
# 
# 
# ggplot(log_SSO, 
#        aes(x = Date, 
#            y = Salmonella)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()+
#   labs(title="Salmonella", 
#        y= "Salmonella Concentration (GC/L)",
#        x="2021")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")


# ####Cov2####
# ggplot(log_SSO, 
#        aes(x = C64, 
#            y = CoV2)) + 
#   # geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()
# 
# ggplot(log_SSO, 
#        aes(x = Enterovirus, 
#            y = CoV2)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()
# 
# ggplot(log_SSO, 
#        aes(x = Norovirus, 
#            y = CoV2)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()
# 
# ggplot(log_SSO, 
#        aes(x = Stx, 
#            y = CoV2)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()
# 
# ggplot(log_SSO, 
#        aes(x = Campy, 
#            y = CoV2)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()
# 
# ggplot(log_SSO, 
#        aes(x = Salmonella, 
#            y = CoV2)) + 
#   geom_miss_point()+
#   facet_wrap(~WWTP)+ theme_bw()
# #### visualising missings in variables####
# gg_miss_var(log_SSO)+theme_bw()
# gg_miss_var(log_SSO, facet = WWTP)+theme_bw()
# ### Numerical summaries of missing values
# dplyr::n_distinct(log_SSO)
# dplyr::n_distinct(log_SSO$HAdV)
# dplyr::n_distinct(log_SSO$Enterovirus)
# 
# n_miss(log_SSO)
# n_miss(log_SSO$Enterovirus)
# n_complete(log_SSO)
# n_complete(log_SSO$CoV2)
# 
# 
# data(TCE)
# attach(TCE) 
# cenboxplot(TCEConc, TCECen, Density) 
# # data(ShePyrene)
# data(Golden) 
# attach(Golden)
# cenboxplot(Blood, BloodCen, DosageGroup) 
# class(Blood)
# class(BloodCen)
# class(DosageGroup)


# #nmds code
# set.seed(123)
# nmds = metaMDS(m_com, distance = "bray")
# nmds

# C2<-try(cov(logSC, use="complete"))
# stopifnot(identical(C2, cov(logSC, use = "na.or.complete")))
# 
# range(eigen(C2, only.values = TRUE)$values) # -1.054311e-16  3.393708e+00
# C3 <- cov(logSC, use = "pairwise")
# 
# range(eigen(C3, only.values = TRUE)$values) # -0.1547514  1.3752272
# 
# 
# ## "pairwise" is closer componentwise,
# summary(abs(c(1 - Rp/R.)))
# summary(abs(c(1 - Rc/R.)))
# 
# ## but "complete" is closer in Eigen space:
# EV <- function(m) eigen(m, only.values=TRUE)$values
# summary(abs(1 - EV(Rp)/EV(R.)) / abs(1 - EV(Rc)/EV(R.)))



####Seasons original data####
# fit <- lm(C64 ~ Season, data=df)
# summary(fit)
# 
# fit <- lm(HAdV ~ Season, data=df)
# summary(fit)
# 
# fit <- lm(Enterovirus ~ Season, data=df)
# summary(fit)
# 
# fit <- lm(Norovirus ~ Season, data=df)
# summary(fit)
# 
# fit <- lm(CoV2 ~ Season, data=df)
# summary(fit)
# 
# fit <- lm(Campy ~ Season, data=df)
# summary(fit)
# 
# fit <- lm(Stx ~ Season, data=df)
# summary(fit)
# 
# fit <- lm(Salmonella ~ Season, data=df)
# summary(fit)

####qq plot normality test####
log_SSO[,4:11][log_SSO[,4:11] <3] <- NA
library("car")
qqPlot(log_SSO$C64)
qqPlot(log_SSO$HAdV)
qqPlot(log_SSO$Enterovirus)
qqPlot(log_SSO$Norovirus)
qqPlot(log_SSO$CoV2)
qqPlot(log_SSO$Campy)
qqPlot(log_SSO$Stx)
qqPlot(log_SSO$Salmonella)
####Seasons log_SSO####
log_SSO$Season<-factor(log_SSO$Season,
                levels=c('Spring','Summer','Fall','Winter'),ordered = TRUE)
ggplot(log_SSO, 
       aes(x = Season, 
           y=C64,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="C64", 
       y= "C64 Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()
ggsave("C64-boxplot.jpeg")

#####Statistic influent log_SSO####
fit <- lm(C64 ~ Influent, data=log_SSO)
summary(fit)
fit <- lm(HAdV ~ Influent, data=log_SSO)
summary(fit)
fit <- lm(Enterovirus ~ Influent, data=log_SSO)
summary(fit)
fit <- lm(Norovirus ~ Influent, data=log_SSO)
summary(fit)
fit <- lm(CoV2 ~ Influent, data=log_SSO)
summary(fit)
fit <- lm(Campy ~ Influent, data=log_SSO)
summary(fit)
fit <- lm(Stx ~ Influent, data=log_SSO)
summary(fit)
fit <- lm(Salmonella ~ Influent, data=log_SSO)
summary(fit)



####Statistic WWTP log_SSO ####
fit <- lm(C64 ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 0.3314 on 135 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.04036,	Adjusted R-squared:  0.01904 
# F-statistic: 1.893 on 3 and 135 DF,  p-value: 0.1338

fit <- lm(HAdV ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 1.045 on 129 degrees of freedom
# (10 observations deleted due to missingness)
# Multiple R-squared:  0.04697,	Adjusted R-squared:  0.02481 
# F-statistic: 2.119 on 3 and 129 DF,  p-value: 0.1009


fit <- lm(Enterovirus ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 1.045 on 129 degrees of freedom
# (10 observations deleted due to missingness)
# Multiple R-squared:  0.04697,	Adjusted R-squared:  0.02481 
# F-statistic: 2.119 on 3 and 129 DF,  p-value: 0.1009

fit <- lm(Norovirus ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 0.6872 on 129 degrees of freedom
# (10 observations deleted due to missingness)
# Multiple R-squared:  0.04614,	Adjusted R-squared:  0.02395 
# F-statistic:  2.08 on 3 and 129 DF,  p-value: 0.1061

fit <- lm(CoV2 ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 0.6328 on 64 degrees of freedom
# (75 observations deleted due to missingness)
# Multiple R-squared:  0.05347,	Adjusted R-squared:  0.009103 
# F-statistic: 1.205 on 3 and 64 DF,  p-value: 0.3151


fit <- lm(Campy ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 0.2524 on 88 degrees of freedom
# (51 observations deleted due to missingness)
# Multiple R-squared:  0.04242,	Adjusted R-squared:  0.00977 
# F-statistic: 1.299 on 3 and 88 DF,  p-value: 0.2798

fit <- lm(Stx ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 0.5537 on 130 degrees of freedom
# (9 observations deleted due to missingness)
# Multiple R-squared:  0.07661,	Adjusted R-squared:  0.0553 
# F-statistic: 3.595 on 3 and 130 DF,  p-value: 0.01545

fit <- lm(Salmonella ~ WWTP, data=log_SSO)
summary(fit)
# Residual standard error: 0.5971 on 122 degrees of freedom
# (17 observations deleted due to missingness)
# Multiple R-squared:  0.04753,	Adjusted R-squared:  0.02411 
# F-statistic: 2.029 on 3 and 122 DF,  p-value: 0.1133

fit <- lm(Influent ~ WWTP, data=log_SSO)
summary(fit)
temp<-as.factor(log_SSO$Season)
####Statistic Season log_SSO ####
fit <- lm(C64 ~ as.character(Season), data=log_SSO)
summary(fit)
# Residual standard error: 0.3045 on 135 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.1896,	Adjusted R-squared:  0.1716 
# F-statistic: 10.53 on 3 and 135 DF,  p-value: 2.864e-06


fit <- lm(HAdV ~ Season, data=log_SSO)
summary(fit)
# Residual standard error: 0.8779 on 129 degrees of freedom
# (10 observations deleted due to missingness)
# Multiple R-squared:  0.3269,	Adjusted R-squared:  0.3112 
# F-statistic: 20.88 on 3 and 129 DF,  p-value: 4.324e-11

fit <- lm(Enterovirus ~ Season, data=log_SSO)
summary(fit)
# Residual standard error: 0.5316 on 115 degrees of freedom
# (24 observations deleted due to missingness)
# Multiple R-squared:  0.4644,	Adjusted R-squared:  0.4505 
# F-statistic: 33.24 on 3 and 115 DF,  p-value: 1.511e-15

fit <- lm(Norovirus ~ Season, data=log_SSO)
summary(fit)
# Residual standard error: 0.6408 on 129 degrees of freedom
# (10 observations deleted due to missingness)
# Multiple R-squared:  0.1708,	Adjusted R-squared:  0.1515 
# F-statistic: 8.859 on 3 and 129 DF,  p-value: 2.208e-05

fit <- lm(CoV2 ~ Season, data=log_SSO)
summary(fit)
# Residual standard error: 0.608 on 64 degrees of freedom
# (75 observations deleted due to missingness)
# Multiple R-squared:  0.126,	Adjusted R-squared:  0.08505 
# F-statistic: 3.076 on 3 and 64 DF,  p-value: 0.03376


fit <- lm(Campy ~ Season, data=log_SSO)
summary(fit)
# Residual standard error: 0.2461 on 88 degrees of freedom
# (51 observations deleted due to missingness)
# Multiple R-squared:  0.08962,	Adjusted R-squared:  0.05858 
# F-statistic: 2.887 on 3 and 88 DF,  p-value: 0.04005


fit <- lm(Stx ~ Season, data=log_SSO)
summary(fit)
# Residual standard error: 0.4806 on 130 degrees of freedom
# (9 observations deleted due to missingness)
# Multiple R-squared:  0.3044,	Adjusted R-squared:  0.2884 
# F-statistic: 18.97 on 3 and 130 DF,  p-value: 2.898e-10

fit <- lm(Salmonella ~ Season, data=log_SSO)
summary(fit)

# Residual standard error: 0.5418 on 122 degrees of freedom
# (17 observations deleted due to missingness)
# Multiple R-squared:  0.2157,	Adjusted R-squared:  0.1964 
# F-statistic: 11.18 on 3 and 122 DF,  p-value: 1.552e-06




fit <- lm(C64 ~ Season, data=log_SSO)
summary(fit)

colnames(log_SSO)
log_SSO[,4:11][log_SSO[,4:11] <3] <- NA

ggplot(log_SSO, 
       aes(x = Season, 
           y=HAdV,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="HAdV", 
       y= "HAdV Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()

ggsave("HAdV-boxplot.jpeg")

fit <- lm(HAdV ~ Season, data=log_SSO)
summary(fit)


ggplot(log_SSO, 
       aes(x = Season, 
           y=Enterovirus,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="Enterovirus", 
       y= "Enterovirus Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()
ggsave("Enterovirus-boxplot.jpeg")





ggplot(log_SSO, 
       aes(x = Season, 
           y=Norovirus,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="Norovirus", 
       y= "Norovirus Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()

ggsave("Norovirus-boxplot.jpeg")
fit <- lm(Norovirus ~ Season, data=log_SSO)
summary(fit)

ggplot(log_SSO, 
       aes(x = Season, 
           y=CoV2,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="SARS-CoV2", 
       y= "SARS-CoV2 Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()


ggsave("CoV2-boxplot.jpeg")

fit <- lm(CoV2 ~ Season, data=log_SSO)
summary(fit)
# Residual standard error: 0.6016 on 69 degrees of freedom
# (75 observations deleted due to missingness)
# Multiple R-squared:  0.1278,	Adjusted R-squared:  0.08991 
# F-statistic: 3.371 on 3 and 69 DF,  p-value: 0.02329

ggplot(log_SSO, 
       aes(x = Season, 
           y=Campy,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="Campylobacter", 
       y= "Campy Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()

  ggsave("Campy-boxplot.jpeg")

  fit <- lm(Campy ~ Season, data=log_SSO)
summary(fit)


ggplot(log_SSO, 
       aes(x = Season, 
           y=Stx,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="Stx", 
       y= "Stx Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()

ggsave("Stx-boxplot.jpeg")

fit <- lm(Stx ~ Season, data=log_SSO)
summary(fit)

ggplot(log_SSO, 
       aes(x = Season, 
           y=Salmonella,
           fill=WWTP)) + 
  geom_boxplot()+
  labs(title="Salmonella", 
       y= "Salmonella Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()

ggsave("Salmonella-boxplot.jpeg")

fit <- lm(Salmonella ~ Season, data=log_SSO)
summary(fit)


fit <- lm(Enterovirus ~ Norovirus, data=log_SSO)
summary(fit)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.6913     0.4496   3.762 0.000327 ***
#   Norovirus     0.4523     0.1160   3.899 0.000205 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.064 on 77 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.1649,	Adjusted R-squared:  0.1541 
# F-statistic: 15.21 on 1 and 77 DF,  p-value: 0.0002046
# abline(1.6913,0.4523)
# abline(fit)







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

##### targets interaction####

ggplotRegression(lm(HAdV~C64, data=log_SSO))
ggsave("HAdV vs C64.jpeg")



ggplotRegression(lm(Enterovirus~C64, data=log_SSO))
ggsave("Enterovirus vs C64.jpeg")

ggplotRegression(lm(Norovirus~C64, data=log_SSO))
ggsave("Norovirus vs C64.jpeg")

ggplotRegression(lm(Stx~C64, data=log_SSO))
ggsave("Stx vs C64.jpeg")

ggplotRegression(lm(CoV2~C64, data=log_SSO))
ggsave("CoV2 vs C64.jpeg")

ggplotRegression(lm(Campy~C64, data=log_SSO))
ggsave("Campy vs C64.jpeg")


ggplotRegression(lm(Salmonella~C64, data=log_SSO))
ggsave("Salmonella vs C64.jpeg")


ggplotRegression(lm(HAdV~Enterovirus, data=log_SSO))
ggsave("HAdV vs Enterovirus.jpeg")

ggplotRegression(lm(HAdV~Norovirus, data=log_SSO))
ggsave("HAdV vs Norovirus.jpeg")

ggplotRegression(lm(HAdV~Salmonella, data=log_SSO))
ggsave("HAdV vs Salmonella.jpeg")

ggplotRegression(lm(HAdV~Campy, data=log_SSO))
ggsave("HAdV vs Campy.jpeg")

ggplotRegression(lm(HAdV~Stx, data=log_SSO))
ggsave("HAdV vs Stx.jpeg")

ggplotRegression(lm(Enterovirus~Stx, data=log_SSO))
ggsave("EQ vs Stx.jpeg")

ggplotRegression(lm(Norovirus~Enterovirus, data=log_SSO))
ggsave("EQ vs Noro.jpeg")

ggplotRegression(lm(Norovirus~Campy, data=log_SSO))
ggsave("Campy vs Noro.jpeg")

ggplotRegression(lm(Norovirus~Salmonella, data=log_SSO))
ggsave("Norovurs vs. Salmonella.jpeg")

ggplotRegression(lm(Stx~Campy, data=log_SSO))
ggsave("Campy vs Stx.jpeg")

ggplotRegression(lm(Stx~Salmonella, data=log_SSO))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/Samlonella vs Stx.jpeg")
#### CoV2 comparison #####
ggplotRegression(lm(CoV2~C64, data=log_SSO))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs C64.jpeg")
ggplotRegression(lm(CoV2~HAdV, data=log_SSO))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs HAdV.jpeg")

ggplotRegression(lm(CoV2~Enterovirus, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Enterovirus.jpeg")
ggplotRegression(lm(CoV2~Enterovirus, data=subset(log_SSO,WWTP=="MF")))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Enterovirus-MF.jpeg")
ggplotRegression(lm(CoV2~Enterovirus, data=subset(log_SSO,WWTP=="ND")))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Enterovirus-ND.jpeg")
ggplotRegression(lm(CoV2~Enterovirus, data=subset(log_SSO,WWTP=="SC")))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Enterovirus-SC.jpeg")
ggplotRegression(lm(CoV2~Enterovirus, data=subset(log_SSO,WWTP=="MLC")))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Enterovirus-MLC.jpeg")

ggplotRegression(lm(CoV2~Norovirus, data=log_SSO))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Norovirus.jpeg")
ggplotRegression(lm(CoV2~Campy, data=log_SSO))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Campy.jpeg")
ggplotRegression(lm(CoV2~Stx, data=log_SSO))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Stx.jpeg")
ggplotRegression(lm(CoV2~Salmonella, data=log_SSO))
ggsave("/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Salmonella.jpeg")


#####targets with flowrate####
log_SSO_MF<-subset(log_SSO,WWTP=="MF")

ggplotRegression(lm(C64~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/C64 vs Influent.jpeg")

ggplotRegression(lm(HAdV~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/HAdV vs Influent.jpeg")

ggplotRegression(lm(Enterovirus~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/Enterovirus vs Influent.jpeg")

ggplotRegression(lm(Norovirus~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/Norovirus vs Influent.jpeg")

ggplotRegression(lm(Stx~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/Stx vs Influent.jpeg")

ggplotRegression(lm(Campy~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/Campy vs Influent.jpeg")

ggplotRegression(lm(Salmonella~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/Salmonella vs Influent.jpeg")

ggplotRegression(lm(CoV2~Influent, data=log_SSO))
ggsave(file="/Users/huiyunwu/Desktop/SSO-plots/CoV2 vs Influent.jpeg")

write.csv(SSO,file="Desktop/qPCR_mastersheet.csv",row.names = F)
#####CoV2 ratios######
CoV2ratio$C64vCoV2<-CoV2ratio$C64/CoV2ratio$CoV2
CoV2ratio$HAdVvCoV2<-CoV2ratio$HAdV/CoV2ratio$CoV2
CoV2ratio$EnterovirusvCoV2<-CoV2ratio$Enterovirus/CoV2ratio$CoV2
CoV2ratio$NorovirusvCoV2<-CoV2ratio$Norovirus/CoV2ratio$CoV2
CoV2ratio$CoV2vCoV2<-CoV2ratio$CoV2/CoV2ratio$CoV2
CoV2ratio$SalmonellavCoV2<-CoV2ratio$Salmonella/CoV2ratio$CoV2
CoV2ratio$CampyvCoV2<-CoV2ratio$Campy/CoV2ratio$CoV2
CoV2ratio$StxvCoV2<-CoV2ratio$Stx/CoV2ratio$CoV2

ggplot(CoV2ratio, 
       aes(x = Date, 
           y = C64vCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="C64vCoV2 ratio", 
       y= "C64 CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/C64 CoV2 ratio.jpeg")

ggplot(CoV2ratio, 
       aes(x = Date, 
           y = HAdVvCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="HAdVvCoV2 ratio", 
       y= "HAdV CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/HAdV CoV2 ratio.jpeg")

ggplot(CoV2ratio, 
       aes(x = Date, 
           y = EnterovirusvCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="EnterovirusvCoV2 ratio", 
       y= "Enterovirus CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Enterovirus CoV2 ratio.jpeg")

ggplot(CoV2ratio, 
       aes(x = Date, 
           y = NorovirusvCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="NorovirusvCoV2 ratio", 
       y= "Novovirus CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Norovirus CoV2 ratio.jpeg")


ggplot(CoV2ratio, 
       aes(x = Date, 
           y = CoV2vCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="CoV2vCoV2 ratio", 
       y= "CoV2 CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/CoV2 CoV2 ratio.jpeg")


ggplot(CoV2ratio, 
       aes(x = Date, 
           y = CampyvCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="CampyvCoV2 ratio", 
       y= "Campy CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Campy CoV2 ratio.jpeg")

ggplot(CoV2ratio, 
       aes(x = Date, 
           y = StxvCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="StxvCoV2 ratio", 
       y= "Stx CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Stx CoV2 ratio.jpeg")

ggplot(CoV2ratio, 
       aes(x = Date, 
           y = SalmonellavCoV2,
           #colour=factor(WWTP)
       )) + 
  geom_point()+
  labs(title="SalmonellavCoV2 ratio", 
       y= "Salmonella CoV2 ratio",
       x="2021")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b")+
  theme_bw()
ggsave(file="Desktop/SSO-plots/Salmonella CoV2 ratio.jpeg")

#####stacked databox plot####
colnames(SSO)
data<-SSO[,c(1:4,13,14,5:12)]
colnames(data)
data_mod <- cbind(data[1:6], stack(data[7:14]))

SSO_mod$WWTP<-gsub('MF','RTP1',SSO_mod$WWTP)
SSO_mod$WWTP<-gsub('ND','RTP2',SSO_mod$WWTP)
SSO_mod$WWTP<-gsub('SC','Charlotte1',SSO_mod$WWTP)
SSO_mod$WWTP<-gsub('MLC','Charlotte2',SSO_mod$WWTP)

write.csv(SSO_mod, file="/Users/huiyunwu/Desktop/EPA paper wastewater pathogen/ANOVA/qPCR_mastersheet_mod.csv")

SSO_mod$log_value<-log10(SSO_mod$values+1)
colnames(SSO_mod)
# [1] "Sample ID.bacteria" "Date"               "WWTP"               "Sample ID.virus"    "Season"             "Influent"           "values"            
# [8] "target"             "log_value"  

library(readr)
qPCR_mastersheet_mod_no_CoV2 <- read_csv("Desktop/EPA paper wastewater pathogen/ANOVA/qPCR_mastersheet_mod_no-CoV2.csv", 
                                           +     col_types = cols(Date = col_date(format = "%m/%d/%y")))

SSO_mod<-qPCR_mastersheet_mod_no_CoV2

SSO_mod$Season<-factor(SSO_mod$Season,
                       levels=c('Spring','Summer','Fall','Winter'),ordered = TRUE)
SSO_mod$target<-factor(SSO_mod$target,
                       levels=c('C64','HAdV','Enterovirus','Norovirus','Stx2','Salmonella','Campy'),ordered = TRUE)
ggplot(SSO_mod,
       aes(x = Season,
           y=log_value,
           fill=target)) +
  geom_boxplot()+
  labs(title="Pathogens by Season",
       y= "Target Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()
ggsave("target_boxplot by Season.jpeg")


ggplot(SSO_mod,
       aes(x = Season,
           y=log_value,
           fill=target)) +
  geom_boxplot()+
  facet_wrap(~WWTP)+
  labs(title="Pathogens by Season",
       y= "target Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()
ggsave("target_boxplot by Season and by WWTP.jpeg")

# ggplot(SSO_mod,
#        aes(x = Date,
#            y=log_value,
#            colour=factor(target))) +
#   geom_point()+
#   labs(title="Pathogens by Season",
#        y= "target Concentration (logGC/L)",
#        x="2021-2022")+
#   theme_bw()
# ggsave("target_scatter.jpeg")

ggplot(SSO_mod,
       aes(x = WWTP,
           y=log_value,
           fill=target)) +
  geom_boxplot()+
  labs(title="Pathogens by WWTP",
       y= "target Concentration (logGC/L)",
       x="2021-2022")+
  theme_bw()
ggsave("Target_boxplot by WWTP.jpeg")

####Spearman test#####
cor.test(log_SSO$C64,log_SSO$HAdV,
         method= "spearman",
         exact =F)

cor.test(log_SSO$C64,log_SSO$Enterovirus,
         method= "spearman",
         exact =F)
cor.test(log_SSO$C64,log_SSO$Norovirus,
         method= "spearman",
         exact =F)
cor.test(log_SSO$C64,log_SSO$CoV2,
         method= "spearman",
         exact =F)
cor.test(log_SSO$C64,log_SSO$Stx,
         method= "spearman",
         exact =F)
cor.test(log_SSO$C64,log_SSO$Campy,
         method= "spearman",
         exact =F)
cor.test(log_SSO$C64,log_SSO$Salmonella,
         method= "spearman",
         exact =F)

cor.test(log_SSO$HAdV,log_SSO$Enterovirus,
         method= "spearman",
         exact =F)

cor.test(log_SSO$HAdV,log_SSO$Norovirus,
         method= "spearman",
         exact =F)

cor.test(log_SSO$HAdV,log_SSO$CoV2,
         method= "spearman",
         exact =F)

cor.test(log_SSO$HAdV,log_SSO$Stx,
         method= "spearman",
         exact =F)

cor.test(log_SSO$HAdV,log_SSO$Campy,
         method= "spearman",
         exact =F)

cor.test(log_SSO$HAdV,log_SSO$Salmonella,
         method= "spearman",
         exact =F)
###multiple testing adjustment p value 
library(Hmisc)
log_SSO_2[is.na(log_SSO_2)]=1.5
matrix1<-rcorr(as.matrix(log_SSO_2[,c(4:12)]), type = "spearman")
print(matrix1)


matrix<-rcorr(as.matrix(log_SSO[,c(5:8,10,9,11:12,4)]), type = "spearman")
print(matrix)
capture.output(print(matrix), file = "spearman_corr.csv")
write.table(as.data.frame(matrix),file="spearman_corr.csv", quote=F,sep=",",row.names=F)


#####POST-HOC TEST COMPARISON Season######
C64_Season<-aov(log_SSO$C64~log_SSO$Season)
summary(C64_Season)
TukeyHSD(C64_Season)

HAdV_Season<-aov(log_SSO$HAdV~log_SSO$Season)
summary(HAdV_Season)
TukeyHSD(HAdV_Season)

Enterovirus_Season<-aov(log_SSO$Enterovirus~log_SSO$Season)
summary(Enterovirus_Season)
TukeyHSD(Enterovirus_Season)

Norovirus_Season<-aov(log_SSO$Norovirus~log_SSO$Season)
summary(Norovirus_Season)
TukeyHSD(Norovirus_Season)

CoV2_Season<-aov(log_SSO$CoV2~log_SSO$Season)
summary(CoV2_Season)
TukeyHSD(CoV2_Season)

Stx_Season<-aov(log_SSO$Stx~log_SSO$Season)
summary(Stx_Season)
TukeyHSD(Stx_Season)

Campy_Season<-aov(log_SSO$Campy~log_SSO$Season)
summary(Campy_Season)
TukeyHSD(Campy_Season)

Salmonella_Season<-aov(log_SSO$Salmonella~log_SSO$Season)
summary(Salmonella_Season)
TukeyHSD(Salmonella_Season)


#####POST-HOC TEST COMPARISON WWTP#####
C64_WWTP<-aov(log_SSO$C64~log_SSO$WWTP)
summary(C64_WWTP)
TukeyHSD(C64_WWTP)

HAdV_WWTP<-aov(log_SSO$HAdV~log_SSO$WWTP)
summary(HAdV_WWTP)
TukeyHSD(HAdV_WWTP)

Enterovirus_WWTP<-aov(log_SSO$Enterovirus~log_SSO$WWTP)
summary(Enterovirus_WWTP)
TukeyHSD(Enterovirus_WWTP)

Norovirus_WWTP<-aov(log_SSO$Norovirus~log_SSO$WWTP)
summary(Norovirus_WWTP)
TukeyHSD(Norovirus_WWTP)

CoV2_WWTP<-aov(log_SSO$CoV2~log_SSO$WWTP)
summary(CoV2_WWTP)
TukeyHSD(CoV2_WWTP)

Stx_WWTP<-aov(log_SSO$Stx~log_SSO$WWTP)
summary(Stx_WWTP)
TukeyHSD(Stx_WWTP)

Campy_WWTP<-aov(log_SSO$Campy~log_SSO$WWTP)
summary(Campy_WWTP)
TukeyHSD(Campy_WWTP)

Salmonella_WWTP<-aov(log_SSO$Salmonella~log_SSO$WWTP)
summary(Salmonella_WWTP)
TukeyHSD(Salmonella_WWTP)

max(SSO$Salmonella,na.rm = T)
which(SSO$Salmonella == 2020002, arr.ind=TRUE)
SSO[114,c(1:4)]

####nmds analysis####
##nonlinear dataset, not euclidean distance, e.g., abundance count data (especially of species)
 library(readr)
#  qPCR_mastersheet_noNA6_10_22 <- read_csv("Desktop/EPA paper wastewater pathogen/ANOVA/qPCR_mastersheet.noNA6.10.22.csv", 
#                                            +     col_types = cols(Date = col_date(format = "%m/%d/%y"), 
#                                                                   +         `Sample ID.virus` = col_character()))
#  # View(qPCR_mastersheet_noNA6_10_22)                                                               0s
 pathogen<-pathogen[,c(2,3,6,7,8,9,11,12,13,14)] 
 write.csv(pathogen, "Desktop/EPA paper wastewater pathogen/ANOVA/pathogen.csv", row.names=FALSE)
#  library(vegan)
#  m_pathogen=as.matrix(pathogen)
#  set.seed(123)
#  nmds = metaMDS(m_pathogen, distance = "bray")
# # 'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE
# # Error in distfun(comm, method = distance, ...) : 
#   # input data must be numeric
#  head(pathogen)
# # A tibble: 6 × 14
# # `Sample ID.bacteria` Date       WWTP  `Sample ID.virus`    C64   HAdV Enterovirus Norovirus    Stx
# # <chr>                <date>     <chr> <chr>              <dbl>  <dbl>       <dbl>     <dbl>  <dbl>
#   # 1 B1                   2021-01-13 MF    1                 2.23e8 1.58e1        3761      15.8  583  
# # # 2 B2                   2021-01-13 SC    3                 2.06e8 7.57e4        1541    3691     15.8
# # 3 B3                   2021-01-20 MF    5                 1.95e8 2.45e3         823      15.8 3011  
# # 4 B4                   2021-01-20 SC    7                 4.42e8 1.28e4        2421    7681   9581  
# # 5 B5                   2021-01-27 MF    9                 2.54e8 1.58e1        1061    2321   1051  
# # 6 B6                   2021-01-27 SC    11                9.07e7 4.41e3        1341    9381   7571  
# … with 5 more variables: CoV2 <dbl>, Campy <dbl>, Salmonella <dbl>, Season <chr>, Influent <dbl>
 colnames(pathogen)
# [1] "Sample ID.bacteria" "Date"               "WWTP"               "Sample ID.virus"   
# [5] "C64"                "HAdV"               "Enterovirus"        "Norovirus"         
# [9] "Stx"                "CoV2"               "Campy"              "Salmonella"        
# [13] "Season"             "Influent"          
 patho. = pathogen[,c(3:7)]
 m_patho. = as.matrix(patho.)
 set.seed(123)
 nmds = metaMDS(m_patho., distance = "bray")
 # Square root transformation
 # Wisconsin double standardization
 # Run 0 stress 0.1558284 
 # Run 1 stress 0.1694202 
 # Run 2 stress 0.1617981 
 # Run 3 stress 0.1864552 
 # Run 4 stress 0.167461 
 # Run 5 stress 0.1899869 
 # Run 6 stress 0.1586215 
 # Run 7 stress 0.1644242 
 # Run 8 stress 0.1586593 
 # Run 9 stress 0.1628658 
 # Run 10 stress 0.1725696 
 # Run 11 stress 0.1590617 
 # Run 12 stress 0.156008 
 # ... Procrustes: rmse 0.008554902  max resid 0.072744 
 # Run 13 stress 0.1769834 
 # Run 14 stress 0.1759196 
 # Run 15 stress 0.1623252 
 # Run 16 stress 0.174482 
 # Run 17 stress 0.1729746 
 # Run 18 stress 0.1677691 
 # Run 19 stress 0.1671679 
 # Run 20 stress 0.1701727 
 # *** No convergence -- monoMDS stopping criteria:
 #   15: stress ratio > sratmax
 # 5: scale factor of the gradient < sfgrmin
# *** No convergence -- monoMDS stopping criteria:
#   15: stress ratio > sratmax
# 5: scale factor of the gradient < sfgrmin
 
 nmds
 # Call:
 #   metaMDS(comm = m_patho., distance = "bray") 
 # 
 # global Multidimensional Scaling using monoMDS
 # 
 # Data:     wisconsin(sqrt(m_patho.)) 
 # Distance: bray 
 # 
 # Dimensions: 2 
 # Stress:     0.1558284 
 # Stress type 1, weak ties
 # No convergent solutions - best solution after 20 tries
 # Scaling: centring, PC rotation, halfchange scaling 
 # Species: expanded scores based on ‘wisconsin(sqrt(m_patho.))’ 


 plot(nmds)
 data.scores = as.data.frame(scores(nmds))
 data.scores = as.data.frame(scores(nmds))
#  data.scores$Sample = pc$Sample
# # Error: object 'pc' not found
 View(pathogen)
 data.scores$WWTP = pathogen$WWTP
 data.scores$Date = pathogen$Date
 data.scores$Season = pathogen$Season
 # data.scores$Influent = pathogen$Influent
 head(data.scores)

# NMDS1      NMDS2 WWTP       Date Season
# 1 -0.23690836  0.7041947   MF 2021-01-13 Winter
# 2 -0.43227095 -0.1274195   SC 2021-01-13 Winter
# 3  0.03788453  0.5574387   MF 2021-01-20 Winter
# 4  0.54516923 -0.1405259   SC 2021-01-20 Winter
# 5  0.30734663 -0.1662230   MF 2021-01-27 Winter
# 6  0.46983863 -0.1825379   SC 2021-01-27 Winter

 library(ggplot2)
 
 # xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
 #  +   geom_point(size = 4, aes( shape = WWTP, colour = Season))+ 
 #  +   theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
 #            +         axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
 #            +         legend.text = element_text(size = 12, face ="bold", colour ="black"), 
 #            +         legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
 #            +         axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
 #            +         legend.title = element_text(size = 14, colour = "black", face = "bold"), 
 #            +         panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
 #            +         legend.key=element_blank()) + 
 #  +   labs(x = "NMDS1", colour = "Time", y = "NMDS2", shape = "Type")  + 
 #  +   scale_colour_manual(values = c("#009E73", "#E69F00")) 

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2,color=Season, fill=Season)) + 
  stat_ellipse(geom="polygon",level=0.7, alpha=0.3,show.legend = FALSE)+
  geom_point(size = 1)+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  scale_colour_manual(values = c("#009E73", "#E69F00","#FF0000","gray45")
                      )+
  scale_fill_manual(values = c("palegreen", "wheat","lightpink","gray85")
                      ) 

xx




# ####PCoA analysis####
# ##principle coordinate analysis##
# ###other distance mearue, not euclidean distance, but still linear, e.g. biogeographic data###
# colnames(qPCR_mastersheet_noNA6_10_22)
# log_targets_noNA<-log10(qPCR_mastersheet_noNA6_10_22[,c(6:9,11,12)])
# log_pathogen<-log_targets_noNA
# 
# colnames(log_targets_noNA)
# env <- SSO[,c(2,3,13,14)]
# log_pathogen_noNA<-cbind(env, log_targets_noNA)
# str(log_pathogen_noNA)
# dim(log_pathogen_noNA)
# 
# library(tidyverse)
# #load data
# data("USArrests")
# 
# #view first six rows of data
# head(USArrests)


####select samples for Illumina sequencing###

EQ1e3.less<-subset(SSO,rowSums(SSO[7]<1e3)>0)
write.csv(EQ1e3.less,"/Users/huiyunwu/Desktop/EQ1e3.less.csv",row.names=T)

EQ1e3to4<-subset(SSO,rowSums(SSO[7]>1e3)>0 &rowSums(SSO[,7]<1e4)>0)
write.csv(EQ1e3to4,"/Users/huiyunwu/Desktop/EQ1e3to4.csv",row.names=T)

EQ1e4<-subset(SSO,rowSums(SSO[7]>1e4)>0)
write.csv(EQ1e4,"/Users/huiyunwu/Desktop/EQ1e4.csv",row.names=T)

EQ1e5<-subset(SSO,rowSums(SSO[7]>1e5)>0)
write.csv(EQ1e5,"/Users/huiyunwu/Desktop/EQ1e5.csv",row.names=T)

EQ1e4to5<-subset(SSO,rowSums(SSO[,7]>10000)>0 &rowSums(SSO[,7]<100000)>0)
write.csv(EQ1e4to5,"/Users/huiyunwu/Desktop/EQ1e4to5.csv",row.names=T)

write.csv(SSO,"/Users/huiyunwu/Desktop/SSO.csv",row.names=T)
