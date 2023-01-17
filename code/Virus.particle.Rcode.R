
library(ggplot2)

STAR<-read.csv(file="/Users/huiyunwu/Desktop/R/47mm.data.cp.L.csv")
colnames(data)
dim(data)
# colnames(STAR)<-c("Sample.ID","WWTP","Collection.date","Pore.size","AdV.JTVX.cp","Enterovirus.cp","Norovirus.cp","MS2.cp","crAssphage.56.cp")
# STAR<-data[,c(1:5,6,8,10,12,14)]

# sapply(STAR, class)
# # str(STAR)
# STAR[,c(6:10)]<-sapply(STAR[,c(6:10)], as.numeric)
summary(STAR)
dim(STAR)
colnames(STAR)
STAR_mod <- cbind(STAR[1:5], stack(STAR[6:10]))

colnames(STAR_mod)[7]<-"Target"

# STAR_mod$values<-STAR_mod$values+1
STAR_mod$log_values<-log10(STAR_mod$values)

STAR_mod$Pore.size[STAR_mod$Pore.size=='0.45um filtrate HFUF+InvP']<-'filtrate'
STAR_mod$Pore.size[STAR_mod$Pore.size=='0.45um filtrate HFUF+ InvP']<-'filtrate'

STAR_mod$Pore.size<-factor(STAR_mod$Pore.size,
                       levels=c('100 um','20 um','3 um','0.45um','filtrate'),ordered = TRUE)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(STAR_mod,
       aes(x = Pore.size,
           y=log_values,
           fill=Target,
           na.rm=TRUE)) +
  geom_boxplot()+
  scale_fill_manual(values=cbPalette)+
  labs(title="Particle associated viruses",
       y= "Target Concentration (logGC/L)",
       x="Membrane pore size")+
  theme_bw()


# ggplot(STAR_mod,
#        aes(x = WWTP,
#            y=log_values,
#            fill=target)) +
#   geom_boxplot()+
#   labs(title="Particle associated viruses",
#        y= "Target Concentration (logGC/L)",
#        x="2022")+
#   facet_wrap(~PoreSize)+
#   theme_bw()

write.csv(STAR,file="/Users/huiyunwu/Desktop/R/47mm.data.cp.L.csv",row.names = FALSE)
STAR<-read.csv(file="/Users/huiyunwu/Desktop/R/47mm.data.cp.L.csv")
