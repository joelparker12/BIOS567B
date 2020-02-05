rm(list=ls())
#FEV data set. 
load("C:/Users/Joel Parker/Desktop/Bios 536A/BIOS576B/R/FEV.DAT.rdata")

fev$Sex <- factor(fev$Sex, levels = c(0,1), labels = c("F","M"))

table(fev$Sex)
prop.table(table(fev$Sex))

lm1 <- lm(fev~Age, data = fev, subset = (Sex=="F"))
summary(lm1)
lm2 <- lm(fev~Age, data = fev, subset = Sex=="M" )

summary(lm2)

library(ggplot2)
ggplot(fev, aes(x=Age, y= fev))+
  geom_point()+
  geom_text(aes(x=Age, y= fev, label = Sex), hjust=0, vjust= 0)+
  geom_smooth(data = subset(fev, Sex =="F"), method = 'lm', formula = y~x, aes(col = "lfit-female"))+
  geom_smooth(data = subset(fev, Sex =="M"), method = 'lm', formula = y~x, aes(col = "lfit-Male"))+
  scale_colour_manual(name="legend", values=c("blue", "red"))
  
