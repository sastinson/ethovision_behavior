#graphs and basic stats for Ethovision behavior data
#modified Code from Florian for graphing and analyzing Ethovision behavior data
#01082020
library(dplyr)
library(plyr)
library(tidyverse)
library(ggplot2)

data = read.table("LFS_distance moved_analyze.csv", sep=";", header=T)
data= subset(data, Distance.moved != 0)
data= subset(data, Distance.moved != "-")
data=subset(data, Treatment !="")

data$Treatment<- factor(data$Treatment, levels = c("Control", "Vehicle", "LD", "MD", "HD"))


data$Time.bin=as.numeric(data$Time.bin)
data$Distance.moved=as.numeric(data$Distance.moved)

means.sem <- ddply(data, c("Treatment", "Time.bin"), summarise,
                   mean=mean(Distance.moved), sem=sd(Distance.moved)/sqrt(length(Distance.moved)))
means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)

#plot Total distance with 1 min bins
qplot(x=Time.bin, y=mean, color=Treatment, data=means.sem)+
  geom_errorbar(aes(ymax=upper, ymin=lower), data=means.sem)+
  labs( x ="Time bin (min)", y = "Total distance moved (cm)")+
  theme_classic()

#Identify Light and Dark cycles
Dark1 = data %>% filter (Time.bin>=5 & Time.bin<15)
res.aovDark1 <- aov(Distance.moved ~ Treatment + Time.bin, data = Dark1)
summary(res.aovDark1)
TukeyHSD(res.aovDark1, "Treatment")

Light1 = data %>% filter (Time.bin>=15 & Time.bin<20)
res.aovLight1 <- aov(Distance.moved ~ Treatment + Time.bin, data = Light1)
summary(res.aovLight1)
TukeyHSD(res.aovLight1, "Treatment")

Dark2 = data %>% filter (Time.bin>=20 & Time.bin<30)
res.aovDark2 <- aov(Distance.moved ~ Treatment + Time.bin, data = Dark2)
summary(res.aovDark2)
TukeyHSD(res.aovDark2, "Treatment")

Light2 = data %>% filter (Time.bin>=30 & Time.bin<35)
res.aovLight2 <- aov(Distance.moved ~ Treatment + Time.bin, data = Light2)
summary(res.aovLight2)
TukeyHSD(res.aovLight2, "Treatment")

Dark3 = data %>% filter (Time.bin>=30 & Time.bin<45)
res.aovDark3 <- aov(Distance.moved ~ Treatment + Time.bin, data = Dark3)
summary(res.aovDark3)
TukeyHSD(res.aovDark3, "Treatment")

##################
##################
##################


means.sem2 <- ddply(data, c("Treatment", "Light.Dark"), summarise,
                    mean=mean(Distance.moved), sem=sd(Distance.moved)/sqrt(length(Distance.moved)))
means.sem2 <- transform(means.sem2, lower=mean-sem, upper=mean+sem)

qplot(x=Treatment, y=mean, color=Light.Dark, data=means.sem2)+
  geom_errorbar(aes(ymax=upper, ymin=lower), data=means.sem2)+
  labs( x ="Treatment", y = "Total distance moved (cm)")+
  theme_classic()

#Bar plot 
means.barplot <- ggplot(data=means.sem2, aes(x=Treatment, y=mean, fill=Light.Dark))
means.barplot <- means.barplot+
  geom_bar(stat = "identity", position="dodge") +
  geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(), data=means.sem2)+
  theme_classic()+
  labs( x ="Treatment", y = "Total distance moved (cm)")
means.barplot

#ANOVA for each Light or Dark period
res.aovDarkLight <- aov(Distance.moved ~ Treatment + Light.Dark+ Treatment:Light.Dark, data = data)
summary(res.aovDarkLight)
TukeyHSD(res.aovDarkLight, "Treatment")