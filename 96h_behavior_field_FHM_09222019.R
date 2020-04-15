#FHM plots - 09222019

#load libraries
library(dplyr)
library(tidyr) 
library(ggplot2)
library(plyr)

#load 96h data (Ethovision exported group stats, 1 min time bins)
data <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/sept_2019/behavior/statsFHM/Statistics-NEW TEMPLATE_stinson_fhm_1minbin.csv", header = TRUE, stringsAsFactors=FALSE)

#sort data by treatment and time bin
neworder <- data[order(data$X,data$X.1),]

#delete extra row header info
edit <- subset(neworder, X.1 != "")

#delete all Start - 01:00, 50:00-51:00 rows (1min bins)
edit1 <- edit[!grepl("Start-0:01:00", edit$X.1),]
edit2 <- edit1[!grepl("0:50:00-0:51:00", edit1$X.1),]

#convert character string to numeric (as.numeric)
str(edit2)
edit2$Distance.moved.1 <- as.numeric(edit2$Distance.moved.1)
edit2$Distance.moved.2 <- as.numeric(edit2$Distance.moved.2)

#calculate mean for all replicates, for each treatment and time point
meany <- aggregate(Distance.moved.1 ~ X + X.1, edit2, mean)
#if re-running this, you must remove the previous object named "meany" or you get an error message:
rm(meany)

#SE for each treatment and time point
SE <- edit2$Distance.moved.2

plot1 <- edit2
str(plot1)

#rename columns
colnames(plot1) <- c("Treatment", "Timebin","","Total_Distance","SE")

#simple plot
qplot(x=Timebin, y=Total_Distance, color=Treatment, data=plot1)+
  labs( x ="Treatment", y = "Total distance moved (mm)")+
  theme_classic()

#ggplot 
plot2 <- ggplot(data=plot1, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))

plot2 <- plot2+
  geom_line(stat = "identity", position="dodge") +
  theme_classic()+
  labs( x ="Treatment", y = "Total distance moved (cm)")
plot2

# Add SE errorbars use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(data=plot1, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(labels = c("","", "", "", "5", "", "", "", "", "10", "", "","","", "15","","","","","20","","","","","25", "","","","","30", "","","","","35", "","","","","40", "","","","","45", "","","","","50")) +
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank())

plot4 <- ggplot(data=plot1data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_rect(data=NULL,aes(xmin=0,xmax=9,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_rect(data=NULL,aes(xmin=19,xmax=29,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_rect(data=NULL,aes(xmin=39,xmax=49,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd) +
  scale_colour_manual(values=c("#56B4E9","#999999", "#E69F00", "purple"), 
                      name="Treatment",
                      breaks=c("Control", "T1 100", "T2 100","T3 100"),
                      labels=c("Control","T1 100", "T2 100", "T3 100")) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "","10","", "", "", "", "", "", "", "", "", "20",
                              "", "", "", "", "","", "","","", "30","", "", "", "", "","","","","","40",
                              "", "", "", "", "","","","","","50","", "", "", "", "", "","","","","60")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  labs(title="P. promelas Total Distance (96h)",x ="Time (minutes)", y = "Total Distance (mm)")
plot4

#add dotted black line at 10min light stim
plot4 + geom_vline(xintercept = 10, linetype="dotted", 
                   color = "black", size=1.5)

#ANOVA 
##more info here: http://www.sthda.com/english/wiki/one-way-anova-test-in-r
##Run one-way ANOVA to determine if there are differences in Total Distance between groups. This compares accross all groups.
res.aov <- aov(Total_Distance ~ Treatment + Timebin, data = plot1)
summary(res.aov)

##To do multiple pairwise comparisons, use Tukey HSD
TukeyHSD(res.aov, "Treatment")

#To look at significance within Light/Dark cycle only
##Identify Light and Dark cycles, then run ANOVA and TUKEY HSD
Dark = plot1[1:90, 1:4]
res.aovDark <- aov(Total_Distance ~ Treatment + Timebin, data = Dark)
summary(res.aovDark)
TukeyHSD(res.aovDark, "Treatment")

Light1 = plot1[91:290,1:4]
res.aovLight1 <- aov(Total_Distance ~ Treatment + Timebin, data = Light1)
summary(res.aovLight1)
TukeyHSD(res.aovLight1, "Treatment")

######
#Plot significant treatments v controls
#subset T3 and T8, which are significant
subset1 <- plot1[plot1$Treatment == "T3" & "T6" & "T8" & "T10",]

subset1 <- plot1 %>% filter(Treatment %in% c("T3", "T6", "T8", "T10"))

#graph just those vs control
plot3 <- ggplot(data=subset1, aes(x=X.1, y=Distance.moved.1, color = X, group=X))
plot3 <- plot3+
  geom_line(stat = "identity", position="dodge") +
  theme_classic()+
  labs( x ="Treatment", y = "Total distance moved (cm)")
plot3

#rename columns
colnames(subset1) <- c("Treatment", "Timebin","Total_Distance","SE")

# Add SE errorbars use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right


ggplot(data=plot1, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_rect(data=NULL,aes(xmin=7.25,xmax=8.75,ymin=-Inf,ymax=Inf),
            fill="darkgreen")+
  geom_line(position=pd) +
  geom_point(position=pd) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(labels = c("", "", "", "5", "", "", "", "", "10", "", "","","", "15","","","","","20","","","","","25", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank())
