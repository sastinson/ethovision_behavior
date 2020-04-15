#append .csv files from ethovision, plot behavior
##03062020

library(dplyr)
library(tidyr) 
library(ggplot2)

#load 96h data (group stats, 1 min time bins)
p1 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/hadeel bader/stats/Statistics-dm02162020_plate1.csv", header = TRUE, stringsAsFactors=FALSE)
p2 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/hadeel bader/stats/Statistics-dm02162020_plate2.csv", header = TRUE, stringsAsFactors=FALSE)
p3 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/hadeel bader/stats/Statistics-dm02162020_plate3.csv", header = TRUE, stringsAsFactors=FALSE)

#rbind to append each file to the end of the previous one  
bind <- rbind(p1, p2, p3)

#sort by treatment and time bin
neworder <- bind[order(bind$X,bind$X.1),]

#delete extra row header info
edit <- subset(neworder, X.1 != "")

#delete all Start - 01:00, 30:00-31:00 rows
edit1 <- edit[!grepl("Start-0:01:00", edit$X.1),]
edit2 <- edit1[!grepl("0:30:00-0:31:00", edit1$X.1),]

#convert character string to numeric (as.numeric)
str(edit2)
edit2$Distance.moved.1 <- as.numeric(edit2$Distance.moved.1)

#calculate mean for all replicates, for each treatment and time point
mean <- aggregate(Distance.moved.1 ~ X + X.1, edit2, mean)

#calc SE for each treatment and time point
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
SE <- aggregate(Distance.moved.1 ~ X + X.1, edit2, st.err)

#add SE as a solumn to mean values
plot1 <- cbind(mean, SE$Distance.moved.1) 

#rename columns
colnames(plot1) <- c("Treatment", "Timebin","Total_Distance","SE")

#simple plot
qplot(x=Timebin, y=Total_Distance, color=Treatment, data=plot1)+
  labs( x ="Treatment", y = "Total distance moved (cm)")+
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
  scale_x_discrete(labels = c("", "", "", "5", "", "", "", "", "10", "", "","","", "15","","","","","20","","","","","25", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank())

#ANOVA 
res.aov <- aov(Total_Distance ~ Treatment, data = plot1)
summary(res.aov)
TukeyHSD(res.aov, "Treatment")

#Identify Light and Dark cycles
Dark = plot1[1:90, 1:4]
res.aovDark <- aov(Total_Distance ~ Treatment, data = Dark)
summary(res.aovDark)
TukeyHSD(res.aovDark, "Treatment")

Light1 = plot1[91:290,1:4]
res.aovLight1 <- aov(Total_Distance ~ Treatment, data = Light1)
summary(res.aovLight1)
TukeyHSD(res.aovLight1, "Treatment")


######

#subset T3 and T8, which are significant
subset1 <- plot1[plot1$X == "T3" & "T6" & "T8" & "T10",]

subset1 <- plot1 %>% filter(X %in% c("T3", "T6", "T8", "T10"))

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
  geom_line(position=pd) +
  geom_point(position=pd) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(labels = c("", "", "", "5", "", "", "", "", "10", "", "","","", "15","","","","","20","","","","","25", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank())
