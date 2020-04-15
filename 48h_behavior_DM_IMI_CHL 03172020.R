#append .csv files from ethovision, plot behavior
##03022020

library(dplyr)
library(tidyr) 
library(ggplot2)

#load 48h data (group stats, 1 min time bins)
p1 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p1.csv", header = TRUE, stringsAsFactors=FALSE)
p2 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p2.csv", header = TRUE, stringsAsFactors=FALSE)
p3 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p3.csv", header = TRUE, stringsAsFactors=FALSE)
p4 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p4.csv", header = TRUE, stringsAsFactors=FALSE)
p5 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p5.csv", header = TRUE, stringsAsFactors=FALSE)
p6 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p6.csv", header = TRUE, stringsAsFactors=FALSE)
p7 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p7.csv", header = TRUE, stringsAsFactors=FALSE)
p8 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p8.csv", header = TRUE, stringsAsFactors=FALSE)
p9 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p9.csv", header = TRUE, stringsAsFactors=FALSE)
p10 <-read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/48h stats/Statistics-dm02132020_template_p10.csv", header = TRUE, stringsAsFactors=FALSE)  

p1 <- select(p1,-c(6,7,8))

#rbind to append each file to the end of the previous one  
bind <- rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

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
plot1data <- cbind(mean, SE$Distance.moved.1) 

#rename columns
colnames(plot1data) <- c("Treatment", "Timebin","Total_Distance","SE")

#simple plot
qplot(x=Timebin, y=Total_Distance, color=Treatment, data=plot1data)+
  labs( x ="Treatment", y = "Total distance moved (cm)")+
  theme_classic()

#ggplot 
plot2 <- ggplot(data=plot1data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))

plot2 <- plot2+
  geom_line(stat = "identity", position="dodge") +
  theme_classic()+
  labs( x ="Treatment", y = "Total distance moved (cm)")
plot2

# Add SE errorbars use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

plot3 <- ggplot(data=plot1data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "","5","", "", "", "", "", "", "", "", "", "10",
                              "", "", "", "", "","", "","","", "15","", "", "", "", "","","","","","20",
                              "", "", "", "", "","","","","","25","", "", "", "", "", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank())+
  labs( x ="Time (minutes)", y = "Total Distance (mm)")
plot3


##plot by treatment
### IMI low and High (Treatments 1 and 2) with T9 control
####subset just the rows with the correct treatments
plot4data <- plot1data %>% filter(Treatment %in% c("T10", "T3", "T6", "T8"))


plot4 <- ggplot(data=plot4data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+
  geom_rect(data=NULL,aes(xmin=0,xmax=5,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd) +
  scale_colour_manual(values=c("#56B4E9","#999999", "#E69F00", "purple"), 
                      name="Treatment",
                      breaks=c("T10", "T3", "T6","T8"),
                      labels=c("Control","CHL Low", "CHL High:IMI Low", "CHL Low:IMI Low")) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "","10","", "", "", "", "20",
                              "", "", "", "","30","", "", "", "","20",
                              "", "", "", "", "","25","", "", "", "", "","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  labs(title="D. magna Total Distance (48h)",x ="Time (minutes)", y = "Total Distance (mm)")
plot4

#ANOVA 
res.aov <- aov(Distance.moved.1 ~ X, data = plot1)
summary(res.aov)
TukeyHSD(res.aov, "X")

#Identify Light and Dark cycles
Dark = plot1[1:90, 1:4]
res.aovDark <- aov(Distance.moved.1 ~ X, data = Dark)
summary(res.aovDark)
TukeyHSD(res.aovDark, "X")

Light1 = plot1[91:290,1:4]
res.aovLight1 <- aov(Distance.moved.1 ~ X, data = Light1)
summary(res.aovLight1)
TukeyHSD(res.aovLight1, "X")


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


ggplot(data=subset1, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(labels = c("", "", "", "5", "", "", "", "", "10", "", "","","", "15","","","","","20","","","","","25", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank())
