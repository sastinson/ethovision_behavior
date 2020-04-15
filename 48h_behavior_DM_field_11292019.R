#plot 48h DM behavior from Ethovision software output (group stats)
##options given for diff time bins
##03212020

#install any packages that you don't already have. 
#You only have to do this once!
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot")

#load libraries
library(dplyr)
library(tidyr) 
library(ggplot2)
library(plyr)

#load 48h data (Ethovision exported group stats, 30 min time bins)
data <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/nov_2019/behavior/stats/Statistics-11292019_DM_redo03132020_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)

#sort data by treatment and time bin
neworder <- data[order(data$X,data$X.1),]

#delete extra row header info
edit <- subset(neworder, X.1 != "")

#delete all Start - 00:30, 30:00 - 30:30 rows (30 sec)
edit1 <- edit[!grepl("Start-0:00:30", edit$X.1),]
#edit2 <- edit2[!grepl("0:29:30-0:30:00", edit1$X.1),]
edit2 <- edit1[!grepl("0:30:00-0:30:30", edit1$X.1),]

#convert character string to numeric (as.numeric)
str(edit2)
edit2$Distance.moved.1 <- as.numeric(edit2$Distance.moved.1)
edit2$Distance.moved.2 <- as.numeric(edit2$Distance.moved.2)

#subset treatment, timebin, total distcance and SE
plot1data <- edit2[,1:5]
plot1data$Distance.moved <- NULL

#rename columns
colnames(plot1data) <- c("Treatment", "Timebin","Total_Distance","SE")

#simple plot
qplot(x=Timebin, y=Total_Distance, color=Treatment, data=plot1data)+
  labs( x ="Treatment", y = "Total distance moved (mm)")+
  theme_classic()

#ggplot 
plot2 <- ggplot(data=plot1data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))

plot2 <- plot2+
  geom_line(stat = "identity", position="dodge") +
  theme_classic()+
  labs( x ="Time (minutes)", y = "Total distance moved (mm)")
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

plot4 <- ggplot(data=plot1data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+
  geom_rect(data=NULL,aes(xmin=0,xmax=19,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd) +
  scale_colour_manual(values=c("#56B4E9","#999999", "#E69F00", "purple"), 
                      name="Treatment",
                      breaks=c("Control", "T1 100", "T2 100","T3 100"),
                      labels=c("Control","T1 100", "T2 100", "T3 100")) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "","5","", "", "", "", "", "", "", "", "", "10",
                              "", "", "", "", "","", "","","", "15","", "", "", "", "","","","","","20",
                              "", "", "", "", "","","","","","25","", "", "", "", "", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  labs(title="D. magna Total Distance (48h)",x ="Time (minutes)", y = "Total Distance (mm)")
plot4

#add dotted black line at 10min light stim
plot4 + geom_vline(xintercept = 20, linetype="dotted", 
                   color = "black", size=1.5)