#plot 96h DM behavior from Ethovision software output (group stats)
##options given for diff time bins
##03062020

#install any packages that you don't already have. 
#You only have to do this once!
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot")

#load libraries
library(dplyr)
library(tidyr) 
library(ggplot2)

#load 96h data (Ethovision exported group stats, 1 min time bins)
p1 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p1.csv", header = TRUE, stringsAsFactors=FALSE)
p2 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p2.csv", header = TRUE, stringsAsFactors=FALSE)
p3 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p3.csv", header = TRUE, stringsAsFactors=FALSE)
p4 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p4.csv", header = TRUE, stringsAsFactors=FALSE)
p5 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p5.csv", header = TRUE, stringsAsFactors=FALSE)
p6 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p6.csv", header = TRUE, stringsAsFactors=FALSE)
p7 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p7.csv", header = TRUE, stringsAsFactors=FALSE)
p8 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p8.csv", header = TRUE, stringsAsFactors=FALSE)
p9 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p9.csv", header = TRUE, stringsAsFactors=FALSE)
p10 <-read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/Statistics-dm02172020_template96_p10.csv", header = TRUE, stringsAsFactors=FALSE)  


#30sec time bins
p1 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_plate1_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p2 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_plate2_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p3 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p3_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p4 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p4_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p5 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p5_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p6 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p6_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p7 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p7_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p8 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p8_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p9 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p9_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)
p10 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/csv30sec/Statistics-dm02172020_template96_p10_30secbin.csv", header = TRUE, stringsAsFactors=FALSE)

#30sec time bins with 0.2mm smoothing
p1 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_plate1_30sec_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p2 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_plate2_30sec_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p3 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_P3_30secbin_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p4 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_p4_30secbin_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p5 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_p5_30secbin_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p6 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_p6_30secbin_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p7 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_p7_30secbin_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p8 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_p8_30secbin_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p9 <- read.csv("/Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_p9_30sec_smooth.csv", header = TRUE, stringsAsFactors=FALSE)
p10 <- read.csv("//Users/WeeRedLass/Box Sync/DPR_2019/exposures/data/IMI-CHL-2020/96h stats/30secbins/30secsmooth/Statistics-dm02172020_template96_p10_30secbin_smooth.csv", header = TRUE, stringsAsFactors=FALSE)


#rbind to append each file to the end of the previous one  
bind <- rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

#sort data by treatment and time bin
neworder <- bind[order(bind$X,bind$X.1),]

#delete extra row header info
edit <- subset(neworder, X.1 != "")

#delete all Start - 01:00, 30:00-31:00 rows (1min bins)
edit1 <- edit[!grepl("Start-0:01:00", edit$X.1),]
edit2 <- edit1[!grepl("0:30:00-0:31:00", edit1$X.1),]

#delete all Start - 00:30, 30:00 - 30:30 rows (30 sec)
edit1 <- edit[!grepl("Start-0:00:30", edit$X.1),]
edit2 <- edit1[!grepl("0:30:00-0:30:30", edit1$X.1),]
edit2 <- edit2[!grepl("0:29:30-0:30:00", edit1$X.1),]

#convert character string to numeric (as.numeric)
str(edit2)
edit2$Distance.moved.1 <- as.numeric(edit2$Distance.moved.1)

#calculate mean for all replicates, for each treatment and time point
meany <- aggregate(Distance.moved.1 ~ X + X.1, edit2, mean)

rm(meany)

#calc SE for each treatment and time point
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
SE <- aggregate(Distance.moved.1 ~ X + X.1, edit2, st.err)

#add SE as a column
plot1data <- cbind(meany, SE$Distance.moved.1) 

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

##plot by treatment
### IMI low and High (Tretaments 1 and 2) with T9 control
####subset just the rows with the correct treatments
plot4data <- plot1data %>% filter(Treatment %in% c("T1", "T2", "T9"))

plot4 <- ggplot(data=plot4data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_rect(data=NULL,aes(xmin=0,xmax=19,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd) +
  scale_colour_manual(values=c("#56B4E9","#999999", "#E69F00"), 
                      name="Imidacloprid",
                      breaks=c("T9","T1", "T2"),
                      labels=c("Control","Low", "High")) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "","5","", "", "", "", "", "", "", "", "", "10",
                              "", "", "", "", "","", "","","", "15","", "", "", "", "","","","","","20",
                              "", "", "", "", "","","","","","25","", "", "", "", "", "","","","","30")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  labs(title="D. magna Total Distance (96h)",x ="Time (minutes)", y = "Total Distance (mm)")
plot4


### CHL low and High (Tretaments 3 and 4) with T10 control
####subset just the rows with the correct treatments
plot5data <- plot1data %>% filter(Treatment %in% c("T3", "T4", "T10"))

plot5 <- ggplot(data=plot5data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_rect(data=NULL,aes(xmin=0,xmax=19,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd) +
  scale_colour_manual(values=c("#56B4E9","#999999", "#E69F00"), 
                          name="Chlorantraniliprole",
                          breaks=c("T10", "T3", "T4"),
                          labels=c("Control", "Low", "High")) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "","5","", "", "", "", "", "", "", "", "", "10",
                              "", "", "", "", "","", "","","", "15","", "", "", "", "","","","","","20",
                              "", "", "", "", "","","","","","25","", "", "", "", "", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  labs(title="D.magna Total Distance (96h)", x ="Time (minutes)", y = "Total Distance (mm)")
plot5


### Mix low low and high high (Tretaments 5 and 8) with T10 control
####subset just the rows with the correct treatments
plot6data <- plot1data %>% filter(Treatment %in% c("T5", "T8", "T10"))

plot6 <- ggplot(data=plot6data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_rect(data=NULL,aes(xmin=0,xmax=19,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd) +
  scale_colour_manual(values=c("#56B4E9","#999999", "#E69F00"), 
                      name="CHL:IMI",
                      breaks=c("T10", "T5", "T8"),
                      labels=c("Control", "Low:Low", "High:High")) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "","5","", "", "", "", "", "", "", "", "", "10",
                              "", "", "", "", "","", "","","", "15","", "", "", "", "","","","","","20",
                              "", "", "", "", "","","","","","25","", "", "", "", "", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  labs(title= "D. magna Total Distance (96h)",x ="Time (minutes)", y = "Total Distance (mm)")
plot6


### Mix low IMI high  CHL and high IMI low CHL (Tretaments 6 and 7) with T10 control
####subset just the rows with the correct treatments
plot7data <- plot1data %>% filter(Treatment %in% c("T6", "T7", "T10"))

plot7 <- ggplot(data=plot7data, aes(x=Timebin, y=Total_Distance, color = Treatment, group=Treatment))+ 
  geom_rect(data=NULL,aes(xmin=0,xmax=19,ymin=-Inf,ymax=Inf),
            fill="gray", linetype=0,alpha=0.5)+
  geom_errorbar(aes(ymin=Total_Distance-SE, ymax=Total_Distance+SE), width=.1, position=pd) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd) +
  scale_colour_manual(values=c("#56B4E9","#999999", "#E69F00"), 
                      name="CHL:IMI",
                      breaks=c("T10", "T6", "T7"),
                      labels=c("Control", "Low:High", "High:Low")) +
  theme(axis.text.x = element_text(angle = 90))+ #scale labels are for 30sec time bins.
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "","5","", "", "", "", "", "", "", "", "", "10",
                              "", "", "", "", "","", "","","", "15","", "", "", "", "","","","","","20",
                              "", "", "", "", "","","","","","25","", "", "", "", "", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  labs(title= "D. magna Total Distance (96h)",x ="Time (minutes)", y = "Total Distance (mm)")
plot7

#ANOVA 
##more info here: http://www.sthda.com/english/wiki/one-way-anova-test-in-r
##Run one-way ANOVA to determine if there are differences in Total Distance between groups. This compares accross all groups.
res.aov <- aov(Total_Distance ~ Treatment, data = plot1)
summary(res.aov)

##To do multiple pairwise comparisons, use Tukey HSD
TukeyHSD(res.aov, "Treatment")

#To look at significance within Light/Dark cycle only
##Identify Light and Dark cycles, then run ANOVA and TUKEY HSD
Dark = plot1[1:189, 1:4]
res.aovDark <- aov(Total_Distance ~ Treatment, data = Dark)
summary(res.aovDark)
TukeyHSD(res.aovDark, "Treatment")

Light1 = plot1[190:588,1:4]
res.aovLight1 <- aov(Total_Distance ~ Treatment, data = Light1)
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
  geom_line(position=pd) +
  geom_point(position=pd) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(labels = c("", "", "", "5", "", "", "", "", "10", "", "","","", "15","","","","","20","","","","","25", "","","","","30")) +
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank())
