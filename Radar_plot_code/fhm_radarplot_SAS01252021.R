#Run this script last after running fhm_stats_radarplot_SAS01192021.R
#Script produces radar plots from velocity behavior data
#adapted from Dr. Paige Mundy https://github.com/insideafish/larvae_behavior/
#last edited 03042021

##load packages
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

##load data
#CZ_merge but the join is not working...see notes about excel file changes
#spider_data<- read.csv("/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/CZ_merge3.csv", header = TRUE)
#str(spider_data)

#spider_data <- CZ_merge3 %>% 
#  mutate(Label = fct_relevel(Label, "Dark1", "Light1", "Dark2", "Light2", "Dark3"),
#         Treatment = fct_relevel(Treatment, "Control", "T1", "T2", "T3"),
#         Parameter = fct_relevel(Parameter, "Cruising\nDuration",
#                                 "Cruising\nFreq", "Turn\nAngle",
#                                "Angular\nVelocity", "Freezing\nDuration",
#                                 "Freezing\nFreq", "Velocity", "Total\nDistance")) #had to restructure to correct the order

##plot aesthetics/labels

label1 <- paste("Control")
label2 <- paste("Sal_Quail")
label3 <- paste("Sal_Hartnell")
label4 <- paste("Sal_Davis")
labellist <- list(label1, label2, label3, label4) #I only do this so I can add the mu symbol, if you don't need greek letters you don't need this


colors <- c("gray20",brewer.pal(4,"YlGnBu")[c(2,3,4)]) #just to have a nice color gradient,  use any you like

spider_plot <- CZ_merge3 %>%
  arrange(as.numeric(Parameter)) %>% 
  ggplot(aes(x=Parameter, y=Value, group=rev(Treatment), color=Treatment, fill=Treatment)) +
  geom_polygon(fill = NA, size=1) + 
  geom_point(data = filter(spider_data, Treatment == "T3"), size=1, alpha = 0.8, color="black", shape=21) +
  geom_point(data = filter(spider_data, Treatment == "T2"), size=1, alpha = 0.8, color="black", shape=21) +
  geom_point(data = filter(spider_data, Treatment == "T1"), size=1, alpha = 0.8, color="black", shape=21) +
  geom_point(data = filter(spider_data, Treatment == "Control"), size=1, alpha = 0.8, color="black", shape=21) + #ok this repetitive geom_point is dumb, it should work with rev in aes- i think it might be my version of R but I'm terrified to upgrade
  ylim(-1, 1) +
  theme_bw() +
  facet_grid(. ~ Label) +
  theme(panel.grid.major = element_line(colour = "gray"), 
        axis.text = element_text(size=15, face = 'bold', color="black"),
        axis.title.x = element_text(size=10), 
        axis.title.y= element_text(size=10), 
        plot.title = element_text(size=10), 
        legend.text = element_text(size=10),
        legend.title = element_text(size=10, color="black"), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 5), #changes parameter text
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size=10), 
        plot.margin=unit(c(-5,1,-5,1), "cm")) +
  coord_polar(clip="off") + 
  labs(title = "96 h Exposure") +
  ylab("Z-score") +
  scale_colour_manual(values = colors, 
                      labels = labellist) +
  #Add scale labels to each plot
  #scale_fill_manual(values = rev(colors), guide = F) +
  #geom_text(x=0.5, y=0, label="0", fontface=2, color="black", size=4) +
  #geom_text(x=0.5, y=-1, label="-1", fontface=2, color="black", size=4) +
  #geom_text(x=0.5, y=-2, label="-2", fontface=2, color="black", size=4) +
  #geom_text(x=0.5, y=1, label="1", fontface=2, color="black", size=4) +
  #geom_text(x=0.5, y=2, label="2", fontface=2, color="black", size=4) +
  scale_fill_manual(values = rev(colors), guide = F) +
  theme(legend.position = "bottom")

spider_plot


##plot with significance
spider_plot2 <- CZ_merge3 %>% 
  arrange(as.numeric(Parameter)) %>%
  ggplot(aes(x=Parameter, y=Value, group=rev(Treatment), color=Treatment)) + 
  #scale_colour_manual(name="Treatment",
                      #values = colors,
                      #labels = labels) +
  scale_fill_manual(values = rev(colors), guide=F) +
  geom_polygon(fill = NA, size=1, ) + 
  ylim(-1, 1) +
  theme_bw() +
  facet_grid(~Label) +
  theme(panel.grid.major = element_line(colour = "gray"),
        plot.margin = margin(0,0,0,0),
        axis.text.x = element_text(size=6.2, color = "black", face = "bold"),
        strip.text = element_text(size=10),
        axis.title = element_text(size=14),
        plot.title = element_text(size=18)) +
  coord_polar(clip="off") +
  geom_point(data = filter(CZ_merge3, Treatment == "T3"), size=1.3, alpha = 0.5, color="black", shape=20, fill="darkorchid1", stroke=0.8) +
  geom_point(data = filter(CZ_merge3, Treatment == "T2"), size=1.3, alpha = 0.5, color="black", shape=20, fill="darkgoldenrod1", stroke=0.8) +
  geom_point(data = filter(CZ_merge3, Treatment == "T1"), size=1.3, alpha = 0.5, color="black", shape=20, fill="brown4", stroke=0.8) +
  geom_point(data = filter(CZ_merge3, Treatment == "Control"), size=1.3, alpha = 0.5, color="black", shape=20, fill="black", stroke=0.8) +
  geom_point(data = filter(CZ_merge3, Significance == "p<0.05"), size=3, alpha = 0.7, color = "blue", shape=23, stroke = 1.5) +
  geom_point(data=filter(CZ_merge3, Significance == "p<0.01"), size=3, alpha = 0.7, color="red", shape=23, stroke = 1.5) +
  labs(title = "96 h") +
  ylab("Z-score") +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c("black", "brown4", "darkgoldenrod1","darkorchid1"), 
                      labels = labellist)

spider_plot2

ggsave("/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/fhm_behavior_spider_plot_02082021.pdf", plot = spider_plot, width = 36, height = 20, units  = 'in', dpi = 300, limitsize = T)
#adjust size accordingly

##load key for figure data
#CZ_merge but the join is not working...see notes about excel file changes
key_spider_data<- read.csv("/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/spider_plot_key2.csv", header = TRUE)
str(key_spider_data)

##plot aesthetics/labels
spider_plot3 <- key_spider_data %>% 
  #arrange(as.numeric(Parameter)) %>%
  ggplot(aes(x=Parameter, y=Value, group=rev(Treatment), color=Treatment)) + 
  scale_fill_manual(values = rev(colors), guide=F) +
  geom_text(x=0.5, y=0, label="0", fontface=2, color="black", size=4) +
  geom_text(x=0.5, y=-0.8, label="-1", fontface=2, color="black", size=4) +
  #geom_text(x=0.5, y=-2, label="-2", fontface=2, color="black", size=4) +
  geom_text(x=0.5, y=0.8, label="1", fontface=2, color="black", size=4) +
  #geom_text(x=0.5, y=2, label="2", fontface=2, color="black", size=4) +
  geom_polygon(fill = NA, size=1, ) + 
  ylim(-1, 1) +
  theme(panel.grid.major = element_line(colour = "gray"),
        plot.margin = margin(0,0,0,0),
        axis.text.x = element_text(size=0),
        strip.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.title = element_text(size=18)) +
  coord_polar(clip="off") +
  geom_point(data = filter(key_spider_data, Treatment == "Control"), size=1.3, alpha = 0.5, color="black", shape=21, fill="black", stroke=0.6) +
  labs(title = "Light Cycle") +
  ylab("Z-score") +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = colors, 
                      labels = labellist)

spider_plot3
