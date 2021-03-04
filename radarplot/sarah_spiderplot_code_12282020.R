#spider plot code from Paige Mundy
#12292020
#https://ourednik.info/maps/2020/04/29/radar-charts-with-r/
#https://towardsdatascience.com/radar-chart-in-r-222c30866b1b

##load packages
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

##load practice data
spider_data<- read.csv("/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/behavior/spider plot code/spider_plot_data.csv")

spider_data <- spider_data %>% 
  mutate(Label = fct_relevel(Label, "Dark1", "Light1", "Dark2", "Light2", "Dark3"),
         Treatment = fct_relevel(Treatment, "Vehicle", "Low", "Med", "High"),
         Parameter = fct_relevel(Parameter, "Cruising\nDuration",
                                 "Cruising\nFrequency", "Bursting\nDuration",
                                 "Bursting\nFrequency", "Freezing\nDuration",
                                 "Freezing\nFrequency", "Velocity")) #had to restructure to correct the order

##plot aesthetics/labels

label1 <- paste("Vehicle")
label2 <- expression(0.05*mu*g/L)
label3 <- expression(0.5*mu*g/L)
label4 <- expression(5*mu*g/L)
labellist <- list(label1, label2, label3, label4) #I only do this so I can add the mu symbol, if you don't need greek letters you don't need this


colors <- c("gray20",brewer.pal(4,"YlGnBu")[c(2,3,4)]) #just to have a nice color gradient,  use any you like

spider_plot <- spider_data %>%
  arrange(as.numeric(Parameter)) %>% 
  ggplot(aes(x=Parameter, y=Value, group=rev(Treatment), color=Treatment, fill=Treatment)) +
  geom_polygon(fill = NA, size=2) + 
  geom_point(data = filter(spider_data, Treatment == "High"), size=5, alpha = 0.8, color="black", shape=21) +
  geom_point(data = filter(spider_data, Treatment == "Med"), size=5, alpha = 0.8, color="black", shape=21) +
  geom_point(data = filter(spider_data, Treatment == "Low"), size=5, alpha = 0.8, color="black", shape=21) +
  geom_point(data = filter(spider_data, Treatment == "Vehicle"), size=5, alpha = 0.8, color="black", shape=21) + #ok this repetitive geom_point is dumb, it should work with rev in aes- i think it might be my version of R but I'm terrified to upgrade
  ylim(-2, 2) +
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
  labs(title = "48 h Exposure") +
  ylab("Z-score") +
  scale_colour_manual(values = colors, 
                      labels = labellist) +
  scale_fill_manual(values = rev(colors), guide = F) +
  geom_text(x=0.5, y=0, label="0", fontface=2, color="black", size=4) +
  geom_text(x=0.5, y=-1, label="-1", fontface=2, color="black", size=4) +
  geom_text(x=0.5, y=-2, label="-2", fontface=2, color="black", size=4) +
  geom_text(x=0.5, y=1, label="1", fontface=2, color="black", size=4) +
  geom_text(x=0.5, y=2, label="2", fontface=2, color="black", size=4)

spider_plot

ggsave("spider_plot.pdf", plot = spider_plot, width = 36, height = 20, units  = 'in', dpi = 300, limitsize = T)
#adjust size accordingly