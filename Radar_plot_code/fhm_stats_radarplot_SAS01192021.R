#stats/plot for fhm behavior radar graph

#Run this script first, then use fhm_radarplot_SAS01252021.R to make spider plot
#Script produces CZMerge3 file to use for making spider plots from velocity behavior data
#adapted from Dr. Paige Mundy https://github.com/insideafish/larvae_behavior/
#last edited 03042021

---
title: "fhm_behavior_stats_andgraph"
output: html_document
---
#  ```{r}
library(tidyr)
library(forcats)
library(purrr)
library(dplyr)
library(magrittr)
library(broom)
library(plotrix)
library(emmeans)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(rstatix)
#```



#upload data
#```{r}
excel.names <- c("Treatment",
                 "Trial",
                 "Well",
                 "Time",
                 "Velocity",
                 "Cruising Freq",
                 "Cruising duration",
                 "Bursting Freq",
                 "Bursting duration",
                 "Freezing freq",
                 "Freezing duration",
                 "Turn Angle", 
                 "Meander",
                 "Angular Velocity", 
                 "Total Distance")

c1 <- read.table("/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/fhm_behaviorplotdata_01192021.txt", 
                 sep = "\t", 
                 skip = 1,
                 col.names = excel.names)
#```

#Tidy data
#```{r}
Ctidy.master.df <- c1 %>% 
  separate(col = Time, into = c("InitialTime", "FinalTime"), sep = "-") %>% 
  mutate(InitialTime = ifelse(InitialTime == "Start", "0:00:00", InitialTime)) %>% 
  separate(col = InitialTime, into = c("Hours", "Minutes", "Seconds"), sep = ":", remove = F) %>% 
  select(-Hours, -Seconds) %>% 
  mutate(Minutes = as.numeric(Minutes)) %>%
  mutate(Label = case_when( 
    Minutes >= 0 & Minutes < 10 ~ "Dark1",
    Minutes >= 10 & Minutes < 20 ~ "Light1",
    Minutes >= 20 & Minutes < 30 ~ "Dark2",
    Minutes >= 30 & Minutes < 40 ~ "Light2",
    Minutes >= 40 ~ "Dark3")) %>% 
  mutate(Velocity = as.numeric(as.character(Velocity)), 
         Meander = as.numeric(as.character(Meander)), 
         Label = as.factor(Label)) 
Ctidy.master.df
str(Ctidy.master.df)
#```



#```{r}
Ctidymaster.forstat <- Ctidy.master.df %>% 
  unite(col = newID, Trial, Well, sep = "_") %>% 
  group_by(Treatment, Label, newID) %>% 
  summarise(MeanVelocity = mean(Velocity), 
            Cruise.freq = mean(Cruising.Freq),
            Cruising.dur = mean(Cruising.duration), 
            Turn.ang = mean(Turn.Angle), 
            Ang.vel = mean(Angular.Velocity), 
            Freeze.freq = mean(Freezing.freq),
            Freeze.dur = mean(Freezing.duration),
            Total.Distance = mean(Total.Distance)) %>% 
  ungroup() %>% 
  mutate(Label = as.factor(Label), 
         newID = as.factor(newID), 
         Treatment = fct_relevel(Treatment, "Control", "T1", "T2", "T3"))
Ctidymaster.forstat
#```


STATISTICS
#```{r}
#SHAPIRO
C_shapiro <- Ctidymaster.forstat %>%
  gather(Variable, Value, -Treatment, -newID, -Label) %>%
  group_by(Variable, Label) %>%
  nest() %>% 
  ungroup() %>%
  mutate(shapiro = map(data, ~tidy(shapiro.test(.x$Value)))) %>%
  unnest(shapiro)
View(C_shapiro)
#LEVENES- TEST FOR HOMOGENEITY OF VARIANCE
C_levene <- Ctidymaster.forstat %>%
  gather(Variable, Value, -Treatment, -newID, -Label) %>%
  group_by(Variable, Label) %>%
  nest() %>% 
  mutate(levene = map(data, ~levene_test(.x, Value ~ Treatment))) %>% 
  unnest(levene) 
View(C_levene)
#KRUSKALL
C_KRUSKAL <- Ctidymaster.forstat %>%
  group_by(Label) %>%
  gather(Variable, Value, -Treatment, -newID, -Label) %>%
  group_by(Variable, Label) %>%
  nest() %>% 
  mutate(kruskal = map(data, ~kruskal_test(.x, Value ~ Treatment))) %>% 
  unnest(kruskal) 
View(C_KRUSKAL)
#DUNN'S
##post-hoc test changed to holm (more power than bonferroni with no additional assumptions)
C_DUNNS <- Ctidymaster.forstat %>%
  gather(Variable, Value, -Treatment, -newID, -Label) %>%
  group_by(Variable, Label) %>%
  nest() %>% 
  mutate(dunns = map(data, ~dunn_test(.x, Value ~ Treatment, p.adjust.method = "holm"))) %>% 
  unnest(dunns)
View(C_DUNNS)
#DUNNET- TRT V CTRL
C_dunnetx <- Ctidymaster.forstat %>% 
  gather(Variable, Value, -Treatment, -newID, -Label) %>%
  group_by(Variable, Label) %>%
  nest() %>% 
  mutate(dunnetx = map(data, 
                       ~tidy(contrast(emmeans((ref_grid(lm(Value ~ Treatment, data=.x))),"Treatment"), method="trt.vs.ctrl")))) %>% 
  unnest(dunnetx) %>% 
  mutate(significant = case_when(adj.p.value > 0.05 ~ "ns",
                                 adj.p.value <= 0.01 ~"**",
                                 adj.p.value <= 0.05 ~"*"))
View(C_dunnetx)
#edit for exporting
C_DUNNETforprint <- C_dunnetx %>% 
  select(-data) %>% 
  ungroup()
View(C_DUNNETforprint)
write.csv(C_DUNNETforprint, "C_DUNNETX.csv")
#```

#Edit stats output to merge with data for graphing
#```{r}
str(C_DUNNETforprint)

C_DUNNETforprint_edit <- read.csv("/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/C_DUNNETX_edit.csv") 

C_Tstatgraph2 <- C_DUNNETforprint_edit %>% 
  mutate(Significant = case_when(
    adj.p.value <= 0.01 ~ "SS",
    adj.p.value <= 0.05 ~ "S",
    adj.p.value >= 0.05 ~ "NS")) %>%
  mutate(Variable = fct_recode(Variable, 
                               V = "MeanVelocity",
                               CD = "Cruising.dur",
                               CF = "Cruise.freq",
                               AV = "Ang.vel",
                               TA = "Turn.ang",
                               FD = "Freeze.dur",
                               FF = "Freeze.freq",
                               TD = "Total.Distance")) %>%  
  unite("Treatment_temp", Label, Variable, Treatment, sep = "_") %>% 
  select(Treatment_temp, Significant)
C_Tstatgraph2

write.csv(C_Tstatgraph2, "C_Tstatgraph2.csv")

#```



#Zscore generation - this is for graphing only
#```{r}
C_velocity <- Ctidy.master.df %>% 
  unite(col = newID, Trial, Well, sep = "_") %>% 
  group_by(Treatment, Label, newID) %>% 
  summarise(Velocity = mean(Velocity, na.rm = T), 
            Cruise.freq = mean(Cruising.Freq),
            Cruising.dur = mean(Cruising.duration), 
            Turn.Angle = mean(Turn.Angle), 
            Angular.Velocity = mean(Angular.Velocity), 
            Freeze.freq = mean(Freezing.freq),
            Freeze.dur = mean(Freezing.duration),
            Total.Distance = mean(Total.Distance)) %>% 
  ungroup() %>% 
  mutate(Label = as.factor(Label), 
         newID = as.factor(newID))
C_velocity 
str(C_velocity)
Czscore = C_velocity %>% 
  gather(Variable, Value, -Treatment, -newID, -Label) %>% 
  group_by(Variable, Label) %>%
  filter(!is.na(Value)) %>% 
  mutate(zscore_group = ((Value - mean(Value)) / sd(Value))) %>% 
  ungroup() %>% 
  mutate(zscore_ungrouped = (Value - mean(Value)) / sd(Value))
View(Czscore)
# Means for each Treatment 
Czscore_means <- Czscore %>%
  select(-zscore_ungrouped, -Value) %>% 
  group_by(Treatment, Label, Variable) %>%
  spread(key = Variable, value = zscore_group) %>% 
  group_by(Treatment, Label) %>%
  summarise(Velocity = mean(Velocity, na.rm = T), 
            Cruise.freq = mean(Cruise.freq), 
            Cruising.dur = mean(Cruising.dur), 
            Turn.Angle = mean(Turn.Angle), 
            Angular.Velocity = mean(Angular.Velocity), 
            Freeze.freq = mean(Freeze.freq), 
            Freeze.dur = mean(Freeze.dur),
            Total.Distance = mean(Total.Distance)) %>% 
  gather(Parameter, Value, -Label, -Treatment) %>% 
  mutate(Parameter = as.factor(Parameter))
View(Czscore_means)

#NORMALIZE TO CONTROL - use this to graph
CZnorm <- Czscore_means %>%
  group_by(Treatment, Label, Parameter) %>%
  spread(key=Treatment, value = Value) %>%
  mutate(Controldiff = (Control - Control), 
         T1diff = (T1 - Control), 
         T2diff = (T2 - Control), 
         T3diff = (T3 - Control)) %>% 
  select(-Control, -T1, -T2, -T3) %>% 
  gather(key = "Treatment", value = "Value", -Label, -Parameter) %>% 
  ungroup() %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Parameter = fct_recode(Parameter,
                                "Cruising\nFreq"= "Cruise.freq",
                                "Cruising\nDuration" = "Cruising.dur",
                                "Turn\nAngle" =  "Turn.Angle",
                                "Angular\nVelocity" =  "Angular.Velocity",
                                "Freezing\nDuration" = "Freeze.dur",
                                "Freezing\nFreq" = "Freeze.freq", 
                                "Velocity" = "Velocity",
                                "Total\nDistance" = "Total.Distance"),
         Treatment = fct_recode(Treatment, 
                                "Control" = "Controldiff", 
                                "T1" = "T1diff",
                                "T2" = "T2diff", 
                                "T3" = "T3diff")) %>% 
  mutate(Treatment = fct_relevel(Treatment, "Control", "T1", "T2", "T3"), 
         Label = fct_relevel(Label, "Dark1", "Light1", "Dark2", "Light2", "Dark3")) %>% 
  mutate(Parameter = fct_relevel(Parameter, "Cruising\nDuration",
                                 "Cruising\nFreq", "Turn\nAngle",
                                 "Angular\nVelocity", "Freezing\nDuration",
                                 "Freezing\nFreq", "Velocity", "Total\nDistance"))
CZnorm
write.csv(CZnorm, "CZnorm.csv")

#```


#Merge Zscores and stats for graphing
#```{r}
colnames(C_Tstatgraph2)
nrow(C_Tstatgraph2)

CZ_merge <- CZnorm %>% mutate(Variable = fct_recode(Parameter, V = "Velocity",
                               CD = "Cruising\nDuration",
                               CF = "Cruising\nFreq",
                               TA = "Turn\nAngle",
                               AV = "Angular\nVelocity",
                               FD = "Freezing\nDuration",
                               FF = "Freezing\nFreq",
                               TD = "Total\nDistance")) %>% 
  unite(Treatment_temp, Label,Variable,Treatment) 
#join_full is not wokring for some reason, so here's the workaround...
#I exported csv file:
write.csv(CZ_merge, "/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/CZmerge.csv")
#in excel, I opened CZ_merge and C_Tstatgraph2, sorted by Treatment_temp column, then copied and pasted all columns into a new csv called CZ_merge2, removed the extra column number column, then
#then loaded the edited file:
CZ_merge2 <- read.csv("/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/CZ_merge2.csv", header = TRUE)

CZ_merge3 <- CZ_merge2 %>% 
  mutate(Significant = ifelse(is.na(Significant), "NS", Significant)) %>% 
  separate(Treatment_temp, into = c("Label", "Parameter", "Treatment"), sep = "_") %>% 
  mutate(Time = "96 h", 
         Label = as.factor(Label), 
         Parameter = as.factor(Parameter), 
         Treatment = as.factor(Treatment)) %>% 
  mutate(Time = as.factor(Time),
         Label = as.factor(Label),
         Label = fct_relevel(Label, "Dark1", "Light1", "Dark2", "Light2", "Dark3"),
         Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, "Control", "T1", "T2", "T3"),
         Parameter = as.factor(Parameter),
         Parameter = fct_relevel(Parameter, "V","CD","CF","TA","AV","FD","FF","TD")) %>% 
  mutate(Significance = as.factor(Significant), 
         Significance = fct_recode(Significance, 
                                   "p>0.05" = "NS", 
                                   "p<0.05" = "S", 
                                   "p<0.01" = "SS"))
str(CZ_merge3)
write.csv(CZ_merge3, "/Users/sarahstinson/Box Sync/DPR_2019/exposures/analysis/fhm_paper_analyses/behavior/spider plot code/CZ_merge3.csv")

#```

Graph
#```