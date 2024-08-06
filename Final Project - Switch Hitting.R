#MSDS 456: Sports Performance Analytics
#Final Project
#Authors: Halperin, Greg and Holsey, Patrick

#Our final project is to analyze the value of switch hitting in the MLB and to build a decision tree
#whether players should continue to switch hit or consider sticking to one side of the plate

#The dataset acquired is from Fangraphs, filtering switch hitters with 600 plate appearances vs
#left-handed pitchers and right-handed pitchers over the years of 2018-2023

#Load packages
library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(plotly)
library(randomForest)
library(caTools)
library(janitor)

#Load data sets
League_vs_LHP <- read_excel("2018-2023 League vs LHP.xlsx")
League_vs_RHP <- read_excel("2018-2023 League vs RHP.xlsx")
SH_vs_LHP <- read_excel("2018-2023 Switch-Hitter vs LHP as RHH.xlsx")
SH_vs_RHP <- read_excel("2018-2023 Switch-Hitter vs RHP as LHH.xlsx")

#combining switch hitter data sets
SHfull <- left_join(SH_vs_LHP,SH_vs_RHP, by = 'PlayerId')

#combining league data sets
league_totals <- left_join(League_vs_LHP, League_vs_RHP, by = 'Season')

#Combining Switch-hitter data for total PAs
SH_binded <- rbind(SH_vs_LHP, SH_vs_RHP)

#Filtering columns required
SH_binded <- SH_binded %>% select(Name, PlayerId, `Pitcher Handedness`, PA, wOBA,
                                  `GB%`,`LD%`, `Hard%`, `BB%`, `K%`)  

#Rounding %s to 3 digits
SH_binded <- SH_binded %>% mutate_if(is.numeric, round, 3)  

#create total PA for League
League_binded <- rbind(League_vs_LHP, League_vs_RHP)

#Filtering columns required
League_binded <- League_binded %>% select(`Pitcher Handedness`, PA, wOBA,
                                          `GB%`,`LD%`, `Hard%`, `BB%`, `K%`)  

#Rounding %s to 3 digits
League_binded <- League_binded %>% mutate_if(is.numeric, round, 3)

#Switch hitter bar chart of PAs
SH_bar <- ggplot(data = SH_binded, aes(x=`Pitcher Handedness`, y= PA, fill=`Pitcher Handedness`)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("Left" = "red", "Right" = "blue")) +
  labs(title = "Switch-Hitter PA vs LHP/RHP", ylab="Plate Appearances") + 
  theme(legend.position="none")

#League bar chart of PAs
League_bar <- ggplot(data = League_binded, aes(x=`Pitcher Handedness`, y= PA, fill=`Pitcher Handedness`)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("Left" = "red", "Right" = "blue")) +
  labs(title = "League PA vs LHP/RHP", ylab="Plate Appearances") +
  theme(legend.position="none")
grid.arrange(SH_bar, League_bar, nrow=1, ncol=2)

#Creating a 'League Average' Player
league_av <- data.frame(Name = "Joe Average",
                        PlayerId = 999999,
                        `Pitcher_Handedness.L` = "Left",
                        PA.L = sum(league_totals$PA.x),
                        wOBA.L = round(mean(league_totals$wOBA.x),3),
                        OPS.L = round(mean(league_totals$OPS.x),3),
                        "GB%.L" = round(mean(league_totals$`GB%.x`),3),
                        "LD%.L" = round(mean(league_totals$`LD%.x`),3),
                        "Hard%.L" = round(mean(league_totals$`Hard%.x`),3),
                        "BB%.L" = round(mean(league_totals$`BB%.x`),3),
                        "K%.L" = round(mean(league_totals$`K%.x`),3),
                        `Pitcher_Handedness.R` = "Right",
                        PA.R = sum(league_totals$PA.y),
                        wOBA.R = round(mean(league_totals$wOBA.y),3),
                        OPS.R = round(mean(league_totals$OPS.y),3),
                        "GB%.R" = round(mean(league_totals$`GB%.y`),3),
                        "LD%.R" = round(mean(league_totals$`LD%.y`),3),
                        "Hard%.R" = round(mean(league_totals$`Hard%.y`),3),
                        "BB%.R" = round(mean(league_totals$`BB%.y`),3),
                        "K%.R" = round(mean(league_totals$`K%.y`),3))

#Had trouble getting % to show up, used code below to fix
names(league_av)=c("Name", "PlayerId", "Pitcher_Handedness.L","PA.L", "wOBA.L", 
                   "OPS.L", "GB%.L", "LD%.L", "Hard%.L", "BB%.L", "K%.L",
                   "Pitcher_Handedness.R", "PA.R", "wOBA.R", "OPS.R", "GB%.R",
                   "LD%.R", "Hard%.R", "BB%.R", "K%.R")


#Filtering Switch hitting variables
SH_filt <- SHfull %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                             OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                             `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                             `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)

#Rounding all numeric values to 3 decimal places
SH_filt <- SH_filt %>% 
  mutate_if(is.numeric, round,3)

#Renaming column names
SH_filt <- SH_filt %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)

names(SH_filt)==names(league_av) #Making sure names are the same before binding

#Add Joe Average to bottom of SH_filt dataset
hit_metrics <- rbind(SH_filt, league_av)
View(hit_metrics)

ggplot(data = hit_metrics, aes(x=OPS.R, y=OPS.L)) +
  geom_point(aes(color=Name, size = PlayerId))

#Add a new column 'Color' with default value 'black'
hit_metrics$Color <- "black"

# Set new column values to appropriate colors for OPS
hit_metrics$Color_OPS[hit_metrics$OPS.R >0.731 & hit_metrics$OPS.L > 0.736]="green"
hit_metrics$Color_OPS[hit_metrics$OPS.R < 0.731 & hit_metrics$OPS.L < 0.736]="red"
hit_metrics$Color_OPS[hit_metrics$OPS.R > 0.731 & hit_metrics$OPS.L < 0.736] = "darkgoldenrod1"
hit_metrics$Color_OPS[hit_metrics$OPS.R < 0.731 & hit_metrics$OPS.L > 0.736] = "darkgoldenrod1"

#Scatterplot of EDA on OPS 
ggplot(data = hit_metrics, aes(x = OPS.R, y = OPS.L)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +
  geom_vline(xintercept = 0.731) +
  geom_hline(yintercept = 0.736) +
  geom_point(color = hit_metrics$Color_OPS) +
  #  geom_text_repel(aes(label = Name), size = 4, color = "black") +
  geom_text_repel(aes(label=ifelse(PlayerId==999999, as.character("League Avg"), '')), 
                  cex = 2.75, hjust = 0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.min(hit_metrics$OPS.R),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.max(hit_metrics$OPS.R),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.min(hit_metrics$OPS.L),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.max(hit_metrics$OPS.L),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  labs(title = "EDA on OPS", x = "OPS.R", y = "OPS.L") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

# Set new column values to appropriate colors for K%
hit_metrics$Color_GB[hit_metrics$`GB%.R` < 0.426 & hit_metrics$`GB%.L` < 0.434]="green"
hit_metrics$Color_GB[hit_metrics$`GB%.R` > 0.426 & hit_metrics$`GB%.L` > 0.434]="red"
hit_metrics$Color_GB[hit_metrics$`GB%.R` < 0.426 & hit_metrics$`GB%.L` > 0.434]="darkgoldenrod1"
hit_metrics$Color_GB[hit_metrics$`GB%.R` > 0.426 & hit_metrics$`GB%.L` < 0.434]="darkgoldenrod1"

#Scatterplot of EDA on GB% 
ggplot(data = hit_metrics, aes(x =`GB%.R`, y = `GB%.L`)) +
  annotate("rect", xmin = 0.2, xmax = 0.6, ymin = 0.2, ymax = 0.6, fill = "white") +
  geom_vline(xintercept = 0.426) +
  geom_hline(yintercept = 0.434) +
  geom_point(color = hit_metrics$Color_GB) +
  scale_x_reverse() +
  scale_y_reverse() +
  #  geom_text_repel(aes(label = Name), size = 4, color = "black") +
  geom_text_repel(aes(label=ifelse(PlayerId==999999, as.character("League Avg"), '')), 
                  cex = 2.75, hjust = 0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.min(hit_metrics$`GB%.R`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.max(hit_metrics$`GB%.R`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.min(hit_metrics$`GB%.L`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.max(hit_metrics$`GB%.L`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  labs(title = "EDA on GB%", x = "GB%.R", y = "GB%.L") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

# Set new column values to appropriate colors for K%
hit_metrics$Color_K[hit_metrics$`K%.R` < 0.229 & hit_metrics$`K%.L` < 0.226]="green"
hit_metrics$Color_K[hit_metrics$`K%.R` > 0.229 & hit_metrics$`K%.L` > 0.226]="red"
hit_metrics$Color_K[hit_metrics$`K%.R` < 0.229 & hit_metrics$`K%.L` > 0.226]="darkgoldenrod1"
hit_metrics$Color_K[hit_metrics$`K%.R` > 0.229 & hit_metrics$`K%.L` < 0.226]="darkgoldenrod1"

#Scatterplot of EDA on K% 
ggplot(data = hit_metrics, aes(x =`K%.R`, y = `K%.L`)) +
  annotate("rect", xmin = 0.1, xmax = 0.3, ymin = 0.1, ymax = 0.3, fill = "white") +
  geom_vline(xintercept = 0.229) +
  geom_hline(yintercept = 0.226) +
  geom_point(color = hit_metrics$Color_K) +
  scale_x_reverse() +
  scale_y_reverse() +
  #  geom_text_repel(aes(label = Name), size = 4, color = "black") +
  geom_text_repel(aes(label=ifelse(PlayerId==999999, as.character("League Avg"), '')), 
                  cex = 2.75, hjust = 0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.min(hit_metrics$`K%.R`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.max(hit_metrics$`K%.R`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.min(hit_metrics$`K%.L`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  geom_text_repel(data = hit_metrics[which.max(hit_metrics$`K%.L`),], 
                  aes(label=Name), cex = 2.75, hjust=0, vjust=0) +
  labs(title = "EDA on K%", x = "K%.R", y = "K%.L") +
  theme(plot.title=element_text(hjust=0.4, size = 15))



#Creating a data set to train the model

LHH_vs_LHP <- read_csv("2018-2023 LHH vs LHP - min200.csv")
LHH_vs_RHP <- read_csv("2018-2023 LHH vs RHP - min500.csv")
RHH_vs_LHP <- read_csv("2018-2023 RHH vs LHP - min250.csv")
RHH_vs_RHP <- read_csv("2018-2023 RHH vs RHP - min500.csv")

#Inner joining to get split data for each hitter
LHH_splits <-  inner_join(LHH_vs_LHP,LHH_vs_RHP, by = 'PlayerId')
RHH_splits <- inner_join(RHH_vs_LHP, RHH_vs_RHP, by = 'PlayerId')

#Filter each splits table
LHH_splits <- LHH_splits %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                                    OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                                    `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                                    `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)
RHH_splits <- RHH_splits %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                                    OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                                    `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                                    `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)

#Rounding all numeric values to 3 decimal places
LHH_splits <- LHH_splits %>% 
  mutate_if(is.numeric, round,3)
RHH_splits <- RHH_splits %>% 
  mutate_if(is.numeric, round,3)

#Adjusting variable names to .L / .R
LHH_splits <- LHH_splits %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)
RHH_splits <- RHH_splits %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)

#Creating the model
set.seed(111)

#Cleaning the data frame variables so it can be read by randomForest
SH_filt <- clean_names(SH_filt)
LHH_splits <- clean_names(LHH_splits)
RHH_splits <-clean_names(RHH_splits)

SH_filt <- SH_filt %>% rename(gb_r = gb_percent_r, gb_l = gb_percent_l, 
                                   ld_r = ld_percent_r, ld_l = ld_percent_l,
                                   hard_r = hard_percent_r, hard_l = hard_percent_l,
                                   bb_r = bb_percent_r, bb_l = bb_percent_l,
                                   k_r = k_percent_r, k_l = k_percent_l)

#Splitting the LHH_splits data set into training and testing data
LHH.index <- sample(2, nrow(LHH_splits), replace = TRUE, prob = c(0.8, 0.2))
LHH.train <- LHH_splits[LHH.index==1,]
LHH.test <- LHH_splits[LHH.index==2, ]

#Creating random forest for predicting woba against LHP using information against RHP for LHH
LHH.rf <- randomForest(w_oba_l ~ w_oba_r + ops_r + gb_r + ld_r + hard_r + bb_r + k_r, 
                       data = LHH.train, importance = TRUE)
print(LHH.rf)

#Splitting the RHH_splits data set into training and testing data
RHH.index <- sample(2, nrow(RHH_splits), replace = TRUE, prob = c(0.8, 0.2))
RHH.train <- RHH_splits[RHH.index==1,]
RHH.test <- RHH_splits[RHH.index==2, ]

#Creating ranomd forest for RHH against RHP using information against LHP
RHH.rf <- randomForest(w_oba_r ~ w_oba_l + ops_l + gb_l + ld_l + hard_l + bb_l + k_l, 
                       data = RHH.train, importance = TRUE)
print(RHH.rf)

#Making predictions for Switch hitters only hitting lefty against LHP
pred_l <- predict(LHH.rf, newdata = SH_filt)
print(pred_l)

#Making predictions for Switch hitters only hitting righty against RHP
pred_r <- predict(RHH.rf, newdata = SH_filt)
print(pred_r)


