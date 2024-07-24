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
library(plotly)

#Load data sets
League_vs_LHP <- read_excel("2018-2023 League vs LHP.xlsx")
League_vs_RHP <- read_excel("2018-2023 League vs RHP.xlsx")
SH_vs_LHP <- read_excel("2018-2023 Switch-Hitter vs LHP as RHH.xlsx")
SH_vs_RHP <- read_excel("2018-2023 Switch-Hitter vs RHP as LHH.xlsx")

#combining switch hitter data sets
SHfull <- left_join(SH_vs_LHP,SH_vs_RHP, by = 'PlayerId')

#combining league data sets
league_totals <- left_join(League_vs_LHP, League_vs_RHP, by = 'Season')

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
hit_metrics <- rbind(SH_filt, league_av)3
View(hit_metrics)

ggplot(data = hit_metrics, aes(x=OPS.R, y=OPS.L)) +
  geom_point(aes(color=Name, size = PlayerId))


ggplot(data = hit_metrics, aes(x = OPS.R, y = OPS.L)) +
  geom_vline(xintercept = 0.731) +
  geom_hline(yintercept = 0.736) +
  annotate("rect", xmin = Inf, xmax = 0.731, ymin = Inf, ymax = 0.736, fill = "chartreuse2") +
  annotate("rect", xmin = -Inf, xmax = 0.731, ymin = -Inf, ymax = 0.736, fill = "coral") +
  annotate("rect", xmin = 0.731, xmax = Inf, ymin = 0.736, ymax = -Inf, fill = "darkgoldenrod1") +
  annotate("rect", xmin = 0.731, xmax = -Inf, ymin = Inf, ymax = 0.736, fill = "darkgoldenrod2") +
  geom_point(size = 2) +
  geom_text_repel(aes(label = Name), size = 2, color = "black") +
  labs(title = "OPS EDA", x = "OPS.R", y = "OPS.L")

hit_metrics2 = hit_metrics

#Add a new column 'Color' with default value 'black'
hit_metrics2$Colour <- "black"

# Set new column values to appropriate colours
hit_metrics2$Colour[hit_metrics2$OPS.R>0.731 & hit_metrics2$OPS.L > 0.736]="green"
hit_metrics2$Colour[hit_metrics2$OPS.R < 0.731 & hit_metrics2$OPS.L < 0.736]="red"
hit_metrics2$Colour[hit_metrics2$OPS.R > 0.731 & hit_metrics2$OPS.L < 0.736] = "darkgoldenrod1"
hit_metrics2$Colour[hit_metrics2$OPS.R < 0.731 & hit_metrics2$OPS.L > 0.736] = "darkgoldenrod1"
# Plot all points at once, using newly generated colours
plot(hit_metrics2$OPS.R,hit_metrics2$OPS.L, 
     abline(v = 0.731, h=0.736), 
     xlim = c(0.5,1), 
     ylim=c(0.5,1), 
     col=hit_metrics2$Colour,
     main = "OPS EDA", 
     xlab = "OPS.R", 
     ylab = "OPS.L")

text(hit_metrics2$OPS.R,
     hit_metrics2$OPS.L,
     labels = hit_metrics2$Name,
     cex = 0.5,
     pos = 1)

hist(hit_metrics$OPS.R)
hist(hit_metrics$OPS.L)
