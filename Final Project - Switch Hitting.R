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
library(plotly)

#Load data sets
League_vs_LHP <- read_excel("2018-2023 League vs LHP.xlsx")
League_vs_RHP <- read_excel("2018-2023 League vs RHP.xlsx")
SH_vs_LHP <- read_excel("2018-2023 Switch-Hitter vs LHP as RHH.xlsx")
SH_vs_RHP <- read_excel("2018-2023 Switch-Hitter vs RHP as LHH.xlsx")

#combining switch hitter data sets
SHtotals <- rbind(SH_vs_LHP, SH_vs_RHP)

SHtotals <- SHtotals %>% filter()
SHtotals <- arrange(SHtotals,Name)
?arrange

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
SHtotals_filt <- SHtotals %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                                     OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                                     `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                                     `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)

#Rounding all numeric values to 3 decimal places
SHtotals_filt <- SHtotals_filt %>% 
  mutate_if(is.numeric, round,3)

#Renaming column names
SHtotals_filt <- SHtotals_filt %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)

names(SHtotals_filt)==names(league_av) #Making sure names are the same before binding

#Add Joe Average to bottom of SHtotals dataset
totals <- rbind(SHtotals_filt, league_av)

