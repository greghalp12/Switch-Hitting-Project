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

#Load datasets
League_vs_LHP <- read_excel("2018-2023 League vs LHP.xlsx")
League_vs_RHP <- read_excel("2018-2023 League vs RHP.xlsx")
SH_vs_LHP <- read_excel("2018-2023 Switch-Hitter vs LHP as RHH.xlsx")
SH_vs_RHP <- read_excel("2018-2023 Switch-Hitter vs RHP as LHH.xlsx")

#subsetting switch-hitters, totaling PAs
SHtotals <- left_join(SH_vs_LHP, SH_vs_RHP, by = 'PlayerId')

league_totals <- left_join(League_vs_LHP, League_vs_RHP, by = 'Season')

league_av <- data.frame(Name = "Johnny Wholestaff",
                        Pitcher_Handedness.L = "Left",
                        PA.L = sum(league_totals$PA.x),
                        wOBA.L = round(mean(league_totals$wOBA.x),3),
                        OPS.L = round(mean(league_totals$OPS.x),3),
                        "GB%.L" = round(mean(league_totals$`GB%.x`),3),
                        "LD%.L" = round(mean(league_totals$`LD%.x`),3),
                        "Hard%.L" = round(mean(league_totals$`Hard%.x`),3),
                        "BB%.L" = round(mean(league_totals$`BB%.x`),3),
                        "K%.L" = round(mean(league_totals$`K%.x`),3),
                        Pitcher_Handedness.R = "Right",
                        PA.R = sum(league_totals$PA.y),
                        wOBA.R = round(mean(league_totals$wOBA.y),3),
                        OPS.R = round(mean(league_totals$OPS.y),3),
                        "GB%.R" = round(mean(league_totals$`GB%.y`),3),
                        "LD%.R" = round(mean(league_totals$`LD%.y`),3),
                        "Hard%.R" = round(mean(league_totals$`Hard%.y`),3),
                        "BB%.R" = round(mean(league_totals$`BB%.y`),3),
                        "K%.R" = round(mean(league_totals$`K%.y`),3))

