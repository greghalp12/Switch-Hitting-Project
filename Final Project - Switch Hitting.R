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
League_vs_LHP <- read_excel("Northwestern/MSDS 456 - Sports Performance Analytics/Final Project/2018-2023 League vs LHP.xlsx")
League_vs_RHP <- read_excel("Northwestern/MSDS 456 - Sports Performance Analytics/Final Project/2018-2023 League vs RHP.xlsx")
SH_vs_LHP <- read_excel("Northwestern/MSDS 456 - Sports Performance Analytics/Final Project/2018-2023 Switch-Hitter vs LHP as RHH.xlsx")
SH_vs_RHP <- read_excel("Northwestern/MSDS 456 - Sports Performance Analytics/Final Project/2018-2023 Switch-Hitter vs RHP as LHH.xlsx")

#subsetting switch-hitters, totaling PAs
Martetotal <- left_join(SH_vs_LHP, SH_vs_RHP, by = (PlayerId == 13613))

#subsetting switch-hitter data by pitcher handedness
MartevsL <- filter(SH_vs_LHP, PlayerId == 13613)

