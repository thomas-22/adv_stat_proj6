# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(data.table)
library(sf)
library(plotly)
library(checkmate)
library(lme4)
library(mgcv)

# paths
path.FCMStress <- "Data/raw/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv"
path.Huntevents_old <- "Data/raw/Hunt Events - CRS= ETRS UTM 33N.csv"
path.Huntevents <- "Data/raw/HuntingEvents_NEW.csv"
path.Movement <- "Data/raw/Movement - CRS=ETRS UTM 33N.csv"
path.Pregancy <- "Data/raw/Reproduction Success Results.xlsx"

# assumptions and global vars
pregancy_duration <- 200
# approx location of winter enclosures
enclosures <- rbind(c(x_ = 382443.5, y_ = 5421950), 
      c(x_ = 394413.2, y_ = 5417547), 
      c(x_ = 378099.6, y_ = 5431399), 
      c(x_ = 375115.9, y_ = 5435760))

theme_set(theme_light())


