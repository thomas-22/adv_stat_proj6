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

# paths
path.FCMStress <- "Data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv"
path.Huntevents <- "Data/Hunt Events - CRS= ETRS UTM 33N.csv"
path.Movement <- "Data/Movement - CRS=ETRS UTM 33N.csv"
path.Pregancy <- "Data/Reproduction Success Results.xlsx"

# assumptions and global vars
pregancy_duration <- 200
# approx location of winter enclosures
enclosures <- rbind(c(x_ = 382443.5, y_ = 5421950), 
      c(x_ = 394413.2, y_ = 5417547), 
      c(x_ = 378099.6, y_ = 5431399), 
      c(x_ = 375115.9, y_ = 5435760))




