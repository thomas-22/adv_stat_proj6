library(readr)
# library(stringr)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
# library(readxl)
# library(purrr)
library(ggplot2)
# library(ggrepel)
# library(ggeffects)
library(patchwork)
library(mgcv)
library(gamm4)
# library(gratia)


path.FCMStress <- "Data/raw/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv"
path.ReproductionSuccess <- "Data/raw/Reproduction Success Results.xlsx"
path.Movement <- "Data/raw/Movement - CRS=ETRS UTM 33N.csv"
path.HuntEvents <- "Data/raw/HuntingEvents_NEW.csv"
