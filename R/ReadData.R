library(readxl)
library(dplyr)
library(tidyr)

data_fcmstress_raw <- read.csv("data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv", 
                               header = TRUE, sep = ",")
data_huntevents_raw <- read.csv("data/Hunt Events - CRS= ETRS UTM 33N.csv", 
                                header = TRUE, sep = ",")
data_movement_raw <- read.csv("data/Movement - CRS=ETRS UTM 33N.csv", 
                              header = TRUE, sep = ";")

