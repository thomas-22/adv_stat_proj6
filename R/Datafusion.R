library(readr)
library(dplyr)
library(sf)
library(lubridate)
library(readxl)
### estimate for the duration of the pregnancy, needs to be confirmed
pregancy_duration <- 200

# calculate backwards from the birth of the foal 200 days.
# column pregnant is time interval

ReproductionSuccess <- readxl::read_excel("Data/Reproduction Success Results.xlsx") %>%
  filter(!is.na(`birth date`)) %>%
  mutate(birth_date = ymd(`birth date`),
         Sender.ID = factor(`Collar ID`),
         .keep = "unused") %>%
  mutate(pregnant_since = birth_date - days(pregancy_duration)) %>%
  mutate(pregnant = interval(pregnant_since, birth_date))

ReproductionSuccess <- ReproductionSuccess[, c("Sender.ID", "pregnant")]

# read Movement data
Movement <- readr::read_delim("Data/Movement - CRS=ETRS UTM 33N.csv", delim = ";")[,2:5] %>%
  mutate(Sender.ID = factor(Sender.ID),
         t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M"))

# ReproductionSuccess gets joined to Movement
# logical idicator "pregnant" is set to TRUE if collar time lies within pregnancy interval
Movement <- Movement %>%
  full_join(ReproductionSuccess, by = c("Sender.ID")) %>%
  mutate(pregnant = if_else(t_ %within% pregnant, TRUE, FALSE))

# # read HuntEvents
# HuntEvents <- read_delim("Data/Hunt Events - CRS= ETRS UTM 33N.csv", delim = ",")[,2:5] %>%
#   mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
#            parse_date_time(orders = "%d/%m/%Y %h:%M:%s"))
# 
# # What to do with NA's? here: just drop the lines. Drop any duplicate entries.
# HuntEventsreduced <- distinct(HuntEvents[!is.na(HuntEvents$t_), 3:5])
# HuntEvents_NoTime <- distinct(HuntEvents[is.na(HuntEvents$t_), 1:4])
# HuntEvents_NoTime <- HuntEvents_NoTime [, c("Datum", "X", "Y")]
# HuntEvents_NoTime$Datum <- as.Date(HuntEvents_NoTime$Datum, format = "%d/%m/%Y")

#########NEW HUNT DATA:
# Read
HuntEvents_new <- read_delim("Data/HuntingEvents_NEW.csv", delim = ";", col_names = TRUE)
# Combine Datum and Zeit
tz <- stringr::str_c(HuntEvents_new$Datum, HuntEvents_new$Zeit, sep = " ")
HuntEvents_new <- HuntEvents_new %>% 
  mutate(t_ = parse_date_time(paste(Datum, Zeit), orders = "%d/%m/%Y %H:%M"))
# Handle missing
HuntEventsNewReduced <- distinct(HuntEvents_new[!is.na(HuntEvents_new$t_), c("Breitengrad_wgs", "Laengengrad_wgs", "t_")])

#Transform Coordinate System:
HuntEvents_sf <- HuntEventsNewReduced %>%
  st_as_sf(coords = c("Laengengrad_wgs", "Breitengrad_wgs"), crs = 4326)

# Transform to UTM (ETRS89 / UTM zone 33N - EPSG:25833)
HuntEvents_utm <- HuntEvents_sf %>%
  st_transform(crs = 25833)

# Extract the UTM coordinates and add them to the dataframe
HuntEvents_Reduced_UTM_New <- HuntEvents_utm %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

HuntEventsreduced <- HuntEvents_Reduced_UTM_New


#NewHunts Without Time:
HuntEventsNew_NoTime <- distinct(HuntEvents_new[is.na(HuntEvents_new$t_), c("Datum", "Breitengrad_wgs", "Laengengrad_wgs")])
HuntEventsNew_NoTime <- HuntEventsNew_NoTime %>% 
  mutate(Datum = as.Date(Datum, format = "%d/%m/%Y"))
################

# read FCMStress
FCMStress <- read_delim("Data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv")[2:14] %>%
  mutate(Collar_t_ = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
           parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
         Waypoint_t_ = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
           parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
         Sender.ID = as.factor(Sender.ID),
         Sex = as.factor(Sex)) %>%
  dplyr::select(-Collar_day, -Collar_time, -Waypoint_day, -Waypoint_time)

# save as RDS
saveRDS(Movement, "data/Movement.RDS")
saveRDS(FCMStress, "data/FCMStress.RDS")
# saveRDS(HuntEvents, "data/Hunts.RDS")
saveRDS(HuntEventsreduced, "data/HuntsReduced.RDS")
