library(readr)
library(dplyr)
library(lubridate)
library(readxl)


# Read all datasets
# 
# Parameters:
# - folder: character(1), folder of the csv/xlsx files
# 
# returns: a named list of data.frames `FCMStress`, `Movement`, `HuntEvents`
read_all_data <- function(folder = "Data") {
  # read FCM data
  path_fcm <- file.path(folder,
                        "FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv")
  FCMStress <- read_delim(path_fcm) %>%
    as_tibble() %>%
    # only keep female deer
    # other entries are errors
    filter(Sex %in% c("f", "w")) %>%
    mutate(
      DefecTime = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
        parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
      SampleTime = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
        parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
      Sender.ID = as.factor(Sender.ID)
    ) %>%
    rename(Sample.ID = Sample_ID) %>%
    select(ng_g, Sender.ID, Sample.ID, SampleTime, DefecTime) %>%
    # sort samples chronologically
    arrange(DefecTime)
  
  sender_ids <- levels(FCMStress$Sender.ID)
  
  # estimate for the duration of the pregnancy, needs to be confirmed
  pregancy_duration <- 200
  # calculate backwards from the birth of the foal 200 days
  # column pregnant is time interval
  path_production <- file.path(folder, "Reproduction Success Results.xlsx")
  ReproductionSuccess <- read_excel(path_production) %>%
    filter(!is.na(`birth date`)) %>%
    mutate(birth_date = ymd(`birth date`),
           Sender.ID = factor(`Collar ID`, levels = sender_ids),
           .keep = "unused",
           pregnant_since = birth_date - days(pregancy_duration),
           pregnant_to = birth_date) %>%
    select(Sender.ID, pregnant_since, pregnant_to)
  
  # ReproductionSuccess gets joined to FCMStress
  join_criterion <- join_by(Sender.ID,
                            between(DefecTime, pregnant_since, pregnant_to))
  FCMStress <- FCMStress %>%
    left_join(ReproductionSuccess, join_criterion) %>%
    mutate(pregnant = (!is.na(pregnant_since)) & (!is.na(pregnant_to))) %>%
    select(-pregnant_since, -pregnant_to)
  
  
  # read movement data
  first_date <- date(first(FCMStress$DefecTime) - hours(50))
  last_date <- date(last(FCMStress$DefecTime))
  
  path_movement <- file.path(folder, "Movement - CRS=ETRS UTM 33N.csv")
  Movement <- readr::read_delim(path_movement, delim = ";") %>%
    mutate(Sender.ID = factor(Sender.ID, levels = sender_ids),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    filter(date(t_) >= first_date, date(t_) <= last_date) %>%
    select(Sender.ID, x_, y_, t_)
  
  
  # read HuntEvents
  path_hunt <- file.path(folder, "Hunt Events - CRS= ETRS UTM 33N.csv")
  HuntEvents <- read_delim(path_hunt, delim = ",") %>%
    mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
             parse_date_time(orders = "%d/%m/%Y %h:%M:%s")) %>%
    mutate(Date = dmy(Datum)) %>%
    select(Date, X, Y, t_)
  
  
  list(FCMStress = FCMStress, Movement = Movement, HuntEvents = HuntEvents)
}


# What to do with NA's? here: just drop the lines. Drop any duplicate entries.
# HuntEventsreduced <- distinct(HuntEvents[!is.na(HuntEvents$t_), 3:5])
# HuntEvents_NoTime <- distinct(HuntEvents[is.na(HuntEvents$t_), 1:4])
# HuntEvents_NoTime <- HuntEvents_NoTime %>% select(-Zeit)
# HuntEvents_NoTime$Date <- as.Date(HuntEvents_NoTime$Datum, format = "%d/%m/%Y")

# save as RDS
# saveRDS(Movement, "data/Movement.RDS")
# saveRDS(FCMStress, "data/FCMStress.RDS")
# saveRDS(HuntEvents, "data/Hunts.RDS")
# saveRDS(HuntEventsreduced, "data/HuntsReduced.RDS")
