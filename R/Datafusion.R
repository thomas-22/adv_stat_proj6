library(readr)
library(dplyr)
library(lubridate)
library(readxl)

# Helper functions to load and pre-process datasets
# 
# Parameters:
# - folder: character(1), folder of the csv/xlsx files
# 
# returns: tibble `FCMStress`, `Movement`, or `HuntEvents`
read_fcm <- function(folder = "Data") {
  # read FCM data
  path_fcm <- file.path(folder,
                        "FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv")
  
  FCMStress <- read_delim(path_fcm, show_col_types = FALSE) %>%
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
    as_tibble() %>%
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
  
  return(FCMStress)
}

read_movement <- function(folder = "Data", fcm_data = read_fcm(folder)) {
  sender_ids <- levels(fcm_data$Sender.ID)
  
  first_date <- date(first(fcm_data$DefecTime) - hours(50))
  last_date <- date(last(fcm_data$DefecTime))
  
  path_movement <- file.path(folder, "Movement - CRS=ETRS UTM 33N.csv")
  Movement <- readr::read_delim(path_movement, delim = ";") %>%
    as_tibble() %>%
    mutate(Sender.ID = factor(Sender.ID, levels = sender_ids),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    filter(date(t_) >= first_date - days(10), date(t_) <= last_date + days(10)) %>%
    select(Sender.ID, x_, y_, t_)
  
  return(Movement)
}

read_hunting <- function(folder = "Data", fcm_data = read_fcm(folder),
                         with.time = NULL) {
  first_date <- date(first(fcm_data$DefecTime) - hours(50))
  last_date <- date(last(fcm_data$DefecTime))
  
  path_hunt <- file.path(folder, "Hunt Events - CRS= ETRS UTM 33N.csv")
  HuntEvents <- read.csv(path_hunt, sep = ",") %>%
    as_tibble() %>%
    mutate(
      TimeMissing = is.na(Zeit),
      t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
        parse_date_time(orders = "%d/%m/%Y %h:%M:%s"),
      Date = dmy(Datum)
    ) %>%
    filter(Date >= first_date, Date <= last_date) %>%
    select(Date, X, Y, t_, TimeMissing) %>%
    distinct() %>%
    mutate(Hunt.ID = row_number())
  
  if (is.null(with.time)) {
    return(HuntEvents)
  }
  if (with.time) {
    return(HuntEvents %>% filter(!TimeMissing))
  }
  return(HuntEvents %>% filter(TimeMissing))
}

# read_hunting_reduced <- function(folder = "Data", fcm_data = read_fcm(folder)) {
#   na.omit(read_hunting(folder), fcm_data)
# }
# 
# read_hunting_imputed <- function(folder = "Data", fcm_data = read_fcm(folder)) {
#   first_date <- date(first(fcm_data$DefecTime) - hours(50))
#   last_date <- date(last(fcm_data$DefecTime))
#   
#   path_hunt <- file.path(folder, "Hunt Events - CRS= ETRS UTM 33N.csv")
#   # read.delim does not work here
#   HuntEvents_imputed <- read.csv(path_hunt, sep = ",") %>%
#     as_tibble() %>%
#     # impute with 00:00:01
#     mutate(
#       TimeMissing = is.na(Zeit),
#       Zeit = ifelse(TimeMissing, "00:00:01", Zeit),
#       t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
#         parse_date_time(orders = "%d/%m/%Y %h:%M:%s"),
#       Date = dmy(Datum)
#     ) %>%
#     filter(Date >= first_date, Date <= last_date) %>%
#     select(Date, X, Y, t_)
#   
#   return(HuntEvents_imputed)
# }


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
