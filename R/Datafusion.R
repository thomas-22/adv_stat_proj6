library(readr)
library(dplyr)
library(lubridate)
library(readxl)

pregancy_duration <- 200

ReproductionSuccess <- readxl::read_excel("Data/Reproduction Success Results.xlsx") %>%
  filter(!is.na(`birth date`)) %>%
  mutate(birth_date = ymd(`birth date`),
         Sender.ID = factor(`Collar ID`),
         .keep = "unused") %>%
  mutate(pregnant_since = birth_date - days(pregancy_duration)) %>%
  mutate(pregnant = interval(pregnant_since, birth_date)) %>%
  select(Sender.ID, pregnant)

Movement <- readr::read_delim("Data/Movement - CRS=ETRS UTM 33N.csv", delim = ";")[,2:5] %>%
  mutate(Sender.ID = factor(Sender.ID),
         t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M"))

Movement <- Movement %>%
  full_join(ReproductionSuccess, by = c("Sender.ID")) %>%
  mutate(pregnant = if_else(t_ %within% pregnant, TRUE, FALSE))

HuntEvents <- read_delim("Data/Hunt Events - CRS= ETRS UTM 33N.csv", delim = ",")[,2:5] %>%
  mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
           parse_date_time(orders = "%d/%m/%Y %h:%M:%s"))

# What to do with NA's? here: just drop the lines. Drop any duplicate entries.
HuntEventsreduced <- distinct(HuntEvents[!is.na(HuntEvents$t_), 3:5])

FCMStress <- read_delim("Data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv")[2:14] %>%
  mutate(Collar_t_ = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
           parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
         Waypoint_t_ = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
           parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
         Sender.ID = as.factor(Sender.ID),
         Sex = as.factor(Sex)) %>%
  select(-Collar_day, -Collar_time, -Waypoint_day, -Waypoint_time)
