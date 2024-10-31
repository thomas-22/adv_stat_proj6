library(readr)
library(dplyr)
library(lubridate)

Movement <- read_delim("Data/Movement - CRS=ETRS UTM 33N.csv", delim = ";")[,2:5] %>%
  mutate(Sender.ID = factor(Sender.ID),
         t_ = lubridate::parse_date_time(t_, orders = "%d/%m/%Y %h:%M"))
str(Movement)

HuntEvents <- read_delim("Data/Hunt Events - CRS= ETRS UTM 33N.csv", delim = ",")[,2:5] %>%
  mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
           lubridate::parse_date_time(orders = "%d/%m/%Y %h:%M:%s"))
str(HuntEvents)
# What to do with NA's? here: just drop the lines
HuntEventsreduced <- HuntEvents[!is.na(HuntEvents$t_), 3:5]

FCMStress <- read_delim("Data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv")[2:14] %>%
  mutate(Collar_t_ = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
           lubridate::parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
         Waypoint_t_ = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
           lubridate::parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M"))) %>%
  select(-Collar_day, -Collar_time, -Waypoint_day, -Waypoint_time)


summary(FCMStress)
vapply(FCMStress, function(x) {all(is.na(x))}, logical(1))



