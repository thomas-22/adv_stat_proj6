library(dplyr)
library(tidyr)
library(lubridate)

Movement <- readr::read_delim("Data/Movement - CRS=ETRS UTM 33N.csv", delim = ";")[,2:5] %>%
  mutate(Sender.ID = factor(Sender.ID),
         t_ = lubridate::parse_date_time(t_, orders = "%d/%m/%Y %h:%M"))
str(Movement)
vapply(Movement, function(x) {all(is.na(x))}, logical(1))

HuntEvents <- readr::read_delim("Data/Hunt Events - CRS= ETRS UTM 33N.csv", delim = ",")[,2:5] %>%
  mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
           lubridate::parse_date_time(orders = "%d/%m/%Y %h:%M:%s"))
str(HuntEvents)
# What to do with NA's? for now: drop the lines
HuntEventsreduced <- HuntEvents[!is.na(HuntEvents$t_), 3:5]

FCMStress <- readr::read_delim("Data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv")[2:14] %>%
  mutate(Collar_t_ = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
           lubridate::parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
         Waypoint_t_ = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
           lubridate::parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
         Sender.ID = as.factor(Sender.ID),
         Sex = as.factor(Sex)) %>%
  select(-Collar_day, -Collar_time, -Waypoint_day, -Waypoint_time)

str(FCMStress)
summary(FCMStress)
vapply(FCMStress, function(x) {all(is.na(x))}, logical(1))

# augment movement date_times to timespans
temp1 <- round_date(Movement$t_ - dminutes(30), unit = "30 mins")
temp2 <- round_date(Movement$t_ + dminutes(30), unit = "30 mins")
Movement$t_dur <- interval(temp1, temp2)
# test
Movement$t_ %within% Movement$t_dur

####



######
mvmt <- Movement %>%
  filter(Sender.ID == 2925) %>%
  select(-Sender.ID) %>%
  arrange(t_)


#### distance to previous point
to_tz_w_dist <- function(ID) {
  mvmt <- Movement %>%
  filter(Sender.ID == ID) %>%
    select(-Sender.ID) %>%
    arrange(t_)
  
  dist <- 0
  for (row in seq_len(dim(mvmt)[1]-1)) {
    temp <- (mvmt$x_[row] - mvmt$x_[row+1])^2 + (mvmt$y_[row] - mvmt$y_[row+1])^2
    dist[length(dist) + 1] <- floor(sqrt(temp))
  }
  mvmt$dist <- dist
  mvmt.tz <- xts::xts(mvmt[,c(1, 2, 4)], order.by = mvmt$t_)
  return(mvmt.tz)
}



mvmt.tz <- list()
for (ID in unique(Movement$Sender.ID)) {
  mvmt.tz[[length(mvmt.tz)+1]] <- to_tz_w_dist(ID)
}
mvmt.tz[1]
names(mvmt.tz) <- unique(Movement$Sender.ID)


unique(Movement$Sender.ID)
