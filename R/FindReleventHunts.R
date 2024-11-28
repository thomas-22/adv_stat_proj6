source("R/Datafusion.R")

library(sf)
library(units)

FCMStress <- read_fcm()
HuntEvents <- read_hunting()
Movement <- read_movement()

max_hours <- 30
min_hours <- 16
max_distance <- 3 # km

RelevantHuntEvents <- FCMStress %>%
  distinct(Sender.ID, DefecTime) %>%
  # get type 1 time-relevant hunting events
  mutate(
    StressDateEarliest = date(DefecTime - days(2)),
    StressDateLatest = date(DefecTime - days(1))
  ) %>%
  left_join(
    HuntEvents %>% select(Hunt.ID, Date),
    join_by(StressDateEarliest <= Date, StressDateLatest >= Date)
  ) %>%
  rename(Hunt.ID_timerel1 = Hunt.ID) %>%
  # just in case of duplicates, keep distinct
  distinct(Sender.ID, DefecTime, Hunt.ID_timerel1) %>%
  # get type 2 time-relevant hunting events
  mutate(
    StressTimeEarliest = DefecTime - hours(max_hours),
    StressTimeLatest = DefecTime - hours(min_hours)
  ) %>%
  left_join(
    HuntEvents %>% select(Hunt.ID, t_),
    join_by(StressTimeEarliest <= t_, StressTimeLatest >= t_)
  ) %>%
  rename(Hunt.ID_timerel2 = Hunt.ID) %>%
  distinct(Sender.ID, DefecTime, Hunt.ID_timerel1, Hunt.ID_timerel2)

# View(RelevantHuntEvents)

# pre-process hunting and movement data
HuntEvents_sf <- st_as_sf(
  HuntEvents,
  coords = c("X", "Y"), crs = 32633,
  sf_column_name = "HuntPoint"
)
Movement_sf <- st_as_sf(
  Movement,
  coords = c("x_", "y_"), crs = 32633,
  sf_column_name = "DeerPoint"
) %>%
  mutate(Date = date(t_))

# get average distances
avg_distances <- RelevantHuntEvents %>%
  distinct(Sender.ID, Hunt.ID_timerel1) %>%
  na.omit() %>%
  # get hunt event location
  left_join(
    HuntEvents_sf %>% select(Hunt.ID, HuntPoint, Date),
    join_by(Hunt.ID_timerel1 == Hunt.ID)
  ) %>%
  # get movement data
  left_join(
    Movement_sf %>% select(Sender.ID, Date, DeerPoint),
    join_by(Sender.ID, Date),
    relationship = "many-to-many"
  ) %>%
  # compute pairwise distances
  mutate(Distance = st_distance(HuntPoint, DeerPoint, by_element = TRUE)) %>%
  # take the average
  summarise(
    AvgDistance = mean(Distance),  # this directly gets rid of the unit
    .by = c(Sender.ID, Hunt.ID_timerel1)
  ) %>%
  # if average distance is missing, indiciate by -1
  mutate(AvgDistance = ifelse(is.na(AvgDistance), -1, AvgDistance))

# View(avg_distances)

#-------------------------------------------------------------------------------
# # TBD: what to do if there are not movement data on the day of hunting event?
# # For example, no movement data are available for deer 22399 in 2020,
# # but there is an FCM sample from that deer on 2020-05-12,
# # and there is a hunting event on 2020-05-11.
# avg_distances[25, ] %>% print(width = Inf)
# Movement_sf %>% filter(Sender.ID == "22399")
# FCMStress %>% filter(Sender.ID == "22399")
# HuntEvents %>% filter(year(Date) == 2020, month(Date) == 5)
#-------------------------------------------------------------------------------

# merge back
RelevantHuntEvents <- RelevantHuntEvents %>%
  left_join(
    avg_distances,
    join_by(Sender.ID, Hunt.ID_timerel1)
  )

# View(RelevantHuntEvents)


# get more accurate distances for type 2
interpolated <- RelevantHuntEvents %>%
  distinct(Sender.ID, Hunt.ID_timerel2) %>%
  na.omit() %>%
  # get hunt event time and location
  left_join(
    HuntEvents %>% select(Hunt.ID, X, Y, t_),
    join_by(Hunt.ID_timerel2 == Hunt.ID)
  ) %>%
  rename(HuntTime = t_, HuntX = X, HuntY = Y) %>%
  # find the movement entry just before hunting event
  # again, there are missing movement data
  left_join(
    Movement %>% select(Sender.ID, t_, x_, y_),
    join_by(Sender.ID, closest(HuntTime >= t_))
  ) %>%
  rename(t_before = t_, x_before = x_, y_before = y_) %>%
  # find the movement entry just after the hunting event
  left_join(
    Movement %>% select(Sender.ID, t_, x_, y_),
    join_by(Sender.ID, closest(HuntTime <= t_))
  ) %>%
  rename(t_after = t_, x_after = x_, y_after = y_) %>%
  # interpolate
  mutate(
    timediff = as.numeric(difftime(t_after, t_before, units = "mins")),
    time_traversed = as.numeric(difftime(HuntTime, t_before, units = "mins")),
    InterpolatedX = x_before +
      time_traversed / timediff * (x_after - x_before),
    InterpolatedY = y_before +
      time_traversed / timediff * (y_after - y_before)
  ) %>%
  # if hunting event happens exactly at full hour, e.g., 23:00,
  # t_before == t_after, so no interpolation is needed
  mutate(
    InterpolatedX = ifelse(timediff == 0, x_after, InterpolatedX),
    InterpolatedY = ifelse(timediff == 0, y_after, InterpolatedY)
  )

hunt_points <- interpolated %>%
  select(HuntX, HuntY) %>%
  st_as_sf(coords = c("HuntX", "HuntY"), crs = 32633) %>%
  pull()
deer_points <- interpolated %>%
  select(InterpolatedX, InterpolatedY) %>%
  st_as_sf(
    coords = c("InterpolatedX", "InterpolatedY"), crs = 32633,
    na.fail = FALSE  # again, missing movement data at time of hunting event
  ) %>%
  pull()

interpolated_distances <- interpolated %>%
  select(Sender.ID, Hunt.ID_timerel2) %>%
  mutate(Distance = st_distance(
    hunt_points,
    deer_points,
    by_element = TRUE
  )) %>%
  mutate(Distance = as.numeric(Distance))  # this gets rid of unit

# View(interpolated_distances)

# merge back
RelevantHuntEvents <- RelevantHuntEvents %>%
  left_join(
    interpolated_distances,
    join_by(Sender.ID, Hunt.ID_timerel2)
  ) %>%
  # if average distance is missing, indiciate by -1
  mutate(Distance = ifelse(is.na(Distance), -1, Distance))

# View(RelevantHuntEvents)

# get time- and space-relevant hunting events
RelevantHuntEvents <- RelevantHuntEvents %>%
  mutate(
    Hunt.ID_rel1 = ifelse(
      AvgDistance <= max_distance * 1000 & AvgDistance >= 0,
      Hunt.ID_timerel1,
      NA
    ),
    Hunt.ID_rel2 = ifelse(
      Distance <= max_distance * 1000 & Distance >= 0,
      Hunt.ID_timerel2,
      NA
    )
  )

# find last relevant hunting event
RelevantHuntEvents <- RelevantHuntEvents %>%
  group_by(Sender.ID, DefecTime) %>%
  # -Inf means "last relevant hunting event" does not exist, i.e.,
  # the sample has no relevant hunting event
  mutate(
    Hunt.ID_lastrel1 = max(Hunt.ID_rel1, na.rm = TRUE),
    Hunt.ID_lastrel2 = max(Hunt.ID_rel2, na.rm = TRUE),
  ) %>%
  ungroup()

#-------------------------------------------------------------------------------
# # How many "relevant" data points do we have?
# View(RelevantHuntEvents)
# RelevantHuntEvents %>%
#   filter(!is.na(Hunt.ID_rel2)) %>%
#   distinct(Sender.ID, DefecTime)

# RelevantHuntEvents %>%
#   filter(!is.na(Hunt.ID_rel1)) %>%
#   distinct(Sender.ID, DefecTime)

# RelevantHuntEvents %>%
#   filter(!is.na(Hunt.ID_rel1) | !is.na(Hunt.ID_rel1)) %>%
#   distinct(Sender.ID, DefecTime)
#-------------------------------------------------------------------------------

# only keep the last relevant hunting event
# type 1
data1 <- RelevantHuntEvents %>%
  filter(Hunt.ID_timerel1 == Hunt.ID_lastrel1) %>%
  left_join(
    HuntEvents %>% select(Hunt.ID, Date),
    join_by(Hunt.ID_lastrel1 == Hunt.ID)
  ) %>%
  mutate(DateDiff = date(DefecTime) - Date) %>%
  select(Sender.ID, DefecTime, Hunt.ID_lastrel1, DateDiff, AvgDistance) %>%
  distinct()

View(data1)

data2 <- RelevantHuntEvents %>%
  filter(Hunt.ID_timerel2 == Hunt.ID_lastrel2) %>%
  left_join(
    HuntEvents %>% select(Hunt.ID, t_),
    join_by(Hunt.ID_lastrel2 == Hunt.ID)
  ) %>%
  mutate(TimeDiff = difftime(DefecTime, t_, units = "hours")) %>%
  select(Sender.ID, DefecTime, Hunt.ID_lastrel2, TimeDiff, Distance) %>%
  distinct()

View(data2)


data <- full_join(data1, data2)
# data$TimeDiff[is.na(data$TimeDiff)] <- 0
# data$Distance[is.na(data$Distance)] <- 0
# data$DateDiff[is.na(data$DateDiff)] <- 0
# data$AvgDistance[is.na(data$AvgDistance)] <- 0
  
View(data)

# merge back
data_full <- FCMStress %>%
  left_join(
    data,
    join_by(Sender.ID, DefecTime)
  ) %>%
  mutate(
    TimeDiff = as.numeric(TimeDiff),
    Distance = Distance / 1000,
    AvgDistance = AvgDistance / 1000,
    TimeDiffInvSq = 1 / TimeDiff,
    DistanceInvSq = 1 / Distance^2,
    AvgDistanceInvSq = 1 / AvgDistance^2,ng_g
  )
 
data_full$TimeDiff[is.na(data_full$TimeDiff)] <- 0
data_full$Distance[is.na(data_full$Distance)] <- 0
data_full$DateDiff[is.na(data_full$DateDiff)] <- 0
data_full$AvgDistance[is.na(data_full$AvgDistance)] <- 0

View(data_full)
summary(data_full)

# LMM
library(lme4)
m1 <- lmer(
  ng_g ~ TimeDiffInvSq + DistanceInvSq + TimeDiffInvSq * DistanceInvSq +
    factor(DateDiff) + AvgDistanceInvSq +
    (1 | Sender.ID),
  data_full
)
summary(m1)

m2 <- lmer(
  ng_g ~ TimeDiff + Distance + TimeDiff * Distance +
    factor(DateDiff) + AvgDistance +
    (1 | Sender.ID),
  data_full
)
summary(m2)
