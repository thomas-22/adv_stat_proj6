library(geosphere) # to calculate the geographic distance
library(tidyverse)
library(lubridate)
library(geosphere)
library(sf)
library(mgcv)

# assuming fcm_data, movement_data, and hunt_data to be loaded already
HuntsReduced <- as.data.frame(HuntsReduced)
FCMStress$Collar_t_ <- ymd_hms(FCMStress$Collar_t_)
Movement$t_ <- ymd_hms(Movement$t_)
HuntsReduced$t_ <- ymd_hms(HuntsReduced$t_)

# to make sure all the columns to have the correct Data type
FCMStress$Sender.ID <- as.character(FCMStress$Sender.ID)
Movement$Sender.ID <- as.character(Movement$Sender.ID)

#Add latitude and longitude to three tables
FCMStress_sf <- st_as_sf(FCMStress, coords = c("X", "Y"), crs = 32633)  
FCMStress_sf <- st_transform(FCMStress_sf, crs = 4326)  
FCMStress$lon <- st_coordinates(FCMStress_sf)[, 1]
FCMStress$lat <- st_coordinates(FCMStress_sf)[, 2]

Movement_sf <- st_as_sf(Movement, coords = c("x_", "y_"), crs = 32633)  
Movement_sf <- st_transform(Movement_sf, crs = 4326)
Movement$lon <- st_coordinates(Movement_sf)[, 1]
Movement$lat <- st_coordinates(Movement_sf)[, 2]

FCMStress <- FCMStress %>%
  dplyr::mutate(between_time = as.numeric(Waypoint_t_ - Collar_t_))

# transform the table of HuntsReduced
HuntsReduced_sf <- st_as_sf(HuntsReduced, coords = c("X", "Y"), crs = 32633)  
HuntsReduced_sf <- st_transform(HuntsReduced_sf, crs = 4326)  
HuntsReduced$lon <- st_coordinates(HuntsReduced_sf)[, 1]
HuntsReduced$lat <- st_coordinates(HuntsReduced_sf)[, 2]
movement_data <- Movement %>%
  arrange(Sender.ID, t_) %>% # sort by individual and time
  group_by(Sender.ID) %>%
  mutate(
    movement_distance = distHaversine(
      cbind(lag(lon), lag(lat)), #  last point
      cbind(lon, lat)            #  current point
    ) / 1000                  # transform the unit to the km
  ) %>%
  ungroup()

FCMStress_with_movement <- FCMStress %>%
  left_join(movement_data, by = c("Sender.ID", "Collar_t_" = "t_"))

FCMStress_with_movement <- FCMStress_with_movement %>%
  dplyr::select(ng_g, Sender.ID, between_time, movement_distance) %>%
  dplyr::filter(!is.na(ng_g) & !is.na(Sender.ID)  & !is.na(between_time) & !is.na(movement_distance)) %>%
  dplyr::filter(between_time >= 0 & between_time <= 15)

gamm_model_no_hunt <- gamm(
  ng_g ~ s(movement_distance) + s(between_time),
  random = list(Sender.ID = ~1), # Individual Random Effects
  data = FCMStress_with_movement,
  family = Gamma(link = "log")  # Gamma distribution
)

summary(gamm_model_no_hunt$gam)
plot(gamm_model_no_hunt$gam, pages = 1, scheme = 1, residuals = TRUE)
plot(gamm_model_no_hunt$gam, pages = 1, scheme = 2)


fcmplot_bettime_nohunt <- ggplot(FCMStress_with_movement, aes(x = between_time, y = ng_g)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), color = "red", se = TRUE) +
  labs(title = "FCM Levels vs time between collar and collect", x = "betweentime", y = "FCM Levels (ng/g)") +
  theme_minimal()
fcmplot_movement_nohunt <-  ggplot(FCMStress_with_movement, aes(x = movement_distance, y = ng_g)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), color = "red", se = TRUE) +
  labs(title = "FCM Levels vs time between collar and collect", x = "betweentime", y = "FCM Levels (ng/g)") +
  theme_minimal()
