library(dplyr)
library(lubridate)
library(geosphere)
library(sf)
library(mgcv)

# assume that fcm_data, movement_data, and hunt_data are dataframes which are alreay loaded
# Formatting the date and time
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

# Transform the table HuntsReduced
HuntsReduced_sf <- st_as_sf(HuntsReduced, coords = c("X", "Y"), crs = 32633)  
HuntsReduced_sf <- st_transform(HuntsReduced_sf, crs = 4326)  
HuntsReduced$lon <- st_coordinates(HuntsReduced_sf)[, 1]
HuntsReduced$lat <- st_coordinates(HuntsReduced_sf)[, 2]

# define the parameter
distance_threshold <- 5000  # threshold of the distance to be 5 km

# initialize the table of results
results_table <- data.frame(
  Sender.ID = character(),
  Stress_t = as.POSIXct(character()),
  defecation_t = as.POSIXct(character()),
  nearest_hunt_time = as.POSIXct(character()),
  distance_to_hunt = numeric(),
  stringsAsFactors = FALSE
)

# overview each line of the table FCMStress
for (i in 1:nrow(FCMStress)) {
  fcm_row <- FCMStress[i, ]
  
  # 1. calculate the time of the stress events
  stress_time <- fcm_row$Collar_t_ - 19 * 60 * 60  # trace back to 19 hours
  
  # 2. select the hunting events
  valid_hunt_events <- HuntsReduced %>%
    filter(
      abs(difftime(stress_time, t_, units = "hours")) <= 5  # 5 hours around the stress events
    )
  
  # If there are no relevant hunting events, then skip the loop
  if (nrow(valid_hunt_events) == 0) next
  
  # 3. select the relevant movements records of the red deers
  movement_candidates <- Movement %>%
    filter(Sender.ID == fcm_row$Sender.ID)  # filter the relevant data of the red deers
  
  # If there are no relevant movement records, then skip the loop
  if (nrow(movement_candidates) == 0) next
  
  # 4. overview valid hunting events and then calculate the distance
  for (j in 1:nrow(valid_hunt_events)) {
    hunt_event <- valid_hunt_events[j, ]
    
    # make sure the existence of the latitude and longituude
    if (is.na(hunt_event$lon) || is.na(hunt_event$lat)) next
    
    # calculate the distance between the location of each red deer and the relevant hunting events
    distances <- movement_candidates %>%
      filter(!is.na(x_) & !is.na(y_)) %>%  # exclude NA
      mutate(
        distance_to_hunt = distHaversine(
          c(hunt_event$lon, hunt_event$lat),  # the location of hunting events
          cbind(lon, lat)                       # matrix of the location of red deers
        )
      ) %>%
      filter(distance_to_hunt <= distance_threshold) %>%  # select the records which meet the distance conditions
      arrange(distance_to_hunt)  # sort by distance
    
    # if there are no records which meet the distance conditions, then skip the current hunting events
    if (nrow(distances) == 0) next
    
    # choose the nearest records in distance(spatial)
    closest_distance <- distances[1, ]
    
    # 5. save the results in the table
    results_table <- rbind(
      results_table,
      data.frame(
        Sender.ID = fcm_row$Sender.ID,
        Stress_t = stress_time,
        defecation_t = fcm_row$Collar_t_,
        nearest_hunt_time = hunt_event$t_,
        distance_to_hunt = closest_distance$distance_to_hunt,
        stringsAsFactors = FALSE
      )
    )
    
    # Once the corret events are found, skip outside the inner loop
    break
  }
}

results_table <- results_table %>%
  dplyr::mutate(time_to_hunt = defecation_t - nearest_hunt_time)
results_table[,"time_to_hunt"] <- as.numeric(results_table[,"time_to_hunt"] )

FCMStress <- FCMStress %>%
  dplyr::mutate(between_time =as.numeric(Waypoint_t_ - Collar_t_))

FCMStress_gamm <- FCMStress %>%
  left_join(
    results_table %>% 
      dplyr::select(Sender.ID, distance_to_hunt, time_to_hunt),
    by = c("Sender.ID" = "Sender.ID")
  ) 

FCMStress_gamm <- FCMStress_gamm %>%
  filter(!is.na(ng_g) & !is.na(distance_to_hunt) & !is.na(Sender.ID) & !is.na(time_to_hunt) & !is.na(between_time)) %>%
  dplyr::select(ng_g, distance_to_hunt, time_to_hunt, Sender.ID, between_time) %>%
  dplyr::filter(between_time > 0 & between_time <15)

gamm_model_assume_gamma <- gamm(
   ng_g ~ s(distance_to_hunt) + s(time_to_hunt) + s(between_time),
  random = list(Sender.ID = ~1),
  data = FCMStress_gamm,
  family = Gamma(link = "log")
)

# check the results of the models
summary(gamm_model_assume_gamma$gam)
summary(gamm_model_assume_gamma$lme)

# extract the residuals and the fitted value
residuals <- residuals(gamm_model_assume_gamma$gam)
fitted_values <- fitted(gamm_model_assume_gamma$gam)

# Residual Diagnostic plot
residual_plot <- ggplot(data = data.frame(residuals, fitted_values), aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# examine the normalitz
qq_plot <- ggplot(data = data.frame(residuals), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

# display the diagnose
residual_plot
qq_plot

# extract the predictions
predicted_values <- predict(gamm_model_assume_gamma$gam, newdata = FCMStress_gamm)

gamm_model_assume_normal <- gamm(
  ng_g ~ s(distance_to_hunt) + s(time_to_hunt) + s(between_time),
  random = list(Sender.ID = ~1),
  data = FCMStress_gamm,
  family = gaussian()
)
predicted_values1 <- predict(gamm_model_assume_normal$gam, newdata = FCMStress_gamm)


# visualize the trend of the Data
trend_plot_distance_gamma <- ggplot(FCMStress_gamm, aes(x = distance_to_hunt, y = ng_g)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), color = "red", se = TRUE) +
  labs(title = "FCM Levels vs Distance to Hunt", x = "Distance to Hunt (scaled)", y = "FCM Levels (ng/g)") +
  theme_minimal()

trend_plot_time_gamma <- ggplot(FCMStress_gamm, aes(x = timediff, y = ng_g)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), color = "red", se = TRUE) +
  labs(title = "FCM Levels vs time after Hunting", x = "Time to Hunt", y = "FCM Levels (ng/g)") +
  theme_minimal()

trend_plot_between_gamma <- ggplot(FCMStress_gamm, aes(x = between_time, y = ng_g)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), color = "red", se = TRUE) +
  labs(title = "FCM Levels vs time between defection and collection", x = "Time between coller and sample", y = "FCM Levels (ng/g)") +
  theme_minimal()

trend_plot_distance 
trend_plot_time
trend_plot_distance_gamma

# extract the results of smoothing functions
plot(gamm_model_assume_gamma$gam, pages = 1, scheme = 1, residuals = TRUE)

# plot the smoothing effect
plot(gamm_model_pori_gamma$gam, pages = 1, scheme = 2)

# calculate the RMSE_gamma
actual_values <- FCMStress_gamm$ng_g
rmse <- sqrt(mean((predicted_values - actual_values)^2))
print(paste("RMSE_gamma:", rmse))

# calculate the RMSE_normal
actual_values1 <- FCMStress_gamm$ng_g
rmse <- sqrt(mean((predicted_values1 - actual_values1)^2))
print(paste("RMSE_normal:", rmse))
