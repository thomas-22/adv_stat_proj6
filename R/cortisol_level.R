library(dplyr)
library(lubridate)
library(geosphere)
library(sf)

# Assuming that FCMstress, Movement, and HuntEventreduced are already loaded dataframes
# Format the date and time
FCMStress$Collar_t_ <- ymd_hms(FCMStress$Collar_t_)
Movement$t_ <- ymd_hms(Movement$t_)
HuntEventsreduced$t_ <- ymd_hms(HuntEventsreduced$t_)

# Make sure it's listed as the correct data type
FCMStress$Sender.ID <- as.character(FCMStress$Sender.ID)
Movement$Sender.ID <- as.character(Movement$Sender.ID)

# Ensure that data frames have X and Y coordinate columns and set their CRS to ETRS UTM 33N (EPSG 32633)
# Convert the FCMStress table
FCMStress_sf <- st_as_sf(FCMStress, coords = c("X", "Y"), crs = 32633)  # EPSG 32633 为 ETRS UTM 33N
FCMStress_sf <- st_transform(FCMStress_sf, crs = 4326)  # EPSG 4326 为 WGS 84
FCMStress$lon <- st_coordinates(FCMStress_sf)[, 1]
FCMStress$lat <- st_coordinates(FCMStress_sf)[, 2]

# Convert Movement 
Movement_sf <- st_as_sf(Movement, coords = c("x_", "y_"), crs = 32633)  # EPSG 32633 为 ETRS UTM 33N
Movement_sf <- st_transform(Movement_sf, crs = 4326)  # EPSG 4326 为 WGS 84
Movement$lon <- st_coordinates(Movement_sf)[, 1]
Movement$lat <- st_coordinates(Movement_sf)[, 2]

# Convert HuntEventsreduced 
HuntEventsreduced_sf <- st_as_sf(HuntEventsreduced, coords = c("X", "Y"), crs = 32633)  # EPSG 32633 为 ETRS UTM 33N
HuntEventsreduced_sf <- st_transform(HuntEventsreduced_sf, crs = 4326)  # EPSG 4326 为 WGS 84
HuntEventsreduced$lon <- st_coordinates(HuntEventsreduced_sf)[, 1]
HuntEventsreduced$lat <- st_coordinates(HuntEventsreduced_sf)[, 2]

results_table <- data.frame(
  Sender.ID = character(),
  Stress_t = as.POSIXct(character()),
  defecation_t = as.POSIXct(character()),
  nearest_hunt_time = as.POSIXct(character()),
  distance_to_hunt = numeric(),
  stringsAsFactors = FALSE
)

# Iterate through each row of the FCMStress table to perform calculations
for (i in 1:nrow(FCMStress)) {
  fcm_row <- FCMStress[i, ]
  
  # Calculate the time of the stressful event (19 hours back)
  stress_time <- fcm_row$Collar_t_ - 19 * 60 * 60

  # Check if the corresponding Sender.ID and time close to stress_time exists in the Movement table.
  movement_candidates <- Movement %>%
    filter(Sender.ID == fcm_row$Sender.ID)

  if (nrow(movement_candidates) > 0) {
    movement_row <- movement_candidates %>%
      filter(abs(difftime(t_, stress_time, units = "mins")) <= 30) %>%
      arrange(abs(difftime(t_, stress_time, units = "mins"))) %>%
      slice(1)

    #Here we chose to extract data from 5 to 40 hours after hunting time
    if (nrow(movement_row) > 0) {
      valid_hunt_times <- HuntEventsreduced$t_[
        difftime(fcm_row$Collar_t_, HuntEventsreduced$t_, units = "hours") >= 5 &
        difftime(fcm_row$Collar_t_, HuntEventsreduced$t_, units = "hours") <= 40
      ]

      nearest_hunt_time <- if (length(valid_hunt_times) > 0) {
        max(valid_hunt_times, na.rm = TRUE)
      } else {
        NA
      }

      if (!is.na(nearest_hunt_time)) {
        nearest_hunt_index <- which.min(abs(HuntEventsreduced$t_ - nearest_hunt_time))
        
        # Distance calculations using converted latitude and longitude
        distance_to_hunt <- distHaversine(
          c(movement_row$lon, movement_row$lat),
          c(HuntEventsreduced$lon[nearest_hunt_index], HuntEventsreduced$lat[nearest_hunt_index])
        )
      } else {
        distance_to_hunt <- NA
      }

      # Ensure that all columns have valid data before adding rows to the result table
      if (!is.na(distance_to_hunt)) {
        results_table <- rbind(
          results_table,
          data.frame(
            Sender.ID = fcm_row$Sender.ID,
            Stress_t = stress_time,
            defecation_t = fcm_row$Collar_t_,
            nearest_hunt_time = nearest_hunt_time,
            distance_to_hunt = distance_to_hunt,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
}



head(results_table)

# Rename columns in the FCMStress dataframe to match the needs of the merge
FCMStress <- FCMStress %>%
  rename(FCM_level = ng_g) %>%
  rename(defecation_t = Collar_t_)

# Merge results_table and FCMStress, using Sender.ID and defecation_t as keys
results_table_com <- results_table %>%
  left_join(FCMStress, by = c("Sender.ID", "defecation_t"))

# Add time_difference column (time difference between hunting events and defecation events in hours)
results_table_com <- results_table_com %>%
  mutate(time_difference = as.numeric(difftime(defecation_t, nearest_hunt_time, units = "hours")))

# Extracting data after 30 hours
post_30_hours_data <- results_table_com %>%
  filter(time_difference >= 30)

# Extracting data after 36 hours
post_36_hours_data <- results_table_com %>%
  filter(time_difference >= 36)

# Extracting data after Hunting event between 0 and 18 hours
pre_hunt_data <- results_table_com %>%
  filter(time_difference < 18)

# Comparison of FCM levels to basal levels at 30/36 hours using t-tests
t_test_result30 <- t.test(post_30_hours_data$FCM_level, pre_hunt_data$FCM_level)
t_test_result36 <- t.test(post_36_hours_data$FCM_level, pre_hunt_data$FCM_level)

print(t_test_result30)
print(t_test_result36)

# Visualize FCM_level change with time_difference
ggplot(results_table_com, aes(x = time_difference, y = FCM_level)) +
  geom_point(color = "blue", alpha = 0.6) +  # 绘制散点图
  geom_smooth(method = "loess", color = "red", se = TRUE) +  # 绘制平滑曲线
  labs(
    title = "FCM Level Changes Over Time After Hunting Event",
    x = "Time Difference (hours)",
    y = "FCM Level (ng/g)"
  ) +
  theme_minimal()
