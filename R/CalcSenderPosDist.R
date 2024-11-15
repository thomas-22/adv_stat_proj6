library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(viridis)
library(scales)
library(lubridate)
library(geosphere)
library(sf)

#getwd()
source("./R/Datafusion.R")

# Initialize an empty data frame
interpolated_positions <- data.frame()

# Loop through all hunting events
for (i in 1:nrow(HuntEventsreduced)) {
  cat(sprintf("Interpolate Positions: %d of %d", i, nrow(HuntEventsreduced)), "\r")
  #cat("\n")
  hunt_time <- HuntEventsreduced$t_[i]
  hunt_x <- HuntEventsreduced$X[i]
  hunt_y <- HuntEventsreduced$Y[i]
  
  # For each hunting event, loop through each unique Sender.ID in movement data
  for (sender_id in unique(Movement$Sender.ID)) {
    # Filter movement data for the specific sender
    sender_data <- Movement[Movement$Sender.ID == sender_id, ]
    
    # Sort data
    sender_data <- sender_data[order(sender_data$t_), ]
    
    # Find the row just before the hunting event
    before_event <- sender_data[sender_data$t_ <= hunt_time, ]
    if (nrow(before_event) > 0) {
      before_event <- before_event[nrow(before_event), ] # Last row before event
    } else {
      next
    }
    
    # Find the row just after the hunting event
    after_event <- sender_data[sender_data$t_ >= hunt_time, ]
    if (nrow(after_event) > 0) {
      after_event <- after_event[1, ] # First row after event
    } else {
      next
    }
    
    # Check if we have both a before and after row
    if (!is.null(before_event) && !is.null(after_event)) {
      # Get coordinates and time difference
      x1 <- as.numeric(before_event$x_)
      y1 <- as.numeric(before_event$y_)
      time1 <- before_event$t_
      
      x2 <- as.numeric(after_event$x_)
      y2 <- as.numeric(after_event$y_)
      time2 <- after_event$t_
      
      # Calculate distance in meters and time difference in seconds
      point1 <- st_point(c(x1, y1))
      point2 <- st_point(c(x2, y2))
      utm_points <- st_sfc(point1, point2, crs = 32633)
      distance <- st_distance(utm_points[1], utm_points[2])
      
      time_diff <- as.numeric(difftime(time2, time1, units = "secs"))
      
      # Calculate average speed (meters per second)
      speed <- distance / time_diff
      
      # Calculate time difference from before event to hunting event
      time_to_hunt <- as.numeric(difftime(hunt_time, time1, units = "secs"))
      
      # Calculate interpolation factor (fraction of distance covered)
      interpolation_factor <- time_to_hunt / time_diff
      
      # Interpolate the position at the hunting event time
      interpolated_x <- x1 + interpolation_factor * (x2 - x1)
      interpolated_y <- y1 + interpolation_factor * (y2 - y1)
      
      # Append result
      interpolated_positions <- rbind(interpolated_positions, data.frame(
        Sender.ID = sender_id,
        HuntEventTime = hunt_time,
        HuntEventX = hunt_x,
        HuntEventY = hunt_y,
        InterpolatedX = interpolated_x,
        InterpolatedY = interpolated_y,
        Speed = speed
      ))
    }
  }
}

#Kick out rows with NAs
StressEvents <- na.omit(interpolated_positions)

#Create the column that contains the distance 
#between the hunting event and the interpolated position
StressEvents$Distance <- rep(0, nrow(StressEvents))
cat("\n")
#Calculate the distances
for (i in 1:nrow(StressEvents)) {
  cat(sprintf("Distance Calculation: %d of %d", i, nrow(StressEvents)), "\r")
  point1 <- st_point(c(StressEvents$HuntEventX[i],
                       StressEvents$HuntEventY[i]))
  point2 <- st_point(c(StressEvents$InterpolatedX[i],
                       StressEvents$InterpolatedY[i]))
  utm_points <- st_sfc(point1, point2, crs = 32633)
  StressEvents$Distance[i] <- st_distance(utm_points[1], utm_points[2])
}
cat("\n")
StressEvents <- StressEvents %>%
  mutate(StressorID = row_number())
