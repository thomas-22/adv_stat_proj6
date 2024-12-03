# # library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(readxl)
# library(ggrepel)
# library(viridis)
# library(scales)
# library(lubridate)
# library(geosphere)
# library(sf)
# 
# #just messing around with the data
# 
# #getwd()
# source("./R/Datafusion.R")
# 
# # Round coordinates
# Movement$x_ <- as.numeric(round(Movement$x_, 0))
# HuntEventsreduced$X <- as.numeric(round(HuntEventsreduced$X, 0))
# 
# # Select a random Sender.ID from the dataset
# set.seed(123)  # Setting a seed for reproducibility
# random_sender <- sample(unique(Movement$Sender.ID), 1)
# 
# # Filter data for the selected Sender.ID and arrange by time
# sender_data <- Movement %>%
#   filter(Sender.ID == random_sender) %>%
#   arrange(t_)
# 
# # Extract the date component and find all unique dates for this Sender.ID
# unique_dates <- as.Date(sender_data$t_)
# unique_dates <- unique(unique_dates)
# 
# # Select a random starting date for the 7-day period
# start_date <- sample(unique_dates, 1)
# end_date <- start_date + 6 # 31 consecutive days
# 
# # Filter data for the selected 7-day period
# week_data <- sender_data %>%
#   filter(as.Date(t_) >= start_date & as.Date(t_) <= end_date)
# 
# # Plot the movement for the 7-day period
# ggplot(week_data, aes(x = x_, y = y_)) +
#   geom_path(aes(group = 1), color = "blue") +  # Draw lines between points
#   geom_point(color = "red", size = 2) +        # Add points at each location
#   labs(
#     title = paste("Movement Path for Sender ID:", random_sender, 
#                   "from", start_date, "to", end_date),
#     x = "x-coordinate",
#     y = "y-coordinate"
#   ) +
#   theme_minimal()
# 
# 
