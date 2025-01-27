library(dplyr)
library(ggplot2)
#install.packages("plotly")
library(plotly)
#install.packages("ggtext")
library(ggtext)
library(ggrepel)

#RUN "Assign_FCMData_to_Stressors.R" FIRST

##########################
#PLOTTING:
##########################
plot_ng_as_func_of_dist_timediff <- function(data, chosen_var = "Distance", add_to_title = c("")) {
  
  unique_sample_ids <- data %>%
    distinct(Sample_ID)
  
  usiv <- unique_sample_ids$Sample_ID
  
  fcm_reference <- FCMStress %>%
    filter(!(Sample_ID %in% usiv))
  
  t_test_fcm <- t.test(data$ng_g, fcm_reference$ng_g)
  
  print(t_test_fcm)
  
  specific_mean <- mean(data$ng_g, na.rm = TRUE)
  specific_n <- sum(!is.na(data$ng_g))
  
  reference_mean <- mean(fcm_reference$ng_g, na.rm = TRUE)
  reference_n <- sum(!is.na(fcm_reference$ng_g))
  
  lm_formula <- as.formula(paste("ng_g ~", chosen_var))
  
  lm1 <- lm(lm_formula, data = data)
  slope <- lm1$coefficients[2]
  
  result <- ggplot(data, aes_string(x = chosen_var, y = "ng_g")) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
    geom_hline(yintercept = reference_mean, color = "red", linetype = "dashed") +
    geom_hline(yintercept = specific_mean, color = "blue", linetype = "dashed") +
    scale_x_log10() +
    labs(
      title = paste("Plot of ng_g vs", chosen_var,
                    ",", add_to_title),
      x = chosen_var,
      y = "ng_g"
    ) +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
             label = paste("data (blue):",
                           "\nn =", specific_n,
                           "\nmean =", round(specific_mean, 2)),
             color = "blue", size = 3.5) +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 2.33,
             label = paste("fcm_reference (red):",
                           "\nn =", reference_n,
                           "\nmean =", round(reference_mean, 2)),
             color = "red", size = 3.5) +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 2.5,
             label = paste("Gut Retention Lower Boundary (Hours):", gut_retention_time_lower, 
                           "\nGut Retention Upper Boundary (Hours):", gut_retention_time_upper, 
                           "\nDistance Threshold (Meters):", distance_threshold, 
                           "\nT_Test:", round(t_test_fcm$statistic, 2),
                           "\nT_Test p_val:", round(t_test_fcm$p.value, 2)),
             color = "green", size = 3.5) +
    theme_minimal() +
    theme(plot.margin = margin(5, 50, 5, 5))
  
  return(result)
}

plot_data_2d <- function() {
  p1 <- plot_ng_as_func_of_dist_timediff(data_cleanedup,
                                   add_to_title = "Data: Discard duplicates, keep lowest score")
  p2 <- plot_ng_as_func_of_dist_timediff(data_cleanedup,
                                   chosen_var = c("TimeDiff"),
                                   add_to_title = "Data: Discard duplicates, keep lowest score")
  
  p3 <- plot_ng_as_func_of_dist_timediff(data_cleanedup_min_distance,
                                   add_to_title = "Data: Discard duplicates, keep lowest distance")
  
  p4 <- plot_ng_as_func_of_dist_timediff(data_cleanedup_min_distance,
                                   chosen_var = c("TimeDiff"),
                                   add_to_title = "Data: Discard duplicates, keep lowest distance")
  
  p5 <- plot_ng_as_func_of_dist_timediff(combo_min_dist_timediff,
                                   add_to_title = "Data: keep lowest distance, keep lowest timediff,\ncombine")
  p6 <- plot_ng_as_func_of_dist_timediff(combo_min_dist_timediff,
                                   chosen_var = "TimeDiff",
                                   add_to_title = "Data: keep lowest distance, keep lowest timediff,\ncombine")
  
  p7 <- plot_ng_as_func_of_dist_timediff(combo_min_dist_timediff_no_dupes,
                                   add_to_title = "Data: keep lowest distance, keep lowest timediff,\ndiscard duplicates")
  p8 <- plot_ng_as_func_of_dist_timediff(combo_min_dist_timediff_no_dupes,
                                   chosen_var = "TimeDiff",
                                   add_to_title = "Data: keep lowest distance, keep lowest timediff,\ndiscard duplicates")
  
  return(list(
    p1 = p1,
    p2 = p2,
    p3 = p3,
    p4 = p4,
    p5 = p5,
    p6 = p6,
    p7 = p7,
    p8 = p8
  ))
}

plot_data_3d <- function() {
  
  #Total FCMData_Assigned (Without nonsense)
  p0 <- plot_ly(interesting_data, x = ~Distance, y = ~as.numeric(TimeDiff), z = ~ng_g,
                type = "scatter3d", mode = "markers",
                marker = list(size = 1, color = 'blue',
                              line = list(color = 'black', width = 2))) %>%
    layout(title = "Data: All Assignments (Without Nonsense)",
           scene = list(
             xaxis = list(title = 'Distance'),
             yaxis = list(title = 'TimeDiff'),
             zaxis = list(title = 'ng_g')
           ))
  
  
  #Data with lowest scores only
  p1 <- plot_ly(data_cleanedup, x = ~Distance, y = ~as.numeric(TimeDiff), z = ~ng_g,
          type = "scatter3d", mode = "markers",
          marker = list(size = 1, color = 'blue',
                        line = list(color = 'black', width = 2))) %>%
    layout(title = "Data: Discard duplicates, keep lowest score",
           scene = list(
             xaxis = list(title = 'Distance'),
             yaxis = list(title = 'TimeDiff'),
             zaxis = list(title = 'ng_g')
           ))
  
  #Keep only min distance combo for each sample:
  p2 <- plot_ly(data_cleanedup_min_distance, x = ~Distance, y = ~as.numeric(TimeDiff), z = ~ng_g,
          type = "scatter3d", mode = "markers",
          marker = list(size = 1, color = 'blue',
                        line = list(color = 'black', width = 2))) %>%
    layout(title = "Data: Discard duplicates, keep lowest distance",
           scene = list(
             xaxis = list(title = 'Distance'),
             yaxis = list(title = 'TimeDiff'),
             zaxis = list(title = 'ng_g')
           ))
  
  #Keep only min timediff combo for each sample:
  p3 <- plot_ly(data_cleanedup_min_timediff, x = ~Distance, y = ~as.numeric(TimeDiff), z = ~ng_g,
          type = "scatter3d", mode = "markers",
          marker = list(size = 1, color = 'blue',
                        line = list(color = 'black', width = 2))) %>%
    layout(title = "Data: Discard duplicates, keep lowest TimeDiff",
           scene = list(
             xaxis = list(title = 'Distance'),
             yaxis = list(title = 'TimeDiff'),
             zaxis = list(title = 'ng_g')
           ))
  
  
  
  p4 <- plot_ly(combo_min_dist_timediff, x = ~Distance, y = ~as.numeric(TimeDiff), z = ~ng_g,
          type = "scatter3d", mode = "markers",
          marker = list(size = 1, color = 'blue',
                        line = list(color = 'black', width = 2))) %>%
    layout(title = "Data: keep lowest distance, keep lowest timediff,\ncombine",
           scene = list(
             xaxis = list(title = 'Distance'),
             yaxis = list(title = 'TimeDiff'),
             zaxis = list(title = 'ng_g')
           ))
  
  
  #Remove all duplicates (drops some samples):
  #nrow(combo_min_dist_timediff[!duplicated(combo_min_dist_timediff$Sample_ID), ])
  #601
  
  p5 <- plot_ly(combo_min_dist_timediff[!duplicated(combo_min_dist_timediff$Sample_ID), ], x = ~Distance, y = ~as.numeric(TimeDiff), z = ~ng_g,
          type = "scatter3d", mode = "markers",
          marker = list(size = 1, color = 'blue',
                        line = list(color = 'black', width = 2))) %>%
    layout(title = "Data: keep lowest distance, keep lowest timediff,\ndiscard duplicates",
           scene = list(
             xaxis = list(title = 'Distance'),
             yaxis = list(title = 'TimeDiff'),
             zaxis = list(title = 'ng_g')
           ))
  
  return(list(
    p0 = p0,
    p1 = p1,
    p2 = p2,
    p3 = p3,
    p4 = p4,
    p5 = p5
  ))
}


generate_hist_timediff <- function(){
  hist(as.numeric(data_cleanedup$TimeDiff), breaks = 120)
}

plot_lognorm_gamma_univar_independent <- function(){
  ###############################################################
  #Discussion about Log-Normal vs Gamma Fit for $ng_g
  ###############################################################
  data1 <- FCMStress %>%
    filter(ng_g > 0)
  
  data <- data1$ng_g
  
  ### Fit Log-Normal Distribution ###
  lognorm_fit <- MASS::fitdistr(data, "lognormal")
  meanlog <- lognorm_fit$estimate["meanlog"]
  sdlog <- lognorm_fit$estimate["sdlog"]
  
  ### Fit Gamma Distribution ###
  gamma_fit <- MASS::fitdistr(data, "gamma")
  shape <- gamma_fit$estimate["shape"]
  rate <- gamma_fit$estimate["rate"]
  
  ### Plot Histogram ###
  hist_data <- hist(data, breaks = 100, probability = TRUE, 
                    main = "Histogram with Log-Normal and Gamma Fits", 
                    xlab = "FCMStress$ng_g", col = "lightblue", border = "black")
  
  # Overlay Log-Normal Fit
  curve(dlnorm(x, meanlog = meanlog, sdlog = sdlog), col = "red", lwd = 2, add = TRUE)
  
  # Overlay Gamma Fit
  curve(dgamma(x, shape = shape, rate = rate), col = "blue", lwd = 2, add = TRUE)
  
  ### Calculate MSE for Log-Normal ###
  bin_midpoints <- hist_data$mids
  observed_probs <- hist_data$density
  lognorm_probs <- dlnorm(bin_midpoints, meanlog = meanlog, sdlog = sdlog)
  lognorm_mse <- mean((observed_probs - lognorm_probs)^2)
  
  ### Calculate MSE for Gamma ###
  gamma_probs <- dgamma(bin_midpoints, shape = shape, rate = rate)
  gamma_mse <- mean((observed_probs - gamma_probs)^2)
  
  # Print MSE values
  cat("Mean Squared Error (MSE) for Log-Normal Fit:", lognorm_mse, "\n")
  cat("Mean Squared Error (MSE) for Gamma Fit:", gamma_mse, "\n")
  
  # Add legend to the plot
  legend("topright", legend = c("Data Histogram", "Log-Normal Fit", "Gamma Fit"), 
         col = c("lightblue", "red", "blue"), lty = c(NA, 1, 1), lwd = c(NA, 2, 2), pch = c(15, NA, NA))
  
  # Add MSE values to the plot
  text(x = max(hist_data$breaks) * 0.5, 
       y = max(hist_data$density) * 0.4, 
       labels = paste("MSE Log-Normal: ", round(lognorm_mse, 10)), 
       col = "red", adj = 0)
  
  text(x = max(hist_data$breaks) * 0.5, 
       y = max(hist_data$density) * 0.3, 
       labels = paste("MSE Gamma: ", round(gamma_mse, 10)), 
       col = "blue", adj = 0)
  ###############################################################
}

plot_collar_t_raw <- function(){
  ggplot(FCMStress, aes(x = Collar_t_)) +
    geom_histogram(binwidth = 604800, fill = "lightblue", color = "black") + # Binwidth = 1 Week = 604800 seconds
    scale_x_datetime(
      date_labels = "%Y-%m-%d",
      date_breaks = "60 days"
    ) + 
    labs(
      title = "Histogram of Collar_t_",
      x = "Timestamp",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
}

#Draws a plot to illustrate why and how we interpolate the positions of the deer
Draw_Illustration_Map <- function(){
  
  # Define individual points
  deer_time_0 <- c(2, 4.5)       # x, y coordinates for Deer @ Time 0
  deer_time_1 <- c(1, 2)         # x, y coordinates for Deer @ Time 1
  hunt_event <- c(4.5, 2.5)      # x, y coordinates for Hunt Event @ Time 0.75
  fecal_sample <- c(1.5, 0.5)    # x, y coordinates for Fecal Sample @ Time 5
  
  # Calculate interpolated position at Time 0.75 (75% from Time 0 to Time 1)
  interpolated_position <- c(
    deer_time_0[1] + 0.75 * (deer_time_1[1] - deer_time_0[1]), # x coordinate
    deer_time_0[2] + 0.75 * (deer_time_1[2] - deer_time_0[2])  # y coordinate
  )
  
  # Calculate distance between Hunt Event and Interpolated Position
  distance <- sqrt((hunt_event[1] - interpolated_position[1])^2 + 
                     (hunt_event[2] - interpolated_position[2])^2)
  distance <- round(distance, 2) # Round to two decimal places for readability
  
  # Combine all points into a data frame
  data <- data.frame(
    x = c(deer_time_0[1], deer_time_1[1], interpolated_position[1], hunt_event[1], fecal_sample[1]),
    y = c(deer_time_0[2], deer_time_1[2], interpolated_position[2], hunt_event[2], fecal_sample[2]),
    label = c("Deer @ Time 0", 
              "Deer @ Time 1", 
              "Interpolated Position @ Time 0.75", 
              "Hunt Event @ Time 0.75", 
              "Fecal Sample @ Time 5\n(TimeDiff: 5 - 0.75 = 4.25 Hrs & Distance: 3.25 Km)"),
    type = c("Deer", "Deer", "Deer Interpolated", "Hunt Event", "Fecal Sample")
  )
  
  # Create the map with improved label positioning, aspect ratio, and additional features
  ggplot(data, aes(x = x, y = y, color = type, label = label)) +
    geom_point(size = 3) + # Plot points
    geom_text_repel( # Use ggrepel for better label placement
      size = 3.25, # Uniform font size
      max.overlaps = Inf
    ) +
    scale_color_manual(values = c("Deer" = "blue", 
                                  "Deer Interpolated" = "darkgreen", 
                                  "Hunt Event" = "red", 
                                  "Faecal Sample" = "purple")) + # Customize colors
    # Add dashed line between Hunt Event and Interpolated Position
    geom_segment(aes(x = hunt_event[1], y = hunt_event[2], 
                     xend = interpolated_position[1], yend = interpolated_position[2]),
                 color = "black", linetype = "dashed", size = 0.5) +
    # Add thin dashed blue line between Deer @ Time 0 and Deer @ Time 1
    geom_segment(aes(x = deer_time_0[1], y = deer_time_0[2],
                     xend = deer_time_1[1], yend = deer_time_1[2]),
                 color = "blue", linetype = "dashed", size = 0.5) +
    # Add thin dashed blue line between Deer @ Time 1 and Fecal Sample
    geom_segment(aes(x = deer_time_1[1], y = deer_time_1[2],
                     xend = fecal_sample[1], yend = fecal_sample[2]),
                 color = "blue", linetype = "dashed", size = 0.5) +
    # Add text for the distance label below the connecting line
    annotate("text", 
             x = mean(c(hunt_event[1], interpolated_position[1])), 
             y = mean(c(hunt_event[2], interpolated_position[2])) - 0.2, # Adjusted to place below
             label = paste("Distance:", distance, "km"), 
             size = 3.25, # Uniform font size
             color = "black") +
    # Fix aspect ratio to 1:1
    coord_fixed() +
    # Adjust plot limits to ensure all labels are visible
    expand_limits(x = c(0, 6), y = c(0, 6)) +
    labs(title = "Illustration of FCM Sample & Hunting Event Assignment Process",
         x = "X [km]",
         y = "Y [Km]") +
    theme_minimal() +
    theme(legend.position = "none") # Remove the legend
  
}
