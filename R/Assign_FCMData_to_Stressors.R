library(dplyr)
library(ggplot2)
#install.packages("plotly")
library(plotly)
library(MASS)  # For fitting distributions
#install.packages("ggtext")
library(ggtext)

#source("./R/CalcSenderPosDist.R")
#source("./R/CalcAvgDistance_ForMissingTimes.R")


transform_time_diff_lognormal <- function(TimeDiff, meanlog = log(20), sdlog = 0.5) {
  dlnorm(TimeDiff, meanlog = meanlog, sdlog = sdlog)
}

ignore_distance_filter <- FALSE

distance_threshold <- 10000 #in Meters

gut_retention_time_lower <- 0 #in Hours
gut_retention_time_upper <- 1000 #in Hours
gut_retention_mean <- (gut_retention_time_lower + gut_retention_time_upper) / 2

# Create an empty data frame to store results
FCMData_Assigned <- data.frame(Sender.ID = integer(),
                      Sample_ID = character(),
                      HairID = character(),
                      Defec_Time = as.POSIXct(character()),
                      Stress_Time = as.POSIXct(character()),
                      Speed = numeric(),
                      Distance = numeric(),
                      ng_g = numeric(),
                      StressorID = integer(),
                      stringsAsFactors = FALSE)
cat("\n")

# Iterate through each row in FCMStress
for (i in 1:nrow(FCMStress)) {
  cat(sprintf("FCM Assignment: %d of %d", i, nrow(FCMStress)), "\r")
  
  sample_row <- FCMStress[i, ]
  sample_id <- sample_row$Sample_ID
  sender_id <- sample_row$Sender.ID
  collar_time <- sample_row$Collar_t_
  x_fcm <- sample_row$X
  y_fcm <- sample_row$Y
  hair_id <- sample_row$HairID
  ng_g <- sample_row$ng_g
  
  matching_events <- StressEvents %>%
    mutate(Sender.ID = as.character(Sender.ID)) %>%
    filter(
      Sender.ID == as.character(sender_id) & 
        difftime(collar_time, HuntEventTime, units = "hours") >= gut_retention_time_lower &
        difftime(collar_time, HuntEventTime, units = "hours") <= gut_retention_time_upper
    )
  
  # If there are matching events, create entries in the FCMData_Assigned data frame
  if (nrow(matching_events) > 0) {
    for (j in 1:nrow(matching_events)) {
      event_row <- matching_events[j, ]
      new_entry <- data.frame(Sender.ID = sender_id,
                              Sample_ID = sample_id,
                              HairID = hair_id,
                              Defec_Time = collar_time,
                              Stress_Time = event_row$HuntEventTime,
                              TimeDiff = difftime(collar_time, event_row$HuntEventTime, units = "hours"),
                              Distance = event_row$Distance,
                              ng_g = ng_g,
                              StressorID = event_row$StressorID,
                              stringsAsFactors = FALSE)
      
      FCMData_Assigned <- rbind(FCMData_Assigned, new_entry)
    }
  }
}

#fcms_assigned_to_huntevents_notime <- fcms_assigned_to_huntevents_notime[, names(FCMData_Assigned)]

#test <- rbind(FCMData_Assigned, fcms_assigned_to_huntevents_notime)

if (ignore_distance_filter == FALSE) {
  interesting_data <- FCMData_Assigned %>%
    filter(Distance <= distance_threshold)
} else {
  interesting_data <- FCMData_Assigned
}

unique_sample_ids <- interesting_data %>%
  distinct(Sample_ID)

#Get rid of duplicate entries and choose which one to keep:
data_cleanedup <- interesting_data
data_cleanedup$TimeDiff <- as.numeric(data_cleanedup$TimeDiff)


# Define a scoring function:
data_cleanedup$Score <- (10000000000/data_cleanedup$Distance^2) * transform_time_diff_lognormal(data_cleanedup$TimeDiff, meanlog = log(20), sdlog = 0.7)

data_cleanedup <- data_cleanedup %>%
  group_by(Sample_ID) %>%
  mutate(
    RowsDiscarded = n() - 1  # Total rows for each Sample_ID minus 1 (kept row)
  ) %>%
  slice_max(Score, n = 1) %>%
  ungroup()

# cor(data_cleanedup$RowsDiscarded, data_cleanedup$ng_g)


fcm_specific <- data.frame()

for (i in 1:nrow(unique_sample_ids)) {
  current_id <- unique_sample_ids$Sample_ID[i]
  rows_with_id <- interesting_data %>%
    filter(Sample_ID == current_id)
  new_entry <- rows_with_id %>%
    filter(Distance == min(Distance)) #Use the Stressor Event that was closest
  new_entry <- new_entry %>%
    arrange(TimeDiff) %>%
    slice_head(n = 1)
  fcm_specific <- rbind(fcm_specific, new_entry)
}

usiv <- unique_sample_ids$Sample_ID

fcm_reference <- FCMStress %>%
  filter(!(Sample_ID %in% usiv))

t_test_fcm <- t.test(fcm_specific$ng_g, fcm_reference$ng_g)

print(t_test_fcm)


specific_mean <- mean(fcm_specific$ng_g, na.rm = TRUE)
specific_n <- sum(!is.na(fcm_specific$ng_g))

reference_mean <- mean(fcm_reference$ng_g, na.rm = TRUE)
reference_n <- sum(!is.na(fcm_reference$ng_g))

lm1 <- lm(fcm_specific$ng_g ~ fcm_specific$Distance)
lm1$coefficients[2]

ggplot(fcm_specific, aes(x = Distance, y = ng_g)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) + 
  geom_hline(yintercept = reference_mean, color = "red", linetype = "dashed") +
  geom_hline(yintercept = specific_mean, color = "blue", linetype = "dashed") +
  labs(
    title = "Plot of ng_g vs Distance (with reference mean)",
    x = "Distance",
    y = "ng_g"
  ) +
  # Add annotations for fcm_specific
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("fcm_specific (blue):",
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
                         "\nDistance Threshhold (Meters):", distance_threshold, 
                         "\nT_Test:", round(t_test_fcm$statistic, 2),
                         "\nT_Test p_val:", round(t_test_fcm$p.value, 2)),
           color = "green", size = 3.5) +
  theme_minimal() +
  theme(plot.margin = margin(5, 50, 5, 5))

#Some magic
fcm_specific$Impact_Factor <- fcm_specific$StressorID

for (i in 1:nrow(fcm_specific)) {
  timediff_accepted <- 0
  
  if (as.numeric(fcm_specific[i,]$TimeDiff) > gut_retention_time_lower &&
      as.numeric(fcm_specific[i,]$TimeDiff) < gut_retention_time_upper) {
    timediff_accepted <- 1
  }
  
  fcm_specific[i,]$Impact_Factor <- ((1/fcm_specific[i,]$Distance^2)*timediff_accepted)*10000000
}

fcm_specific_impacted <- subset(fcm_specific, Impact_Factor > 0.1)

lm1 <- lm(ng_g ~ Impact_Factor, data=fcm_specific_impacted)
summary(lm1)

ggplot(fcm_specific_impacted, aes(x = Impact_Factor, y = ng_g)) +
  geom_point(color = "blue") +
  scale_x_log10() +
  geom_smooth( color = "blue", linewidth = 0.5, se = FALSE) + 
  geom_hline(yintercept = reference_mean, color = "red", linetype = "dashed") +
  geom_hline(yintercept = specific_mean, color = "blue", linetype = "dashed") +
  labs(
    title = "Plot of ng_g vs\nImpact Factor(1/distance^2, TimeDiff within Gut Retention Boundaries)",
    x = "Impact Factor",
    y = "ng_g"
  ) +
  theme_minimal() +
  theme(plot.margin = margin(5, 50, 5, 5))

##3D Plot:
plot_ly(fcm_specific, x = ~Distance, y = ~TimeDiff, z = ~ng_g,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = 'blue',
                      line = list(color = 'black', width = 2))) %>%
  layout(title = "3D Plot with Black Outline",
         scene = list(
           xaxis = list(title = 'Distance'),
           yaxis = list(title = 'TimeDiff'),
           zaxis = list(title = 'ng_g')
         ))

hist(as.numeric(fcm_specific$TimeDiff), breaks = 120)

###############################################################
#Discussion about Log-Normal vs Gamme Fit for $ng_g
###############################################################
data1 <- FCMStress %>%
  filter(ng_g > 0)

data <- data1$ng_g

### Fit Log-Normal Distribution ###
lognorm_fit <- fitdistr(data, "lognormal")
meanlog <- lognorm_fit$estimate["meanlog"]
sdlog <- lognorm_fit$estimate["sdlog"]

### Fit Gamma Distribution ###
gamma_fit <- fitdistr(data, "gamma")
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

ggplot(FCMStress, aes(x = Collar_t_)) +
  geom_histogram(binwidth = 604800, fill = "lightblue", color = "black") + # Binwidth = 1 Week
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





