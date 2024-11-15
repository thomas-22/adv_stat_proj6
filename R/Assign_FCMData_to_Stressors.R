library(dplyr)
library(ggplot2)

#source("./R/CalcSenderPosDist.R")

# Create an empty data frame to store results
FCMData_Assigned <- data.frame(Sender.ID = integer(),
                      Sample_ID = character(),
                      HairID = character(),
                      Defec_Time = as.POSIXct(character()),
                      Stress_Time = as.POSIXct(character()),
                      X_fcm = numeric(),
                      Y_fcm = numeric(),
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
    filter(Sender.ID == sender_id &
             difftime(HuntEventTime, collar_time, units = "hours") >= 16 &
             difftime(HuntEventTime, collar_time, units = "hours") <= 21)
  
  # If there are matching events, create entries in the FCMData_Assigned data frame
  if (nrow(matching_events) > 0) {
    for (j in 1:nrow(matching_events)) {
      event_row <- matching_events[j, ]
      new_entry <- data.frame(Sender.ID = sender_id,
                              Sample_ID = sample_id,
                              HairID = hair_id,
                              Defec_Time = collar_time,
                              Stress_Time = event_row$HuntEventTime,
                              X_fcm = x_fcm,
                              Y_fcm = y_fcm,
                              Speed = event_row$Speed,
                              Distance = event_row$Distance,
                              ng_g = ng_g,
                              StressorID = event_row$StressorID,
                              stringsAsFactors = FALSE)
      
      # Append the new entry to the FCMData_Assigned data frame
      FCMData_Assigned <- rbind(FCMData_Assigned, new_entry)
    }
  }
}

Data_Clean <- FCMData_Assigned %>%
  group_by(Sample_ID) %>%
  summarise(
    ng_g = mean(ng_g, na.rm = TRUE),
    distance = min(Distance, na.rm = TRUE)  # Assuming you also want the average distance per Sample_ID
  )

# Data_Clean <- Data_Clean %>%
#   filter(distance < 5000)


lm_model <- lm(ng_g ~ distance, data = Data_Clean)

# Step 3: Plot the data with the linear model fit
ggplot(Data_Clean, aes(x = distance, y = ng_g)) +
  geom_point() +                                  # Scatter plot of the averages
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add linear model fit line with confidence interval
  labs(title = "Average Distance vs. Average ng_g with Linear Model Fit",
       x = "Distance",
       y = "ng_g") +
  theme_minimal()

summary(lm_model)

