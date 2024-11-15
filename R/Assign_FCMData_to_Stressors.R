library(dplyr)
library(ggplot2)

#source("./R/CalcSenderPosDist.R")

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
    filter(Sender.ID == sender_id &
             difftime(collar_time, HuntEventTime, units = "hours") >= 16 &
             difftime(collar_time, HuntEventTime, units = "hours") <= 21)
  
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

Samples_Relevant <- FCMData_Assigned %>%
  group_by(Sample_ID) %>%
  summarise(
    ng_g = mean(ng_g, na.rm = TRUE),
    distance = min(Distance, na.rm = TRUE)
  )

# Samples_Relevant <- Samples_Relevant %>%
#    filter(distance < 2000)



