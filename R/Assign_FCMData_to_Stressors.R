library(dplyr)


#RUN "CalcSenderPosDist_new.R" FIRST


transform_time_diff_lognormal <- function(TimeDiff, meanlog = log(20), sdlog = 0.5) {
  dlnorm(TimeDiff, meanlog = meanlog, sdlog = sdlog)
}

ignore_distance_filter <- FALSE

distance_threshold <- 100000 #in Meters

gut_retention_time_lower <- 0 #in Hours
gut_retention_time_upper <- 1000000 #in Hours
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
                      #StressorID = integer(),
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
  
  matching_events <- StressEvents_new %>%
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
                              #StressorID = event_row$StressorID,
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

interesting_data$TimeDiff <- as.numeric(interesting_data$TimeDiff)


data_cleanedup <- interesting_data
data_cleanedup$TimeDiff <- as.numeric(data_cleanedup$TimeDiff)

#Get rid of duplicate entries and choose which one to keep:
# Define a scoring function:
data_cleanedup$Score <- (10000000000/data_cleanedup$Distance^2) *
  transform_time_diff_lognormal(data_cleanedup$TimeDiff, meanlog = log(32), sdlog = 0.7)

data_cleanedup <- data_cleanedup %>%
  group_by(Sample_ID) %>%
  mutate(
    RowsDiscarded = n() - 1  # Total rows for each Sample_ID minus 1 (kept row)
  ) %>%
  slice_max(Score, n = 1) %>%
  ungroup()



data_cleanedup_min_distance <- interesting_data %>%
  group_by(Sample_ID) %>%
  mutate(
    RowsDiscarded = n() - 1  # Total rows for each Sample_ID minus 1 (kept row)
  ) %>%
  slice_min(Distance, n = 1) %>%
  ungroup()

data_cleanedup_min_timediff <- interesting_data %>%
  group_by(Sample_ID) %>%
  mutate(
    RowsDiscarded = n() - 1  # Total rows for each Sample_ID minus 1 (kept row)
  ) %>%
  slice_min(TimeDiff, n = 1) %>%
  ungroup()

#Add both together:
combo_min_dist_timediff <- rbind(data_cleanedup_min_distance,
                                 data_cleanedup_min_timediff)

combo_min_dist_timediff_no_dupes <- combo_min_dist_timediff[!duplicated(combo_min_dist_timediff$Sample_ID), ]


#Documentation of Dataset Variants:

# 1. FCMData_Assigned: Contains all possible stressor-event assignments for each FCM sample, 
#    based on gut retention time and sender match criteria.

# 2. interesting_data: Subset of FCMData_Assigned filtered by a specified maximum distance threshold
#    if 'ignore_distance_filter' is set to FALSE. This removes nonsense assignments due to faulty data.
#   USE INSTEAD OF FCMData_Assigned

# 3. data_cleanedup: Deduplicated version of 'interesting_data', where the best stressor event for each 
#    sample is selected based on a scoring function combining distance and time difference.

# 4. data_cleanedup_min_distance: Deduplicated dataset retaining the stressor event with the minimum distance 
#    for each sample.

# 5. data_cleanedup_min_timediff: Deduplicated dataset retaining the stressor event with the minimum time 
#    difference for each sample.

# 6. combo_min_dist_timediff: Combined dataset containing rows from both 'data_cleanedup_min_distance' and 
#    'data_cleanedup_min_timediff', including duplicates.

# 7. combo_min_dist_timediff_no_dupes: Final combined dataset where duplicate entries based on 'Sample_ID' 
#    are removed, retaining only one unique assignment per sample.

#FOR MODELLING USE: 3-7

