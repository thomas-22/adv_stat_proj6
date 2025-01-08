#RUN "CalcSenderPosDist_new.R" FIRST
library(dplyr)

transform_time_diff_lognormal <- function(TimeDiff, meanlog = log(20), sdlog = 0.5) {
  dlnorm(TimeDiff, meanlog = meanlog, sdlog = sdlog)
}


Assign_FCMData_to_Hunts <- function()
{
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

  FCMStress_joined <- FCMStress %>%
    mutate(
      Sender.ID = as.character(Sender.ID),
      collar_time = Collar_t_  # rename for clarity
    )
  
  StressEvents_joined <- StressEvents_new %>%
    mutate(Sender.ID = as.character(Sender.ID))
  
  FCMData_Assigned <- FCMStress_joined %>%
    inner_join(
      StressEvents_joined,
      by = "Sender.ID",
      relationship = "many-to-many"
    ) %>%
    mutate(
      TimeDiff = as.numeric(difftime(collar_time, HuntEventTime, units = "hours"))
    ) %>%
    filter(
      TimeDiff >= gut_retention_time_lower,
      TimeDiff <= gut_retention_time_upper
    ) %>%
    transmute(
      Sender.ID = Sender.ID,
      Sample_ID = Sample_ID,
      HairID = HairID,
      Defec_Time = collar_time,
      Stress_Time = HuntEventTime,
      TimeDiff = TimeDiff,
      Distance = Distance,
      ng_g = ng_g
    )
  
  # Filtering by distance threshold
  if (ignore_distance_filter == FALSE) {
    interesting_data <- FCMData_Assigned %>%
      filter(Distance <= distance_threshold)
  } else {
    interesting_data <- FCMData_Assigned
  }
  
  interesting_data$TimeDiff <- as.numeric(interesting_data$TimeDiff)
  
  data_cleanedup <- interesting_data
  data_cleanedup$TimeDiff <- as.numeric(data_cleanedup$TimeDiff)
  
  # Get rid of duplicate entries and choose which one to keep:
  # Define a scoring function:
  data_cleanedup$Score <- (10000000000 / data_cleanedup$Distance^2) *
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
  
  # Add both together:
  combo_min_dist_timediff <- rbind(data_cleanedup_min_distance,
                                   data_cleanedup_min_timediff)
  combo_min_dist_timediff_no_dupes <- combo_min_dist_timediff[!duplicated(combo_min_dist_timediff$Sample_ID), ]
  
  return(list(
    interesting_data = interesting_data,
    data_cleanedup = data_cleanedup,
    data_cleanedup_min_distance = data_cleanedup_min_distance,
    data_cleanedup_min_timediff = data_cleanedup_min_timediff,
    combo_min_dist_timediff = combo_min_dist_timediff,
    combo_min_dist_timediff_no_dupes = combo_min_dist_timediff_no_dupes
  ))
}

# Documentation of Dataset Variants:

# 1. FCMData_Assigned: Contains all possible stressor-event assignments for each FCM sample, 
#    based on gut retention time and sender match criteria.

# 2. interesting_data: Subset of FCMData_Assigned filtered by a specified maximum distance threshold
#    if 'ignore_distance_filter' is set to FALSE. This removes nonsense assignments due to faulty data.
#    USE INSTEAD OF FCMData_Assigned

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

# FOR MODELLING USE: 3-7
