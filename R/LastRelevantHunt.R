source("R/Datafusion.R")
source("R/CalcSenderPosDist.R")

# Find last relevant hunting event for each FCM sample
#
# Parameters: TBD.
#
# returns: tibble with the same columns plus the following columns:
# - "LRHE": indicator if a last relevant hunting event (LRHE) exists.
# - "Timediff": time since LRHE, if LRH exists and has timestamp; 0 otherwise.
# - "Distance": distance to LRHE, if LRHE exists and has timestamp; 0 otherwise.
# - "AvgDistance": average distance to LRHE on the day of LRHE,
#   if LRHE exists but timestamp is missing; 0 otherwise.
# - "numRHE": number of relevant hunting events.
# - "numHE15": number of hunting events within last 15 days.
get_LRHE_data <- function(folder = "Data",
                          relevant_delay = c(16, 30),
                          relevant_distance = 3,
                          max_distance = 5) {
  FCMStress <- read_fcm(folder)
  HuntEvents <- read_hunting(folder, FCMStress)
  HuntEvents_with_time <- HuntEvents %>% filter(!TimeMissing)
  HuntEvents_no_time <- HuntEvents %>% filter(TimeMissing)
  # Movement <- read_movement(folder, FCMStress)
  
  data_nHunts_10days <- FCMStress %>%
    mutate(DefecTime_minus_15days = DefecTime - days(10)) %>%
    left_join(HuntEvents, join_by(DefecTime_minus_15days >= Date)) %>%
    # count number of hunting events within last 10 days
    mutate(HuntDate = Date, HuntX = X, HuntY = Y, HuntTime = t_,
           HuntTimeMissing = TimeMissing) %>%
    group_by(Sender.ID, Sample.ID) %>%
    summarise(nHunts_10days = sum(!is.na(Date)), .groups = "keep")
  
  # find relevant hunting events within the relevant time frame
  data_rhe_with_time <- FCMStress %>%
    mutate(StressTimeEarliest = DefecTime - hours(max(relevant_delay)),
           StressTimeLatest = DefecTime - hours(min(relevant_delay))) %>%
    left_join(HuntEvents_with_time,
              join_by(StressTimeEarliest <= t_, StressTimeLatest >= t_)) %>%
    rename(HuntX = X, HuntY = Y, HuntTime = t_,
           HuntDate = Date, HuntTimeMissing = TimeMissing)
    
  data_rhe_no_time <- FCMStress %>%
    mutate(StressDateEarliest = date(DefecTime - days(1)),
           StressDateLatest = date(DefecTime - days(1))) %>%
    left_join(HuntEvents_no_time,
              join_by(StressDateEarliest <= Date, StressDateLatest >= Date)) %>%
    rename(HuntX = X, HuntY = Y, HuntTime = t_,
           HuntDate = Date, HuntTimeMissing = TimeMissing)
  
  data_rhe_all <- full_join(
    data_rhe_with_time %>% select(-StressTimeEarliest, -StressTimeLatest),
    data_rhe_no_time %>% select(-StressDateEarliest, -StressDateLatest),
  ) %>%
    arrange(DefecTime)
  
  # get the combinations of deer and hunting events, for which we compute distances
  data_to_locate <- data_rhe_all %>%
    select(Sender.ID, Hunt.ID) %>%
    filter(!is.na(Hunt.ID)) %>%
    distinct()
  
  data_distances <- get_distances(data_to_locate)
  
  StressEvents <- data_rhe_all %>%
    left_join(data_distances,
              join_by(Sender.ID, Hunt.ID, HuntDate, HuntTime, HuntTimeMissing))
  
  StressEvents
}
