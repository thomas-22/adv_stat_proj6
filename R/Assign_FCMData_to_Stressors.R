########### Assign FCM data ##############

# For each FCM sample, find all hunting events prior to that
# 
# `daydiff.threshold`: specify a preliminary filter for hunting events that we
# consider for each FCM sample. This threshold should be loose. We will further
# apply filters based on distance in time and space.
# By default, the threshold is 100 days.
AssignFCMData <- function(FCMStress, 
                          Movement,
                          HuntEvents,
                          max.datediff = 100,
                          distance.unit = "km") {
  # Find all combinations of deer and hunting events where the hunting event
  # happened within the specified time frame before the FCM sample was taken
  data <- FCMStress %>%
    mutate(
      StressDateEarliest = as_date(DefecTime - days(max.datediff)),
      StressDateLatest = as_date(DefecTime),
    ) %>%
    left_join(
      HuntEvents %>% select(Hunt.ID, HuntDate, HuntTime),
      join_by(StressDateEarliest <= HuntDate, StressDateLatest >= HuntDate)
    ) %>%
    distinct()
  
  # Find all deer & hunting event pairs with full time & space information
  deer_hunting_pairs <- data %>%
    filter(!is.na(HuntTime)) %>%
    distinct(Sender.ID, Hunt.ID) %>%
    na.omit()
  # Approximate distance between deer and hunting event
  distances <- CalcDist(deer_hunting_pairs, Movement, HuntEvents, distance.unit) %>%
    select(Sender.ID, Hunt.ID, starts_with("Distance"))

  # Merge back
  data %>%
    left_join(distances, by = c("Sender.ID", "Hunt.ID")) %>%
    mutate(TimeDiff = as.numeric(difftime(DefecTime, HuntTime, unit = "hours"))) %>%
    filter(TimeDiff > 0 | is.na(TimeDiff)) %>%
    # Get potential confounders
    mutate(
      # time diff between defecation and sampling
      SampleDelay = as.numeric(difftime(SampleTime, DefecTime, unit = "hours")),
      # time of day
      DefecHour = hour(DefecTime),
      # hunting hour
      HuntHour = hour(HuntTime),
      # season
      Season = get_season(DefecTime)
      # TBD more?
    ) %>%
    # Add pregnancy information
    left_join(prep.Pregnancy.data(), by = "Sender.ID") %>%
    mutate(
      Pregnant = (pregnant_since <= as_date(DefecTime)) & (as_date(DefecTime) <= pregnant_until),
      Pregnant = ifelse(is.na(Pregnant), FALSE, Pregnant)
    ) %>%
    # TBD: add herds information?
    # Here are all the columns that are potentially needed for some model.
    # Add more if you have.
    distinct(
      Sample.ID, Sender.ID,
      ng_g,
      DefecTime,
      Hunt.ID, HuntTime, HuntDate,
      Distance, DistanceX, DistanceY,
      TimeDiff, SampleDelay,
      Pregnant, DefecHour, HuntHour, Season
    )
}

########### Filter Assigned Data ##############

# Keep temporally closest hunting event,
# while taking gut retention time into account.
# Count the number of additional hunting events (with or without timestamp) in
# the pervious k days.
# You can add more filtering options, like setting an upper threshold for gut
# retention time or do something about distance.
# The input data needed for this function is produced by `AssignFCMData`.
FilterClosestTime <- function(data_full, gut.retention.hours = 14, k = 5) {
  data_full %>%
    filter(
      TimeDiff >= gut.retention.hours | is.na(TimeDiff),
      date(DefecTime) - HuntDate <= days(k)
    ) %>%
    group_by(Sender.ID, Sample.ID) %>%
    mutate(
      nOtherHuntEvents = sum(!is.na(Hunt.ID)) - 1,
      lognHuntEvents = log(nOtherHuntEvents + 1)
    ) %>%
    filter(HuntTime == max(HuntTime), !is.na(Distance)) %>%
    ungroup()
}

# TBD: FilterClosestDistance -- keep the hunting event that is closest in space?
