########### Assign FCM data ##############
AssignFCMData <- function(FCMStress,
                          Movement,
                          HuntEvents,
                          max.datediff = 100,
                          distance.unit = "km") {
  # For each FCM sample, find all hunting events within a certain time frame,
  # calculate distance and time diff, and add potential confounders.
  # 
  # `daydiff.threshold`: specify a preliminary filter for hunting events that we
  # consider for each FCM sample. This threshold should be loose. We will further
  # apply filters based on distance in time and space.
  # By default, the threshold is 100 days.
  # Supported distance units are "km" and "m".


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
  
  # Get deer & hunting event pairs with full time & space information
  deer_hunting_pairs <- data %>%
    filter(!is.na(HuntTime)) %>%
    distinct(Sender.ID, Hunt.ID) %>%
    na.omit()
  
  # Get distance between deer and hunting event
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

FilterClosestTime <- function(data_full, gut.retention.hours = 14, k = 5) {
  # Keep temporally closest hunting event, accounting for gut retention time.
  # The input `data_full` needed for this function is produced by `AssignFCMData`.

  data_full %>%
    filter(
      TimeDiff >= gut.retention.hours | is.na(TimeDiff),
      date(DefecTime) - HuntDate <= days(k)
    ) %>%
    group_by(Sender.ID, Sample.ID) %>%
    mutate(
      # Count the number of other hunting events (with or without timestamp) in
      # the pervious k days (including the day of defecation).
      # This could be an indicator of intensity of hunting and hence a confounder.
      nOtherHuntEvents = sum(!is.na(Hunt.ID)) - 1,
      # Log of number of hunting events in the previous k days, including the
      # last hunting event, whose time diff and distance will enter the model as
      # main covariates. This is to avoid log(0).
      # Whether to use the log version or the raw version is your choice.
      lognHuntEvents = log(nOtherHuntEvents + 1)
    ) %>%
    # Only keep the last hunting event with time and distance information
    filter(HuntTime == max(HuntTime, na.rm = TRUE), !is.na(Distance)) %>%
    ungroup()
}

# TBD: FilterClosestDistance -- keep the hunting event that is closest in space?
