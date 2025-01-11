#RUN "CalcSenderPosDist_new.R" FIRST
library(dplyr)

transform_time_diff_lognormal <- function(TimeDiff, meanlog = log(20), sdlog = 0.5) {
  dlnorm(TimeDiff, meanlog = meanlog, sdlog = sdlog)
}

# For each FCM sample, find all hunting events within a certain time frame,
# calculate distance and time diff, and add potential confounders.
assign_hunts_to_fcm <- function(FCMStress, HuntEvents, Movement,
    ignore_distance_filter=FALSE,
    daydiff_threshold = 30, # days
    distance_threshold = 10, # km
    gut_retention_hours = 14, # hours
    # gut_retention_upper = Inf, # hours
    filter_criterion = "last" # possible alternatives: "nearest", "score"
  ) {
  # Algorithm:
  # 1. For each FCM sample, find all hunting events within `daydiff_threshold` days
  # prior to the defecation time.
  # These are *potentially relevant* hunting events.
  # 2. Calculate the distances between the deer and the hunting events.
  # Now we have a many-to-many relationship between FCM samples and hunting events.
  # 3. Find the "last hunting event" for each FCM sample.
  # This hunting event should be temporally as close as possible to the sample,
  # but not closer than `gut_retention_hours`.
  
  #-----------------------------------------------------------------------------
  # !!!!!!! Is any other filtering method justifiable?
  # The main goal of our project is to analyze the effect of the last hunting
  # event.
  # Can you justify defining the last hunting event to be the spatially closest one?
  #-----------------------------------------------------------------------------

  # If a lot of hunting events happened in a short time period (say, a week) before
  # defecation, the effects, if exist, might accumulate.
  # Thus, we use the number of *relevant* hunting events as a covariate.
  #
  # 4. Define relevant hunting events.
  # Version 1: All hunting events within `daydiff_threshold` are relevant.
  # Version 2: All hunting events wihtin `daydiff_threshold` and `distance_threshold`
  # are relevant.
  # Version 3: All hunting events with a score above some threshold are relevant?
  
  # Find all combinations of samples and hunting events, where the hunting time
  # was within `daydiff_threshold` days before the defecation time.
  deer_sample_hunt <- FCMStress %>%
    mutate(
      StressDateEarliest = as_date(DefecTime - days(daydiff_threshold)),
      StressDateLatest = as_date(DefecTime),
    ) %>%
    left_join(
      HuntEvents,
      join_by(StressDateEarliest <= HuntDate, StressDateLatest >= HuntDate)
    ) %>%
    select(-StressDateLatest, -StressDateEarliest) %>%
    distinct()
  # View(deer_sample_hunt)

  # Get combinations of deer and hunting events
  deer_hunt_pairs <- deer_sample_hunt %>%
    filter(!is.na(Hunt.ID)) %>%
    distinct(Sender.ID, Hunt.ID)

  # Get distances
  distances <- CalcDist(deer_hunt_pairs, Movement, HuntEvents) %>%
    select(Sender.ID, Hunt.ID, starts_with("Distance")) %>%
    na.omit()
  # View(distances)

  # Merge back
  deer_sample_hunt_distance <- deer_sample_hunt %>%
    left_join(distances, by = c("Sender.ID", "Hunt.ID"))
  # View(deer_sample_hunt_distance)

  # Calculate time difference
  deer_sample_hunt_distance_timediff <- deer_sample_hunt_distance %>%
    mutate(
      TimeDiff = as.numeric(difftime(DefecTime, HuntTime, unit = "hours")),
      TimeDiffStress = TimeDiff - gut_retention_hours
    ) %>%
    # Keep temporally relevant hunting events
    filter(TimeDiffStress > 0 | is.na(TimeDiff))
  # View(deer_sample_hunt_distance_timediff)

  # Remove outliers by default
  if (ignore_distance_filter) {
    data_cleanedup <- deer_sample_hunt_distance_timediff
  } else {
    data_cleanedup <- deer_sample_hunt_distance_timediff %>%
      filter(!is.na(Distance), Distance <= distance_threshold)
  }

  data_cleanedup <- data_cleanedup %>% group_by(Sender.ID, Sample.ID) %>%
    mutate(
      # Count the number of other hunting events (with or without timestamp) in
      # the pervious k days (including the day of defecation).
      # This could be an indicator of intensity of hunting and hence a confounder.
      NumOtherHunts = sum(!is.na(Hunt.ID)) - 1
    )

  # Filter by criterion
  data <- if (filter_criterion == "last") {
    data_cleanedup %>%
      filter(TimeDiff == min(TimeDiff, na.rm = TRUE), !is.na(Distance)) %>%
      ungroup()
  } else if (filter_criterion == "nearest") {
    data_cleanedup %>%
      group_by(Sender.ID, Sample.ID) %>%
      filter(Distance == min(Distance, na.rm = TRUE), !is.na(TimeDiff)) %>%
      ungroup()
  } else if (filter_criterion == "score") {
    data_cleanedup %>%
      group_by(Sender.ID, Sample.ID) %>%
      mutate(Score = (10000000000 / Distance^2) * transform_time_diff_lognormal(TimeDiff)) %>%
      filter(Score == max(Score, na.rm = TRUE)) %>%
      ungroup()
  } else {
    stop("Invalid filter_criterion")
  }

  # Add potential confounders
  data %>%
    mutate(
      # time diff between defecation and sampling
      SampleDelay = as.numeric(difftime(SampleTime, DefecTime, unit = "hours")),
      # time of day
      DefecHour = hour(DefecTime),
      # hunting hour
      HuntHour = hour(HuntTime),
      # month
      DefecMonth = month(DefecTime),
      # season
      Season = get_season(DefecTime)
      # Pregnancy status is already in FCMStress
    )
}
