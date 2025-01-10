#RUN "CalcSenderPosDist_new.R" FIRST
library(dplyr)

transform_time_diff_lognormal <- function(TimeDiff, meanlog = log(20), sdlog = 0.5) {
  dlnorm(TimeDiff, meanlog = meanlog, sdlog = sdlog)
}

# For each FCM sample, find all hunting events within a certain time frame,
# calculate distance and time diff, and add potential confounders.
Assign_Hunts_to_FCM <- function(FCMStress, HuntEvents,
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
  
  # Find all combinations of deer and hunting events where the hunting event
  # happened within `daydiff_threshold` before the FCM sample was taken
  deer_sample_hunt <- FCMStress %>%
    mutate(
      StressDateEarliest = as_date(DefecTime - days(daydiff_threshold)),
      StressDateLatest = as_date(DefecTime),
    ) %>%
    left_join(
      HuntEvents,
      join_by(StressDateEarliest <= HuntDate, StressDateLatest >= HuntDate)
    )

  deer_hunt_pairs <- deer_sample_hunt %>%
    filter(!is.na(Hunt.ID), !is.na(HuntTime), !is.na(HuntX), !is.na(HuntY)) %>%
    distinct(Sender.ID, Hunt.ID)
  # Get distance between deer and hunting event
  distances <- CalcDist(deer_hunting_pairs, Movement, HuntEvents) %>%
    select(Sender.ID, Hunt.ID, starts_with("Distance"))

  # Merge back
  merged <- deer_sample_hunt %>%
    left_join(distances, by = c("Sender.ID", "Hunt.ID")) %>%
    mutate(TimeDiff = as.numeric(difftime(DefecTime, HuntTime, unit = "hours"))) %>%
    # Get rid of definitely irrelevant hunting events
    filter(TimeDiff > 0 | is.na(TimeDiff)) %>%
    # Count relevant hunting events
    mutate(
      relevant = (
        !is.na(TimeDiff) & TimeDiff >= gut_retention_hours
      ) & (
        !is.na(Distance) & Distance <= distance_threshold
      ),
      relevant_include_missing = (
        (TimeDiff >= gut_retention_hours) | is.na(TimeDiff)
      ) & (
        Distance <= distance_threshold | is.na(Distance)
      )
    ) %>%
    mutate(
      NumRelevantHunts = sum(relevant),
      NumRelevantHunts_include_missing = sum(relevant_include_missing),
      .by = Sample.ID
    )

  if (filter_criterion == "last") {
    data <- merged %>%
      group_by(Sender.ID, Sample.ID) %>%
      filter(TimeDiff == min(TimeDiff, na.rm = TRUE), !is.na(Distance)) %>%
      ungroup()
    return(data)
  }
  if (filter_criterion == "nearest") {
    data <- merged %>%
      group_by(Sender.ID, Sample.ID) %>%
      filter(Distance == min(Distance, na.rm = TRUE), !is.na(TimeDiff)) %>%
      ungroup()
    return(data)
  }
  if (filter_criterion == "score") {
    data <- merged %>%
      group_by(Sender.ID, Sample.ID) %>%
      mutate(Score = (10000000000 / Distance^2) * transform_time_diff_lognormal(TimeDiff)) %>%
      filter(Score == max(Score, na.rm = TRUE)) %>%
      ungroup()
    return(data)
  }

  stop("Invalid filter_criterion")

  # # Get potential confounders
  # mutate(
  #   # time diff between defecation and sampling
  #   SampleDelay = as.numeric(difftime(SampleTime, DefecTime, unit = "hours")),
  #   # time of day
  #   DefecHour = hour(DefecTime),
  #   # hunting hour
  #   HuntHour = hour(HuntTime),
  #   # season
  #   Season = get_season(DefecTime)
  #   # TBD: pregnancy, herds, etc?
  # ) %>%
  # distinct(
  #   Sample.ID, Sender.ID,
  #   ng_g,
  #   DefecTime,
  #   Hunt.ID, HuntTime, HuntDate, HuntHour,
  #   Distance, DistanceX, DistanceY,
  #   TimeDiff, SampleDelay,
  #   Season
  # )
}
