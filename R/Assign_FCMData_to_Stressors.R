#RUN "CalcSenderPosDist_new.R" FIRST
library(dplyr)

transform_time_diff_lognormal <- function(TimeDiff, meanlog = log(20), sdlog = 0.5) {
  dlnorm(TimeDiff, meanlog = meanlog, sdlog = sdlog)
}

# Better timediff eval curve for the scoring function:
timediff_evaluation_asym_curve <- function(x, peak = 19, rise_sigma = 2, fall_sigma = 2.5, height = 1) {
  ifelse(
    x <= peak,
    height * exp(-((x - peak)^2) / (2 * rise_sigma^2)),  # Rising phase (Gaussian)
    height * exp(-(x - peak) / fall_sigma)              # Falling phase (Exponential decay)
  )
  
  # To see how it looks:
  # plot(seq(0, 40, by = 0.1), timediff_evaluation_asym_curve(seq(0, 40, by = 0.1), peak = 19, rise_sigma = 2, fall_sigma = 2.5, height = 1), type = "l", col = "blue", lwd = 2, 
  #      xlab = "Time (hours)", ylab = "ng_g Impact", main = "ng_g Impact ~ TimeDiff Curve")
  # 
  
}

save_jpg_timediff_function <- function(){
  
  #getwd()
  
  output_directory <- "./Figures"
  output_filename <- "Timediff_Function_impact_curve.jpg"
  output_path <- file.path(output_directory, output_filename)
  
  jpeg(
    filename = output_path,    # Full path to save the JPEG
    width = 800,               # Width in pixels
    height = 600,              # Height in pixels
    units = "px",              # Units for width and height
    quality = 100               # Quality of the JPEG (1-100)
  )
  
  plot(seq(0, 40, by = 0.1), timediff_evaluation_asym_curve(seq(0, 40, by = 0.1), peak = 19, rise_sigma = 2, fall_sigma = 2.5, height = 1), type = "l", col = "blue", lwd = 2,
       xlab = "Time (hours)", ylab = "Score Impact", main = "Score Impact ~ TimeDiff Curve")
  
  
  dev.off()
  
  
}
save_jpg_timediff_function()

# For each FCM sample, find all hunting events within a certain time frame,
# calculate distance and time diff, and add potential confounders.
assign_hunts_to_fcm <- function(FCMStress, HuntEvents, Movement,
    ignore_distance_filter = FALSE,
    distance_threshold = 10, # km
    gut_retention_time_lower = 19, # hours
    gut_retention_time_upper = 50, # hours
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
  # was within a meaningful time frame.
  deer_sample_hunt <- FCMStress %>%
    mutate(
      StressTimeEarliest = DefecTime - hours(gut_retention_time_upper),
      StressTimeLatest = DefecTime - hours(gut_retention_time_lower),
    ) %>%
    left_join(
      HuntEvents,
      join_by(StressTimeEarliest <= HuntTime, StressTimeLatest >= HuntTime)
    ) %>%
    dplyr::select(-StressTimeLatest, -StressTimeEarliest) %>%
    distinct()

  # Get combinations of deer and hunting events
  deer_hunt_pairs <- deer_sample_hunt %>%
    filter(!is.na(Hunt.ID)) %>%
    distinct(Sender.ID, Hunt.ID)

  # Get distances
  distances <- CalcDist(deer_hunt_pairs, Movement, HuntEvents) %>%
    dplyr::select(Sender.ID, Hunt.ID, starts_with("Distance")) %>%
    na.omit()

  # Merge back
  deer_sample_hunt_distance <- deer_sample_hunt %>%
    left_join(distances, by = c("Sender.ID", "Hunt.ID"))

  # Calculate time difference
  deer_sample_hunt_distance_timediff <- deer_sample_hunt_distance %>%
    mutate(
      TimeDiff = as.numeric(difftime(DefecTime, HuntTime, unit = "hours")),
      TimeDiffStress = TimeDiff - gut_retention_time_lower
    )

  # Remove outliers by default (ignore_distance_filter = FALSE)
  if (ignore_distance_filter == TRUE || filter_criterion == "score") {
    interesting_data <- deer_sample_hunt_distance_timediff %>%
      filter(!is.na(Distance))
  } else {
    interesting_data <- deer_sample_hunt_distance_timediff %>%
      filter(!is.na(Distance), Distance <= distance_threshold)
  }

  interesting_data <- interesting_data %>% group_by(Sender.ID, Sample.ID) %>%
    mutate(
      # Count the number of other hunting events.
      # This could be an indicator of intensity of hunting and hence a confounder.
      NumOtherHunts = sum(!is.na(Hunt.ID)) - 1
    )

  # Filter by criterion
  data <- if (filter_criterion == "last") {
    interesting_data %>%
      group_by(Sender.ID, Sample.ID) %>%
      filter(abs(TimeDiff - 19) == min(abs(TimeDiff - 19), na.rm = TRUE), !is.na(Distance)) %>%
      #Selects assignment that is closest to 19 hrs
      ungroup()
  } else if (filter_criterion == "nearest") {
    interesting_data %>%
      group_by(Sender.ID, Sample.ID) %>%
      filter(Distance == min(Distance, na.rm = TRUE), !is.na(TimeDiff)) %>%
      ungroup()
  } else if (filter_criterion == "score") {
    interesting_data %>%
      group_by(Sender.ID, Sample.ID) %>%
      mutate(Score = (10000000000 / Distance^2) * timediff_evaluation_asym_curve(TimeDiff)) %>%
      filter(Score == max(Score, na.rm = TRUE) & Score > 250) %>% # Score > 250: Minimum score to be considered.
      ungroup()
  } else {
    stop("Invalid filter_criterion")
  }

  # Add potential confounders
  data %>%
    mutate(
      # time diff between defecation and sampling
      SampleDelay = as.numeric(difftime(SampleTime, DefecTime, unit = "hours")),
      # others (not necessarily used)
      DefecDay = as.numeric(floor_date(DefecTime, "day") - floor_date(DefecTime, "year")),
      DefecHour = hour(DefecTime),
      HuntHour = hour(HuntTime),
      DefecMonth = month(DefecTime)
      # Pregnancy status is already in FCMStress
    )
}
