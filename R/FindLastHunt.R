# source("R/Datafusion.R")

# Find the last hunting event for each FCM sample
#
# Returned tibble has the following columns:
# - ng_g, Sample_ID, Sender.ID,
# - SampleTime: Waypoint_time, i.e. when the sample was taken by the researcher
# - DefecTime: Collar_time, i.e. when the sample was produced by the deer
# - StressTime: DefecTime - 19 hours
# - LastHuntTime: time point of the hunting event that is prior to StressTime and closest to StressTime
# - LastHuntRepeat: How many hunting events are recorded at LastHuntTime?
# - HuntEventsBetween: How many hunting events have happened between previous StressTime and LastHuntTime?
#     If it has been 30 days or longer since the previous StressTime, only count the events in the last 30 days.
find_last_hunt <- function(delay_hours = 19, save = FALSE) {
  requireNamespace("lubridate")

  delay_time <- hms(paste0(delay_hours, ":00:00"))

  HuntEvents_tmp <- HuntEvents %>% group_by(t_) %>% count()
  data <- FCMStress %>%
    select(-HairID) %>%
    rename(SampleTime = Waypoint_t_, DefecTime = Collar_t_) %>%
    arrange(SampleTime) %>%
    mutate(StressTime = DefecTime - delay_time) %>%
    mutate(StressTime_lag1 = lag(StressTime), StressTime_minus_1month = StressTime %m+% days(-30)) %>%
    # find all hunting events up to stress time (defecation time - 19 hours)
    # left_join(HuntEvents_tmp, by = join_by(StressTime >= t_)) %>%
    # rename(HuntingTime = t_) %>%
    # find last hunting event
    left_join(HuntEvents_tmp, by = join_by(closest(StressTime >= t_))) %>%
    rename(LastHuntTime = t_, LastHuntRepeat = n) %>%
    # find hunting events between two FCM samples
    left_join(HuntEvents_tmp, by = join_by(LastHuntTime > t_, StressTime_lag1 <= t_, StressTime_minus_1month <= t_)) %>%
    rename(HuntEventTime = t_) %>%
    summarise(
      HuntEventsBetween = sum(n),
      .by = c(ng_g, Sample_ID, Sender.ID, SampleTime, DefecTime, StressTime, LastHuntTime, LastHuntRepeat)
    ) %>%
    mutate(HuntEventsBetween = ifelse(is.na(HuntEventsBetween), 0, HuntEventsBetween))

  if (save) {
    saveRDS(data, "Data/FCMStress_last_hunt.Rds")
  }

  return(data)
}

# find_last_hunt() %>% View()
