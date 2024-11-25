#getwd()
# source("./R/Datafusion.R")


# Parameters:
# - `data_to_locate`: tibble with columns "Sender.ID", "Hunt.ID"
get_distances <- function(data_to_locate, folder = "Data") {
  # library(data.table)
  library(sf)
  
  Movement <- read_movement(folder)
  HuntEvents <- read_hunting(folder)
  
  # get hunt event details
  data_to_locate <- data_to_locate %>%
    left_join(HuntEvents, by = "Hunt.ID") %>%
    rename(HuntX = X, HuntY = Y,
           HuntDate = Date, HuntTime = t_, HuntTimeMissing = TimeMissing)
  
  # If timestamp exsits, use linear interpolation to find the location of the
  # deer at the time of hunting
  data_with_time <- data_to_locate %>%
    filter(!is.na(HuntTime)) %>%
    distinct() %>%  # just in case of duplicates
    # find deer location just before hunting event
    left_join(Movement, join_by(Sender.ID, closest(HuntTime >= t_))) %>%
    rename(x_before = x_, y_before = y_, t_before = t_) %>%
    # find deer location just after hunting event
    left_join(Movement, join_by(Sender.ID, closest(HuntTime <= t_))) %>%
    rename(x_after = x_, y_after = y_, t_after = t_) %>%
    # interpolate
    mutate(
      time_total = as.numeric(difftime(t_after, t_before), units = "secs"),
      time_traversed = as.numeric(difftime(HuntTime, t_before, units = "secs")),
      InterpolatedX = x_before +
        time_traversed / time_total * (x_after - x_before),
      InterpolatedY = y_before +
        time_traversed / time_total * (y_after - y_before)
    ) %>%
    # if location before hunting event == location after hunting event,
    # no interpolation is needed
    mutate(
      InterpolatedX = ifelse(time_total == 0, x_after, InterpolatedX),
      InterpolatedY = ifelse(time_total == 0, y_after, InterpolatedY)
    ) %>%
    # calculate distance
    rowwise() %>%
    mutate(Distance = st_distance(
      st_sfc(st_point(c(HuntX, HuntY)), crs = 32633),
      st_sfc(st_point(c(InterpolatedX, InterpolatedY)), crs = 32633)
    )[[1]]) %>%
    select(Sender.ID, Hunt.ID, HuntDate, HuntTime, HuntTimeMissing, Distance)

  # If timestamp does not exist, find average distance on the day of hunting event.
  # TBD: compute the average distance from the deer to the hunting event
  # on the day of the hunting event
  data_without_time <- data_with_time[0, ]
  
  data_all <- full_join(
    data_with_time,
    data_without_time,
    join_by(Sender.ID, Hunt.ID, HuntDate, HuntTime, HuntTimeMissing, Distance)
  )
  
  return(data_all)
}
