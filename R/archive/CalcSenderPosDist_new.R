# Calculuate the distances between deer and hunting events
#
# `sender_hunt_pairs` is a data.frame with Sender.ID and Hunt.ID as columns.
# The function interpolates the position of the deer at the time of the hunting
# event and calculates the Euclidean distance, the distance in X direction, and
# the distance in Y direction.
CalcDist <- function(deer_hunt_pairs, Movement, HuntEvents) {
  
  interpolated_positions <- interpolate(
    deer_hunt_pairs, Movement, HuntEvents,
    verbose = FALSE
  )
  # Calculate the distances in km
  interpolated_positions %>%
    mutate(
      Distance = sqrt(
        (InterpolatedX - HuntX)^2 + (InterpolatedY - HuntY)^2
      ) / 1e3,
      DistanceX = abs(InterpolatedX - HuntX) / 1e3,
      DistanceY = abs(InterpolatedY - HuntY) / 1e3
      # Mahalanobis distance to account for the correlation between X and Y?
      # Distances in PC directions?
    )
}

# Interpolate the position of the deer at the time of the hunting event
#
# Turn on `verbose` to see all columns used in the interpolation.
interpolate <- function(deer_hunt_pairs, Movement, HuntEvents, verbose = FALSE) {
  # TBD assertions?
  
  # Filter out hunting events with no timestamp and remove duplicates,
  # just in case the pre-processing step is skipped
  HuntEvents <- HuntEvents %>%
    filter(!is.na(HuntTime)) %>%
    distinct()
  # If an input hunting event ID has no timestamp, give a warning
  lapply(deer_hunt_pairs$Hunt.ID, function(hunt_id) {
    if (!hunt_id %in% HuntEvents$Hunt.ID) {
      warning(paste("Hunting event with ID", hunt_id, "has no timestamp"))
    }
  })
  
  interpolated_positions <- deer_hunt_pairs %>%
    as_tibble() %>%
    # Get hunting events with the given ids
    left_join(
      HuntEvents %>% dplyr::select(Hunt.ID, HuntX, HuntY, HuntTime),
      by = "Hunt.ID"
    ) %>%
    # Find the movement entry just before hunting event
    left_join(
      Movement %>% dplyr::select(Sender.ID, t_, x_, y_),
      join_by(Sender.ID, closest(HuntTime >= t_))
    ) %>%
    rename(t_before = t_, x_before = x_, y_before = y_) %>%
    # Find the movement entry just after the hunting event
    left_join(
      Movement %>% dplyr::select(Sender.ID, t_, x_, y_),
      join_by(Sender.ID, closest(HuntTime <= t_))
    ) %>%
    rename(t_after = t_, x_after = x_, y_after = y_) %>%
    # Interpolate
    mutate(
      time_total = as.numeric(difftime(t_after, t_before, units = "secs")),
      time_traversed = as.numeric(difftime(HuntTime, t_before, units = "secs")),
      InterpolatedX = x_before +
        time_traversed / time_total * (x_after - x_before),
      InterpolatedY = y_before +
        time_traversed / time_total * (y_after - y_before)
    ) %>%
    # If hunting event happens exactly at full hour, e.g., 23:00,
    # t_before == t_after, so no interpolation is needed
    mutate(
      InterpolatedX = ifelse(time_total == 0, x_after, InterpolatedX),
      InterpolatedY = ifelse(time_total == 0, y_after, InterpolatedY)
    )
  
  if (verbose) return(interpolated_positions)
  
  dplyr::select(
    interpolated_positions,
    Sender.ID,
    Hunt.ID, HuntTime, HuntX, HuntY,
    InterpolatedX, InterpolatedY
  )
}
