# Calculuate the distances between deer and hunting events
#
# `sender_hunt_pairs` is a data.frame with Sender.ID and Hunt.ID as columns.
# The function interpolates the position of the deer at the time of the hunting
# event and calculates the Euclidean distance, the distance in X direction, and
# the distance in Y direction.
CalcDist <- function(deer_hunt_pairs, Movement, HuntEvents) {
  # TBD assertions?

  interpolated_positions <- interpolate(
    deer_hunt_pairs, Movement, HuntEvents,
    verbose = FALSE
  )
  # Calculate the distances
  # (Since we just want the Euclidean distances, no need to use the sf package)
  interpolated_positions %>%
    mutate(
      Distance = sqrt(
        (InterpolatedX - HuntEventX)^2 + (InterpolatedY - HuntEventY)^2
      ),
      DistanceX = abs(InterpolatedX - HuntEventX),
      DistanceY = abs(InterpolatedY - HuntEventY),
      DistanceX2 = (HuntEventX - InterpolatedX)^2,
      DistanceY2 = (HuntEventY - InterpolatedY)^2
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
    filter(!is.na(t_)) %>%
    distinct()
  # If an input hunting event ID has no timestamp, give a warning
  lapply(deer_hunt_pairs$Hunt.ID, function(hunt_id) {
    if (!hunt_id %in% HuntEvents$Hunt.ID) {
      warning(paste("Hunting event with ID", hunt_id, "has no timestamp"))
    }
  })

  interpolated_positions <- as_tibble(deer_hunt_pairs) %>%
    # Get hunting events with the given ids
    left_join(
      HuntEvents %>% select(Hunt.ID, X, Y, t_),
      by = "Hunt.ID"
    ) %>%
    rename(HuntEventTime = t_, HuntEventX = X, HuntEventY = Y) %>%
    # Find the movement entry just before hunting event
    left_join(
      Movement %>% select(Sender.ID, t_, x_, y_),
      join_by(Sender.ID, closest(HuntEventTime >= t_))
    ) %>%
    rename(t_before = t_, x_before = x_, y_before = y_) %>%
    # Find the movement entry just after the hunting event
    left_join(
      # Remove the selection here to join all columns of Movement
      # into the result, but I don't recommend it
      Movement %>% select(Sender.ID, t_, x_, y_),
      join_by(Sender.ID, closest(HuntEventTime <= t_))
    ) %>%
    rename(t_after = t_, x_after = x_, y_after = y_) %>%
    # Interpolate
    mutate(
      time_total = as.numeric(difftime(t_after, t_before, units = "secs")),
      time_traversed = as.numeric(difftime(HuntEventTime, t_before, units = "secs")),
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
  return(select(interpolated_positions,
    Sender.ID,
    HuntEventTime, HuntEventX, HuntEventY,
    InterpolatedX, InterpolatedY
  ))
}

#-------------------------------------------------------------------------------
# Example (written for old data, now outdated, but same issues may still exist)

# add id column for easier joining
HuntEvents <- HuntEvents_Reduced_UTM_New %>%
  distinct() %>%
  mutate(Hunt.ID = as.factor(row_number()))

# Here we just get all possible combinations of deer and hunting events,
# although in reality we are only interested in hunting events that happened
# in a certain time period before each defecation event.
all_hunt_ids <- HuntEvents %>% filter(!is.na(t_)) %>% select(Hunt.ID)
all_sender_ids <- Movement %>% select(Sender.ID) %>% distinct()
all_pairs <- cross_join(all_sender_ids, all_hunt_ids)
interpolated_positions <- interpolate(
  all_pairs,
  Movement,
  HuntEvents,
  verbose = TRUE
)
View(interpolated_positions)

# A few issues here:

# A hunting event might happen between two movement entries that are days apart.
# Is interpolation still meaningful?
interpolated_positions %>%
  filter(date(t_after) - date(t_before) > days(1)) %>%
  select(Sender.ID, HuntEventTime, t_before, t_after) %>%
  View()

# In some cases, there is no movement data prior to the hunting event.
no_t_before <- interpolated_positions %>%
  filter(is.na(t_before)) %>%
  select(Sender.ID, HuntEventTime, t_before, t_after)
View(no_t_before)
# These are the most relevant cases:
no_t_before %>%
  distinct(Sender.ID, HuntEventTime) %>%
  left_join(
    FCMStress %>% select(Sender.ID, Collar_t_),
    join_by(Sender.ID, HuntEventTime <= Collar_t_)
  ) %>%
  filter(Collar_t_ - HuntEventTime < hours(30))

# There might also be hunting events with no movement data after them.

#-------------------------------------------------------------------------------
# Get distances
StressEvents_new <- CalcDist(all_pairs, Movement, HuntEvents) %>%
  na.omit()
View(StressEvents_new)
#-------------------------------------------------------------------------------
