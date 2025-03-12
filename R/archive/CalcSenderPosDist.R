library(data.table)
library(sf)

#getwd()
source("./R/Datafusion.R")

MovementDT <- as.data.table(Movement)
setkey(MovementDT, Sender.ID, t_)
HuntEventsDT <- as.data.table(HuntEventsreduced)

interpolated_positions <- rbindlist(lapply(1:nrow(HuntEventsDT), function(i) {
  hunt_time <- HuntEventsDT$t_[i]
  hunt_x <- HuntEventsDT$X[i]
  hunt_y <- HuntEventsDT$Y[i]
  
  # Filter rows for the same Sender.IDs with timestamps around the hunting event
  relevant_data <- MovementDT[
    , .SD[t_ <= hunt_time | t_ >= hunt_time], by = Sender.ID]
  
  # Sort relevant_data by time to ensure accuracy
  setorder(relevant_data, t_)
  
  # Handle exact match cases explicitly
  exact_match <- relevant_data[t_ == hunt_time]
  if (nrow(exact_match) > 0) {
    exact_match <- exact_match[, .(
      Sender.ID,
      HuntEventTime = hunt_time,
      HuntEventX = hunt_x,
      HuntEventY = hunt_y,
      InterpolatedX = x_,
      InterpolatedY = y_
    )]
  } else {
    # Calculate closest rows before and after the event per Sender.ID
    before_event <- relevant_data[, .SD[t_ < hunt_time][.N], by = Sender.ID]
    after_event <- relevant_data[, .SD[t_ > hunt_time][1], by = Sender.ID]
    
    # Merge before and after events
    merged_events <- merge(before_event, after_event, by = "Sender.ID", suffixes = c("_before", "_after"))
    
    # Perform interpolation
    exact_match <- merged_events[
      !is.na(t__before) & !is.na(t__after),
      .(
        Sender.ID,
        HuntEventTime = hunt_time,
        HuntEventX = hunt_x,
        HuntEventY = hunt_y,
        InterpolatedX = x__before + (as.numeric(difftime(hunt_time, t__before, units = "secs")) /
                                       as.numeric(difftime(t__after, t__before, units = "secs"))) * (x__after - x__before),
        InterpolatedY = y__before + (as.numeric(difftime(hunt_time, t__before, units = "secs")) /
                                       as.numeric(difftime(t__after, t__before, units = "secs"))) * (y__after - y__before)
      )
    ]
  }
  
  exact_match
}))


# Convert the result back to a data.frame
interpolated_positions <- as.data.frame(interpolated_positions)

#Kick out rows with NAs
StressEvents <- na.omit(interpolated_positions)

# Calculate the distances and assign StressorID
StressEvents <- StressEvents %>%
  rowwise() %>%
  mutate(
    Distance = st_distance(
      st_sfc(st_point(c(HuntEventX, HuntEventY)), crs = 32633),
      st_sfc(st_point(c(InterpolatedX, InterpolatedY)), crs = 32633)
    )
  ) %>%
  ungroup() %>%
  mutate(StressorID = row_number())
StressEvents$Distance <- as.numeric(StressEvents$Distance)

#View(StressEvents)
