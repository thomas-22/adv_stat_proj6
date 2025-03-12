# -------------------------
# utility function for interaction when showing multiple plots
# -------------------------
readkey <- function()
{
  cat ("##############\nPress [enter] to continue\n##############")
  line <- readline()
}

# -------------------------
# timediff eval curve for the scoring function:
# -------------------------
timediff_evaluation_asym_curve <- function(x, peak = 19, rise_sigma = 2, fall_sigma = 2.5, height = 1) {
  ifelse(
    x <= peak,
    height * exp(-((x - peak)^2) / (2 * rise_sigma^2)),  # Rising phase (Gaussian)
    height * exp(-(x - peak) / fall_sigma)              # Falling phase (Exponential decay)
  )
}

# -------------------------
# print or save the score curve for x = 0...40
# -------------------------
score_curve <- function(save = FALSE) {
  plot <- tibble(x = seq(0, 40, .1)) %>%
    mutate(y = timediff_evaluation_asym_curve(x)) %>%
    ggplot(aes(x, y)) +
    geom_line(color = "blue") +
    labs(x = "Time (hours)", y = "Score", title = "Time Difference vs. Score") 
  if(save) {
    ggsave(plot = plot, 
           filename = "Figures/Timediff_Function_impact_curve.png", device = "png")
  } else {
    print(plot)
  }
}


# -------------------------
# Calculuate the distances between deer and hunting events
# The function interpolates the position of the deer at the time of the hunting
# event and calculates the Euclidean distance, the distance in X direction, and
# the distance in Y direction.
# -------------------------
CalcDist <- function(deer_hunt_pairs, Movement, HuntEvents) {
  checkmate::assertDataFrame(Movement)
  checkmate::assertDataFrame(HuntEvents)
  checkmate::assertDataFrame(deer_hunt_pairs)
  checkmate::assertSubset(c("Sender.ID", "Hunt.ID"), choices = names(deer_hunt_pairs))
  interpolated_positions <- interpolate(
    deer_hunt_pairs, Movement, HuntEvents,
    verbose = FALSE
  )
  # Calculate and return the distances in km
  interpolated_positions %>%
    mutate(
      Distance = sqrt(
        (InterpolatedX - HuntX)^2 + (InterpolatedY - HuntY)^2
      ) / 1e3,
      DistanceX = abs(InterpolatedX - HuntX) / 1e3,
      DistanceY = abs(InterpolatedY - HuntY) / 1e3
    )
}

# -------------------------
# utility function to save model data
# -------------------------
save.model.data <- function(data) {
  data$set <- seq_len(nrow(data))
  data <- relocate(data, set)
  data$unique_deers <- vapply(data$data, function(x) length(unique(x$Deer.ID)), numeric(1))
  data$obs <- vapply(data$data, nrow, numeric(1))
  saveRDS(data$data, "Data/processed/ModellingData.RDS")
  data$data <- NULL
  saveRDS(data, "Data/processed/Datasets.RDS")
}


