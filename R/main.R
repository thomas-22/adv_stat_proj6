# -------------------------
# sourcing files
# -------------------------
source("R/settings.R")
source("R/source.all.R")
# -------------------------
# Data fusion
# -------------------------
cat("Running data fusion...\n")
prepared_data <- suppressWarnings(run_datafusion(save = TRUE))
Movement <- prepared_data$Movement
FCMStress <- prepared_data$FCMStress
HuntEvents <- prepared_data$HuntEvents

# -------------------------
# Prepare data for modeling
# -------------------------
cat("Preparing data for modeling...\n")
param_grid <- purrr::list_rbind(list(
  # last
  data.frame(gut_retention_time_lower = 0, 
             gut_retention_time_upper = 36, 
             distance_threshold = 10, 
             filter_criterion = "last"),
  # nearest
  data.frame(gut_retention_time_lower = 0, 
             gut_retention_time_upper = 36, 
             distance_threshold = 10, 
             filter_criterion = "nearest"),
  # score
  data.frame(gut_retention_time_lower = 0, 
             gut_retention_time_upper = 200, 
             distance_threshold = 15, 
             filter_criterion = "score")
))

datasets <- param_grid %>%
  purrr::pmap(
    ~ assign_hunts_to_fcm(
      FCMStress, HuntEvents, Movement,
      gut_retention_time_lower = ..1,
      gut_retention_time_upper = ..2,
      distance_threshold = ..3,
      filter_criterion = ..4
    )
  )

res <- tibble(param_grid, data = datasets)
save.model.data(res)

