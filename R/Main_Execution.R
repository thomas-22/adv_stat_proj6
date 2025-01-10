# --------------------------------------
# MAIN EXECTUION
# --------------------------------------
library(readr)
library(stringr)
library(dplyr)
library(sf)
library(lubridate)
library(readxl)



# -------------------------
# Overview & Source Everything
# -------------------------

#Preprocess
source("R/Datafusion.R")
source("R/Utils.R")
source("R/CalcSenderPosDist_new.R")
source("R/Assign_FCMData_to_Stressors.R")

#Plots on Data
source("R/Plot_FinalData.R")

#Modelling
source("R/XGBoost_Model.R")



# -----------------------------------
# DATA PREPROCESSING
# -----------------------------------

# -------------------------
# Data fusion
# -------------------------
prepared_data <- suppressWarnings(run_datafusion())

Movement <- prepared_data$Movement
FCMStress <- prepared_data$FCMStress
HuntEvents <- prepared_data$HuntEvents
# HuntEvents_reduced <- prepared_data$HuntEvents_reduced

# Remove hunting events occurred after the last FCM sample
# last_sample_date <- max(FCMStress$SampleDate)
# HuntEvents <- filter(HuntEvents, HuntDate > last_sample_date)

# Sanity checks
summary(HuntEvents)
summary(FCMStress)
summary(Movement)

# -------------------------
# Prepare data for modeling
# -------------------------

# # just a sanity check
# data_least_filtered <- Assign_Hunts_to_FCM(
#   FCMStress, HuntEvents, Movement,
#   daydiff_threshold = 1000,
#   gut_retention_hours = 0,
#   distance_threshold = 100 # km
# )
# summary(data_least_filtered)

# Variant 1:
# For each FCM sample, find the last hunting event before defecation with
# complete time and location information.
# All other hunting events (including missing data) within
# - a certain number of days prior to defecation and
# - distance below a certain threshold
# are considered *potentially relevant*.
datasets <- list()

param_grid <- rbind(
  # Dataset 1:
  # Gut retention time: 19 hours -- prior knowledge.
  # -- This means the last hunting event must be at least 19 hours before the FCM sample.
  # Daydiff: 2 days, because FCM level returns to normal after 14-36 hours.
  # -- Here we can only be as precise as days, because some hunting events have no time information.
  # Distance threshold only serves as outlier filter.
  data.frame(daydiff_threshold = 2, gut_retention_hours = 19, distance_threshold = 20),
  data.frame(daydiff_threshold = 7, gut_retention_hours = 19, distance_threshold = 20),
  data.frame(daydiff_threshold = 14, gut_retention_hours = 19, distance_threshold = 20),
  # Change gut rentention time to 14 hours. This affects the last hunting event.
  data.frame(daydiff_threshold = 2, gut_retention_hours = 14, distance_threshold = 20),
  data.frame(daydiff_threshold = 7, gut_retention_hours = 14, distance_threshold = 20),
  data.frame(daydiff_threshold = 14, gut_retention_hours = 14, distance_threshold = 20)
)

n_configs = nrow(param_grid)
datasets <- lapply(seq_len(n_configs), function(i) {
  params = param_grid[i, ]
  assign_hunts_to_fcm(FCMStress, HuntEvents, Movement,
    daydiff_threshold = params[["daydiff_threshold"]],
    gut_retention_hours = params[["gut_retention_hours"]],
    distance_threshold = params[["distance_threshold"]]
  )
})

datasets19 = datasets[param_grid$gut_retention_hours == 19]
datasets14 = datasets[param_grid$gut_retention_hours == 14]

# Variant 2: use distance-based filtering.
# Variant 3: use score-based filtering.

# -------------------------
# Plot Data
# -------------------------

# Draw_Illustration_Map()

# #Example Calls for one of the datasets
# plot_ng_as_func_of_dist_timediff(data_cleanedup, chosen_var = "Distance")
# plot_ng_as_func_of_dist_timediff(data_cleanedup, chosen_var = "TimeDiff")

# plots_2d <- plot_data_2d()
# lapply(plots_2d, print)

# plots_3d <- plot_data_3d()
# lapply(plots_3d, print)

# generate_hist_timediff()
# plot_lognorm_gamma_univar_independent()
# plot_collar_t_raw()

# -----------------------------------
# MODELING
# -----------------------------------
library(mgcv)
library(ggeffects)
library(patchwork)

fit_gam <- function(data) {
  gam(
    ng_g ~ s(TimeDiff, bs = "ps") + s(Distance, bs = "ps") + s(SampleDelay, bs = "ps") +
      Pregnant + NumOtherHunts + Season,
    data = data,
    family = Gamma(link = "log")
  )
}


plot_marginal_pred <- function(model, covariate, title, xlab, xmin = NULL) {
  model_data <- model$model
  predict_data <- ggpredict(model, terms = covariate, typical = "median") %>%
    as_tibble()
  if (!is.null(xmin)) {
    predict_data <- filter(predict_data, x >= xmin)
  }

  ggplot(predict_data, aes(x = x, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_rug(data = model_data, aes(x = .data[[covariate]]), inherit.aes = FALSE) +
    labs(title = title, y = "FCM level [ng/g]", x = xlab) +
    ylim(50, 900) +
    if (!is.null(xmin)) xlim(xmin, NA)
}

# PLOTS ARE BAD (not comparable because of range differences)
# # Models for gut_retention_hours = 19
# gams19 <- lapply(datasets19, fit_gam)

# plots_timediff19 <- lapply(seq_along(gams19), function(i) {
#   plot_marginal_pred(gams[[i]],
#     covariate = "TimeDiff",
#     title = paste(
#       param_grid[param_grid$gut_retention_hours == 19, ]$daydiff_threshold[[i]],
#       "days"
#     ),
#     xlab = paste("Time difference [hours]"),
#     xmin = 19
#   )
# })
# plots_distance19 <- lapply(seq_along(gams19), function(i) {
#   plot_marginal_pred(gams[[i]],
#     covariate = "Distance",
#     title = paste(
#       param_grid[param_grid$gut_retention_hours == 19, ]$daydiff_threshold[[i]],
#       "days"
#     ),
#     xlab = "Distance [km]"
#   )
# })
# plots_sampledelay19 <- lapply(seq_along(gams19), function(i) {
#   plot_marginal_pred(gams[[i]],
#     covariate = "SampleDelay",
#     title = paste(
#       param_grid[param_grid$gut_retention_hours == 19, ]$daydiff_threshold[[i]],
#       "days"
#     ),
#     xlab = "Sample delay [hours]"
#   )
# })
# plots_otherhunts19 <- lapply(seq_along(gams19), function(i) {
#   plot_marginal_pred(gams[[i]],
#     covariate = "NumOtherHunts",
#     title = paste(
#       param_grid[param_grid$gut_retention_hours == 19, ]$daydiff_threshold[[i]],
#       "days"
#     ),
#     xlab = "Other hunting events"
#   )
# })

# wrap_plots(
#   c(plots_timediff19, plots_distance19, plots_sampledelay19, plots_otherhunts19),
#   ncol = 4, byrow = FALSE
# )


# # -------------------------
# # XGBoost Model
# # -------------------------

# # Set tune = TRUE if you want to run hyperparameter tuning.
# # Leave it = FALSE if you want to use the included result of the tuning. (Models/final_xgboost..)
# # WARNING: Setting tune = TRUE means the function will take very long (Multiple Hours+) to execute due to many computations.
# # Max_Iterations: Only relevant if tune = TRUE

# xg_boost_results <- XGBoost_run_default_pipeline(data_cleanedup,
#                                                  covariables = c("TimeDiff", "Distance"),
#                                                  tune = FALSE,
#                                                  max_iterations = 3)
# xg_boost_results_transformed <- XGBoost_run_transformed_pipeline(data_cleanedup,
#                                                                  tune = FALSE,
#                                                                  max_iterations = 3)

# #3D Figure of Model
# xg_boost_results$plotly_fig
# #3D Figure of Model of transformed variables
# xg_boost_results_transformed$plotly_fig 

