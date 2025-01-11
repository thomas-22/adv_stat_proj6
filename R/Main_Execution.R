# --------------------------------------
# MAIN EXECTUION
# --------------------------------------
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(readxl)
library(purrr)


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

# Plotting Models
source("R/PlotModels.R")

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

# Remove FCM outliers -> does not affect much
# FCMStress <- FCMStress %>% filter(ng_g < 1000)

# -------------------------
# Prepare data for modeling
# -------------------------

# Variant 1:
# For each FCM sample, find the last hunting event before defecation with
# complete time and location information.
# All other hunting events (including missing data) within
# - a certain number of days prior to defecation and
# - distance below a certain threshold
# are considered *potentially relevant*.
data_2_19 <- assign_hunts_to_fcm(
  FCMStress, HuntEvents, Movement,
  daydiff_threshold = 2, gut_retention_hours = 19, distance_threshold = 20
)
data_2_14 <- assign_hunts_to_fcm(
  FCMStress, HuntEvents, Movement,
  daydiff_threshold = 2, gut_retention_hours = 14, distance_threshold = 20
)

# sanity check
datasets %>%
  filter(daydiff_threshold == 14, gut_retention_hours == 19) %>%
  pull(data) %>% map(summary)

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
library(gamm4)

fit_gam <- function(data, family = gaussian()) {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr") + s(Distance, bs = "cr") + s(SampleDelay, bs = "cr") +
      Pregnant + NumOtherHunts + Season,
    data = data,
    family = family
  )
}

fit_gamm <- function(data, family = gaussian()) {
  gamm4(
    ng_g ~ s(TimeDiff, bs = "cr") + s(Distance, bs = "cr") + s(SampleDelay, bs = "cr") +
      Pregnant + NumOtherHunts,
    random = ~ (1 | Sender.ID) + (1 | DefecMonth),
    data = data,
    family = family
  )
}

fit_gamm_interact <- function(data, family = gaussian()) {
  gamm4(
    ng_g ~ t2(TimeDiff, Distance, bs = "cr") + s(SampleDelay, bs = "cr") +
      Pregnant + NumOtherHunts,
    random = ~ (1 | Sender.ID) + (1 | DefecMonth),
    data = data,
    family = family
  )
}

fig_gam_tp <- function(data, family = gaussian()) {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr") + s(DistanceX, DistanceY, bs = "tp") + s(SampleDelay, bs = "cr") +
      Pregnant + NumOtherHunts,
    # random = list(Sender.ID = ~1, DefecMonth = ~1),
    data = data,
    family = family
  )
}

# -------------------------
# Try Gaussian

gaussian_gam_2_19 <- fit_gam(data_2_19, family = gaussian())
summary(gaussian_gam_2_19)
qq.gam(gaussian_gam_2_19)

plots <- list(
  plot_predictions(gaussian_gam_2_19, covariate = "TimeDiff", xlab = "Time difference [hours]", xmin = 19),
  plot_predictions(gaussian_gam_2_19, covariate = "Distance", xlab = "Distance [km]"),
  plot_predictions(gaussian_gam_2_19, covariate = "SampleDelay", xlab = "Sample delay [hours]"),
  plot_predictions(gaussian_gam_2_19, covariate = "NumOtherHunts", xlab = "Other hunting events"),
  plot_predictions(gaussian_gam_2_19, covariate = "Pregnant", xlab = "Pregnant")
)
wrap_plots(plots, axis_titles = "collect")

gaussian_gamm_2_19 <- fit_gamm(data_2_19, family = gaussian())
qq.gam(gaussian_gamm_2_19$gam)

plots <- list(
  plot_predictions(gaussian_gamm_2_19, covariate = "TimeDiff", xlab = "Time difference [hours]", xmin = 19),
  plot_predictions(gaussian_gamm_2_19, covariate = "Distance", xlab = "Distance [km]"),
  plot_predictions(gaussian_gamm_2_19, covariate = "SampleDelay", xlab = "Sample delay [hours]"),
  plot_predictions(gaussian_gamm_2_19, covariate = "NumOtherHunts", xlab = "Other hunting events"),
  plot_predictions(gaussian_gamm_2_19, covariate = "Pregnant", xlab = "Pregnant")
)
wrap_plots(plots, axis_titles = "collect")

# -------------------------


#-------------------------
# Try Gamma

# 1. 19 hours gut retention, 5 days time frame to count relevant hunting events
gamma_gam_2_19 <- fit_gam(data_2_19, family = Gamma(link = "log"))
summary(gamma_gam_2_19)
qq.gam(gamma_gam_2_19)

plots <- list(
  plot_predictions(gamma_gamm_2_19, covariate = "TimeDiff", xlab = "Time difference [hours]", xmin = 19),
  plot_predictions(gamma_gamm_2_19, covariate = "Distance", xlab = "Distance [km]"),
  plot_predictions(gamma_gamm_2_19, covariate = "SampleDelay", xlab = "Sample delay [hours]"),
  plot_predictions(gamma_gamm_2_19, covariate = "NumOtherHunts", xlab = "Other hunting events"),
  plot_predictions(gamma_gamm_2_19, covariate = "Pregnant", xlab = "Pregnant")
)
wrap_plots(plots)


gamma_gamm_2_19 <- fit_gamm(data_2_19, family = Gamma(link = "log"))
summary(gamma_gamm_2_19$gam)
qq.gam(gamma_gamm_2_19$gam, type = "deviance")
summary(gamma_gamm_2_19$mer)

plots <- list(
  plot_predictions(gamma_gamm_2_19, covariate = "TimeDiff", xlab = "Time difference [hours]", xmin = 19),
  plot_predictions(gamma_gamm_2_19, covariate = "Distance", xlab = "Distance [km]"),
  plot_predictions(gamma_gamm_2_19, covariate = "SampleDelay", xlab = "Sample delay [hours]"),
  plot_predictions(gamma_gamm_2_19, covariate = "NumOtherHunts", xlab = "Other hunting events"),
  plot_predictions(gamma_gamm_2_19, covariate = "Pregnant", xlab = "Pregnant")
)
wrap_plots(plots)


# 2 Vary gut retention time
gamma_gamm_2_14 <- assign_hunts_to_fcm(
  FCMStress, HuntEvents, Movement,
  daydiff_threshold = 2, gut_retention_hours = 14, distance_threshold = 20
) %>%
  fit_gamm(family = Gamma(link = "log"))
gam.check(gamma_gamm_2_14$gam)
summary(gamma_gamm_2_14$gam)
summary(gamma_gamm_2_14$mer)

plots <- list(
  plot_predictions(gamma_gamm_2_14$gam, covariate = "TimeDiff", xlab = "Time difference [hours]", xmin = 14),
  plot_predictions(gamma_gamm_2_14$gam, covariate = "Distance", xlab = "Distance [km]"),
  plot_predictions(gamma_gamm_2_14$gam, covariate = "SampleDelay", xlab = "Sample delay [hours]"),
  plot_predictions(gamma_gamm_2_14$gam, covariate = "NumOtherHunts", xlab = "Other hunting events")
)
wrap_plots(plots)


# ## Larger distance, higher FCM levels? WHY?
# library(car)
# data <- assign_hunts_to_fcm(
#   FCMStress, HuntEvents, Movement,
#   daydiff_threshold = 2, gut_retention_hours = 19, distance_threshold = 20
# )
# model <- lm(ng_g ~ Distance + TimeDiff + NumOtherHunts, data = data)
# vif(model)
# # Collinearity is not a problem.

# 3. Interaction between TimeDiff and Distance
gamma_gamm_2_19_interact <- fit_gamm_interact(data_2_19, family = Gamma(link = "log"))
summary(gamma_gamm_2_19_interact$gam)
# bad plot
vis.gam(gamma_gamm_2_19_interact$gam, view = c("Distance", "TimeDiff"), plot.type = "contour")

# 4. Separate distance into 2 directions
gamma_gam_2_19_tp <- fig_gam_tp(data_2_19, family = Gamma(link = "log"))
summary(gamma_gam_2_19_tp)
# bad plots
vis.gam(gamma_gam_2_19_tp, view = c("DistanceX", "DistanceY"), plot.type = "contour")
plot.gam(gamma_gam_2_19_tp, page = 1)


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

