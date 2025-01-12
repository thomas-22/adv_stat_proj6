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
library(ggplot2)
library(ggeffects)
library(patchwork)

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

# Sanity checks
summary(HuntEvents)
summary(FCMStress)
summary(Movement)

# Remove FCM outliers -> does not affect much
# FCMStress <- FCMStress %>% filter(ng_g < 1000)

# -------------------------
# Prepare data for modeling
# -------------------------

param_grid <- expand.grid(
  gut_retention_time_lower = c(19, 14),
  gut_retention_time_upper = 50,
  distance_threshold = c(10, 20),
  filter_criterion = c("last", "nearest")
)
View(param_grid)

datasets <- param_grid %>%
  pmap(
    ~ assign_hunts_to_fcm(
      FCMStress, HuntEvents, Movement,
      gut_retention_time_lower = ..1,
      gut_retention_time_upper = ..2,
      distance_threshold = ..3,
      filter_criterion = ..4
    )
  )
res <- tibble(param_grid, data = datasets)

# sainity check
res %>%
  filter(gut_retention_time_lower == 19, distance_threshold == 20, filter_criterion == "last") %>%
  pull(data) %>%
  map(summary)

res %>%
  filter(gut_retention_time_lower == 19, distance_threshold == 10, filter_criterion == "last") %>%
  pull(data) %>%
  map(summary)

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
      Pregnant + NumOtherHunts + s(DefecDay, bs = "cr"),
    data = data,
    family = family
  )
}

fit_gamm <- function(data, family = gaussian()) {
  gamm4(
    ng_g ~ s(TimeDiff, bs = "cr") + s(Distance, bs = "cr") + s(SampleDelay, bs = "cr") +
      Pregnant + NumOtherHunts + s(DefecDay, bs = "cr"),
    random = ~ (1 | Sender.ID),
    data = data,
    family = family
  )
}

fit_gamm_interact <- function(data, family = gaussian()) {
  gamm4(
    ng_g ~ t2(TimeDiff, Distance, bs = "cr") + s(SampleDelay, bs = "cr") +
      Pregnant + NumOtherHunts + s(DefecDay, bs = "cr"),
    random = ~ (1 | Sender.ID),
    data = data,
    family = family
  )
}

fig_gam_tp <- function(data, family = gaussian()) {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr") + s(DistanceX, DistanceY, bs = "tp") + s(SampleDelay, bs = "cr") +
      Pregnant + NumOtherHunts + s(DefecDay, bs = "cr"),
    # random = list(Sender.ID = ~1, DefecMonth = ~1),
    data = data,
    family = family
  )
}

# Gaussian
res <- res %>%
  mutate(
    gaussian_gam = map(data, fit_gam),
    gaussian_gamm = map(data, fit_gamm)
  )
# Inspect some results
m0 <- res %>%
  filter(gut_retention_time_lower == 19, distance_threshold == 20, filter_criterion == "last") %>%
  pull(gaussian_gam)
m0 <- m0[[1]]
summary(m0)
qq.gam(m0)  # Gaussian is not suitable

# Gamma (takes a while)
res <- res %>%
  mutate(
    gamma_gam = map(data, fit_gam, family = Gamma(link = "log")),
    gamma_gamm = map(data, fit_gamm, family = Gamma(link = "log"))
  )
# Inspect some results
m0 <- res %>%
  filter(gut_retention_time_lower == 19, distance_threshold == 20, filter_criterion == "last") %>%
  pull(gamma_gam)
m0 <- m0[[1]]
summary(m0)
qq.gam(m0)

# Define distance and timediff w.r.t. last hunting event
# Plot GAMs
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "last",
  x = "TimeDiff",
  xlab = "Time difference [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "last",
  x = "Distance",
  xlab = "Distance [km]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "last",
  x = "SampleDelay",
  xlab = "Sample delay [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "last",
  x = "NumOtherHunts",
  xlab = "Other hunting events"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "last",
  x = "DefecDay",
  xlab = "Defecation day"
)

# Plot GAMMs
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "last",
  x = "TimeDiff",
  xlab = "Time difference [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "last",
  x = "Distance",
  xlab = "Distance [km]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "last",
  x = "SampleDelay",
  xlab = "Sample delay [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "last",
  x = "NumOtherHunts",
  xlab = "Other hunting events"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "last",
  x = "DefecDay",
  xlab = "Defecation day"
)


# Define distance and timediff w.r.t. nearest hunting event within 19~50 hours
# Plot GAMs
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "nearest",
  x = "TimeDiff",
  xlab = "Time difference [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "nearest",
  x = "Distance",
  xlab = "Distance [km]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "nearest",
  x = "SampleDelay",
  xlab = "Sample delay [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "nearest",
  x = "NumOtherHunts",
  xlab = "Other Hunting Events"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gam",
  filter_criterion = "nearest",
  x = "DefecDay",
  xlab = "Defecation day"
)

# Plot GAMMs
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "nearest",
  x = "TimeDiff",
  xlab = "Time difference [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "nearest",
  x = "Distance",
  xlab = "Distance [km]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "nearest",
  x = "SampleDelay",
  xlab = "Sample delay [hours]"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "nearest",
  x = "NumOtherHunts",
  xlab = "Other Hunting Events"
)
plot_predictions_across_datasets(res,
  model_type = "gamma_gamm",
  filter_criterion = "nearest",
  x = "DefecDay",
  xlab = "Defecation day"
)


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

