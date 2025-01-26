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
theme_set(theme_bw(base_size = 22))

library(ggeffects)
library(patchwork)
library(mgcv)
library(gamm4)
library(gratia)

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
# source("R/XGBoost_Model.R")

# Plotting Models
source("R/PlotModels.R")

# -----------------------------------
# DATA PREPROCESSING
# -----------------------------------

# -------------------------
# Data fusion
# -------------------------
cat("Running data fusion...\n")
prepared_data <- suppressWarnings(run_datafusion())

Movement <- prepared_data$Movement
FCMStress <- prepared_data$FCMStress
HuntEvents <- prepared_data$HuntEvents

# Sanity checks
# summary(HuntEvents)
# summary(FCMStress)
# summary(Movement)

# Remove FCM outliers -> does not affect much
# FCMStress <- FCMStress %>% filter(ng_g < 1000)

# -------------------------
# Explorative analysis
# -------------------------

# FCM levels: Please use your plot for Zwischenpraesentation
# ggplot(FCMStress) +
#   geom_boxplot(aes(x = Sender.ID, y = ng_g))

# When the samples where taken?
# Message: irregular sampling times.

# deer_order <- FCMStress %>%
#   group_by(Deer.ID) %>%
#   filter(DefecTime == max(DefecTime)) %>%
#   ungroup() %>%
#   arrange(DefecTime) %>%
#   distinct(Deer.ID) %>%
#   pull(Deer.ID)

# ggplot(FCMStress) +
#   geom_line(aes(x = DefecTime, y = Deer.ID)) +
#   geom_point(aes(x = DefecTime, y = Deer.ID)) +
#   labs(x = "Defecation time", y = "Deer") +
#   scale_y_discrete(limits = rev(deer_order)) +
#   theme(axis.text.y = element_blank())

# -- This block is not very relvant --
# # Sample intervals: we do need to consider correlation between samples.
# FCMStress %>%
#   group_by(Sender.ID) %>%
#   arrange(DefecTime, .by_group = TRUE) %>%
#   mutate(DefecTime_lag = lag(DefecTime, default = NA)) %>%
#   # filter(!is.na(DefecTime_lag)) %>%
#   mutate(interval = as.numeric(difftime(DefecTime, DefecTime_lag, units = "days"))) %>%
#   ungroup() %>%
#   pull(interval) %>%
#   summary()
# ----



# -------------------------
# Prepare data for modeling
# -------------------------
cat("Preparing data for modeling...\n")
param_grid <- list_rbind(list(
  # last
  data.frame(gut_retention_time_lower = 19, gut_retention_time_upper = 50, distance_threshold = 10, filter_criterion = "last"),
  # nearest
  data.frame(gut_retention_time_lower = 19, gut_retention_time_upper = 50, distance_threshold = 10, filter_criterion = "nearest"),
  # score
  data.frame(gut_retention_time_lower = 0, gut_retention_time_upper = 200, distance_threshold = 15, filter_criterion = "score")
))

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

table_datasets <- tibble(param_grid, data = datasets)
table_datasets$set <- seq_len(nrow(table_datasets))
table_datasets <- relocate(table_datasets, set)
table_datasets$unique_deers <- vapply(table_datasets$data, function(x) length(unique(x$Deer.ID)), numeric(1))
table_datasets$obs <- vapply(table_datasets$data, nrow, numeric(1))
table_datasets$data <- NULL




saveRDS(table_datasets, "Data/Datasets.RDS")





# plot(res[[5]][[3]]$Score, res[[5]][[3]]$ng_g,
#      main = "Scatter Plot: Score vs ng/g (Log Scale)",
#      xlab = "Score (log scale)",
#      ylab = "ng/g",
#      col = "blue",
#      pch = 19,    # Solid circle for points
#      cex = 1.5,   # Size of points
#      log = "x")   # Logarithmic scale for x-axis
# grid()            # Add a grid to the plot

  
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
fit_gam <- function(data, family = gaussian()) {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr", k = 20) + s(Distance, bs = "cr", k = 10) +
      s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
      Pregnant + NumOtherHunts,
    data = data,
    family = family
  )
}

fit_gamm <- function(data, family = gaussian()) {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr", k = 20) + s(Distance, bs = "cr", k = 10) +
      s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
      Pregnant + NumOtherHunts + s(Deer.ID, bs = "re"),
    data = data,
    family = family
  )
}

# fit_gamm_interact <- function(data, family = gaussian()) {
#   gam(
#     ng_g ~ te(TimeDiff, Distance, k = 20) +
#       s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 10) +
#       Pregnant + NumOtherHunts + s(Deer.ID, bs = "re"),
#     data = data,
#     family = family
#   )
# }

# fit_gam_tp <- function(data, family = gaussian()) {
#   gam(
#     ng_g ~ s(TimeDiff, bs = "ps") + s(DistanceX, DistanceY, bs = "tp") + s(SampleDelay, bs = "ps") +
#       Pregnant + NumOtherHunts + s(DefecDay, bs = "ps"),
#     # random = list(Deer.ID = ~1, DefecMonth = ~1),
#     data = data,
#     family = family
#   )
# }

cat("Fitting models...\n")

# -------------------------
# Last
m_L <- fit_gamm(res$data[[1]], family = Gamma(link = "log"))
saveRDS(m_L, "Models/m_L.RDS")

p_L_TimeDiff <- ggpredict(m_L, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "")
# Predicted values of the FCM level for a "typical deer", i.e.,
# the other covariates are held constant at their mean values or reference category
# (Pregnant = FALSE).
# (Random effect is set to that of one deer, but since it's just an intercept,
# it is irrelevant to our interpretation of the overall shape/tendency of the curve.)

p_L_Distance <- ggpredict(m_L, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "")

p_L_SampleDelay <- ggpredict(m_L, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "")

p_L_Day <- ggpredict(m_L, terms = c("DefecDay"), title = "") %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "")

p_L <- p_L_TimeDiff + p_L_Distance + p_L_SampleDelay +
  plot_layout(ncol = 3, axis_titles = "collect")
saveRDS(p_L, "Figures/p_L.RDS")
# -------------------------

# -------------------------
# Nearest
m_N <- fit_gamm(res$data[[2]], family = Gamma(link = "log"))
saveRDS(m_N, "Models/m_N.RDS")

p_N_TimeDiff <- ggpredict(m_N, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "")

p_N_Distance <- ggpredict(m_N, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "")

p_N_SampleDelay <- ggpredict(m_N, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "")

p_N_Day <- ggpredict(m_N, terms = c("DefecDay"), title = "") %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "")

p_N <- p_N_TimeDiff + p_N_Distance + p_N_SampleDelay +
  plot_layout(ncol = 3, axis_titles = "collect")
saveRDS(p_L, "Figures/p_N.RDS")
# -------------------------

# -------------------------
# Score
m_S <- fit_gamm(res$data[[3]], family = Gamma(link = "log"))
saveRDS(m_S, "Models/m_S.RDS")

p_S_TimeDiff <- ggpredict(m_S, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "")

p_S_Distance <- ggpredict(m_S, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "")

p_S_SampleDelay <- ggpredict(m_S, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "")

p_S_Day <- ggpredict(m_S, terms = c("DefecDay"), title = "") %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "")

p_S <- p_S_TimeDiff + p_S_Distance + p_S_SampleDelay +
  plot_layout(ncol = 3, axis_titles = "collect")
saveRDS(p_S, "Figures/p_S.RDS")
# -------------------------

# # Gaussian
# res <- res %>%
#   mutate(
#     gaussian_gam = map(data, fit_gam),
#     gaussian_gamm = map(data, fit_gamm)
#   )

# # Gamma (takes a while)
# res <- res %>%
#   mutate(
#     gamma_gam = map(data, fit_gam, family = Gamma(link = "log")),
#     gamma_gamm = map(data, fit_gamm, family = Gamma(link = "log"))
#   )

# # Define distance and timediff w.r.t. last hunting event
# # Plot GAMs
# p_gam_L_TD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "last",
#   x = "TimeDiff",
#   xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gam_L_TD, device = "png", filename = "Plots/p_gam_L_TD.png")

# p_gam_L_Dist <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "last",
#   x = "Distance",
#   xlab = "Distance [km]"
# )
# ggsave(plot = p_gam_L_Dist, device = "png", filename = "Plots/p_gam_L_Dist.png")

# p_gam_L_SD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "last",
#   x = "SampleDelay",
#   xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gam_L_SD, device = "png", filename = "Plots/p_gam_L_SD.png")

# p_gam_L_OtherEvents <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "last",
#   x = "NumOtherHunts",
#   xlab = "Other hunting events"
# )
# ggsave(plot = p_gam_L_OtherEvents, device = "png", filename = "Plots/p_gam_L_OtherEvents.png")

# p_gam_L_Day <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "last",
#   x = "DefecDay",
#   xlab = "Defecation day"
# )
# ggsave(plot = p_gam_L_Day, device = "png", filename = "Plots/p_gam_L_Day.png")

# #####################
# #### Plot GAMMs ####
# #####################
# p_gamm_L_TD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "last",
#   x = "TimeDiff",
#   xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gamm_L_TD, device = "png", filename = "Plots/p_gamm_L_TD.png")


# p_gamm_L_Dist <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "last",
#   x = "Distance",
#   xlab = "Distance [km]"
# )
# ggsave(plot = p_gamm_L_Dist, device = "png", filename = "Plots/p_gamm_L_Dist.png")

# p_gamm_L_SD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "last",
#   x = "SampleDelay",
#   xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gamm_L_SD, device = "png", filename = "Plots/p_gamm_L_SD.png")

# p_gamm_L_OtherEvents <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "last",
#   x = "NumOtherHunts",
#   xlab = "Other hunting events"
# )
# ggsave(plot = p_gamm_L_OtherEvents, device = "png", filename = "Plots/p_gamm_L_OtherEvents.png")

# p_gamm_L_Day <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "last",
#   x = "DefecDay",
#   xlab = "Defecation day"
# )
# ggsave(plot = p_gamm_L_Day, device = "png", filename = "Plots/p_gamm_L_Day.png")


# # Define distance and timediff w.r.t. nearest hunting event within 19~50 hours
# # Plot GAMs
# p_gam_N_TD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "nearest",
#   x = "TimeDiff",
#   xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gam_N_TD, device = "png", filename = "Plots/p_gam_N_TD.png")

# p_gam_N_Dist <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "nearest",
#   x = "Distance",
#   xlab = "Distance [km]"
# )
# ggsave(plot = p_gam_N_Dist, device = "png", filename = "Plots/p_gam_N_Dist.png")

# p_gam_N_SD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "nearest",
#   x = "SampleDelay",
#   xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gam_N_SD, device = "png", filename = "Plots/p_gam_N_SD.png")

# p_gam_N_OtherEvents <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "nearest",
#   x = "NumOtherHunts",
#   xlab = "Other Hunting Events"
# )
# ggsave(plot = p_gam_N_OtherEvents, device = "png", filename = "Plots/p_gam_N_OtherEvents.png")

# p_gam_N_Day <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gam",
#   filter_criterion = "nearest",
#   x = "DefecDay",
#   xlab = "Defecation day"
# )
# ggsave(plot = p_gam_N_Day, device = "png", filename = "Plots/p_gam_N_Day.png")

# # Plot GAMMs
# p_gamm_N_TD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "nearest",
#   x = "TimeDiff",
#   xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gamm_N_TD, device = "png", filename = "Plots/p_gamm_N_TD.png")


# p_gamm_N_Dist <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "nearest",
#   x = "Distance",
#   xlab = "Distance [km]"
# )
# ggsave(plot = p_gamm_N_Dist, device = "png", filename = "Plots/p_gamm_N_Dist.png")

# p_gamm_N_SD <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "nearest",
#   x = "SampleDelay",
#   xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gamm_N_SD, device = "png", filename = "Plots/p_gamm_N_SD.png")

# p_gamm_N_OtherEvents <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "nearest",
#   x = "NumOtherHunts",
#   xlab = "Other Hunting Events"
# )
# ggsave(plot = p_gamm_N_OtherEvents, device = "png", filename = "Plots/p_gamm_N_OtherEvents.png")

# p_gamm_N_Day <- plot_predictions_across_datasets(res,
#   model_type = "gamma_gamm",
#   filter_criterion = "nearest",
#   x = "DefecDay",
#   xlab = "Defecation day"
# )
# ggsave(plot = p_gamm_N_Day, device = "png", filename = "Plots/p_gamm_N_Day.png")


# ####### Gaussian Gam/Gamm 
# p_gau_L_TD <- plot_predictions_across_datasets(res,
#                                                model_type = "gaussian_gam",
#                                                filter_criterion = "last",
#                                                x = "TimeDiff",
#                                                xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gau_L_TD, device = "png", filename = "Plots/p_gau_L_TD.png")

# p_gau_L_Dist <- plot_predictions_across_datasets(res,
#                                                  model_type = "gaussian_gam",
#                                                  filter_criterion = "last",
#                                                  x = "Distance",
#                                                  xlab = "Distance [km]"
# )
# ggsave(plot = p_gau_L_Dist, device = "png", filename = "Plots/p_gau_L_Dist.png")

# p_gau_L_SD <- plot_predictions_across_datasets(res,
#                                                model_type = "gaussian_gam",
#                                                filter_criterion = "last",
#                                                x = "SampleDelay",
#                                                xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gau_L_SD, device = "png", filename = "Plots/p_gau_L_SD.png")

# p_gau_L_OtherEvents <- plot_predictions_across_datasets(res,
#                                                         model_type = "gaussian_gam",
#                                                         filter_criterion = "last",
#                                                         x = "NumOtherHunts",
#                                                         xlab = "Other hunting events"
# )
# ggsave(plot = p_gau_L_OtherEvents, device = "png", filename = "Plots/p_gau_L_OtherEvents.png")

# p_gau_L_Day <- plot_predictions_across_datasets(res,
#                                                 model_type = "gaussian_gam",
#                                                 filter_criterion = "last",
#                                                 x = "DefecDay",
#                                                 xlab = "Defecation day"
# )
# ggsave(plot = p_gau_L_Day, device = "png", filename = "Plots/p_gau_L_Day.png")

# #####################
# #### Plot GAMMs ####
# #####################
# p_gaum_L_TD <- plot_predictions_across_datasets(res,
#                                                 model_type = "gaussian_gamm",
#                                                 filter_criterion = "last",
#                                                 x = "TimeDiff",
#                                                 xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gaum_L_TD, device = "png", filename = "Plots/p_gaum_L_TD.png")


# p_gaum_L_Dist <- plot_predictions_across_datasets(res,
#                                                   model_type = "gaussian_gamm",
#                                                   filter_criterion = "last",
#                                                   x = "Distance",
#                                                   xlab = "Distance [km]"
# )
# ggsave(plot = p_gaum_L_Dist, device = "png", filename = "Plots/p_gaum_L_Dist.png")

# p_gaum_L_SD <- plot_predictions_across_datasets(res,
#                                                 model_type = "gaussian_gamm",
#                                                 filter_criterion = "last",
#                                                 x = "SampleDelay",
#                                                 xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gaum_L_SD, device = "png", filename = "Plots/p_gaum_L_SD.png")

# p_gaum_L_OtherEvents <- plot_predictions_across_datasets(res,
#                                                          model_type = "gaussian_gamm",
#                                                          filter_criterion = "last",
#                                                          x = "NumOtherHunts",
#                                                          xlab = "Other hunting events"
# )
# ggsave(plot = p_gaum_L_OtherEvents, device = "png", filename = "Plots/p_gaum_L_OtherEvents.png")

# p_gaum_L_Day <- plot_predictions_across_datasets(res,
#                                                  model_type = "gaussian_gam",
#                                                  filter_criterion = "last",
#                                                  x = "DefecDay",
#                                                  xlab = "Defecation day"
# )
# ggsave(plot = p_gaum_L_Day, device = "png", filename = "Plots/p_gaum_L_Day.png")


# # Define distance and timediff w.r.t. nearest hunting event within 19~50 hours
# # Plot GAMs
# p_gau_N_TD <- plot_predictions_across_datasets(res,
#                                                model_type = "gaussian_gam",
#                                                filter_criterion = "nearest",
#                                                x = "TimeDiff",
#                                                xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gau_N_TD, device = "png", filename = "Plots/p_gau_N_TD.png")

# p_gau_N_Dist <- plot_predictions_across_datasets(res,
#                                                  model_type = "gaussian_gam",
#                                                  filter_criterion = "nearest",
#                                                  x = "Distance",
#                                                  xlab = "Distance [km]"
# )
# ggsave(plot = p_gau_N_Dist, device = "png", filename = "Plots/p_gau_N_Dist.png")

# p_gau_N_SD <- plot_predictions_across_datasets(res,
#                                                model_type = "gaussian_gam",
#                                                filter_criterion = "nearest",
#                                                x = "SampleDelay",
#                                                xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gau_N_SD, device = "png", filename = "Plots/p_gau_N_SD.png")

# p_gau_N_OtherEvents <- plot_predictions_across_datasets(res,
#                                                         model_type = "gaussian_gam",
#                                                         filter_criterion = "nearest",
#                                                         x = "NumOtherHunts",
#                                                         xlab = "Other Hunting Events"
# )
# ggsave(plot = p_gau_N_OtherEvents, device = "png", filename = "Plots/p_gau_N_OtherEvents.png")

# p_gau_N_Day <- plot_predictions_across_datasets(res,
#                                                 model_type = "gaussian_gam",
#                                                 filter_criterion = "nearest",
#                                                 x = "DefecDay",
#                                                 xlab = "Defecation day"
# )
# ggsave(plot = p_gau_N_Day, device = "png", filename = "Plots/p_gau_N_Day.png")

# # Plot GAMMs
# p_gaum_N_TD <- plot_predictions_across_datasets(res,
#                                                 model_type = "gaussian_gamm",
#                                                 filter_criterion = "nearest",
#                                                 x = "TimeDiff",
#                                                 xlab = "Time difference [hours]"
# )
# ggsave(plot = p_gaum_N_TD, device = "png", filename = "Plots/p_gaum_N_TD.png")


# p_gaum_N_Dist <- plot_predictions_across_datasets(res,
#                                                   model_type = "gaussian_gamm",
#                                                   filter_criterion = "nearest",
#                                                   x = "Distance",
#                                                   xlab = "Distance [km]"
# )
# ggsave(plot = p_gaum_N_Dist, device = "png", filename = "Plots/p_gaum_N_Dist.png")

# p_gaum_N_SD <- plot_predictions_across_datasets(res,
#                                                 model_type = "gaussian_gamm",
#                                                 filter_criterion = "nearest",
#                                                 x = "SampleDelay",
#                                                 xlab = "Sample delay [hours]"
# )
# ggsave(plot = p_gaum_N_SD, device = "png", filename = "Plots/p_gaum_N_SD.png")

# p_gaum_N_OtherEvents <- plot_predictions_across_datasets(res,
#                                                          model_type = "gaussian_gamm",
#                                                          filter_criterion = "nearest",
#                                                          x = "NumOtherHunts",
#                                                          xlab = "Other Hunting Events"
# )
# ggsave(plot = p_gaum_N_OtherEvents, device = "png", filename = "Plots/p_gaum_N_OtherEvents.png")

# p_gaum_N_Day <- plot_predictions_across_datasets(res,
#                                                  model_type = "gaussian_gamm",
#                                                  filter_criterion = "nearest",
#                                                  x = "DefecDay",
#                                                  xlab = "Defecation day"
# )
# ggsave(plot = p_gaum_N_Day, device = "png", filename = "Plots/p_gaum_N_Day.png")


# # # -------------------------
# # # XGBoost Model
# # # -------------------------

# # # Set tune = TRUE if you want to run hyperparameter tuning.
# # # Leave it = FALSE if you want to use the included result of the tuning. (Models/final_xgboost..)
# # # WARNING: Setting tune = TRUE means the function will take very long (Multiple Hours+) to execute due to many computations.
# # # Max_Iterations: Only relevant if tune = TRUE

# # xg_boost_results <- XGBoost_run_default_pipeline(data_cleanedup,
# #                                                  covariables = c("TimeDiff", "Distance"),
# #                                                  tune = FALSE,
# #                                                  max_iterations = 3)
# # xg_boost_results_transformed <- XGBoost_run_transformed_pipeline(data_cleanedup,
# #                                                                  tune = FALSE,
# #                                                                  max_iterations = 3)

# # #3D Figure of Model
# # xg_boost_results$plotly_fig
# # #3D Figure of Model of transformed variables
# # xg_boost_results_transformed$plotly_fig 

