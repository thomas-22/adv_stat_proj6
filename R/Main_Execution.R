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
library(ggrepel)

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
source("R/XGBoost_Model.R")

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

deer_order <- FCMStress %>%
  group_by(Deer.ID) %>%
  filter(DefecTime == max(DefecTime)) %>%
  ungroup() %>%
  arrange(DefecTime) %>%
  distinct(Deer.ID) %>%
  pull(Deer.ID)

p_fcm_sample_times <- ggplot(FCMStress) +
  geom_line(aes(x = DefecTime, y = Deer.ID)) +
  geom_point(aes(x = DefecTime, y = Deer.ID)) +
  labs(x = "Defecation time", y = "Deer") +
  scale_y_discrete(limits = rev(deer_order)) +
  theme_bw(base_size = 16) +
  theme(axis.text.y = element_blank())
# ggsave("Figures/p_fcm_sample_times.svg", p_fcm_sample_times, width = 8, height = 5, dpi = 300)

p_fcm_levels <- ggplot(FCMStress) +
  geom_boxplot(aes(x = Deer.ID, y = ng_g)) +
  labs(x = "Deer", y = "FCM level [ng/g]") +
  scale_x_discrete(limits = deer_order) +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank())
# ggsave("Figures/p_fcm_levels.svg", p_fcm_levels, width = 8, height = 5, dpi = 300)

# Plot the number of FCM samples and hunting events over time
p_fcm_dates <- FCMStress %>%
  mutate(nSamples = n(), .by = "DefecDate") %>%
  ggplot() +
  geom_segment(aes(x = DefecDate, y = nSamples, yend = 0), color = "purple") +
  labs(x = "", y = "Count", title = "Daily Count of FCM Samples") +
  scale_x_date(
    date_breaks = "6 months",
    limits = as_date(c("2020-04-01", "2022-12-01"))
  ) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_bw(base_size = 16)
p_hunt_dates <- HuntEvents %>% na.omit() %>%
  mutate(nHunts = n(), .by = "HuntDate") %>%
  ggplot() +
  geom_segment(aes(x = HuntDate, y = nHunts, yend = 0), color = "red") +
  labs(x = "", y = "Count", title = "Daily Count of Hunting Events") +
  scale_x_date(
    date_breaks = "6 months",
    limits = as_date(c("2020-04-01", "2022-12-01"))
  ) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_bw(base_size = 16)
p_fcm_hunt_dates <- p_fcm_dates / p_hunt_dates +
  plot_layout(axis_titles = "collect")
# ggsave("Figures/p_fcm_hunt_dates.png", p_fcm_hunt_dates, width = 10, height = 6, dpi = 300)

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

# Plot number of FCM samples and hunting events over time
p_fcm_dates <- FCMStress %>%
  summarise(nSamples = n(), .by = "DefecDate") %>%
  ggplot() +
  geom_segment(aes(x = DefecDate, y = nSamples, yend = 0), color = "purple") +
  labs(x = "", y = "Count", title = "Daily Count of Faecel Samples") +
  scale_x_date(
    date_breaks = "6 months",
    limits = as_date(c("2020-04-01", "2022-12-01"))
  ) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_bw(base_size = 16)
p_hunt_dates <- HuntEvents %>% drop_na() %>%
  summarise(nHunts = n(), .by = "HuntDate") %>%
  ggplot() +
  geom_segment(aes(x = HuntDate, y = nHunts, yend = 0), color = "red") +
  labs(x = "", y = "Count", title = "Daily Count of Hunting Events") +
  scale_x_date(
    date_breaks = "6 months",
    limits = as_date(c("2020-04-01", "2022-12-01"))
  ) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_bw(base_size = 16)
p_fcm_hunt_dates <- p_fcm_dates / p_hunt_dates
ggsave("Figures/p_fcm_hunt_dates.png", p_fcm_hunt_dates, width = 10, height = 6, dpi = 300)

# -------------------------
# Prepare data for modeling
# -------------------------
cat("Preparing data for modeling...\n")
param_grid <- list_rbind(list(
  # last
  data.frame(gut_retention_time_lower = 0, gut_retention_time_upper = 36, distance_threshold = 10, filter_criterion = "last"),
  # nearest
  data.frame(gut_retention_time_lower = 0, gut_retention_time_upper = 36, distance_threshold = 10, filter_criterion = "nearest"),
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

p_interpolation <- Draw_Illustration_Map()
ggsave("Figures/p_interpolation.png", p_interpolation, width = 9, height = 5, dpi = 300)

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
      NumOtherHunts,
    data = data,
    family = family
  )
}

fit_gamm <- function(data, family = gaussian(), method = "GCV.Cp") {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr", k = 20) + s(Distance, bs = "cr", k = 10) +
      s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
      NumOtherHunts + s(Deer.ID, bs = "re"),
    data = data,
    family = family,
    method = method
  )
}

# fit_gamm_interact <- function(data, family = gaussian()) {
#   gam(
#     ng_g ~ te(TimeDiff, Distance, k = 20) +
#       s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 10) +
#       hasCalf + NumOtherHunts + s(Deer.ID, bs = "re"),
#     data = data,
#     family = family
#   )
# }

# fit_gam_tp <- function(data, family = gaussian()) {
#   gam(
#     ng_g ~ s(TimeDiff, bs = "ps") + s(DistanceX, DistanceY, bs = "tp") + s(SampleDelay, bs = "ps") +
#       hasCalf + NumOtherHunts + s(DefecDay, bs = "ps"),
#     # random = list(Deer.ID = ~1, DefecMonth = ~1),
#     data = data,
#     family = family
#   )
# }

cat("Fitting models...\n")

# -------------------------
# Last
m_L <- fit_gamm(res$data[[1]], family = Gamma(link = "log"), method = "GCV.Cp")
draw(m_L, select = 5)

p_L_diagnostic <- appraise(m_L, method = "simulate") & theme_bw()
ggsave("Figures/p_L_diagnostic.png", p_L_diagnostic, width = 7, height = 7, dpi = 300)

p_L_TimeDiff <- ggpredict(m_L, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))
# Predicted values of the FCM level for a "typical deer", i.e.,
# the other covariates are held constant at their mean values or reference category
# (hasCalf = FALSE).
# (Random effect is set to that of one deer, but since it's just an intercept,
# it is irrelevant to our interpretation of the overall shape/tendency of the curve.)

p_L_Distance <- ggpredict(m_L, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))

p_L_SampleDelay <- ggpredict(m_L, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))

p_L_Day <- ggpredict(m_L, terms = c("DefecDay"), title = "") %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "") +
  theme_bw(base_size = 16)

p_L <- p_L_TimeDiff + p_L_Distance + p_L_SampleDelay +
  plot_layout(ncol = 3, axis_titles = "collect")
ggsave("Figures/p_L.png", p_L, width = 12, height = 6, dpi = 300)

# -------------------------

# -------------------------
# Nearest
m_N <- fit_gamm(res$data[[2]], family = Gamma(link = "log"), method = "GCV.Cp")
draw(m_N, select = 5)

p_N_diagnostic <- appraise(m_N, method = "simulate") & theme_bw()
# ggsave("Figures/p_N_diagnostic.png", p_N_diagnostic, width = 7, height = 7, dpi = 300)

p_N_TimeDiff <- ggpredict(m_N, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))

p_N_Distance <- ggpredict(m_N, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))
# ggsave("Figures/p_N_Distance.png", p_N_Distance, width = 12, height = 6, dpi = 300)

p_N_SampleDelay <- ggpredict(m_N, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))

p_N_Day <- ggpredict(m_N, terms = c("DefecDay"), title = "") %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))

p_N <- p_N_TimeDiff + p_N_Distance + p_N_SampleDelay +
  plot_layout(ncol = 3, axis_titles = "collect")
ggsave("Figures/p_N.png", p_N, width = 12, height = 6, dpi = 300)

# -------------------------
# Score
m_S <- fit_gamm(res$data[[3]], family = Gamma(link = "log"), method = "GCV.Cp")
draw(m_S, select = 5)
# saveRDS(m_S, "Models/m_S.RDS")

p_S_diagnostic <- appraise(m_S, method = "simulate") & theme_bw()
ggsave("Figures/p_S_diagnostic.png", p_S_diagnostic, width = 7, height = 7, dpi = 300)

p_S_TimeDiff <- ggpredict(m_S, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))

p_S_Distance <- ggpredict(m_S, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))

p_S_SampleDelay <- ggpredict(m_S, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600))
ggsave("Figures/p_S_SampleDelay.png", p_S_SampleDelay, width = 15, height =15, dpi = 300)

p_S_Day <- ggpredict(m_S, terms = c("DefecDay"), title = "") %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "") +
  theme_bw(base_size = 16)

p_S <- p_S_TimeDiff + p_S_Distance + p_S_SampleDelay +
  plot_layout(ncol = 3, axis_titles = "collect")
ggsave("Figures/p_S.png", p_S, width = 12, height = 6, dpi = 300)

# -------------------------
# Examples for presentation

m_L <- fit_gamm(res$data[[1]], family = Gamma(link = "log"), method = "GCV.Cp")
m_N <- fit_gamm(res$data[[2]], family = Gamma(link = "log"), method = "GCV.Cp")
m_S <- fit_gamm(res$data[[3]], family = Gamma(link = "log"), method = "GCV.Cp")

p_L_re_gcv <- draw(m_L, select = 5) +
  theme_bw(base_size = 16) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_re_gcv <- draw(m_N, select = 5) +
  theme_bw(base_size = 16) +
  ggtitle("Dataset \"Nearest\"")
p_S_re_gcv <- draw(m_S, select = 5) +
  theme_bw(base_size = 16) +
  ggtitle("Dataset \"Highest Score\"")
p_re_gcv <- p_L_re_gcv + p_N_re_gcv + p_S_re_gcv +
  plot_layout(ncol = 3, axis_titles = "collect") +
  plot_annotation(
    caption = "Method = GCV",
    theme = theme(text = element_text(size = 16))
  )
ggsave("Figures/p_re_gcv.png", p_re_gcv, width = 12, height = 6, dpi = 300)

m_L_reml <- fit_gamm(res$data[[1]], family = Gamma(link = "log"), method = "REML")
m_N_reml <- fit_gamm(res$data[[2]], family = Gamma(link = "log"), method = "REML")
m_S_reml <- fit_gamm(res$data[[3]], family = Gamma(link = "log"), method = "REML")

p_L_re_reml <- draw(m_L_reml, select = 5) +
  theme_bw(base_size = 16) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_re_reml <- draw(m_N_reml, select = 5) +
  theme_bw(base_size = 16) +
  ggtitle("Dataset \"Nearest\"")
p_S_re_reml <- draw(m_S_reml, select = 5) +
  theme_bw(base_size = 16) +
  ggtitle("Dataset \"Highest Score\"")
p_re_reml <- p_L_re_reml + p_N_re_reml + p_S_re_reml +
  plot_layout(ncol = 3, axis_titles = "collect") +
  plot_annotation(
    caption = "Method = REML",
    theme = theme(text = element_text(size = 16))
  )
ggsave("Figures/p_re_reml.png", p_re_reml, width = 12, height = 6, dpi = 300)

# -------------------------
p_L_TimeDiff_gcv <- ggpredict(m_L, terms = c("TimeDiff")) %>% plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 500)) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_TimeDiff_gcv <- ggpredict(m_N, terms = c("TimeDiff")) %>% plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 500)) +
  ggtitle("Dataset \"Nearest\"")
p_S_TimeDiff_gcv <- ggpredict(m_S, terms = c("TimeDiff")) %>% plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 500)) +
  ggtitle("Dataset \"Highest Score\"")
ggsave(
  "Figures/p_TimeDiff_gcv.png",
  p_L_TimeDiff_gcv + p_N_TimeDiff_gcv + p_S_TimeDiff_gcv +
    plot_annotation(
      caption = "Method = GCV",
      theme = theme(text = element_text(size = 16))
    ) +
    plot_layout(axis_titles = "collect"),
  width = 12, height = 6, dpi = 300
)

p_L_SampleDelay_reml <- ggpredict(m_L_reml, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600)) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_SampleDelay_reml <- ggpredict(m_N_reml, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600)) +
  ggtitle("Dataset \"Nearest\"")
p_S_SampleDelay_reml <- ggpredict(m_S_reml, terms = c("SampleDelay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(150, 600)) +
  ggtitle("Dataset \"Highest Score\"")
p_SampleDelay_reml <- p_L_SampleDelay_reml + p_N_SampleDelay_reml + p_S_SampleDelay_reml +
  plot_annotation(
    caption = "Method = REML",
    theme = theme(text = element_text(size = 16))
  ) +
  plot_layout(axis_titles = "collect")
ggsave(
  "Figures/p_SampleDelay_reml.png",
  p_SampleDelay_reml,
  width = 12, height = 6, dpi = 300
)

p_L_TimeDiff_reml <- ggpredict(m_L_reml, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_TimeDiff_reml <- ggpredict(m_N_reml, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Nearest\"")
p_S_TimeDiff_reml <- ggpredict(m_S_reml, terms = c("TimeDiff")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Highest Score\"")
p_TimeDiff_reml <- p_L_TimeDiff_reml + p_N_TimeDiff_reml + p_S_TimeDiff_reml +
  plot_annotation(
    caption = "Method = REML",
    theme = theme(text = element_text(size = 16))
  ) +
  plot_layout(axis_titles = "collect")
ggsave(
  "Figures/p_TimeDiff_reml.png",
  p_TimeDiff_reml,
  width = 12, height = 6, dpi = 300
)

p_L_Distance_reml <- ggpredict(m_L_reml, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_Distance_reml <- ggpredict(m_N_reml, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Nearest\"")
p_S_Distance_reml <- ggpredict(m_S_reml, terms = c("Distance")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Highest Score\"")
p_Distance_reml <- p_L_Distance_reml + p_N_Distance_reml + p_S_Distance_reml +
  plot_annotation(
    caption = "Method = REML",
    theme = theme(text = element_text(size = 16))
  ) +
  plot_layout(axis_titles = "collect")
ggsave(
  "Figures/p_Distance_reml.png",
  p_Distance_reml,
  width = 12, height = 6, dpi = 300
)

p_L_DefecDay_reml <- ggpredict(m_L_reml, terms = c("DefecDay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_DefecDay_reml <- ggpredict(m_N_reml, terms = c("DefecDay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Nearest\"")
p_S_DefecDay_reml <- ggpredict(m_S_reml, terms = c("DefecDay")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Defecation day", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Highest Score\"")
ggsave(
  "Figures/p_DefecDay_reml.png",
  p_L_DefecDay_reml + p_N_DefecDay_reml + p_S_DefecDay_reml +
    plot_annotation(
      caption = "Method = REML",
      theme = theme(text = element_text(size = 16))
    ) +
    plot_layout(axis_titles = "collect"),
  width = 12, height = 6, dpi = 300
)

p_L_OtherHunts_reml <- ggpredict(m_L_reml, terms = c("NumOtherHunts")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Number of other relevant hunting events", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Closest in Time\"")
p_N_OtherHunts_reml <- ggpredict(m_N_reml, terms = c("NumOtherHunts")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Number of other relevant hunting events", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Nearest\"")
p_S_OtherHunts_reml <- ggpredict(m_S_reml, terms = c("NumOtherHunts")) %>%
  plot() +
  labs(y = "FCM level [ng/g]", x = "Number of other relevant hunting events", title = "") +
  theme_bw(base_size = 16) +
  coord_cartesian(ylim = c(200, 500)) +
  ggtitle("Dataset \"Highest Score\"")
ggsave(
  "Figures/p_OtherHunts_reml.png",
  p_L_OtherHunts_reml + p_N_OtherHunts_reml + p_S_OtherHunts_reml +
    plot_annotation(
      caption = "Method = REML",
      theme = theme(text = element_text(size = 16))
    ) +
    plot_layout(axis_titles = "collect"),
  width = 12, height = 6, dpi = 300
)

# Fixed effect table
table_otherHunts <- data.frame(
  Method = c("REML", "REML", "REML", "GCV", "GCV", "GCV"),
  Dataset = rep(c("Closest in Time", "Nearest", "Highest Score"), 2),
  Estimate = c(
    coef(m_L_reml)["NumOtherHunts"],
    coef(m_N_reml)["NumOtherHunts"],
    coef(m_S_reml)["NumOtherHunts"],
    coef(m_L)["NumOtherHunts"],
    coef(m_N)["NumOtherHunts"],
    coef(m_S)["NumOtherHunts"]
  ),
  `exp(Estimate)` = c(
    exp(coef(m_L_reml)["NumOtherHunts"]),
    exp(coef(m_N_reml)["NumOtherHunts"]),
    exp(coef(m_S_reml)["NumOtherHunts"]),
    exp(coef(m_L)["NumOtherHunts"]),
    exp(coef(m_N)["NumOtherHunts"]),
    exp(coef(m_S)["NumOtherHunts"])
  ),
  `Standard error` = c(
    summary(m_L_reml)$p.table["NumOtherHunts", "Std. Error"],
    summary(m_N_reml)$p.table["NumOtherHunts", "Std. Error"],
    summary(m_S_reml)$p.table["NumOtherHunts", "Std. Error"],
    summary(m_L)$p.table["NumOtherHunts", "Std. Error"],
    summary(m_N)$p.table["NumOtherHunts", "Std. Error"],
    summary(m_S)$p.table["NumOtherHunts", "Std. Error"]
  )
)
saveRDS(table_otherHunts, "Data/table_otherHunts.RDS")

# Instability w.r.t. estimation methods
p_L_TimeDiff_gcv_instable <- p_L_TimeDiff_gcv +
  ggtitle("Dataset \"Closest in Time\"; Method = GCV")
p_L_TimeDiff_reml_instable <- p_L_TimeDiff_reml +
  ggtitle("Dataset \"Closest in Time\"; Method = REML")
ggsave(
  "Figures/p_instable.png",
  p_L_TimeDiff_gcv_instable + p_L_TimeDiff_reml_instable +
    plot_layout(axis_titles = "collect"),
  width = 12, height = 6, dpi = 300
)


# -------------------------
# Save coefficient tables

extract_coefficients <- function(model, label) {
  coeff_summary <- summary(model)$p.table
  data.frame(
    Dataset = label,
    Term = rownames(coeff_summary),
    Estimate = coeff_summary[, "Estimate"],
    Std_Error = coeff_summary[, "Std. Error"],
    stringsAsFactors = FALSE
  )
}

coeff_closest <- extract_coefficients(m_L, "Closest in Time")
coeff_nearest <- extract_coefficients(m_N, "Nearest")
coeff_highest <- extract_coefficients(m_S, "Highest Score")

rownames(coeff_closest) <- NULL
rownames(coeff_nearest) <- NULL
rownames(coeff_highest) <- NULL
saveRDS(coeff_closest, "Data/coeff_closest.RDS")
saveRDS(coeff_nearest, "Data/coeff_nearest.RDS")
saveRDS(coeff_highest, "Data/coeff_highest.RDS")

# -------------------------

# -------------------------

# Create a structured data frame for model results summary
model_results <- data.frame(
  Category = c(
    "Diagnostics", "Diagnostics", "Diagnostics", "Diagnostics",
    "Random Effects", "Random Effects",
    "Linear Effects"
  ),
  Subcategory = c(
    "QQ Plot", "Residuals vs Predictor", "Histogram", "Observed vs Fitted",
    "Time & Space Effects", "Sample Delay",
    "other hunting events"
  ),
  Description = c(
    "Residuals mostly follow expected distribution",
    "No major pattern",
    "Reasonable fit, some variance",
    "Moderate spread, some unexplained variance",
    "Weak or inconsistent",
    "Shows some effect",
    "No significant impact"
  )
)
saveRDS(model_results, "Data/model_result.RDS")

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
# # # Max_Iterations: Max iterations for the readjustment (setting new param grid to explore) for the Hyperparameter Tuning process.
# # # Max_Iterations is only relevant if tune = TRUE.

# Recommended values: max_iterations = 3, num_runs = 5

# # LAST Dataset
# pipeline_results_last <- run_multiple_xgboost_pipelines_aggregated(
#   data_cleanedup = res$data[[1]],
#   covariables = c("TimeDiff", "Distance"),
#   tune = TRUE,
#   base_model_path = "Models/xgboost_model_last",
#   max_iterations = 1,
#   num_runs = 5,
#   seed = NULL
# )
# 
# 
# # NEAREST Dataset
# pipeline_results_nearest <- run_multiple_xgboost_pipelines_aggregated(
#   data_cleanedup = res$data[[2]],
#   covariables = c("TimeDiff", "Distance"),
#   tune = TRUE,
#   base_model_path = "Models/xgboost_model_nearest",
#   max_iterations = 1,
#   num_runs = 5,
#   seed = NULL
# )
# 
# # SCORE Dataset
# pipeline_results_score <- run_multiple_xgboost_pipelines_aggregated(
#   data_cleanedup = res$data[[3]],
#   covariables = c("TimeDiff", "Distance"),
#   tune = TRUE,
#   base_model_path = "Models/xgboost_model_score",
#   max_iterations = 1,
#   num_runs = 5,
#   seed = NULL
# )

# xgboost_model_last_all_runs <- read_rds("Models/xgboost_model_last_all_runs.rds")
# xgboost_model_nearest_all_runs <- read_rds("Models/xgboost_model_nearest_all_runs.rds")
# xgboost_model_score_all_runs <- read_rds("Models/xgboost_model_score_all_runs.rds")
# 
# 
# xg_bestmodel_last <- xgboost_model_last_all_runs[[
#   which.min(
#     sapply(xgboost_model_last_all_runs, function(model) {
#       if (length(model$evaluation_log$train_rmse) > 0) {
#         min(model$evaluation_log$train_rmse)
#       } else {
#         Inf
#       }
#     })
#   )
# ]]
# 
# xg_bestmodel_nearest <- xgboost_model_nearest_all_runs[[
#   which.min(
#     sapply(xgboost_model_nearest_all_runs, function(model) {
#       if (length(model$evaluation_log$train_rmse) > 0) {
#         min(model$evaluation_log$train_rmse)
#       } else {
#         Inf
#       }
#     })
#   )
# ]]
# 
# xg_bestmodel_score <- xgboost_model_score_all_runs[[
#   which.min(
#     sapply(xgboost_model_score_all_runs, function(model) {
#       if (length(model$evaluation_log$train_rmse) > 0) {
#         min(model$evaluation_log$train_rmse)
#       } else {
#         Inf
#       }
#     })
#   )
# ]]
# 
# xg_bestmodel_last$best_params <- xg_bestmodel_last$final_model$params
# xg_bestmodel_last$best_nrounds <- as.integer(13)
# xg_bestmodel_last$best_params <- modifyList(xg_bestmodel_last$best_params, list(validate_parameters = NULL))
# xg_bestmodel_last <- modifyList(xg_bestmodel_last, list(comparisons = NULL,
#                                                         plotly_fig = NULL))
# 
# 
# xg_bestmodel_nearest$best_params <- xg_bestmodel_nearest$final_model$params
# xg_bestmodel_nearest$best_nrounds <- as.integer(13)
# xg_bestmodel_nearest$best_params <- modifyList(xg_bestmodel_nearest$best_params, list(validate_parameters = NULL))
# xg_bestmodel_nearest <- modifyList(xg_bestmodel_nearest, list(comparisons = NULL,
#                                                         plotly_fig = NULL))
# 
# xg_bestmodel_score$best_params <- xg_bestmodel_score$final_model$params
# xg_bestmodel_score$best_nrounds <- as.integer(13)
# xg_bestmodel_score$best_params <- modifyList(xg_bestmodel_score$best_params, list(validate_parameters = NULL))
# xg_bestmodel_score <- modifyList(xg_bestmodel_score, list(comparisons = NULL,
#                                                         plotly_fig = NULL))
# 
# 
# 
# saveRDS(xg_bestmodel_last, "Models/best_xgboost_model_LAST.rds")
# saveRDS(xg_bestmodel_nearest, "Models/best_xgboost_model_NEAREST.rds")
# saveRDS(xg_bestmodel_score, "Models/best_xgboost_model_SCORE.rds")


#-----------------------------------
# USE PREVIOUSLY CALCULATED MODELS:

# LAST Dataset
final_pipeline_results_last <- run_multiple_xgboost_pipelines_aggregated(
  data_cleanedup = res$data[[1]],
  covariables = c("TimeDiff", "Distance"),
  tune = FALSE,
  base_model_path = "Models/best_xgboost_model_LAST",
  max_iterations = 1,
  num_runs = 40,
  seed = NULL
)
final_pipeline_results_last$XXX$Y_real <- res$data[[1]]$ng_g
# calculate_rmse(actual = final_pipeline_results_last$XXX$Y_real,
#                predicted = final_pipeline_results_last$XXX$Y_pred)


# NEAREST Dataset
final_pipeline_results_nearest <- run_multiple_xgboost_pipelines_aggregated(
  data_cleanedup = res$data[[2]],
  covariables = c("TimeDiff", "Distance"),
  tune = FALSE,
  base_model_path = "Models/best_xgboost_model_NEAREST",
  max_iterations = 1,
  num_runs = 40,
  seed = NULL
)
final_pipeline_results_nearest$XXX$Y_real <- res$data[[2]]$ng_g
# calculate_rmse(actual = final_pipeline_results_nearest$XXX$Y_real,
#                predicted = final_pipeline_results_nearest$XXX$Y_pred)

# SCORE Dataset
final_pipeline_results_score <- run_multiple_xgboost_pipelines_aggregated(
  data_cleanedup = res$data[[3]],
  covariables = c("TimeDiff", "Distance"),
  tune = FALSE,
  base_model_path = "Models/best_xgboost_model_SCORE",
  max_iterations = 1,
  num_runs = 40,
  seed = NULL
)
final_pipeline_results_score$XXX$Y_real <- res$data[[3]]$ng_g
# calculate_rmse(actual = final_pipeline_results_score$XXX$Y_real,
#                predicted = final_pipeline_results_score$XXX$Y_pred)



aggregated_last <- final_pipeline_results_last$aggregated_results
aggregated_nearest <- final_pipeline_results_nearest$aggregated_results
aggregated_score <- final_pipeline_results_score$aggregated_results

# Compile the XGBoost Summary Table
xgboost_summary <- data.frame(
  Dataset = c("last", "nearest", "score"),
  Mean_RMSE = c(
    aggregated_last$mean_rmse_test_final,
    aggregated_nearest$mean_rmse_test_final,
    aggregated_score$mean_rmse_test_final
  ),
  SD_RMSE = c(
    aggregated_last$sd_rmse_test_final,
    aggregated_nearest$sd_rmse_test_final,
    aggregated_score$sd_rmse_test_final
  ),
  Number_of_Observations = c(
    nrow(res$data[[1]]),
    nrow(res$data[[2]]),
    nrow(res$data[[3]])
  )
)




plot_xgboost_last <- plot_predicted_vs_actual(Y_pred = final_pipeline_results_last$XXX$Y_pred,
                                              Y_actual = final_pipeline_results_last$XXX$Y_real,
                                              RMSE = aggregated_last$mean_rmse_test_final,
                                              Text = "\nDataset: Last,")

plot_xgboost_nearest <- plot_predicted_vs_actual(Y_pred = final_pipeline_results_nearest$XXX$Y_pred,
                                                 Y_actual = final_pipeline_results_nearest$XXX$Y_real,
                                                 RMSE = aggregated_nearest$mean_rmse_test_final,
                                                 Text = "\nDataset: Closest,")

plot_xgboost_score <- plot_predicted_vs_actual(Y_pred = final_pipeline_results_score$XXX$Y_pred,
                                               Y_actual = final_pipeline_results_score$XXX$Y_real,
                                               RMSE = aggregated_score$mean_rmse_test_final,
                                               Text = "\nDataset: Score,")

ggsave(filename = "Figures/XGBoost_PredVSActual_last.jpg", plot = plot_xgboost_last, device = "jpeg",
       width = 6, height = 6, units = "in", dpi = 500)
ggsave(filename = "Figures/XGBoost_PredVSActual_nearest.jpg", plot = plot_xgboost_nearest, device = "jpeg",
       width = 6, height = 6, units = "in", dpi = 500)
ggsave(filename = "Figures/XGBoost_PredVSActual_score.jpg", plot = plot_xgboost_score, device = "jpeg",
       width = 6, height = 6, units = "in", dpi = 500)


model_last <- read_rds("Models/best_xgboost_model_LAST.rds")$best_params
model_nearest <- read_rds("Models/best_xgboost_model_NEAREST.rds")$best_params
model_score <- read_rds("Models/best_xgboost_model_SCORE.rds")$best_params


models_list <- list(
  Last = model_last,
  Nearest = model_nearest,
  Score = model_score
)
models_df <- bind_rows(models_list, .id = "Model")
models_df <- models_df %>%
  mutate(
    eta = round(eta, 4),
    gamma = round(gamma, 3),
    subsample = round(subsample, 4),
    colsample_bytree = round(colsample_bytree, 4),
    min_child_weight = round(min_child_weight, 3)
  )
models_df
xgboost_summary_df <- bind_rows(xgboost_summary)


xgboost_summary_df$Dataset <- tolower(xgboost_summary_df$Dataset)
models_df$Model <- tolower(models_df$Model)

combined_df <- merge(models_df, xgboost_summary_df, by.x = "Model", by.y = "Dataset")




write.csv(combined_df, "Models/xgboost_models_overview.csv", row.names = FALSE)


#-----------------------------------


# # #3D Figure of Model
# # xg_boost_results$plotly_fig
# # #3D Figure of Model of transformed variables
# # xg_boost_results_transformed$plotly_fig 

