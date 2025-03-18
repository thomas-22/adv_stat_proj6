# -------------------------
# sourcing files
# -------------------------
source("R/settings.R")
source("R/source.all.R")
# -------------------------
# Data fusion (see Datafusion.R)
# -------------------------
cat("Running data fusion...\n")
prepared_data <- suppressWarnings(run_datafusion(save = TRUE))
Movement <- prepared_data$Movement
FCMStress <- prepared_data$FCMStress
HuntEvents <- prepared_data$HuntEvents

# -------------------------
# Data analytics (see DataAnalysis.R)
# -------------------------
cat("Running analytics..\n")
debugonce(analytics)
analytics(data = prepared_data, method = "save")

# -------------------------
# Prepare data for modeling
# -------------------------
cat("Preparing data for modeling...\n")
param_grid <- purrr::list_rbind(list(
  # last
  data.frame(gut_retention_time_lower = 0, 
             gut_retention_time_upper = 36, 
             distance_threshold = 10, 
             filter_criterion = "Closest in time"),
  # nearest
  data.frame(gut_retention_time_lower = 0, 
             gut_retention_time_upper = 36, 
             distance_threshold = 10, 
             filter_criterion = "Nearest"),
  # score
  data.frame(gut_retention_time_lower = 0, 
             gut_retention_time_upper = 200, 
             distance_threshold = 15, 
             filter_criterion = "Highest score")
))

saveRDS(param_grid, "Data/intermediate/param_grid.RDS")

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

# -------------------------
# fitting models
# see models.R
# -------------------------
cat("Fitting Models...\n")

debugonce(fit_models)
fits <- fit_models(df = res %>% left_join(
  expand.grid(filter_criterion = c("Closest in time", "Nearest", "Highest score"), method = c("GCV.Cp", "REML"))),
  fit.fn = fit_gamm)

# -------------------------
# diagnosing models
# for the used functions, see ModelAnalysis.R
# -------------------------
cat("Saving Model diagnostics (gratia)...\n")
# plot_diagnostics_gratia(fits)
plot_diagnostics_gratia(fits, method = "save")

cat("Saving Model diagnostics (custom)...\n")
# plot_diagnostics_custom(fits)
plot_diagnostics_custom(fits, method = "save")

cat("Saving Model partial effects plots...\n")
# plot_partial_effects(fits)
plot_partial_effects(fits, method = "save")


# -------------------------
# XGBoost.
# Set `tune = TRUE`` to re-train the models and run hyperparameter tuning.
# THIS WILL TAKE A LONG TIME!!!
# Cached models are available in the Models folder.
# -------------------------
cat("Loading XGBoost models...\n")
xgboost_last <- XGBoost_run_default_pipeline(res$data[[1]],
  tune = FALSE,
  model_path = "Models/best_xgboost_model_LAST.rds"
)
# nearest
xgboost_nearest <- XGBoost_run_default_pipeline(res$data[[2]],
  tune = FALSE,
  model_path = "Models/best_xgboost_model_NEAREST.rds"
)
# score
xgboost_score <- XGBoost_run_default_pipeline(res$data[[3]],
  tune = FALSE,
  model_path = "Models/best_xgboost_model_SCORE.rds"
)

cat("Plotting prediction surfaces...\n")
p_xgboost_last <- plot_xgboost_2d(xgboost_last, res$data[[1]]) +
  ggtitle("Dataset: \"closest in time\"")
p_xgboost_nearest <- plot_xgboost_2d(xgboost_nearest, res$data[[2]]) +
  ggtitle("Dataset: \"nearest\"")
p_xgboost_score <- plot_xgboost_2d(xgboost_score, res$data[[3]]) +
  ggtitle("Dataset: \"highest score\"")
p_xgboost_combined <- (p_xgboost_last + p_xgboost_nearest + p_xgboost_score) +
  plot_layout(
    ncol = 3,
    guides = "collect",
    axes = "collect"
  )
ggsave("Figures/Models/xgboost_combined.png", p_xgboost_combined, width = 12, height = 6, dpi = 300)

cat("Generating 3D plots...\n")
p_xgboost_3d_last <- plot_xgboost_3d(xgboost_last, res$data[[1]])
p_xgboost_3d_nearest <- plot_xgboost_3d(xgboost_nearest, res$data[[2]])
p_xgboost_3d_score <- plot_xgboost_3d(xgboost_score, res$data[[3]])

# Comparison regarding goodness of fit
cat("Running comparison...\n")
rmse_gamm <- fits %>%
  select(filter_criterion, method, fit) %>%
  mutate(
    rmse = purrr::map_dbl(fit, function(model) {
      rsd <- residuals(model, type = "response")
      sqrt(mean(rsd^2))
    }),
  ) %>%
  select(-fit) %>%
  mutate(method = paste("GAMM", method, sep = "-"))
rmse_xgboost <- res %>%
  # Grab datasets from GAMM results
  select(filter_criterion, data) %>%
  mutate(models = list(xgboost_last, xgboost_nearest, xgboost_score)) %>%
  mutate(
    rmse = purrr::map2_dbl(data, models, function(data, model) {
      calculate_rmse_full_data(model, data)
    }),
  ) %>%
  select(filter_criterion, rmse) %>%
  mutate(method = "XGBoost")
rmse_all <- rbind(rmse_gamm, rmse_xgboost) %>%
  arrange(filter_criterion)
# # change "last" to "closest in time" for report
# rmse_all$filter_criterion[rmse_all$filter_criterion == "last"] <- "closest in time"
# save table
saveRDS(rmse_all, "Data/processed/ComparisonRMSE.RDS")
