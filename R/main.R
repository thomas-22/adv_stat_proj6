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
fits <- fit_models(df = res %>% left_join(
  expand.grid(filter_criterion = c("last", "nearest", "score"), method = c("GCV.Cp", "REML"))),
  fit.fn = fit_gamm)

# -------------------------
# diagnosing models
# for the used functions, see ModelAnalysis.R
# -------------------------
cat("saving Model diagnostics (gratia)...\n")
# plot_diagnostics_gratia(fits)
plot_diagnostics_gratia(fits, method = "save")

cat("saving Model diagnostics (custom)...\n")
# plot_diagnostics_custom(fits)
plot_diagnostics_custom(fits, method = "save")

cat("saving Model partial effects plots...\n")
# plot_partial_effects(fits)
plot_partial_effects(fits, method = "save")

