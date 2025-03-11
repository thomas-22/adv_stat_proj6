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

