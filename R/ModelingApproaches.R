library(xgboost)
library(caTools)
library(ggplot2)
if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
  install.packages("fitdistrplus")
}
library(fitdistrplus)
library(plotly)

# Prepare the data
modeling_data <- fcm_specific %>%
  filter(ng_g > 0)

# Initialize tuning parameters
initialize_tuning <- FALSE #SET TO TRUE IF RUNNING FIRST TIME
tuning_converge <- FALSE
tuning_iterations <- 0
best_rmse <- 0

rmse_converge_tolerance <- 0.01


while (!tuning_converge) {
  # Define hyperparameter grid
  if (initialize_tuning) {
    param_grid <- expand.grid(
      max_depth = c(2, 3, 4),
      eta = c(0.15, 0.155, 0.16, 0.165, 0.17),
      gamma = c(5.9, 6, 6.1),
      subsample = c(0.4, 0.5, 0.6),
      colsample_bytree = c(1),
      min_child_weight = c(4.25, 4.5, 4.75)
    )
  } else {
    param_grid <- expand.grid(
      max_depth = ifelse(c(best_params$max_depth - 1, best_params$max_depth, best_params$max_depth + 1) < 2,
                         2,
                         c(best_params$max_depth - 1, best_params$max_depth, best_params$max_depth + 1)),
      eta = ifelse(c(best_params$eta - 0.01, best_params$eta, best_params$eta + 0.01) < 0.01,
                   0.01,
                   c(best_params$eta - 0.01, best_params$eta, best_params$eta + 0.01)),
      gamma = ifelse(c(best_params$gamma - 0.1, best_params$gamma, best_params$gamma + 0.1) < 0,
                     c(1,2),
                     c(best_params$gamma - 0.1, best_params$gamma, best_params$gamma + 0.1)),
      subsample = ifelse(
        best_params$subsample < 0.5,
        0.5,
        ifelse(
          best_params$subsample + 0.01 > 1,
          c(1),
          c(best_params$subsample - 0.01, best_params$subsample, best_params$subsample + 0.01)
        )
      ),
      colsample_bytree = ifelse(c(best_params$colsample_bytree - 0.01, best_params$colsample_bytree, best_params$colsample_bytree + 0.01) < 0.5,
                                0.5,
                                ifelse(c(best_params$colsample_bytree - 0.01, best_params$colsample_bytree, best_params$colsample_bytree + 0.01) > 1,
                                       1,
                                       c(best_params$colsample_bytree - 0.01, best_params$colsample_bytree, best_params$colsample_bytree + 0.01))),
      min_child_weight = ifelse(c(best_params$min_child_weight - 0.1, best_params$min_child_weight, best_params$min_child_weight + 0.1) < 0.1,
                                0.1,
                                c(best_params$min_child_weight - 0.1, best_params$min_child_weight, best_params$min_child_weight + 0.1))
    )
  }
  
  # Reset best RMSE for this iteration
  iteration_best_rmse <- Inf
  
  # Total combinations in grid
  total_combinations <- nrow(param_grid)
  
  # Perform grid search
  for (i in seq_len(total_combinations)) {
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = param_grid$max_depth[i],
      eta = param_grid$eta[i],
      gamma = param_grid$gamma[i],
      subsample = param_grid$subsample[i],
      colsample_bytree = param_grid$colsample_bytree[i],
      min_child_weight = param_grid$min_child_weight[i]
    )
    
    cat(sprintf("Trying combination %d of %d: max_depth=%d, eta=%.2f, gamma=%.1f, subsample=%.2f, colsample_bytree=%.2f, min_child_weight=%.1f, Tuning Iterations=%d\n",
                i, total_combinations,
                param_grid$max_depth[i],
                param_grid$eta[i],
                param_grid$gamma[i],
                param_grid$subsample[i],
                param_grid$colsample_bytree[i],
                param_grid$min_child_weight[i],
                tuning_iterations))
    
    # Perform cross-validation
    cv_results <- xgb.cv(
      params = params,
      data = X_train,
      label = y_train,
      nrounds = 10000,
      nfold = 10,
      verbose = 0,
      early_stopping_rounds = 30
    )
    
    # Get metrics
    mean_rmse <- min(cv_results$evaluation_log$test_rmse_mean)
    best_iter <- cv_results$best_iteration
    
    
    
    # Update best parameters for this iteration
    if (mean_rmse < iteration_best_rmse) {
      iteration_best_rmse <- mean_rmse
      best_params <- params
      best_iteration <- best_iter
    }
    
    # Log current results
    log_results <- rbind(log_results, data.frame(
      combination = i,
      max_depth = param_grid$max_depth[i],
      eta = param_grid$eta[i],
      gamma = param_grid$gamma[i],
      subsample = param_grid$subsample[i],
      colsample_bytree = param_grid$colsample_bytree[i],
      min_child_weight = param_grid$min_child_weight[i],
      rmse = mean_rmse,
      best_iteration = best_iter
    ))
  }
  
  # Train the final model using the best parameters from this iteration
  final_model <- xgboost(
    params = best_params,
    data = X_train,
    label = y_train,
    nrounds = best_iteration,
    verbose = 1
  )
  
  # Make predictions
  y_pred <- predict(final_model, X_test)
  final_rmse <- sqrt(mean((y_test - y_pred)^2))
  
  # Check convergence
  if (abs(iteration_best_rmse - best_rmse) / best_rmse < rmse_converge_tolerance) {
    tuning_converge <- TRUE
  } else {
    best_rmse <- iteration_best_rmse
  }
  
  # After the grid search in each iteration, check the RMSE improvement
  cat("Iteration Best RMSE:", iteration_best_rmse, "\n")
  cat("Current Best RMSE:", best_rmse, "\n")
  
  tuning_iterations <- tuning_iterations + 1
  initialize_tuning <- FALSE
}

# Save results
write.csv(log_results, "hyperparameter_tuning_log.csv", row.names = FALSE)
saveRDS(final_model, "final_xgboost_model.rds")


# Plot the results
ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
  geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
  xlim(0, 1500) +
  ylim(0, 1500) +
  geom_point(size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "XGBoost: Predicted vs Actual, ng_g, After Grid Search Tuning", x = "Actual", y = "Predicted") +
  theme_light() +
  coord_fixed(ratio = 1) +
  annotate("text", x = 500, y = Inf, hjust = 1, vjust = 2.5,
           label = paste("RMSE:", round(final_rmse, 2)),
           color = "black", size = 3.5)
final_model

#Work on Visualising the Model:
# Create sequences
time_diff <- seq(1, 7500, by = 10)
distance <- seq(1, 7500, by = 10)

# Generate all combinations
combinations <- expand.grid(TimeDiff = time_diff, Distance = distance)


# Convert the data frame to a matrix (which has 2 columns)
combinations <- as.matrix(combinations)

visual_y_pred <- predict(final_model, combinations)

visual_final_model <- cbind(combinations, ng_g = visual_y_pred)

# Create a 3D plot with TimeDiff and Distance on the horizontal plane and ng_g on the vertical plane
fig <- plot_ly(data = as.data.frame(visual_final_model), 
               x = ~TimeDiff, 
               y = ~Distance, 
               z = ~ng_g, 
               type = "s", 
               mode = "markers", 
               marker = list(size = 4, color = ~ng_g, colorscale = "Viridis", showscale = TRUE))

# Show the plot
fig







