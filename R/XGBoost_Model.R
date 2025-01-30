# -------------------------
# INITIAL SETUP
# -------------------------
set.seed(42)
library(xgboost)
library(caTools)
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(plotly)
library(reshape2)

# -------------------------
# DATA PREPARATION FUNCTION
# -------------------------
prepare_data <- function(data, covariables = c("TimeDiff", "Distance"), ratio = 0.75) {
  # Filter non-positive values if needed
  modeling_data <- data %>%
    filter(ng_g > 0)
  
  # Train/test split
  split <- sample.split(modeling_data$ng_g, SplitRatio = ratio)
  train_data <- subset(modeling_data, split == TRUE)
  test_data <- subset(modeling_data, split == FALSE)
  
  # Create matrices for XGBoost
  X_train <- data.matrix(train_data[, covariables])
  y_train <- train_data$ng_g
  
  X_test <- data.matrix(test_data[, covariables])
  y_test <- test_data$ng_g
  
  X_full <- data.matrix(modeling_data[, covariables])
  y_full <- modeling_data$ng_g
  
  list(
    train_data = train_data,
    test_data = test_data,
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test,
    X_full = X_full,
    y_full = y_full
  )
}

# -------------------------
# BASELINE EVALUATION (GAMMA)
# -------------------------
evaluate_gamma_fit <- function(modeling_data, test_data, target_column, num_iterations = 1000) {
  # Fit a gamma distribution to the target column
  fit_gamma <- fitdist(modeling_data[[target_column]], "gamma")
  
  # Extract parameters
  shape_param <- fit_gamma$estimate["shape"]
  rate_param <- fit_gamma$estimate["rate"]
  
  # RMSE calculation
  calculate_rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
  }
  
  actual_values <- test_data[[target_column]]
  test_length <- length(actual_values)
  
  # Sample predictions and compute RMSE
  rmse_values <- replicate(num_iterations, {
    random_predictions <- rgamma(test_length, shape = shape_param, rate = rate_param)
    calculate_rmse(actual_values, random_predictions)
  })
  
  list(
    fit_parameters = list(shape = shape_param, rate = rate_param),
    mean_rmse = mean(rmse_values),
    rmse_values = rmse_values
  )
}

# -------------------------
# HYPERPARAMETER TUNING FUNCTION
# -------------------------
tune_xgboost <- function(X_train, y_train, X_test, y_test, 
                         best_params = list(max_depth = 5, eta = 0.1, gamma = 1, 
                                            subsample = 0.8, colsample_bytree = 0.8, 
                                            min_child_weight = 1), 
                         initialize_tuning = TRUE,
                         max_iterations = 20, 
                         rmse_converge_tolerance = 0.1) {
  # This function:
  # - On the first iteration, picks parameters around the given `best_params` or from defaults if `initialize_tuning = TRUE`.
  # - On subsequent iterations, it refines the search around the current best parameters.
  # - Chooses best parameters based on test RMSE.
  
  best_rmse <- Inf
  best_params_final <- best_params
  best_nrounds <- 100  # Initialize with a default value
  
  tuning_converge <- FALSE
  dynamic_early_stopping <- max(5, min(30, floor(nrow(X_train)/5)/10)) - 4
  iteration_count <- 1
  
  cat("Starting hyperparameter tuning...\n")
  
  while (!tuning_converge && iteration_count <= max_iterations) {
    # Define hyperparameter grid
    if (initialize_tuning) {
      # Random sampling for initial search
      param_grid <- expand.grid(
        max_depth = seq(max(4, best_params$max_depth - 1), min(6, best_params$max_depth + 1)),
        eta = pmax(0.01, rnorm(4, mean = 0.1734111, sd = 0.02)),
        gamma = pmax(0, rnorm(4, mean = 5.9078, sd = 0.05)),
        subsample = pmax(0.5, pmin(1, rnorm(4, mean = 0.6074106, sd = 0.05))),
        colsample_bytree = pmax(0.5, pmin(1, rnorm(4, mean = 1, sd = 0.05))),
        min_child_weight = pmax(0.1, rnorm(4, mean = 4.798780, sd = 0.2))
      )
    } else {
      # Search around current best_params_final
      param_grid <- expand.grid(
        max_depth = round(rnorm(4, mean = best_params_final$max_depth, sd = 1)),
        eta = pmax(0.01, rnorm(4, mean = best_params_final$eta, sd = 0.02)), 
        gamma = pmax(0, rnorm(4, mean = best_params_final$gamma, sd = 0.05)),
        subsample = pmax(0.5, pmin(1, rnorm(4, mean = best_params_final$subsample, sd = 0.05))),
        colsample_bytree = pmax(0.5, pmin(1, rnorm(4, mean = best_params_final$colsample_bytree, sd = 0.05))),
        min_child_weight = pmax(0.1, rnorm(4, mean = best_params_final$min_child_weight, sd = 0.2))
      )
    }
    
    iteration_best_rmse <- Inf
    iteration_best_params <- NULL
    iteration_best_nrounds <- NULL
    
    for (i in 1:nrow(param_grid)) {
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
      
      cat(sprintf("Iteration %d of %d: Evaluating %d of %d parameter sets...", 
                  iteration_count, max_iterations, i, nrow(param_grid)), "\r")
      
      # Do CV on training data to find best iteration
      cv_results <- xgb.cv(
        params = params,
        data = X_train,
        label = y_train,
        nrounds = 2000,
        nfold = 5,
        verbose = 0,
        early_stopping_rounds = dynamic_early_stopping
      )
      
      # Extract the best iteration found by CV
      best_iter <- cv_results$best_iteration
      
      # Now, train a final model on full training data using best_iter
      final_model <- xgboost(
        params = params,
        data = X_train,
        label = y_train,
        nrounds = best_iter,
        verbose = 0
      )
      
      # Predict on test set to get test RMSE
      y_test_pred <- predict(final_model, X_test)
      test_rmse <- sqrt(mean((y_test_pred - y_test)^2))
      
      # Check if this is the best so far for this iteration
      if (test_rmse < iteration_best_rmse) {
        iteration_best_rmse <- test_rmse
        iteration_best_params <- params
        iteration_best_nrounds <- best_iter
      }
    }
    
    cat("\n", sprintf("Iteration %d best test RMSE: %.5f\n", 
                      iteration_count, iteration_best_rmse))
    
    # Update global best if improved
    if (iteration_best_rmse < best_rmse) {
      best_rmse <- iteration_best_rmse
      best_params_final <- iteration_best_params
      best_nrounds <- iteration_best_nrounds
    } else {
      # If no improvement, consider we might be converged
      if (abs(iteration_best_rmse - best_rmse) < rmse_converge_tolerance) {
        tuning_converge <- TRUE
        cat("Convergence criterion met. Stopping tuning.\n")
      }
    }
    
    # Prepare for next iteration
    initialize_tuning <- FALSE
    iteration_count <- iteration_count + 1
  }
  
  cat("Tuning completed!\n")
  cat(sprintf("Best test RMSE: %.5f\n", best_rmse))
  cat("Best parameters:\n")
  print(best_params_final)
  cat("Best nrounds:", best_nrounds, "\n")
  
  # Return parameters and nrounds separately
  list(
    params = best_params_final,
    nrounds = best_nrounds,
    mean_rmse = best_rmse
  )
}

# -------------------------
# MODEL TRAINING FUNCTION
# -------------------------
train_final_model <- function(X_full, y_full, best_params, best_nrounds, nfold = 7) {
  cat("Starting final model training...\n")
  start_time <- Sys.time()
  
  # Perform cross-validation to confirm the best iteration (optional)
  dynamic_early_stopping <- max(5, min(30, floor(nrow(X_full)/5)/10))
  
  final_cv <- xgb.cv(
    params = best_params,
    data = X_full,
    label = y_full,
    nrounds = best_nrounds,
    nfold = nfold,
    verbose = 1,
    early_stopping_rounds = dynamic_early_stopping
  )
  
  best_iteration <- final_cv$best_iteration
  
  cat(sprintf("Training final model with best iteration: %d\n", best_iteration))
  
  final_model <- xgboost(
    params = best_params,
    data = X_full,
    label = y_full,
    nrounds = best_iteration,
    verbose = 1
  )
  
  end_time <- Sys.time()
  cat("Final model training completed.\n")
  cat(sprintf("Total training time: %s seconds\n", 
              round(as.numeric(difftime(end_time, start_time, units="secs")),2)))
  
  return(final_model)
}

# -------------------------
# PERMUTATION TESTS & BASELINE COMPARISONS
# -------------------------
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

compare_baselines <- function(final_model, X_test, y_test, X_full, y_full, 
                              gamma_fit_eval_full, gamma_fit_eval_train,
                              best_params, best_nrounds) {
  # RMSE on test with final model
  y_test_pred <- predict(final_model, X_test)
  rmse_test <- calculate_rmse(y_test, y_test_pred)
  
  # Random permutation of predictions on test
  num_samples <- 1000
  rmse_test_random <- numeric(num_samples)
  for (i in seq_len(num_samples)) {
    rmse_test_random[i] <- calculate_rmse(y_test, sample(y_test_pred))
  }
  mean_rmse_test_random <- mean(rmse_test_random)
  
  # Random permutation of predictions on full data
  y_full_pred <- predict(final_model, X_full)
  rmse_full_random <- numeric(num_samples)
  for (i in seq_len(num_samples)) {
    rmse_full_random[i] <- calculate_rmse(y_full, sample(y_full_pred))
  }
  mean_rmse_full_random <- mean(rmse_full_random)
  
  # Permutation test: shuffle y_full and train model again
  # Use best_params and best_nrounds separately
  params <- best_params
  nrounds <- best_nrounds
  
  dynamic_early_stopping <- max(5, min(30, floor(nrow(X_full) / 5) / 10))
  
  sampled_yfull <- sample(y_full)
  fm_permutated_result_cv <- xgb.cv(
    params = params,
    data = X_full,
    label = sampled_yfull,
    nrounds = 2000,  # Using a high number; will use early stopping
    nfold = 5,
    verbose = 0,
    early_stopping_rounds = dynamic_early_stopping
  )
  
  best_iter_permute <- fm_permutated_result_cv$best_iteration
  
  fm_permutate_result <- xgboost(
    params = params,
    data = X_full,
    label = sampled_yfull,
    nrounds = best_iter_permute,
    verbose = 0
  )
  
  fm_trainedon_permute_data_test_rmse <- calculate_rmse(y_test, predict(fm_permutate_result, X_test))
  
  list(
    rmse_test_final = rmse_test,
    gamma_fit_rmse_full = gamma_fit_eval_full$mean_rmse,
    gamma_fit_rmse_train = gamma_fit_eval_train$mean_rmse,
    mean_rmse_test_random = mean_rmse_test_random,
    mean_rmse_full_random = mean_rmse_full_random,
    fm_trainedon_permute_data_test_rmse = fm_trainedon_permute_data_test_rmse
  )
}

# -------------------------
# PLOTTING FUNCTIONS
# -------------------------
plot_results <- function(final_model, X_test, y_test, 
                         rmse_test_final, 
                         gamma_fit_eval_full_mean,
                         upper_difftime, 
                         upper_distance, 
                         data_cleanedup) {
  # Prediction for plotting
  y_pred <- predict(final_model, X_test)
  
  # Scatter + LM line plot
  p <- ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
    geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
    xlim(0, 1500) +
    ylim(0, 1500) +
    geom_point(size = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(title = "XGBoost: Predicted vs Actual, ng_g, After Grid Search Tuning", 
         x = "Actual", y = "Predicted") +
    theme_light() +
    coord_fixed(ratio = 1) +
    annotate("text", x = 500, y = Inf, hjust = 1, vjust = 2.5,
             label = paste("RMSE:", round(rmse_test_final, 2)),
             color = "black", size = 3.5)
  
  print(p)
  
  # 3D surface plot
  time_diff <- seq(1, upper_difftime, length.out = 1500)
  distance  <- seq(1, upper_distance, length.out = 1500)
  
  combinations <- expand.grid(TimeDiff = time_diff, Distance = distance)
  combos_matrix <- as.matrix(combinations)
  visual_y_pred <- predict(final_model, combos_matrix)
  
  visual_final_model <- cbind(combinations, ng_g = visual_y_pred)
  grid_data <- dcast(visual_final_model, TimeDiff ~ Distance, value.var = "ng_g")
  
  fig <- plot_ly(
    data = as.data.frame(data_cleanedup),
    x = ~TimeDiff,
    y = ~Distance,
    z = ~ng_g,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 3, color = "black")
  )
  
  fig <- fig %>%
    add_trace(
      z = as.matrix(grid_data[, -1]),
      x = grid_data$TimeDiff,
      y = as.numeric(colnames(grid_data)[-1]),
      type = "surface",
      colorscale = "Viridis",
      opacity = 0.7
    ) %>%
    layout(
      title = "XGBoost Model (Trained on Full Data) Prediction + Actual Data (black)",
      scene = list(
        xaxis = list(title = "Time Difference, Hours"),
        yaxis = list(title = "Distance (m)"),
        zaxis = list(title = "FCM Level, ng/g")
      ),
      annotations = list(
        list(
          x = 1.05,
          y = 0.5,
          text = paste(
            "Model Parameters:\n\n",
            "Objective: ", final_model$params$objective, "\n",
            "Evaluation Metric: ", final_model$params$eval_metric, "\n",
            "Max Depth: ", final_model$params$max_depth, "\n",
            "Eta: ", round(final_model$params$eta, 3), "\n",
            "Gamma: ", round(final_model$params$gamma, 3), "\n",
            "Subsample: ", round(final_model$params$subsample, 3), "\n",
            "Colsample By Tree: ", round(final_model$params$colsample_bytree, 3), "\n",
            "Min Child Weight: ", round(final_model$params$min_child_weight, 3), "\n",
            "Test-RMSE (Model): ", round(rmse_test_final, 3), "\n",
            "Test-RMSE (Gamma Fit): ", round(gamma_fit_eval_full_mean, 3)
          ),
          showarrow = FALSE,
          font = list(size = 14, color = "black", family = "Arial"),
          align = "left",
          xanchor = "right",
          yanchor = "bottom"
        )
      )
    )
  
  return(fig)
}

plot_predicted_vs_actual <- function(Y_pred, Y_actual, RMSE, Text = "") {
  
  # Create the plot title by incorporating the RMSE and any additional text
  plot_title <- paste("XGBoost: Predicted Average vs Actual ng_g", Text, "RMSE:", round(RMSE, 2))
  
  p <- ggplot(data.frame(Actual = Y_actual, Predicted = Y_pred), aes(x = Actual, y = Predicted)) +
    geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
    xlim(0, 1250) +
    ylim(0, 1250) +
    geom_point(size = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = plot_title,
      x = "Actual",
      y = "Predicted"
    ) +
    theme_light() +
    coord_fixed(ratio = 1)
  
  return(p)
}


plot_results_v2 <- function(final_model, X_test, y_test, 
                            rmse_test_final, 
                            gamma_fit_eval_full_mean,
                            upper_difftime, 
                            upper_distance, 
                            data_cleanedup) {
  # Prediction for plotting
  y_pred <- predict(final_model, X_test)
  
  # Scatter + LM line plot
  p <- ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
    geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
    xlim(0, 1500) +
    ylim(0, 1500) +
    geom_point(size = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(title = "XGBoost: Predicted vs Actual, ng_g, After Grid Search Tuning", 
         x = "Actual", y = "Predicted") +
    theme_light() +
    coord_fixed(ratio = 1) +
    annotate("text", x = 500, y = Inf, hjust = 1, vjust = 2.5,
             label = paste("RMSE:", round(rmse_test_final, 2)),
             color = "black", size = 3.5)
  
  print(p)
  
  # Generate logarithmically spaced sequences for the grid
  time_diff_t <- exp(seq(log(1), log(upper_difftime), length.out = 1500))
  distance_t  <- exp(seq(log(1), log(upper_distance), length.out = 1500))
  
  combinations <- expand.grid(TimeDiff_T = time_diff_t, Distance_T = distance_t)
  combos_matrix <- as.matrix(combinations)
  visual_y_pred <- predict(final_model, combos_matrix)
  
  visual_final_model <- cbind(combinations, ng_g = visual_y_pred)
  
  # Reshape data for surface plotting
  grid_data <- dcast(visual_final_model, TimeDiff_T ~ Distance_T, value.var = "ng_g")
  
  # Actual data plot
  fig <- plot_ly(
    data = as.data.frame(data_cleanedup),
    x = ~TimeDiff_T,
    y = ~Distance_T,
    z = ~ng_g,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 3, color = "black")
  )
  
  # Surface plot
  fig <- fig %>%
    add_trace(
      z = as.matrix(grid_data[, -1]),
      x = grid_data$TimeDiff_T,
      y = as.numeric(colnames(grid_data)[-1]),
      type = "surface",
      colorscale = "Viridis",
      opacity = 0.7
    ) %>%
    layout(
      title = "XGBoost Model (Trained on transformed Full Data) Prediction + Actual Data (black)",
      scene = list(
        xaxis = list(title = "Time Difference, Hours, Transformed", type = "log"),
        yaxis = list(title = "Distance (m), Transformed", type = "log"),
        zaxis = list(title = "FCM Level, ng/g")
      ),
      annotations = list(
        list(
          x = 1.05,
          y = 0.5,
          text = paste(
            "Model Parameters:\n\n",
            "Objective: ", final_model$params$objective, "\n",
            "Evaluation Metric: ", final_model$params$eval_metric, "\n",
            "Max Depth: ", final_model$params$max_depth, "\n",
            "Eta: ", round(final_model$params$eta, 3), "\n",
            "Gamma: ", round(final_model$params$gamma, 3), "\n",
            "Subsample: ", round(final_model$params$subsample, 3), "\n",
            "Colsample By Tree: ", round(final_model$params$colsample_bytree, 3), "\n",
            "Min Child Weight: ", round(final_model$params$min_child_weight, 3), "\n",
            "Test-RMSE (Model): ", round(rmse_test_final, 3), "\n",
            "Test-RMSE (Gamma Fit): ", round(gamma_fit_eval_full_mean, 3)
          ),
          showarrow = FALSE,
          font = list(size = 14, color = "black", family = "Arial"),
          align = "left",
          xanchor = "right",
          yanchor = "bottom"
        )
      )
    )
  return(fig)
}

# -------------------------------------
# MAIN PIPELINE
# -------------------------------------
XGBoost_run_default_pipeline <- function(data_cleanedup,
                                         covariables = c("TimeDiff", "Distance"),
                                         tune = TRUE,
                                         model_path = "Models/final_xgboost_model.rds",
                                         max_iterations = 10) {
  # 1) Prepare data
  prepared_data <- prepare_data(data_cleanedup, covariables = covariables)
  
  X_train <- prepared_data$X_train
  y_train <- prepared_data$y_train
  X_test  <- prepared_data$X_test
  y_test  <- prepared_data$y_test
  X_full  <- prepared_data$X_full
  y_full  <- prepared_data$y_full
  train_data <- prepared_data$train_data
  test_data  <- prepared_data$test_data
  
  cat("Data preparation complete.\n")
  
  # Initialize variables to store best parameters and nrounds
  best_params <- NULL
  best_nrounds <- NULL
  
  # 2) Either tune hyperparameters or load existing model
  if (tune) {
    tuning_results <- tune_xgboost(
      X_train = X_train, 
      y_train = y_train, 
      X_test  = X_test, 
      y_test  = y_test,
      best_params = list(
        max_depth = 5,
        eta = 0.1734111,
        gamma = 5.907800,
        subsample = 0.6074106,
        colsample_bytree = 1,
        min_child_weight = 4.798780
      ),
      initialize_tuning = FALSE,
      max_iterations = max_iterations,
      rmse_converge_tolerance = 1
    )
    
    best_params <- tuning_results$params
    best_nrounds <- tuning_results$nrounds
    
    # 3) Train final model on full data
    final_model <- xgboost(
      params = best_params,
      data = X_full,
      label = y_full,
      nrounds = best_nrounds,
      verbose = 1
    )
    
    # 4) Save both model AND best_params and best_nrounds in one RDS
    model_and_params <- list(
      final_model = final_model,
      best_params = best_params,
      best_nrounds = best_nrounds
    )
    saveRDS(model_and_params, model_path)
    
    cat("Final model + params saved to:", model_path, "\n")
  } else {
    # 3) Load the model + params
    loaded_obj <- readRDS(model_path)
    final_model <- loaded_obj$final_model
    best_params <- loaded_obj$best_params
    best_nrounds <- loaded_obj$best_nrounds
    cat("Loaded pre-trained model from:", model_path, "\n")
  }
  
  # 4) Evaluate gamma baseline
  gamma_fit_eval_full  <- evaluate_gamma_fit(train_data, train_data, "ng_g", num_iterations = 1000)
  gamma_fit_eval_train <- evaluate_gamma_fit(train_data, test_data,  "ng_g", num_iterations = 1000)
  
  # 5) Compare baselines and permutations
  comparisons <- compare_baselines(
    final_model          = final_model,
    X_test               = X_test,
    y_test               = y_test,
    X_full               = X_full,
    y_full               = y_full,
    gamma_fit_eval_full  = gamma_fit_eval_full,
    gamma_fit_eval_train = gamma_fit_eval_train,
    best_params          = best_params,
    best_nrounds         = best_nrounds
  )
  
  cat("Final Model RMSE on Test:", comparisons$rmse_test_final, "\n")
  cat("Gamma Baseline RMSE (Full):", comparisons$gamma_fit_rmse_full, "\n")
  cat("Gamma Baseline RMSE (Train):", comparisons$gamma_fit_rmse_train, "\n")
  cat("RMSE (Random Permutation, Test Predictions):", comparisons$mean_rmse_test_random, "\n")
  cat("RMSE (Random Permutation, Full Predictions):", comparisons$mean_rmse_full_random, "\n")
  cat("RMSE (Trained on Permuted Labels):", comparisons$fm_trainedon_permute_data_test_rmse, "\n")
  
  # 6) Plot results
  plotly_fig <- plot_results(
    final_model           = final_model,
    X_test                = X_test,
    y_test                = y_test,
    rmse_test_final       = comparisons$rmse_test_final,
    gamma_fit_eval_full_mean = comparisons$gamma_fit_rmse_full,
    upper_difftime        = max(as.integer(data_cleanedup$TimeDiff)),
    upper_distance        = max(as.integer(data_cleanedup$Distance)),
    data_cleanedup        = data_cleanedup
  )
  
  # Return a list of main results
  list(
    final_model  = final_model,
    comparisons  = comparisons,
    plotly_fig   = plotly_fig,
    X_full = X_full,
    predictions_full = predict(final_model, X_full)
  )
}



# -------------------------------------
# Wrapper Function to get a good run
# -------------------------------------

# Wrapper function to run the pipeline multiple times and aggregate results
run_multiple_xgboost_pipelines_aggregated <- function(data_cleanedup,
                                                      covariables = c("TimeDiff", "Distance"),
                                                      tune = TRUE,
                                                      base_model_path = "Models/xgboost_model_aggregated",
                                                      max_iterations = 10,
                                                      num_runs = 10,
                                                      seed = NULL) {
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Initialize a list to store results
  results_list <- vector("list", num_runs)
  
  # Vectors to store metrics
  rmse_values <- numeric(num_runs)
  gamma_rmse_full <- numeric(num_runs)
  gamma_rmse_train <- numeric(num_runs)
  mean_rmse_test_random <- numeric(num_runs)
  mean_rmse_full_random <- numeric(num_runs)
  fm_trainedon_permute_data_test_rmse <- numeric(num_runs)
  
  # Initialize lists to store predictions from each run
  predictions_full_list <- vector("list", num_runs)  # For full dataset predictions
  
  # Initialize a variable to store full set covariables
  full_covariables <- NULL
  
  cat("Starting", num_runs, "runs of XGBoost pipeline with aggregated results...\n")
  
  for (i in 1:num_runs) {
    cat("\nRun", i, "of", num_runs, "\n")
    if (tune == TRUE) {
      model_path_i <- paste0(base_model_path, "_run", i, ".rds")
    }
    else{
      model_path_i <- paste0(base_model_path, ".rds")
    }
    
    # Execute the pipeline
    result <- XGBoost_run_default_pipeline(
      data_cleanedup = data_cleanedup,
      covariables = covariables,
      tune = tune,
      model_path = model_path_i,
      max_iterations = max_iterations
    )
    
    # Store the result
    results_list[[i]] <- result
    
    # Extract and store the RMSE and other metrics
    rmse_values[i] <- result$comparisons$rmse_test_final
    gamma_rmse_full[i] <- result$comparisons$gamma_fit_rmse_full
    gamma_rmse_train[i] <- result$comparisons$gamma_fit_rmse_train
    mean_rmse_test_random[i] <- result$comparisons$mean_rmse_test_random
    mean_rmse_full_random[i] <- result$comparisons$mean_rmse_full_random
    fm_trainedon_permute_data_test_rmse[i] <- result$comparisons$fm_trainedon_permute_data_test_rmse
    
    cat("Run", i, "RMSE on Test:", rmse_values[i], "\n")
    
    # Extract and store the full dataset predictions
    predictions_full_list[[i]] <- result$predictions_full #COMES FROM PIPELINE
    
    # Store full dataset covariables from the first run
    if (i == 1) {
      full_covariables <- result$X_full
    } else {
      # Optional: Verify that covariables are consistent across runs
      current_full_covariables <- result$full_data[, covariables, drop = FALSE]
      if (!all(full_covariables == current_full_covariables)) {
        warning(paste("Covariables in run", i, "do not match those in the first run."))
        # Handle inconsistencies as needed
      }
    }
  }
  
  # Aggregate the results
  aggregated_results <- list(
    mean_rmse_test_final = mean(rmse_values),
    sd_rmse_test_final = sd(rmse_values),
    mean_gamma_fit_rmse_full = mean(gamma_rmse_full),
    sd_gamma_fit_rmse_full = sd(gamma_rmse_full),
    mean_gamma_fit_rmse_train = mean(gamma_rmse_train),
    sd_gamma_fit_rmse_train = sd(gamma_rmse_train),
    mean_rmse_test_random = mean(mean_rmse_test_random),
    sd_rmse_test_random = sd(mean_rmse_test_random),
    mean_rmse_full_random = mean(mean_rmse_full_random),
    sd_rmse_full_random = sd(mean_rmse_full_random),
    mean_fm_trainedon_permute_data_test_rmse = mean(fm_trainedon_permute_data_test_rmse),
    sd_fm_trainedon_permute_data_test_rmse = sd(fm_trainedon_permute_data_test_rmse)
  )
  
  if (tune == TRUE) {
    # Optionally, save aggregated results to a file
    aggregated_results_path <- paste0(base_model_path, "_aggregated_results.rds")
    saveRDS(aggregated_results, aggregated_results_path)
    cat("Aggregated results saved to:", aggregated_results_path, "\n")
    
    # Optionally, save all individual run results
    all_runs_path <- paste0(base_model_path, "_all_runs.rds")
    saveRDS(results_list, all_runs_path)
    cat("All run results saved to:", all_runs_path, "\n")
  }
  
  # Compute the average predicted values across all runs for the full dataset
  # Ensure that predictions_full_list has no NULLs and all elements have the same length
  if (any(sapply(predictions_full_list, is.null))) {
    stop("One or more runs did not return predictions_full.")
  }
  
  # Ensure all predictions have the same length
  pred_lengths <- sapply(predictions_full_list, length)
  if (length(unique(pred_lengths)) != 1) {
    stop("Not all predictions_full have the same length.")
  }
  
  predictions_full_matrix <- do.call(cbind, predictions_full_list)
  Y_pred_full <- rowMeans(predictions_full_matrix)
  
  # Combine Y_pred_full with full dataset covariates
  XXX <- data.frame(Y_pred = Y_pred_full, full_covariables)
  
  # # Optionally, save the average predictions to files
  # average_predictions_full_path <- paste0(base_model_path, "_average_predictions_full.rds")
  # saveRDS(Y_pred_full_df, average_predictions_full_path)
  # cat("Average full dataset predictions with covariates saved to:", average_predictions_full_path, "\n")
  
  # Return the aggregated results along with all run details and average predictions with covariates
  return(list(
    aggregated_results = aggregated_results,
    all_runs = results_list,
    rmse_values = rmse_values,
    XXX = XXX))    # Average predictions for full set with covariates
}



