library(xgboost)
library(caTools)
library(reshape2)
library(grid)
library(gridExtra)

# -------------------------
# DATA PREPARATION FUNCTION
# -------------------------
prepare_data <- function(data, covariables = c("TimeDiff", "Distance"), ratio = 0.75) {
  # Filter non-positive values if needed
  modeling_data <- data #%>%
    #filter(ng_g > 0)
  
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

# -------------------------------------
# MAIN PIPELINE
# -------------------------------------
XGBoost_run_default_pipeline <- function(
    data_cleanedup,
    covariables = c("TimeDiff", "Distance"),
    tune = TRUE,
    model_path = "Models/final_xgboost_model.rds",
    max_iterations = 10
  ) {
  # Prepare data
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
  
  # Either tune hyperparameters or load existing model
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
    
    # Train final model on full data
    final_model <- xgboost(
      params = best_params,
      data = X_full,
      label = y_full,
      nrounds = best_nrounds,
      verbose = 1
    )
    
    # Save both model AND best_params and best_nrounds in one RDS
    model_and_params <- list(
      final_model = final_model,
      best_params = best_params,
      best_nrounds = best_nrounds
    )
    saveRDS(model_and_params, model_path)
    
    cat("Final model + params saved to:", model_path, "\n")
  } else {
    # Load the model + params
    loaded_obj <- readRDS(model_path)
    final_model <- loaded_obj$final_model
    best_params <- loaded_obj$best_params
    best_nrounds <- loaded_obj$best_nrounds
    cat("Loaded pre-trained model from:", model_path, "\n")
  }
  
  return(final_model)
}

# -------------------------
# PLOTTING FUNCTIONS
# 3D plot uses plotly. 2D plot uses ggplot2.
# -------------------------
plot_xgboost_3d <- function(model, data) {
  upper_difftime <- ceiling(max(data$TimeDiff))
  upper_distance <- ceiling(max(data$Distance))
  time_diff <- seq(1, upper_difftime, length.out = 1500)
  distance  <- seq(1, upper_distance, length.out = 1500)
  
  combinations <- expand.grid(TimeDiff = time_diff, Distance = distance)
  combos_matrix <- as.matrix(combinations)
  visual_y_pred <- predict(model, combos_matrix)
  
  visual_final_model <- cbind(combinations, ng_g = visual_y_pred)
  grid_data <- dcast(visual_final_model, TimeDiff ~ Distance, value.var = "ng_g")
  
  fig <- plotly::plot_ly()
  fig <- fig %>% plotly::add_trace(
    data = as.data.frame(data),
    x = ~TimeDiff,
    y = ~Distance,
    z = ~ng_g,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 3, color = "black")
  )  
  fig <- fig %>%
    plotly::add_trace(
      z = as.matrix(grid_data[, -1]),
      x = grid_data$TimeDiff,
      y = as.numeric(colnames(grid_data)[-1]),
      type = "surface",
      colorscale = "Viridis",
      opacity = 0.7
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Time Difference [h]"),
        yaxis = list(title = "Distance [m]"),
        zaxis = list(title = "FCM Level [ng/g]", tickvals = c(200, 400, 600, 800, 1000)),
        camera = list(
          eye = list(x = -2, y = -1.5, z = 1)
        )
      )
    ) %>%
    plotly::colorbar(title = "FCM Level [ng/g]")
  
  return(fig)
}

plot_xgboost_2d <- function(
    model, data,
    rmse_test_final = 0,
    gamma_fit_eval_full_mean = 0
  ) {
  upper_difftime <- ceiling(max(data$TimeDiff))
  upper_distance <- ceiling(max(data$Distance))
  time_diff <- seq(0, upper_difftime, length.out = 500)
  distance  <- seq(0, upper_distance, length.out = 500)
  
  combinations <- expand.grid(TimeDiff = time_diff, Distance = distance)
  combos_matrix <- as.matrix(combinations)
  visual_y_pred <- predict(model, combos_matrix)
  
  visual_final_model <- cbind(combinations, ng_g = visual_y_pred)

  ggplot() +
    geom_raster(
      data = visual_final_model,
      aes(x = TimeDiff, y = Distance, fill = ng_g)
    ) +
    geom_point(
      data = data,
      aes(x = TimeDiff, y = Distance, fill = ng_g),
      shape = 21, color = "grey", size = 3
    ) +
    scale_fill_viridis_c(limits = c(15, 1100)) +  # approximate range of FCM levels in data
    labs(x = "Time difference [h]", y = "Distance [m]", fill = "FCM Level [ng/g]")
}

calculate_rmse_full_data <- function(model, data) {
  actual <- data$ng_g
  predicted <- predict(model, data.matrix(data[, c("TimeDiff", "Distance")]))
  
  sqrt(mean((actual - predicted)^2))
}

save_hyperparameter_png <- function() {
  data <- read.csv("./Models/xgboost_models_overview.csv", header = TRUE, stringsAsFactors = FALSE)
  data <- data[, !(names(data) %in% c("objective", "Mean_RMSE", "SD_RMSE"))]
  table_grob <- tableGrob(data, rows = NULL, theme = ttheme_minimal())
  table_width <- convertWidth(sum(table_grob$widths), "inches", valueOnly = TRUE)
  table_height <- convertHeight(sum(table_grob$heights), "inches", valueOnly = TRUE)
  dpi <- 96
  png("./Figures/Models/hyperparametertable.png",
      width = table_width * dpi,
      height = table_height * dpi,
      res = dpi)
  grid.draw(table_grob)
  dev.off()
}
