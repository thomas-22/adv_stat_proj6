library(xgboost)
library(caTools)
library(ggplot2)
if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
  install.packages("fitdistrplus")
}
library(fitdistrplus)
library(plotly)
library(reshape2)

#source("./R/Assign_FCMData_to_Stressors.R")

# Prepare the data
modeling_data <- data_cleanedup %>%
  filter(ng_g > 0)

# Initialize tuning parameters
best_params <- list()
initialize_tuning <- TRUE # SET TO TRUE IF RUNNING FIRST TIME
tuning_converge <- FALSE

# Logging
log_results <- data.frame(
  combination = integer(),
  max_depth = integer(),
  eta = numeric(),
  gamma = numeric(),
  subsample = numeric(),
  colsample_bytree = numeric(),
  min_child_weight = numeric(),
  rmse = numeric(),
  best_iteration = integer()
)
best_param_collection <- data.frame(
  max_depth = integer(),
  eta = numeric(),
  gamma = numeric(),
  subsample = numeric(),
  colsample_bytree = numeric(),
  min_child_weight = numeric(),
  test_rmse = numeric()
)

tuning_iterations <- 1
max_iterations <- 100

test_rmse <- numeric(max_iterations)

best_iteration <- 0
best_rmse <- Inf

# Split the data
rmse_converge_tolerance <- 0.1
ratio <- 0.75

split <- sample.split(modeling_data$ng_g, SplitRatio = ratio)
train_data <- subset(modeling_data, split == TRUE)
test_data <- subset(modeling_data, split == FALSE)

# Prepare the data for XGBoost
X_train <- data.matrix(train_data[, c("TimeDiff", "Distance")])
y_train <- train_data$ng_g

X_test <- data.matrix(test_data[, c("TimeDiff", "Distance")])
y_test <- test_data$ng_g

X_full <- data.matrix(modeling_data[, c("TimeDiff", "Distance")])
y_full <- modeling_data$ng_g

# Early stopping rounds in grid:
observations_per_fold <- floor(nrow(X_train) / 5) #5 fold CV
dynamic_early_stopping <- max(5, min(30, observations_per_fold / 10))

# Early stopping rounds in final model (best per iteration):
observations_per_fold_FM <- floor(nrow(X_test) / 5)  # 5-fold CV
dynamic_early_stopping_FM <- max(5, min(30, observations_per_fold / 10))

while (!tuning_converge) {
  # Define hyperparameter grid
  if (initialize_tuning) {
    # Random sampling for initial search (Values from previous runs of the code)
    param_grid <-  expand.grid(
      max_depth = round(rnorm(5, mean = 5, sd = 1)),
      eta = pmax(0.01, rnorm(5, mean = 0.1734111, sd = 0.02)), # Learning rate >= 0.01
      gamma = pmax(0, rnorm(5, mean = 5.907800, sd = 0.05)), # Gamma >= 0
      subsample = pmax(0.5, pmin(1, rnorm(5, mean = 0.6074106, sd = 0.05))), # Clipped to [0.5, 1]
      colsample_bytree = pmax(0.5, pmin(1, rnorm(5, mean = 1, sd = 0.05))), # Clipped to [0.5, 1]
      min_child_weight = pmax(0.1, rnorm(5, mean = 4.798780, sd = 0.2)) # Min >= 0.1
    )
  } else {
    # Search around best_params (normal distribution)
    param_grid <-  expand.grid(
      max_depth = round(rnorm(5, mean = best_params$max_depth, sd = 1)),
      eta = pmax(0.01, rnorm(5, mean = best_params$eta, sd = 0.02)), # Learning rate >= 0.01
      gamma = pmax(0, rnorm(5, mean = best_params$gamma, sd = 0.05)), # Gamma >= 0
      subsample = pmax(0.5, pmin(1, rnorm(5, mean = best_params$subsample, sd = 0.05))), # Clipped to [0.5, 1]
      colsample_bytree = pmax(0.5, pmin(1, rnorm(5, mean = best_params$colsample_bytree, sd = 0.05))), # Clipped to [0.5, 1]
      min_child_weight = pmax(0.1, rnorm(5, mean = best_params$min_child_weight, sd = 0.2)) # Min >= 0.1
    )
  }
  
  # Reset best RMSE for this iteration
  iteration_best_rmse <- Inf
  
  # Total combinations in grid
  total_combinations <- nrow(param_grid)
  
  # Perform search
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
    
    cat(sprintf("Trying combination %d of %d: max_depth=%d, eta=%.3f, gamma=%.2f, subsample=%.2f, colsample_bytree=%.2f, min_child_weight=%.2f, Tuning Iterations=%d\n",
                i, total_combinations,
                param_grid$max_depth[i],
                param_grid$eta[i],
                param_grid$gamma[i],
                param_grid$subsample[i],
                param_grid$colsample_bytree[i],
                param_grid$min_child_weight[i],
                tuning_iterations))
    
    cv_results <- xgb.cv(
      params = params,
      data = X_train,
      label = y_train,
      nrounds = 2000,
      nfold = 5,
      verbose = 0,
      early_stopping_rounds = dynamic_early_stopping
    )
    
    # Get metrics
    mean_rmse <- cv_results$evaluation_log$test_rmse_mean[cv_results$best_iteration]
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
  

  
  cv_final_model <- xgb.cv(
    params = best_params,
    data = X_test,
    label = y_test,
    nrounds = 2000,
    nfold = 5,
    verbose = 0,
    early_stopping_rounds = dynamic_early_stopping_FM
  )
  
  # Get metrics for final Model using the test data instead of training Data
  mean_rmse_FM <- mean(cv_final_model$evaluation_log$test_rmse_mean)
  best_iter_FM <- cv_final_model$best_iteration
  
  # Make predictions
  # y_pred <- predict(final_model, X_test)
  
  test_rmse[tuning_iterations] <- mean_rmse_FM
  
  new_param_entry <- data.frame(
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    subsample = best_params$subsample,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    test_rmse = test_rmse[tuning_iterations]
  )
  best_param_collection <- rbind(best_param_collection, new_param_entry)
  
  cat(sprintf("Test RMSE: %.4f\n", test_rmse[tuning_iterations]))
  
  # Check convergence
  if (tuning_iterations >= max_iterations) {
    tuning_converge <- TRUE
  } else if (abs(test_rmse[tuning_iterations] - best_rmse) < rmse_converge_tolerance && tuning_iterations > 0) {
    tuning_converge <- TRUE
  } else {
    best_rmse <- iteration_best_rmse  # Update the best RMSE found so far
  }
  
  cat("Iteration Best RMSE:", iteration_best_rmse, "\n")
  cat("Current Best RMSE:", best_rmse, "\n")
  
  tuning_iterations <- tuning_iterations + 1
  initialize_tuning <- FALSE
  
  
  #Change params to the best currently available ones again:
  current_best_param_config <- log_results[which.min(log_results$rmse), ]
  best_params$max_depth <-        current_best_param_config$max_depth
  best_params$eta <-              current_best_param_config$eta
  best_params$gamma <-            current_best_param_config$gamma
  best_params$subsample <-        current_best_param_config$subsample
  best_params$colsample_bytree <- current_best_param_config$colsample_bytree
  best_params$min_child_weight <- current_best_param_config$min_child_weight
}

#Final Eval
possible_good_model1 <- log_results[which.min(log_results$rmse), ]
best_params$max_depth <-        possible_good_model1$max_depth
best_params$eta <-              possible_good_model1$eta
best_params$gamma <-            possible_good_model1$gamma
best_params$subsample <-        possible_good_model1$subsample
best_params$colsample_bytree <- possible_good_model1$colsample_bytree
best_params$min_child_weight <- possible_good_model1$min_child_weight

final_model_cv <- xgb.cv(
  params = best_params,
  data = X_full,
  label = y_full,
  nrounds = 1000,
  verbose = 1,
  nfold = 10,
  early_stopping_rounds = 5
)

final_model <- xgboost(
  params = best_params,
  data = X_full,
  label = y_full,
  nrounds = final_model_cv$best_iteration,
  verbose = 1
)



# # Save results
# write.csv(log_results, "hyperparameter_tuning_log.csv", row.names = FALSE)
# write.csv(log_results, "hyperparameter_tuning_log_bestmodels.csv", row.names = FALSE)
# 
# saveRDS(final_model, "final_xgboost_model.rds")


#final_model <- readRDS("final_xgboost_model.rds", refhook = NULL)


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
           label = paste("RMSE:", round(test_rmse, 2)),
           color = "black", size = 3.5)







#Work on Visualising the Model:
# Create sequences
time_diff <- seq(1, gut_retention_time_upper, by = 1)
distance <- seq(1, distance_threshold, by = 10)
#rows_discarded <- seq(0, 30, by = 1)

# Generate all combinations
combinations <- expand.grid(TimeDiff = time_diff, Distance = distance)


# Convert the data frame to a matrix
combinations <- as.matrix(combinations)

visual_y_pred <- predict(final_model, combinations)

visual_final_model <- as.data.frame(cbind(combinations, ng_g = visual_y_pred))

grid_data <- dcast(visual_final_model, TimeDiff ~ Distance, value.var = "ng_g")

# First, create the 3D scatter plot with black markers from 'data_cleanedup'
fig <- plot_ly(
  data = as.data.frame(data_cleanedup),  # Use 'data_cleanedup'
  x = ~TimeDiff,                        # x values (TimeDiff)
  y = ~Distance,                        # y values (Distance)
  z = ~ng_g,                            # z values (ng_g)
  type = "scatter3d",                   # 3D scatter plot
  mode = "markers",                     # Use markers
  marker = list(
    size = 3,                           # Marker size
    color = "black"                    # Set marker color to black
  )
)


fig <- fig %>%
  add_trace(
    z = as.matrix(grid_data[, -1]),      # Convert data to matrix, excluding 'TimeDiff' column
    x = grid_data$TimeDiff,              # x values (TimeDiff)
    y = colnames(grid_data)[-1],         # y values (Distance)
    type = "surface",                    # Create surface plot
    colorscale = "Viridis",              # Use Viridis color scale
    opacity = 0.7                        # Make translucent
  )

#Add title and labels
fig <- fig %>%
  layout(
    title = "XGBoost Model Prediction + Actual Data (black)",  # Set the title text
    titlefont = list(
      size = 20,                # Title font size
      color = "black",           # Title font color
      family = "Arial"           # Title font family
    ),
    scene = list(
      xaxis = list(title = "Time Difference, Hours (Hunting & Defecation Event)"),  # Label for x-axis
      yaxis = list(title = "Distance (Hunting Event & Animal), Meters"),         # Label for y-axis
      zaxis = list(title = "FCM Level, ng/g")              # Label for z-axis
    ),
    annotations = list(
      # Add custom text annotation
      list(
        x = 1.05,  # Positioning on the x-axis (just outside the plot)
        y = 0.5,   # Positioning on the y-axis (adjust as needed)
        text = paste(
          "Model Parameters:\n\n",
          "Objective: ", final_model$params$objective, "\n",
          "Evaluation Metric: ", final_model$params$eval_metric, "\n",
          "Max Depth: ", final_model$params$max_depth, "\n",
          "Learning Rate (Eta): ", final_model$params$eta, "\n",
          "Gamma: ", final_model$params$gamma, "\n",
          "Subsample: ", final_model$params$subsample, "\n",
          "Colsample By Tree: ", final_model$params$colsample_bytree, "\n",
          "Min Child Weight: ", final_model$params$min_child_weight, "\n",
          "RMSE (Model): "
        ),
        showarrow = FALSE,  # No arrow pointing to the plot
        font = list(
          size = 14,           # Font size of the text
          color = "black",      # Font color
          family = "Arial"      # Font family
        ),
        align = "left",      # Align the text to the left
        xanchor = "right",    # Anchor the text to the left of the x-position
        yanchor = "bottom"   # Anchor the text in the middle of the y-position
      )
    )
  )

# Display the plot
fig


