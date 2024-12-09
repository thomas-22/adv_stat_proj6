library(xgboost)
library(caTools)
library(ggplot2)
if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
  install.packages("fitdistrplus")
}
library(fitdistrplus)
library(plotly)
library(reshape2)

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
rmse_converge_tolerance <- 0.01 # Convergence based on relative improvement
ratio <- 0.7

split <- sample.split(modeling_data$ng_g, SplitRatio = ratio)
train_data <- subset(modeling_data, split == TRUE)
test_data <- subset(modeling_data, split == FALSE)

# Prepare the data for XGBoost
X_train <- data.matrix(train_data[, c("TimeDiff", "Distance")])
y_train <- train_data$ng_g

X_test <- data.matrix(test_data[, c("TimeDiff", "Distance")])
y_test <- test_data$ng_g

while (!tuning_converge) {
  # Define hyperparameter grid
  if (initialize_tuning) {
    param_grid <- expand.grid(
      max_depth = c(5,6,7),
      eta = c(0.16, 0.17, 0.18, 0.19),
      gamma = c(5.9, 5.95, 6, 6.05, 6.1),
      subsample = c(0.55, 0.6, 0.65),
      colsample_bytree = c(1),
      min_child_weight = c(4.65, 4.7, 4.75, 4.8, 4.85, 5)
    )
  } else {
    param_grid <- expand.grid(
      max_depth = seq(max(2, best_params$max_depth - 1), best_params$max_depth + 1),
      eta = pmax(0.01, c(best_params$eta - 0.01, best_params$eta, best_params$eta + 0.01)),
      gamma = pmax(0, c(best_params$gamma - 0.05, best_params$gamma, best_params$gamma + 0.05)),
      subsample = pmax(0.5, pmin(1, c(best_params$subsample - 0.01, best_params$subsample, best_params$subsample + 0.01))),
      colsample_bytree = pmax(0.5, pmin(1, c(best_params$colsample_bytree - 0.01, best_params$colsample_bytree, best_params$colsample_bytree + 0.01))),
      min_child_weight = pmax(0.1, c(best_params$min_child_weight - 0.01, best_params$min_child_weight, best_params$min_child_weight + 0.01))
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
      nrounds = 2000,
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
  test_rmse[tuning_iterations] <- sqrt(mean((y_test - y_pred)^2))
  
  new_param_entry <- data.frame(
                                      max_depth = param_grid$max_depth[i],
                                      eta = param_grid$eta[i],
                                      gamma = param_grid$gamma[i],
                                      subsample = param_grid$subsample[i],
                                      colsample_bytree = param_grid$colsample_bytree[i],
                                      min_child_weight = param_grid$min_child_weight[i],
                                      test_rmse = test_rmse[tuning_iterations]
  )
  best_param_collection <- rbind(best_param_collection, new_param_entry)
  
  cat(sprintf("Test RMSE: %.4f\n", test_rmse))
  
  #Check convergence
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
}

# Save results
write.csv(log_results, "hyperparameter_tuning_log.csv", row.names = FALSE)
write.csv(log_results, "hyperparameter_tuning_log_bestmodels.csv", row.names = FALSE)

saveRDS(final_model, "final_xgboost_model.rds")


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


