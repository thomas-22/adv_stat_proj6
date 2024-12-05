library(xgboost)
#install.packages("caTools")
library(caTools)
library(ggplot2)
if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
  install.packages("fitdistrplus")
}
library(fitdistrplus)

# source("./R/Assign_FCMData_to_Stressors.R")

# modeling_data <- FCMData_Assigned %>%
#   filter(ng_g > 0)
modeling_data <- fcm_specific %>%
  filter(ng_g > 0)

# Split the data
ratio <- 0.7
split <- sample.split(modeling_data, SplitRatio = ratio)
train_data <- subset(modeling_data, split == TRUE)
test_data <- subset(modeling_data, split == FALSE)

# Prepare the data for XGBoost
X_train <- data.matrix(train_data[, c("TimeDiff", "Distance")])
y_train <- train_data$ng_g

X_test <- data.matrix(test_data[, c("TimeDiff", "Distance")])
y_test <- test_data$ng_g

# Hyperparameter grid
param_grid <- expand.grid(
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1, 0.15, 0.2),
  gamma = c(0, 1, 5),
  subsample = c(0.5, 0.7, 1),
  colsample_bytree = c(0.5, 0.7, 1),
  min_child_weight = c(1, 5, 10)
)

# Initialize variables to store the best parameters and RMSE
best_params <- list()
best_rmse <- Inf
best_iteration <- 0

total_combinations <- nrow(param_grid)

# Perform grid search
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
  
  cat(sprintf("Trying combination %d of %d: max_depth=%d, eta=%.2f, gamma=%.1f, subsample=%.2f, colsample_bytree=%.2f, min_child_weight=%d\n",
              i, total_combinations,
              param_grid$max_depth[i],
              param_grid$eta[i],
              param_grid$gamma[i],
              param_grid$subsample[i],
              param_grid$colsample_bytree[i],
              param_grid$min_child_weight[i]))
  
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
  
  # Update best parameters if a better RMSE is found
  mean_rmse <- min(cv_results$evaluation_log$test_rmse_mean)
  best_iter <- cv_results$best_iteration
  if (mean_rmse < best_rmse) {
    best_rmse <- mean_rmse
    best_params <- params
    best_iteration <- best_iter
  }
}

# Train the final model using the best parameters
final_model <- xgboost(
  params = best_params,
  data = X_train,
  label = y_train,
  nrounds = best_iteration,
  verbose = 1
)

# Make predictions
y_pred <- predict(final_model, X_test)
rmse <- sqrt(mean((y_test - y_pred)^2))

# Plot the results
ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
  geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
  xlim(0, 1500) +
  ylim(0, 1500) +
  geom_point(size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual ng_g, After Grid Search Tuning", x = "Actual", y = "Predicted") +
  theme_light() +
  coord_fixed(ratio = 1) +
  annotate("text", x = 500, y = Inf, hjust = 1, vjust = 2.5,
           label = paste("RMSE:", round(rmse, 2)),
           color = "black", size = 3.5)

# Display best hyperparameters and RMSE
print(list(
  Best_Parameters = best_params,
  Best_RMSE = best_rmse,
  Best_Iteration = best_iteration
))





