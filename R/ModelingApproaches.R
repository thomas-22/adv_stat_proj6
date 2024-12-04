library(xgboost)
#install.packages("caTools")
library(caTools)
library(ggplot2)

# Split the data (70% training, 30% testing)
split <- sample.split(y, SplitRatio = 0.7)
train_data <- subset(FCMData_Assigned, split == TRUE)
test_data <- subset(FCMData_Assigned, split == FALSE)

# Prepare the data for XGBoost
X_train <- data.matrix(train_data[, c("TimeDiff", "Distance")])
y_train <- train_data$ng_g

X_test <- data.matrix(test_data[, c("TimeDiff", "Distance")])
y_test <- test_data$ng_g

# Set parameters for XGBoost
params <- list(
  objective = "reg:squarederror",  # Regression task
  eval_metric = "rmse",            # RMSE (Root Mean Squared Error) for evaluation
  max_depth = 7,                   # Maximum tree depth
  eta = 0.15,                       # Learning rate
  nthread = 2                      # Number of threads for parallel computation
)

# Train the model
xgb_model <- xgboost(
  params = params, 
  data = X_train, 
  label = y_train, 
  nrounds = 10000,
  verbose = 1,
  early_stopping_rounds = 10
)

y_pred <- predict(xgb_model, X_test)
rmse <- numeric(2)
rmse[1] <- sqrt((sum(((y_test-y_pred)^2))/length(y_test)))

ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
  geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
  xlim(0,1500) +
  ylim(0,1500) +
  geom_point(size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual ng_g, Before Tuning", x = "Actual", y = "Predicted") +
  theme_light() +
  coord_fixed(ratio = 1) +
  annotate("text", x = 500, y = Inf, hjust = 1, vjust = 2.5,
           label = paste("RMSE:", round(rmse[1], 2)),
           color = "black", size = 3.5)

# Perform Cross-validation to tune the model
cv_results <- xgb.cv(
  params = params,
  data = X_train,
  label = y_train,
  nrounds = 10000,
  nfold = 5,        # 5-fold cross-validation
  verbose = 1,
  early_stopping_rounds = 10
)


final_model <- xgboost(
  params = params,
  data = X_train,
  label = y_train,
  nrounds = cv_results$best_iteration,
  verbose = 1
)
y_pred <- predict(final_model, X_test)
rmse[2] <- sqrt((sum(((y_test-y_pred)^2))/length(y_test)))
rmse_results <- data.frame(
  Tuning = c("Before", "After"),
  RMSE = rmse
)

ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
  geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
  xlim(0,1500) +
  ylim(0,1500) +
  geom_point(size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual ng_g, After Tuning", x = "Actual", y = "Predicted") +
  theme_light() +
  coord_fixed(ratio = 1) +
  annotate("text", x = 500, y = Inf, hjust = 1, vjust = 2.5,
           label = paste("RMSE:", round(rmse[2], 2)),
           color = "black", size = 3.5)

rmse_results




