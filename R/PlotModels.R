library(ggeffects)
library(patchwork)

# Plot effects
# plot marginal predictions
# -- given a specific covariate vector (mean, pregnant = FALSE),
#    vary one covariate and see how the prediction changes
plot_predictions <- function(model, covariate, xlab, xmin = NULL, title = "") {
  predict_response(model, terms = covariate) %>%
    plot(show_data = FALSE) +  # turn on to show data points
    labs(title = title, y = "FCM level [ng/g]", x = xlab) +
    theme_bw() +
    ylim(50, 800) +
    if (!is.null(xmin)) xlim(xmin, NA)
}

# Plot effects across datasets
plot_predictions_across_datasets <- function(res, model_type, filter_criterion, x, xlab) {
  model_data <- res[res$filter_criterion == filter_criterion, ]
  predictions <- lapply(1:nrow(model_data), function(i) {
    model <- model_data[i, (model_type), drop = TRUE][[1]]
    tibble(
      gut_retention_time_lower = paste("Gut retention hours = ", res$gut_retention_time_lower[[i]]),
      # gut_retention_time_upper = res$gut_retention_time_upper[[i]],
      distance_threshold = paste("Distance threshold = ", res$distance_threshold[[i]]),
      as_tibble(predict_response(model, terms = x))
    )
  })
  prediction_data <- bind_rows(predictions)

  ggplot() +
    geom_line(data = prediction_data, aes(x = x, y = predicted)) +
    geom_ribbon(data = prediction_data,
      aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    facet_grid(
      gut_retention_time_lower ~ distance_threshold,
      scales = "free_x"
    ) +
    labs(
      title = paste0("Dataset \"", filter_criterion, "\""),
      x = xlab,
      y = "FCM level [ng/g]"
    ) +
    theme_bw() +
    ylim(0, 1000)
}
