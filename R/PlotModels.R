library(ggeffects)
library(patchwork)

# Plot effects
# plot marginal predictions
# -- given a specific covariate vector (median), vary one covariate and see how the prediction changes
plot_predictions <- function(model, covariate, xlab, xmin = NULL, title = "") {
  predict_response(model, terms = covariate) %>%
    plot(show_data = FALSE) +  # turn on to show data points
    labs(title = title, y = "FCM level [ng/g]", x = xlab) +
    theme_bw() +
    ylim(50, 800) +
    if (!is.null(xmin)) xlim(xmin, NA)
}

# # Plot all effects
# plot_model <- function(model, gut_retention_hours) {
#   plots <- list(
#     plot_predictions(model, covariate = "TimeDiff", xlab = "Time difference [hours]", xmin = gut_retention_hours),
#     plot_predictions(model, covariate = "Distance", xlab = "Distance [km]"),
#     plot_predictions(model, covariate = "SampleDelay", xlab = "Sample delay [hours]"),
#     plot_predictions(model, covariate = "NumOtherHunts", xlab = "Other hunting events"),
#     plot_predictions(model, covariate = "Pregnant", xlab = "Pregnant"),
#     plot_predictions(model, covariate = "Season", xlab = "Season")
#   )
#   wrap_plots(plots)
# }
