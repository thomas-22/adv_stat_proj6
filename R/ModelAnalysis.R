# -------------------------
# generates gratia-diagnostics for multiple models
# methods: show / return / save
# -------------------------
plot_diagnostics_gratia <- function(data, method = "show") {
  checkmate::assertDataFrame(data)
  checkmate::assertSubset(c("fit", "filter_criterion", "method"), choices = names(data))
  checkmate::assertSubset(method, choices = c("show", "return", "save"))
  
  diagnostics <- list()
  for (i in seq_len(nrow(data))) {
    diagnostics[[i]] <- gratia::appraise(data[i, ]$fit[[1]], method = "simulate") & theme_bw()
  }
  names(diagnostics) <- paste0(data$filter_criterion, "_", data$method)
  
  if (method == "show") {
    cat("if you want to save the plots to /Figures, choose method = 'save'!\n")
    cat("if you want the function to return the plots, choose method = 'return'!\n\n")
    for (i in seq_along(diagnostics)) {
      cat(sprintf("Diagnostic Plot:\nfilter criterion used: %s\nmethod used: %s\n\n",
                  data[i, ]$filter_criterion,
                  data[i, ]$method))
      print(diagnostics[[i]])
      readkey()
    }
  } else if (method == "return") {
    return(diagnostics)
  } else {
    for (i in seq_len(nrow(fits))) {
      filename <- sprintf("Figures/Models/%s_%s_diagnostic.png", fits[i, ]$filter_criterion, fits[i, ]$method)
      ggsave(filename = filename, plot = diagnostics[[i]], width = 7, height = 7, dpi = 300)
    }
  }
}

plot_diagnostics_custom <- function(data, method = "show") {
  checkmate::assertDataFrame(data)
  checkmate::assertSubset(c("fit", "filter_criterion", "method"), choices = names(data))
  checkmate::assertSubset(method, choices = c("show", "return", "save"))
  
  diagnostics <- list()
  for (i in seq_len(nrow(data))) {
    # Predicted values of the FCM level for a "typical deer", i.e.,
    # the other covariates are held constant at their mean values or reference category
    # (hasCalf = FALSE).
    # (Random effect is set to that of one deer, but since it's just an intercept,
    # it is irrelevant to our interpretation of the overall shape/tendency of the curve.)
    TimeDiff <- ggpredict(data[i, ]$fit[[1]], terms = c("TimeDiff")) %>%
      plot() +
      labs(y = "FCM level [ng/g]", x = "Time difference [hours]", title = "") +
      theme_bw(base_size = 16) +
      coord_cartesian(ylim = c(150, 600))
    
    Distance <- ggpredict(data[i, ]$fit[[1]], terms = c("Distance")) %>%
      plot() +
      labs(y = "FCM level [ng/g]", x = "Distance [km]", title = "") +
      theme_bw(base_size = 16) +
      coord_cartesian(ylim = c(150, 600))
    
    SampleDelay <- ggpredict(data[i, ]$fit[[1]], terms = c("SampleDelay")) %>%
      plot() +
      labs(y = "FCM level [ng/g]", x = "Sample delay [hours]", title = "") +
      theme_bw(base_size = 16) +
      coord_cartesian(ylim = c(150, 600))
    
    # Day <- ggpredict(data[i, ]$fit[[1]], terms = c("DefecDay"), title = "") %>%
    #   plot() +
    #   labs(y = "FCM level [ng/g]", x = "Defecation day", title = "") +
    #   theme_bw(base_size = 16)
    
    diagnostics[[i]] <- TimeDiff + Distance + SampleDelay +
      plot_layout(ncol = 3, axis_titles = "collect") #+
      # plot_annotation(title = sprintf("Filter used: %s; Method used: %s",
      #                                            data[i, ]$filter_criterion[[1]],
      #                                            data[i, ]$method[[1]]))
    #names(diagnostics[[i]]) <- paste0(data[i, ]$filter_criterion, "_", data[i, ]$method)
  }
  names(diagnostics) <- paste0(data$filter_criterion, "_", data$method)
  
  if (method == "show") {
    cat("if you want to save the plots to /Figures, choose method = 'save'!\n")
    cat("if you want the function to return the plots, choose method = 'return'!\n\n")
    for (i in seq_along(diagnostics)) {
      cat(sprintf("Diagnostic Plot:\nfilter criterion used: %s\nmethod used: %s\n\n",
                  data[i, ]$filter_criterion,
                  data[i, ]$method))
      print(diagnostics[[i]])
      readkey()
    }
  } else if (method == "return") {
    return(diagnostics)
  } else {
    for (i in seq_len(nrow(fits))) {
      filename <- sprintf("Figures/Models/%s_%s_diagnostic_custom.png", fits[i, ]$filter_criterion, fits[i, ]$method)
      ggsave(filename = filename, plot = diagnostics[[i]], width = 12, height = 6, dpi = 300)
    }
  }
}

plot_partial_effects <- function(data, features = NULL, method = "show") {
  checkmate::assertDataFrame(data)
  checkmate::assertSubset(c("fit", "filter_criterion", "method"), choices = names(data))
  checkmate::assertSubset(method, choices = c("show", "return", "save"))
  ifelse(is.null(features), features <- 1:5, features)
  partial_effect <- list()
  for (k in seq_along(features)) {
    plots <- list()
    for (i in seq_len(nrow(data))) {
      plots[[i]] <- gratia::draw(data[i, ]$fit[[1]], select = features[[k]])+
        theme_bw(base_size = 16) +
        labs(subtitle = sprintf("Dataset: \"%s\"\nMethod: \"%s\"", data[i, ]$filter_criterion, data[i, ]$method))
    }
    partial_effect[[k]] <- wrap_plots(plots, ncol = 3, byrow = FALSE)
  }
  if (method == "show") {
    cat("if you want to save the plots to /Figures/Models, choose method = 'save'!\n")
    cat("if you want the function to return the plots, choose method = 'return'!\n\n")
    for (i in seq_along(partial_effect)) {
      cat(sprintf("Partial effect plot of feature %d\n", i))
      print(partial_effect[[i]])
      readkey()
    }
  } else if (method == "return") {
    return(partial_effect)
  } else {
    for (i in seq_len(nrow(fits))) {
      filename <- sprintf("Figures/Models/partial_effect_feature_%d.png", i)
      ggsave(filename = filename, plot = partial_effect[[i]], width = 12, height = 6, dpi = 300)
    }
  }
}


# Plot effects
# plot marginal predictions
# -- given a specific covariate vector (mean, pregnant = FALSE),
#    vary one covariate and see how the prediction changes
plot_predictions <- function(model, covariate, xlab, xmin = NULL, title = "") {
  predict_response(model, terms = covariate) %>%
    plot(show_data = FALSE) +  # turn on to show data points
    labs(title = title, y = "FCM level [ng/g]", x = xlab) +
    theme_bw() +
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
    facet_wrap(~gut_retention_time_lower, scales = "free") +
    labs(
      title = paste0("Dataset \"", filter_criterion, "\""),
      x = xlab,
      y = "FCM level [ng/g]"
    ) +
    theme_bw()
}
