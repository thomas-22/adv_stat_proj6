source("R/settings.R")
source("R/source.all.R")

prepared_data <- suppressWarnings(run_datafusion())

Movement <- prepared_data$Movement
FCMStress <- prepared_data$FCMStress
HuntEvents <- prepared_data$HuntEvents


check.sanity <- function() {
  cat("Summary of HuntEvents\n--------\n")
  print(summary(HuntEvents))
  readkey()
  cat("Summary of FCM Stress\n--------\n")
  print(summary(FCMStress))
  readkey()
  cat("Summary of Movement Data\n--------\n")
  print(summary(Movement))
}


plot.fcm.data <- function(save = FALSE) {
  fcm_sample_times <- ggplot(FCMStress) +
    geom_line(aes(x = DefecTime, y = Deer.ID)) +
    geom_point(aes(x = DefecTime, y = Deer.ID)) +
    labs(x = "Defecation time", y = "Deer") +
    scale_y_discrete(limits = rev(deer_order)) +
    theme_bw(base_size = 16) +
    theme(axis.text.y = element_blank())

  fcm_levels <- ggplot(FCMStress) +
    geom_boxplot(aes(x = Deer.ID, y = ng_g)) +
    labs(x = "Deer", y = "FCM level [ng/g]") +
    scale_x_discrete(limits = deer_order) +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_blank())
  
  fcm_daily_count <- FCMStress %>%
    mutate(nSamples = n(), .by = "DefecDate") %>%
    ggplot() +
    geom_segment(aes(x = DefecDate, y = nSamples, yend = 0), color = "purple") +
    labs(x = "", y = "Count", title = "Daily Count of FCM Samples") +
    scale_x_date(
      date_breaks = "6 months",
      limits = as_date(c("2020-04-01", "2022-12-01"))
    ) +
    scale_y_continuous(breaks = seq(0, 14, by = 2)) +
    theme_bw(base_size = 16)
  
  hunt_dates <- HuntEvents %>% na.omit() %>%
    mutate(nHunts = n(), .by = "HuntDate") %>%
    ggplot() +
    geom_segment(aes(x = HuntDate, y = nHunts, yend = 0), color = "red") +
    labs(x = "", y = "Count", title = "Daily Count of Hunting Events") +
    scale_x_date(
      date_breaks = "6 months",
      limits = as_date(c("2020-04-01", "2022-12-01"))
    ) +
    scale_y_continuous(breaks = seq(0, 14, by = 2)) +
    theme_bw(base_size = 16)
  
  fcm_hunt_dates <- fcm_daily_count / hunt_dates +
    plot_layout(axis_titles = "collect")
  
  plots <- list(
    fcm_sample_times = fcm_sample_times,
    fcm_levels = fcm_levels,
    fcm_hunt_dates = fcm_hunt_dates
  )
  
  if (save) {
    for (i in seq_along(plots)) {
      name = names(plots)[[i]]
      ggsave(plot = plots[[i]], filename = paste0("Figures/", name, ".png"), device = "png")
    }
  } else {
    for (plot in plots) {
      print(plot)
      readkey()
    }
  }
}
