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

# -------------------------
# Data Analysis of FCM Samples
# -------------------------
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
    labs(x = "", y = "Count") +
    scale_x_date(
      date_breaks = "6 months",
      limits = as_date(c("2020-04-01", "2022-12-01")),
      date_labels = "%B '%y",
      guide = guide_axis(angle = 45)
    ) +
    scale_y_continuous(breaks = seq(0, 14, by = 2)) +
    theme_bw(base_size = 16) +
    theme(panel.grid.minor = element_blank())
  
  # hunt_dates <- HuntEvents %>% na.omit() %>%
  #   mutate(nHunts = n(), .by = "HuntDate") %>%
  #   ggplot() +
  #   geom_segment(aes(x = HuntDate, y = nHunts, yend = 0), color = "red") +
  #   labs(x = "", y = "Count", title = "Daily Count of Hunting Events") +
  #   scale_x_date(
  #     date_breaks = "6 months",
  #     limits = as_date(c("2020-04-01", "2022-12-01")),
  #     guide = guide_axis(angle = 45)
  #   ) +
  #   scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  #   theme_bw(base_size = 16)
  
  # fcm_hunt_dates <- fcm_daily_count / hunt_dates +
  #   plot_layout(axis_titles = "collect")
  
  plots <- list(
    fcm_sample_times = fcm_sample_times,
    fcm_levels = fcm_levels,
    fcm_daily_count = fcm_daily_count
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

# plot.fcm.data(save = TRUE)

# -------------------------
# Data Analysis of Hunting Events
# -------------------------
plot.hunt.events <- function(save = FALSE) {
  hunts_incomplete_timestamp <- HuntEvents %>%
    mutate(missing.time = ifelse(is.na(HuntTime), "Timestamp\n incomplete", "Timestamp\n complete")) %>%
    ggplot(aes(y = missing.time)) +
    geom_bar(aes(fill = missing.time)) +
    theme_bw(base_size = 16) +
    guides(fill = "none") +
    labs(title = "", x = "", y = "") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank())
  
  hunt_dates <- HuntEvents %>% na.omit() %>%
    mutate(nHunts = n(), .by = "HuntDate") %>%
    ggplot() +
    geom_segment(aes(x = HuntDate, y = nHunts, yend = 0), color = "red") +
    labs(x = "", y = "Count") +
    scale_x_date(
      date_breaks = "6 months",
      limits = as_date(c("2020-04-01", "2022-12-01")),
      date_labels = "%B '%y",
      guide = guide_axis(angle = 45)
    ) +
    scale_y_continuous(breaks = seq(0, 14, by = 2)) +
    theme_bw(base_size = 16) +
    theme(panel.grid.minor = element_blank())
  
  # save plots as list
  plots <- list(
    hunts_incomplete_timestamp = hunts_incomplete_timestamp,
    hunt_dates = hunt_dates
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

# plot.hunt.events(save = TRUE)

