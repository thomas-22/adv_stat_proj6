library(ggplot2)
library(patchwork)

# plot all hunting events with time and FCM samples
p_hunt_fcm_locations <- ggplot() +
  geom_point(
    data = HuntEventsreduced,
    mapping = aes(x = X / 1000, y = Y / 1000, color = "Hunting event"),
    alpha = 0.5, size = 0.5
  ) +
  geom_point(
    data = FCMStress,
    mapping = aes(x = X / 1000, y = Y / 1000, color = "FCM sample"),
    alpha = 0.5, size = 0.5
  ) +
  theme_light() +
  scale_color_manual(
    values = c("Hunting event" = "brown1", "FCM sample" = "skyblue")
  ) +
  labs(
    x = "X [km]", y = "Y [km]", color = "",
    title = "Locations of hunting events and FCM samples"
  )
ggsave(
  "Figures/hunt_fcm_locations.png",
  p_hunt_fcm_locations,
  width = 6, height = 4, dpi = 300
)

p_hunt_fcm_locations_same_scale <- ggplot() +
  geom_point(
    data = HuntEventsreduced,
    mapping = aes(x = X / 1000, y = Y / 1000, color = "Hunting event"),
    alpha = 0.5, size = 0.5
  ) +
  geom_point(
    data = FCMStress,
    mapping = aes(x = X / 1000, y = Y / 1000, color = "FCM sample"),
    alpha = 0.5, size = 0.5
  ) +
  theme_light() +
  scale_color_manual(
    values = c("Hunting event" = "brown1", "FCM sample" = "skyblue")
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = "X [km]", y = "Y [km]", color = "",
    title = "Locations of hunting events\nand FCM samples"
  )
ggsave(
  "Figures/hunt_fcm_locations_same_scale.png",
  p_hunt_fcm_locations_same_scale,
  width = 3, height = 4, dpi = 300
)

# remove outliers and make aspect ratio 1
p_hunt_fcm_locations_cropped <- ggplot() +
  geom_point(
    data = HuntEventsreduced,
    mapping = aes(x = X / 1000, y = Y / 1000, color = "Hunting event"),
    alpha = 0.5, size = 0.5
  ) +
  geom_point(
    data = FCMStress,
    mapping = aes(x = X / 1000, y = Y / 1000, color = "FCM sample"),
    alpha = 0.5, size = 0.5
  ) +
  theme_light() +
  scale_color_manual(
    values = c("Hunting event" = "brown1", "FCM sample" = "skyblue")
  ) +
  labs(
    x = "X [km]", y = "Y [km]", color = "",
    title = "Locations of hunting events and FCM samples\n(cropped)"
  ) +
  coord_fixed(ratio = 1, xlim = c(365, 400), ylim = c(5410, 5445))
ggsave(
  "Figures/hunt_fcm_locations_cropped.png",
  p_hunt_fcm_locations_cropped,
  width = 6, height = 4, dpi = 300
)

# # hunting events with vs. without time
# ggplot() +
#   geom_point(
#     data = HuntEvents,
#     mapping = aes(x = X / 1000, y = Y / 1000, color = !is.na(t_)),
#     alpha = 0.4
#   ) +
#   theme_light() +
#   scale_color_brewer(
#     palette = "Set2", name = "Timestamp available",
#     labels = c("No", "Yes")
#   ) +
#   labs(
#     x = "X [km]", y = "Y [km]", color = "Timestamp available",
#     title = "Locations of hunting events with or without timestamp"
#   )

# when were hunting events recorded?
p_hunt_per_week <- HuntEvents %>%
  mutate(
    Timestamp = round_date(dmy(Datum), unit = "week", week_start = "Monday")
  ) %>%
  ggplot(aes(x = Timestamp, fill = !is.na(t_))) +
  geom_bar(
    color = "black",
    linewidth = 0.1, just = 1, width = 7
  ) +
  scale_x_date(
    breaks = ymd(c(
      "2020-04-01", "2020-07-01", "2020-10-01",
      "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01",
      "2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01",
      "2023-01-01", "2023-04-01"
    )),
    limits = ymd(c("2020-03-01", "2023-05-01"))
  ) +
  scale_fill_brewer(
    palette = "Set2", name = "Time recorded",
    labels = c("No", "Yes")
  ) +
  labs(
    x = "Date", y = "Frequency", color = "",
    title = "Hunting events per week"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  )
ggsave(
  "Figures/hunt_per_week.png", p_hunt_per_week,
  width = 6, height = 4, dpi = 300, bg = "white"
)

# FCM samples per week for comparison
p_fcm_per_week <- FCMStress %>%
  mutate(
    Timestamp = round_date(as_date(Collar_t_), unit = "week", week_start = "Monday")
  ) %>%
  ggplot(aes(x = Timestamp)) +
  geom_bar(
    color = "black", fill = "lightblue",
    linewidth = 0.1, just = 1, width = 7
  ) +
  scale_x_date(
    breaks = ymd(c(
      "2020-04-01", "2020-07-01", "2020-10-01",
      "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01",
      "2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01",
      "2023-01-01", "2023-04-01"
    )),
    limits = ymd(c("2020-03-01", "2023-05-01"))
  ) +
  labs(
    title = "FCM samples per week",
    x = "Date",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =  10))
ggsave(
  "Figures/fcm_per_week.png", p_fcm_per_week,
  width = 6, height = 4, dpi = 300, bg = "white"
)

# previous two plots combined
p_fcm_hunt_per_week_combined <- (p_fcm_per_week + theme(
  axis.text.x =  element_blank(),
  axis.title.x = element_blank()
)) / (p_hunt_per_week +
  theme(
    axis.text.x = element_text(size = 9, hjust = 1, angle = 30),
    axis.title.x = element_blank()
  )) +
  plot_layout(axis_titles = "collect_y")
ggsave(
  "Figures/fcm_hunt_per_week_combined.png", p_fcm_hunt_per_week_combined,
  width = 8, height = 6, dpi = 300, bg = "white"
)

# at what time of day did hunting events occur?
p_hunt_per_hour <- HuntEventsreduced %>%
  mutate(Hour = hour(t_)) %>%
  ggplot() +
  geom_bar(
    aes(x = Hour),
    color = "black", fill = "#fc8d62",
    linewidth = 0.1, just = 0, width = 1
  ) +
  theme_light() +
  labs(
    x = "Hour of day", y = "Count", color = "",
    title = "Hunting events per hour of day (only events with timestamp)"
  ) +
  scale_x_continuous(breaks = 0:24, minor_breaks = NULL) +
  theme(legend.position = "bottom")
ggsave(
  "Figures/hunt_per_hour.png", p_hunt_per_hour,
  width = 6, height = 4, dpi = 300
)


#Draws a plot to illustrate why and how we interpolate the positions of the deer
Draw_Illustration_Map <- function(){
  row_index <- 7
  row_data <- interpolated_positions[row_index, ]
  
  plot_df <- data.frame(
    x = c(row_data$HuntEventX, row_data$x_before, row_data$x_after, row_data$InterpolatedX),
    y = c(row_data$HuntEventY, row_data$y_before, row_data$y_after, row_data$InterpolatedY),
    label = c("Hunting\nEvent", "Deer\nBefore", "Deer\nAfter", "Interpolated")
  )
  
  mid_x <- (row_data$HuntEventX + row_data$InterpolatedX) / 2
  mid_y <- (row_data$HuntEventY + row_data$InterpolatedY) / 2
  
  dx <- row_data$InterpolatedX - row_data$HuntEventX
  dy <- row_data$InterpolatedY - row_data$HuntEventY
  
  offset_ratio <- -0.05
  label_x <- mid_x + offset_ratio * dx
  label_y <- mid_y
  
  ggplot(plot_df, aes(x = x, y = y, color = label)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = label,
          vjust = ifelse(label %in% c("Deer\nBefore", "Deer\nAfter"), 1.5, -1)),
      show.legend = FALSE
    ) +
    geom_segment(
      aes(
        x = row_data$HuntEventX,
        y = row_data$HuntEventY,
        xend = row_data$InterpolatedX,
        yend = row_data$InterpolatedY
      ),
      linetype = "dashed",
      color = "black",
      inherit.aes = FALSE
    ) +
    
    annotate(
      "text",
      x = label_x,
      y = label_y,
      label = "Distance",
      color = "black",
      vjust = -0.5
    ) +
    
    labs(
      title = "Position Interpolation Illustration",
      x = "X",
      y = "Y"
    ) +
    
    # "zoom out" on the map
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = 0.2)) +
    
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}


