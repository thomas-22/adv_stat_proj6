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
    Timestamp = round_date(dmy(Datum), unit = "week")
  ) %>%
  ggplot() +
  geom_bar(
    aes(x = Timestamp, fill = !is.na(t_)),
    color = "black",
    linewidth = 0.1, just = 1, width = 7
  ) +
  scale_x_date(
    breaks = ymd(c(
      "2020-04-01", "2020-07-01", "2020-10-01",
      "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01",
      "2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01",
      "2023-01-01", "2023-04-01"
    ))
  ) +
  scale_fill_brewer(
    palette = "Set2", name = "Timestamp available",
    labels = c("No", "Yes")
  ) +
  labs(
    x = "Date", y = "Count", color = "",
    title = "Hunting events per week"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )
ggsave(
  "Figures/hunt_per_week.png", p_hunt_per_week,
  width = 6, height = 4, dpi = 300
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
