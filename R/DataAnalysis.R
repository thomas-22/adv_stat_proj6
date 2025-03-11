# -------------------------
# generates plots for descriptive analytics
# methods: show / return / save
# -------------------------
analytics <- function(data, method = "show") {
  checkmate::assertList(data)
  lapply(data, checkmate::assertDataFrame)
  checkmate::assertSubset(c("Movement", "FCMStress", "HuntEvents"), choices = names(data))
  checkmate::assertSubset(method, choices = c("save", "return", "show"))
  
  Movement <- data$Movement
  FCMStress <- data$FCMStress
  HuntEvents <- data$HuntEvents
  
  # -------------------------
  # FCM Samples times per deer
  # -------------------------
  samples_dates_per_deer <- ggplot(FCMStress) +
    geom_line(aes(x = DefecTime, y = Deer.ID)) +
    geom_point(aes(x = DefecTime, y = Deer.ID)) +
    labs(x = "Defecation time", y = "Deer", title = "Samples per day and deer") +
    scale_y_discrete(limits = rev(deer_order)) +
    theme_bw(base_size = 16) +
    theme(axis.text.y = element_blank())
  
  # -------------------------
  # FCM Samples levels per deer
  # -------------------------
  samples_levels_per_deer <- ggplot(FCMStress) +
    geom_boxplot(aes(y = Deer.ID, x = ng_g)) +
    labs(y = "Deer", x = "FCM level [ng/g]", title = "FCM level per deer") +
    scale_y_discrete(limits = deer_order) +
    theme_bw(base_size = 16) +
    theme(axis.text.y = element_blank())
  
  # -------------------------
  # FCM Samples count per day
  # -------------------------
  samples_daily_count <- FCMStress %>%
    mutate(nSamples = n(), .by = "DefecDate") %>%
    ggplot() +
    geom_segment(aes(x = DefecDate, y = nSamples, yend = 0), color = "purple") +
    labs(x = "", y = "Count", title = "Number of Samples per day") +
    scale_x_date(
      date_breaks = "6 months",
      limits = as_date(c("2020-04-01", "2022-12-01")),
      date_labels = "%B '%y",
      guide = guide_axis(angle = 45)
    ) +
    scale_y_continuous(breaks = seq(0, 14, by = 2)) +
    theme_bw(base_size = 16) +
    theme(panel.grid.minor = element_blank())
  
  # -------------------------
  # Hunts: observations w/ complete timestamp
  # -------------------------
  hunts_incomplete_timestamp <- HuntEvents %>%
    mutate(missing.time = ifelse(is.na(HuntTime), "Timestamp\n incomplete", "Timestamp\n complete")) %>%
    ggplot(aes(y = missing.time)) +
    geom_bar(aes(fill = missing.time)) +
    theme_bw(base_size = 16) +
    guides(fill = "none") +
    labs(x = "", y = "", title = "Hunt Events:\ncomplete vs. incomplete timestamp") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank())
  
  # -------------------------
  # Hunts: count per day
  # -------------------------
  hunts_dates <- HuntEvents %>% na.omit() %>%
    mutate(nHunts = n(), .by = "HuntDate") %>%
    ggplot() +
    geom_segment(aes(x = HuntDate, y = nHunts, yend = 0), color = "red") +
    labs(x = "", y = "Count", title = "Number of Hunt Events per day") +
    scale_x_date(
      date_breaks = "6 months",
      limits = as_date(c("2020-04-01", "2022-12-01")),
      date_labels = "%B '%y",
      guide = guide_axis(angle = 45)
    ) +
    scale_y_continuous(breaks = seq(0, 14, by = 2)) +
    theme_bw(base_size = 16) +
    theme(panel.grid.minor = element_blank())
  
  # -------------------------
  # construct sf object for movement data, but only select 4 random deer,
  # to prevent overfitting
  # -------------------------
  set.seed(42)
  Movement_sf_selected_4 <- Movement %>%
    select(Sender.ID, x_, y_) %>%
    filter(Sender.ID %in% sample(unique(Movement$Sender.ID), 4)) %>%
    st_as_sf(coords = c("x_", "y_"), crs = 25833)
  
  # -------------------------
  # construct sf object for Hunting data
  # -------------------------
  HuntEvents_sf <- HuntEvents %>%
    drop_na() %>%
    dplyr::select(HuntX, HuntY) %>%
    st_as_sf(coords = c("HuntX", "HuntY"), crs = 25833)
  
  # -------------------------
  # construct sf object for Samples data
  # -------------------------
  FCM_Samples_sf <- FCMStress %>%
    dplyr::select(DefecX, DefecY) %>%
    st_as_sf(coords = c("DefecX", "DefecY"), crs = 25833)
  
  # -------------------------
  # get data from OpenStreetMap (OSM)
  # -------------------------
  # National Park Area
  Park <- opq(bbox = 'Bavarian Forest National Park') %>%
    add_osm_feature(key = "boundary", value = "national_park") %>%
    osmdata_sf()
  # Bavaria Area
  BAY <- getbb("Bavaria", format_out = "sf_polygon")
  # Munich
  MUC <- getbb("Munich") %>% 
    opq() %>% 
    add_osm_feature(key = "place", value = "city") %>%
    osmdata_sf()
  
  # -------------------------
  # plot location of national park within bavaria
  # lightgreen: location of park
  # -------------------------
  sp_park_location <- ggplot(BAY) +
    geom_sf() +
    geom_sf(data = Park$osm_multipolygons[1, ], fill = "lightgreen") +
    geom_sf(data = MUC$osm_points, color = "black") +
    geom_sf_text(data = MUC$osm_points, color = "black", aes(label = "Munich"), nudge_y = 0.1) +
    annotation_scale(location = "br") +
    annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
    ggtitle("Location of the Bavarian Forest National Park (Green Area)") +
    theme_spatial()
  
  # -------------------------
  # plot movement of selected deer in park
  # blue: deer locations
  # -------------------------
  sp_movement_4 <- ggplot() +
    geom_sf(data = Park$osm_multipolygons[1, ]) +
    geom_sf(data = Movement_sf_selected_4, color = "blue", alpha = .1, size = .1) +
    facet_wrap(~Sender.ID, ncol = 2) +
    guides(color = "none") +
    annotation_scale(location = "bl") +
    ggtitle("Recorded Locations of four Random Deer") +
    theme_spatial() +
    theme(strip.text = element_blank())
 
  # -------------------------
  # plot Hunting events in the park
  # red: hunting events
  # -------------------------
  sp_hunts <- ggplot() +
    geom_sf(data = Park$osm_multipolygons[1, ]) +
    geom_sf(data = HuntEvents_sf, color = "red", alpha = .5) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
    ggtitle("Hunting Event Locations") +
    theme_spatial()
  
  # -------------------------
  # plot location of samples in the park
  # purple: sample locations
  # -------------------------
  sp_samples <- ggplot() +
    geom_sf(data = Park$osm_multipolygons[1, ]) +
    geom_sf(data = FCM_Samples_sf, color = "purple", alpha = .5) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
    ggtitle("Faecal Sample Locations") +
    theme_spatial()
  
  # -------------------------
  # plot location of hunting events vs location of 4 random deer in the park
  # blue: deer locations
  # red: hunting events
  # -------------------------
  sp_hunts_vs_deer_4 <- ggplot() +
    geom_sf(data = Park$osm_multipolygons[1, ]) +
    geom_sf(data = Movement_sf_selected, aes(color = "Deer"), alpha = .1, size = .1) +
    geom_sf(data = HuntEvents_sf, aes(color = "Hunting Event"), alpha = .5, size = .5) +
    facet_wrap(~Sender.ID, ncol = 2) +
    scale_color_manual(
      values = c("blue", "red"),
      labels = c("deer", "hunting event")
    ) +
    guides(
      color = guide_legend(override.aes = list(alpha = 1, size = 2), title = "")
    ) +
    annotation_scale(location = "bl") +
    ggtitle("Hunting Events and four Random Deer") +
    theme_spatial() +
    theme(strip.text = element_blank(), legend.position = "bottom")
  
  # -------------------------
  # bind plots to list
  # -------------------------
  plots <- list(
    samples_dates_per_deer = samples_dates_per_deer,
    samples_levels_per_deer = samples_levels_per_deer,
    samples_daily_count = samples_daily_count,
    hunts_incomplete_timestamp = hunts_incomplete_timestamp,
    hunts_dates = hunts_dates,
    sp_park_location = sp_park_location,
    sp_movement_4 = sp_movement_4,
    sp_hunts = sp_hunts,
    sp_samples = sp_samples,
    sp_hunts_vs_deer_4 = sp_hunts_vs_deer_4
  )
  
  # -------------------------
  # depending on chosen method, save/return/show plots
  # -------------------------
  if (method == "save") {
    for (i in seq_along(plots)) {
      name = names(plots)[[i]]
      cat("saving", name, "\n")
      ggsave(plot = plots[[i]] + ggtitle(""), filename = paste0("Figures/", name, ".png"), device = "png")
    }
  } else if(method == "return") {
    return(plots)
  } else {
    cat("if you want to save the plots to /Figures, choose method = 'save'!\n")
    cat("if you want the function to return the plots, choose method = 'return'!\n")
    for (plot in plots) {
      print(plot)
      readkey()
    }
  }
}
