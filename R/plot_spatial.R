library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(sf)
library(osmdata)
library(ggspatial)
library(patchwork)

source("R/Datafusion.R")
prepared_data <- suppressWarnings(run_datafusion(remove_outlers = TRUE))

Movement <- prepared_data$Movement
FCMStress <- prepared_data$FCMStress
HuntEvents <- prepared_data$HuntEvents

### construct sf
Movement_sf <- Movement %>%
  dplyr::select(Sender.ID, x_, y_) %>%
  st_as_sf(coords = c("x_", "y_"), crs = 25833)

set.seed(42)
Movement_sf_selected <- Movement %>%
  dplyr::select(Sender.ID, x_, y_) %>%
  # select randomly 3 deer to prevent overplotting
  filter(Sender.ID %in% sample(unique(Movement$Sender.ID), 4)) %>%
  st_as_sf(coords = c("x_", "y_"), crs = 25833)

HuntEvents_sf <- HuntEvents %>%
  drop_na(HuntX, HuntY) %>%
  dplyr::select(HuntX, HuntY) %>%
  st_as_sf(coords = c("HuntX", "HuntY"), crs = 25833)
  
FCM_Samples_sf <- FCMStress %>%
  dplyr::select(DefecX, DefecY) %>%
  st_as_sf(coords = c("DefecX", "DefecY"), crs = 25833)

### get other data from OSM
# National Park
Park <- opq(bbox = 'Bavarian Forest National Park') %>%
  add_osm_feature(key = "boundary", value = "national_park") %>%
  osmdata_sf()
# Bavaria
BAY <- getbb("Bavaria", format_out = "sf_polygon")
# Munich
MUC <- getbb("Munich") %>% 
  opq() %>% 
  add_osm_feature(key = "place", value = "city") %>%
  osmdata_sf()

#### Plots ####
theme_set(theme_light() + 
            theme(plot.title = element_text(hjust = 0.5, size = 16), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank())
)

plot_overview <- ggplot(BAY) +
  geom_sf() +
  geom_sf(data = Park$osm_multipolygons[1, ], fill = "lightgreen") +
  # change color from red to black, because red is used for hunting events
  geom_sf(data = MUC$osm_points, color = "black") +
  geom_sf_text(data = MUC$osm_points, color = "black", aes(label = "Munich"), nudge_y = 0.1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("Location of the Bavarian Forest National Park (Green Area)")

# plot_Movement <- ggplot() +
#   geom_sf(data = Park$osm_multipolygons[1, ]) +
#   geom_sf(data = Movement_sf, aes(color = Sender.ID), alpha = .5) +
#   guides(color = "none") +
#   annotation_scale(location = "bl") +
#   annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
#   ggtitle("Deer Locations")

plot_Movement <- ggplot() +
  geom_sf(data = Park$osm_multipolygons[1, ]) +
  # geom_sf_label(data = Park$osm_multipolygons, aes(label = c("DEU", "CZE"))) +
  # blue for movement data -- also used in the interpolation plot
  geom_sf(data = Movement_sf, color = "blue", alpha = .1, size = .1) +
  facet_wrap(~Sender.ID) +
  guides(color = "none") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("Recorded Locations of 4 Random Deer") +
  theme(strip.text = element_blank())

plot_Hunts <- ggplot() +
  geom_sf(data = Park$osm_multipolygons[1, ]) +
  # red for hunting events -- also used in the interpolation plot
  geom_sf(data = HuntEvents_sf, color = "red", alpha = .5) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("Hunting Event Locations")

plot_samples <- ggplot() +
  geom_sf(data = Park$osm_multipolygons[1, ]) +
  # pruple for FCM samples -- also used in the interpolation plot
  geom_sf(data = FCM_Samples_sf, color = "purple", alpha = .5) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("FCM Sample Locations")

ggsave(plot = plot_overview, filename = "Figures/Maps/overview.png", device = "png", width = 6, height = 6)
ggsave(plot = plot_Movement, filename = "Figures/Maps/Movement.png", device = "png", width = 6, height = 6)
ggsave(plot = plot_Hunts, filename = "Figures/Maps/Hunts.png", device = "png", width = 6, height = 6)
ggsave(plot = plot_samples, filename = "Figures/Maps/Samples.png", device = "png", width = 6, height = 6)


# plot_Movement | plot_Hunts | plot_samples
