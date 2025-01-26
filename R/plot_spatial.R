library(sf)
library(dplyr)
library(osmdata)
library(ggspatial)
library(patchwork)
### construct sf
Movement_sf <- read.csv("data/Movement - CRS=ETRS UTM 33N.csv", 
                        header = TRUE, sep = ";") %>%
  select(Sender.ID, x_, y_) %>%
  st_as_sf(coords = c("x_", "y_"), crs = 32633)

HuntEvents_sf <- read.csv("data/Hunt Events - CRS= ETRS UTM 33N.csv", 
                          header = TRUE, sep = ",") %>%
  select(X, Y) %>% 
  filter(Y < max(Y) & Y > min(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 32633)
  
FCM_Samples_sf <- read.csv("data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv", 
                           header = TRUE, sep = ",") %>%
  select(X, Y) %>%
  filter(Y < max(Y) & Y > min(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 32633)

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
            theme(plot.title = element_text(hjust = 0.5, size = 14), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
)

plot_overview <- ggplot(BAY) +
  geom_sf() +
  geom_sf(data = Park$osm_multipolygons, fill = c("lightgreen", "lightblue")) +
  # geom_sf_label(data = Park$osm_multipolygons, aes(label = c("DEU", "CZE"))) +
  geom_sf(data = MUC$osm_points, color = "red") +
  # geom_sf_text(data = MUC2$osm_points, color = "red", aes(label = "Munich")) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("Location of National Park")

plot_Movement <- ggplot(data = Park$osm_multipolygons) + 
  geom_sf(fill = c("lightgreen", "lightblue")) +
  # geom_sf_label(data = Park$osm_multipolygons, aes(label = c("DEU", "CZE"))) +
  geom_sf(data = Park$osm_lines) +
  geom_sf(data = Movement_sf, aes(color = as.factor(Sender.ID)), alpha = .7) +
  guides(color = "none") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("Location of Deers")

plot_Hunts <- ggplot(data = Park$osm_multipolygons) + 
  geom_sf(fill = c("lightgreen", "lightblue")) +
  geom_sf(data = Park$osm_lines) +
  geom_sf(data = HuntEvents_sf, color = "black", alpha = .7, shape = 4) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("Location of Hunting Events")

plot_samples <- ggplot(data = Park$osm_multipolygons) + 
  geom_sf(fill = c("lightgreen", "lightblue")) +
  geom_sf(data = Park$osm_lines) +
  geom_sf(data = FCM_Samples_sf, color = "black", alpha = .7, shape = 2) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  ggtitle("Sample Locations")

ggsave(plot = plot_overview, filename = "Figures/Maps/overview.png", device = "png")
ggsave(plot = plot_Movement, filename = "Figures/Maps/Movement.png", device = "png")
ggsave(plot = plot_Hunts, filename = "Figures/Maps/Hunts.png", device = "png")
ggsave(plot = plot_samples, filename = "Figures/Maps/Samples.png", device = "png")


# plot_Movement | plot_Hunts | plot_samples
