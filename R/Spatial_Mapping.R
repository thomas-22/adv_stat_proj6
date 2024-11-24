library(sf)
library(dplyr)
library(mapview)
df <- data_movement_raw %>%
  filter(Sender.ID == sample(unique(data_movement_raw$Sender.ID), size = 1)) %>%
  select(Sender.ID, x_, y_)

mysf <- st_as_sf(df %>% slice_sample(prop = .05), coords = c("x_", "y_"), crs = 32633)
mv <- mapview(mysf)

mv

png_file <- tempfile(fileext = ".png")
mapshot(mv, file = "map.png")

mysf


sf2 <- st_as_sf(df, coords = c("x_", "y_"), crs = 32633)
temp <- elevatr::get_elev_point(sf2, src = "aws")

hist(temp$elevation)
