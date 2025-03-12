read.FCMStress <- function(path) {
  read.csv(path, header = TRUE, sep = ",") %>%
    as_tibble() %>%
    mutate(DefecTime = paste(Collar_day, Collar_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           DefecDate = as_date(DefecTime),
           SampleTime = paste(Waypoint_day, Waypoint_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           SampleDate = dmy(Waypoint_day),
           Sender.ID = as.factor(Sender.ID),
           Deer.ID = as.factor(HairID)) %>%
    rename(Sample.ID = Sample_ID,
           DefecX = X,
           DefecY = Y) %>%
    # Filter out error
    filter(ng_g > 0) %>%
    # If there are multiple FCM measurements from the same sample, take the mean
    # (only affects 2 samples)
    group_by(Sample.ID, Sender.ID) %>%
    mutate(ng_g = mean(ng_g)) %>%
    ungroup() %>%
    distinct(
      Sample.ID, Sender.ID, Deer.ID,
      # FCM level (our response variable)
      ng_g,
      # info about defecation event
      DefecTime, DefecDate, DefecX, DefecY,
      # info about sample collection
      SampleDate, SampleTime
    )
}


read.Reproduction <- function(path, deer.id.levels = NULL) {
  readxl::read_excel(path) %>%
    mutate(Deer.ID = factor(`Genetic_id`, levels = deer.id.levels)) %>%
    distinct(Deer.ID, preg_year, calf)
}

read.Movement <- function(path, sender.id.levels = NULL) {
  read.csv(path, header = TRUE, sep = ";") %>%
    as_tibble() %>%
    mutate(Sender.ID = factor(Sender.ID, levels = sender.id.levels),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    group_by(Sender.ID) %>%
    mutate(DistanceTraveled = sqrt((x_ - lag(x_))^2 + (y_ - lag(y_))^2)) %>%
    mutate(DistanceTraveled = ifelse(is.na(DistanceTraveled), 0, DistanceTraveled)) %>%
    ungroup()
}

read.HuntEvents <- function(path) {
  HuntEvents <- read.csv(path, header = TRUE, sep = ";") %>%
    as_tibble() %>%
    mutate(HuntTime = paste(Datum, Zeit, sep = " ") %>%
             parse_date_time(orders = "%d/%m/%Y %h:%M"),
           HuntDate = dmy(Datum)) %>%
    arrange(HuntDate) %>%
    mutate(
      lon = as.numeric(Laengengrad_wgs), # one entry is "13.NA", i.e., NA
      lat = as.numeric(Breitengrad_wgs),
    ) %>%
    # Remove wrong entries
    mutate(
      lon = ifelse(lon == 0, NA, lon),
      lat = ifelse(lat == 0, NA, lat)
    ) %>%
    # Load coordinates
    st_as_sf(coords = c("lon", "lat"), crs = 4326, na.fail = FALSE, remove = FALSE) %>%
    # Transform to UTM (ETRS89 / UTM zone 33N - EPSG:25833)
    st_transform(crs = 25833) %>%
    mutate(
      HuntX = st_coordinates(.)[, 1],
      HuntY = st_coordinates(.)[, 2]
    ) %>%
    # Drop sf related information. We only need X, Y coordinates.
    st_drop_geometry() %>%
    distinct(HuntDate, HuntTime, HuntX, HuntY) %>%
    mutate(Hunt.ID = row_number()) %>%
    dplyr::select(Hunt.ID, HuntDate, HuntTime, HuntX, HuntY)
}
