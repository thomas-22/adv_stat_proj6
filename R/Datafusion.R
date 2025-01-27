run_datafusion <- function (save=FALSE) {
  # Read FCM data
  path.FCMStress <- "Data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv"
  FCMStress <- read.csv(path.FCMStress, header = TRUE, sep = ",") %>%
    as_tibble() %>%
    mutate(DefecTime = str_c(Collar_day, Collar_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           DefecDate = as_date(DefecTime),
           SampleTime = str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
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
  
  sender_ids = levels(FCMStress$Sender.ID)
  
  # Add pregnancy information
  path.ReproductionSuccess <- "Data/Reproduction Success Results.xlsx"
  ReproductionSuccess <- readxl::read_excel(path.ReproductionSuccess) %>%
    mutate(Sender.ID = factor(`Collar ID`, levels = sender_ids)) %>%
    filter(calf == 1) %>%
    distinct(Sender.ID, preg_year)
  
  FCMStress <- FCMStress %>%
    left_join(ReproductionSuccess, by = "Sender.ID") %>%
    mutate(
      Pregnant = preg_year == year(DefecTime),
      Pregnant = ifelse(is.na(Pregnant), FALSE, Pregnant),
      Pregnant = factor(Pregnant)
    ) %>%
    dplyr::select(-preg_year)

  # Read movement data
  path.Movement <- "Data/Movement - CRS=ETRS UTM 33N.csv"
  Movement <- read.csv(path.Movement, header = TRUE, sep = ";") %>%
    as_tibble() %>%
    mutate(Sender.ID = factor(Sender.ID, levels = sender_ids),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    group_by(Sender.ID) %>%
    mutate(DistanceTraveled = sqrt((x_ - lag(x_))^2 + (y_ - lag(y_))^2)) %>%
    mutate(DistanceTraveled = ifelse(is.na(DistanceTraveled), 0, DistanceTraveled)) %>%
    ungroup()
  
  # Read hunting events data
  path.HuntEvents <- "Data/HuntingEvents_NEW.csv"
  HuntEvents <- read.csv(path.HuntEvents, header = TRUE, sep = ";") %>%
    as_tibble() %>%
    mutate(HuntTime = stringr::str_c(Datum, Zeit, sep = " ") %>%
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
  
  # remove spatial outliers
  FCMStress <- FCMStress %>%
    filter(DefecX > 370000, DefecY < 5450000)
  HuntEvents <- HuntEvents %>%
    filter(HuntY > 5400000, HuntY < 5460000, HuntX > 365000)

  # save as RDS
  if (save) {
    saveRDS(Movement, "data/Movement.RDS")
    saveRDS(FCMStress, "data/FCMStress.RDS")
    saveRDS(HuntEvents, "data/Hunts.RDS")
    # saveRDS(HuntEventsreduced, "data/HuntsReduced.RDS")
    # saveRDS(HuntEvents_Reduced_UTM_New, "data/HuntEvents_Reduced_UTM_New.RDS")
  }  

  return(list(
    Movement = Movement,
    FCMStress = FCMStress,
    HuntEvents = HuntEvents,
    HuntEvents_reduced = na.omit(HuntEvents)
  ))
}
