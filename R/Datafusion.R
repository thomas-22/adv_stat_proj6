run_datafusion <- function (save=FALSE) {
  #-----------------------------------------------------------------------------
  # Pregnancy still needs correction!
  
  # # calculate backwards from the birth of the foal 200 days.
  # # column pregnant is time interval
  
  # ReproductionSuccess <- readxl::read_excel("Data/Reproduction Success Results.xlsx") %>%
  #   filter(!is.na(`birth date`)) %>%
  #   mutate(birth_date = ymd(`birth date`),
  #          Sender.ID = factor(`Collar ID`),
  #          .keep = "unused") %>%
  #   mutate(pregnant_since = birth_date - days(pregancy_duration)) %>%
  #   mutate(pregnant = interval(pregnant_since, birth_date))
  
  # ReproductionSuccess <- ReproductionSuccess[, c("Sender.ID", "pregnant")]
  #-----------------------------------------------------------------------------
  
  # Read movement data
  Movement <- read.csv("Data/Movement - CRS=ETRS UTM 33N.csv", header = TRUE, sep = ";") %>%
    as_tibble() %>%
    mutate(Sender.ID = factor(Sender.ID),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    group_by(Sender.ID) %>%
    mutate(DistanceTraveled = sqrt((x_ - lag(x_))^2 + (y_ - lag(y_))^2)) %>%
    mutate(DistanceTraveled = ifelse(is.na(DistanceTraveled), 0, DistanceTraveled)) %>%
    ungroup()
  
  # Read hunting events data
  HuntEvents <- read.csv("Data/HuntingEvents_NEW.csv", header = TRUE, sep = ";") %>%
    as_tibble() %>%
    mutate(HuntTime = stringr::str_c(Datum, Zeit, sep = " ") %>%
            parse_date_time(orders = "%d/%m/%Y %h:%M"),
           HuntDate = dmy(Datum)) %>%
    arrange(HuntDate) %>%
    mutate(
      lon = as.numeric(Laengengrad_wgs), # one entry is "13.NA", i.e., NA
      lat = as.numeric(Breitengrad_wgs),
    ) %>%
    filter(lon != 0, lat != 0, !is.na(lon), !is.na(lat)) %>%
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
    select(Hunt.ID, HuntDate, HuntTime, HuntX, HuntY)
  
  # Read FCM data
  FCMStress <- read_delim("Data/FCM Stress - Collared Deer - CRS=ETRS UTM 33N.csv") %>%
    as_tibble()
    mutate(DefecTime = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           DefecDate = as_date(DefecTime),
           SampleTime = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           Sender.ID = as.factor(Sender.ID)) %>%
    rename(Sample.ID = Sample_ID,
           DefecX = X,
           DefecY = Y) %>%
    distinct(
      Sample.ID, Sender.ID,
      # FCM level (our response variable)
      ng_g,
      # info about defecation event
      DefecTime, DefecDate, DefecX, DefecY,
      # info about sample collection
      SampleTime
    )
  
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
