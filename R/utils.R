# add doc strings

prep.Movement.data <- function() {
  read.csv(path.Movement, header = TRUE, sep = ";")[,2:5] %>%
    mutate(Sender.ID = factor(Sender.ID),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    distinct(Sender.ID, x_, y_, t_)
  # add distance traveled here
}

# prep.HuntEvents.data_old <- function() {
#   read.csv(path.Huntevents_old, header = TRUE, sep = ",")[2:5] %>%
#   mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
#            parse_date_time(orders = "%d/%m/%Y %h:%M:%s"),
#          Date = parse_date_time(Datum, orders = "%d/%m/%Y")) %>%
#     select(-Zeit, -Datum) %>%
#     distinct() %>%
#     mutate(Hunt.ID = as.factor(row_number()))
# }

prep.HuntEvents.data <- function() {
  read.csv(path.Huntevents, header = TRUE, sep = ";") %>%
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
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, na.fail = FALSE, remove = FALSE) %>%
    # project into UTM 33N
    sf::st_transform(crs = 25833) %>%
    # get x, y coordinates
    mutate(
      HuntX = sf::st_coordinates(.)[, 1],
      HuntY = sf::st_coordinates(.)[, 2]
    ) %>%
    # Drop sf related information. We only need X, Y coordinates.
    sf::st_drop_geometry() %>%
    distinct(HuntDate, HuntTime, HuntX, HuntY) %>%
    mutate(Hunt.ID = as.factor(row_number())) %>%
    select(Hunt.ID, HuntDate, HuntTime, HuntX, HuntY)
}

prep.FCMStress.data <- function() {
  # Rename columns for readability
  # Defec = defecation
  read.csv(path.FCMStress, header = TRUE, sep = ",")[2:14] %>%
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
}

prep.Pregnancy.data <- function() {
  # calculate backwards from the birth of the foal pregnancy_duration days.
  # column pregnant is time interval
  readxl::read_excel(path.Pregancy) %>%
    filter(!is.na(`birth date`)) %>%
    mutate(birth_date = ymd(`birth date`),
           Sender.ID = factor(`Collar ID`),
           .keep = "unused") %>%
    mutate(pregnant_since = birth_date - days(pregancy_duration),
           pregnant_until = birth_date) %>%
    distinct(Sender.ID, pregnant_since, pregnant_until)
}

# TBD: Assign herd number to FCM samples,
# i.e., to which herd does the deer at the defecation time belong?
get.Herds <- function(Movement.data, enclosures) {
  checkmate::assertDataFrame(Movement.data)
  Movement.data <- na.omit(Movement.data)
  Position <- Movement.data %>% select(x_, y_)
  km.out <- kmeans(Position, centers = enclosures)
  cbind(Movement.data, group = as.factor(km.out$cluster))
}


# helper functions

get_season <- function(datetimes) {
  res <- vapply(
    datetimes,
    function(datetime) {
      if (month(datetime) %in% c(3, 4, 5)) return("spring")
      if (month(datetime) %in% c(6, 7, 8)) return("summer")
      if (month(datetime) %in% c(9, 10, 11)) return("autumn")
      if (month(datetime) %in% c(12, 1, 2)) return("winter")
    },
    character(1)
  )
  factor(res, levels = c("spring", "summer", "autumn", "winter"))
}
