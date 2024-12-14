prep.Movement.data <- function() {
  # load Movement Data located in path.Movement
  # mutates 
  # >> Sender.ID as factor
  # >> t_ as datetime
  # >> DistanceTraveled as distance traveled respective to the point before using euclidean distance
  # This is the response variable for our optional task.
  read.csv(path.Movement, header = TRUE, sep = ";")[,2:5] %>%
    mutate(Sender.ID = factor(Sender.ID),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    group_by(Sender.ID) %>%
    mutate(DistanceTraveled = sqrt((x_ - lag(x_))^2 + (y_ - lag(y_))^2)) %>%
    mutate(DistanceTraveled = ifelse(is.na(DistanceTraveled), 0, DistanceTraveled)) %>%
  ungroup()
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
  # load HuntEvents Data located at path.Huntevents
  # mutates:
  # >> t_ as date time consisting of Datum & Zeit
  # >> Date as ymd of Datum
  # only returns unique entries
  # adds unique Hunt.ID to each unique HuntingEvent
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
  # reads FCMStress data located at path.FCMStress
  # only takes columns 2-14 (first column is irrelevant row ID)
  # mutates:
  # >> Collar_t_ as Datetime consisting of Colalr_day and Collar_time
  # >> Waypoint_t in identical manner
  # >> Sender.ID as factor
  # returns only Sample_ID, Sender.ID, ng_g, Sender.ID, X, Y, Collar_t_, Waypoint_t_
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
  # read Pregnancy data located path.Pregnancy
  # filters only for entries, where a birth date of an offspring is available
  # mutates:
  # >> birth_date as datetime
  # >> Sender.ID as factor
  # >> pregnant since: the timestamp pregnancy_duration days before birth_Date
  # >> pregnant: intervall from pregannt_since to birthdate
  # returns Sender.ID and pregnant
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
  # takes 
  # > Movement data
  # > enlosures: a numeric vector containing a x and y coordinate for every enclosure center
  # omits rows with NAs in movement data
  # gives x and y coordinates to kmeans() together with enlcosures as centers
  # returns movement.data augmented with a vector containg the group number
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
