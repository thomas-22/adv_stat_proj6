prep.Movement.data <- function() {
  # load Movement Data located in path.Movement
  # mutates 
  # >> Sender.ID as factor
  # >> t_ as datetime
  # >> dist_traveled as distance traveled respective to the point before using euclidean distance
  read.csv(path.Movement, header = TRUE, sep = ";")[,2:5] %>%
    mutate(Sender.ID = factor(Sender.ID),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M")) %>%
    group_by(Sender.ID) %>%
    mutate(dist_traveled = sqrt((x_ - lag(x_))^2 + (y_ - lag(y_))^2)) %>%
    mutate(dist_traveled = ifelse(is.na(dist_traveled), 0, dist_traveled)) %>%
  ungroup()
}

# prep.HuntEvents.data <- function() {
#   # load HuntEvents Data located at path.Huntevents
#   # mutates:
#   # >> t_ as date time consisting of Datum & Zeit
#   # >> Date as ymd of Datum
#   # only returns unique entries
#   # adds unique Hunt.ID to each unique HuntingEvent
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
    mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
            parse_date_time(orders = "%d/%m/%Y %h:%M"),
          Date = dmy(Datum)) %>%
    distinct() %>%
    mutate(Hunt.ID = as.factor(row_number())) %>%
    select(-Datum, -Zeit, -X.) %>%
    mutate(
      lon = as.numeric(Laengengrad_wgs), # one entry is "13.NA", i.e., NA
      lat = as.numeric(Breitengrad_wgs),
    ) %>%
    select(-Laengengrad_wgs, -Breitengrad_wgs) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, na.fail = FALSE, remove = FALSE) %>%
    # project into UTM 33N
    sf::st_transform(crs = 25833) %>%
    # get x, y coordinates
    mutate(
      X = sf::st_coordinates(.)[, 1],
      Y = sf::st_coordinates(.)[, 2]
    ) %>%
    # Drop sf related information. We only need X, Y coordinates.
    sf::sf_drop_geometry()
}

prep.FCMStress.data <- function() {
  # reads FCMStress data located at path.FCMStress
  # only takes columns 2-14
  # mutates:
  # >> Collar_t_ as Datetime consisting of Colalr_day and Collar_time
  # >> Waypoint_t in identical manner
  # >> Sender.ID as factor
  # returns only Sample_ID, Sender.ID, ng_g, Sender.ID, X, Y, Collar_t_, Waypoint_t_
  read.csv(path.FCMStress, header = TRUE, sep = ",")[2:14] %>%
    mutate(Collar_t_ = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           Waypoint_t_ = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           Sender.ID = as.factor(Sender.ID)) %>%
    select(Sample_ID, Sender.ID, ng_g, Sender.ID, X, Y, Collar_t_, Waypoint_t_)
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
    mutate(pregnant_since = birth_date - days(pregancy_duration)) %>%
    mutate(pregnant = interval(pregnant_since, birth_date)) %>%
    select(Sender.ID, pregnant)
}

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
