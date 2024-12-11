# add doc strings

prep.Movement.data <- function() {
  read.csv(path.Movement, header = TRUE, sep = ";")[,2:5] %>%
    mutate(Sender.ID = factor(Sender.ID),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M"))
  # add distance traveled here
}

prep.HuntEvents.data_old <- function() {
  read.csv(path.Huntevents_old, header = TRUE, sep = ",")[2:5] %>%
  mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
           parse_date_time(orders = "%d/%m/%Y %h:%M:%s"),
         Date = parse_date_time(Datum, orders = "%d/%m/%Y")) %>%
    select(-Zeit, -Datum) %>%
    distinct() %>%
    mutate(Hunt.ID = as.factor(row_number()))
}

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
    sf::st_transform(crs = 32633) %>%
    # get x, y coordinates
    mutate(
      X = sf::st_coordinates(.)[, 1],
      Y = sf::st_coordinates(.)[, 2]
    ) %>%
    # Drop sf related information. We only need X, Y coordinates.
    sf::sf_drop_geometry()
}

prep.FCMStress.data <- function() {
  read.csv(path.FCMStress, header = TRUE, sep = ",")[2:14] %>%
    mutate(Collar_t_ = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           Waypoint_t_ = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           Sender.ID = as.factor(Sender.ID)) %>%
    select(-Collar_day, -Collar_time, -Waypoint_day, -Waypoint_time, -Sex, -Comment, -Waypoint.number)
}

prep.Pregnancy.data <- function() {
  # calculate backwards from the birth of the foal pregnancy_duration days.
  # column pregnant is time interval
  Preganncy_raw <- readxl::read_excel(path.Pregancy) %>%
    filter(!is.na(`birth date`)) %>%
    mutate(birth_date = ymd(`birth date`),
           Sender.ID = factor(`Collar ID`),
           .keep = "unused") %>%
    mutate(pregnant_since = birth_date - days(pregancy_duration)) %>%
    mutate(pregnant = interval(pregnant_since, birth_date)) %>%
    select(Sender.ID, pregnant)
}

get.Herds <- function(Movement.data, enclosures) {
  checkmate::assertDataFrame(Movement.data)
  Movement.data <- na.omit(Movement.data)
  Position <- Movement.data %>% select(x_, y_)
  km.out <- kmeans(Position, centers = enclosures)
  cbind(Movement.data, group = as.factor(km.out$cluster))
}

