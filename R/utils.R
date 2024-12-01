prep.Movement.data <- function() {
  read.csv(path.Movement, header = TRUE, sep = ";")[,2:5] %>%
    mutate(Sender.ID = factor(Sender.ID),
           t_ = parse_date_time(t_, orders = "%d/%m/%Y %h:%M"))
}

prep.Huntevents.data <- function() {
  read.csv(path.Huntevents, header = TRUE, sep = ",")[2:5] %>%
  mutate(t_ = stringr::str_c(Datum, Zeit, sep = " ") %>%
           parse_date_time(orders = "%d/%m/%Y %h:%M:%s"))
}

prep.FCMStress.data <- function() {
  read.csv(path.FCMStress, header = TRUE, sep = ",")[2:14] %>%
    mutate(Collar_t_ = stringr::str_c(Collar_day, Collar_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           Waypoint_t_ = stringr::str_c(Waypoint_day, Waypoint_time, sep = " ") %>%
             parse_date_time(orders = c("%d/%m/%Y %h:%M:%s", "%d/%m/%Y %h:%M")),
           Sender.ID = as.factor(Sender.ID),
           Sex = as.factor(Sex)) %>%
    select(-Collar_day, -Collar_time, -Waypoint_day, -Waypoint_time)
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
  cbind(Movement.data, group = km.out$cluster)
}
