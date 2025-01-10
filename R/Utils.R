get_season <- function(datetimes) {
  res <- vapply(
    datetimes,
    function(datetime) {
      if (month(datetime) %in% c(3, 4, 5)) return("spring")
      if (month(datetime) %in% c(6, 7, 8)) return("summer")
      return("autumn")
      # only one single FCM sample in winter (12-01)
    },
    character(1)
  )
  factor(res, levels = c("spring", "summer", "autumn"))
}


get_herds <- function(Movement.data, enclosures) {
  # takes 
  # > Movement data
  # > enlosures: a numeric vector containing a x and y coordinate for every enclosure center
  # omits rows with NAs in movement data
  # gives x and y coordinates to kmeans() together with enlcosures as centers
  # returns movement.data augmented with a vector containg the group number
  Movement.data <- na.omit(Movement.data)
  Position <- Movement.data %>% select(x_, y_)
  km.out <- kmeans(Position, centers = enclosures)
  cbind(Movement.data, group = as.factor(km.out$cluster))
}
