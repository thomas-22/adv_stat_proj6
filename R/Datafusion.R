run_datafusion <- function(save = FALSE, remove_outlers = TRUE) {
  # Read FCM data
  FCMStress <- read.FCMStress(path = path.FCMStress)
  # ------------
  #
  # Read pregnancy information
  ReproductionSuccess <- read.Reproduction(path = path.ReproductionSuccess,
                                           deer.id.levels = levels(FCMStress$Deer.ID))
  # ------------
  #
  # Read movement data
  Movement <- read.Movement(path = path.Movement,
                            sender.id.levels = levels(FCMStress$Sender.ID))
  # ------------
  #
  # Read hunting events data
  HuntEvents <- read.HuntEvents(path = path.HuntEvents)
  # ------------
  #
  # add pregnancy to hunt events data
  FCMStress <- FCMStress %>%
    left_join(ReproductionSuccess, by = "Deer.ID") %>%
    mutate(
      hasCalf = preg_year == year(DefecTime) & calf == 1,
      hasCalf = ifelse(preg_year == year(DefecTime) & is.na(calf), NA, hasCalf),
      hasCalf = ifelse(preg_year != year(DefecTime), FALSE, hasCalf),
      hasCalf = factor(hasCalf, levels = c(FALSE, TRUE, NA))
    )
  # ------------
  #
  # remove spatial outliers  
  if (remove_outlers) {
    FCMStress <- FCMStress %>%
      filter(DefecX > 370000, DefecY < 5450000)
    HuntEvents <- HuntEvents %>%
      filter(HuntY > 5400000, HuntY < 5460000, HuntX > 365000)
  }
  # ------------
  #
  # save as RDS
  if (save) {
    saveRDS(Movement, "data/intermediate/Movement.RDS")
    saveRDS(FCMStress, "data/intermediate/FCMStress.RDS")
    saveRDS(HuntEvents, "data/intermediate/Hunts.RDS")
  }  
  # ------------
  #
  # return datasets
  return(list(
    Movement = Movement,
    FCMStress = FCMStress,
    HuntEvents = HuntEvents,
    HuntEvents_reduced = na.omit(HuntEvents)
  ))
}
