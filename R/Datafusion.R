# get dataframes
ReproductionSuccess <- prep.Pregnancy.data()
Movement <- prep.Movement.data()
HuntEvents <- prep.Huntevents.data()
FCMStress <- prep.FCMStress.data()
Groups <- get.Herds(Movement, enclosures)

# What to do with NA's? here: just drop the lines. Drop any duplicate entries.
HuntEventsreduced <- distinct(HuntEvents[!is.na(HuntEvents$t_), 3:5])
HuntEvents_NoTime <- distinct(HuntEvents[is.na(HuntEvents$t_), 1:4])
HuntEvents_NoTime <- HuntEvents_NoTime %>% select(-Zeit)
HuntEvents_NoTime$Datum <- as.Date(HuntEvents_NoTime$Datum, format = "%d/%m/%Y")

# ReproductionSuccess gets joined to Movement
# logical idicator "pregnant" is set to TRUE if collar time lies within pregnancy interval
# then joined by groups
Movement %>%
  left_join(ReproductionSuccess, by = c("Sender.ID")) %>%
  mutate(pregnant = if_else(t_ %within% pregnant, TRUE, FALSE)) %>%
  left_join(Groups, by = c("Sender.ID", "t_"), relationship = "many-to-many")

StressData <- CalcSenderPosDist(Movement, HuntEvents)

# save as RDS
# saveRDS(Movement, "data/Movement.RDS")
# saveRDS(FCMStress, "data/FCMStress.RDS")
# saveRDS(HuntEvents, "data/Hunts.RDS")
# saveRDS(HuntEventsreduced, "data/HuntsReduced.RDS")
