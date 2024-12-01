# library(dplyr)
# 
# fcms_assigned_to_huntevents_notime  <- data.frame()
# 
# for (i in 1:nrow(FCMStress)) {
#   current_row <- FCMStress[i,]
#   
#   collar_date <- as.Date(current_row$Collar_t_)
#   hunts_date_fits <- HuntEvents_NoTime %>%
#     filter(as.Date(Datum) == collar_date)
#   if (nrow(hunts_date_fits) > 0) {
#     total_dist <- 0
#     for (n in 1:nrow(hunts_date_fits)) {
#       current_hunt <- hunts_date_fits[n,]
#       current_single_distance <- as.numeric(st_distance(
#         st_sfc(st_point(c(current_hunt$X, current_hunt$Y)), crs = 32633),
#         st_sfc(st_point(c(current_row$X, current_row$Y)), crs = 32633)))
#       total_dist <- total_dist + current_single_distance
#     }
#     avg_distance <- total_dist/nrow(hunts_date_fits)
#     new_entry <- data.frame(
#       Sender.ID = current_row$Sender.ID,
#       Sample_ID = current_row$Sample_ID,
#       HairID = current_row$HairID,
#       Defec_Time = current_row$Collar_t_,
#       Stress_Time = collar_date,
#       Distance = avg_distance,
#       ng_g = current_row$ng_g)
#     
#     fcms_assigned_to_huntevents_notime <- rbind(fcms_assigned_to_huntevents_notime, new_entry)
#   }
# }
