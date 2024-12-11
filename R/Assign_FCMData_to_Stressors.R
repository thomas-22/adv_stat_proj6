########### assign FCM data ##############

### rewrite using joins
Assign_FCMData <- function(FCMStress.data, 
                           data.full,
                           gut.retention.threshold = c(9, 29),
                           distance.threshold = 5000,
                           ignore.filters = FALSE) {
  assertIntegerish(distance.threshold, lower = 0, any.missing = FALSE, len = 1)
  assertLogical(ignore.filters, len = 1, any.missing = FALSE)
  assertIntegerish(gut.retention.threshold, lower = 0, any.missing = FALSE, len = 2, sorted = TRUE)
  # Create an empty data frame to store results
  FCMData_Assigned <- data.frame(Sender.ID = integer(),
                                 Sample_ID = character(),
                                 HairID = character(),
                                 Defec_Time = as.POSIXct(character()),
                                 Stress_Time = as.POSIXct(character()),
                                 Speed = numeric(),
                                 Distance = numeric(),
                                 ng_g = numeric(),
                                 StressorID = integer(),
                                 stringsAsFactors = FALSE)
  # Iterate through each row in FCMStress
  for (i in seq_len(nrow(FCMStress.data))) {
    #cat(sprintf("FCM Assignment: %d of %d", i, nrow(FCMStress)), "\r")
    sample_row <- FCMStress.data[i, ]
    sample_id <- sample_row$Sample_ID
    sender_id <- sample_row$Sender.ID
    collar_time <- sample_row$Collar_t_
    x_fcm <- sample_row$X
    y_fcm <- sample_row$Y
    hair_id <- sample_row$HairID
    ng_g <- sample_row$ng_g
    #
    matching_events <- Stress %>% ##### look up stress
      mutate(Sender.ID = as.character(Sender.ID)) %>%
      filter(
        Sender.ID == as.character(sender_id) &
          difftime(collar_time, HuntEventTime, units = "hours") >= gut.retention.threshold[[1]] &
          difftime(collar_time, HuntEventTime, units = "hours") <= gut.retention.threshold[[2]]
      )
    # If there are matching events, create entries in the FCMData_Assigned data frame
    if (nrow(matching_events) > 0) {
      for (j in 1:nrow(matching_events)) {
        event_row <- matching_events[j, ]
        new_entry <- data.frame(Sender.ID = sender_id,
                                Sample_ID = sample_id,
                                HairID = hair_id,
                                Defec_Time = collar_time,
                                Stress_Time = event_row$HuntEventTime,
                                TimeDiff = difftime(collar_time, event_row$HuntEventTime, units = "hours"),
                                Distance = event_row$Distance,
                                ng_g = ng_g,
                                StressorID = event_row$StressorID,
                                stringsAsFactors = FALSE)
        FCMData_Assigned <- rbind(FCMData_Assigned, new_entry)
      }
    }
  }
  df <- data.full %>% inner_join(FCMData_Assigned, by = c("Sender.ID", "StressorID", "Distance"))
  tidy.data(df, ignore.filters, distance.threshold)$data
}

tidy.data <- function(data,
                      ignore.filters, 
                      distance.threshold) {
  if (!ignore.filters) {
    data <- data %>% filter(Distance <= distance.threshold)
  }
  unique.samples <- data %>% distinct(Sample_ID)
  df <- data.frame()
  for (i in seq_len(nrow(unique.samples))) {
    current_id <- unique.samples$Sample_ID[i]
    #
    new_entry <- data %>%
      filter(Sample_ID == current_id) %>%
      filter(Distance == min(Distance)) %>%
      arrange(TimeDiff) %>%
      slice_head(n = 1)
    df <- rbind(df, new_entry)
  }
  return(list(data = df, unique.samples = unique.samples))
}

# #fcms_assigned_to_huntevents_notime <- fcms_assigned_to_huntevents_notime[, names(FCMData_Assigned)]
# #test <- rbind(FCMData_Assigned, fcms_assigned_to_huntevents_notime)
# 
# #
# fcm_specific <- tidy.data(data.full, FALSE, 5000)$data
# usiv <- tidy.data(data.full, FALSE, 5000)$unique.samples
# #
# fcm_reference <- FCMStress %>%
#   filter(!(Sample_ID %in% usiv))
# #
# t_test_fcm <- t.test(fcm_specific$ng_g, fcm_reference$ng_g)
# 
# print(t_test_fcm)
# 
# 
# specific_mean <- mean(fcm_specific$ng_g, na.rm = TRUE)
# specific_n <- sum(!is.na(fcm_specific$ng_g))
# 
# reference_mean <- mean(fcm_reference$ng_g, na.rm = TRUE)
# reference_n <- sum(!is.na(fcm_reference$ng_g))
# 
# lm1 <- lm(fcm_specific$ng_g ~ fcm_specific$Distance)
# lm1$coefficients[2]
# 
# ggplot(fcm_specific, aes(x = Distance, y = ng_g)) +
#   geom_point(color = "blue") +
#   geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
#   geom_hline(yintercept = reference_mean, color = "red", linetype = "dashed") +
#   geom_hline(yintercept = specific_mean, color = "blue", linetype = "dashed") +
#   labs(
#     title = "Plot of ng_g vs Distance (with reference mean)",
#     x = "Distance",
#     y = "ng_g"
#   ) +
#   # Add annotations for fcm_specific
#   annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
#            label = paste("fcm_specific (blue):",
#                          "\nn =", specific_n,
#                          "\nmean =", round(specific_mean, 2)),
#            color = "blue", size = 3.5) +
#   annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 2.33,
#            label = paste("fcm_reference (red):",
#                          "\nn =", reference_n,
#                          "\nmean =", round(reference_mean, 2)),
#            color = "red", size = 3.5) +
#   annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 2.5,
#            label = paste("Gut Retention Lower Boundary (Hours):", gut_retention_time_lower,
#                          "\nGut Retention Upper Boundary (Hours):", gut_retention_time_upper,
#                          "\nDistance Threshhold (Meters):", distance_threshold,
#                          "\nT_Test:", round(t_test_fcm$statistic, 2),
#                          "\nT_Test p_val:", round(t_test_fcm$p.value, 2)),
#            color = "green", size = 3.5) +
#   theme_minimal() +
#   theme(plot.margin = margin(5, 50, 5, 5))
# 
# #Some magic
# fcm_specific$Impact_Factor <- fcm_specific$StressorID
# for (i in 1:nrow(fcm_specific)) {
#   fcm_specific[i,]$Impact_Factor <- ((1/fcm_specific[i,]$Distance^2)*
#                                        (1/(abs(diff(c(as.numeric(fcm_specific[i,]$TimeDiff), gut_retention_mean))))^0.5))*
#     10000000
# }
# fcm_specific_impacted <- subset(fcm_specific, Impact_Factor > 0.1)
# 
# lm1 <- lm(ng_g ~ Impact_Factor, data=fcm_specific_impacted)
# summary(lm1)
# 
# ggplot(fcm_specific_impacted, aes(x = Impact_Factor, y = ng_g)) +
#   geom_point(color = "blue") +
#   scale_x_log10() +
#   geom_smooth(method = "lm", color = "blue", linewidth = 0.5, se = FALSE) +
#   geom_hline(yintercept = reference_mean, color = "red", linetype = "dashed") +
#   geom_hline(yintercept = specific_mean, color = "blue", linetype = "dashed") +
#   labs(
#     title = "Plot of ng_g vs\nImpact Factor(1/distance^2 * 1/Absolute Difference to 19 Hrs)",
#     x = "Impact Factor",
#     y = "ng_g"
#   ) +
#   theme_minimal() +
#   theme(plot.margin = margin(5, 50, 5, 5))
# 
# #3D Plot:
# # plot_ly(fcm_specific, x = ~Distance, y = ~TimeDiff, z = ~ng_g,
# #         type = "scatter3d", mode = "markers",
# #         marker = list(size = 5, color = 'blue',
# #                       line = list(color = 'black', width = 2))) %>%
# #   layout(title = "3D Plot with Black Outline",
# #          scene = list(
# #            xaxis = list(title = 'Distance'),
# #            yaxis = list(title = 'TimeDiff'),
# #            zaxis = list(title = 'ng_g')
# #          ))
# 
# 
# 
# 
# 
