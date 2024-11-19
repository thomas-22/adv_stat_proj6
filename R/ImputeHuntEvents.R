library(ggplot2)
library(kknn)
library(lubridate)

source("R/Datafusion.R")

HuntEvents_pre <- arrange(HuntEvents, t_) %>%
  mutate(
    missing_t = is.na(t_),
    type = ifelse(is.na(t_), "time missing", "time recorded"),
    # mean-center and convert to km
    X = (X - mean(X)) / 1000,
    Y = (Y - mean(Y)) / 1000
  )
View(HuntEvents_pre)

#############################################################
# some explorative analysis

COLOR_OBSERVED <- "cornflowerblue"
COLOR_MISSING <- "red"

start_date <- min(HuntEvents$Date, na.rm = TRUE)
end_date <- max(HuntEvents$Date, na.rm = TRUE)
break_dates <- seq(start_date, end_date, length.out = 4)

# visualize missing data
ggplot(HuntEvents_pre) +
  geom_bar(aes(x = dmy(Date), fill = type), position = "stack") +
  # geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_date(
    limits = c(ymd("2020-04-01"), ymd("2023-04-30")),
    breaks = seq(ymd("2020-04-01"), ymd("2023-04-30"), by = "quarter"),
    minor_breaks = "month"
  ) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), minor_breaks = 0:40) +
  scale_fill_manual(
    breaks = c("time recorded", "time missing"),
    values = c(COLOR_OBSERVED, COLOR_MISSING)
  ) +
  labs(title = "Hunting events per day",
       x = "Date", y = "Count",
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

# At what time in a day do hunting events happen?
HuntEvents %>%
  na.omit() %>%
  ggplot() +
  geom_bar(aes(x = hour(t_)), just = 0, width = 0.95) +
  scale_x_continuous(breaks = 0:24, minor_breaks = NULL) +
  labs(x = "Hour")
# Observation: no hunting event happend between 14 and 15
# => Define a "hunting day" -- 15:00 ~ 14:00 (next day)

##############################################################################
# kNN imputation

HuntEvents_knn <- HuntEvents_pre %>%
  distinct() %>%
  mutate(
    t_target = t_ - hours(15),
    days_since_start = as.numeric(
      difftime(
        floor_date(t_target, unit = "day"),
        floor_date(t_target[[1]], unit = "day"),
        units = "days"
      )
    )
  ) %>%
  mutate(t_target = as.numeric(
    difftime(
      t_target,
      floor_date(t_target, unit = "day"),
      units = "hours"
    )
  ))
View(HuntEvents_knn)

# Trial 1: hold-out
all_data <- na.omit(HuntEvents_knn)
n_data <- nrow(all_data)
train_data <- all_data[sample.int(n_data, 0.8 * n_data), ]
test_data <- setdiff(all_data, train_data)

form <- formula(t_target ~ X + Y)
knn1 <- kknn(form, train_data, test_data, kernel = "rectangular")

res1 <- tibble(
  t_test = test_data$t_target,
  t_pred = knn1$fitted.values
)
View(res1)  # FAILURE

# Trail 1: CV
knn2 <- train.kknn(form, train_data, kmax = 20, kernel = "optimal")
knn2  # best k = 2

best_kernel = knn2$best.parameters$kernel
best_k = knn2$best.parameters$k

res2 <- tibble(
  Date = test_data$Date,
  t_ = test_data$t_,
  t_test = test_data$t_target,
  t_pred = kknn(form, train_data, test_data, k = best_k, kernel = best_kernel)$fitted.values
) %>%
  mutate(
    t_pred_datetime = dmy_hms(paste(Date, "00:00:00")) + as.difftime(t_pred, units = "hours") + hours(15)
  )
View(res2)
# prediction is accurate <=> difference to true time smaller than 1 hour
res2 %>%
  summarise(
    accuracy = sum(abs(t_test - t_pred) < 1) / nrow(res2)
  )
# FAILURE!


##############################################################################

# more explorative analysis about missing data

# Imputation with a single number
# "00:00:01" never occurred => use to impute
HuntEvents %>%
  filter(hour(t_) == 0, minute(t_) == 0, second(t_) == 1)

HuntEvents_imp1 <- read.csv(
    "data/Hunt Events - CRS= ETRS UTM 33N.csv", 
    header = TRUE, sep = ","
  ) %>%
  as_tibble() %>%
  select(-1) %>%
  rename(Date = Datum, Time = Zeit) %>%
  mutate(
    missing_t = is.na(Time),
    Time = ifelse(is.na(Time), "00:00:01", Time),
    t_ = dmy_hms(paste(Date, Time))
  )
View(HuntEvents_imp1)
                            
FCMStress19 <- FCMStress %>%
  select(ng_g, Sample_ID, Sender.ID, X, Y, Collar_t_, Waypoint_t_) %>%
  rename(SampleX = X, SampleY = Y, DefecTime = Collar_t_, SampleTime = Waypoint_t_) %>%
  mutate(StressTime = DefecTime - hours(19)) %>%
  arrange(StressTime)
View(FCMStress19)

# 3 observations with no sample time (missing Waypoint_t_)
FCMStress19 %>% filter(is.na(SampleTime))

data <- HuntEvents_imp1 %>%
  distinct() %>%
  filter(t_ <= max(FCMStress19$DefecTime)) %>%
  left_join(FCMStress19, join_by(closest(t_ <= StressTime))) %>%
  rename(HuntTime = t_, HuntX = X, HuntY = Y) %>%
  # filter(missing_t == TRUE) %>%
  # select(Sample_ID, Sender.ID, HuntTime, StressTime, DefecTime) %>%
  mutate(
    TimeHuntStress = difftime(StressTime, HuntTime, units = "hours"),
    TimeHuntDefec = difftime(DefecTime, HuntTime, units = "hours"),
  )
View(data %>% filter(missing_t == TRUE))
