#### 2020 ####
winter20 <- Movement %>%
  filter(t_ %within% as.interval(as_datetime("2020-03-30"), as_datetime("2020-03-31"))) %>%
  group_by(Sender.ID) %>%
  slice_sample(n = 1) %>%
  ungroup()

train20 <- winter20 %>% select(x_, y_)
kmout20 <- kmeans(train20, centers = 3)

winter20$group <- kmout20$cluster
  
winter20$group <- factor(winter20$group, levels = c(1,2,3), labels = c("A", "B", "C"))

ggplot(winter20, aes(x_, y_, color = group)) + 
  geom_point()




#### 2021 ####
winter21 <- Movement %>%
  filter(t_ %within% as.interval(as_datetime("2021-03-30"), as_datetime("2021-03-31"))) %>%
  group_by(Sender.ID) %>%
  slice_sample(n = 1) %>%
  ungroup()

train21 <- winter21 %>% select(x_, y_)

kmout21 <- kmeans(train21, centers = 4)

winter21$group <- kmout21$cluster

winter21$group <- factor(winter21$group, levels = c(1,2,3,4), labels = c("A", "B", "C", "D"))

ggplot(winter21, aes(x_, y_, color = group)) + 
  geom_point()

#### 2022 ####
winter22 <- Movement %>%
  filter(t_ %within% as.interval(as_datetime("2022-03-30"), as_datetime("2022-03-31"))) %>%
  group_by(Sender.ID) %>%
  slice_sample(n = 1) %>%
  ungroup()

train22 <- winter22 %>% select(x_, y_)
kmout22 <- kmeans(train22, centers = 3)

winter22$group <- kmout22$cluster

winter22$group <- factor(winter22$group, levels = c(1,2,3), labels = c("A", "B", "C"))

ggplot(winter22, aes(x_, y_, color = group)) + 
  geom_point()


#### 2023 ####
winter23 <- Movement %>%
  filter(t_ %within% as.interval(as_datetime("2023-01-30"), as_datetime("2023-01-31"))) %>%
  group_by(Sender.ID) %>%
  slice_sample(n = 1) %>%
  ungroup()

train23 <- winter23 %>% select(x_, y_)
kmout23 <- kmeans(train23, centers = 3)

winter23$group <- kmout23$cluster

winter23$group <- factor(winter23$group, levels = c(1,2,3), labels = c("A", "B", "C"))

ggplot(winter23, aes(x_, y_, color = group)) + 
  geom_point()


min(Movement$t_)
