Movement_grouped <- na.omit(readRDS("data/Movement.RDS"))

### retrieve subset of movement data: spatial and temporal components
Position <- Movement_grouped %>%
  select(x_, y_, t_) %>%
  mutate(date = lubridate::decimal_date(t_), .keep = "unused") %>%
  select(-date)

### from previous data analysis - the actual winter enclosure coordinates would be handy
centers_winter <- rbind(c(x_ = 382443.5, y_ = 5421950), 
                         c(x_ = 394413.2, y_ = 5417547), 
                         c(x_ = 378099.6, y_ = 5431399), 
                         c(x_ = 375115.9, y_ = 5435760))

### use spatial position with kmeans to get the four clusters
km_xy <- kmeans(Position, centers = centers_winter)
Movement_grouped <- cbind(Movement_grouped, group = km_xy$cluster)

### visualize the positions of the groups
p1 <- Movement_grouped %>%
  ggplot(aes(x = x_, y = y_)) +
  geom_point(aes(colour = Sender.ID), alpha = .2) +
  facet_wrap(~group) +
  guides(color = "none", x = "none", y = "none") +
  theme_light()
#p1

### check on 9 random deer if the groups change mutch
p2 <- Movement_grouped %>%
  filter(Sender.ID %in% sample(unique(Movement_grouped$Sender.ID), 9)) %>%
  ggplot(aes(x_, y_)) +
  geom_point(aes(color = as.factor(group)), alpha = .5) +
  geom_path(alpha = .05) + 
  facet_wrap(~Sender.ID) +
  theme_light() +
  guides(x = "none", y = "none") +
  labs(color = "Group")
#p2

### save as RDS
saveRDS(Movement_grouped, "data/Movement_grouped.RDS")
