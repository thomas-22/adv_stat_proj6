FCMStress$delta_t <- abs(as.numeric(FCMStress$Waypoint_t_ - FCMStress$Collar_t_))

FCMStress <- FCMStress %>%
  filter(delta_t < 500)

model1 <- lm(ng_g ~ delta_t, data = FCMStress)
model2 <- lm(ng_g ~delta_t + I(delta_t^2), data = FCMStress)
model3 <- lm(ng_g ~delta_t + I(delta_t^2) + I(delta_t^3) + I(delta_t^4) + I(delta_t^5), data = FCMStress)

plot(model1)
summary(model1)
plot(model2)
summary(model2)
plot(model3)
summary(model3)


ggplot(FCMStress, aes(delta_t, ng_g)) +
  geom_point(alpha = .4) +
  geom_line(aes(y = augment(model1)$.fitted), color = "magenta", linewidth = 1) +
  geom_line(aes(y = augment(model2)$.fitted), color = "blue", linewidth = 1) +
  geom_line(aes(y = augment(model3)$.fitted), color = "green", linewidth = 1)
