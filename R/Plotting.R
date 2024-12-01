# ### visualize the positions of the groups
# get.Herds(Movement, enclosures) %>%
#   ggplot(aes(x = x_, y = y_)) +
#   geom_point(aes(colour = Sender.ID), alpha = .2) +
#   facet_wrap(~group) +
#   guides(color = "none", x = "none", y = "none") +
#   theme_light()
# 
# ### check on 9 random deer if the groups change mutch
# Movement_grouped %>%
#   filter(Sender.ID %in% sample(unique(Movement_grouped$Sender.ID), 9)) %>%
#   ggplot(aes(x_, y_)) +
#   geom_point(aes(color = as.factor(group)), alpha = .5) +
#   geom_path(alpha = .05) + 
#   facet_wrap(~Sender.ID) +
#   theme_light() +
#   guides(x = "none", y = "none") +
#   labs(color = "Group")
