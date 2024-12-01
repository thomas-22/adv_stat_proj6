Datafusion <- function(Movement.data, Pregnancy.data, Stress.data, Groups.data) {
  #
  full.data <- Movement.data %>%
    left_join(Pregnancy.data, by = c("Sender.ID")) %>%
    mutate(pregnant = if_else(t_ %within% pregnant, TRUE, FALSE)) %>%
    left_join(Groups.data,
              by = c("Sender.ID", "t_", "x_", "y_"),
              relationship = "many-to-many") %>%
    mutate(t_low =t_ - dminutes(30), 
           t_high = t_ + dminutes(30))
  #
  full.data <- Stress.data %>% left_join(full.data,
                           by = join_by(
                             "Sender.ID",
                             between(x = HuntEventTime, y_lower = t_low, y_upper = t_high))
                           ) %>%
    select(-c(x_, y_, t_, t_low, t_high)) %>%
    distinct()
  #
  return(full.data)
}
