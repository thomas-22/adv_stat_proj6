plot_herds <- function() {
  ### visualize the positions of the groups
  p <- get.Herds(Movement, enclosures) %>%
    ggplot(aes(x = x_, y = y_)) +
    geom_point(aes(colour = Sender.ID), alpha = .2) +
    facet_wrap(~group) +
    guides(color = "none", x = "none", y = "none") +
    labs(color = "Deer", x = "lattitude", y = "longitude", 
         title = "Locations of deers over time respective to assigned Group",
         caption = "Color represents individual deer") +
    theme_light()
  ggsave("Plots/location_of_herds.pdf", plot = p, device = "pdf")
}

plot_herd_assignment <- function() {
  ### check on 9 random deer if the groups change much
  #29498 is interesting
  p <- get.Herds(Movement, enclosures) %>%
    ggplot(aes(x = x_, y = y_)) +
    geom_point(aes(colour = Sender.ID), alpha = .2) +
    facet_wrap(~group) +
    guides(color = "none", x = "none", y = "none") +
    labs(color = "Deer", x = "lattitude", y = "longitude", 
         title = "Locations of deers over time respective to assigned Group",
         caption = "Color represents individual deer") +
    theme_light()
  ggsave("Plots/Movement_of_9_random_deer.pdf", plot = p, device = "pdf")
}

plot_cortisol_levels <- function() {
  p <- FCMStress %>%
    ggplot(aes(x = forcats::fct_reorder(Sender.ID, ng_g, .desc = TRUE), y = ng_g)) +
    geom_boxplot(outliers = FALSE) +
    geom_hline(yintercept = median(FCMStress$ng_g), color = "red", linetype = "dashed", linewidth = .75) +
    guides(x = guide_axis(angle = 60)) +
    labs(x = "individual deer", 
         y = "cortisol level", 
         title = "cortisol levels of every collared deer", 
         caption = "red dashed line: overall median level") +
    theme_light()
  ggsave("Plots/cortisol_level_per_deer.pdf", plot = p, device = "pdf")
}

plot_sample_count <- function() {
  p <- FCMStress %>%
    ggplot(aes(x = forcats::fct_reorder(Sender.ID, ng_g, .desc = TRUE))) +
    geom_bar(fill = "lightblue")+
    guides(x = guide_axis(angle = 60)) +
    labs(x = "individual deer", 
         y = "number of samples", 
         title = "sample count of every collared deer") +
    theme_light()
  ggsave("Plots/sample_count_per_deer.pdf", plot = p, device = "pdf")
}

