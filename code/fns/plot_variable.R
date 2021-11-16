plot_variable <- function(var) {
  df %>%
    filter(variable == var,
           value > 0) %>%
    ggplot(aes(year, value, colour = country)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(labels = number_format()) +
    labs(title = paste0("Evolution/adoption of ", var, " over time"),
         x = NULL,
         y = NULL)
}
