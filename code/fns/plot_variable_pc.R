plot_variable_pc <- function(var) {
  df %>%
    filter(variable %in% c(var, "Population")) %>%
    pivot_wider(names_from = variable) %>%
    rowwise() %>%
    mutate(
      value_pc = !!sym(var) / Population,
      value_pc = replace_na(value_pc, 0)
    ) %>%
    filter(value_pc > 0) %>%
    ggplot(aes(year, value_pc, colour = country)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(labels = number_format()) +
    labs(x = NULL,
         title = paste0("Evolution/adoption of ",
                        var, " per capita over time"),
         y = paste0(var, " per person"))
}
