# AI_project_functions.r

make_rating_plot <- function(data, outcome, title) {
  
  if (nrow(data) == 0) {
    stop("The input data has 0 rows. Check your filter before calling make_rating_plot().")
  }
  
  outcome_name <- rlang::as_name(rlang::enquo(outcome))
  
  data %>%
    mutate(
      message_id = factor(message_id),
      rater_id = factor(rater_id),
      true_source = factor(true_source),
      outcome_value = as.numeric(as.character({{ outcome }}))
    ) %>%
    ggplot(
      aes(
        x = message_id,
        y = outcome_value,
        color = rater_id
      )
    ) +
    geom_point(
      position = position_jitter(width = 0, height = 0.15),
      alpha = 0.5,
      size = 2
    ) +
    facet_wrap(~ true_source, nrow = 1, scales = "free_x") +
    scale_y_continuous(
      breaks = 1:5
    ) +
    coord_cartesian(
      ylim = c(0.8, 5.2)
    ) +
    labs(
      title = title,
      x = "Messages",
      y = stringr::str_to_title(outcome_name),
      color = "Rater ID"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "right"
    )
}