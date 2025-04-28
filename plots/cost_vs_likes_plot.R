create_cost_vs_likes_plot <- function(data) {
  ggplot(data, aes(x = Likes, y = Cost)) +
    geom_point(alpha = 0.5, color = "darkblue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = "Hotel Cost vs. Number of Likes",
      x = "Number of Likes",
      y = "Cost (NGN)"
    ) +
    theme_minimal() +
    # Likes axis - log scale might still show scientific, standard practice
    scale_x_continuous(
      trans = "log10",
      breaks = scales::trans_breaks("log10", function(x) 10^x, n = 5),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_y_continuous(labels = scales::comma) + # Format Cost axis with commas
    theme(plot.title = element_text(hjust = 0.5))
}
