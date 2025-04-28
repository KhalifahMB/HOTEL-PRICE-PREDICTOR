create_cost_by_location_plot <- function(data) {
  # Use original Location for counting and filtering top N
  location_counts_plot <- table(data$Location)
  # Use the same threshold as in global.R for consistency
  top_locations_plot <- names(location_counts_plot[location_counts_plot >= 20])

  df_top_locations_plot <- data %>%
    filter(Location %in% top_locations_plot)

  # Plotting uses the original Location names for the axis
  ggplot() +
  geom_bar(
    data = df_top_locations_plot, 
    mapping = aes(x = reorder(Location, Cost, FUN = median),y=Cost, fill=Location),
    stat = "identity"
    ) +
    labs(
      title = "Hotel Cost Distribution by Top Locations in Abuja",
      x = "Location",
      y = "Cost (NGN)"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9), # size 9 for vertical labels after flip
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_discrete() + # Ensure discrete scale for the flipped categorical axis
    scale_y_continuous(labels = scales::comma) + # Format y-axis (Cost) labels with commas
    coord_flip() # Flip axes
}
