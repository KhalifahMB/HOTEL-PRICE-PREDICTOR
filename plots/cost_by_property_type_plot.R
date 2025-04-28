create_cost_by_property_type_plot <- function(data) {
  ggplot() +
    geom_bar(
    data = data,
    mapping= aes(x = reorder(Property_type_Grouped, Cost, FUN = median), y = Cost, fill= Property_type_Grouped),
    stat = "identity"
    ) +
    labs(
      title = "Hotel Cost Distribution by Grouped Property Type",
      x = "Property Type Group",
      y = "Cost (NGN)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(labels = scales::comma) # Format y-axis (Cost) labels with commas
}
