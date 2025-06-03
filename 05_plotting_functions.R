# 05_plotting_functions.R - Reusable Plotting Functions

# Custom theme for ggplot2
custom_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = THEME_CONFIG$font_family),
      axis.text.x = element_text(size = THEME_CONFIG$font_size_base, 
                                 color = THEME_CONFIG$text_secondary),
      axis.text.y = element_text(size = THEME_CONFIG$font_size_base, 
                                 color = THEME_CONFIG$text_secondary),
      axis.title = element_text(size = THEME_CONFIG$font_size_large, 
                                color = THEME_CONFIG$text_primary, 
                                face = "bold"),
      plot.title = element_text(size = THEME_CONFIG$font_size_large + 2, 
                                color = THEME_CONFIG$text_primary, 
                                face = "bold", 
                                hjust = 0.5),
      legend.position = "none",
      plot.background = element_rect(fill = THEME_CONFIG$bg_primary, color = NA),
      panel.background = element_rect(fill = THEME_CONFIG$bg_primary, color = NA),
      panel.grid.major = element_line(color = alpha(THEME_CONFIG$text_secondary, 0.2)),
      panel.grid.minor = element_blank()
    )
}

# Create world map with updated lighter color and better country identification
create_world_map <- function(map_data) {
  leaflet(map_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~lon, lat = ~lat,
      radius = ~scaled_size,
      color = THEME_CONFIG$primary_colors[1],  # Using the new lighter coral color
      fillOpacity = 0.7,
      popup = ~paste(name, ": ", count, " articles"),
      label = ~paste(name, ": ", count),
      layerId = ~name  # Add this to help identify countries on click
    ) %>%
    addControl(
      html = paste0(
        "<h4 style='color:", THEME_CONFIG$text_primary, 
        "; text-align:center; font-family:", THEME_CONFIG$font_family, 
        ";'>Countries of origin of the samples in the dataset</h4>"
      ),
      position = "topright"
    )
}