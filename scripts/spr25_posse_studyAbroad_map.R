# Load libraries
library(ggplot2)
library(maps)
library(dplyr)

# Coordinates and labels
study_abroad <- data.frame(
  Location = c("Barcelona", "Paris", "Singapore", "Madrid", 
               "Montpellier", "Prague", "Tokyo",
               "Washington DC", "New York",
               "Kathmandu", "Brisbane"),
  lon = c(2.1734, 2.3522, 103.8198, -3.7038, 3.8777, 14.4378, 139.6917,
          -77.0369, -74.0060, 85.3240, 153.0251),
  lat = c(41.3851, 48.8566, 1.3521, 40.4168, 43.6117, 50.0755, 35.6895,
          38.9072, 40.7128, 27.7172, -27.4698)
)

# Adjust label y-offsets
study_abroad <- study_abroad %>%
  mutate(
    y_offset = case_when(
      Location == "Montpellier" ~ 0.6,
      Location == "Barcelona" ~ -0.8,
      Location == "New York" ~ 0.3,            # Move label closer
      Location == "Washington DC" ~ -1.2,
      Location == "Brisbane" ~ 1.2,
      TRUE ~ 0
    )
  )

# Get base map
world <- map_data("world")

# Plot the map
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(data = study_abroad, aes(x = lon, y = lat), color = "#C5050C", size = 3) +
  geom_text(
    data = study_abroad,
    aes(
      x = lon,
      y = lat + y_offset,
      label = Location,
      vjust = case_when(
        Location == "Barcelona" ~ 1.2,
        Location == "New York" ~ -1,
        Location == "Brisbane" ~ -1,
        Location == "Washington DC" ~ 1.5,
        TRUE ~ 0.5
      ),
      hjust = case_when(
        Location == "Washington DC" ~ -0.1,
        Location == "Singapore" ~ 1.1,
        Location == "Tokyo" ~ -0.1,
        Location == "New York" ~ -0.2,          # Bring label closer
        Location == "Brisbane" ~ 1.2,
        Location %in% c("Paris", "Madrid") ~ 1.1,
        TRUE ~ -0.1
      )
    ),
    size = 5,
    color = "black"
  ) +
  coord_fixed(1.3, xlim = c(-75, 160), ylim = c(-40, 60)) +
  theme_void() +
  theme(
    plot.title = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# Save to Downloads
ggsave("~/Downloads/viz15_study_abroad_map.png", width = 12, height = 6, dpi = 300, bg = "transparent")