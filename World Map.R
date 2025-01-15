install.packages(c("ggplot2", "rworldmap", "dplyr", "sf"))

library(ggplot2)
library(rworldmap)
library(dplyr)
library(sf)

# Get a world map as an sf (simple features) object
world_map <- st_as_sf(getMap(resolution = "low"))

# Merge your study data with the world map data
data <- read.csv('Map_data.csv')

map_data <- world_map %>%
  left_join(data, by = c("NAME" = "Country"))

# Plot the map
ggplot(map_data) +
  geom_sf(aes(fill = Frequency)) +  # Fill countries by study frequency
  scale_fill_gradient(low = "peachpuff", high = "firebrick", na.value = "white") +
  labs(fill = "Study Frequency", title = "Geographical Frequency of Host Blood Gene Biomarker Studies for Tuberculosis") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
