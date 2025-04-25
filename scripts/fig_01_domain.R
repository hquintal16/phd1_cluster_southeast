#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce study area figure with red box within conus extent

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_01_domain.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Define the output directory using the here package (this will create an "output" folder in your project root)
output_dir <- here("figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load US states data and convert to an sf object
# us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
globe <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
us.states <- globe

# Create the plot with a thick red box over the CONUS region
p <- ggplot() +
  geom_sf(data = us.states, fill = "white", color = "black") +
  geom_rect(aes(xmin = -95, xmax = -75, ymin = 24, ymax = 40),
            fill = NA, color = "red", size = 1) +
  # coord_sf(xlim = c(-125, -67), ylim = c(25, 50)) +
  theme_bw()

p

# Define file paths using here
png_path <- here("figures","01_domain.png")
svg_path <- here("figures","01_domain.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = p, width = 8, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p, width = 8, height = 6, device = "svg")

