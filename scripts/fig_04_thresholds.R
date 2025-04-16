#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce two panel figure of buffered thresholds

# download tigris data from https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_04_thresholds.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

## Read in koppen-geiger geotiffs using here
directory <- here::here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")
regions <- terra::rast(directory)

# Read in domain of interest
domain <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))
domain <- domain[[1]]/domain[[1]]

# # Read in united states shapefile and convert to sf
# united_states <- tigris::counties(state = c('AL','AR','CO','CT','DE','DC','FL','GA','AZ',
#                                             'IL','IN','IA','KS','KY','LA','ME','MD','MA',
#                                             'MI','MN','MS','MO','NE','NH','NJ','NM','UT',
#                                             'NY','NC','ND','OH','OK','PA','RI','SC','SD',
#                                             'TN','TX','VT','VA','WV','WI','NV','CA'),
#                                   cb = TRUE) %>% st_as_sf()

united_states <- sf::st_read(here('data','input','threshold','tl_2022_us_county','tl_2022_us_county.shp'))
# subset to conus
united_states <- united_states[united_states$STATEFP %in% c('01','04','05','06',
                                                             '08','09','10','11',
                                                             '12','13','16','17',
                                                             '18','19','20','21',
                                                             '22','23','24','25',
                                                             '26','27','28','29',
                                                             '30','31','32','33',
                                                             '34','35','36','37',
                                                             '38','39','40','41',
                                                             '42','44','45','46',
                                                             '47','48','49','50',
                                                             '51','53','54','55','56'),]

# Convert GEOID to a numeric column
united_states <- united_states %>%
  mutate(GEOID_numeric = as.numeric(GEOID)) %>%
  select(GEOID_numeric, geometry)
# Convert united states to raster with intended resolution
# united_states <- stars::st_rasterize(united_states %>% dplyr::select(AFFGEOID, geometry))
united_states <- stars::st_rasterize(united_states %>% dplyr::select(GEOID_numeric, geometry))
# sum(unique(united_states$GEOID_numeric))
united_states <- terra::rast(united_states)
terra::res(united_states) <- c(0.008333333, 0.008333333)
# plot(united_states)
## 1-year 24-hr threshold compilation
atlas.14 <- threshold.compile(ne.dir = here::here("data", "input", "threshold", "ne1yr24ha", "ne1yr24ha.asc"),
                              orb.dir = here::here("data", "input", "threshold", "orb1yr24ha", "orb1yr24ha.asc"),
                              se.dir  = here::here("data", "input", "threshold", "se1yr24ha", "se1yr24ha.asc"),
                              sw.dir  = here::here("data", "input", "threshold", "sw1yr24ha", "sw1yr24ha.asc"),
                              tx.dir  = here::here("data", "input", "threshold", "tx1yr24ha", "tx1yr24ha.asc"),
                              mw.dir  = here::here("data", "input", "threshold", "mw1yr24ha", "mw1yr24ha.asc"))
# plot(atlas.14)
# Crop to southeast
atlas.14.se <- terra::crop(atlas.14, terra::ext(domain))

# Heat Index shapefiles
wfo      <- st_as_sf(read_sf(here::here("data", "input", "threshold", "wfo_usa.shp")))
wfo.cwa  <- st_as_sf(read_sf(here::here("data", "input", "threshold", "cwa.shp")))
wfo.state<- st_as_sf(read_sf(here::here("data", "input", "threshold", "state.shp")))
wfo.domain<-st_as_sf(read_sf(here::here("data", "output","03_cluster", "00_threshold", "wfo_southeast_expansion_dissolved.shp")))

# Convert wfo to terra SpatVector and rasterize using the 'ADVISORY' field
wfo_vect   <- terra::vect(wfo.domain)
template   <- atlas.14.se
wfo_raster <- rasterize(wfo_vect, template, field = "ADVISORY")

# Precipitation shapefiles
atlas.domain <- st_as_sf(read_sf(here::here("data", "output","03_cluster", "00_threshold", "atlas14_southeast_expansion_dissolved.shp")))
atlas.domain <- terra::vect(atlas.domain)
atlas.raster <- rasterize(atlas.domain, template, field = "kpp__01")

# Prepare data frames for ggplot
atlas_df <- as.data.frame(atlas.raster, xy = TRUE)
names(atlas_df)[3] <- "atlas_value"

wfo_df <- as.data.frame(wfo_raster, xy = TRUE)
names(wfo_df)[3] <- "advisory"

# Define plotting extent
xlims <- c(-95, -75)
ylims <- c(24, 40)

## Southeast ----

# Plot 2: Precipitation with shapefile overlays
p2 <- ggplot() +
  geom_raster(data = atlas_df, aes(x = x, y = y, fill = atlas_value)) +
  scale_fill_viridis_c(name = "Precipitation (mm)", option = "viridis") +
  geom_sf(data = wfo.cwa, fill = NA, color = "white", size = 1) +
  geom_sf(data = wfo.state, fill = NA, color = "black", size = 1) +
  coord_sf(xlim = xlims, ylim = ylims) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")

# Plot 1: Heat Index with shapefile overlays
p1 <- ggplot() +
  geom_raster(data = wfo_df, aes(x = x, y = y, fill = advisory)) +
  scale_fill_viridis_c(name = "Heat Index (°C)", option = "viridis") +
  geom_sf(data = wfo.cwa, fill = NA, color = "white", size = 1) +
  geom_sf(data = wfo.state, fill = NA, color = "black", size = 1) +
  coord_sf(xlim = xlims, ylim = ylims) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")

# Combine the two plots into one figure with panels labeled "a" and "b"
combined_plot <- p1 + p2 + plot_annotation(tag_levels = 'a')

# Define file paths using here
png_path <- here("figures","04_threholds_southeast.png")
svg_path <- here("figures","04_threholds_southeast.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = combined_plot, width = 12, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = combined_plot, width = 12, height = 6, device = "svg")

### Heat Index only ----
# Define file paths using here
png_path <- here("figures","04_threholds_southeast_heatindex.png")
svg_path <- here("figures","04_threholds_southeast_heatindex.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = p1, width = 6, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p1, width = 6, height = 6, device = "svg")

## CONUS ----
wfo_raster <- rasterize(wfo_vect, atlas.14, field = "ADVISORY")

# Prepare data frames for ggplot
atlas_df <- as.data.frame(atlas.14, xy = TRUE)
names(atlas_df)[3] <- "atlas_value"

wfo_df <- as.data.frame(wfo_raster, xy = TRUE)
names(wfo_df)[3] <- "advisory"

# Plot 1: Precipitation with shapefile overlays
p2 <- ggplot() +
  geom_raster(data = atlas_df, aes(x = x, y = y, fill = atlas_value)) +
  scale_fill_viridis_c(name = "(mm)", option = "viridis") +
  geom_sf(data = wfo.cwa, fill = NA, color = "white", size = 1) +
  geom_sf(data = wfo.state, fill = NA, color = "black", size = 1) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"))

# Plot 2: Heat Index with shapefile overlays
p1 <- ggplot() +
  geom_raster(data = wfo_df, aes(x = x, y = y, fill = advisory)) +
  scale_fill_viridis_c(name = "(°C)", option = "viridis") +
  geom_sf(data = wfo.cwa, fill = NA, color = "white", size = 1) +
  geom_sf(data = wfo.state, fill = NA, color = "black", size = 1) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"))

# Combine the two plots into one figure with panels labeled "a" and "b"
combined_plot <- p1 + p2 + plot_annotation(tag_levels = 'a')

# Define file paths using here
png_path <- here("figures","04_threholds_conus.png")
svg_path <- here("figures","04_threholds_conus.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = combined_plot, width = 12, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = combined_plot, width = 12, height = 6, device = "svg")
