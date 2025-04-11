#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 7 panel figures of noaa events, intensity, duration, exposed area, impacts

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s06_noaa_time_series.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# time series ----

# Read in the CSV file (update the file path as needed)
df <- read_csv(here("data", "output", "05_validation", "summary", "noaa", "NOAA_excess_heat_summary.csv"))

# Convert the start_time and end_time columns to Date objects and compute county_count.
# The county_count is the number of entries in exposed_counties (i.e. number of semicolons + 1).
df <- df %>%
  mutate(start_time = as.Date(start_time, format = "%m/%d/%Y"),
         end_time   = as.Date(end_time, format = "%m/%d/%Y"),
         county_count = str_count(exposed_counties, ";") + 1)

# Expand each event so that each date in the event's range is counted,
# carrying along the county_count value.
df_expanded <- df %>%
  rowwise() %>%
  mutate(event_dates = list(seq.Date(start_time, end_time, by = "day"))) %>% 
  unnest(cols = c(event_dates)) %>%
  ungroup()

# Aggregate the expanded data: sum the county_count per date.
daily_counts <- df_expanded %>%
  group_by(event_dates) %>%
  summarize(daily_instances = sum(county_count), .groups = "drop")

# Fill in missing dates with 0 instances using tidyr::complete
daily_counts_complete <- daily_counts %>%
  complete(event_dates = seq.Date(min(event_dates), max(event_dates), by = "day"),
           fill = list(daily_instances = 0)) %>%
  arrange(event_dates)

# Compute a 7-day rolling average (centered)
# daily_counts_complete <- daily_counts_complete %>%
  # mutate(rolling_avg = zoo::rollmean(daily_instances, k = 7, fill = NA, align = "center"))

# Create a time series plot with the rolling average.
# The plot spans from 1940-01-01 to 2023-12-31 with x-axis ticks every 10 years.
p <- ggplot(daily_counts_complete, aes(x = event_dates)) +
  geom_line(aes(y = daily_instances), color = "darkred") +
  labs(x = "Date",
       y = "Counties w/ a NOAA Excess Heat Episode") +
  scale_x_date(
    limits = c(as.Date("1940-01-01"), as.Date("2023-12-31")),
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  theme_bw() +
  # Remove borders, axis lines, and grid lines; set interior panel to white
  theme(
    panel.border      = element_blank(),
    axis.line         = element_blank(),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  )

# Save the plot to PNG and SVG files.
png_path_noaa <- here("figures", "06_noaa_time_series.png")
svg_path_noaa <- here("figures", "06_noaa_time_series.svg")

ggsave(filename = png_path_noaa, plot = p, width = 12, height = 4, dpi = 300)
ggsave(filename = svg_path_noaa, plot = p, width = 12, height = 4, device = "svg")

# spatial map ----

# Define your folder path containing the .nc files
folder_path <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")

# List all .nc files in the folder that include "Daily_Extent" in their filename
nc_files <- list.files(path = folder_path, pattern = "Daily_Extent.*\\.nc$", full.names = TRUE)

# Check if any files are found
if(length(nc_files) == 0) {
  stop("No .nc files with 'Daily_Extent' found in the specified folder.")
}

# Read each .nc file as a terra SpatRaster object
raster_list <- lapply(nc_files, rast)

# Sum all the rasters element-wise using Reduce and the '+' operator.
# (This assumes that all rasters share the same extent, resolution, and projection)
raster_sum <- Reduce('+', raster_list)
raster_sum <- sum(raster_sum)  # if additional aggregation is needed

# Read the shapefile (adjust the path as needed)
shapefile_path <- here("data", "input", "threshold", "state.shp")
shp <- vect(shapefile_path)

# Convert the terra shapefile to an sf object for ggplot
shp_sf <- st_as_sf(shp)

# Convert the summed raster to a data frame with x, y coordinates.
# The resulting data frame contains columns for x, y, and the raster values.
raster_df <- as.data.frame(raster_sum, xy = TRUE)
colnames(raster_df)[3] <- "value"  # Rename the third column to 'value'

# Determine the maximum value from the raster data (excluding NA)
max_value <- max(raster_df$value, na.rm = TRUE)

# Create bins:
# - Assign a separate bin ("0") for cells where the value is exactly 0.
# - For positive values, divide the range (0, max_value] into 4 equal intervals.
raster_df$bin <- NA  # initialize bin column

# Bin for value exactly 0
raster_df$bin[raster_df$value == 0] <- "0"

# For positive values, calculate equally spaced intervals
non0_idx <- which(raster_df$value > 0)
step <- max_value / 4
# Define breakpoints and corresponding bin labels
breaks_vec <- c(0, step, 2 * step, 3 * step, 4 * step)
bin_labels <- c(paste0("(0, ", round(step, 2), "]"),
                paste0("(", round(step, 2), ", ", round(2 * step, 2), "]"),
                paste0("(", round(2 * step, 2), ", ", round(3 * step, 2), "]"),
                paste0("(", round(3 * step, 2), ", ", round(4 * step, 2), "]"))

# Bin the positive values (excluding 0)
raster_df$bin[non0_idx] <- as.character(
  cut(raster_df$value[non0_idx],
      breaks = breaks_vec,
      include.lowest = FALSE,  # do not include 0
      right = TRUE,
      labels = bin_labels)
)

# Order the bins so that "0" comes first, then the positive bins
raster_df$bin <- factor(raster_df$bin, levels = c("0", bin_labels))

# Create a color palette: assign white for "0" and generate 4 colors from lightpink to darkred
positive_colors <- colorRampPalette(c("lightpink", "darkred"))(4)
fill_colors <- c("0" = "white", setNames(positive_colors, bin_labels))

# Create the ggplot
p <- ggplot() +
  # Use geom_raster to plot the binned data (NA values will be colored white)
  geom_raster(data = raster_df, aes(x = x, y = y, fill = bin)) +
  # Overlay the shapefile with black borders
  geom_sf(data = shp_sf, fill = NA, color = "black", size = 1) +
  # Limit plot extent to the defined longitude and latitude ranges
  coord_sf(xlim = c(-95, -75), ylim = c(24, 40)) +
  # Use scale_fill_manual with our custom color palette
  scale_fill_manual(values = fill_colors, na.value = "white") +
  labs(fill = "Days") +
  theme_bw() +
  # Remove the x and y axis labels and adjust the legend position
  theme(
    axis.title = element_blank(),
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom")
  )

# Print the plot
print(p)

# Save the plot
png_path_noaa <- here("figures", "06_noaa_spatial_counts.png")
svg_path_noaa <- here("figures", "06_noaa_spatial_counts.svg")

ggsave(filename = png_path_noaa, plot = p, width = 5, height = 5, dpi = 300)
ggsave(filename = svg_path_noaa, plot = p, width = 5, height = 5, device = "svg")
