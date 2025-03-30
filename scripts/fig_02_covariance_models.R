#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce four panel figure of monthly covariance models

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_02_covariance_models.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# --- File reading & data processing (assumed already done) ---

# Define directories for heat index and precipitation
heat_dir <- here("data", "output", "02_covariance", "03_space_time_metric", "heat_index", "month")
precip_dir <- here("data", "output", "02_covariance", "03_space_time_metric", "precipitation", "month")

# List all files in the directories that contain "optimal" in their names
heat_files <- list.files(heat_dir, pattern = "optimal", full.names = TRUE)
precip_files <- list.files(precip_dir, pattern = "optimal", full.names = TRUE)

# Read the files and add a "type" column for identification
heat_data <- read_csv(heat_files) %>% mutate(type = "heat_index")
precip_data <- read_csv(precip_files) %>% mutate(type = "precipitation")

# Combine the two datasets
data_all <- bind_rows(heat_data, precip_data)

# --- Compute curves ---
# Update the x sequence to start at 0
compute_curves <- function(sill, alpha1, alpha2, ar1, ar2, at1, at2, model) {
  # Create a sequence from 0 to 15 in increments of 0.1
  x <- seq(0, 10, 0.1)
  
  # At x = 0, exp(- (0^2)/(ar^2)) = 1 so value = sill*(alpha1+alpha2)
  space <- sill * (alpha1 * exp( -(x^2)/(ar1^2) ) + alpha2 * exp(-3 * x / ar2))
  time  <- sill * (alpha1 * exp( -(x^2)/(at1^2) ) + alpha2 * exp(-3 * x / at2))

  return(list(x = x, space = space, time = time))
}

# Original, if I hadn't accidentally given nested exp-exp the same equation as gau-exp
# compute_curves <- function(sill, alpha1, alpha2, ar1, ar2, at1, at2, model) {
#   # Create a sequence from 0 to 15 in increments of 0.1
#   x <- seq(0, 10, 0.1)
#   
#   if(model == "Nested Gaussian Exponential") {
#     # At x = 0, exp(- (0^2)/(ar^2)) = 1 so value = sill*(alpha1+alpha2)
#     space <- sill * (alpha1 * exp( -(x^2)/(ar1^2) ) + alpha2 * exp(-3 * x / ar2))
#     time  <- sill * (alpha1 * exp( -(x^2)/(at1^2) ) + alpha2 * exp(-3 * x / at2))
#   } else if(model == "Nested Exponential") {
#     space <- sill * (alpha1 * exp((-3 * x) / ar1) + alpha2 * exp(x / ar2))
#     time  <- sill * (alpha1 * exp((-3 * x) / at1) + alpha2 * exp(x / at2))
#   } else {
#     space <- rep(NA, length(x))
#     time  <- rep(NA, length(x))
#   }
#   return(list(x = x, space = space, time = time))
# }

# Add a unique identifier for each row
data_all <- data_all %>% mutate(id = row_number())

# Compute curves for each row
data_curves <- data_all %>%
  rowwise() %>%
  mutate(curve = list(compute_curves(sill, alpha1, alpha2, ar1, ar2, at1, at2, model)),
         x     = list(curve$x),
         space = list(curve$space),
         time  = list(curve$time)) %>%
  ungroup()

# Unnest the computed vectors
data_long <- data_curves %>%
  select(id, type, year_mo, model, x, space, time) %>%
  unnest(cols = c(x, space, time))

# Reshape data: combine space and time columns with an indicator
data_long2 <- data_long %>%
  pivot_longer(cols = c(space, time), names_to = "model_type", values_to = "value")

# --- Compute y-axis limits ---
# For each type, we want the maximum value at x==0 across both space and time curves
heat_ylim_upper <- data_long2 %>% 
  filter(type == "heat_index", x == 0) %>% 
  summarise(max_val = max(value, na.rm = TRUE)) %>% 
  pull(max_val)

precip_ylim_upper <- data_long2 %>% 
  filter(type == "precipitation", x == 0) %>% 
  summarise(max_val = max(value, na.rm = TRUE)) %>% 
  pull(max_val)

# --- Create individual plots ---
# Subset data for each panel
data_heat_space   <- data_long2 %>% filter(type == "heat_index", model_type == "space")
data_heat_time    <- data_long2 %>% filter(type == "heat_index", model_type == "time")
data_precip_space <- data_long2 %>% filter(type == "precipitation", model_type == "space")
data_precip_time  <- data_long2 %>% filter(type == "precipitation", model_type == "time")

# Top left: Heat Index, Space
p_heat_space <- ggplot(data_heat_space, aes(x = x, y = value, group = id)) +
  geom_line(alpha = 0.1) +
  theme_bw() +
  labs(x = "Spatial Range (0.25°)", y = "Covariance of heat index (°C²)",
       title = "a") +
  ylim(0, heat_ylim_upper) +
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10))) 

# Top right: Heat Index, Time
p_heat_time <- ggplot(data_heat_time, aes(x = x, y = value, group = id)) +
  geom_line(alpha = 0.1) +
  theme_bw() +
  labs(x = "Temporal Lag (days)", y = NULL,
       title = "b") +
  ylim(0, heat_ylim_upper) +
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold"))

# Bottom left: Precipitation, Space
p_precip_space <- ggplot(data_precip_space, aes(x = x, y = value, group = id)) +
  geom_line(alpha = 0.1) +
  theme_bw() +
  labs(x = "Spatial Range (0.25°)", y = "Covariance of precipitation (mm²)",
       title = "c") +
  ylim(0, precip_ylim_upper) +
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10))) 

# Bottom right: Precipitation, Time
p_precip_time <- ggplot(data_precip_time, aes(x = x, y = value, group = id)) +
  geom_line(alpha = 0.1) +
  theme_bw() +
  labs(x = "Temporal Lag (hours)", y = NULL,
       title = "d") +
  ylim(0, precip_ylim_upper) +
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold"))

# --- Combine the plots into a 2x2 grid ---
p_combined <- (p_heat_space + p_heat_time) / (p_precip_space + p_precip_time)

# Define file paths using here
png_path <- here("figures","02_covariance.png")
svg_path <- here("figures","02_covariance.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = p_combined, width = 8, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = p_combined, width = 8, height = 6, device = "svg")
