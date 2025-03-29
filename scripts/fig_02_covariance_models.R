#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce study area figure with red box within conus extent

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_02_covariance_models.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Load required packages
library(here)      # For file paths
library(dplyr)     # For data manipulation
library(tidyr)     # For data reshaping
library(ggplot2)   # For plotting
library(readr)     # For reading CSV files

# # Define file paths using the here package
heat_file <- here("data","output","02_covariance","03_space_time_metric","heat_index","month","heat_index_space_time_metric_optimal.csv")
# precip_file <- here("data","output","02_covariance","03_space_time_metric","precipitation","month","precipitation_space_time_metric_optimal.csv")
# a <- read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/02_covariance/03_space_time_metric/precipitation/month/precipitation_space_time_metric_optimal.csv')
# a <- read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/02_covariance/03_space_time_metric/heat_index/month/heat_index_space_time_metric_optimal.csv')
# a <- read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/02_covariance/03_space_time_metric/precipitation/month/precipitation_space_time_metric_nested_exponential.csv')

# Read the CSV files and add a "type" column for identification
heat_data <- read_csv(heat_file) %>% mutate(type = "heat_index")
precip_data <- read_csv(precip_file) %>% mutate(type = "precipitation")

# Combine the two datasets
data_all <- bind_rows(heat_data, precip_data)

# Define a function to compute the space and time curves
compute_curves <- function(sill, alpha1, alpha2, ar1, ar2, at1, at2, model) {
  x <- 1:100
  if(model == "Nested Gaussian Exponential") {
    # Simplify (-3 * (x^2))/(3 * (ar1)^2) to -(x^2)/(ar1^2)
    space <- sill * (alpha1 * exp(-3 (x^2) / (3*ar1^2)) + alpha2 * exp(-3 * x / ar2))
    time  <- sill * (alpha1 * exp(-3 (x^2) / (3*at1^2)) + alpha2 * exp(-3 * x / at2))
  } else if(model == "Nested Exponential") {
    space <- sill * (alpha1 * exp((-3 * x) / (ar1)) + alpha2 * exp(x / ar2))
    time  <- sill * (alpha1 * exp((-3 * x) / (at1)) + alpha2 * exp(x / at2))
  } else {
    # For any other model type, return NA vectors
    space <- rep(NA, length(x))
    time  <- rep(NA, length(x))
  }
  return(list(x = x, space = space, time = time))
}

# Add a unique identifier for each row
data_all <- data_all %>% mutate(id = row_number())

# Compute curves for each row using the compute_curves function
data_curves <- data_all %>%
  rowwise() %>%
  mutate(curve = list(compute_curves(sill, alpha1, alpha2, ar1, ar2, at1, at2, model)),
         x     = list(curve$x),
         space = list(curve$space),
         time  = list(curve$time)) %>%
  ungroup()

# Unnest the computed vectors so that each row yields 100 rows of (x, space, time)
data_long <- data_curves %>%
  select(id, type, year_mo, model, x, space, time) %>%
  unnest(cols = c(x, space, time))

# Reshape data so that space and time values are in one column with an indicator variable "model_type"
data_long2 <- data_long %>%
  pivot_longer(cols = c(space, time), names_to = "model_type", values_to = "value")

# Create the 2x2 panel plot:
# - Top panels: heat_index (space on left, time on right)
# - Bottom panels: precipitation (space on left, time on right)
p <- ggplot(data_long2, aes(x = x, y = value, group = id)) +
  geom_line(alpha = 0.1) +
  facet_grid(type ~ model_type) +
  theme_minimal() +
  labs(x = "Index (1:100)", 
       y = "Model Value", 
       title = "Space and Time Models for Heat Index and Precipitation")

# Display the plot
print(p)
