#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce two panel figure of space time metric 1

# Load Libraries & Set Project Root ----
library(here)
here::set_here("V:/users/hquintal/phd1_cluster_southeast")
here::i_am("scripts/fig_s03_space_time_metric_1.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))


# Helper function: Floor a year to the start of its decade
floor_decade <- function(year) {
  year - year %% 10
}

# Function to read and prepare data
prepare_stm <- function(file_path, var_name) {
  stm <- readr::read_csv(file_path)
  
  stm <- stm %>%
    mutate(
      date = as.Date(paste0(as.character(year_mo), "01"), format = "%Y%m%d"),
      year = year(date),
      decade = floor_decade(year),
      month_num = month(date),
      season = case_when(
        month_num %in% c(12, 1, 2) ~ "Winter",
        month_num %in% c(3, 4, 5) ~ "Spring",
        month_num %in% c(6, 7, 8) ~ "Summer",
        TRUE ~ "Autumn"
      ),
      month = factor(month.abb[month_num],
                     levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", 
                                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),
      season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
      variable = var_name
    )
  return(stm)
}

# Define file paths (adjust as necessary)
heat_file <- here("data", "output", "02_covariance", "03_space_time_metric", 
                  "heat_index", "month", "heat_index_space_time_metric_optimal.txt")
precip_file <- here("data", "output", "02_covariance", "03_space_time_metric", 
                    "precipitation", "month", "precipitation_space_time_metric_optimal.txt")

# Prepare the datasets
stm_heat <- prepare_stm(heat_file, "HeatIndex")
stm_precip <- prepare_stm(precip_file, "Precipitation")

# Replace Inf values in stm1 with NA in the precipitation dataset
stm_precip <- stm_precip %>%
  mutate(stm1 = ifelse(is.infinite(stm1), NA, stm1))

# Combine the datasets
raw_data <- bind_rows(stm_heat, stm_precip)

# Define colors with 60% opacity
colors <- c("HeatIndex" = scales::alpha("red", 0.6), 
            "Precipitation" = scales::alpha("blue", 0.6))

# Compute common y-axis range for stm1 from both datasets
common_ylim <- range(raw_data$stm1, na.rm = TRUE)

## Create individual plots

# HeatIndex panel: Scatter plot with loess smoother (with CI)
p_hi_stm1 <- ggplot(filter(raw_data, variable == "HeatIndex"), aes(x = date, y = stm1)) +
  geom_point(color = colors["HeatIndex"]) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Space Time Metric", x = "Date") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# Precipitation panel: Scatter plot with loess smoother (with CI)
p_pr_stm1 <- ggplot(filter(raw_data, variable == "Precipitation"), aes(x = date, y = stm1)) +
  geom_point(color = colors["Precipitation"]) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Space Time Metric", x = "Date") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw() +
  # Remove y-axis elements for the right panel to avoid redundancy
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Combine the two panels side-by-side using patchwork
combined_plot <- p_hi_stm1 + p_pr_stm1 + plot_annotation(tag_levels = "a")

# Define file paths to save the plot
png_path <- here("figures", "s03_space_time_metric_stm1.png")
svg_path <- here("figures", "s03_space_time_metric_stm1.svg")

# Save the plot as PNG and SVG
ggsave(filename = png_path, plot = combined_plot, width = 9, height = 4.5, dpi = 300)
ggsave(filename = svg_path, plot = combined_plot, width = 9, height = 4.5, device = "svg")
