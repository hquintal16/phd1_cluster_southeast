#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce four panel figure of space time metrics aggregated at different temporal units

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_02_covariance_models.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Define a helper function to floor a year to the start of its decade
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

# Combine the datasets for plotting
raw_data <- bind_rows(stm_heat, stm_precip)

# Define colors with alpha adjustment (60% opacity)
colors <- c("HeatIndex" = alpha("red", 0.6), "Precipitation" = alpha("blue", 0.6))

# For the Precipitation plots, force a common y-axis range:
precip_data <- filter(raw_data, variable == "Precipitation")
common_ylim <- range(precip_data$stm2, na.rm = TRUE)

# Create individual plots

# (a) HeatIndex Raw: Scatter plot of raw data (points)
p_hi_raw <- ggplot(filter(raw_data, variable == "HeatIndex"), aes(x = date, y = stm2)) +
  geom_point(color = colors["HeatIndex"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Heat Index Space Time Metric", x = "Date") +
  theme_bw()

# (b) HeatIndex Month: Boxplot aggregated by month
p_hi_month <- ggplot(filter(raw_data, variable == "HeatIndex"), aes(x = month, y = stm2)) +
  geom_boxplot(fill = colors["HeatIndex"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Heat Index Space Time Metric", x = "Month") +
  theme_bw()

# (c) HeatIndex Season: Boxplot aggregated by season with legend inside
p_hi_season <- ggplot(filter(raw_data, variable == "HeatIndex"), aes(x = season, y = stm2)) +
  geom_boxplot(fill = colors["HeatIndex"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Heat Index Space Time Metric", x = "Season") +
  theme_bw()

# (d) Precipitation Raw: Scatter plot (points), with common y-axis limits
p_pr_raw <- ggplot(filter(raw_data, variable == "Precipitation"), aes(x = date, y = stm2)) +
  geom_point(color = colors["Precipitation"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Precipitation Space Time Metric", x = "Date") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# (e) Precipitation Month: Boxplot aggregated by month, with common y-axis limits
p_pr_month <- ggplot(filter(raw_data, variable == "Precipitation"), aes(x = month, y = stm2)) +
  geom_boxplot(fill = colors["Precipitation"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Precipitation Space Time Metric", x = "Month") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# (f) Precipitation Season: Boxplot aggregated by season, with common y-axis limits
p_pr_season <- ggplot(filter(raw_data, variable == "Precipitation"), aes(x = season, y = stm2)) +
  geom_boxplot(fill = colors["Precipitation"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Precipitation Space Time Metric", x = "Season") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# Remove redundant axes elements:
# For the top row (HeatIndex panels), remove x-axis elements.
p_hi_raw <- p_hi_raw + theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank())
p_hi_month <- p_hi_month + theme(axis.title.x = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank())
p_hi_season <- p_hi_season + theme(axis.title.x = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.ticks.x = element_blank())

# For non-left panels, remove y-axis elements by using both theme() and scale_y_continuous.
p_hi_month <- p_hi_month +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(guide = "none")
p_hi_season <- p_hi_season +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(guide = "none")
p_pr_month <- p_pr_month +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(guide = "none")
p_pr_season <- p_pr_season +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(guide = "none")

# Combine the panels using patchwork
# Layout:
# Top row (HeatIndex): p_hi_raw | p_hi_month | p_hi_season
# Bottom row (Precipitation): p_pr_raw | p_pr_month | p_pr_season
combined_plot <- (p_hi_raw + p_hi_month + p_hi_season) /
  (p_pr_raw + p_pr_month + p_pr_season) +
  plot_annotation(tag_levels = "a")

# Define file paths using here
png_path <- here("figures","02_space_time_metric.png")
svg_path <- here("figures","02_space_time_metric.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = combined_plot, width = 9, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = combined_plot, width = 9, height = 6, device = "svg")
