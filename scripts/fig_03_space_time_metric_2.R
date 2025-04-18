#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce four panel figure of space time metrics aggregated at different temporal units

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_03_space_time_metric_2.R")  # Adjust this file path as needed
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

# Define file paths
heat_file <- here("data", "output", "02_covariance", "03_space_time_metric", 
                  "heat_index", "month", "heat_index_space_time_metric_optimal.txt")
precip_file <- here("data", "output", "02_covariance", "03_space_time_metric", 
                    "precipitation", "month", "precipitation_space_time_metric_optimal.txt")

# Prepare the datasets
stm_heat   <- prepare_stm(heat_file, "HeatIndex")
stm_precip <- prepare_stm(precip_file, "Precipitation")
raw_data   <- bind_rows(stm_heat, stm_precip)

# Define colors with alpha adjustment (60% opacity)
colors <- c("HeatIndex" = alpha("red",   0.6),
            "Precipitation" = alpha("blue", 0.6))

# Precipitation y‐limits (for reference in bottom row)
precip_data <- filter(raw_data, variable == "Precipitation")
common_ylim <- range(precip_data$stm2, na.rm = TRUE)

# (a) HeatIndex Raw: Scatter plot of raw data
p_hi_raw <- ggplot(filter(raw_data, variable == "HeatIndex"),
                   aes(x = date, y = stm2)) +
  geom_point(color = colors["HeatIndex"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Heat Index Space Time Metric") +
  coord_cartesian(ylim = c(0, 6)) +
  theme_bw() +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank()
  )

# (b) HeatIndex Month: Boxplot by month
p_hi_month <- ggplot(filter(raw_data, variable == "HeatIndex"),
                     aes(x = month, y = stm2)) +
  geom_boxplot(fill = colors["HeatIndex"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Month") +
  coord_cartesian(ylim = c(0, 6)) +
  theme_bw() +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank()
  ) +
  scale_y_continuous(guide = "none")

# (c) HeatIndex Season: Boxplot by season
p_hi_season <- ggplot(filter(raw_data, variable == "HeatIndex"),
                      aes(x = season, y = stm2)) +
  geom_boxplot(fill = colors["HeatIndex"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Season") +
  coord_cartesian(ylim = c(0, 6)) +
  theme_bw() +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank()
  ) +
  scale_y_continuous(guide = "none")

# (d) Precipitation Raw: Scatter plot, with its own y‐limits
p_pr_raw <- ggplot(filter(raw_data, variable == "Precipitation"),
                   aes(x = date, y = stm2)) +
  geom_point(color = colors["Precipitation"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = "Precipitation Space Time Metric", x = "Date") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# (e) Precipitation Month: Boxplot by month
p_pr_month <- ggplot(filter(raw_data, variable == "Precipitation"),
                     aes(x = month, y = stm2)) +
  geom_boxplot(fill = colors["Precipitation"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Month") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank()
  ) +
  scale_y_continuous(guide = "none")

# (f) Precipitation Season: Boxplot by season
p_pr_season <- ggplot(filter(raw_data, variable == "Precipitation"),
                      aes(x = season, y = stm2)) +
  geom_boxplot(fill = colors["Precipitation"]) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Season") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank()
  ) +
  scale_y_continuous(guide = "none")

# Combine with patchwork
combined_plot <- (p_hi_raw + p_hi_month + p_hi_season) /
  (p_pr_raw + p_pr_month + p_pr_season) +
  plot_annotation(tag_levels = "a")


# Define file paths using here
png_path <- here("figures","03_space_time_metric.png")
svg_path <- here("figures","03_space_time_metric.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = combined_plot, width = 9, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = combined_plot, width = 9, height = 6, device = "svg")

# Hyp testing ----
stm_heat <- readr::read_csv(here("data", "output", "02_covariance", "03_space_time_metric", 
                                 "heat_index", "month", "heat_index_space_time_metric_optimal.txt"))
stm_precip <- readr::read_csv(here("data", "output", "02_covariance", "03_space_time_metric", 
                                    "precipitation", "month", "precipitation_space_time_metric_optimal.txt"))
## Shapiro-Wilk test ----
# Test for normality
# Extract the sample of interest (ar2 column)
stm_heat <- stm_heat$stm2
stm_precip <- stm_precip$stm2

# Test for normality using the Shapiro-Wilk test
stm_heat_shapiro_result <- shapiro.test(stm_heat)
stm_precip_shapiro_result <- shapiro.test(stm_precip)

## Wilcoxon-signed rank test ----
stm_heat_wilcox_test_result <- wilcox.test(stm_heat, mu = 1)
stm_precip_wilcox_test_result <- wilcox.test(stm_precip, mu = 1)
