#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce 6 panel plot of sse, numerators, denominators

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s02_sse_numerator_denominator.R")  # Adjust this file path as needed
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

# Define colors with 60% opacity for each variable
colors <- c("HeatIndex" = scales::alpha("red", 0.6), 
            "Precipitation" = scales::alpha("blue", 0.6))

# For the Precipitation plots, enforce a common y-axis range based on the sse column.
precip_data <- filter(raw_data, variable == "Precipitation")
common_ylim <- range(precip_data$sse, na.rm = TRUE)

## Create individual plots

# --- HeatIndex Plots (Left Column) ---

# (a) HeatIndex Raw: Scatter plot of raw sse data
p_hi_raw <- ggplot(filter(raw_data, variable == "HeatIndex"), aes(x = date, y = sse)) +
  geom_point(color = colors["HeatIndex"]) +
  labs(y = "SSE", x = "Date") +
  theme_bw()

# (b) HeatIndex Numerator: Time series with loess smoother and confidence interval
p_hi_numer <- ggplot(filter(raw_data, variable == "HeatIndex"), aes(x = date, y = stm1.numerator)) +
  geom_point(color = colors["HeatIndex"]) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(y = "Numerator", x = "Date") +
  theme_bw()

# (c) HeatIndex Denom: Time series with loess smoother and confidence interval
p_hi_denom <- ggplot(filter(raw_data, variable == "HeatIndex"), aes(x = date, y = stm1.denominator)) +
  geom_point(color = colors["HeatIndex"]) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(y = "Denominator", x = "Date") +
  theme_bw()

# --- Precipitation Plots (Right Column) ---

# (d) Precipitation Raw: Scatter plot of raw sse data with common y-axis limits
p_pr_raw <- ggplot(filter(raw_data, variable == "Precipitation"), aes(x = date, y = sse)) +
  geom_point(color = colors["Precipitation"]) +
  labs(y = "SSE", x = "Date") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# (e) Precipitation Numerator: Time series with loess smoother and confidence interval
p_pr_numer <- ggplot(filter(raw_data, variable == "Precipitation"), aes(x = date, y = stm1.numerator)) +
  geom_point(color = colors["Precipitation"]) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(y = "Numerator", x = "Date") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# (f) Precipitation Denom: Time series with loess smoother and confidence interval
p_pr_denom <- ggplot(filter(raw_data, variable == "Precipitation"), aes(x = date, y = stm1.denominator)) +
  geom_point(color = colors["Precipitation"]) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(y = "Denominator", x = "Date") +
  coord_cartesian(ylim = common_ylim) +
  theme_bw()

# Remove redundant axes elements:
# For the upper rows (raw and numerator), remove x-axis elements.
p_hi_raw <- p_hi_raw + theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank())
p_pr_raw <- p_pr_raw + theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank())
p_hi_numer <- p_hi_numer + theme(axis.title.x = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank())
p_pr_numer <- p_pr_numer + theme(axis.title.x = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank())

# For non-left panels (right column), remove y-axis elements.
p_pr_raw <- p_pr_raw +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(guide = "none")
p_pr_numer <- p_pr_numer +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(guide = "none")
p_pr_denom <- p_pr_denom +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(guide = "none")

# Combine the panels using patchwork into a 3-row by 2-column layout:
# Row 1: Raw plots; Row 2: Numerator plots; Row 3: Denom plots.
combined_plot <- (p_hi_raw + p_pr_raw) /
  (p_hi_numer + p_pr_numer) /
  (p_hi_denom + p_pr_denom) +
  plot_annotation(tag_levels = "a")

# Define file paths using here
png_path <- here("figures", "s02_sse_numerator_denominator.png")
svg_path <- here("figures", "s02_sse_numerator_denominator.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = combined_plot, width = 9, height = 9, dpi = 300)
ggsave(filename = svg_path, plot = combined_plot, width = 9, height = 9, device = "svg")

# Define file paths (adjust as necessary)
heat_file <- here("data", "output", "02_covariance", "03_space_time_metric", 
                  "heat_index", "month", "heat_index_space_time_metric_optimal.txt")
precip_file <- here("data", "output", "02_covariance", "03_space_time_metric", 
                    "precipitation", "month", "precipitation_space_time_metric_optimal.txt")

stm_heat <- readr::read_csv(heat_file)
stm_precip <- readr::read_csv(precip_file)

# calculate 1st, 3rd quantiles of stm2
quantile(stm_heat$stm2,c(1/4,1/2,3/4))
quantile(stm_precip$stm2,c(1/4,1/2,3/4))

# calculate median of stm2
median(stm_heat$stm2)
median(stm_precip$stm2)

# calculate # of HI stm = 1, < 1
sum(stm_heat$stm2 == 1)
sum(stm_heat$stm2 < 1)
sum(stm_heat$stm2 > 1)
sum(stm_precip$stm2 < 1)

# 