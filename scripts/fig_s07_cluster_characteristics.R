#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 7 panel figures of noaa events, intensity, duration, exposed area, impacts

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s07_cluster_characteristics.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# -----------------------
#  Load required packages
# -----------------------
library(tidyverse)
library(lubridate)
library(patchwork)
library(here)  # for reproducible file paths

# -------------------------------------------------------------
#  Helper function to read one file and compute cluster metrics
# -------------------------------------------------------------
process_file <- function(filepath) {
  
  # 1) Extract the numeric grid resolution from the file name (e.g. 0.25)
  grid_size <- str_extract(filepath, "(?<=_)[0-9.]+(?=_)") %>% as.numeric()
  
  # 2) Read in the CSV file
  df <- read_csv(filepath)
  
  # 3) Convert the date column to Date format explicitly using as.Date()
  #    Assuming the format is m/d/Y (e.g. "8/28/1941")
  df <- df %>% mutate(date = as.Date(date, format = "%m/%d/%Y"))
  
  # 4) Exclude rows where cluster equals 0
  df <- df %>% filter(cluster != 0)
  
  # ----------------------
  #   Compute cluster-level statistics
  # ----------------------
  
  # a) Duration: Difference in days between the maximum and minimum date in each cluster.
  df_cl <- df %>%
    group_by(cluster) %>%
    summarize(
      earliest_date = min(date, na.rm = TRUE),
      latest_date   = max(date, na.rm = TRUE),
      duration      = as.numeric(difftime(latest_date, earliest_date, units = "days")),
      .groups       = "drop"
    )
  
  # b) Daily spatial extent:
  #    For each cluster and date, count unique lat-long pairs.
  #    Then, for each cluster, pick the maximum count among all dates.
  df_daily <- df %>%
    group_by(cluster, date) %>%
    summarize(daily_count = n_distinct(paste0(long, "_", lat)), .groups = "drop") %>%
    group_by(cluster) %>%
    summarize(max_daily_count = max(daily_count, na.rm = TRUE), .groups = "drop")
  
  # c) Total spatial extent:
  #    For each cluster, count the unique lat-long pairs over all dates.
  df_total <- df %>%
    group_by(cluster) %>%
    summarize(total_count = n_distinct(paste0(long, "_", lat)), .groups = "drop")
  
  # d) Merge the statistics and compute the extents.
  #    For the bottom plots, we now calculate the extents as:
  #       number of unique points × (grid_size)^2.
  out <- df_cl %>%
    left_join(df_daily, by = "cluster") %>%
    left_join(df_total, by = "cluster") %>%
    mutate(
      # For plots 2 & 3: use squared grid resolution.
      daily_extent = max_daily_count * (grid_size^2),
      total_extent = total_count   * (grid_size^2)
    ) %>%
    select(cluster, duration, daily_extent, total_extent)
  
  # e) Add a column for the grid size (as a factor) for legend labeling.
  out <- out %>% mutate(grid = factor(grid_size))
  
  return(out)
}

# ------------------------------------------------------------------
#  Main function that processes three files and creates a 3-panel plot with overlayed histograms
#  and vertical lines for the mean and median of each dataset.
# ------------------------------------------------------------------
my_function <- function(file1, file2, file3, base_filename = "three_panel_plot") {
  
  # 1) Process each file individually to compute cluster metrics.
  df1 <- process_file(file1)
  df2 <- process_file(file2)
  df3 <- process_file(file3)
  
  # 2) Combine all datasets.
  combined_df <- bind_rows(df1, df2, df3)
  
  # 3) Compute global x-axis limits for each variable across all datasets.
  global_duration_range    <- c(min(combined_df$duration, na.rm = TRUE),    max(combined_df$duration, na.rm = TRUE))
  global_daily_extent_range <- c(min(combined_df$daily_extent, na.rm = TRUE), max(combined_df$daily_extent, na.rm = TRUE))
  global_total_extent_range <- c(min(combined_df$total_extent, na.rm = TRUE), max(combined_df$total_extent, na.rm = TRUE))
  
  # 4) Compute summary statistics (mean and median) for each group and variable.
  duration_stats <- combined_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(duration, na.rm = TRUE),
              median_val = median(duration, na.rm = TRUE), .groups = "drop")
  
  daily_extent_stats <- combined_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(daily_extent, na.rm = TRUE),
              median_val = median(daily_extent, na.rm = TRUE), .groups = "drop")
  
  total_extent_stats <- combined_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(total_extent, na.rm = TRUE),
              median_val = median(total_extent, na.rm = TRUE), .groups = "drop")
  
  # 5) Create histograms for each variable with an overlay for each dataset.
  #    We use an alpha value so that the overlays are transparent.
  p1 <- ggplot(combined_df, aes(x = duration, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = duration_stats, aes(xintercept = mean_val, color = grid),
               linetype = "dashed", size = 1) +
    geom_vline(data = duration_stats, aes(xintercept = median_val, color = grid),
               linetype = "dotted", size = 1) +
    scale_x_continuous(limits = global_duration_range) +
    theme_minimal() +
    labs(title = "Distribution of Durations",
         x = "Duration (days)",
         y = "Count")
  
  p2 <- ggplot(combined_df, aes(x = daily_extent, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = daily_extent_stats, aes(xintercept = mean_val, color = grid),
               linetype = "dashed", size = 1) +
    geom_vline(data = daily_extent_stats, aes(xintercept = median_val, color = grid),
               linetype = "dotted", size = 1) +
    scale_x_continuous(limits = global_daily_extent_range) +
    theme_minimal() +
    labs(title = "Distribution of Daily Extents",
         x = "Daily Extent (degrees²)",
         y = "Count")
  
  p3 <- ggplot(combined_df, aes(x = total_extent, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = total_extent_stats, aes(xintercept = mean_val, color = grid),
               linetype = "dashed", size = 1) +
    geom_vline(data = total_extent_stats, aes(xintercept = median_val, color = grid),
               linetype = "dotted", size = 1) +
    scale_x_continuous(limits = global_total_extent_range) +
    theme_minimal() +
    labs(title = "Distribution of Total Extents",
         x = "Total Extent (degrees²)",
         y = "Count")
  
  # 6) Combine the three plots vertically into one figure.
  final_plot <- p1 / p2 / p3
  
  # 7) Save the combined plot in PNG, SVG, and PDF formats using the here package.
  png_path <- here("figures", paste0(base_filename, ".png"))
  svg_path <- here("figures", paste0(base_filename, ".svg"))
  pdf_path <- here("figures", paste0(base_filename, ".pdf"))
  
  ggsave(filename = png_path, plot = final_plot, width = 10, height = 14, dpi = 300)
  ggsave(filename = svg_path, plot = final_plot, width = 10, height = 14, device = "svg")
  ggsave(filename = pdf_path, plot = final_plot, width = 10, height = 14)
  
  # 8) Return the combined metrics data frame invisibly (optional).
  return(invisible(combined_df))
}

# Example usage (update file paths as needed):
# my_function("data/heat_index_0.25_clustered_extremes.csv",
#             "data/heat_index_0.50_clustered_extremes.csv",
#             "data/heat_index_1.00_clustered_extremes.csv")



# Example usage (update the file paths as needed):
my_function(here::here("data", "output", "03_cluster", "02_cluster", "heat_index_0.25_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster", "heat_index_0.3075_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster", "heat_index_0.39_clustered_extremes.csv"))
