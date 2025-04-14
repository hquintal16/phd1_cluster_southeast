#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 7 panel figures of noaa events, intensity, duration, exposed area, impacts

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s07_cluster_characteristics.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Spatiotemporal extent ----
process_file <- function(filepath) {
  # Extract the numeric grid resolution from the file name (e.g. 0.25)
  grid_size <- str_extract(filepath, "(?<=_)[0-9.]+(?=_)") %>% as.numeric()
  
  # Conversion constant: kilometers per degree (approximate)
  km_per_deg <- 111.32
  
  # Read in the CSV file
  df <- read_csv(filepath)
  
  # Convert the date column to Date format explicitly using as.Date()
  df <- df %>% mutate(date = as.Date(date, format = "%m/%d/%Y"))
  
  # Exclude rows where cluster equals 0
  df <- df %>% filter(cluster != 0)
  
  # a) Duration: Difference in days between the minimum and maximum date per cluster.
  #    (Then add 1 to the computed duration.)
  df_cl <- df %>%
    group_by(cluster) %>%
    summarize(
      earliest_date = min(date, na.rm = TRUE),
      latest_date   = max(date, na.rm = TRUE),
      duration      = as.numeric(difftime(latest_date, earliest_date, units = "days")),
      .groups       = "drop"
    ) %>%
    mutate(duration = duration + 1)  # Add 1 to duration
  
  # b) Daily spatial extent: For each cluster and date, count unique lat-long pairs,
  #    then take the maximum daily count.
  df_daily <- df %>%
    group_by(cluster, date) %>%
    summarize(daily_count = n_distinct(paste0(long, "_", lat)), .groups = "drop") %>%
    group_by(cluster) %>%
    summarize(max_daily_count = max(daily_count, na.rm = TRUE), .groups = "drop")
  
  # c) Total spatial extent: For each cluster, count unique lat-long pairs over all dates.
  df_total <- df %>%
    group_by(cluster) %>%
    summarize(total_count = n_distinct(paste0(long, "_", lat)), .groups = "drop")
  
  # d) Merge statistics and compute extents in km².
  out <- df_cl %>%
    left_join(df_daily, by = "cluster") %>%
    left_join(df_total, by = "cluster") %>%
    mutate(
      daily_extent = max_daily_count * (grid_size * km_per_deg)^2,
      total_extent = total_count   * (grid_size * km_per_deg)^2
    ) %>%
    select(cluster, duration, daily_extent, total_extent) %>%
    mutate(grid = factor(grid_size))  # For legend labeling
  
  return(out)
}

my_function <- function(file1, file2, file3, file4, file5, file6, base_filename = "six_panel_plot") {
  
  ## Process the files:
  # Left column: files 1 to 3.
  left_df <- bind_rows(process_file(file1), process_file(file2), process_file(file3))
  # Right column: files 4 to 6.
  right_df <- bind_rows(process_file(file4), process_file(file5), process_file(file6))
  
  # Combine both sets for global x-axis limits
  combined_df_all <- bind_rows(left_df, right_df)
  
  ## Compute global x-axis limits for each variable:
  global_duration_range    <- c(min(combined_df_all$duration, na.rm = TRUE),    max(combined_df_all$duration, na.rm = TRUE))
  global_daily_extent_range <- c(min(combined_df_all$daily_extent, na.rm = TRUE), max(combined_df_all$daily_extent, na.rm = TRUE))
  global_total_extent_range <- c(min(combined_df_all$total_extent, na.rm = TRUE), max(combined_df_all$total_extent, na.rm = TRUE))
  
  # Adjust lower limits if necessary for log scales (daily and total extents)
  if (global_daily_extent_range[1] <= 0) {
    pos_daily <- combined_df_all$daily_extent[combined_df_all$daily_extent > 0]
    global_daily_extent_range[1] <- if(length(pos_daily) > 0) min(pos_daily, na.rm = TRUE) else 1
  }
  if (global_total_extent_range[1] <= 0) {
    pos_total <- combined_df_all$total_extent[combined_df_all$total_extent > 0]
    global_total_extent_range[1] <- if(length(pos_total) > 0) min(pos_total, na.rm = TRUE) else 1
  }
  
  ## Compute summary statistics for each column (left/right) and each variable:
  # Left column stats:
  left_duration_stats <- left_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(duration, na.rm = TRUE),
              median_val = median(duration, na.rm = TRUE), .groups = "drop")
  left_daily_stats <- left_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(daily_extent, na.rm = TRUE),
              median_val = median(daily_extent, na.rm = TRUE), .groups = "drop")
  left_total_stats <- left_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(total_extent, na.rm = TRUE),
              median_val = median(total_extent, na.rm = TRUE), .groups = "drop")
  
  # Right column stats:
  right_duration_stats <- right_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(duration, na.rm = TRUE),
              median_val = median(duration, na.rm = TRUE), .groups = "drop")
  right_daily_stats <- right_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(daily_extent, na.rm = TRUE),
              median_val = median(daily_extent, na.rm = TRUE), .groups = "drop")
  right_total_stats <- right_df %>%
    group_by(grid) %>%
    summarize(mean_val = mean(total_extent, na.rm = TRUE),
              median_val = median(total_extent, na.rm = TRUE), .groups = "drop")
  
  common_theme <- theme_bw() +
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )
  
  # Define scales for the resolution legend for both fill and color.
  resolution_scales <- list(
    scale_fill_discrete(name = "Resolution (°/day)"),
    scale_color_discrete(name = "Resolution (°/day)")
  )
  
  # Define scale for the linetype (for dashed and dotted lines), with legend title "Statistic".
  linetype_scale <- scale_linetype_manual(
    values = c("Mean" = "dashed", "Median" = "dotted"),
    name = "Statistic"
  )
  
  # Top row: Duration histograms (non-log scale)
  p1_left <- ggplot(left_df, aes(x = duration, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = left_duration_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
    geom_vline(data = left_duration_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
    scale_x_continuous(limits = global_duration_range) +
    common_theme +
    resolution_scales +
    linetype_scale +
    labs(x = "Duration (days)", y = "Count")
  
  p1_right <- ggplot(right_df, aes(x = duration, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = right_duration_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
    geom_vline(data = right_duration_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
    scale_x_continuous(limits = global_duration_range) +
    common_theme +
    resolution_scales +
    linetype_scale +
    labs(x = "Duration (days)", y = "Count")
  
  # Middle row: Daily Extent histograms (log scale)
  p2_left <- ggplot(left_df, aes(x = daily_extent, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = left_daily_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
    geom_vline(data = left_daily_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
    scale_x_log10(limits = global_daily_extent_range) +
    common_theme +
    resolution_scales +
    linetype_scale +
    labs(x = "Daily Extent (km²)", y = "Count")
  
  p2_right <- ggplot(right_df, aes(x = daily_extent, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = right_daily_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
    geom_vline(data = right_daily_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
    scale_x_log10(limits = global_daily_extent_range) +
    common_theme +
    resolution_scales +
    linetype_scale +
    labs(x = "Daily Extent (km²)", y = "Count")
  
  # Bottom row: Total Extent histograms (log scale)
  p3_left <- ggplot(left_df, aes(x = total_extent, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = left_total_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
    geom_vline(data = left_total_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
    scale_x_log10(limits = global_total_extent_range) +
    common_theme +
    resolution_scales +
    linetype_scale +
    labs(x = "Total Extent (km²)", y = "Count")
  
  p3_right <- ggplot(right_df, aes(x = total_extent, fill = grid)) +
    geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
    geom_vline(data = right_total_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
    geom_vline(data = right_total_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
    scale_x_log10(limits = global_total_extent_range) +
    common_theme +
    resolution_scales +
    linetype_scale +
    labs(x = "Total Extent (km²)", y = "Count")
  
  # Create dummy (empty) plots to hold the column titles.
  left_col_title <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "Heat Advisory", size = 6, fontface = "bold") +
    theme(plot.margin = margin(b = 10))
  
  right_col_title <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "Heat Warning", size = 6, fontface = "bold") +
    theme(plot.margin = margin(b = 10))
  
  col_titles <- left_col_title | right_col_title
  
  # Combine the six panels in a 3-row x 2-column grid.
  panel_grid <- (p1_left | p1_right) /
    (p2_left | p2_right) /
    (p3_left | p3_right)
  
  # Place the column titles above the panels.
  final_plot <- col_titles / panel_grid + 
    plot_layout(heights = c(0.1, 1))  # adjust height ratio as needed
  
  png_path <- here("figures", paste0(base_filename, ".png"))
  svg_path <- here("figures", paste0(base_filename, ".svg"))
  
  ggsave(filename = png_path, plot = final_plot, width = 12, height = 12, dpi = 300)
  ggsave(filename = svg_path, plot = final_plot, width = 12, height = 12, device = "svg")
  
  # Return the combined data invisibly (optional)
  return(invisible(combined_df_all))
}

my_function(here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.25_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.3075_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.39_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.25_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.3075_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.39_clustered_extremes.csv"))

# Seasonality ----

# 1. Helper Function: process_cluster_dates
# This function reads a file and uses the specified date column (e.g., "start_date" or "end_date")
# to compute the Day of Year (DOY) and assign a decade (with an "All" group).
process_cluster_dates <- function(filepath, heat, date_col = "start_date") {
  # Extract the grid resolution from the file name (assumes the grid value is between underscores)
  grid_size <- str_extract(filepath, "(?<=_)[0-9.]+(?=_)") %>% as.numeric()
  
  # Read in the CSV file and convert the chosen date column to Date.
  df <- read_csv(filepath) %>%
    mutate(
      date = as.Date(.data[[date_col]], format = "%m/%d/%Y"),
      DOY = as.numeric(format(date, "%j")),
      year = as.numeric(format(date, "%Y")),
      grid = as.character(grid_size),
      heat = heat
    )
  
  # Create a 'decade' variable based on the year; includes decades from 1940 to 2029.
  df <- df %>%
    mutate(decade = case_when(
      year >= 1940 & year <= 1949 ~ "1940-1949",
      year >= 1950 & year <= 1959 ~ "1950-1959",
      year >= 1960 & year <= 1969 ~ "1960-1969",
      year >= 1970 & year <= 1979 ~ "1970-1979",
      year >= 1980 & year <= 1989 ~ "1980-1989",
      year >= 1990 & year <= 1999 ~ "1990-1999",
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2029 ~ "2020-2029",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(decade))
  
  # Duplicate the data so that all rows are also assigned to the "All" group.
  df_all <- df %>% mutate(decade = "All")
  df <- bind_rows(df, df_all)
  
  # Set the factor levels for decade in the desired order.
  df$decade <- factor(df$decade, levels = c("1940-1949", "1950-1959", "1960-1969",
                                            "1970-1979", "1980-1989", "1990-1999",
                                            "2000-2009", "2010-2019", "2020-2029", "All"))
  return(df)
}

# 2. Main Function: my_overlay_violin_function (Grouped Version)
# This function creates grouped horizontal violin plots.
# For each panel:
#   - The x-axis represents the decade (with violins for each decade arranged side-by-side).
#   - The y-axis represents the DOY.
#   - Fill and color identify the grid resolution (0.25, 0.3075, 0.39).
# The final output is a 2 x 2 layout:
#  - Top row: Start of Cluster DOY distributions.
#  - Bottom row: End of Cluster DOY distributions.
#  - Left column: Heat Advisory; Right column: Heat Warning.
# A single shared legend is placed at the bottom of the entire figure.
my_overlay_violin_function <- function(file1, file2, file3, file4, file5, file6, 
                                       base_filename = "grouped_violin_plot") {
  # Process files for start_date:
  start_advisory <- bind_rows(
    process_cluster_dates(file1, "Heat Advisory", date_col = "start_date"),
    process_cluster_dates(file2, "Heat Advisory", date_col = "start_date"),
    process_cluster_dates(file3, "Heat Advisory", date_col = "start_date")
  )
  start_warning <- bind_rows(
    process_cluster_dates(file4, "Heat Warning", date_col = "start_date"),
    process_cluster_dates(file5, "Heat Warning", date_col = "start_date"),
    process_cluster_dates(file6, "Heat Warning", date_col = "start_date")
  )
  
  # Process files for end_date:
  end_advisory <- bind_rows(
    process_cluster_dates(file1, "Heat Advisory", date_col = "end_date"),
    process_cluster_dates(file2, "Heat Advisory", date_col = "end_date"),
    process_cluster_dates(file3, "Heat Advisory", date_col = "end_date")
  )
  end_warning <- bind_rows(
    process_cluster_dates(file4, "Heat Warning", date_col = "end_date"),
    process_cluster_dates(file5, "Heat Warning", date_col = "end_date"),
    process_cluster_dates(file6, "Heat Warning", date_col = "end_date")
  )
  
  # Define a common theme (same for all plots)
  common_theme <- theme_bw() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )
  
  # In these grouped plots, fill and color represent grid resolution.
  grid_scales <- list(
    scale_fill_discrete(name = "Resolution (°/day)"),
    scale_color_discrete(name = "Resolution (°/day)")
  )
  
  # Create break values and labels for the DOY axis using a representative non-leap year (2021).
  month_breaks <- as.numeric(format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")), "%j"))
  month_labels <- format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")), "%b %d")
  
  # --- Start Date Panels ---
  # Map x = decade (categorical) and y = DOY (continuous). Fill/color by grid.
  # Use position_dodge() to separate violins within each decade.
  # Then flip the coordinates so that the decades appear along the vertical.
  p_start_advisory <- ggplot(start_advisory, aes(x = decade, y = DOY, fill = grid, color = grid)) +
    geom_violin(position = position_dodge(width = 0.9), trim = FALSE, alpha = 0.5) +
    scale_y_continuous(breaks = month_breaks, labels = month_labels) +
    labs(x = "Decade", y = "Start of Cluster") +
    grid_scales +
    common_theme +
    coord_flip()
  
  p_start_warning <- ggplot(start_warning, aes(x = decade, y = DOY, fill = grid, color = grid)) +
    geom_violin(position = position_dodge(width = 0.9), trim = FALSE, alpha = 0.5) +
    scale_y_continuous(breaks = month_breaks, labels = month_labels) +
    labs(x = "Decade", y = "Start of Cluster") +
    grid_scales +
    common_theme +
    coord_flip()
  
  # --- End Date Panels ---
  p_end_advisory <- ggplot(end_advisory, aes(x = decade, y = DOY, fill = grid, color = grid)) +
    geom_violin(position = position_dodge(width = 0.9), trim = FALSE, alpha = 0.5) +
    scale_y_continuous(breaks = month_breaks, labels = month_labels) +
    labs(x = "Decade", y = "End of Cluster") +
    grid_scales +
    common_theme +
    coord_flip()
  
  p_end_warning <- ggplot(end_warning, aes(x = decade, y = DOY, fill = grid, color = grid)) +
    geom_violin(position = position_dodge(width = 0.9), trim = FALSE, alpha = 0.5) +
    scale_y_continuous(breaks = month_breaks, labels = month_labels) +
    labs(x = "Decade", y = "End of Cluster") +
    grid_scales +
    common_theme +
    coord_flip()
  
  # Combine the four panels in a 2 row x 2 column layout and collect the legends.
  # Then remove individual legends by collecting them into one single legend below the plot.
  final_plot <- (p_start_advisory | p_start_warning) / (p_end_advisory | p_end_warning) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Save the final plot in PNG and SVG formats:
  png_path <- here("figures", paste0(base_filename, ".png"))
  svg_path <- here("figures", paste0(base_filename, ".svg"))
  ggsave(filename = png_path, plot = final_plot, width = 14, height = 8, dpi = 300)
  ggsave(filename = svg_path, plot = final_plot, width = 14, height = 8, device = "svg")
  
  # Optionally, return the processed datasets as a list.
  return(invisible(list(
    start_advisory = start_advisory,
    start_warning = start_warning,
    end_advisory = end_advisory,
    end_warning = end_warning
  )))
}


my_overlay_violin_function(here::here("data", "output", "05_validation", "summary","advisory","cluster", "cluster_0.25_excess_heat_summary.csv"),
                           here::here("data", "output", "05_validation", "summary","advisory","cluster", "cluster_0.3075_excess_heat_summary.csv"),
                           here::here("data", "output", "05_validation", "summary","advisory","cluster", "cluster_0.39_excess_heat_summary.csv"),
                           here::here("data", "output", "05_validation", "summary","warning","cluster", "cluster_0.25_excess_heat_summary.csv"),
                           here::here("data", "output", "05_validation", "summary","warning","cluster", "cluster_0.3075_excess_heat_summary.csv"),
                           here::here("data", "output", "05_validation", "summary","warning","cluster", "cluster_0.39_excess_heat_summary.csv"))
