#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 7 panel figures of noaa events, intensity, duration, exposed area, impacts

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s07_cluster_characteristics.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# process_file <- function(filepath) {
#   
#   # 1) Extract the numeric grid resolution from the file name (e.g. 0.25)
#   grid_size <- str_extract(filepath, "(?<=_)[0-9.]+(?=_)") %>% as.numeric()
#   
#   # Conversion constant: kilometers per degree (approximate)
#   km_per_deg <- 111.32
#   
#   # 2) Read in the CSV file
#   df <- read_csv(filepath)
#   
#   # 3) Convert the date column to Date format explicitly using as.Date()
#   #    Assuming the format is m/d/Y (e.g. "8/28/1941")
#   df <- df %>% mutate(date = as.Date(date, format = "%m/%d/%Y"))
#   
#   # 4) Exclude rows where cluster equals 0
#   df <- df %>% filter(cluster != 0)
#   
#   # a) Duration: difference in days between min and max date per cluster
#   df_cl <- df %>%
#     group_by(cluster) %>%
#     summarize(
#       earliest_date = min(date, na.rm = TRUE),
#       latest_date   = max(date, na.rm = TRUE),
#       duration      = as.numeric(difftime(latest_date, earliest_date, units = "days")),
#       .groups       = "drop"
#     )
#   
#   # b) Daily spatial extent: For each cluster and date, count unique lat-long pairs,
#   #    then take the maximum daily count.
#   df_daily <- df %>%
#     group_by(cluster, date) %>%
#     summarize(daily_count = n_distinct(paste0(long, "_", lat)), .groups = "drop") %>%
#     group_by(cluster) %>%
#     summarize(max_daily_count = max(daily_count, na.rm = TRUE), .groups = "drop")
#   
#   # c) Total spatial extent: For each cluster, count unique lat-long pairs over all dates.
#   df_total <- df %>%
#     group_by(cluster) %>%
#     summarize(total_count = n_distinct(paste0(long, "_", lat)), .groups = "drop")
#   
#   # d) Merge statistics and compute extents in km²:
#   #    area per point = (grid_size * km_per_deg)².
#   out <- df_cl %>%
#     left_join(df_daily, by = "cluster") %>%
#     left_join(df_total, by = "cluster") %>%
#     mutate(
#       daily_extent = max_daily_count * (grid_size * km_per_deg)^2,
#       total_extent = total_count   * (grid_size * km_per_deg)^2
#     ) %>%
#     select(cluster, duration, daily_extent, total_extent)
#   
#   # e) Add grid size as a factor for legend labeling
#   out <- out %>% mutate(grid = factor(grid_size))
#   
#   return(out)
# }
# 
# my_function <- function(file1, file2, file3, file4, file5, file6, base_filename = "six_panel_plot") {
#   
#   ## 1) Process the files:
#   # Left column: files 1 to 3.
#   left_df <- bind_rows(process_file(file1),
#                        process_file(file2),
#                        process_file(file3))
#   
#   # Right column: files 4 to 6.
#   right_df <- bind_rows(process_file(file4),
#                         process_file(file5),
#                         process_file(file6))
#   
#   # Combine both sets to compute global limits for consistency across panels.
#   combined_df_all <- bind_rows(left_df, right_df)
#   
#   ## 2) Compute global x-axis limits for each variable:
#   global_duration_range    <- c(min(combined_df_all$duration, na.rm = TRUE),    max(combined_df_all$duration, na.rm = TRUE))
#   global_daily_extent_range <- c(min(combined_df_all$daily_extent, na.rm = TRUE), max(combined_df_all$daily_extent, na.rm = TRUE))
#   global_total_extent_range <- c(min(combined_df_all$total_extent, na.rm = TRUE), max(combined_df_all$total_extent, na.rm = TRUE))
#   
#   # Ensure lower limits are > 0 (required for log transformation)
#   if (global_duration_range[1] <= 0) {
#     positive_duration <- combined_df_all$duration[combined_df_all$duration > 0]
#     global_duration_range[1] <- if (length(positive_duration) > 0) min(positive_duration, na.rm = TRUE) else 1
#   }
#   if (global_daily_extent_range[1] <= 0) {
#     positive_daily <- combined_df_all$daily_extent[combined_df_all$daily_extent > 0]
#     global_daily_extent_range[1] <- if (length(positive_daily) > 0) min(positive_daily, na.rm = TRUE) else 1
#   }
#   if (global_total_extent_range[1] <= 0) {
#     positive_total <- combined_df_all$total_extent[combined_df_all$total_extent > 0]
#     global_total_extent_range[1] <- if (length(positive_total) > 0) min(positive_total, na.rm = TRUE) else 1
#   }
#   
#   ## 3) Compute summary statistics separately for left and right columns:
#   # Left column statistics:
#   left_duration_stats <- left_df %>%
#     group_by(grid) %>%
#     summarize(mean_val = mean(duration, na.rm = TRUE),
#               median_val = median(duration, na.rm = TRUE), .groups = "drop")
#   left_daily_extent_stats <- left_df %>%
#     group_by(grid) %>%
#     summarize(mean_val = mean(daily_extent, na.rm = TRUE),
#               median_val = median(daily_extent, na.rm = TRUE), .groups = "drop")
#   left_total_extent_stats <- left_df %>%
#     group_by(grid) %>%
#     summarize(mean_val = mean(total_extent, na.rm = TRUE),
#               median_val = median(total_extent, na.rm = TRUE), .groups = "drop")
#   
#   # Right column statistics:
#   right_duration_stats <- right_df %>%
#     group_by(grid) %>%
#     summarize(mean_val = mean(duration, na.rm = TRUE),
#               median_val = median(duration, na.rm = TRUE), .groups = "drop")
#   right_daily_extent_stats <- right_df %>%
#     group_by(grid) %>%
#     summarize(mean_val = mean(daily_extent, na.rm = TRUE),
#               median_val = median(daily_extent, na.rm = TRUE), .groups = "drop")
#   right_total_extent_stats <- right_df %>%
#     group_by(grid) %>%
#     summarize(mean_val = mean(total_extent, na.rm = TRUE),
#               median_val = median(total_extent, na.rm = TRUE), .groups = "drop")
#   
#   ## 4) Create histograms for each variable with updated themes and legends.
#   # Each plot now:
#   #   - Uses theme_bw() with a thin black panel border.
#   #   - Maps linetype in the vline layers so that the "Mean" (dashed) and "Median" (dotted) lines appear in the legend.
#   #   - Renames the color/fill legend to "Resolution (°/day)".
#   #   - Places legends inside the panel (top right).
#   # Also, the top left and top right panels are given the titles "Heat Advisory" and "Heat Warning", respectively.
#   
#   # Left column plots:
#   p1_left <- ggplot(left_df, aes(x = duration, fill = grid)) +
#     geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
#     geom_vline(data = left_duration_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
#     geom_vline(data = left_duration_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
#     scale_x_log10(limits = global_duration_range) +
#     scale_color_discrete(name = "Resolution (°/day)") +
#     scale_fill_discrete(name = "Resolution (°/day)") +
#     scale_linetype_manual(name = "Statistic", values = c("Mean" = "dashed", "Median" = "dotted")) +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#           legend.position = c(0.95, 0.95),
#           legend.justification = c("right", "top")) +
#     labs(x = "Duration (days)", y = "Count") +
#     ggtitle("Heat Advisory")
#   
#   p2_left <- ggplot(left_df, aes(x = daily_extent, fill = grid)) +
#     geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
#     geom_vline(data = left_daily_extent_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
#     geom_vline(data = left_daily_extent_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
#     scale_x_log10(limits = global_daily_extent_range) +
#     scale_color_discrete(name = "Resolution (°/day)") +
#     scale_fill_discrete(name = "Resolution (°/day)") +
#     scale_linetype_manual(name = "Statistic", values = c("Mean" = "dashed", "Median" = "dotted")) +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#           legend.position = c(0.95, 0.95),
#           legend.justification = c("right", "top")) +
#     labs(x = "Daily Extent (km²)", y = "Count")
#   
#   p3_left <- ggplot(left_df, aes(x = total_extent, fill = grid)) +
#     geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
#     geom_vline(data = left_total_extent_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
#     geom_vline(data = left_total_extent_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
#     scale_x_log10(limits = global_total_extent_range) +
#     scale_color_discrete(name = "Resolution (°/day)") +
#     scale_fill_discrete(name = "Resolution (°/day)") +
#     scale_linetype_manual(name = "Statistic", values = c("Mean" = "dashed", "Median" = "dotted")) +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#           legend.position = c(0.95, 0.95),
#           legend.justification = c("right", "top")) +
#     labs(x = "Total Extent (km²)", y = "Count")
#   
#   # Right column plots:
#   p1_right <- ggplot(right_df, aes(x = duration, fill = grid)) +
#     geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
#     geom_vline(data = right_duration_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
#     geom_vline(data = right_duration_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
#     scale_x_log10(limits = global_duration_range) +
#     scale_color_discrete(name = "Resolution (°/day)") +
#     scale_fill_discrete(name = "Resolution (°/day)") +
#     scale_linetype_manual(name = "Statistic", values = c("Mean" = "dashed", "Median" = "dotted")) +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#           legend.position = c(0.95, 0.95),
#           legend.justification = c("right", "top")) +
#     labs(x = "Duration (days)", y = "Count") +
#     ggtitle("Heat Warning")
#   
#   p2_right <- ggplot(right_df, aes(x = daily_extent, fill = grid)) +
#     geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
#     geom_vline(data = right_daily_extent_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
#     geom_vline(data = right_daily_extent_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
#     scale_x_log10(limits = global_daily_extent_range) +
#     scale_color_discrete(name = "Resolution (°/day)") +
#     scale_fill_discrete(name = "Resolution (°/day)") +
#     scale_linetype_manual(name = "Statistic", values = c("Mean" = "dashed", "Median" = "dotted")) +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#           legend.position = c(0.95, 0.95),
#           legend.justification = c("right", "top")) +
#     labs(x = "Daily Extent (km²)", y = "Count")
#   
#   p3_right <- ggplot(right_df, aes(x = total_extent, fill = grid)) +
#     geom_histogram(position = "identity", bins = 30, alpha = 0.4) +
#     geom_vline(data = right_total_extent_stats, aes(xintercept = mean_val, color = grid, linetype = "Mean"), size = 1) +
#     geom_vline(data = right_total_extent_stats, aes(xintercept = median_val, color = grid, linetype = "Median"), size = 1) +
#     scale_x_log10(limits = global_total_extent_range) +
#     scale_color_discrete(name = "Resolution (°/day)") +
#     scale_fill_discrete(name = "Resolution (°/day)") +
#     scale_linetype_manual(name = "Statistic", values = c("Mean" = "dashed", "Median" = "dotted")) +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#           legend.position = c(0.95, 0.95),
#           legend.justification = c("right", "top")) +
#     labs(x = "Total Extent (km²)", y = "Count")
#   
#   ## 5) Combine the six plots into a 3-row x 2-column figure using patchwork.
#   final_plot <- (p1_left | p1_right) /
#     (p2_left | p2_right) /
#     (p3_left | p3_right)
#   
#   ## 6) Save the combined plot in PNG and SVG formats:
#   png_path <- here("figures", paste0(base_filename, ".png"))
#   svg_path <- here("figures", paste0(base_filename, ".svg"))
#   
#   ggsave(filename = png_path, plot = final_plot, width = 12, height = 18, dpi = 300)
#   ggsave(filename = svg_path, plot = final_plot, width = 12, height = 18, device = "svg")
#   
#   # Return the combined metrics data frame invisibly (optional)
#   return(invisible(combined_df_all))
# }

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
  df_cl <- df %>%
    group_by(cluster) %>%
    summarize(
      earliest_date = min(date, na.rm = TRUE),
      latest_date   = max(date, na.rm = TRUE),
      duration      = as.numeric(difftime(latest_date, earliest_date, units = "days")),
      .groups       = "drop"
    )
  
  # b) Daily spatial extent: maximum unique lat-long count on any date.
  df_daily <- df %>%
    group_by(cluster, date) %>%
    summarize(daily_count = n_distinct(paste0(long, "_", lat)), .groups = "drop") %>%
    group_by(cluster) %>%
    summarize(max_daily_count = max(daily_count, na.rm = TRUE), .groups = "drop")
  
  # c) Total spatial extent: unique lat-long count over all dates.
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
  
  # Adjust lower limits if necessary (for log scale; not needed for duration but required for extents)
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
  
  # Define common theme additions:
  common_theme <- theme_bw() +
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )
  
  # Define scale for the resolution legend for both fill and color.
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
  
  ggsave(filename = png_path, plot = final_plot, width = 12, height = 14, dpi = 300)
  ggsave(filename = svg_path, plot = final_plot, width = 12, height = 14, device = "svg")
  
  # Return the combined data invisibly (optional)
  return(invisible(combined_df_all))
}

# Heat Index Advisory ----
my_function(here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.25_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.3075_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.39_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.25_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.3075_clustered_extremes.csv"),
            here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.39_clustered_extremes.csv"))
