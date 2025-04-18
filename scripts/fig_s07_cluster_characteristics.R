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
## Decade ----
process_cluster_dates <- function(filepath, heat, date_col = "start_date") {
  grid_size <- str_extract(filepath, "(?<=_)[0-9.]+(?=_)") %>% as.numeric()
  df <- read_csv(filepath) %>%
    mutate(
      date = as.Date(.data[[date_col]], format = "%m/%d/%Y"),
      DOY = as.numeric(format(date, "%j")),
      year = as.numeric(format(date, "%Y")),
      grid = as.character(grid_size),
      heat = heat
    )
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
  # Do not create an "All" group in this version.
  df$decade <- factor(df$decade, levels = c("1940-1949", "1950-1959", "1960-1969",
                                            "1970-1979", "1980-1989", "1990-1999",
                                            "2000-2009", "2010-2019", "2020-2029"))
  return(df)
}

my_histogram_function_start <- function(file1, file2, file3, file4, file5, file6,
                                        base_filename = "histogram_start") {
  adv_df <- bind_rows(
    process_cluster_dates(file1, "Heat Advisory", date_col = "start_date"),
    process_cluster_dates(file2, "Heat Advisory", date_col = "start_date"),
    process_cluster_dates(file3, "Heat Advisory", date_col = "start_date")
  )
  warn_df <- bind_rows(
    process_cluster_dates(file4, "Heat Warning", date_col = "start_date"),
    process_cluster_dates(file5, "Heat Warning", date_col = "start_date"),
    process_cluster_dates(file6, "Heat Warning", date_col = "start_date")
  )
  all_df <- bind_rows(adv_df, warn_df)
  
  bins <- seq(0.5, 366.5, by = 1)
  hist_counts <- all_df %>%
    group_by(heat, decade, grid) %>%
    summarize(count = max(table(cut(DOY, breaks = bins))), .groups = "drop")
  global_y <- max(hist_counts$count, na.rm = TRUE)
  
  # Compute month breaks and labels (for a representative non-leap year; 2021)
  all_month_breaks <- as.numeric(format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")),
                                        "%j"))
  all_month_labels <- format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")),
                             "%b %d")
  # Subset to the range May 1 (DOY 121) to November 1 (DOY 305)
  month_breaks <- all_month_breaks[all_month_breaks >= 121 & all_month_breaks <= 305]
  month_labels <- all_month_labels[all_month_breaks >= 121 & all_month_breaks <= 305]
  
  p <- ggplot(all_df, aes(x = DOY, fill = grid)) +
    geom_histogram(breaks = bins, position = "identity", alpha = 0.5, binwidth = 1, color = NA) +
    scale_x_continuous(breaks = month_breaks, labels = month_labels, limits = c(121, 305)) +
    scale_y_continuous(limits = c(0, global_y * 1.1)) +
    labs(x = "Cluster Initiation", y = "Cluster Count") +
    facet_grid(rows = vars(decade), cols = vars(heat)) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  png_path <- here("figures", paste0(base_filename, ".png"))
  svg_path <- here("figures", paste0(base_filename, ".svg"))
  ggsave(filename = png_path, plot = p, width = 12, height = 10, dpi = 300)
  ggsave(filename = svg_path, plot = p, width = 12, height = 10, device = "svg")
  
  return(invisible(all_df))
}

my_histogram_function_end <- function(file1, file2, file3, file4, file5, file6,
                                      base_filename = "histogram_end") {
  adv_df <- bind_rows(
    process_cluster_dates(file1, "Heat Advisory", date_col = "end_date"),
    process_cluster_dates(file2, "Heat Advisory", date_col = "end_date"),
    process_cluster_dates(file3, "Heat Advisory", date_col = "end_date")
  )
  warn_df <- bind_rows(
    process_cluster_dates(file4, "Heat Warning", date_col = "end_date"),
    process_cluster_dates(file5, "Heat Warning", date_col = "end_date"),
    process_cluster_dates(file6, "Heat Warning", date_col = "end_date")
  )
  all_df <- bind_rows(adv_df, warn_df)
  
  bins <- seq(0.5, 366.5, by = 1)
  hist_counts <- all_df %>%
    group_by(heat, decade, grid) %>%
    summarize(count = max(table(cut(DOY, breaks = bins))), .groups = "drop")
  global_y <- max(hist_counts$count, na.rm = TRUE)
  
  all_month_breaks <- as.numeric(format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")),
                                        "%j"))
  all_month_labels <- format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")),
                             "%b %d")
  month_breaks <- all_month_breaks[all_month_breaks >= 121 & all_month_breaks <= 305]
  month_labels <- all_month_labels[all_month_breaks >= 121 & all_month_breaks <= 305]
  
  p <- ggplot(all_df, aes(x = DOY, fill = grid)) +
    geom_histogram(breaks = bins, position = "identity", alpha = 0.5, binwidth = 1, color = NA) +
    scale_x_continuous(breaks = month_breaks, labels = month_labels, limits = c(121, 305)) +
    scale_y_continuous(limits = c(0, global_y * 1.1)) +
    labs(x = "Cluster Termination", y = "Cluster Count") +
    facet_grid(rows = vars(decade), cols = vars(heat)) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  png_path <- here("figures", paste0(base_filename, ".png"))
  svg_path <- here("figures", paste0(base_filename, ".svg"))
  ggsave(filename = png_path, plot = p, width = 12, height = 10, dpi = 300)
  ggsave(filename = svg_path, plot = p, width = 12, height = 10, device = "svg")
  
  return(invisible(all_df))
}

my_histogram_function_start(
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.25_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.3075_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.39_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.25_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.3075_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.39_excess_heat_summary.csv")
)

my_histogram_function_end(
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.25_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.3075_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.39_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.25_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.3075_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.39_excess_heat_summary.csv")
)

## All ----

process_cluster_dates <- function(filepath, heat, date_col = "start_date") {
  grid_size <- str_extract(filepath, "(?<=_)[0-9.]+(?=_)") %>% as.numeric()
  df <- read_csv(filepath) %>%
    mutate(
      date = as.Date(.data[[date_col]], format = "%m/%d/%Y"),
      DOY  = as.numeric(format(date, "%j")),
      year = as.numeric(format(date, "%Y")),
      grid = as.character(grid_size),
      heat = heat
    )
  # Compute decade (for informational purposes only)
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
  df$decade <- factor(df$decade, levels = c("1940-1949", "1950-1959", "1960-1969",
                                            "1970-1979", "1980-1989", "1990-1999",
                                            "2000-2009", "2010-2019", "2020-2029"))
  return(df)
}

my_fourpanel_histogram_all_years_total <- function(file_adv1, file_adv2, file_adv3,
                                                   file_warn1, file_warn2, file_warn3,
                                                   base_filename = "fourpanel_histogram_all_years_total") {
  # Process cluster initiation (start_date)
  adv_start <- bind_rows(
    process_cluster_dates(file_adv1, "Heat Advisory", date_col = "start_date"),
    process_cluster_dates(file_adv2, "Heat Advisory", date_col = "start_date"),
    process_cluster_dates(file_adv3, "Heat Advisory", date_col = "start_date")
  )
  warn_start <- bind_rows(
    process_cluster_dates(file_warn1, "Heat Warning", date_col = "start_date"),
    process_cluster_dates(file_warn2, "Heat Warning", date_col = "start_date"),
    process_cluster_dates(file_warn3, "Heat Warning", date_col = "start_date")
  )
  
  # Process cluster termination (end_date)
  adv_end <- bind_rows(
    process_cluster_dates(file_adv1, "Heat Advisory", date_col = "end_date"),
    process_cluster_dates(file_adv2, "Heat Advisory", date_col = "end_date"),
    process_cluster_dates(file_adv3, "Heat Advisory", date_col = "end_date")
  )
  warn_end <- bind_rows(
    process_cluster_dates(file_warn1, "Heat Warning", date_col = "end_date"),
    process_cluster_dates(file_warn2, "Heat Warning", date_col = "end_date"),
    process_cluster_dates(file_warn3, "Heat Warning", date_col = "end_date")
  )
  
  # Add an event indicator to distinguish cluster initiation vs. termination
  adv_start <- adv_start %>% mutate(event = "Cluster Initiation")
  warn_start <- warn_start %>% mutate(event = "Cluster Initiation")
  adv_end   <- adv_end   %>% mutate(event = "Cluster Termination")
  warn_end  <- warn_end  %>% mutate(event = "Cluster Termination")
  
  # Combine all data
  data_all <- bind_rows(adv_start, warn_start, adv_end, warn_end)
  
  # Restrict the x-axis to May 1 (DOY 121) to November 1 (DOY 305)
  data_all <- data_all %>% filter(DOY >= 121, DOY <= 305)
  
  # We'll use the entire dataset per panel (no breakdown by decade)
  # Define histogram bins: 1-day bins from 120.5 to 305.5
  bins <- seq(120.5, 305.5, by = 1)
  
  # Compute cumulative counts per bin for each panel (group by heat and event only)
  cumulative_counts <- data_all %>%
    mutate(bin = cut(DOY, breaks = bins, include.lowest = TRUE)) %>%
    group_by(heat, event, bin) %>%
    summarize(n = n(), .groups = "drop")
  common_y_limit <- max(cumulative_counts$n, na.rm = TRUE)
  
  # Define x-axis breaks and labels for a representative year (2021) restricted to 121-305
  all_month_breaks <- as.numeric(format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")), "%j"))
  all_month_labels <- format(as.Date(paste0("2021-", sprintf("%02d", 1:12), "-01")), "%b %d")
  month_breaks <- all_month_breaks[all_month_breaks >= 121 & all_month_breaks <= 305]
  month_labels <- all_month_labels[all_month_breaks >= 121 & all_month_breaks <= 305]
  
  # Define a common theme with a shared legend at the bottom
  common_theme <- theme_bw() +
    theme(legend.position = "bottom")
  
  # Create the histogram plot using overlapping histograms with alpha = 0.5 and no outline
  p <- ggplot(data_all, aes(x = DOY, fill = grid)) +
    geom_histogram(breaks = bins, binwidth = 1, position = "identity", alpha = 0.5, color = NA) +
    scale_x_continuous(breaks = month_breaks, labels = month_labels, limits = c(121, 305)) +
    # scale_y_continuous(limits = c(0, common_y_limit * 1.1)) +
    scale_y_continuous(limits = c(0, 85)) +
    labs(x = "Date", y = "Cluster Count") +
    facet_grid(rows = vars(event), cols = vars(heat)) +
    common_theme
  
  # Save the figure
  png_path <- here("figures", paste0(base_filename, ".png"))
  svg_path <- here("figures", paste0(base_filename, ".svg"))
  ggsave(filename = png_path, plot = p, width = 12, height = 8, dpi = 300)
  ggsave(filename = svg_path, plot = p, width = 12, height = 8, device = "svg")
  
  return(invisible(data_all))
}

my_fourpanel_histogram_all_years_total(
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.25_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.3075_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.39_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.25_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.3075_excess_heat_summary.csv"),
  here::here("data", "output", "05_validation", "summary", "warning", "cluster", "cluster_0.39_excess_heat_summary.csv")
)

# 2025-04-18 updates ----

# ——— Helper to turn one NetCDF cluster‑file into a small stats tibble ———
process_nc_file <- function(nc_path) {
  # infer grid spacing (°) from the filename, e.g. “_0.25_”
  grid_size <- str_extract(basename(nc_path), "(?<=_)[0-9.]+(?=_)") %>% as.numeric()
  km_per_deg <- 111.32
  
  # read the NetCDF as a SpatRaster with a daily time dimension
  r <- rast(nc_path)
  times <- time(r)
  if (is.null(times)) stop("No time dimension in ", nc_path)
  
  # pull out all non‑zero cluster IDs across space×time
  df <- as.data.frame(r, xy = TRUE) %>%
    pivot_longer(-c(x,y), names_to = "time", values_to = "cluster") %>%
    mutate(
      date = as.Date(time),
      lon  = x,
      lat  = y
    ) %>%
    filter(cluster != 0)
  
  # per‑cluster duration and total spatial count
  stats <- df %>%
    group_by(cluster) %>%
    summarize(
      # +1 so a single‑day event has duration = 1
      duration     = as.numeric(diff(range(date))) + 1,
      total_count  = n_distinct(paste0(lon, "_", lat)),
      .groups      = "drop"
    ) %>%
    mutate(
      total_extent = total_count * (grid_size * km_per_deg)^2,
      grid         = factor(grid_size)
    ) %>%
    select(cluster, grid, duration, total_extent)
  
  return(stats)
}

# ——— Main plotting function ———
plot_cluster_duration_extent <- function(heat_dirs, precip_dir, base_filename = "2x2_duration_extent") {
  # (1) gather all .nc in each group
  heat_files   <- unlist(lapply(heat_dirs,   list.files, pattern = "\\.nc$", full.names = TRUE))
  precip_files <- list.files(precip_dir, pattern = "\\.nc$", full.names = TRUE)
  
  # (2) process
  heat_df   <- bind_rows(lapply(heat_files,   process_nc_file))
  precip_df <- bind_rows(lapply(precip_files, process_nc_file))
  
  # (3) compute per‑panel x‑limits
  dur_lim_heat   <- range(heat_df$duration,   na.rm = TRUE)
  dur_lim_precip <- range(precip_df$duration, na.rm = TRUE)
  ext_lim_heat   <- range(heat_df$total_extent[heat_df$total_extent>0],   na.rm=TRUE)
  ext_lim_precip <- range(precip_df$total_extent[precip_df$total_extent>0], na.rm=TRUE)
  
  # common styling
  common_theme <- theme_bw() +
    theme(legend.position = "bottom",
          panel.border    = element_rect(color="black", fill=NA))
  res_scale <- scale_fill_discrete(name = "Grid (°)")
  
  # (4) top row: duration
  p1_heat <- ggplot(heat_df, aes(duration, fill = grid)) +
    geom_histogram(bins = 30, position = "identity", alpha = 0.4, color = NA) +
    scale_x_continuous(limits = dur_lim_heat) +
    labs(x = "Duration (days)", y = "Count", title = "Heat clusters") +
    common_theme + res_scale
  
  p1_precip <- ggplot(precip_df, aes(duration, fill = grid)) +
    geom_histogram(bins = 30, position = "identity", alpha = 0.4, color = NA) +
    scale_x_continuous(limits = dur_lim_precip) +
    labs(x = "Duration (days)", y = NULL, title = "Precip. clusters") +
    common_theme + res_scale
  
  # (5) bottom row: total_extent (log scale)
  p2_heat <- ggplot(heat_df, aes(total_extent, fill = grid)) +
    geom_histogram(bins = 30, position = "identity", alpha = 0.4, color = NA) +
    scale_x_log10(limits = ext_lim_heat) +
    labs(x = "Total extent (km²)", y = "Count") +
    common_theme + res_scale
  
  p2_precip <- ggplot(precip_df, aes(total_extent, fill = grid)) +
    geom_histogram(bins = 30, position = "identity", alpha = 0.4, color = NA) +
    scale_x_log10(limits = ext_lim_precip) +
    labs(x = "Total extent (km²)", y = NULL) +
    common_theme + res_scale
  
  # (6) patchwork 2×2 layout
  combined <- (p1_heat   | p1_precip) /
    (p2_heat   | p2_precip) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # save
  ggsave(paste0(base_filename, ".png"), combined, width = 12, height = 8, dpi = 300)
  ggsave(paste0(base_filename, ".svg"), combined, width = 12, height = 8, device = "svg")
  
  invisible(list(heat = heat_df, precip = precip_df))
}

# ——— Example call ———
plot_cluster_duration_extent(
  heat_dirs   = c(here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.25_excess_heat_summary.csv"),
                  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.3075_excess_heat_summary.csv"),
                  here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.39_excess_heat_summary.csv")),
  precip_dir  = "data/precipitation/clusters",
  base_filename = "dur_ext_2x2"
)
