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
  precip_dir  = here::here("data", "output", "05_validation", "summary", "advisory", "cluster", "cluster_0.25_excess_heat_summary.csv"),
  base_filename = "dur_ext_2x2"
)

# 2025-04-21 updates ----

# 1. Read + preprocess CSV, filter to NC/SC clusters, expand counties, compute areas
process_cluster_csv <- function(file_path) {
  # 1) Read all as character
  df <- readr::read_csv(file_path, col_types = readr::cols(.default = "c"))
  
  # 2) Always‐present numeric coercions
  df <- df %>%
    mutate(
      cluster_id   = as.integer(cluster_id),
      exposed_area = as.double(exposed_area),
      era5_mean    = as.double(era5_mean),
      era5_median  = as.double(era5_median),
      era5_max     = as.double(era5_max)
    )
  
  # 3) Check which duration column exists and coerce it
  has_h <- "duration_hours" %in% names(df)
  has_d <- "duration"       %in% names(df)
  if (!has_h && !has_d) {
    stop("CSV must have either `duration_hours` or `duration`.")
  }
  if (has_h) df <- df %>% mutate(duration_hours = as.double(duration_hours))
  if (has_d) df <- df %>% mutate(duration       = as.double(duration))
  
  # 4) Rename the start‐date column
  if ("start_datetime" %in% names(df)) {
    df <- rename(df, raw_start = start_datetime)
  } else if ("start_date" %in% names(df)) {
    df <- rename(df, raw_start = start_date)
  } else {
    stop("No `start_datetime` or `start_date` in ", file_path)
  }
  
  # 5) Robustly parse raw_start → start_date
  df <- df %>%
    mutate(
      parsed_iso_ts = ymd_hms(raw_start, quiet = TRUE, tz = "UTC"),
      parsed_iso    = ymd     (raw_start, quiet = TRUE, tz = "UTC"),
      parsed_us_ts  = mdy_hms(raw_start, quiet = TRUE, tz = "UTC"),
      parsed_us     = mdy     (raw_start, quiet = TRUE, tz = "UTC"),
      start_date    = coalesce(parsed_iso_ts,
                               parsed_iso,
                               parsed_us_ts,
                               parsed_us)
    ) %>%
    select(-parsed_iso_ts, -parsed_iso,
           -parsed_us_ts,  -parsed_us)
  
  if (any(is.na(df$start_date))) {
    bad <- unique(df$raw_start[is.na(df$start_date)])
    stop("Failed to parse these raw_start values:\n",
         paste0("  • ", bad, collapse = "\n"))
  }
  
  # 6) Now compute duration_days in separate steps
  if (has_d && !has_h) {
    # only `duration` present
    df <- df %>% mutate(duration_days = duration)
  } else if (has_h && !has_d) {
    # only `duration_hours` present
    df <- df %>% mutate(duration_days = duration_hours / 24)
  } else {
    # both present: prefer `duration` if not NA, otherwise convert hours
    df <- df %>% mutate(
      duration_days = if_else(
        !is.na(duration),
        duration,
        duration_hours / 24
      )
    )
  }
  if (any(is.na(df$duration_days))) {
    stop("Some rows lack a valid duration; check your `duration`/`duration_hours` columns.")
  }
  
  # 7) Split out exposed_counties
  df <- df %>%
    mutate(exposed_counties_list = str_split(exposed_counties, ";")) %>%
    select(-raw_start)
  
  # 8) Filter to any NC/SC clusters
  df <- df %>%
    filter(
      purrr::map_lgl(
        exposed_counties_list,
        ~ any(str_detect(.x,
                         regex("north carolina,|south carolina,", ignore_case = TRUE)))
      )
    )
  
  # 9) Explode to one row per county
  df_long <- df %>%
    unnest(exposed_counties_list) %>%
    rename(exposed_county = exposed_counties_list) %>%
    mutate(
      state  = str_to_title(str_extract(exposed_county, "^[^,]+")),
      county = str_to_title(str_trim(str_extract(exposed_county, "(?<=,).*")))
    ) %>%
    filter(state %in% c("North Carolina", "South Carolina"))
  
  # 10) Load NC/SC counties + compute area
  counties_sf <- counties(state = c("NC","SC"), cb = TRUE, class = "sf") %>%
    transmute(
      state = case_when(
        STATEFP=="37" ~ "North Carolina",
        STATEFP=="45" ~ "South Carolina"
      ),
      county = NAME,
      geometry
    ) %>%
    mutate(county_area_km2 = as.numeric(st_area(geometry)) / 1e6)
  
  # 11) Join area back, summarize per‐cluster
  areas <- st_set_geometry(counties_sf, NULL) %>%
    select(state, county, county_area_km2)
  
  df_long <- df_long %>%
    left_join(areas, by = c("state","county"))
  
  cluster_area <- df_long %>%
    group_by(cluster_id, start_date, duration_days) %>%
    summarize(
      total_county_area = sum(county_area_km2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 12) Return for plotting
  list(
    df           = df,
    df_long      = df_long,
    counties_sf  = counties_sf,
    cluster_area = cluster_area
  )
}
# 1) Daily cluster count → points + loess
plot_cluster_count <- function(df) {
  df %>%
    count(start_date) %>%
    ggplot(aes(x = start_date, y = n)) +
    geom_point(size = 1.5) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      title = "Heatwave daily count over time across the Carolinas",
      x     = "Date",
      y     = "Number of heatwaves"
    ) +
    theme_bw()
}

# Daily cluster durations → raw points + loess
plot_cluster_duration <- function(df) {
  ggplot(df, aes(x = start_date, y = duration_days + 1)) +
    geom_point(size = 1.5) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      title = "Heatwave duration over time across the Carolinas",
      x     = "Date",
      y     = "Duration (days)"
    ) +
    theme_bw()
}

# Daily cluster exposed areas → raw points + loess
plot_cluster_area <- function(cluster_area) {
  ggplot(cluster_area, aes(x = start_date, y = total_county_area)) +
    geom_point(size = 1.5) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      title = "Heatwave exposed area over time across the Carolinas",
      x     = "Date",
      y     = "Area (km²)"
    ) +
    theme_bw()
}


# 4. Plot choropleth of county‑level exposure counts
plot_county_map <- function(df_long, counties_sf) {
  county_counts <- df_long %>%
    count(state, county, name = "cluster_count")
  
  counties_sf %>%
    left_join(county_counts, by = c("state","county")) %>%
    replace_na(list(cluster_count = 0)) %>%
    ggplot() +
    geom_sf(aes(fill = cluster_count), color = "white") +
    scale_fill_viridis_c(option = "viridis", name = "Heatwaves") +
    labs(title = "Number of Heatwaves by county across the Carolinas") +
    theme_bw() +
    theme(
      legend.position     = c(0.99, 0.01),
      legend.justification = c("right", "bottom"),
      legend.background   = element_rect(fill = alpha("white", 0.7), color = NA)
    )
}

# ——— Example usage ———
res <- process_cluster_csv(here::here("data", "output", "03_cluster", "02_cluster", "advisory","points","0.39","heat_index", "cluster_idf.csv"))
p1  <- plot_cluster_count(res$df)
p2  <- plot_cluster_duration(res$df)
p3  <- plot_cluster_area(res$cluster_area)
p4  <- plot_county_map(res$df_long, res$counties_sf)
print(p1); print(p2); print(p3); print(p4)

png_path <- here("figures", paste0("Carolina_heatwaves_daily_count", ".png"))
svg_path <- here("figures", paste0("Carolina_heatwaves_daily_count", ".svg"))
ggsave(filename = png_path, plot = p1, width = 7, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p1, width = 7, height = 6, device = "svg")

png_path <- here("figures", paste0("Carolina_heatwaves_duration", ".png"))
svg_path <- here("figures", paste0("Carolina_heatwaves_duration", ".svg"))
ggsave(filename = png_path, plot = p2, width = 7, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p2, width = 7, height = 6, device = "svg")

png_path <- here("figures", paste0("Carolina_heatwaves_area", ".png"))
svg_path <- here("figures", paste0("Carolina_heatwaves_area", ".svg"))
ggsave(filename = png_path, plot = p3, width = 7, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p3, width = 7, height = 6, device = "svg")

png_path <- here("figures", paste0("Carolina_heatwaves_spatial_count", ".png"))
svg_path <- here("figures", paste0("Carolina_heatwaves_spatial_count", ".svg"))
ggsave(filename = png_path, plot = p4, width = 7, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p4, width = 7, height = 6, device = "svg")

# All counties ----
process_cluster_csv <- function(file_path,
                                states = c("North Carolina", "South Carolina")) {
  # ——————————————————————————————
  # 0) Build a lookup of state.abbr ↔ state.name
  state_map <- tibble::tibble(
    abbr = state.abb,
    name = state.name
  )
  
  # 1) Interpret the `states` parameter: allow c("NC","SC") or c("North Carolina",…)
  param <- states
  abbrs      <- character(length(param))
  full_names <- character(length(param))
  
  for (i in seq_along(param)) {
    s <- param[i]
    if (toupper(s) %in% state_map$abbr) {
      abbrs[i]      <- toupper(s)
      full_names[i] <- state_map$name[state_map$abbr == abbrs[i]]
    } else if (s %in% state_map$name) {
      full_names[i] <- s
      abbrs[i]      <- state_map$abbr[state_map$name == s]
    } else {
      stop("`states` must be valid US state names or abbreviations. Problem: ", s)
    }
  }
  
  # ——————————————————————————————
  # 2) Read all columns as character, coerce known numerics
  df <- readr::read_csv(file_path, col_types = readr::cols(.default = "c")) %>%
    mutate(
      cluster_id     = as.integer(cluster_id),
      exposed_area   = as.double(exposed_area),
      era5_mean      = as.double(era5_mean),
      era5_median    = as.double(era5_median),
      era5_max       = as.double(era5_max)
    )
  
  # 3) Detect & coerce duration column
  has_h <- "duration_hours" %in% names(df)
  has_d <- "duration"       %in% names(df)
  if (!has_h && !has_d) {
    stop("CSV must have either `duration_hours` or `duration`.")
  }
  if (has_h) df <- df %>% mutate(duration_hours = as.double(duration_hours))
  if (has_d) df <- df %>% mutate(duration       = as.double(duration))
  
  # 4) Rename the start‐date column to `raw_start`
  if ("start_datetime" %in% names(df)) {
    df <- rename(df, raw_start = start_datetime)
  } else if ("start_date" %in% names(df)) {
    df <- rename(df, raw_start = start_date)
  } else {
    stop("No `start_datetime` or `start_date` column found in ", file_path)
  }
  
  # 5) Robustly parse `raw_start` into `start_date`
  df <- df %>%
    mutate(
      iso_ts = ymd_hms(raw_start, quiet = TRUE, tz = "UTC"),
      iso    = ymd     (raw_start, quiet = TRUE, tz = "UTC"),
      us_ts  = mdy_hms(raw_start, quiet = TRUE, tz = "UTC"),
      us     = mdy     (raw_start, quiet = TRUE, tz = "UTC"),
      start_date = coalesce(iso_ts, iso, us_ts, us)
    ) %>%
    select(-iso_ts, -iso, -us_ts, -us)
  
  if (any(is.na(df$start_date))) {
    bad <- unique(df$raw_start[is.na(df$start_date)])
    stop("Failed to parse these raw_start values:\n",
         paste0("  • ", bad, collapse = "\n"))
  }
  
  # 6) Compute `duration_days` in separate branches
  if (has_d && !has_h) {
    df <- df %>% mutate(duration_days = duration)
  } else if (has_h && !has_d) {
    df <- df %>% mutate(duration_days = duration_hours / 24)
  } else {
    df <- df %>% mutate(
      duration_days = if_else(!is.na(duration),
                              duration,
                              duration_hours / 24)
    )
  }
  if (any(is.na(df$duration_days))) {
    stop("Some rows have no valid duration. Check `duration`/`duration_hours`.")
  }
  
  # 7) Split out counties and drop raw_start
  df <- df %>%
    mutate(exposed_counties_list = str_split(exposed_counties, ";")) %>%
    select(-raw_start)
  
  # 8) Filter to any clusters hitting *any* of the specified states
  df <- df %>%
    filter(
      purrr::map_lgl(
        exposed_counties_list,
        ~ any(str_detect(.x,
                         regex(paste0(full_names, collapse = "|"),
                               ignore_case = TRUE)))
      )
    )
  
  # 9) Explode to one row per county, extract state + county names
  df_long <- df %>%
    unnest(exposed_counties_list) %>%
    rename(exposed_county = exposed_counties_list) %>%
    mutate(
      state  = str_to_title(str_extract(exposed_county, "^[^,]+")),
      county = str_to_title(str_trim(str_extract(exposed_county, "(?<=,).*")))
    ) %>%
    filter(state %in% full_names)
  
  # 10) Load only the counties for those states, compute each area (km²)
  counties_sf <- counties(state = abbrs, cb = TRUE, class = "sf") %>%
    transmute(
      state = case_when(
        STATEFP %in% abbrs ~ state_map$name[match(STATEFP, state_map$abbr)]
      ),
      county = NAME,
      geometry
    ) %>%
    mutate(county_area_km2 = as.numeric(st_area(geometry)) / 1e6)
  
  # 11) Join areas back to df_long, then summarize per‐cluster
  areas <- st_set_geometry(counties_sf, NULL) %>%
    select(state, county, county_area_km2)
  
  df_long <- left_join(df_long, areas, by = c("state","county"))
  
  cluster_area <- df_long %>%
    group_by(cluster_id, start_date, duration_days) %>%
    summarize(
      total_county_area = sum(county_area_km2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 12) Return everything for downstream plotting
  list(
    df           = df,
    df_long      = df_long,
    counties_sf  = counties_sf,
    cluster_area = cluster_area
  )
}


plot_county_map_all <- function(df_long) {
  # 1) Build a lookup from FIPS → full state name + county name
  fips_lookup <- tigris::fips_codes %>%
    mutate(
      # strip trailing " County" from the fips_codes county column
      county_name = str_remove(county, " County$")
    ) %>%
    select(
      STATEFP   = state_code,
      state     = state_name,
      county    = county_name
    ) %>%
    distinct()
  
  # 2) Load every county in the U.S. (cb = cartographic boundary)
  counties_all <- tigris::counties(cb = TRUE, class = "sf") %>%
    left_join(fips_lookup, by = "STATEFP") %>%
    # keep only the two columns we need plus geometry
    transmute(state, county = NAME, geometry)
  
  # 3) Count clusters per county in your df_long
  county_counts <- df_long %>%
    count(state, county, name = "cluster_count")
  
  # 4) Join and keep only counties that actually appear in county_counts
  plot_sf <- inner_join(counties_all, county_counts,
                        by = c("state","county"))
  
  # 5) Plot
  ggplot(plot_sf) +
    geom_sf(aes(fill = cluster_count), color = "white", size = 0.1) +
    scale_fill_viridis_c(option = "viridis", name = "Heatwaves") +
    labs(title = "Heatwaves by County (all reported)") +
    theme_bw() +
    theme(
      legend.position      = c(0.99, 0.01),
      legend.justification = c("right", "bottom"),
      legend.background    = element_rect(fill = alpha("white", 0.7), color = NA)
    )
}
southeast_states <- c("AL","FL","GA","KY","LA","MS","NC","SC","TN","VA","TX",
                      "OK","AR","MO","KS","IL","OH","IN","WV","MD","DE","NJ",
                      "PA")
res <- process_cluster_csv(here::here("data", "output", "03_cluster", "02_cluster", "advisory","points","0.39","heat_index", "cluster_idf.csv"), states = southeast_states)
p_all <- plot_county_map_all(res$df_long)

png_path <- here("figures", paste0("Southeast_heatwaves_spatial_count", ".png"))
svg_path <- here("figures", paste0("Southeast_heatwaves_spatial_count", ".svg"))
ggsave(filename = png_path, plot = p_all, width = 7, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p_all, width = 7, height = 6, device = "svg")
