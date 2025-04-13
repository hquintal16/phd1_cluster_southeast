#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 7 panel figures of noaa events, intensity, duration, exposed area, impacts

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s04_cluster_noaa_idf_impacts.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Heat Index Advisory ----
plot_normalized_time_series_updated <- function(noaa_path, cluster_path, date_format = "%m/%d/%Y") {
  
  # Helper function to read in a file or get the first CSV in a directory
  read_input_data <- function(path) {
    if (file.exists(path)) {
      # If the given path exists as a file, read it directly.
      return(list(data = read_csv(path), file = path))
    } else {
      # Otherwise, treat path as a directory and look for CSV files.
      csv_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)
      if (length(csv_files) == 0) {
        stop("No valid CSV files found in directory: ", path)
      }
      return(list(data = read_csv(csv_files[1]), file = csv_files[1]))
    }
  }
  
  # Read NOAA data and file info
  noaa_input <- read_input_data(noaa_path)
  noaa_data <- noaa_input$data
  noaa_file <- noaa_input$file
  
  # Read Cluster data and file info
  cluster_input <- read_input_data(cluster_path)
  cluster_data <- cluster_input$data
  cluster_file <- cluster_input$file
  
  ### STANDARDIZE COLUMN NAMES AND FORMATS
  standardize_data <- function(df, date_format) {
    # Rename alternative column names for start_time
    if (!"start_time" %in% names(df)) {
      if ("start_datetime" %in% names(df)) {
        df <- df %>% rename(start_time = start_datetime)
      } else if ("start_date" %in% names(df)) {
        df <- df %>% rename(start_time = start_date)
      }
    }
    # Rename alternative column names for end_time
    if (!"end_time" %in% names(df)) {
      if ("end_date_time" %in% names(df)) {
        df <- df %>% rename(end_time = end_date_time)
      } else if ("end_date" %in% names(df)) {
        df <- df %>% rename(end_time = end_date)
      }
    }
    # Standardize duration: if duration is missing, rename duration_hours and convert from hours to days.
    if (!"duration" %in% names(df)) {
      if ("duration_hours" %in% names(df)) {
        df <- df %>% rename(duration = duration_hours) %>% 
          mutate(duration = duration / 24)
      }
    }
    # Convert start_time to a Date object using the specified format.
    df <- df %>% mutate(start_time = as.Date(start_time, format = date_format))
    return(df)
  }
  
  noaa_data <- standardize_data(noaa_data, date_format)
  cluster_data <- standardize_data(cluster_data, date_format)
  
  ### VERIFY THAT THE KEY COLUMNS EXIST
  # Required columns:
  # - start_time (Time)
  # - era5_mean (Mean Heat Index in °C)
  # - duration (Duration in days)
  # - exposed_area (Exposed Area in km²)
  required_columns <- c("start_time", "era5_mean", "duration", "exposed_area")
  for (col in required_columns) {
    if (!col %in% names(noaa_data)) stop(paste("Column", col, "not found in NOAA data."))
    if (!col %in% names(cluster_data)) stop(paste("Column", col, "not found in Cluster data."))
  }
  
  ### COMPUTE GLOBAL SCALE LIMITS
  # Combine both datasets so that scales (time, era5_mean, duration, exposed_area) are normalized.
  combined_data <- bind_rows(noaa_data, cluster_data)
  
  global_x_range      <- range(combined_data$start_time, na.rm = TRUE)
  global_y_range      <- range(combined_data$era5_mean, na.rm = TRUE)
  global_size_range   <- range(combined_data$duration, na.rm = TRUE)
  global_color_range  <- range(combined_data$exposed_area, na.rm = TRUE)
  
  ### CREATE NORMALIZED PLOTS
  # NOAA Plot
  plot_noaa <- ggplot(noaa_data, aes(x = start_time, y = era5_mean,
                                     size = duration, color = exposed_area)) +
    geom_point() +
    labs(title = "NOAA Data",
         x = "Time",
         y = "Mean Heat Index (°C)",
         size = "Duration (days)",
         color = "Exposed Area (km²)") +
    scale_x_date(limits = global_x_range) +
    # Set the lower limit of y to 35
    scale_y_continuous(limits = c(37.77, global_y_range[2])) +
    # Set custom size breaks
    scale_size_continuous(limits = global_size_range,
                          breaks = c(0, 7, 14, 30, 60, 90),
                          labels = c("0", "7", "14", "30", "60", "90+")) +
    scale_color_gradient(limits = global_color_range, low = "lightgreen", high = "darkgreen") +
    theme_bw()
  
  # Cluster Plot
  plot_cluster <- ggplot(cluster_data, aes(x = start_time, y = era5_mean,
                                           size = duration, color = exposed_area)) +
    geom_point() +
    labs(title = "Cluster Data",
         x = "Time",
         y = "Mean Heat Index (°C)",
         size = "Duration (days)",
         color = "Exposed Area (km²)") +
    scale_x_date(limits = global_x_range) +
    scale_y_continuous(limits = c(37.77, global_y_range[2])) +
    scale_size_continuous(limits = global_size_range,
                          breaks = c(0, 7, 14, 30, 60, 90),
                          labels = c("0", "7", "14", "30", "60", "90+")) +
    scale_color_gradient(limits = global_color_range, low = "lightgreen", high = "darkgreen") +
    theme_bw()
  
  ### SAVE THE PLOTS WITH FILENAMES DERIVED FROM INPUT FILE NAMES
  # Extract base names (without extension) from the input file paths.
  noaa_base <- tools::file_path_sans_ext(basename(noaa_file))
  cluster_base <- tools::file_path_sans_ext(basename(cluster_file))
  
  # Build file names using the base names.
  png_path_noaa    <- here("figures", paste0("05_", noaa_base, "_advisory_plot.png"))
  svg_path_noaa    <- here("figures", paste0("05_", noaa_base, "_advisory_plot.svg"))
  png_path_cluster <- here("figures", paste0("05_", cluster_base, "_advisory_plot.png"))
  svg_path_cluster <- here("figures", paste0("05_", cluster_base, "_advisory_plot.svg"))
  
  # Save NOAA plot
  ggsave(filename = png_path_noaa, plot = plot_noaa, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_noaa, plot = plot_noaa, width = 9, height = 6, device = "svg")
  
  # Save Cluster plot
  ggsave(filename = png_path_cluster, plot = plot_cluster, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_cluster, plot = plot_cluster, width = 9, height = 6, device = "svg")
  
  # Return the plots for further processing if needed
  return(list(noaa_plot = plot_noaa, cluster_plot = plot_cluster))
}

## 0.25 deg / day ----
results <- plot_normalized_time_series_updated(
  noaa_path = here::here("data", "output", "05_validation", "summary", "noaa", "NOAA_excess_heat_summary.csv"),
  cluster_path = here::here("data", "output", "05_validation", "summary","advisory", "cluster", "cluster_0.25_excess_heat_summary.csv")
)
## 0.3075 deg / day ----
results <- plot_normalized_time_series_updated(
  noaa_path = here::here("data", "output", "05_validation", "summary", "noaa", "NOAA_excess_heat_summary.csv"),
  cluster_path = here::here("data", "output", "05_validation", "summary","advisory", "cluster", "cluster_0.3075_excess_heat_summary.csv")
)
## 0.39 deg / day ----
results <- plot_normalized_time_series_updated(
  noaa_path = here::here("data", "output", "05_validation", "summary", "noaa", "NOAA_excess_heat_summary.csv"),
  cluster_path = here::here("data", "output", "05_validation", "summary","advisory", "cluster", "cluster_0.39_excess_heat_summary.csv")
)

# Heat Index Warning ----
plot_normalized_time_series_updated <- function(noaa_path, cluster_path, date_format = "%m/%d/%Y") {
  
  # Helper function to read in a file or get the first CSV in a directory
  read_input_data <- function(path) {
    if (file.exists(path)) {
      # If the given path exists as a file, read it directly.
      return(list(data = read_csv(path), file = path))
    } else {
      # Otherwise, treat path as a directory and look for CSV files.
      csv_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)
      if (length(csv_files) == 0) {
        stop("No valid CSV files found in directory: ", path)
      }
      return(list(data = read_csv(csv_files[1]), file = csv_files[1]))
    }
  }
  
  # Read NOAA data and file info
  noaa_input <- read_input_data(noaa_path)
  noaa_data <- noaa_input$data
  noaa_file <- noaa_input$file
  
  # Read Cluster data and file info
  cluster_input <- read_input_data(cluster_path)
  cluster_data <- cluster_input$data
  cluster_file <- cluster_input$file
  
  ### STANDARDIZE COLUMN NAMES AND FORMATS
  standardize_data <- function(df, date_format) {
    # Rename alternative column names for start_time
    if (!"start_time" %in% names(df)) {
      if ("start_datetime" %in% names(df)) {
        df <- df %>% rename(start_time = start_datetime)
      } else if ("start_date" %in% names(df)) {
        df <- df %>% rename(start_time = start_date)
      }
    }
    # Rename alternative column names for end_time
    if (!"end_time" %in% names(df)) {
      if ("end_date_time" %in% names(df)) {
        df <- df %>% rename(end_time = end_date_time)
      } else if ("end_date" %in% names(df)) {
        df <- df %>% rename(end_time = end_date)
      }
    }
    # Standardize duration: if duration is missing, rename duration_hours and convert from hours to days.
    if (!"duration" %in% names(df)) {
      if ("duration_hours" %in% names(df)) {
        df <- df %>% rename(duration = duration_hours) %>% 
          mutate(duration = duration / 24)
      }
    }
    # Convert start_time to a Date object using the specified format.
    df <- df %>% mutate(start_time = as.Date(start_time, format = date_format))
    return(df)
  }
  
  noaa_data <- standardize_data(noaa_data, date_format)
  cluster_data <- standardize_data(cluster_data, date_format)
  
  ### VERIFY THAT THE KEY COLUMNS EXIST
  # Required columns:
  # - start_time (Time)
  # - era5_mean (Mean Heat Index in °C)
  # - duration (Duration in days)
  # - exposed_area (Exposed Area in km²)
  required_columns <- c("start_time", "era5_mean", "duration", "exposed_area")
  for (col in required_columns) {
    if (!col %in% names(noaa_data)) stop(paste("Column", col, "not found in NOAA data."))
    if (!col %in% names(cluster_data)) stop(paste("Column", col, "not found in Cluster data."))
  }
  
  ### COMPUTE GLOBAL SCALE LIMITS
  # Combine both datasets so that scales (time, era5_mean, duration, exposed_area) are normalized.
  combined_data <- bind_rows(noaa_data, cluster_data)
  
  global_x_range      <- range(combined_data$start_time, na.rm = TRUE)
  global_y_range      <- range(combined_data$era5_mean, na.rm = TRUE)
  global_size_range   <- range(combined_data$duration, na.rm = TRUE)
  global_color_range  <- range(combined_data$exposed_area, na.rm = TRUE)
  
  ### CREATE NORMALIZED PLOTS
  # NOAA Plot
  plot_noaa <- ggplot(noaa_data, aes(x = start_time, y = era5_mean,
                                     size = duration, color = exposed_area)) +
    geom_point() +
    labs(title = "NOAA Data",
         x = "Time",
         y = "Mean Heat Index (°C)",
         size = "Duration (days)",
         color = "Exposed Area (km²)") +
    scale_x_date(limits = global_x_range) +
    # Set the lower limit of y to 35
    scale_y_continuous(limits = c(42.77, global_y_range[2])) +
    # Set custom size breaks
    scale_size_continuous(limits = global_size_range,
                          breaks = c(0, 7, 14, 30, 60, 90),
                          labels = c("0", "7", "14", "30", "60", "90+")) +
    scale_color_gradient(limits = global_color_range, low = "lightgreen", high = "darkgreen") +
    theme_bw()
  
  # Cluster Plot
  plot_cluster <- ggplot(cluster_data, aes(x = start_time, y = era5_mean,
                                           size = duration, color = exposed_area)) +
    geom_point() +
    labs(title = "Cluster Data",
         x = "Time",
         y = "Mean Heat Index (°C)",
         size = "Duration (days)",
         color = "Exposed Area (km²)") +
    scale_x_date(limits = global_x_range) +
    scale_y_continuous(limits = c(42.77, global_y_range[2])) +
    scale_size_continuous(limits = global_size_range,
                          breaks = c(0, 7, 14, 30, 60, 90),
                          labels = c("0", "7", "14", "30", "60", "90+")) +
    scale_color_gradient(limits = global_color_range, low = "lightgreen", high = "darkgreen") +
    theme_bw()
  
  ### SAVE THE PLOTS WITH FILENAMES DERIVED FROM INPUT FILE NAMES
  # Extract base names (without extension) from the input file paths.
  noaa_base <- tools::file_path_sans_ext(basename(noaa_file))
  cluster_base <- tools::file_path_sans_ext(basename(cluster_file))
  
  # Build file names using the base names.
  png_path_noaa    <- here("figures", paste0("05_", noaa_base, "_warning_plot.png"))
  svg_path_noaa    <- here("figures", paste0("05_", noaa_base, "_warning_plot.svg"))
  png_path_cluster <- here("figures", paste0("05_", cluster_base, "_warning_plot.png"))
  svg_path_cluster <- here("figures", paste0("05_", cluster_base, "_warning_plot.svg"))
  
  # Save NOAA plot
  ggsave(filename = png_path_noaa, plot = plot_noaa, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_noaa, plot = plot_noaa, width = 9, height = 6, device = "svg")
  
  # Save Cluster plot
  ggsave(filename = png_path_cluster, plot = plot_cluster, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_cluster, plot = plot_cluster, width = 9, height = 6, device = "svg")
  
  # Return the plots for further processing if needed
  return(list(noaa_plot = plot_noaa, cluster_plot = plot_cluster))
}

## 0.25 deg / day ----
results <- plot_normalized_time_series_updated(
  noaa_path = here::here("data", "output", "05_validation", "summary", "noaa", "NOAA_excess_heat_summary.csv"),
  cluster_path = here::here("data", "output", "05_validation", "summary","warning", "cluster", "cluster_0.25_excess_heat_summary.csv")
)
## 0.3075 deg / day ----
results <- plot_normalized_time_series_updated(
  noaa_path = here::here("data", "output", "05_validation", "summary", "noaa", "NOAA_excess_heat_summary.csv"),
  cluster_path = here::here("data", "output", "05_validation", "summary","warning", "cluster", "cluster_0.3075_excess_heat_summary.csv")
)
## 0.39 deg / day ----
results <- plot_normalized_time_series_updated(
  noaa_path = here::here("data", "output", "05_validation", "summary", "noaa", "NOAA_excess_heat_summary.csv"),
  cluster_path = here::here("data", "output", "05_validation", "summary","warning", "cluster", "cluster_0.39_excess_heat_summary.csv")
)
