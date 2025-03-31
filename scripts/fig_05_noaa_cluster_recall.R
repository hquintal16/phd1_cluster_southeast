#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 9 panel figures of noaa events, clusters, and recall by county

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_05_noaa_cluster_recall.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# make_9panel_figure <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
#   # library(terra)
#   # library(sf)
#   # library(ggplot2)
#   # library(patchwork)
#   # library(maps)
#   # library(dplyr)
#   # library(here)
#   # library(pbapply)
#   # library(stringr)
#   
#   message("DEBUG: noaa_dir: ", noaa_dir)
#   message("DEBUG: output_dir: ", output_dir)
#   message("DEBUG: year_range: ", paste(year_range, collapse = ", "))
#   message("DEBUG: cluster_dirs:")
#   for(nm in names(cluster_dirs)){
#     message("  ", nm, ": ", cluster_dirs[[nm]])
#   }
#   message("DEBUG: recall_dirs:")
#   for(nm in names(recall_dirs)){
#     message("  ", nm, ": ", recall_dirs[[nm]])
#   }
#   
#   message("Loading reference raster and county boundaries...")
#   # Load a reference raster to set target CRS and extent
#   region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
#                             "heat_index_daily_maximum_194001.nc"))
#   
#   # Load Koppen-Geiger raster and derive a binary resolution raster
#   kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
#                   "1991_2020", "koppen_geiger_0p1.tif")
#   resolution <- rast(kg_path)
#   resolution <- resolution / resolution
#   
#   # Load US county boundaries (via maps) and convert to an sf object and SpatVector
#   us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
#   us.states.vect <- vect(us.states)
#   us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
#   us.states.rast <- crop(us.states.rast, ext(region.crs))
#   
#   ### Helper function: Process files by filtering for a required substring (if provided), then
#   ### extract the year from the filename and subset to the given year range. Then read all files,
#   ### combine them into a SpatRaster (one layer per file), and sum the layers using sum().
#   process_sum_mask <- function(dir_path, file_prefix, yr_range, required_substring = "") {
#     if (required_substring != "") {
#       files <- list.files(dir_path, pattern = required_substring, full.names = TRUE)
#       message("Found ", length(files), " files in ", dir_path, " containing '", required_substring, "'.")
#       if (length(files) > 0) {
#         extracted_years <- as.numeric(str_extract(basename(files), "\\d{4}"))
#         message("Extracted years: ", paste(extracted_years, collapse = ", "))
#         files <- files[extracted_years %in% yr_range]
#         message("After year filtering, ", length(files), " files remain.")
#       }
#     } else {
#       pattern <- paste0(file_prefix, ".*(", paste(yr_range, collapse = "|"), ").*\\.nc$")
#       files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
#     }
#     if(length(files) == 0) {
#       warning("No files found in ", dir_path, " for years ", paste(yr_range, collapse = ", "))
#       return(NULL)
#     }
#     message("Reading ", length(files), " files from ", dir_path)
#     rast_obj <- rast(files)  # Each file becomes one layer
#     message("Summing ", nlyr(rast_obj), " layers using sum()...")
#     summed <- sum(rast_obj)  # Sums all layers into one SpatRaster
#     message("Global sum after summing: ", global(summed, fun = "sum", na.rm = TRUE)[1])
#     masked <- mask(summed, us.states.rast, maskvalue = NA)
#     return(masked)
#   }
#   
#   ### Process NOAA data:
#   message("Processing NOAA data...")
#   noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
#   if(is.null(noaa_raster)) stop("NOAA raster not found!")
#   
#   ### Process Cluster directories (three expected: stm1, record, stm4)
#   message("Processing Cluster data...")
#   cluster_rasters <- lapply(names(cluster_dirs), function(name) {
#     dir_path <- cluster_dirs[[name]]
#     r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
#     if(is.null(r)) return(NULL)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(cluster_rasters) <- names(cluster_dirs)
#   
#   ### Process Recall files (direct file paths)
#   message("Processing Recall data...")
#   recall_rasters <- lapply(names(recall_dirs), function(name) {
#     file_path <- recall_dirs[[name]]
#     r <- rast(file_path)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(recall_rasters) <- names(recall_dirs)
#   
#   ### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf()
#   plot_raster <- function(r) {
#     df <- as.data.frame(r, xy = TRUE)
#     if(ncol(df) < 3) stop("Raster does not have expected columns.")
#     value_col <- names(df)[3]
#     p <- ggplot() +
#       geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#       scale_fill_viridis_c(na.value = "grey", name = "Value") +
#       geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#       coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#       theme_minimal() +
#       theme(axis.title = element_blank(),
#             axis.text = element_blank(),
#             axis.ticks = element_blank(),
#             plot.title = element_blank())
#     return(p)
#   }
#   
#   ### Create panels:
#   message("Creating panels...")
#   p_a <- plot_raster(noaa_raster)
#   p_b <- plot_raster(noaa_raster)
#   p_c <- plot_raster(noaa_raster)
#   
#   p_d <- plot_raster(cluster_rasters[["stm1"]])
#   p_e <- plot_raster(cluster_rasters[["record"]])
#   p_f <- plot_raster(cluster_rasters[["stm4"]])
#   
#   p_g <- plot_raster(recall_rasters[["stm1"]])
#   p_h <- plot_raster(recall_rasters[["record"]])
#   p_i <- plot_raster(recall_rasters[["stm4"]])
#   
#   message("Assembling 9-panel figure using patchwork...")
#   # library(patchwork)
#   figure <- (p_a + p_b + p_c) / (p_d + p_e + p_f) / (p_g + p_h + p_i) +
#     plot_annotation(tag_levels = "a")
#   
#   message("Figure creation complete.")
#   return(figure)
# }

make_9panel_figure <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
  # library(terra)
  # library(sf)
  # library(ggplot2)
  # library(patchwork)
  # library(maps)
  # library(dplyr)
  # library(here)
  # library(pbapply)
  # library(stringr)
  
  message("DEBUG: noaa_dir: ", noaa_dir)
  message("DEBUG: output_dir: ", output_dir)
  message("DEBUG: year_range: ", paste(year_range, collapse = ", "))
  message("DEBUG: cluster_dirs:")
  for(nm in names(cluster_dirs)){
    message("  ", nm, ": ", cluster_dirs[[nm]])
  }
  message("DEBUG: recall_dirs:")
  for(nm in names(recall_dirs)){
    message("  ", nm, ": ", recall_dirs[[nm]])
  }
  
  message("Loading reference raster and county boundaries...")
  # Load a reference raster (for target CRS and extent)
  region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index", 
                          "heat_index_daily_maximum_194001.nc"))
  
  # Load Koppen-Geiger raster and derive a binary resolution raster
  kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
                  "1991_2020", "koppen_geiger_0p1.tif")
  resolution <- rast(kg_path)
  resolution <- resolution / resolution
  
  # Load US county boundaries via maps and convert to an sf object and SpatVector
  us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
  us.states.vect <- vect(us.states)
  us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
  us.states.rast <- crop(us.states.rast, ext(region.crs))
  
  ### Helper function: Process files by summing layers over a year range and masking to counties.
  process_sum_mask <- function(dir_path, file_prefix, yr_range, required_substring = "") {
    if (required_substring != "") {
      files <- list.files(dir_path, pattern = required_substring, full.names = TRUE)
      message("Found ", length(files), " files in ", dir_path, " containing '", required_substring, "'.")
      if (length(files) > 0) {
        extracted_years <- as.numeric(str_extract(basename(files), "\\d{4}"))
        message("Extracted years: ", paste(extracted_years, collapse = ", "))
        files <- files[extracted_years %in% yr_range]
        message("After year filtering, ", length(files), " files remain.")
      }
    } else {
      pattern <- paste0(file_prefix, ".*(", paste(yr_range, collapse = "|"), ").*\\.nc$")
      files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
    }
    if (length(files) == 0) {
      warning("No files found in ", dir_path, " for years ", paste(yr_range, collapse = ", "))
      return(NULL)
    }
    message("Reading ", length(files), " files from ", dir_path)
    rast_obj <- rast(files)  # Each file becomes one layer
    message("Summing ", nlyr(rast_obj), " layers using sum()...")
    summed <- sum(rast_obj)
    message("Global sum after summing: ", global(summed, fun = "sum", na.rm = TRUE)[1])
    masked <- mask(summed, us.states.rast, maskvalue = NA)
    return(masked)
  }
  
  ### Process NOAA data:
  message("Processing NOAA data...")
  noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
  if (is.null(noaa_raster)) stop("NOAA raster not found!")
  
  ### Process Cluster directories:
  message("Processing Cluster data...")
  cluster_rasters <- lapply(names(cluster_dirs), function(name) {
    dir_path <- cluster_dirs[[name]]
    r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
    if (is.null(r)) return(NULL)
    mask(r, us.states.rast, maskvalue = NA)
  })
  names(cluster_rasters) <- names(cluster_dirs)
  
  ### Process Recall files (direct file paths):
  message("Processing Recall data...")
  recall_rasters <- lapply(names(recall_dirs), function(name) {
    file_path <- recall_dirs[[name]]
    r <- rast(file_path)
    mask(r, us.states.rast, maskvalue = NA)
  })
  names(recall_rasters) <- names(recall_dirs)
  
  ### Determine common scale limits for Cluster rasters:
  valid_cluster <- cluster_rasters[!sapply(cluster_rasters, is.null)]
  if (length(valid_cluster) == 0) stop("No valid cluster rasters available.")
  max_cluster <- max(unlist(sapply(valid_cluster, function(r) {
    global(r, fun = "max", na.rm = TRUE)[1]
  })), na.rm = TRUE)
  message("Maximum value across cluster rasters: ", max_cluster)
  
  ### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf()
  plot_raster <- function(r, scale_limits = NULL) {
    df <- as.data.frame(r, xy = TRUE)
    if (ncol(df) < 3) stop("Raster does not have expected columns.")
    value_col <- names(df)[3]
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
      scale_fill_viridis_c(na.value = "grey", name = "Value", limits = scale_limits, direction = -1) +
      geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
      coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_blank())
    return(p)
  }
  
  ### Create panels:
  message("Creating panels...")
  p_a <- plot_raster(noaa_raster)
  p_b <- plot_raster(noaa_raster)
  p_c <- plot_raster(noaa_raster)
  
  p_d <- plot_raster(cluster_rasters[["stm1"]], scale_limits = c(0, max_cluster))
  p_e <- plot_raster(cluster_rasters[["record"]], scale_limits = c(0, max_cluster))
  p_f <- plot_raster(cluster_rasters[["stm4"]], scale_limits = c(0, max_cluster))
  
  p_g <- plot_raster(recall_rasters[["stm1"]])
  p_h <- plot_raster(recall_rasters[["record"]])
  p_i <- plot_raster(recall_rasters[["stm4"]])
  
  message("Assembling 9-panel figure using patchwork...")
  # library(patchwork)
  figure <- (p_a + p_b + p_c) / (p_d + p_e + p_f) / (p_g + p_h + p_i) +
    plot_annotation(tag_levels = "a")
  
  message("Figure creation complete.")
  return(figure)
}

# Excess Heat ----
## Week ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")

# Cluster directories: a named list of three directories
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster", "points", "record", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "stm4", "heat_index", "summary")
)

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_week_stm1_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall", "raster", "2019_2023_week_record_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_week_stm4_excess_heat.nc")
)

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_9panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_excess_heat_week.png")
svg_path <- here("figures","05_recall_excess_heat_week.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = figure, width = 9, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = figure, width = 9, height = 6, device = "svg")

## Day ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")

# Cluster directories: a named list of three directories
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster", "points", "record", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "stm4", "heat_index", "summary")
)

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_record_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm4_excess_heat.nc")
)

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_9panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_excess_heat_day.png")
svg_path <- here("figures","05_recall_excess_heat_day.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = figure, width = 9, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = figure, width = 9, height = 6, device = "svg")

# Heat ----

## Week ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "heat")

# Cluster directories: a named list of three directories
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster", "points", "record", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "stm4", "heat_index", "summary")
)

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_week_stm1_heat.nc"),
  record = here("data", "output", "05_validation", "recall", "raster", "2019_2023_week_record_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_week_stm4_heat.nc")
)

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_9panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_heat_week.png")
svg_path <- here("figures","05_recall_heat_week.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = figure, width = 9, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = figure, width = 9, height = 6, device = "svg")

## Day ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "heat")

# Cluster directories: a named list of three directories
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster", "points", "record", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "stm4", "heat_index", "summary")
)

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_heat.nc"),
  record = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_record_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm4_heat.nc")
)

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_9panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_heat_day.png")
svg_path <- here("figures","05_recall_heat_day.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = figure, width = 9, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = figure, width = 9, height = 6, device = "svg")

# Aggregated recall ----
aggregate_validation_csv_overall <- function(input_folder, year_range) {
  
  message("Listing CSV files in folder: ", input_folder)
  csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
  message("Found ", length(csv_files), " CSV files.")
  
  if(length(csv_files) == 0) {
    stop("No CSV files found in ", input_folder)
  }
  
  # Extract a 4-digit year from each filename (assumes filename like "2000_validation_results.csv")
  extracted_years <- as.numeric(str_extract(basename(csv_files), "\\d{4}"))
  message("Extracted years: ", paste(extracted_years, collapse = ", "))
  
  # Subset files to only those in the specified year range
  valid_files <- csv_files[extracted_years %in% year_range]
  message("Using ", length(valid_files), " files for years: ", paste(year_range, collapse = ", "))
  
  if(length(valid_files) == 0) {
    stop("No CSV files found for the specified year range: ", paste(year_range, collapse = ", "))
  }
  
  # Read each CSV file into a data frame
  df_list <- lapply(valid_files, function(f) {
    message("Reading file: ", f)
    read.csv(f, stringsAsFactors = FALSE)
  })
  
  # Combine all data frames
  combined_df <- bind_rows(df_list)
  message("Combined data has ", nrow(combined_df), " rows.")
  
  # Ensure required columns are present (TP and FN)
  required_cols <- c("TP", "FN")
  if(!all(required_cols %in% names(combined_df))) {
    stop("Not all required columns (", paste(required_cols, collapse = ", "), ") are present in the data.")
  }
  
  # Sum all rows (overall) without grouping by any identifier
  aggregated <- combined_df %>%
    summarise(
      TP = sum(TP, na.rm = TRUE),
      FN = sum(FN, na.rm = TRUE)
    )
  
  aggregated$recall <- ifelse((aggregated$TP + aggregated$FN) > 0,
                              aggregated$TP / (aggregated$TP + aggregated$FN),
                              NA)
  
  message("Aggregation complete. Overall recall: ", aggregated$recall)
  return(aggregated)
}

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "excess_heat")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
excess_heat <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "record", "excess_heat")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
excess_heat <- rbind(excess_heat,overall_results)

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "heat")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
heat <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "record", "heat")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
heat <- rbind(heat,overall_results)

# Time series recall ----
combine_validation_daily <- function(input_folder, year_range) {

  # input_folder <- here("data", "output", "05_validation", "recall", "day", "record", "excess_heat")
  # year_range <- 1996:2023
  
  message("Listing CSV files in folder: ", input_folder)
  csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
  message("Found ", length(csv_files), " CSV files.")
  
  if(length(csv_files) == 0) {
    stop("No CSV files found in ", input_folder)
  }
  
  # Extract a 4-digit year from each filename (e.g., "2000_validation_results.csv")
  extracted_years <- as.numeric(str_extract(basename(csv_files), "\\d{4}"))
  message("Extracted years from filenames: ", paste(extracted_years, collapse = ", "))
  
  # Subset to files whose extracted year is in the specified year_range
  valid_files <- csv_files[extracted_years %in% year_range]
  message("Using ", length(valid_files), " files for years: ", paste(year_range, collapse = ", "))
  
  if(length(valid_files) == 0) {
    stop("No CSV files found in ", input_folder, " for years: ", paste(year_range, collapse = ", "))
  }
  
  # Read each CSV file into a data frame
  df_list <- lapply(valid_files, function(f) {
    message("Reading file: ", f)
    read.csv(f, stringsAsFactors = FALSE)
  })
  
  # Combine all data frames into one
  combined_df <- bind_rows(df_list)
  message("Combined data has ", nrow(combined_df), " rows.")
  
  # Check that a Date column exists and convert it to Date format.
  if(!"Date" %in% names(combined_df)) {
    stop("The combined data does not have a 'Date' column.")
  }
  # combined_df$Date <- as.Date(combined_df$Date, format = "%d/%m/%Y")
  
  # Optionally, filter to only include dates within the specified year_range
  combined_df <- combined_df %>% filter(year(Date) %in% as.numeric(year_range))
  
  # Convert TP and FN to numeric in case they were read as characters.
  combined_df$TP <- as.numeric(combined_df$TP)
  combined_df$FN <- as.numeric(combined_df$FN)
  
  # Calculate recall for each row (if TP+FN > 0, else NA)
  combined_df <- combined_df %>% mutate(recall = ifelse((TP + FN) > 0, TP / (TP + FN), NA))
  
  message("Aggregation complete. Returning combined data with daily recall per row.")
  return(combined_df)
}

## Excess Heat ----
### 0.25 ----
input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "excess_heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
# Ensure the Date column is of class Date
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_excess_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  # geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### 0.39 ----
input_folder <- here("data", "output", "05_validation", "recall", "day", "record", "excess_heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
# Ensure the Date column is of class Date
daily_recall$Date <- as.Date(daily_recall$Date)

p_record_excess_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Heat ----
### 0.25 ----
input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
# Ensure the Date column is of class Date
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_heat  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  # geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### 0.39 ----
input_folder <- here("data", "output", "05_validation", "recall", "day", "record", "heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
# Ensure the Date column is of class Date
daily_recall$Date <- as.Date(daily_recall$Date)

p_record_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# final_plot <- ggpubr::ggarrange(p_stm1_heat,p_record_heat,
#                                 p_stm1_excess_heat,p_record_excess_heat,
#                                 nrow = 2,ncol = 2)

final_plot <- (p_stm1_heat + p_record_heat) / (p_stm1_excess_heat + p_record_excess_heat) +
  plot_annotation(tag_levels = "a")

# Define file paths using here
png_path <- here("figures","05_recall_time_series.png")
svg_path <- here("figures","05_recall_time_series.svg")

# Save the plot as a PNG file
ggsave(filename = png_path, plot = final_plot, width = 9, height = 6, dpi = 300)

# Save the plot as an SVG file
ggsave(filename = svg_path, plot = final_plot, width = 9, height = 6, device = "svg")
