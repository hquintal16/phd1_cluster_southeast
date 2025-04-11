#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 9 panel figures of noaa events, clusters, and recall by county

# leftoff calculating both 2019-2023 and 1996-2023 recall. update year range, 
# file extensions, save file names

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_05_noaa_cluster_recall.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# make_6panel_figure <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
#   # library(terra)
#   # library(sf)
#   # library(ggplot2)
#   # library(patchwork)
#   # library(maps)
#   # library(dplyr)
#   # library(here)
#   # library(pbapply)
#   # library(stringr)
#   # library(scales)
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
#   # Load a reference raster (for target CRS and extent)
#   region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
#                           "heat_index_daily_maximum_194001.nc"))
#   
#   # Load Koppen-Geiger raster and derive a binary resolution raster
#   kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
#                   "1991_2020", "koppen_geiger_0p1.tif")
#   resolution <- rast(kg_path)
#   resolution <- resolution / resolution
#   
#   # Load US county boundaries via maps and convert to an sf object and SpatVector
#   us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
#   us.states.vect <- vect(us.states)
#   us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
#   us.states.rast <- crop(us.states.rast, ext(region.crs))
#   
#   ### Helper function: Process files by summing layers over a year range and masking to counties.
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
#     if (length(files) == 0) {
#       warning("No files found in ", dir_path, " for years ", paste(yr_range, collapse = ", "))
#       return(NULL)
#     }
#     message("Reading ", length(files), " files from ", dir_path)
#     rast_obj <- rast(files)  # Each file becomes one layer.
#     message("Summing ", nlyr(rast_obj), " layers using sum()...")
#     summed <- sum(rast_obj)
#     message("Global sum after summing: ", global(summed, fun = "sum", na.rm = TRUE)[1])
#     masked <- mask(summed, us.states.rast, maskvalue = NA)
#     return(masked)
#   }
#   
#   ### Process NOAA data:
#   message("Processing NOAA data...")
#   noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
#   if (is.null(noaa_raster)) stop("NOAA raster not found!")
#   
#   ### Process Cluster directories:
#   message("Processing Cluster data...")
#   cluster_rasters <- lapply(names(cluster_dirs), function(name) {
#     dir_path <- cluster_dirs[[name]]
#     r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
#     if (is.null(r)) return(NULL)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(cluster_rasters) <- names(cluster_dirs)
#   
#   ### Process Recall files (direct file paths):
#   message("Processing Recall data...")
#   recall_rasters <- lapply(names(recall_dirs), function(name) {
#     file_path <- recall_dirs[[name]]
#     r <- rast(file_path)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(recall_rasters) <- names(recall_dirs)
#   
#   ### Determine separate common scale limits for NOAA and Cluster:
#   noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
#   message("NOAA range: 0 - ", noaa_max)
#   
#   valid_cluster <- cluster_rasters[c("stm1", "record")]
#   cluster_max <- as.numeric(max(unlist(sapply(valid_cluster, function(r) {
#     global(r, fun = "max", na.rm = TRUE)[1]
#   })), na.rm = TRUE))
#   message("Cluster range: 0 - ", cluster_max)
#   
#   ### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf().
#   ### Accepts an optional scale_limits argument, a palette argument, and na_color for NA values.
#   plot_raster <- function(r, scale_limits = NULL, 
#                           palette = c("grey40", "blue", "white", "red"), na_color = "white") {
#     df <- as.data.frame(r, xy = TRUE)
#     if (ncol(df) < 3) stop("Raster does not have expected columns.")
#     value_col <- names(df)[3]
#     if(is.null(scale_limits)) {
#       max_val <- max(df[[value_col]], na.rm = TRUE)
#       scale_limits <- c(0, max_val)
#     }
#     lower <- scale_limits[1]
#     upper <- scale_limits[2]
#     eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
#     
#     # Define breakpoints based on the length of the palette
#     if(length(palette) == 3) {
#       brks <- c(lower, lower + eps, upper)
#     } else if(length(palette) == 4) {
#       brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
#     } else {
#       brks <- seq(lower, upper, length.out = length(palette))
#     }
#     
#     p <- ggplot() +
#       geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#       scale_fill_gradientn(name = "",
#                            colors = palette,
#                            values = scales::rescale(brks, from = scale_limits),
#                            limits = scale_limits,
#                            na.value = na_color, oob = squish) +
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
#   # Top row: NOAA panels (2 panels) using red–white–blue with common limits and NA as white.
#   p_a <- plot_raster(noaa_raster, scale_limits = c(0, noaa_max), 
#                      palette = c("grey40", "blue", "white", "red"))
#   p_b <- plot_raster(noaa_raster, scale_limits = c(0, noaa_max), 
#                      palette = c("grey40", "blue", "white", "red"))
#   
#   # Middle row: Cluster panels (for stm1 and record) using red–white–blue with common limits.
#   p_d <- plot_raster(cluster_rasters[["stm1"]], scale_limits = c(0, cluster_max), 
#                      palette = c("grey40", "blue", "white", "red"))
#   p_e <- plot_raster(cluster_rasters[["record"]], scale_limits = c(0, cluster_max), 
#                      palette = c("grey40", "blue", "white", "red"))
#   
#   # Bottom row: Recall panels (for stm1 and record) using an orange to purple gradient
#   # and NA values colored as black.
#   p_g <- plot_raster(recall_rasters[["stm1"]], scale_limits = c(0, 1), 
#                      palette = c("orange", "purple"), na_color = "black")
#   p_h <- plot_raster(recall_rasters[["record"]], scale_limits = c(0, 1), 
#                      palette = c("orange", "purple"), na_color = "black")
#   
#   message("Assembling 6-panel figure using patchwork...")
#   library(patchwork)
#   figure <- (p_a + p_b) / (p_d + p_e) / (p_g + p_h) +
#     plot_annotation(tag_levels = "a")
#   
#   message("Figure creation complete.")
#   return(figure)
# }
# make_6panel_figure <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
#   # library(terra)
#   # library(sf)
#   # library(ggplot2)
#   # library(patchwork)
#   # library(maps)
#   # library(dplyr)
#   # library(here)
#   # library(pbapply)
#   # library(stringr)
#   # library(scales)
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
#   # Load a reference raster (for target CRS and extent)
#   region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
#                           "heat_index_daily_maximum_194001.nc"))
#   
#   # Load Koppen-Geiger raster and derive a binary resolution raster
#   kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
#                   "1991_2020", "koppen_geiger_0p1.tif")
#   resolution <- rast(kg_path)
#   resolution <- resolution / resolution
#   
#   # Load US county boundaries via maps and convert to an sf object and SpatVector
#   us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
#   us.states.vect <- vect(us.states)
#   us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
#   us.states.rast <- crop(us.states.rast, ext(region.crs))
#   
#   ### Helper function: Process files by summing layers over a year range and masking to counties.
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
#     if (length(files) == 0) {
#       warning("No files found in ", dir_path, " for years ", paste(yr_range, collapse = ", "))
#       return(NULL)
#     }
#     message("Reading ", length(files), " files from ", dir_path)
#     rast_obj <- rast(files)  # Each file becomes one layer.
#     message("Summing ", nlyr(rast_obj), " layers using sum()...")
#     summed <- sum(rast_obj)
#     message("Global sum after summing: ", global(summed, fun = "sum", na.rm = TRUE)[1])
#     masked <- mask(summed, us.states.rast, maskvalue = NA)
#     return(masked)
#   }
#   
#   ### Process NOAA data:
#   message("Processing NOAA data...")
#   noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
#   if (is.null(noaa_raster)) stop("NOAA raster not found!")
#   
#   ### Process Cluster directories:
#   message("Processing Cluster data...")
#   cluster_rasters <- lapply(names(cluster_dirs), function(name) {
#     dir_path <- cluster_dirs[[name]]
#     r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
#     if (is.null(r)) return(NULL)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(cluster_rasters) <- names(cluster_dirs)
#   
#   ### Process Recall files (direct file paths):
#   message("Processing Recall data...")
#   recall_rasters <- lapply(names(recall_dirs), function(name) {
#     file_path <- recall_dirs[[name]]
#     r <- rast(file_path)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(recall_rasters) <- names(recall_dirs)
#   
#   ### Determine common scale limits for NOAA and Cluster:
#   noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
#   message("NOAA max value: ", noaa_max)
#   
#   valid_cluster <- cluster_rasters[c("stm1", "record")]
#   cluster_max <- as.numeric(max(unlist(sapply(valid_cluster, function(r) {
#     global(r, fun = "max", na.rm = TRUE)[1]
#   })), na.rm = TRUE))
#   message("Cluster max value: ", cluster_max)
#   
#   # Set a common maximum for both NOAA and Cluster datasets.
#   common_max <- max(noaa_max, cluster_max)
#   message("Common NOAA and Cluster range: 0 - ", common_max)
#   
#   ### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf().
#   ### Accepts an optional scale_limits argument, a palette argument, and na_color for NA values.
#   plot_raster <- function(r, scale_limits = NULL, 
#                           palette = c("grey40", "blue", "white", "red"), na_color = "white") {
#     df <- as.data.frame(r, xy = TRUE)
#     if (ncol(df) < 3) stop("Raster does not have expected columns.")
#     value_col <- names(df)[3]
#     if(is.null(scale_limits)) {
#       max_val <- max(df[[value_col]], na.rm = TRUE)
#       scale_limits <- c(0, max_val)
#     }
#     lower <- scale_limits[1]
#     upper <- scale_limits[2]
#     eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
#     
#     # Define breakpoints based on the length of the palette.
#     # When using a 3-color palette (grey40, yellow, red), the breakpoints are set so that
#     # the value 0 maps exactly to grey40 and values just above 0 start at yellow.
#     if(length(palette) == 3) {
#       brks <- c(lower, lower + eps, upper)
#     } else if(length(palette) == 4) {
#       brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
#     } else {
#       brks <- seq(lower, upper, length.out = length(palette))
#     }
#     
#     p <- ggplot() +
#       geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#       scale_fill_gradientn(name = "",
#                            colors = palette,
#                            values = scales::rescale(brks, from = scale_limits),
#                            limits = scale_limits,
#                            na.value = na_color, oob = squish) +
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
#   # Top row: NOAA panels with a grey40 at 0 and a yellow-to-red ramp using common scale limits.
#   p_a <- plot_raster(noaa_raster, scale_limits = c(0, common_max), 
#                      palette = c("grey40", "yellow", "red"))
#   p_b <- plot_raster(noaa_raster, scale_limits = c(0, common_max), 
#                      palette = c("grey40", "yellow", "red"))
# 
#   # Middle row: Cluster panels (for stm1 and record) with the same palette and common limits.
#   p_d <- plot_raster(cluster_rasters[["stm1"]], scale_limits = c(0, common_max), 
#                      palette = c("grey40", "yellow", "red"))
#   p_e <- plot_raster(cluster_rasters[["record"]], scale_limits = c(0, common_max), 
#                      palette = c("grey40", "yellow", "red"))
#   
#   # Bottom row: Recall panels remain unchanged.
#   p_g <- plot_raster(recall_rasters[["stm1"]], scale_limits = c(0, 1), 
#                      palette = c("orange", "purple"))
#   p_h <- plot_raster(recall_rasters[["record"]], scale_limits = c(0, 1), 
#                      palette = c("orange", "purple"))
#   
#   message("Assembling 6-panel figure using patchwork...")
#   library(patchwork)
#   figure <- (p_a + p_b) / (p_d + p_e) / (p_g + p_h) +
#     plot_annotation(tag_levels = "a")
#   
#   message("Figure creation complete.")
#   return(figure)
# }
# 
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
  # library(scales)
  
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
  us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
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
    rast_obj <- rast(files)  # Each file becomes one layer.
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
  
  ### Determine common scale limits for NOAA and Cluster:
  noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
  message("NOAA max value: ", noaa_max)
  
  # Compute the cluster maximum using all available directories.
  cluster_max <- max(unlist(lapply(cluster_rasters, function(r) {
    if (!is.null(r)) global(r, fun = "max", na.rm=TRUE)[1] else NA
  })), na.rm = TRUE)
  message("Cluster max value: ", cluster_max)
  
  # Set a common maximum for both NOAA and Cluster datasets.
  common_max <- max(noaa_max, cluster_max)
  message("Common NOAA and Cluster range: 0 - ", common_max)
  
  ### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf().
  ### The theme is modified so that axis tick marks (and their labels) are shown.
  plot_raster <- function(r, scale_limits = NULL, 
                          palette = c("grey40", "blue", "white", "red"), na_color = "white") {
    df <- as.data.frame(r, xy = TRUE)
    if (ncol(df) < 3) stop("Raster does not have expected columns.")
    value_col <- names(df)[3]
    if(is.null(scale_limits)) {
      max_val <- max(df[[value_col]], na.rm = TRUE)
      scale_limits <- c(0, max_val)
    }
    lower <- scale_limits[1]
    upper <- scale_limits[2]
    eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
    
    # Define breakpoints based on the length of the palette.
    if(length(palette) == 3) {
      brks <- c(lower, lower + eps, upper)
    } else if(length(palette) == 4) {
      brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
    } else {
      brks <- seq(lower, upper, length.out = length(palette))
    }
    
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
      scale_fill_gradientn(name = "",
                           colors = palette,
                           values = scales::rescale(brks, from = scale_limits),
                           limits = scale_limits,
                           na.value = na_color, oob = squish) +
      geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
      coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            # Removing the removal of axis texts and ticks so that lat-long labels appear.
            plot.title = element_blank())
    return(p)
  }
  
  ### Create panels:
  message("Creating panels...")
  # Top row: NOAA panels (the extra panel is a duplicate of the NOAA panel, adjust if needed).
  p_a <- plot_raster(noaa_raster, scale_limits = c(0, common_max), 
                     palette = c("grey40", "yellow", "red"))
  p_b <- plot_raster(noaa_raster, scale_limits = c(0, common_max), 
                     palette = c("grey40", "yellow", "red"))
  p_c <- plot_raster(noaa_raster, scale_limits = c(0, common_max), 
                     palette = c("grey40", "yellow", "red"))
  
  # Middle row: Cluster panels from three directories (e.g., "stm1", "record", and "extra").
  p_d <- plot_raster(cluster_rasters[[1]], scale_limits = c(0, common_max), 
                     palette = c("grey40", "yellow", "red"))
  p_e <- plot_raster(cluster_rasters[[2]], scale_limits = c(0, common_max), 
                     palette = c("grey40", "yellow", "red"))
  p_f <- plot_raster(cluster_rasters[[3]], scale_limits = c(0, common_max), 
                     palette = c("grey40", "yellow", "red"))
  
  # Bottom row: Recall panels from three directories (e.g., "stm1", "record", and "extra").
  p_g <- plot_raster(recall_rasters[[1]], scale_limits = c(0, 1), 
                     palette = c("orange", "purple"))
  p_h <- plot_raster(recall_rasters[[2]], scale_limits = c(0, 1), 
                     palette = c("orange", "purple"))
  p_i <- plot_raster(recall_rasters[[3]], scale_limits = c(0, 1), 
                     palette = c("orange", "purple"))
  
  message("Assembling 9-panel figure using patchwork...")
  library(patchwork)
  top_row <- p_a + p_b + p_c
  middle_row <- p_d + p_e + p_f
  bottom_row <- p_g + p_h + p_i
  figure <- top_row / middle_row / bottom_row +
    plot_annotation(tag_levels = "a")
  
  message("Figure creation complete.")
  return(figure)
}

make_9panel_figure_binned <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
  # library(terra)
  # library(sf)
  # library(ggplot2)
  # library(patchwork)
  # library(maps)
  # library(dplyr)
  # library(here)
  # library(pbapply)
  # library(stringr)
  # library(scales)
  
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
  # Load reference raster (for target CRS and extent)
  region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
                          "heat_index_daily_maximum_194001.nc"))
  
  # Load Koppen-Geiger raster and derive a binary resolution raster
  kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
                  "1991_2020", "koppen_geiger_0p1.tif")
  resolution <- rast(kg_path)
  resolution <- resolution / resolution
  
  # Load US county boundaries (maps -> sf -> SpatVector)
  us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  us.states.vect <- vect(us.states)
  us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
  us.states.rast <- crop(us.states.rast, ext(region.crs))
  
  ### Helper: Sum layers over a year range and mask to counties.
  process_sum_mask <- function(dir_path, file_prefix, yr_range, required_substring = "") {
    if (required_substring != "") {
      files <- list.files(dir_path, pattern = required_substring, full.names = TRUE)
      message("Found ", length(files), " files in ", dir_path, " containing '", required_substring, "'.")
      if (length(files) > 0) {
        extracted_years <- as.numeric(str_extract(basename(files), "\\d{4}"))
        message("Extracted years: ", paste(extracted_years, collapse = ", "))
        files <- files[extracted_years %in% yr_range]
        message("After filtering, ", length(files), " files remain.")
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
    rast_obj <- rast(files)  # Each file becomes a layer.
    message("Summing ", nlyr(rast_obj), " layers using sum()...")
    summed <- sum(rast_obj)
    message("Global sum: ", global(summed, fun = "sum", na.rm = TRUE)[1])
    masked <- mask(summed, us.states.rast, maskvalue = NA)
    return(masked)
  }
  
  ### Process data.
  message("Processing NOAA data...")
  noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
  if (is.null(noaa_raster)) stop("NOAA raster not found!")
  
  message("Processing Cluster data...")
  cluster_rasters <- lapply(names(cluster_dirs), function(name) {
    dir_path <- cluster_dirs[[name]]
    r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
    if (is.null(r)) return(NULL)
    mask(r, us.states.rast, maskvalue = NA)
  })
  names(cluster_rasters) <- names(cluster_dirs)
  
  message("Processing Recall data...")
  recall_rasters <- lapply(names(recall_dirs), function(name) {
    file_path <- recall_dirs[[name]]
    r <- rast(file_path)
    mask(r, us.states.rast, maskvalue = NA)
  })
  names(recall_rasters) <- names(recall_dirs)
  
  ### Determine common scale limits.
  noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
  message("NOAA max value: ", noaa_max)
  cluster_max <- max(unlist(lapply(cluster_rasters, function(r) {
    if (!is.null(r)) global(r, fun = "max", na.rm = TRUE)[1] else NA
  })), na.rm = TRUE)
  message("Cluster max value: ", cluster_max)
  common_max <- max(noaa_max, cluster_max)
  message("Common NOAA and Cluster range: 0 - ", common_max)
  
  ### Define equal-interval bin breaks for nonzero values (for NOAA/Cluster).
  vals_noaa <- as.vector(noaa_raster[])
  vals_cluster <- unlist(lapply(cluster_rasters, function(r) as.vector(r[])))
  all_vals <- c(vals_noaa, vals_cluster)
  all_vals <- all_vals[!is.na(all_vals)]
  nonzero_all <- all_vals[all_vals > 0]
  if(length(nonzero_all) == 0) stop("No nonzero values found for binning.")
  bmin <- min(nonzero_all)
  bmax <- common_max
  # Create 7 total bins: one for 0 and 6 equal-interval bins for nonzero values.
  bin_breaks <- seq(bmin, bmax, length.out = 7)
  message("Bin breakpoints (nonzero): ", paste(round(bin_breaks, 2), collapse = ", "))
  
  # For Recall panels, use fixed breaks every 0.2 from 0 to 1.
  recall_breaks <- seq(0, 1, by = 0.2)
  
  ### Plotting helper.
  # Added a parameter zero_special (default TRUE). When TRUE, values of 0 get a special category ("0").
  # When FALSE, no special treatment is applied.
  plot_raster <- function(r, scale_limits = NULL, palette = NULL, na_color = "white",
                          bin_breaks = NULL, zero_special = TRUE) {
    df <- as.data.frame(r, xy = TRUE)
    if (ncol(df) < 3) stop("Raster does not have expected columns.")
    value_col <- names(df)[3]
    
    if (!is.null(bin_breaks)) {
      if (zero_special) {
        # For NOAA/Cluster: treat 0 specially.
        nz_values <- df[[value_col]][df[[value_col]] > 0]
        if (length(nz_values) > 0) {
          nz_bins <- cut(nz_values, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
          levels_cut <- levels(nz_bins)
        } else {
          levels_cut <- character(0)
        }
        df$bin <- ifelse(df[[value_col]] == 0, "0",
                         as.character(cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)))
        bin_levels <- c("0", levels_cut)
        df$bin <- factor(df$bin, levels = bin_levels)
        if (is.null(palette)) {
          custom_palette <- c("grey40", colorRampPalette(c("yellow", "red"))(length(levels_cut)))
        } else {
          custom_palette <- palette
        }
      } else {
        # For Recall: simply bin using the breaks (do not separate 0).
        df$bin <- cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
        bin_levels <- levels(df$bin)
        if (is.null(palette)) {
          custom_palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
        } else {
          custom_palette <- palette
        }
      }
      
      p <- ggplot() +
        geom_tile(data = df, aes(x = x, y = y, fill = bin)) +
        scale_fill_manual(name = "", values = custom_palette, na.value = na_color) +
        geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
        coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              plot.title = element_blank(),
              legend.position = "none") +
        guides(fill = "none")
    } else {
      # For continuous panels (not used here).
      if (is.null(scale_limits)) {
        max_val <- max(df[[value_col]], na.rm = TRUE)
        scale_limits <- c(0, max_val)
      }
      lower <- scale_limits[1]
      upper <- scale_limits[2]
      eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
      if (is.null(palette)) palette <- c("orange", "purple")
      if(length(palette) == 3) {
        brks <- c(lower, lower + eps, upper)
      } else if(length(palette) == 4) {
        brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
      } else {
        brks <- seq(lower, upper, length.out = length(palette))
      }
      p <- ggplot() +
        geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
        scale_fill_gradientn(name = "",
                             colors = palette,
                             values = scales::rescale(brks, from = scale_limits),
                             limits = scale_limits,
                             na.value = na_color, oob = squish) +
        geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
        coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              plot.title = element_blank(),
              legend.position = "none") +
        guides(fill = "none")
    }
    return(p)
  }
  
  ### Custom legend for binned scale (for NOAA/Cluster).
  create_binned_legend <- function(bin_breaks) {
    dummy <- seq(min(bin_breaks), max(bin_breaks), length.out = 100)
    dummy_bins <- cut(dummy, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
    levels_cut <- levels(dummy_bins)
    bin_levels <- c("0", levels_cut)
    custom_palette <- c("grey40", colorRampPalette(c("yellow", "red"))(length(levels_cut)))
    legend_df <- data.frame(bin = factor(bin_levels, levels = bin_levels),
                            x = 1,
                            y = seq_along(bin_levels))
    legend_plot <- ggplot(legend_df, aes(x = x, y = y, fill = bin)) +
      geom_tile(width = 0.9, height = 0.9) +
      scale_fill_manual(name = "Days", values = custom_palette) +
      scale_y_continuous(breaks = seq_along(bin_levels), labels = bin_levels) +
      coord_fixed(xlim = c(0, 1), ylim = c(0, length(bin_levels))) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(hjust = 1),
            panel.grid = element_blank(),
            legend.position = "none") +
      ggtitle("Days") +
      theme(plot.title = element_text(hjust = 1))
    return(legend_plot)
  }
  
  ### Custom binned legend for Recall.
  create_binned_legend_recall <- function(recall_breaks) {
    dummy <- seq(min(recall_breaks), max(recall_breaks), length.out = 100)
    dummy_bins <- cut(dummy, breaks = recall_breaks, include.lowest = TRUE, right = FALSE)
    bin_levels <- levels(dummy_bins)  # No extra "0" category.
    custom_palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
    legend_df <- data.frame(bin = factor(bin_levels, levels = bin_levels),
                            x = 1,
                            y = seq_along(bin_levels))
    legend_plot <- ggplot(legend_df, aes(x = x, y = y, fill = bin)) +
      geom_tile(width = 0.9, height = 0.9) +
      scale_fill_manual(name = "Recall", values = custom_palette) +
      scale_y_continuous(breaks = seq_along(bin_levels), labels = bin_levels) +
      coord_fixed(xlim = c(0, 1), ylim = c(0, length(bin_levels))) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(hjust = 1),
            panel.grid = element_blank(),
            legend.position = "none") +
      ggtitle("Recall") +
      theme(plot.title = element_text(hjust = 1))
    return(legend_plot)
  }
  
  ### Create individual data panels.
  message("Creating panels...")
  # NOAA panels (binned, with zero_special = TRUE)
  p_a <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
  p_b <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
  p_c <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
  
  # Cluster panels (binned, with zero_special = TRUE)
  p_d <- plot_raster(cluster_rasters[[1]], bin_breaks = bin_breaks, zero_special = TRUE)
  p_e <- plot_raster(cluster_rasters[[2]], bin_breaks = bin_breaks, zero_special = TRUE)
  p_f <- plot_raster(cluster_rasters[[3]], bin_breaks = bin_breaks, zero_special = TRUE)
  
  # Recall panels (binned, with fixed breaks recall_breaks, and zero_special = FALSE)
  p_g <- plot_raster(recall_rasters[[1]], bin_breaks = recall_breaks, zero_special = FALSE)
  p_h <- plot_raster(recall_rasters[[2]], bin_breaks = recall_breaks, zero_special = FALSE)
  p_i <- plot_raster(recall_rasters[[3]], bin_breaks = recall_breaks, zero_special = FALSE)
  
  # Create legend panels.
  legend_binned <- create_binned_legend(bin_breaks)
  legend_recall <- create_binned_legend_recall(recall_breaks)
  
  ### Assemble rows with one extra legend column.
  library(patchwork)
  row1 <- wrap_plots(list(p_d, p_e, p_f, legend_binned), ncol = 4) +
    plot_layout(widths = c(1, 1, 1, 1))
  row2 <- wrap_plots(list(p_a, p_b, p_c, legend_binned), ncol = 4) +
    plot_layout(widths = c(1, 1, 1, 1))
  row3 <- wrap_plots(list(p_g, p_h, p_i, legend_recall), ncol = 4) +
    plot_layout(widths = c(1, 1, 1, 1))
  
  figure <- row1 / row2 / row3 + plot_annotation(tag_levels = "a")
  
  message("Figure creation complete.")
  return(figure)
}

## Excess Heat ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")

# Cluster directories: a named list of three directories
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "0.39", "heat_index", "summary")
)

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_0.39_excess_heat.nc")
)

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
# figure <- make_9panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
# png_path <- here("figures","05_recall_excess_heat_day_2010_2023.png")
# svg_path <- here("figures","05_recall_excess_heat_day_2010_2023.svg")
png_path <- here("figures","05_recall_excess_heat_day_binned_2019_2023.png")
svg_path <- here("figures","05_recall_excess_heat_day_binned_2019_2023.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

# Define the year range
year_range <- 2010:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")

# Cluster directories: a named list of three directories
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "0.39", "heat_index", "summary")
)

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall", "raster", "2010_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall", "raster", "2010_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall", "raster", "2010_2023_day_0.39_excess_heat.nc")
)

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
# figure <- make_9panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
# png_path <- here("figures","05_recall_excess_heat_day_2010_2023.png")
# svg_path <- here("figures","05_recall_excess_heat_day_2010_2023.svg")
png_path <- here("figures","05_recall_excess_heat_day_binned_2010_2023.png")
svg_path <- here("figures","05_recall_excess_heat_day_binned_2010_2023.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

# Define the year range
year_range <- 2000:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")

# Cluster directories: a named list of three directories
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "0.39", "heat_index", "summary")
)

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall", "raster", "2000_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall", "raster", "2000_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall", "raster", "2000_2023_day_0.39_excess_heat.nc")
)

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
# figure <- make_9panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
# png_path <- here("figures","05_recall_excess_heat_day_2010_2023.png")
# svg_path <- here("figures","05_recall_excess_heat_day_2010_2023.svg")
png_path <- here("figures","05_recall_excess_heat_day_binned_2000_2023.png")
svg_path <- here("figures","05_recall_excess_heat_day_binned_2000_2023.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

# # Define the year range
# year_range <- 1996:2023
# 
# # NOAA directory: example path
# noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")
# 
# # Cluster directories: a named list of three directories
# cluster_dirs <- list(
#   stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "heat_index", "summary"),
#   record = here("data", "output", "03_cluster", "02_cluster", "points", "record", "heat_index", "summary"),
#   stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "stm4", "heat_index", "summary")
# )
# 
# # Recall directories: a named list of three file paths to .nc files
# recall_dirs <- list(
#   stm1 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_excess_heat.nc"),
#   record = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_record_excess_heat.nc"),
#   stm4 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm4_excess_heat.nc")
# )
# 
# # Output directory for the figure
# output_directory <- here("figures")
# 
# # Create the 9-panel figure
# figure <- make_6panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)
# 
# # Define file paths using here
# png_path <- here("figures","05_recall_excess_heat_day_1996_2023.png")
# svg_path <- here("figures","05_recall_excess_heat_day_1996_2023.svg")
# 
# # Save the plot as a PNG + SVG file
# ggsave(filename = png_path, plot = figure, width = 6, height = 6, dpi = 300)
# ggsave(filename = svg_path, plot = figure, width = 6, height = 6, device = "svg")

## Heat ----

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
figure <- make_6panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_heat_day_2019_2023.png")
svg_path <- here("figures","05_recall_heat_day_2019_2023.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 6, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 6, height = 6, device = "svg")

# # Define the year range
# year_range <- 2019:2023
# 
# # NOAA directory: example path
# noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "heat")
# 
# # Cluster directories: a named list of three directories
# cluster_dirs <- list(
#   stm1 = here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "heat_index", "summary"),
#   record = here("data", "output", "03_cluster", "02_cluster", "points", "record", "heat_index", "summary"),
#   stm4 = here("data", "output", "03_cluster", "02_cluster", "points", "stm4", "heat_index", "summary")
# )
# 
# # Recall directories: a named list of three file paths to .nc files
# recall_dirs <- list(
#   stm1 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_heat.nc"),
#   record = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_record_heat.nc"),
#   stm4 = here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm4_heat.nc")
# )
# 
# # Output directory for the figure
# output_directory <- here("figures")
# 
# # Create the 9-panel figure
# figure <- make_6panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)
# 
# # Define file paths using here
# png_path <- here("figures","05_recall_heat_day_1996_2023.png")
# svg_path <- here("figures","05_recall_heat_day_1996_2023.svg")
# 
# # Save the plot as a PNG + SVG file
# ggsave(filename = png_path, plot = figure, width = 6, height = 6, dpi = 300)
# ggsave(filename = svg_path, plot = figure, width = 6, height = 6, device = "svg")

make_3panel_figure <- function(noaa_dir, cluster_dir, recall_file, year_range, output_dir) {
  # library(terra)
  # library(sf)
  # library(ggplot2)
  # library(patchwork)
  # library(maps)
  # library(dplyr)
  # library(here)
  # library(scales)
  
  message("DEBUG: noaa_dir: ", noaa_dir)
  message("DEBUG: cluster_dir: ", cluster_dir)
  message("DEBUG: recall_file: ", recall_file)
  message("DEBUG: output_dir: ", output_dir)
  
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
  us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
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
    rast_obj <- rast(files)
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
  
  ### Process Cluster data:
  message("Processing Cluster data...")
  cluster_raster <- process_sum_mask(cluster_dir, "County_heat_index_", year_range, required_substring = "Daily_Extent")
  if (is.null(cluster_raster)) stop("Cluster raster not found!")
  
  ### Process Recall data:
  message("Processing Recall data...")
  recall_raster <- rast(recall_file)
  recall_raster <- mask(recall_raster, us.states.rast, maskvalue = NA)
  
  ### Determine common scale limits for NOAA and Cluster:
  noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
  message("NOAA range: 0 - ", noaa_max)
  
  cluster_max <- as.numeric(global(cluster_raster, fun = "max", na.rm = TRUE)[1])
  message("Cluster range: 0 - ", cluster_max)
  
  ### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf().
  plot_raster <- function(r, scale_limits = NULL, 
                          palette = c("grey40", "blue", "white", "red"), na_color = "white") {
    df <- as.data.frame(r, xy = TRUE)
    if (ncol(df) < 3) stop("Raster does not have expected columns.")
    value_col <- names(df)[3]
    if(is.null(scale_limits)) {
      max_val <- max(df[[value_col]], na.rm = TRUE)
      scale_limits <- c(0, max_val)
    }
    lower <- scale_limits[1]
    upper <- scale_limits[2]
    eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
    
    if(length(palette) == 3) {
      brks <- c(lower, lower + eps, upper)
    } else if(length(palette) == 4) {
      brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
    } else {
      brks <- seq(lower, upper, length.out = length(palette))
    }
    
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
      scale_fill_gradientn(name = "",
                           colors = palette,
                           values = scales::rescale(brks, from = scale_limits),
                           limits = scale_limits,
                           na.value = na_color, oob = squish) +
      geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
      coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_blank())
    return(p)
  }
  
  ### Create individual panels:
  message("Creating panels...")
  # NOAA panel using a red–white–blue gradient.
  p_noaa <- plot_raster(noaa_raster, scale_limits = c(0, noaa_max), 
                        palette = c("grey40", "blue", "white", "red"))
  
  # Cluster panel using a red–white–blue gradient.
  p_cluster <- plot_raster(cluster_raster, scale_limits = c(0, cluster_max), 
                           palette = c("grey40", "blue", "white", "red"))
  
  # Recall panel using an orange-to-purple gradient with NA values in black.
  p_recall <- plot_raster(recall_raster, scale_limits = c(0, 1), 
                          palette = c("orange", "purple"), na_color = "black")
  
  message("Assembling 3-panel figure using patchwork...")
  library(patchwork)
  # Arrange the three panels vertically (one plot per panel)
  figure <- p_noaa + p_cluster + p_recall + plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "a")
  
  message("Figure creation complete.")
  return(figure)
}


## Flash Flood ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "flash_flood")

# Cluster directories: a named list of three directories
cluster_dirs <- here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "precipitation", "summary")

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_flash_flood.nc")

# Output directory for the figure
output_directory <- here("figures")

# Create the 3-panel figure
figure <- make_3panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_flash_flood_day.png")
svg_path <- here("figures","05_recall_flash_flood_day.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 3, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 3, height = 6, device = "svg")

## Flood ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "flood")

# Cluster directories: a named list of three directories
cluster_dirs <- here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "precipitation", "summary")

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_flood.nc")

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_3panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_flood_day.png")
svg_path <- here("figures","05_recall_flood_day.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 3, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 3, height = 6, device = "svg")

## Heavy Rain ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "heavy_rain")

# Cluster directories: a named list of three directories
cluster_dirs <- here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "precipitation", "summary")

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_heavy_rain.nc")

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_3panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_heavy_rain_day.png")
svg_path <- here("figures","05_recall_heavy_rain_day.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 3, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 3, height = 6, device = "svg")

## Hurricanes ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "hurricane")

# Cluster directories: a named list of three directories
cluster_dirs <- here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "precipitation", "summary")

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_hurricane.nc")

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_3panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_hurricane_day.png")
svg_path <- here("figures","05_recall_hurricane_day.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 3, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 3, height = 6, device = "svg")

## Tropical Depression ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "tropical_depression")

# Cluster directories: a named list of three directories
cluster_dirs <- here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "precipitation", "summary")

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_tropical_depression.nc")

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_3panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_tropical_depression_day.png")
svg_path <- here("figures","05_recall_tropical_depression_day.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 3, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 3, height = 6, device = "svg")

## Tropical Storm ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "tropical_storm")

# Cluster directories: a named list of three directories
cluster_dirs <- here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "precipitation", "summary")

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_tropical_storm.nc")

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_3panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_tropical_storm_day.png")
svg_path <- here("figures","05_recall_tropical_storm_day.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 3, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 3, height = 6, device = "svg")

## Typhoon ----

# Define the year range
year_range <- 2019:2023

# NOAA directory: example path
noaa_directory <- here("data", "output", "05_noaa", "southeast", "summary", "typhoon")

# Cluster directories: a named list of three directories
cluster_dirs <- here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "precipitation", "summary")

# Recall directories: a named list of three file paths to .nc files
recall_dirs <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_stm1_typhoon.nc")

# Output directory for the figure
output_directory <- here("figures")

# Create the 9-panel figure
figure <- make_3panel_figure(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Define file paths using here
png_path <- here("figures","05_recall_typhoon_day.png")
svg_path <- here("figures","05_recall_typhoon_day.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = figure, width = 3, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 3, height = 6, device = "svg")

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

## 0.25 ----

input_folder <- here("data", "output", "05_validation", "recall", "day", "0.25", "excess_heat")
year_range <- 2000:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
excess_heat <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "flash_flood")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
flash_flood <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "flood")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
flood <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "heat")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
heat <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "heavy_rain")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
heavy_rain <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "hurricane")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
hurricane <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "tropical_depression")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
tropical_depression <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "tropical_storm")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
tropical_storm <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "stm1", "typhoon")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
typhoon <- overall_results

agg_recall <- data.frame(stm1 = c(excess_heat$recall,flash_flood$recall,flood$recall,heat$recall,heavy_rain$recall,
                                  hurricane$recall,tropical_depression$recall,tropical_storm$recall))

## 0.3075 ----

input_folder <- here("data", "output", "05_validation", "recall", "day", "0.3075", "excess_heat")
year_range <- 2000:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
excess_heat <- overall_results

## 0.39 ----

input_folder <- here("data", "output", "05_validation", "recall", "day", "0.39", "excess_heat")
year_range <- 2000:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
excess_heat <- overall_results

input_folder <- here("data", "output", "05_validation", "recall", "day", "record", "heat")
year_range <- 2019:2023
overall_results <- aggregate_validation_csv_overall(input_folder, year_range)
heat <- overall_results

agg_recall$record <- c(excess_heat$recall,NA,NA,heat$recall,NA,NA,NA,NA)
row.names(agg_recall) <- c('Excess Heat','Flash Flood','Flood','Heat','Heavy Rain','Hurricane','Tropical Depression','Tropical Storm')

# save
write.csv(agg_recall,here('data','output','05_validation','recall','2019_2023_recall.csv'))

# Key Figure ----
# Load a reference raster (for target CRS and extent)
region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
                        "heat_index_daily_maximum_194001.nc"))

# Load Koppen-Geiger raster and derive a binary resolution raster
kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
                "1991_2020", "koppen_geiger_0p1.tif")
resolution <- rast(kg_path)
resolution <- resolution / resolution

# Load US county boundaries via maps and convert to an sf object and SpatVector
us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
us.states.vect <- vect(us.states)
us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
us.states.rast <- crop(us.states.rast, ext(region.crs))
file_path <- here("data", "output", "05_validation", "recall", "raster", "2019_2023_day_record_excess_heat.nc")
### Process Recall files (direct file paths):
r <- rast(file_path)
recall_rasters <- mask(r, us.states.rast, maskvalue = NA)
# plot(recall_rasters)
### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf().
### Accepts an optional scale_limits argument, a palette argument, and na_color for NA values.
plot_raster <- function(r, scale_limits = NULL, 
                        palette = c("grey40", "blue", "white", "red"), na_color = "white") {
  df <- as.data.frame(r, xy = TRUE)
  if (ncol(df) < 3) stop("Raster does not have expected columns.")
  value_col <- names(df)[3]
  if(is.null(scale_limits)) {
    max_val <- max(df[[value_col]], na.rm = TRUE)
    scale_limits <- c(0, max_val)
  }
  lower <- scale_limits[1]
  upper <- scale_limits[2]
  eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
  
  # Define breakpoints based on the length of the palette
  if(length(palette) == 3) {
    brks <- c(lower, lower + eps, upper)
  } else if(length(palette) == 4) {
    brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
  } else {
    brks <- seq(lower, upper, length.out = length(palette))
  }
  
  p <- ggplot() +
    geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
    scale_fill_gradientn(name = "",
                         colors = palette,
                         values = scales::rescale(brks, from = scale_limits),
                         limits = scale_limits,
                         na.value = na_color, oob = squish) +
    geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
    coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = F) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_blank(),
          legend.position = c(.9,.25))
  return(p)
}
key_figure <- plot_raster(recall_rasters, scale_limits = c(0, 1), 
                   palette = c("orange", "purple"), na_color = "black")
key_figure

# Define file paths using here
png_path <- here("figures","00_key_figure.png")
svg_path <- here("figures","00_key_figure.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = key_figure, width = 4, height = 4, dpi = 300)
ggsave(filename = svg_path, plot = key_figure, width = 4, height = 4, device = "svg")
