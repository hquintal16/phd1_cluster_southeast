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

# 1. Spatial recall ----

# make_9panel_figure_binned <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
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
#   # Load reference raster (for target CRS and extent)
#   region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
#                           "heat_index_daily_maximum_194001.nc"))
#   
#   # Load Koppen-Geiger raster and derive a binary resolution raster
#   kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
#                   "1991_2020", "koppen_geiger_0p1.tif")
#   resolution <- rast(kg_path)
#   resolution <- resolution / resolution
#   
#   # Load US county boundaries (maps -> sf -> SpatVector)
#   us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
#   us.states.vect <- vect(us.states)
#   us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
#   us.states.rast <- crop(us.states.rast, ext(region.crs))
#   
#   ### Helper: Sum layers over a year range and mask to counties.
#   process_sum_mask <- function(dir_path, file_prefix, yr_range, required_substring = "") {
#     if (required_substring != "") {
#       files <- list.files(dir_path, pattern = required_substring, full.names = TRUE)
#       message("Found ", length(files), " files in ", dir_path, " containing '", required_substring, "'.")
#       if (length(files) > 0) {
#         extracted_years <- as.numeric(str_extract(basename(files), "\\d{4}"))
#         message("Extracted years: ", paste(extracted_years, collapse = ", "))
#         files <- files[extracted_years %in% yr_range]
#         message("After filtering, ", length(files), " files remain.")
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
#     rast_obj <- rast(files)  # Each file becomes a layer.
#     message("Summing ", nlyr(rast_obj), " layers using sum()...")
#     summed <- sum(rast_obj)
#     message("Global sum: ", global(summed, fun = "sum", na.rm = TRUE)[1])
#     masked <- mask(summed, us.states.rast, maskvalue = NA)
#     return(masked)
#   }
#   
#   ### Process data.
#   message("Processing NOAA data...")
#   noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
#   if (is.null(noaa_raster)) stop("NOAA raster not found!")
#   
#   message("Processing Cluster data...")
#   cluster_rasters <- lapply(names(cluster_dirs), function(name) {
#     dir_path <- cluster_dirs[[name]]
#     r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
#     if (is.null(r)) return(NULL)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(cluster_rasters) <- names(cluster_dirs)
#   
#   message("Processing Recall data...")
#   recall_rasters <- lapply(names(recall_dirs), function(name) {
#     file_path <- recall_dirs[[name]]
#     r <- rast(file_path)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(recall_rasters) <- names(recall_dirs)
#   
#   ### Determine common scale limits.
#   noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
#   message("NOAA max value: ", noaa_max)
#   cluster_max <- max(unlist(lapply(cluster_rasters, function(r) {
#     if (!is.null(r)) global(r, fun = "max", na.rm = TRUE)[1] else NA
#   })), na.rm = TRUE)
#   message("Cluster max value: ", cluster_max)
#   common_max <- max(noaa_max, cluster_max)
#   message("Common NOAA and Cluster range: 0 - ", common_max)
#   
#   ### Define equal-interval bin breaks for nonzero values (for NOAA/Cluster).
#   vals_noaa <- as.vector(noaa_raster[])
#   vals_cluster <- unlist(lapply(cluster_rasters, function(r) as.vector(r[])))
#   all_vals <- c(vals_noaa, vals_cluster)
#   all_vals <- all_vals[!is.na(all_vals)]
#   nonzero_all <- all_vals[all_vals > 0]
#   if(length(nonzero_all) == 0) stop("No nonzero values found for binning.")
#   bmin <- min(nonzero_all)
#   bmax <- common_max
#   # Create 7 total bins: one for 0 and 6 equal-interval bins for nonzero values.
#   bin_breaks <- seq(bmin, bmax, length.out = 7)
#   message("Bin breakpoints (nonzero): ", paste(round(bin_breaks, 2), collapse = ", "))
#   
#   # For Recall panels, use fixed breaks every 0.2 from 0 to 1.
#   recall_breaks <- seq(0, 1, by = 0.2)
#   
#   ### Plotting helper.
#   # Added a parameter zero_special (default TRUE). When TRUE, values of 0 get a special category ("0").
#   # When FALSE, no special treatment is applied.
#   plot_raster <- function(r, scale_limits = NULL, palette = NULL, na_color = "white",
#                           bin_breaks = NULL, zero_special = TRUE) {
#     df <- as.data.frame(r, xy = TRUE)
#     if (ncol(df) < 3) stop("Raster does not have expected columns.")
#     value_col <- names(df)[3]
#     
#     if (!is.null(bin_breaks)) {
#       if (zero_special) {
#         # For NOAA/Cluster: treat 0 specially.
#         nz_values <- df[[value_col]][df[[value_col]] > 0]
#         if (length(nz_values) > 0) {
#           nz_bins <- cut(nz_values, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#           levels_cut <- levels(nz_bins)
#         } else {
#           levels_cut <- character(0)
#         }
#         df$bin <- ifelse(df[[value_col]] == 0, "0",
#                          as.character(cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)))
#         bin_levels <- c("0", levels_cut)
#         df$bin <- factor(df$bin, levels = bin_levels)
#         if (is.null(palette)) {
#           custom_palette <- c("grey40", colorRampPalette(c("yellow", "red"))(length(levels_cut)))
#         } else {
#           custom_palette <- palette
#         }
#       } else {
#         # For Recall: simply bin using the breaks (do not separate 0).
#         df$bin <- cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#         bin_levels <- levels(df$bin)
#         if (is.null(palette)) {
#           custom_palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
#         } else {
#           custom_palette <- palette
#         }
#       }
#       
#       p <- ggplot() +
#         geom_tile(data = df, aes(x = x, y = y, fill = bin)) +
#         scale_fill_manual(name = "", values = custom_palette, na.value = na_color) +
#         geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#         coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#         theme_minimal() +
#         theme(axis.title = element_blank(),
#               plot.title = element_blank(),
#               legend.position = "none") +
#         guides(fill = "none")
#     } else {
#       # For continuous panels (not used here).
#       if (is.null(scale_limits)) {
#         max_val <- max(df[[value_col]], na.rm = TRUE)
#         scale_limits <- c(0, max_val)
#       }
#       lower <- scale_limits[1]
#       upper <- scale_limits[2]
#       eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
#       if (is.null(palette)) palette <- c("orange", "purple")
#       if(length(palette) == 3) {
#         brks <- c(lower, lower + eps, upper)
#       } else if(length(palette) == 4) {
#         brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
#       } else {
#         brks <- seq(lower, upper, length.out = length(palette))
#       }
#       p <- ggplot() +
#         geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#         scale_fill_gradientn(name = "",
#                              colors = palette,
#                              values = scales::rescale(brks, from = scale_limits),
#                              limits = scale_limits,
#                              na.value = na_color, oob = squish) +
#         geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#         coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#         theme_minimal() +
#         theme(axis.title = element_blank(),
#               plot.title = element_blank(),
#               legend.position = "none") +
#         guides(fill = "none")
#     }
#     return(p)
#   }
#   
#   ### Custom legend for binned scale (for NOAA/Cluster).
#   create_binned_legend <- function(bin_breaks) {
#     dummy <- seq(min(bin_breaks), max(bin_breaks), length.out = 100)
#     dummy_bins <- cut(dummy, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#     levels_cut <- levels(dummy_bins)
#     bin_levels <- c("0", levels_cut)
#     custom_palette <- c("grey40", colorRampPalette(c("yellow", "red"))(length(levels_cut)))
#     legend_df <- data.frame(bin = factor(bin_levels, levels = bin_levels),
#                             x = 1,
#                             y = seq_along(bin_levels))
#     legend_plot <- ggplot(legend_df, aes(x = x, y = y, fill = bin)) +
#       geom_tile(width = 0.9, height = 0.9) +
#       scale_fill_manual(name = "Days", values = custom_palette) +
#       scale_y_continuous(breaks = seq_along(bin_levels), labels = bin_levels) +
#       coord_fixed(xlim = c(0, 1), ylim = c(0, length(bin_levels))) +
#       theme_minimal() +
#       theme(axis.title = element_blank(),
#             axis.text.x = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.text.y = element_text(hjust = 1),
#             panel.grid = element_blank(),
#             legend.position = "none") +
#       ggtitle("Days") +
#       theme(plot.title = element_text(hjust = 1))
#     return(legend_plot)
#   }
#   
#   ### Custom binned legend for Recall.
#   create_binned_legend_recall <- function(recall_breaks) {
#     dummy <- seq(min(recall_breaks), max(recall_breaks), length.out = 100)
#     dummy_bins <- cut(dummy, breaks = recall_breaks, include.lowest = TRUE, right = FALSE)
#     bin_levels <- levels(dummy_bins)  # No extra "0" category.
#     custom_palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
#     legend_df <- data.frame(bin = factor(bin_levels, levels = bin_levels),
#                             x = 1,
#                             y = seq_along(bin_levels))
#     legend_plot <- ggplot(legend_df, aes(x = x, y = y, fill = bin)) +
#       geom_tile(width = 0.9, height = 0.9) +
#       scale_fill_manual(name = "Recall", values = custom_palette) +
#       scale_y_continuous(breaks = seq_along(bin_levels), labels = bin_levels) +
#       coord_fixed(xlim = c(0, 1), ylim = c(0, length(bin_levels))) +
#       theme_minimal() +
#       theme(axis.title = element_blank(),
#             axis.text.x = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.text.y = element_text(hjust = 1),
#             panel.grid = element_blank(),
#             legend.position = "none") +
#       ggtitle("Recall") +
#       theme(plot.title = element_text(hjust = 1))
#     return(legend_plot)
#   }
#   
#   ### Create individual data panels.
#   message("Creating panels...")
#   # NOAA panels (binned, with zero_special = TRUE)
#   p_a <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   p_b <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   p_c <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   
#   # Cluster panels (binned, with zero_special = TRUE)
#   p_d <- plot_raster(cluster_rasters[[1]], bin_breaks = bin_breaks, zero_special = TRUE)
#   p_e <- plot_raster(cluster_rasters[[2]], bin_breaks = bin_breaks, zero_special = TRUE)
#   p_f <- plot_raster(cluster_rasters[[3]], bin_breaks = bin_breaks, zero_special = TRUE)
#   
#   # Recall panels (binned, with fixed breaks recall_breaks, and zero_special = FALSE)
#   p_g <- plot_raster(recall_rasters[[1]], bin_breaks = recall_breaks, zero_special = FALSE)
#   p_h <- plot_raster(recall_rasters[[2]], bin_breaks = recall_breaks, zero_special = FALSE)
#   p_i <- plot_raster(recall_rasters[[3]], bin_breaks = recall_breaks, zero_special = FALSE)
#   
#   # Create legend panels.
#   legend_binned <- create_binned_legend(bin_breaks)
#   legend_recall <- create_binned_legend_recall(recall_breaks)
#   
#   ### Assemble rows with one extra legend column.
#   library(patchwork)
#   row1 <- wrap_plots(list(p_d, p_e, p_f, legend_binned), ncol = 4) +
#     plot_layout(widths = c(1, 1, 1, 1))
#   row2 <- wrap_plots(list(p_a, p_b, p_c, legend_binned), ncol = 4) +
#     plot_layout(widths = c(1, 1, 1, 1))
#   row3 <- wrap_plots(list(p_g, p_h, p_i, legend_recall), ncol = 4) +
#     plot_layout(widths = c(1, 1, 1, 1))
#   
#   figure <- row1 / row2 / row3 + plot_annotation(tag_levels = "a")
#   
#   message("Figure creation complete.")
#   return(figure)
# }

# make_9panel_figure_binned <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
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
#   # Load reference raster (for target CRS and extent)
#   region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
#                           "heat_index_daily_maximum_194001.nc"))
#   
#   # Load Koppen-Geiger raster and derive a binary resolution raster
#   kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
#                   "1991_2020", "koppen_geiger_0p1.tif")
#   resolution <- rast(kg_path)
#   resolution <- resolution / resolution
#   
#   # Load US county boundaries (maps -> sf -> SpatVector)
#   us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
#   us.states.vect <- vect(us.states)
#   us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
#   us.states.rast <- crop(us.states.rast, ext(region.crs))
#   
#   ### Helper: Sum layers over a year range and mask to counties.
#   process_sum_mask <- function(dir_path, file_prefix, yr_range, required_substring = "") {
#     if (required_substring != "") {
#       files <- list.files(dir_path, pattern = required_substring, full.names = TRUE)
#       message("Found ", length(files), " files in ", dir_path, " containing '", required_substring, "'.")
#       if (length(files) > 0) {
#         extracted_years <- as.numeric(str_extract(basename(files), "\\d{4}"))
#         message("Extracted years: ", paste(extracted_years, collapse = ", "))
#         files <- files[extracted_years %in% yr_range]
#         message("After filtering, ", length(files), " files remain.")
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
#     rast_obj <- rast(files)  # Each file becomes a layer.
#     message("Summing ", nlyr(rast_obj), " layers using sum()...")
#     summed <- sum(rast_obj)
#     message("Global sum: ", global(summed, fun = "sum", na.rm = TRUE)[1])
#     masked <- mask(summed, us.states.rast, maskvalue = NA)
#     return(masked)
#   }
#   
#   ### Process data.
#   message("Processing NOAA data...")
#   noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
#   if (is.null(noaa_raster)) stop("NOAA raster not found!")
#   
#   message("Processing Cluster data...")
#   cluster_rasters <- lapply(names(cluster_dirs), function(name) {
#     dir_path <- cluster_dirs[[name]]
#     r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
#     if (is.null(r)) return(NULL)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(cluster_rasters) <- names(cluster_dirs)
#   
#   message("Processing Recall data...")
#   recall_rasters <- lapply(names(recall_dirs), function(name) {
#     file_path <- recall_dirs[[name]]
#     r <- rast(file_path)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(recall_rasters) <- names(recall_dirs)
#   
#   ### Determine common scale limits.
#   noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
#   message("NOAA max value: ", noaa_max)
#   cluster_max <- max(unlist(lapply(cluster_rasters, function(r) {
#     if (!is.null(r)) global(r, fun = "max", na.rm = TRUE)[1] else NA
#   })), na.rm = TRUE)
#   message("Cluster max value: ", cluster_max)
#   common_max <- max(noaa_max, cluster_max)
#   message("Common NOAA and Cluster range: 0 - ", common_max)
#   
#   ### Define equal-interval bin breaks for nonzero values (for NOAA/Cluster).
#   vals_noaa <- as.vector(noaa_raster[])
#   vals_cluster <- unlist(lapply(cluster_rasters, function(r) as.vector(r[])))
#   all_vals <- c(vals_noaa, vals_cluster)
#   all_vals <- all_vals[!is.na(all_vals)]
#   nonzero_all <- all_vals[all_vals > 0]
#   if(length(nonzero_all) == 0) stop("No nonzero values found for binning.")
#   bmin <- min(nonzero_all)
#   bmax <- common_max
#   # Create 5 total bins: one for 0 and 4 equal-interval bins for nonzero values.
#   bin_breaks <- seq(bmin, bmax, length.out = 5)
#   message("Bin breakpoints (nonzero): ", paste(round(bin_breaks, 2), collapse = ", "))
#   
#   # For Recall panels, use fixed breaks every 0.2 from 0 to 1.
#   recall_breaks <- seq(0, 1, by = 0.2)
#   
#   ### Plotting helper.
#   plot_raster <- function(r, scale_limits = NULL, palette = NULL, na_color = "white",
#                           bin_breaks = NULL, zero_special = TRUE) {
#     df <- as.data.frame(r, xy = TRUE)
#     if (ncol(df) < 3) stop("Raster does not have expected columns.")
#     value_col <- names(df)[3]
#     
#     if (!is.null(bin_breaks)) {
#       if (zero_special) {
#         # For NOAA/Cluster: treat 0 specially.
#         nz_values <- df[[value_col]][df[[value_col]] > 0]
#         if (length(nz_values) > 0) {
#           nz_bins <- cut(nz_values, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#           levels_cut <- levels(nz_bins)
#         } else {
#           levels_cut <- character(0)
#         }
#         df$bin <- ifelse(df[[value_col]] == 0, "0",
#                          as.character(cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)))
#         bin_levels <- c("0", levels_cut)
#         df$bin <- factor(df$bin, levels = bin_levels)
#         if (is.null(palette)) {
#           custom_palette <- c("grey40", colorRampPalette(c("yellow", "red"))(length(levels_cut)))
#         } else {
#           custom_palette <- palette
#         }
#       } else {
#         # For Recall: simply bin using the breaks (do not separate 0).
#         df$bin <- cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#         bin_levels <- levels(df$bin)
#         if (is.null(palette)) {
#           custom_palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
#         } else {
#           custom_palette <- palette
#         }
#       }
#       
#       p <- ggplot() +
#         # First plot the shapefile with a grey40 fill.
#         geom_sf(data = us.states, fill = "grey40", color = NA) +
#         # Then the raster tiles.
#         geom_tile(data = df, aes(x = x, y = y, fill = bin)) +
#         # Finally, draw the shapefile boundaries.
#         geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#         coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#         scale_y_continuous(breaks = seq(24, 40, by = 4)) +
#         scale_fill_manual(name = "", values = custom_palette, na.value = na_color) +
#         theme_minimal() +
#         theme(axis.title = element_blank(),
#               plot.title = element_blank(),
#               legend.position = c(0.9, 0.1),
#               panel.border = element_rect(color = "black", fill = NA, size = 0.3),
#               legend.background = element_rect(color = "black"),
#               panel.grid = element_blank())
#     } else {
#       # For continuous panels.
#       if (is.null(scale_limits)) {
#         max_val <- max(df[[value_col]], na.rm = TRUE)
#         scale_limits <- c(0, max_val)
#       }
#       lower <- scale_limits[1]
#       upper <- scale_limits[2]
#       eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
#       if (is.null(palette)) palette <- c("orange", "purple")
#       if(length(palette) == 3) {
#         brks <- c(lower, lower + eps, upper)
#       } else if(length(palette) == 4) {
#         brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
#       } else {
#         brks <- seq(lower, upper, length.out = length(palette))
#       }
#       p <- ggplot() +
#         geom_sf(data = us.states, fill = "grey40", color = NA) +
#         geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#         geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#         coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#         scale_y_continuous(breaks = seq(24, 40, by = 4)) +
#         scale_fill_gradientn(name = "",
#                              colors = palette,
#                              values = scales::rescale(brks, from = scale_limits),
#                              limits = scale_limits,
#                              na.value = na_color, oob = squish) +
#         theme_minimal() +
#         theme(axis.title = element_blank(),
#               plot.title = element_blank(),
#               legend.position = c(0.9, 0.1),
#               panel.border = element_rect(color = "black", fill = NA, size = 0.3),
#               legend.background = element_rect(color = "black"),
#               panel.grid = element_blank())
#     }
#     return(p)
#   }
#   
#   ### Create individual data panels.
#   message("Creating panels...")
#   # NOAA panels (binned, with zero_special = TRUE)
#   p_a <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   p_b <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   p_c <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   
#   # Cluster panels (binned, with zero_special = TRUE)
#   p_d <- plot_raster(cluster_rasters[[1]], bin_breaks = bin_breaks, zero_special = TRUE)
#   p_e <- plot_raster(cluster_rasters[[2]], bin_breaks = bin_breaks, zero_special = TRUE)
#   p_f <- plot_raster(cluster_rasters[[3]], bin_breaks = bin_breaks, zero_special = TRUE)
#   
#   # Recall panels (binned, with fixed breaks recall_breaks, and zero_special = FALSE)
#   p_g <- plot_raster(recall_rasters[[1]], bin_breaks = recall_breaks, zero_special = FALSE)
#   p_h <- plot_raster(recall_rasters[[2]], bin_breaks = recall_breaks, zero_special = FALSE)
#   p_i <- plot_raster(recall_rasters[[3]], bin_breaks = recall_breaks, zero_special = FALSE)
#   
#   ### Assemble the figure with custom row and column titles.
#   # Remove the extra legend column; legends are now inside each panel.
#   library(patchwork)
#   
#   # Helper to create a text-only plot (for row/column titles).
#   get_text_plot <- function(text, size = 5) {
#     ggplot() + 
#       annotate("text", x = 0.5, y = 0.5, label = text, size = size, hjust = 0.5, vjust = 0.5) +
#       theme_void()
#   }
#   
#   # Column titles.
#   col1_title <- get_text_plot("0.25°/day", size = 6)
#   col2_title <- get_text_plot("0.3075°/day", size = 6)
#   col3_title <- get_text_plot("0.39°/day", size = 6)
#   top_row <- wrap_plots(list(plot_spacer(), col1_title, col2_title, col3_title), ncol = 4, widths = c(0.2, 1, 1, 1))
#   
#   # Row titles.
#   row1_title <- get_text_plot("Clusters", size = 6)
#   row2_title <- get_text_plot("NOAA Episodes", size = 6)
#   row3_title <- get_text_plot("Recall", size = 6)
#   
#   cluster_row <- wrap_plots(list(row1_title, p_d, p_e, p_f), ncol = 4, widths = c(0.2, 1, 1, 1))
#   noaa_row    <- wrap_plots(list(row2_title, p_a, p_b, p_c), ncol = 4, widths = c(0.2, 1, 1, 1))
#   recall_row  <- wrap_plots(list(row3_title, p_g, p_h, p_i), ncol = 4, widths = c(0.2, 1, 1, 1))
#   
#   figure <- top_row / cluster_row / noaa_row / recall_row +
#     plot_layout(heights = c(0.2, 1, 1, 1))
#   
#   message("Figure creation complete.")
#   return(figure)
# }

# make_9panel_figure_binned <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
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
#   # Load reference raster (for target CRS and extent)
#   region.crs <- rast(here("data", "output", "01_era5", "daily", "heat_index",
#                           "heat_index_daily_maximum_194001.nc"))
#   
#   # Load Koppen-Geiger raster and derive a binary resolution raster
#   kg_path <- here("data", "input", "regional_aggregation", "koppen_geiger",
#                   "1991_2020", "koppen_geiger_0p1.tif")
#   resolution <- rast(kg_path)
#   resolution <- resolution / resolution
#   
#   # Load US county boundaries (maps -> sf -> SpatVector)
#   us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
#   us.states.vect <- vect(us.states)
#   us.states.rast <- rasterize(us.states.vect, resolution, field = "ID")
#   us.states.rast <- crop(us.states.rast, ext(region.crs))
#   
#   ### Helper: Sum layers over a year range and mask to counties.
#   process_sum_mask <- function(dir_path, file_prefix, yr_range, required_substring = "") {
#     if (required_substring != "") {
#       files <- list.files(dir_path, pattern = required_substring, full.names = TRUE)
#       message("Found ", length(files), " files in ", dir_path, " containing '", required_substring, "'.")
#       if (length(files) > 0) {
#         extracted_years <- as.numeric(str_extract(basename(files), "\\d{4}"))
#         message("Extracted years: ", paste(extracted_years, collapse = ", "))
#         files <- files[extracted_years %in% yr_range]
#         message("After filtering, ", length(files), " files remain.")
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
#     rast_obj <- rast(files)  # Each file becomes a layer.
#     message("Summing ", nlyr(rast_obj), " layers using sum()...")
#     summed <- sum(rast_obj)
#     message("Global sum: ", global(summed, fun = "sum", na.rm = TRUE)[1])
#     masked <- mask(summed, us.states.rast, maskvalue = NA)
#     return(masked)
#   }
#   
#   ### Process data.
#   message("Processing NOAA data...")
#   noaa_raster <- process_sum_mask(noaa_dir, "NOAA_", year_range, required_substring = "")
#   if (is.null(noaa_raster)) stop("NOAA raster not found!")
#   
#   message("Processing Cluster data...")
#   cluster_rasters <- lapply(names(cluster_dirs), function(name) {
#     dir_path <- cluster_dirs[[name]]
#     r <- process_sum_mask(dir_path, "County_heat_index_", year_range, required_substring = "Daily_Extent")
#     if (is.null(r)) return(NULL)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(cluster_rasters) <- names(cluster_dirs)
#   
#   message("Processing Recall data...")
#   recall_rasters <- lapply(names(recall_dirs), function(name) {
#     file_path <- recall_dirs[[name]]
#     r <- rast(file_path)
#     mask(r, us.states.rast, maskvalue = NA)
#   })
#   names(recall_rasters) <- names(recall_dirs)
#   
#   ### Determine common scale limits.
#   noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
#   message("NOAA max value: ", noaa_max)
#   cluster_max <- max(unlist(lapply(cluster_rasters, function(r) {
#     if (!is.null(r)) global(r, fun = "max", na.rm = TRUE)[1] else NA
#   })), na.rm = TRUE)
#   message("Cluster max value: ", cluster_max)
#   common_max <- max(noaa_max, cluster_max)
#   message("Common NOAA and Cluster range: 0 - ", common_max)
#   
#   ### Define equal-interval bin breaks for nonzero values (for NOAA/Cluster).
#   vals_noaa <- as.vector(noaa_raster[])
#   vals_cluster <- unlist(lapply(cluster_rasters, function(r) as.vector(r[])))
#   all_vals <- c(vals_noaa, vals_cluster)
#   all_vals <- all_vals[!is.na(all_vals)]
#   nonzero_all <- all_vals[all_vals > 0]
#   if(length(nonzero_all) == 0) stop("No nonzero values found for binning.")
#   bmin <- min(nonzero_all)
#   bmax <- common_max
#   # Create 5 total bins: one for 0 and 4 equal-interval bins for nonzero values.
#   bin_breaks <- seq(bmin, bmax, length.out = 5)
#   message("Bin breakpoints (nonzero): ", paste(round(bin_breaks, 2), collapse = ", "))
#   
#   # For Recall panels, use fixed breaks every 0.2 from 0 to 1.
#   recall_breaks <- seq(0, 1, by = 0.2)
#   
#   ### Plotting helper.
#   plot_raster <- function(r, scale_limits = NULL, palette = NULL, na_color = "white",
#                           bin_breaks = NULL, zero_special = TRUE) {
#     df <- as.data.frame(r, xy = TRUE)
#     if (ncol(df) < 3) stop("Raster does not have expected columns.")
#     value_col <- names(df)[3]
#     
#     if (!is.null(bin_breaks)) {
#       if (zero_special) {
#         # For NOAA/Cluster: treat 0 specially.
#         nz_values <- df[[value_col]][df[[value_col]] > 0]
#         if (length(nz_values) > 0) {
#           nz_bins <- cut(nz_values, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#           levels_cut <- levels(nz_bins)
#         } else {
#           levels_cut <- character(0)
#         }
#         df$bin <- ifelse(df[[value_col]] == 0, "0",
#                          as.character(cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)))
#         bin_levels <- c("0", levels_cut)
#         df$bin <- factor(df$bin, levels = bin_levels)
#         if (is.null(palette)) {
#           custom_palette <- c("grey40", colorRampPalette(c("yellow", "red"))(length(levels_cut)))
#         } else {
#           custom_palette <- palette
#         }
#       } else {
#         # For Recall: simply bin using the breaks (do not separate 0).
#         df$bin <- cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#         bin_levels <- levels(df$bin)
#         if (is.null(palette)) {
#           custom_palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
#         } else {
#           custom_palette <- palette
#         }
#       }
#       
#       p <- ggplot() +
#         # First plot the shapefile with a grey40 fill.
#         geom_sf(data = us.states, fill = "grey40", color = NA) +
#         # Then the raster tiles.
#         geom_tile(data = df, aes(x = x, y = y, fill = bin)) +
#         # Finally, draw the shapefile boundaries.
#         geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#         coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#         scale_y_continuous(breaks = seq(24, 40, by = 4)) +
#         scale_fill_manual(name = "", values = custom_palette, na.value = na_color) +
#         theme_minimal() +
#         theme(axis.title = element_blank(),
#               plot.title = element_blank(),
#               legend.position = c(0.9, 0.1),
#               legend.title = element_blank(),
#               legend.key.size = unit(0.3, "lines"),
#               legend.text = element_text(size = 7),
#               legend.background = element_blank(),
#               panel.border = element_rect(color = "black", fill = NA, size = 0.3),
#               panel.grid = element_blank())
#     } else {
#       # For continuous panels.
#       if (is.null(scale_limits)) {
#         max_val <- max(df[[value_col]], na.rm = TRUE)
#         scale_limits <- c(0, max_val)
#       }
#       lower <- scale_limits[1]
#       upper <- scale_limits[2]
#       eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
#       if (is.null(palette)) palette <- c("orange", "purple")
#       if(length(palette) == 3) {
#         brks <- c(lower, lower + eps, upper)
#       } else if(length(palette) == 4) {
#         brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
#       } else {
#         brks <- seq(lower, upper, length.out = length(palette))
#       }
#       p <- ggplot() +
#         geom_sf(data = us.states, fill = "grey40", color = NA) +
#         geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#         geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#         coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#         scale_y_continuous(breaks = seq(24, 40, by = 4)) +
#         scale_fill_gradientn(name = "",
#                              colors = palette,
#                              values = scales::rescale(brks, from = scale_limits),
#                              limits = scale_limits,
#                              na.value = na_color, oob = squish) +
#         theme_minimal() +
#         theme(axis.title = element_blank(),
#               plot.title = element_blank(),
#               legend.position = c(0.9, 0.1),
#               legend.title = element_blank(),
#               legend.key.size = unit(0.3, "lines"),
#               legend.text = element_text(size = 7),
#               legend.background = element_blank(),
#               panel.border = element_rect(color = "black", fill = NA, size = 0.3),
#               panel.grid = element_blank())
#     }
#     return(p)
#   }
#   
#   ### Helper for creating text-only plots.
#   # Now with an angle argument (default 0) so that row labels can be rotated.
#   get_text_plot <- function(text, size = 5, angle = 0) {
#     ggplot() + 
#       annotate("text", x = 0.5, y = 0.5, label = text, size = size, angle = angle, hjust = 0.5, vjust = 0.5) +
#       theme_void()
#   }
#   
#   ### Create individual data panels.
#   message("Creating panels...")
#   # NOAA panels (binned, with zero_special = TRUE)
#   p_a <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   p_b <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   p_c <- plot_raster(noaa_raster, bin_breaks = bin_breaks, zero_special = TRUE)
#   
#   # Cluster panels (binned, with zero_special = TRUE)
#   p_d <- plot_raster(cluster_rasters[[1]], bin_breaks = bin_breaks, zero_special = TRUE)
#   p_e <- plot_raster(cluster_rasters[[2]], bin_breaks = bin_breaks, zero_special = TRUE)
#   p_f <- plot_raster(cluster_rasters[[3]], bin_breaks = bin_breaks, zero_special = TRUE)
#   
#   # Recall panels (binned, with fixed breaks recall_breaks, and zero_special = FALSE)
#   p_g <- plot_raster(recall_rasters[[1]], bin_breaks = recall_breaks, zero_special = FALSE)
#   p_h <- plot_raster(recall_rasters[[2]], bin_breaks = recall_breaks, zero_special = FALSE)
#   p_i <- plot_raster(recall_rasters[[3]], bin_breaks = recall_breaks, zero_special = FALSE)
#   
#   ### Assemble the figure with custom row and column titles.
#   # Remove the extra legend column; legends are now inside each panel.
#   library(patchwork)
#   
#   # Column titles (not rotated).
#   col1_title <- get_text_plot("0.25°/day", size = 6, angle = 0)
#   col2_title <- get_text_plot("0.3075°/day", size = 6, angle = 0)
#   col3_title <- get_text_plot("0.39°/day", size = 6, angle = 0)
#   top_row <- wrap_plots(list(plot_spacer(), col1_title, col2_title, col3_title), ncol = 4, widths = c(0.2, 1, 1, 1))
#   
#   # Row titles: now rotated 90 degrees.
#   row1_title <- get_text_plot("Clusters", size = 6, angle = 90)
#   row2_title <- get_text_plot("NOAA Episodes", size = 6, angle = 90)
#   row3_title <- get_text_plot("Recall", size = 6, angle = 90)
#   
#   cluster_row <- wrap_plots(list(row1_title, p_d, p_e, p_f), ncol = 4, widths = c(0.2, 1, 1, 1))
#   noaa_row    <- wrap_plots(list(row2_title, p_a, p_b, p_c), ncol = 4, widths = c(0.2, 1, 1, 1))
#   recall_row  <- wrap_plots(list(row3_title, p_g, p_h, p_i), ncol = 4, widths = c(0.2, 1, 1, 1))
#   
#   figure <- top_row / cluster_row / noaa_row / recall_row +
#     plot_layout(heights = c(0.2, 1, 1, 1))
#   
#   message("Figure creation complete.")
#   return(figure)
# }

make_9panel_figure_binned <- function(noaa_dir, cluster_dirs, recall_dirs, year_range, output_dir) {
  
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
  # Create 5 total bins: one for 0 and 4 equal-interval bins for nonzero values.
  bin_breaks <- seq(bmin, bmax, length.out = 5)
  message("Bin breakpoints (nonzero): ", paste(round(bin_breaks, 2), collapse = ", "))
  
  # For Recall panels, use fixed breaks every 0.2 from 0 to 1.
  recall_breaks <- seq(0, 1, by = 0.2)
  
  ### Plotting helper.
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
        # First plot the shapefile with a grey40 fill.
        geom_sf(data = us.states, fill = "grey40", color = NA) +
        # Then the raster tiles.
        geom_tile(data = df, aes(x = x, y = y, fill = bin)) +
        # Finally, draw the shapefile boundaries.
        geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
        coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
        scale_y_continuous(breaks = seq(24, 40, by = 4)) +
        scale_fill_manual(name = "", values = custom_palette, na.value = na_color) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              plot.title = element_blank(),
              legend.position = c(0.1, 0.1),
              legend.title = element_blank(),
              legend.key.size = unit(0.3, "lines"),
              legend.text = element_text(size = 7),
              legend.background = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, size = 0.3),
              panel.grid = element_blank())
    } else {
      # For continuous panels.
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
        geom_sf(data = us.states, fill = "grey40", color = NA) +
        geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
        geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
        coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
        scale_y_continuous(breaks = seq(24, 40, by = 4)) +
        scale_fill_gradientn(name = "",
                             colors = palette,
                             values = scales::rescale(brks, from = scale_limits),
                             limits = scale_limits,
                             na.value = na_color, oob = squish) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              plot.title = element_blank(),
              legend.position = c(0.1, 0.1),
              legend.title = element_blank(),
              legend.key.size = unit(0.3, "lines"),
              legend.text = element_text(size = 7),
              legend.background = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, size = 0.3),
              panel.grid = element_blank())
    }
    return(p)
  }
  
  ### Helper for creating text-only plots.
  # Now with an angle argument (default 0) so that row labels can be rotated.
  get_text_plot <- function(text, size = 5, angle = 0) {
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = text, size = size, angle = angle, hjust = 0.5, vjust = 0.5) +
      theme_void()
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
  
  ### Assemble the figure with custom row and column titles.
  # Remove the extra legend column; legends are now inside each panel.
  library(patchwork)
  
  # Column titles (not rotated).
  col1_title <- get_text_plot("0.25°/day", size = 6, angle = 0)
  col2_title <- get_text_plot("0.3075°/day", size = 6, angle = 0)
  col3_title <- get_text_plot("0.39°/day", size = 6, angle = 0)
  top_row <- wrap_plots(list(plot_spacer(), col1_title, col2_title, col3_title), ncol = 4, widths = c(0.2, 1, 1, 1))
  
  # Row titles: now rotated 90 degrees.
  row1_title <- get_text_plot("Clusters", size = 6, angle = 90)
  row2_title <- get_text_plot("NOAA Episodes", size = 6, angle = 90)
  row3_title <- get_text_plot("Recall", size = 6, angle = 90)
  
  cluster_row <- wrap_plots(list(row1_title, p_d, p_e, p_f), ncol = 4, widths = c(0.2, 1, 1, 1))
  noaa_row    <- wrap_plots(list(row2_title, p_a, p_b, p_c), ncol = 4, widths = c(0.2, 1, 1, 1))
  recall_row  <- wrap_plots(list(row3_title, p_g, p_h, p_i), ncol = 4, widths = c(0.2, 1, 1, 1))
  
  figure <- top_row / cluster_row / noaa_row / recall_row +
    plot_layout(heights = c(0.2, 1, 1, 1))
  
  message("Figure creation complete.")
  return(figure)
}


## Heat Index Advisory ----
### 2019-2023 ----
year_range <- 2019:2023
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.39", "heat_index", "summary")
)
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall","advisory", "raster", "2019_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall","advisory", "raster", "2019_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall","advisory", "raster", "2019_2023_day_0.39_excess_heat.nc")
)
output_directory <- here("figures")
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Save the plot as a PNG + SVG file
png_path <- here("figures","05_recall_excess_heat_advisory_day_binned_2019_2023.png")
svg_path <- here("figures","05_recall_excess_heat_advisory_day_binned_2019_2023.svg")
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

### 2010-2023 ----
year_range <- 2010:2023
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.39", "heat_index", "summary")
)
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall","advisory", "raster", "2010_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall","advisory", "raster", "2010_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall","advisory", "raster", "2010_2023_day_0.39_excess_heat.nc")
)
output_directory <- here("figures")
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Save the plot as a PNG + SVG file
png_path <- here("figures","05_recall_excess_heat_advisory_day_binned_2010_2023.png")
svg_path <- here("figures","05_recall_excess_heat_advisory_day_binned_2010_2023.svg")
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

### 2000-2023 ----
year_range <- 2000:2023
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.39", "heat_index", "summary")
)
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall","advisory", "raster", "2000_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall","advisory", "raster", "2000_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall","advisory", "raster", "2000_2023_day_0.39_excess_heat.nc")
)
output_directory <- here("figures")
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Save the plot as a PNG + SVG file
png_path <- here("figures","05_recall_excess_heat_advisory_day_binned_2000_2023.png")
svg_path <- here("figures","05_recall_excess_heat_advisory_day_binned_2000_2023.svg")
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

## Heat Index Warning ----
### 2019-2023 ----
year_range <- 2019:2023
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.39", "heat_index", "summary")
)
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall","warning", "raster", "2019_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall","warning", "raster", "2019_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall","warning", "raster", "2019_2023_day_0.39_excess_heat.nc")
)
output_directory <- here("figures")
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Save the plot as a PNG + SVG file
png_path <- here("figures","05_recall_excess_heat_warning_day_binned_2019_2023.png")
svg_path <- here("figures","05_recall_excess_heat_warning_day_binned_2019_2023.svg")
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

### 2010-2023 ----
year_range <- 2010:2023
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.39", "heat_index", "summary")
)
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall","warning", "raster", "2010_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall","warning", "raster", "2010_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall","warning", "raster", "2010_2023_day_0.39_excess_heat.nc")
)
output_directory <- here("figures")
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Save the plot as a PNG + SVG file
png_path <- here("figures","05_recall_excess_heat_warning_day_binned_2010_2023.png")
svg_path <- here("figures","05_recall_excess_heat_warning_day_binned_2010_2023.svg")
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

### 2000-2023 ----
year_range <- 2000:2023
noaa_directory <- here("data", "output", "04_noaa", "southeast", "summary", "excess_heat")
cluster_dirs <- list(
  stm1 = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.25", "heat_index", "summary"),
  record = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.3075", "heat_index", "summary"),
  stm4 = here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.39", "heat_index", "summary")
)
recall_dirs <- list(
  stm1 = here("data", "output", "05_validation", "recall","warning", "raster", "2000_2023_day_0.25_excess_heat.nc"),
  record = here("data", "output", "05_validation", "recall","warning", "raster", "2000_2023_day_0.3075_excess_heat.nc"),
  stm4 = here("data", "output", "05_validation", "recall","warning", "raster", "2000_2023_day_0.39_excess_heat.nc")
)
output_directory <- here("figures")
figure <- make_9panel_figure_binned(noaa_directory, cluster_dirs, recall_dirs, year_range, output_directory)

# Save the plot as a PNG + SVG file
png_path <- here("figures","05_recall_excess_heat_warning_day_binned_2000_2023.png")
svg_path <- here("figures","05_recall_excess_heat_warning_day_binned_2000_2023.svg")
ggsave(filename = png_path, plot = figure, width = 9, height = 7, dpi = 300)
ggsave(filename = svg_path, plot = figure, width = 9, height = 7, device = "svg")

# make_3panel_figure <- function(noaa_dir, cluster_dir, recall_file, year_range, output_dir) {
#   # library(terra)
#   # library(sf)
#   # library(ggplot2)
#   # library(patchwork)
#   # library(maps)
#   # library(dplyr)
#   # library(here)
#   # library(scales)
#   
#   message("DEBUG: noaa_dir: ", noaa_dir)
#   message("DEBUG: cluster_dir: ", cluster_dir)
#   message("DEBUG: recall_file: ", recall_file)
#   message("DEBUG: output_dir: ", output_dir)
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
#     rast_obj <- rast(files)
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
#   ### Process Cluster data:
#   message("Processing Cluster data...")
#   cluster_raster <- process_sum_mask(cluster_dir, "County_heat_index_", year_range, required_substring = "Daily_Extent")
#   if (is.null(cluster_raster)) stop("Cluster raster not found!")
#   
#   ### Process Recall data:
#   message("Processing Recall data...")
#   recall_raster <- rast(recall_file)
#   recall_raster <- mask(recall_raster, us.states.rast, maskvalue = NA)
#   
#   ### Determine common scale limits for NOAA and Cluster:
#   noaa_max <- as.numeric(global(noaa_raster, fun = "max", na.rm = TRUE)[1])
#   message("NOAA range: 0 - ", noaa_max)
#   
#   cluster_max <- as.numeric(global(cluster_raster, fun = "max", na.rm = TRUE)[1])
#   message("Cluster range: 0 - ", cluster_max)
#   
#   ### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf().
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
#   ### Create individual panels:
#   message("Creating panels...")
#   # NOAA panel using a red–white–blue gradient.
#   p_noaa <- plot_raster(noaa_raster, scale_limits = c(0, noaa_max), 
#                         palette = c("grey40", "blue", "white", "red"))
#   
#   # Cluster panel using a red–white–blue gradient.
#   p_cluster <- plot_raster(cluster_raster, scale_limits = c(0, cluster_max), 
#                            palette = c("grey40", "blue", "white", "red"))
#   
#   # Recall panel using an orange-to-purple gradient with NA values in black.
#   p_recall <- plot_raster(recall_raster, scale_limits = c(0, 1), 
#                           palette = c("orange", "purple"), na_color = "black")
#   
#   message("Assembling 3-panel figure using patchwork...")
#   library(patchwork)
#   # Arrange the three panels vertically (one plot per panel)
#   figure <- p_noaa + p_cluster + p_recall + plot_layout(ncol = 1) +
#     plot_annotation(tag_levels = "a")
#   
#   message("Figure creation complete.")
#   return(figure)
# }

# 2. Aggregate recall ----
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

# Heat Index Advisory ----
recall <- data.frame(res_0.25 = rep(NA,3),
                     res_0.3075 = rep(NA,3),
                     res_0.39 = rep(NA,3))

## 0.25 deg / day ----
input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.25", "excess_heat")
year_range <- 2000:2023
recall$res_0.25[1] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.25", "excess_heat")
year_range <- 2010:2023
recall$res_0.25[2] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.25", "excess_heat")
year_range <- 2019:2023
recall$res_0.25[3] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

## 0.3075 deg / day----
input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.3075", "excess_heat")
year_range <- 2000:2023
recall$res_0.3075[1] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.3075", "excess_heat")
year_range <- 2010:2023
recall$res_0.3075[2] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.3075", "excess_heat")
year_range <- 2019:2023
recall$res_0.3075[3] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

## 0.39 deg / day ----
input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.39", "excess_heat")
year_range <- 2000:2023
recall$res_0.39[1] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.39", "excess_heat")
year_range <- 2010:2023
recall$res_0.39[2] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","advisory", "day", "0.39", "excess_heat")
year_range <- 2019:2023
recall$res_0.39[3] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

row.names(recall) <- c('2000-2023','2010-2023','2019-2023')

# save
write.csv(recall,here('data','output','05_validation','recall','advisory','advisory_recall.csv'))

# Heat Index Warning ----
recall <- data.frame(res_0.25 = rep(NA,3),
                     res_0.3075 = rep(NA,3),
                     res_0.39 = rep(NA,3))

## 0.25 deg / day ----
input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.25", "excess_heat")
year_range <- 2000:2023
recall$res_0.25[1] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.25", "excess_heat")
year_range <- 2010:2023
recall$res_0.25[2] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.25", "excess_heat")
year_range <- 2019:2023
recall$res_0.25[3] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

## 0.3075 deg / day----
input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.3075", "excess_heat")
year_range <- 2000:2023
recall$res_0.3075[1] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.3075", "excess_heat")
year_range <- 2010:2023
recall$res_0.3075[2] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.3075", "excess_heat")
year_range <- 2019:2023
recall$res_0.3075[3] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

## 0.39 deg / day ----
input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.39", "excess_heat")
year_range <- 2000:2023
recall$res_0.39[1] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.39", "excess_heat")
year_range <- 2010:2023
recall$res_0.39[2] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

input_folder <- here("data", "output", "05_validation", "recall","warning", "day", "0.39", "excess_heat")
year_range <- 2019:2023
recall$res_0.39[3] <- aggregate_validation_csv_overall(input_folder, year_range)[,3]

row.names(recall) <- c('2000-2023','2010-2023','2019-2023')

# save
write.csv(recall,here('data','output','05_validation','recall','warning','warning_recall.csv'))

# 3. Key Figure ----
# plot(recall_rasters)
### Plotting helper: Convert a SpatRaster to a ggplot object using geom_tile() and coord_sf().
### Accepts an optional scale_limits argument, a palette argument, and na_color for NA values.
# plot_raster <- function(r, scale_limits = NULL, 
#                         palette = c("grey40", "blue", "white", "red"), na_color = "white") {
#   df <- as.data.frame(r, xy = TRUE)
#   if (ncol(df) < 3) stop("Raster does not have expected columns.")
#   value_col <- names(df)[3]
#   if(is.null(scale_limits)) {
#     max_val <- max(df[[value_col]], na.rm = TRUE)
#     scale_limits <- c(0, max_val)
#   }
#   lower <- scale_limits[1]
#   upper <- scale_limits[2]
#   eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
#   
#   # Define breakpoints based on the length of the palette
#   if(length(palette) == 3) {
#     brks <- c(lower, lower + eps, upper)
#   } else if(length(palette) == 4) {
#     brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
#   } else {
#     brks <- seq(lower, upper, length.out = length(palette))
#   }
#   
#   p <- ggplot() +
#     geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#     scale_fill_gradientn(name = "",
#                          colors = palette,
#                          values = scales::rescale(brks, from = scale_limits),
#                          limits = scale_limits,
#                          na.value = na_color, oob = squish) +
#     geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#     coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = F) +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           plot.title = element_blank(),
#           legend.position = c(.9,.25))
#   return(p)
# }

# plot_raster <- function(r, scale_limits = NULL, palette = c("orange", "purple"), na_color = "white") {
#   df <- as.data.frame(r, xy = TRUE)
#   if (ncol(df) < 3) stop("Raster does not have expected columns.")
#   value_col <- names(df)[3]
#   
#   # Default to a 0–1 scale if not provided
#   if (is.null(scale_limits)) {
#     scale_limits <- c(0, 1)
#   }
#   lower <- scale_limits[1]
#   upper <- scale_limits[2]
#   eps <- ifelse(upper - lower > 0, (upper - lower) * 1e-3, 1e-6)
#   
#   # Define breakpoints for mapping the palette colors
#   if(length(palette) == 3) {
#     brks <- c(lower, lower + eps, upper)
#   } else if(length(palette) == 4) {
#     brks <- c(lower, lower + eps, (lower + upper) / 2, upper)
#   } else {
#     brks <- seq(lower, upper, length.out = length(palette))
#   }
#   
#   # Fixed legend breaks from 0 to 1 by increments of 0.2
#   fixed_breaks <- seq(0, 1, by = 0.2)
#   
#   p <- ggplot() +
#     geom_tile(data = df, aes(x = x, y = y, fill = get(value_col))) +
#     scale_fill_gradientn(name = "",
#                          colors = palette,
#                          values = scales::rescale(brks, from = scale_limits),
#                          limits = scale_limits,
#                          breaks = fixed_breaks,
#                          labels = fixed_breaks,
#                          na.value = na_color, 
#                          oob = squish) +
#     geom_sf(data = us.states, fill = NA, color = "black", size = 0.3) +
#     coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           plot.title = element_blank(),
#           legend.position = c(0.9, 0.25))
#   
#   return(p)
# }

# plot_single_recall_panel <- function(recall_raster, 
#                                      bin_breaks = seq(0, 1, by = 0.2), 
#                                      na_color = "white", 
#                                      palette = NULL) {
#   # Load US state boundaries (for overlay)
#   us_states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
#   
#   # Convert the raster to a data frame with coordinates.
#   df <- as.data.frame(recall_raster, xy = TRUE)
#   if(ncol(df) < 3) stop("Raster does not have expected columns.")
#   value_col <- names(df)[3]
#   
#   # Bin the raster values using the specified breaks.
#   df$bin <- cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
#   bin_levels <- levels(df$bin)
#   
#   # Use orange-to-purple gradient if no palette is provided.
#   if (is.null(palette)) {
#     palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
#   }
#   
#   p <- ggplot() +
#     geom_tile(data = df, aes(x = x, y = y, fill = bin)) +
#     scale_fill_manual(name = "Recall", values = palette, na.value = na_color) +
#     # Overlay US state boundaries.
#     geom_sf(data = us_states, fill = NA, color = "black", size = 0.3) +
#     coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
#     theme_minimal() +
#     theme(axis.title = element_blank(),
#           plot.title = element_blank(),
#           legend.position = "right")
#   
#   return(p)
# }

plot_single_recall_panel <- function(recall_raster, 
                                     bin_breaks = seq(0, 1, by = 0.2), 
                                     na_color = "white", 
                                     palette = NULL) {
  # Load US state boundaries (for overlay)
  us_states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  
  # Convert the raster to a data frame with coordinates.
  df <- as.data.frame(recall_raster, xy = TRUE)
  if(ncol(df) < 3) stop("Raster does not have expected columns.")
  value_col <- names(df)[3]
  
  # Bin the raster values using the specified breaks.
  df$bin <- cut(df[[value_col]], breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
  bin_levels <- levels(df$bin)
  
  # Use orange-to-purple gradient if no palette is provided.
  if (is.null(palette)) {
    palette <- colorRampPalette(c("orange", "purple"))(length(bin_levels))
  }
  
  p <- ggplot() +
    geom_tile(data = df, aes(x = x, y = y, fill = bin)) +
    scale_fill_manual(name = "Recall", values = palette, na.value = na_color) +
    # Overlay US state boundaries.
    geom_sf(data = us_states, fill = NA, color = "black", size = 0.3) +
    coord_sf(xlim = c(-95, -75), ylim = c(24, 40), expand = FALSE) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          plot.title = element_blank(),
          # Place the legend at the bottom right
          legend.position = c(1, 0),
          legend.justification = c(1, 0))
  
  return(p)
}


# 
# 
# key_figure <- plot_raster(recall_rasters, scale_limits = c(0, 1), 
#                    palette = c("orange", "purple"), na_color = "black")

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
file_path <- here("data", "output", "05_validation", "recall","advisory", "raster", "2019_2023_day_record_excess_heat.nc")
### Process Recall files (direct file paths):
r <- rast(file_path)
recall_raster <- mask(r, us.states.rast, maskvalue = NA)

# Generate the plot.
recall_plot <- plot_single_recall_panel(recall_raster)

# Save the plot as a PNG + SVG file
png_path <- here("figures","00_key_figure.png")
svg_path <- here("figures","00_key_figure.svg")
ggsave(filename = png_path, plot = recall_plot, width = 4, height = 4, dpi = 300)
ggsave(filename = svg_path, plot = recall_plot, width = 4, height = 4, device = "svg")
