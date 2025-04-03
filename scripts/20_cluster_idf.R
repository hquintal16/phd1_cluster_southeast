# #Setup----
# #Updated April 2025
# #Linked to GitHub
# #Author: Hunter Quintal
# #purpose: Calculate cluster idf + exposure 
# 
# # MOVE THIS SCRIPT TO 19_DATAFRAME_CLUSTER UPON SUCCESS
# 
# # Load Libraries & Set Project Root ----
# library(here)
# here::i_am("scripts/20_cluster_idf.R")
# source(here::here("scripts", "01_library.R"))
# 
# test <- terra::rast(here('data','output','03_cluster','02_cluster','points','stm1','heat_index','county','county_event_1940-06-09_cluster_0001.nc'))
# test <- sum(test)
# sum(test[terra::values(test) > 0])
# 
# ## Look up table ----
# 
# create_lookup_table <- function(input_directory, output_csv_path) {
#   # Resolve the input directory using here::here
#   in_dir <- here::here(input_directory)
#   
#   # List all .nc files with the naming convention yyyy-mm-dd_cluster_x.nc
#   files <- list.files(
#     path = in_dir,
#     pattern = "^\\d{4}-\\d{2}-\\d{2}_cluster_\\d+\\.nc$",
#     full.names = TRUE
#   )
#   
#   # Check if any files were found
#   if (length(files) == 0) {
#     message("No matching .nc files found in ", in_dir)
#     return(NULL)
#   }
#   
#   # Extract the date from each filename (assumes the date is the first 10 characters)
#   file_dates <- as.Date(substr(basename(files), 1, 10))
#   
#   # Order files by date, and use the filename as a tiebreaker to ensure reproducibility
#   sorted_order <- order(file_dates, basename(files))
#   files_sorted <- files[sorted_order]
#   
#   # Build the lookup table
#   lookup_table <- data.frame(
#     original_file   = basename(files_sorted),
#     original_index  = sub(".*_cluster_(\\d+)\\.nc$", "\\1", basename(files_sorted)),
#     new_index       = sprintf("%04d", seq_along(files_sorted)),
#     new_file        = paste0(substr(basename(files_sorted), 1, 10), "_cluster_", sprintf("%04d", seq_along(files_sorted)), ".nc"),
#     stringsAsFactors = FALSE
#   )
#   
#   # Write the lookup table to a CSV file
#   write.csv(lookup_table, file = output_csv_path, row.names = FALSE)
#   
#   # Return the lookup table (invisibly)
#   invisible(lookup_table)
# }
# 
# ### 0.25 ----
# create_lookup_table(
#   input_directory = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','raw'),
#   output_csv_path = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','lookup_table.csv')
# )
# ### 0.39 ----
# create_lookup_table(
#   input_directory = here('data','output','03_cluster','02_cluster','points','record','heat_index','raw'),
#   output_csv_path = here('data','output','03_cluster','02_cluster','points','record','heat_index','lookup_table.csv')
# )
# 
# # Load a reference raster to set the CRS
# region.crs <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))
# 
# # Read in the Koppen-Geiger geotiff and create a "resolution" raster
# directory <- here::here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")
# resolution <- terra::rast(directory)
# resolution <- resolution / resolution
# 
# # Load US county boundaries using the maps package, convert to an sf object, then to a SpatVector
# us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
# us.states.vect <- terra::vect(us.states)
# 
# # Rasterize using the resolution from the Koppen-Geiger file (this will have character values)
# us.states.rast <- terra::rasterize(us.states.vect, resolution, field = 'ID')
# us.states.rast <- terra::crop(us.states.rast, terra::ext(region.crs))
# 
# # create_cluster_summary <- function(
# #     lookup_file_path,
# #     cluster_file_path,
# #     cluster_folder,
# #     us_states_rast,
# #     output_csv_path = NULL
# # ) {
# #   # Load required packages
# #   # library(terra)
# #   
# #   # Read the CSV files
# #   lookup <- read.csv(lookup_file_path, stringsAsFactors = FALSE)
# #   cluster_data <- read.csv(cluster_file_path, stringsAsFactors = FALSE)
# #   
# #   # Convert the 'date' column to Date (if you still need it for other calculations)
# #   # Adjust the format string if your dates differ
# #   cluster_data$date <- as.Date(cluster_data$date, format = "%m/%d/%Y")
# #   
# #   # Ensure the lookup table is processed in sequential order (by new_index)
# #   lookup <- lookup[order(lookup$new_index), ]
# #   
# #   # Initialize a list to store summary rows for each cluster
# #   summary_list <- list()
# #   
# #   # Create a progress bar with a maximum value equal to the number of clusters
# #   pb <- txtProgressBar(min = 0, max = nrow(lookup), style = 3)
# #   
# #   # Loop over each cluster (each row in the lookup table)
# #   for (i in seq_len(nrow(lookup))) {
# #     current_lookup <- lookup[i, ]
# #     cluster_id <- current_lookup$new_index      # new_index becomes the cluster_id
# #     original_index <- as.character(current_lookup$original_index)
# #     
# #     # Subset cluster_data where 'cluster' equals the original index
# #     # (Only used for ERA5 stats now, not for date range)
# #     subset_cluster <- subset(cluster_data, as.character(cluster) == original_index)
# #     
# #     # Calculate ERA5 statistics if data exists in the CSV; otherwise, assign NA
# #     if (nrow(subset_cluster) > 0) {
# #       era5_mean   <- mean(subset_cluster$observation, na.rm = TRUE)
# #       era5_median <- median(subset_cluster$observation, na.rm = TRUE)
# #       era5_max    <- max(subset_cluster$observation, na.rm = TRUE)
# #     } else {
# #       era5_mean   <- NA
# #       era5_median <- NA
# #       era5_max    <- NA
# #     }
# #     
# #     # Default date/duration values
# #     start_date <- NA
# #     end_date   <- NA
# #     duration   <- NA
# #     
# #     # Build the file name. First try using new_file from the lookup table.
# #     file_name_candidate <- current_lookup$new_file
# #     nc_file_path <- file.path(cluster_folder, file_name_candidate)
# #     
# #     # If the file is not found, prepend "county_event_" to the file name.
# #     if (!file.exists(nc_file_path)) {
# #       file_name_candidate <- paste0("county_event_", current_lookup$new_file)
# #       nc_file_path <- file.path(cluster_folder, file_name_candidate)
# #     }
# #     
# #     # Initialize exposed_area and exposed_counties as NA by default
# #     exposed_area <- NA
# #     exposed_counties <- NA
# #     
# #     # If the file exists, proceed to read and extract info
# #     if (file.exists(nc_file_path)) {
# #       # Read in the .nc file as a raster
# #       event_rast <- rast(nc_file_path)
# #       
# #       # --- Extract Time Dimension for Start/End Dates ---
# #       # 'terra::time()' should return a vector of Dates or POSIXct times
# #       time_vector <- terra::time(event_rast)
# #       
# #       # If the time dimension is valid and non-empty, compute start_date, end_date, and duration
# #       if (!is.null(time_vector) && length(time_vector) > 0) {
# #         # Convert to Date if it is POSIXct (or numeric “days since…”). 
# #         # terra often does this automatically if the NetCDF is CF-compliant.
# #         # If it's already Date, this will be fine.
# #         time_vector <- as.Date(time_vector)
# #         
# #         start_date <- min(time_vector, na.rm = TRUE)
# #         end_date   <- max(time_vector, na.rm = TRUE)
# #         duration   <- as.numeric(end_date - start_date)
# #       }
# #       
# #       # --- Exposed Area Calculation ---
# #       # Sum over layers (if needed) and create a binary mask (nonzero cells)
# #       noaa_sum <- app(event_rast, fun = sum, na.rm = TRUE)
# #       event_mask_binary <- noaa_sum != 0
# #       
# #       # Calculate cell area, accounting for geographic coordinates if applicable
# #       res_vals <- res(event_rast)
# #       if (is.lonlat(event_rast)) {
# #         cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
# #       } else {
# #         cell_area <- (res_vals[1] * res_vals[2]) / 1e6
# #       }
# #       exposed_area <- sum(values(event_mask_binary), na.rm = TRUE) * cell_area
# #       
# #       # --- Extract Exposed Counties/States ---
# #       event_cells <- which(values(event_mask_binary) != 0)
# #       if (length(event_cells) > 0) {
# #         # Use terra::xyFromCell to get coordinates
# #         coords <- terra::xyFromCell(event_rast, event_cells)
# #         # Extract from the provided SpatRaster
# #         extracted <- terra::extract(us_states_rast, coords)
# #         # Assume the region names are stored in the first layer
# #         layer_name <- names(us_states_rast)[1]
# #         unique_regions <- unique(extracted[[layer_name]])
# #         exposed_counties <- paste(unique_regions, collapse = ";")
# #       }
# #     } else {
# #       warning("File not found: ", nc_file_path, 
# #               ". Skipping exposed area and county extraction for cluster ", cluster_id)
# #     }
# #     
# #     # Combine the metrics for the current cluster into a data frame row
# #     summary_list[[i]] <- data.frame(
# #       cluster_id       = cluster_id,
# #       start_date       = start_date,
# #       end_date         = end_date,
# #       duration         = duration,
# #       exposed_counties = exposed_counties,
# #       exposed_area     = exposed_area,
# #       era5_mean        = era5_mean,
# #       era5_median      = era5_median,
# #       era5_max         = era5_max,
# #       stringsAsFactors = FALSE
# #     )
# #     
# #     # Update the progress bar
# #     setTxtProgressBar(pb, i)
# #   }
# #   
# #   # Close the progress bar
# #   close(pb)
# #   
# #   # Combine all rows into one summary data frame
# #   summary_df <- do.call(rbind, summary_list)
# #   
# #   # Optionally write the summary to a CSV file if an output path is provided
# #   if (!is.null(output_csv_path)) {
# #     write.csv(summary_df, file = output_csv_path, row.names = FALSE)
# #   }
# #   
# #   return(summary_df)
# # }
# 
# ## Cluster IDF ----
# 
# create_cluster_summary <- function(
#     lookup_file_path,
#     cluster_file_path,
#     cluster_folder,
#     us_states_rast,
#     output_csv_path = NULL
# ) {
#   # Load required package
#   # library(terra)
#   
#   # Read the CSV files
#   lookup <- read.csv(lookup_file_path, stringsAsFactors = FALSE)
#   cluster_data <- read.csv(cluster_file_path, stringsAsFactors = FALSE)
#   
#   # Convert the 'date' column to Date (assumes format like "6/27/1940")
#   cluster_data$date <- as.Date(cluster_data$date, format = "%m/%d/%Y")
#   
#   # Order lookup by new_index
#   lookup <- lookup[order(lookup$new_index), ]
#   
#   # Initialize list to store summary rows for each cluster
#   summary_list <- list()
#   
#   # Create a progress bar
#   pb <- txtProgressBar(min = 0, max = nrow(lookup), style = 3)
#   
#   # Loop over each cluster (each row in the lookup table)
#   for (i in seq_len(nrow(lookup))) {
#     current_lookup <- lookup[i, ]
#     cluster_id <- current_lookup$new_index      # new_index becomes cluster_id
#     original_index <- as.character(current_lookup$original_index)
#     
#     # Subset cluster_data for ERA5 stats
#     subset_cluster <- subset(cluster_data, as.character(cluster) == original_index)
#     
#     if (nrow(subset_cluster) > 0) {
#       era5_mean   <- mean(subset_cluster$observation, na.rm = TRUE)
#       era5_median <- median(subset_cluster$observation, na.rm = TRUE)
#       era5_max    <- max(subset_cluster$observation, na.rm = TRUE)
#     } else {
#       era5_mean   <- NA
#       era5_median <- NA
#       era5_max    <- NA
#     }
#     
#     # Default values for dates and duration
#     start_date <- NA
#     end_date <- NA
#     duration <- NA
#     
#     # Build file name using new_file from lookup
#     file_name_candidate <- current_lookup$new_file
#     nc_file_path <- file.path(cluster_folder, file_name_candidate)
#     
#     # If file not found, try prepending "county_event_"
#     if (!file.exists(nc_file_path)) {
#       file_name_candidate <- paste0("county_event_", current_lookup$new_file)
#       nc_file_path <- file.path(cluster_folder, file_name_candidate)
#     }
#     
#     # Initialize exposed_area and exposed_counties as NA
#     exposed_area <- NA
#     exposed_counties <- NA
#     
#     if (file.exists(nc_file_path)) {
#       # Read the .nc file as a SpatRaster
#       event_rast <- tryCatch({
#         rast(nc_file_path)
#       }, error = function(e) {
#         message("Error reading raster file: ", nc_file_path)
#         stop(e)
#       })
#       
#       # --- Extract Time Dimension for Start/End Dates ---
#       time_vector <- terra::time(event_rast)
#       if (!is.null(time_vector) && length(time_vector) > 0) {
#         time_vector <- as.Date(time_vector)
#         start_date <- min(time_vector, na.rm = TRUE)
#         end_date <- max(time_vector, na.rm = TRUE)
#         duration <- as.numeric(end_date - start_date)
#       }
#       
#       # --- Exposed Area Calculation ---
#       # Sum across layers; this returns a single-layer raster.
#       sum_rast <- tryCatch({
#         sum(event_rast, na.rm = TRUE)
#       }, error = function(e) {
#         message("Error in terra::sum for file: ", nc_file_path)
#         stop(e)
#       })
#       
#       # Create a binary mask: cells with sum > 0 are exposed.
#       mask <- sum_rast > 0
#       
#       # Count the number of exposed cells
#       num_exposed <- sum(terra::values(mask), na.rm = TRUE)
#       
#       # Calculate cell area based on resolution
#       res_vals <- res(event_rast)
#       if (is.lonlat(event_rast)) {
#         cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
#       } else {
#         cell_area <- (res_vals[1] * res_vals[2]) / 1e6
#       }
#       
#       exposed_area <- num_exposed * cell_area
#       
#       # --- Extract Exposed Counties/States ---
#       event_cells <- which(terra::values(mask) != 0)
#       if (length(event_cells) > 0) {
#         coords <- terra::xyFromCell(event_rast, event_cells)
#         extracted <- terra::extract(us_states_rast, coords)
#         layer_name <- names(us_states_rast)[1]
#         unique_regions <- unique(extracted[[layer_name]])
#         exposed_counties <- paste(unique_regions, collapse = ";")
#       }
#     } else {
#       warning("File not found: ", nc_file_path, 
#               ". Skipping exposed area and county extraction for cluster ", cluster_id)
#     }
#     
#     # Combine metrics into a data frame row
#     summary_list[[i]] <- data.frame(
#       cluster_id       = cluster_id,
#       start_date       = start_date,
#       end_date         = end_date,
#       duration         = duration,
#       exposed_counties = exposed_counties,
#       exposed_area     = exposed_area,
#       era5_mean        = era5_mean,
#       era5_median      = era5_median,
#       era5_max         = era5_max,
#       stringsAsFactors = FALSE
#     )
#     
#     setTxtProgressBar(pb, i)
#   }
#   
#   close(pb)
#   
#   summary_df <- do.call(rbind, summary_list)
#   
#   if (!is.null(output_csv_path)) {
#     write.csv(summary_df, file = output_csv_path, row.names = FALSE)
#   }
#   
#   return(summary_df)
# }
# 
# ### 0.25 ----
# summary_df <- create_cluster_summary(
#   lookup_file_path = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','lookup_table.csv'),
#   cluster_file_path = here('data','output','03_cluster','02_cluster','heat_index_stm1_clustered_extremes.csv'),
#   cluster_folder = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','county'),
#   us_states_rast = us.states.rast,
#   output_csv_path = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','cluster_idf.csv')
# )
# 
# ### 0.39 ----
# summary_df <- create_cluster_summary(
#   lookup_file_path = here('data','output','03_cluster','02_cluster','points','record','heat_index','lookup_table.csv'),
#   cluster_file_path = here('data','output','03_cluster','02_cluster','heat_index_record_clustered_extremes.csv'),
#   cluster_folder = here('data','output','03_cluster','02_cluster','points','record','heat_index','county'),
#   us_states_rast = us.states.rast,
#   output_csv_path = here('data','output','03_cluster','02_cluster','points','record','heat_index','cluster_idf.csv')
# )
# 
# # Cluster Impacts ----
# merge_cluster_with_noaa <- function(
#     cluster_idf_path,
#     noaa_summary_path,
#     output_path
# ) {
#   # Load required package for string manipulation
#   # library(stringr)
#   
#   # 1. Read the two CSV files
#   cluster_df <- read.csv(cluster_idf_path, stringsAsFactors = FALSE)
#   noaa_df    <- read.csv(noaa_summary_path, stringsAsFactors = FALSE)
#   
#   # 2. Rename NOAA file date columns: start_time -> start_date, end_time -> end_date
#   names(noaa_df)[names(noaa_df) == "start_time"] <- "start_date"
#   names(noaa_df)[names(noaa_df) == "end_time"]   <- "end_date"
#   
#   # 3. Parse the date columns using the yyyy-mm-dd format for both files
#   cluster_df$start_date <- as.Date(trimws(cluster_df$start_date), format = "%Y-%m-%d")
#   cluster_df$end_date   <- as.Date(trimws(cluster_df$end_date),   format = "%Y-%m-%d")
#   
#   noaa_df$start_date <- as.Date(trimws(noaa_df$start_date), format = "%Y-%m-%d")
#   noaa_df$end_date   <- as.Date(trimws(noaa_df$end_date),   format = "%Y-%m-%d")
#   
#   # 4. Initialize new NOAA-related columns in the cluster data frame
#   cluster_df$NOAA_episode      <- NA_character_
#   cluster_df$injuries_direct   <- 0
#   cluster_df$injuries_indirect <- 0
#   cluster_df$deaths_direct     <- 0
#   cluster_df$deaths_indirect   <- 0
#   cluster_df$damage_property   <- 0
#   cluster_df$damage_crops      <- 0
#   
#   # 5. Helper function for exact county matching:
#   #    It splits the semicolon-delimited strings, trims whitespace, converts to lower case,
#   #    and returns TRUE if any chunk in the cluster string exactly appears in the NOAA string.
#   has_exact_county_overlap <- function(cluster_counties_str, noaa_counties_str) {
#     if (is.na(cluster_counties_str) || is.na(noaa_counties_str) ||
#         nchar(cluster_counties_str) == 0 || nchar(noaa_counties_str) == 0) {
#       return(FALSE)
#     }
#     cluster_counties <- tolower(str_trim(unlist(strsplit(cluster_counties_str, ";"))))
#     noaa_counties    <- tolower(str_trim(unlist(strsplit(noaa_counties_str, ";"))))
#     return(length(intersect(cluster_counties, noaa_counties)) > 0)
#   }
#   
#   # 6. Create a progress bar for processing each row in the cluster data
#   pb <- txtProgressBar(min = 0, max = nrow(cluster_df), style = 3)
#   
#   # 7. Loop through each row in the cluster data frame
#   for (i in seq_len(nrow(cluster_df))) {
#     c_start    <- cluster_df$start_date[i]
#     c_end      <- cluster_df$end_date[i]
#     c_counties <- cluster_df$exposed_counties[i]
#     
#     # If either date is NA, skip processing this row
#     if (is.na(c_start) || is.na(c_end)) {
#       setTxtProgressBar(pb, i)
#       next
#     }
#     
#     # Subset NOAA rows that overlap in time.
#     # Condition: cluster start_date <= NOAA end_date AND cluster end_date >= NOAA start_date.
#     noaa_sub <- subset(
#       noaa_df,
#       !is.na(start_date) & !is.na(end_date) &
#         (c_start <= end_date) & (c_end >= start_date)
#     )
#     
#     # Further filter to NOAA rows that share at least one exact county match
#     if (!is.na(c_counties) && nchar(c_counties) > 0 && nrow(noaa_sub) > 0) {
#       keep_index <- sapply(noaa_sub$exposed_counties, function(x) {
#         has_exact_county_overlap(c_counties, x)
#       })
#       noaa_matched <- noaa_sub[keep_index, ]
#     } else {
#       noaa_matched <- data.frame()
#     }
#     
#     # If any NOAA rows matched, update the NOAA-related columns in the cluster row
#     if (nrow(noaa_matched) > 0) {
#       cluster_df$NOAA_episode[i]      <- paste(noaa_matched$episode_id, collapse = ";")
#       cluster_df$injuries_direct[i]   <- sum(noaa_matched$injuries_direct, na.rm = TRUE)
#       cluster_df$injuries_indirect[i] <- sum(noaa_matched$injuries_indirect, na.rm = TRUE)
#       cluster_df$deaths_direct[i]     <- sum(noaa_matched$deaths_direct, na.rm = TRUE)
#       cluster_df$deaths_indirect[i]   <- sum(noaa_matched$deaths_indirect, na.rm = TRUE)
#       cluster_df$damage_property[i]   <- sum(noaa_matched$damage_property, na.rm = TRUE)
#       cluster_df$damage_crops[i]      <- sum(noaa_matched$damage_crops, na.rm = TRUE)
#     }
#     
#     # Update the progress bar
#     setTxtProgressBar(pb, i)
#   }
#   
#   # 8. Close the progress bar
#   close(pb)
#   
#   # 9. Write the merged data frame to the output CSV file
#   write.csv(cluster_df, file = output_path, row.names = FALSE)
#   
#   # Return the final merged data frame
#   return(cluster_df)
# }
# 
# ### 0.25 ----
# #### Excess Heat ----
# merged_df <- merge_cluster_with_noaa(
#   cluster_idf_path   = here::here("data","output","03_cluster","02_cluster","points","stm1","heat_index","cluster_idf.csv"),
#   noaa_summary_path  = here::here("data","output","05_validation","summary","NOAA_excess_heat_summary.csv"),
#   output_path        = here::here("data","output","05_validation","summary","cluster_stm1_excess_heat_summary.csv")
# )
# 
# #### Heat ----
# merged_df <- merge_cluster_with_noaa(
#   cluster_idf_path   = here::here("data","output","03_cluster","02_cluster","points","stm1","heat_index","cluster_idf.csv"),
#   noaa_summary_path  = here::here("data","output","05_validation","summary","NOAA_heat_summary.csv"),
#   output_path        = here::here("data","output","05_validation","summary","cluster_stm1_heat_summary.csv")
# )
# 
# ### 0.39 ----
# #### Excess Heat ----
# merged_df <- merge_cluster_with_noaa(
#   cluster_idf_path   = here::here("data","output","03_cluster","02_cluster","points","record","heat_index","cluster_idf.csv"),
#   noaa_summary_path  = here::here("data","output","05_validation","summary","NOAA_excess_heat_summary.csv"),
#   output_path        = here::here("data","output","05_validation","summary","cluster_record_excess_heat_summary.csv")
# )
# 
# #### Heat ----
# merged_df <- merge_cluster_with_noaa(
#   cluster_idf_path   = here::here("data","output","03_cluster","02_cluster","points","record","heat_index","cluster_idf.csv"),
#   noaa_summary_path  = here::here("data","output","05_validation","summary","NOAA_heat_summary.csv"),
#   output_path        = here::here("data","output","05_validation","summary","cluster_record_heat_summary.csv")
# )
