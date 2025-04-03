#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Calculate cluster idf + exposure 

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/20_cluster_idf.R")
source(here::here("scripts", "01_library.R"))

create_lookup_table <- function(input_directory, output_csv_path) {
  # Resolve the input directory using here::here
  in_dir <- here::here(input_directory)
  
  # List all .nc files with the naming convention yyyy-mm-dd_cluster_x.nc
  files <- list.files(
    path = in_dir,
    pattern = "^\\d{4}-\\d{2}-\\d{2}_cluster_\\d+\\.nc$",
    full.names = TRUE
  )
  
  # Check if any files were found
  if (length(files) == 0) {
    message("No matching .nc files found in ", in_dir)
    return(NULL)
  }
  
  # Extract the date from each filename (assumes the date is the first 10 characters)
  file_dates <- as.Date(substr(basename(files), 1, 10))
  
  # Order files by date, and use the filename as a tiebreaker to ensure reproducibility
  sorted_order <- order(file_dates, basename(files))
  files_sorted <- files[sorted_order]
  
  # Build the lookup table
  lookup_table <- data.frame(
    original_file   = basename(files_sorted),
    original_index  = sub(".*_cluster_(\\d+)\\.nc$", "\\1", basename(files_sorted)),
    new_index       = sprintf("%04d", seq_along(files_sorted)),
    new_file        = paste0(substr(basename(files_sorted), 1, 10), "_cluster_", sprintf("%04d", seq_along(files_sorted)), ".nc"),
    stringsAsFactors = FALSE
  )
  
  # Write the lookup table to a CSV file
  write.csv(lookup_table, file = output_csv_path, row.names = FALSE)
  
  # Return the lookup table (invisibly)
  invisible(lookup_table)
}

### 0.25 ----
create_lookup_table(
  input_directory = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','raw'),
  output_csv_path = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','lookup_table.csv')
)
### 0.39 ----
create_lookup_table(
  input_directory = here('data','output','03_cluster','02_cluster','points','record','heat_index','raw'),
  output_csv_path = here('data','output','03_cluster','02_cluster','points','record','heat_index','lookup_table.csv')
)

# Load a reference raster to set the CRS
region.crs <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))

# Read in the Koppen-Geiger geotiff and create a "resolution" raster
directory <- here::here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")
resolution <- terra::rast(directory)
resolution <- resolution / resolution

# Load US county boundaries using the maps package, convert to an sf object, then to a SpatVector
us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
us.states.vect <- terra::vect(us.states)

# Rasterize using the resolution from the Koppen-Geiger file (this will have character values)
us.states.rast <- terra::rasterize(us.states.vect, resolution, field = 'ID')
us.states.rast <- terra::crop(us.states.rast, terra::ext(region.crs))

# create_cluster_summary <- function(lookup_file_path, cluster_file_path, cluster_folder, us_states_rast, output_csv_path = NULL) {
#   # Load required package(s)
#   # library(terra)
#   
#   # Read the CSV files
#   lookup <- read.csv(lookup_file_path, stringsAsFactors = FALSE)
#   cluster_data <- read.csv(cluster_file_path, stringsAsFactors = FALSE)
#   
#   # Convert the 'date' column to Date (assumes format like "6/27/1940")
#   cluster_data$date <- as.Date(cluster_data$date, format = "%m/%d/%Y")
#   
#   # Ensure the lookup table is processed in sequential order (by new_index)
#   lookup <- lookup[order(lookup$new_index), ]
#   
#   # Initialize list to store summary rows for each cluster
#   summary_list <- list()
#   
#   # Create a progress bar with a maximum value equal to the number of clusters
#   pb <- txtProgressBar(min = 0, max = nrow(lookup), style = 3)
#   
#   # Loop over each cluster (each row in the lookup table)
#   for (i in seq_len(nrow(lookup))) {
#     current_lookup <- lookup[i, ]
#     cluster_id <- current_lookup$new_index  # new_index becomes the cluster_id
#     original_index <- as.character(current_lookup$original_index)
#     
#     # Subset cluster_data where 'cluster' equals the original index
#     subset_cluster <- subset(cluster_data, as.character(cluster) == original_index)
#     
#     # Calculate event dates and ERA5 statistics if data exists; otherwise, assign NA
#     if (nrow(subset_cluster) > 0) {
#       start_date <- min(subset_cluster$date, na.rm = TRUE)
#       end_date   <- max(subset_cluster$date, na.rm = TRUE)
#       duration   <- as.numeric(end_date - start_date)
#       era5_mean  <- mean(subset_cluster$observation, na.rm = TRUE)
#       era5_median<- median(subset_cluster$observation, na.rm = TRUE)
#       era5_max   <- max(subset_cluster$observation, na.rm = TRUE)
#     } else {
#       start_date <- NA
#       end_date   <- NA
#       duration   <- NA
#       era5_mean  <- NA
#       era5_median<- NA
#       era5_max   <- NA
#     }
#     
#     # Build the file name. First try using new_file from the lookup table.
#     file_name_candidate <- current_lookup$new_file
#     nc_file_path <- file.path(cluster_folder, file_name_candidate)
#     
#     # If the file is not found, prepend "county_event_" to the file name.
#     if (!file.exists(nc_file_path)) {
#       file_name_candidate <- paste0("county_event_", current_lookup$new_file)
#       nc_file_path <- file.path(cluster_folder, file_name_candidate)
#     }
#     
#     # Initialize exposed_area and exposed_counties as NA by default
#     exposed_area <- NA
#     exposed_counties <- NA
#     
#     # If the file exists, proceed to calculate the exposed area and counties.
#     if (file.exists(nc_file_path)) {
#       # Read in the .nc file as a raster
#       event_rast <- rast(nc_file_path)
#       
#       # --- Exposed Area Calculation ---
#       # Sum over layers (if needed) and create a binary mask (nonzero cells)
#       noaa_sum <- app(event_rast, fun = sum, na.rm = TRUE)
#       event_mask_binary <- noaa_sum != 0
#       
#       # Calculate cell area, accounting for geographic coordinates if applicable
#       res_vals <- res(event_rast)
#       if (is.lonlat(event_rast)) {
#         cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
#       } else {
#         cell_area <- (res_vals[1] * res_vals[2]) / 1e6
#       }
#       exposed_area <- sum(values(event_mask_binary), na.rm = TRUE) * cell_area
#       
#       # --- Extract Exposed Counties/States ---
#       event_cells <- which(values(event_mask_binary) != 0)
#       if (length(event_cells) > 0) {
#         # Use terra::xyFromCell to get coordinates (ensuring a matrix with columns x and y)
#         coords <- terra::xyFromCell(event_rast, event_cells)
#         # Explicitly call terra::extract so the correct method for SpatRaster is used
#         extracted <- terra::extract(us_states_rast, coords)
#         # Assume the region names are stored in the first layer of the raster
#         layer_name <- names(us_states_rast)[1]
#         unique_regions <- unique(extracted[[layer_name]])
#         exposed_counties <- paste(unique_regions, collapse = ";")
#       }
#     } else {
#       warning("File not found: ", nc_file_path, ". Skipping exposed area and county extraction for cluster ", cluster_id)
#     }
#     
#     # Combine the metrics for the current cluster into a data frame row
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
#     # Update the progress bar
#     setTxtProgressBar(pb, i)
#   }
#   
#   # Close the progress bar
#   close(pb)
#   
#   # Combine all rows into one summary data frame
#   summary_df <- do.call(rbind, summary_list)
#   
#   # Optionally write the summary to a CSV file if an output path is provided
#   if (!is.null(output_csv_path)) {
#     write.csv(summary_df, file = output_csv_path, row.names = FALSE)
#   }
#   
#   return(summary_df)
# }

create_cluster_summary <- function(
    lookup_file_path,
    cluster_file_path,
    cluster_folder,
    us_states_rast,
    output_csv_path = NULL
) {
  # Load required packages
  # library(terra)
  
  # Read the CSV files
  lookup <- read.csv(lookup_file_path, stringsAsFactors = FALSE)
  cluster_data <- read.csv(cluster_file_path, stringsAsFactors = FALSE)
  
  # Convert the 'date' column to Date (if you still need it for other calculations)
  # Adjust the format string if your dates differ
  cluster_data$date <- as.Date(cluster_data$date, format = "%m/%d/%Y")
  
  # Ensure the lookup table is processed in sequential order (by new_index)
  lookup <- lookup[order(lookup$new_index), ]
  
  # Initialize a list to store summary rows for each cluster
  summary_list <- list()
  
  # Create a progress bar with a maximum value equal to the number of clusters
  pb <- txtProgressBar(min = 0, max = nrow(lookup), style = 3)
  
  # Loop over each cluster (each row in the lookup table)
  for (i in seq_len(nrow(lookup))) {
    current_lookup <- lookup[i, ]
    cluster_id <- current_lookup$new_index      # new_index becomes the cluster_id
    original_index <- as.character(current_lookup$original_index)
    
    # Subset cluster_data where 'cluster' equals the original index
    # (Only used for ERA5 stats now, not for date range)
    subset_cluster <- subset(cluster_data, as.character(cluster) == original_index)
    
    # Calculate ERA5 statistics if data exists in the CSV; otherwise, assign NA
    if (nrow(subset_cluster) > 0) {
      era5_mean   <- mean(subset_cluster$observation, na.rm = TRUE)
      era5_median <- median(subset_cluster$observation, na.rm = TRUE)
      era5_max    <- max(subset_cluster$observation, na.rm = TRUE)
    } else {
      era5_mean   <- NA
      era5_median <- NA
      era5_max    <- NA
    }
    
    # Default date/duration values
    start_date <- NA
    end_date   <- NA
    duration   <- NA
    
    # Build the file name. First try using new_file from the lookup table.
    file_name_candidate <- current_lookup$new_file
    nc_file_path <- file.path(cluster_folder, file_name_candidate)
    
    # If the file is not found, prepend "county_event_" to the file name.
    if (!file.exists(nc_file_path)) {
      file_name_candidate <- paste0("county_event_", current_lookup$new_file)
      nc_file_path <- file.path(cluster_folder, file_name_candidate)
    }
    
    # Initialize exposed_area and exposed_counties as NA by default
    exposed_area <- NA
    exposed_counties <- NA
    
    # If the file exists, proceed to read and extract info
    if (file.exists(nc_file_path)) {
      # Read in the .nc file as a raster
      event_rast <- rast(nc_file_path)
      
      # --- Extract Time Dimension for Start/End Dates ---
      # 'terra::time()' should return a vector of Dates or POSIXct times
      time_vector <- terra::time(event_rast)
      
      # If the time dimension is valid and non-empty, compute start_date, end_date, and duration
      if (!is.null(time_vector) && length(time_vector) > 0) {
        # Convert to Date if it is POSIXct (or numeric “days since…”). 
        # terra often does this automatically if the NetCDF is CF-compliant.
        # If it's already Date, this will be fine.
        time_vector <- as.Date(time_vector)
        
        start_date <- min(time_vector, na.rm = TRUE)
        end_date   <- max(time_vector, na.rm = TRUE)
        duration   <- as.numeric(end_date - start_date)
      }
      
      # --- Exposed Area Calculation ---
      # Sum over layers (if needed) and create a binary mask (nonzero cells)
      noaa_sum <- app(event_rast, fun = sum, na.rm = TRUE)
      event_mask_binary <- noaa_sum != 0
      
      # Calculate cell area, accounting for geographic coordinates if applicable
      res_vals <- res(event_rast)
      if (is.lonlat(event_rast)) {
        cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
      } else {
        cell_area <- (res_vals[1] * res_vals[2]) / 1e6
      }
      exposed_area <- sum(values(event_mask_binary), na.rm = TRUE) * cell_area
      
      # --- Extract Exposed Counties/States ---
      event_cells <- which(values(event_mask_binary) != 0)
      if (length(event_cells) > 0) {
        # Use terra::xyFromCell to get coordinates
        coords <- terra::xyFromCell(event_rast, event_cells)
        # Extract from the provided SpatRaster
        extracted <- terra::extract(us_states_rast, coords)
        # Assume the region names are stored in the first layer
        layer_name <- names(us_states_rast)[1]
        unique_regions <- unique(extracted[[layer_name]])
        exposed_counties <- paste(unique_regions, collapse = ";")
      }
    } else {
      warning("File not found: ", nc_file_path, 
              ". Skipping exposed area and county extraction for cluster ", cluster_id)
    }
    
    # Combine the metrics for the current cluster into a data frame row
    summary_list[[i]] <- data.frame(
      cluster_id       = cluster_id,
      start_date       = start_date,
      end_date         = end_date,
      duration         = duration,
      exposed_counties = exposed_counties,
      exposed_area     = exposed_area,
      era5_mean        = era5_mean,
      era5_median      = era5_median,
      era5_max         = era5_max,
      stringsAsFactors = FALSE
    )
    
    # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  
  # Combine all rows into one summary data frame
  summary_df <- do.call(rbind, summary_list)
  
  # Optionally write the summary to a CSV file if an output path is provided
  if (!is.null(output_csv_path)) {
    write.csv(summary_df, file = output_csv_path, row.names = FALSE)
  }
  
  return(summary_df)
}


### 0.25 ----
summary_df <- create_cluster_summary(
  lookup_file_path = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','lookup_table.csv'),
  cluster_file_path = here('data','output','03_cluster','02_cluster','heat_index_stm1_clustered_extremes.csv'),
  cluster_folder = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','county'),
  us_states_rast = us.states.rast,
  output_csv_path = here('data','output','03_cluster','02_cluster','points','stm1','heat_index','cluster_idf.csv')
)

summary_df <- create_cluster_summary(
  lookup_file_path = here('data','output','03_cluster','02_cluster','points','record','heat_index','lookup_table.csv'),
  cluster_file_path = here('data','output','03_cluster','02_cluster','heat_index_record_clustered_extremes.csv'),
  cluster_folder = here('data','output','03_cluster','02_cluster','points','record','heat_index','county'),
  us_states_rast = us.states.rast,
  output_csv_path = here('data','output','03_cluster','02_cluster','points','record','heat_index','cluster_idf.csv')
)
