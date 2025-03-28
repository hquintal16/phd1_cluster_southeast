#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: subset NOAA events to southeast
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/07_noaa_southeast.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

process_netcdf_seq <- function(input_directory, output_directory, reference_extent) {
  # Start timing
  start_time <- Sys.time()
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  # List all NetCDF files in the input directory
  nc_files <- list.files(input_directory, pattern = "\\.nc$", full.names = TRUE)
  if (length(nc_files) == 0) {
    stop("No NetCDF files found in the input directory.")
  }
  
  # Pre-compute the numeric extent from the reference spatraster
  ref_ext <- ext(reference_extent)
  
  # Set up a progress bar
  pb <- txtProgressBar(min = 0, max = length(nc_files), style = 3)
  
  # Process each file sequentially
  for (i in seq_along(nc_files)) {
    file <- nc_files[i]
    
    tryCatch({
      # Read only the first layer for an efficient check.
      r_first <- rast(file, lyrs = 1)
      
      # Crop the first layer to the reference extent.
      cropped_first <- crop(r_first, ref_ext)
      
      # Check if any non-zero value exists (ignoring NA values)
      if (any(cropped_first[] != 0, na.rm = TRUE)) {
        # If non-zero values exist, read the full raster.
        r_full <- rast(file)
        # Crop the full raster to the reference extent.
        cropped_full <- crop(r_full, ref_ext)
        # Construct the output file path with the same file name.
        out_file <- file.path(output_directory, basename(file))
        # Write the cropped raster to a NetCDF file using writeCDF.
        writeCDF(cropped_full, filename = out_file, overwrite = TRUE)
      }
    }, error = function(e) {
      warning(paste("Error processing", file, ":", e$message))
    }, finally = {
      gc()  # Force garbage collection to reduce memory issues.
    })
    
    # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # Compute and display the total processing time
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")
  cat("Total processing time:", total_time, "minutes\n")
}


# Subset NOAA events to Southeast for each hazard type.
# The reference domain used below is a daily maximum heat index file.

# Define the domain and reference extent (using the new file root)
domain <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))
ref_extent <- domain[[1]] / domain[[1]]

# Excess Heat ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "excess_heat")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "excess_heat")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Flash Flood ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "flash_flood")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "flash_flood")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Flood ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "flood")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "flood")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Heat ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "heat")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "heat")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Heavy Rain ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "heavy_rain")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "heavy_rain")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Hurricane ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "hurricane")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "hurricane")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Tropical Depression ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "tropical_depression")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "tropical_depression")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Tropical Storm ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "tropical_storm")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "tropical_storm")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Typhoon ----
input_directory <- here::here("data", "output", "04_noaa", "conus", "typhoon")
output_directory <- here::here("data", "output", "04_noaa", "southeast", "typhoon")
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)
