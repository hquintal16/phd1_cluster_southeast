#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: subset NOAA events to southeast
#outputs saved in folder: V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast
#study area: Southeast

# Load Libraries & Functions ----
source("V:/users/hquintal/phd2_southeast/scripts/01_library.R")

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

# Excess Heat ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/excess_heat'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/excess_heat'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Flash Flood ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/flash_flood'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/flash_flood'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Flood ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/flood'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/flood'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Heat ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/heat'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/heat'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Heavy Rain ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/heavy_rain'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/heavy_rain'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Hurricane ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/hurricane'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/hurricane'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Tropical Depression ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/tropical_depression'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/tropical_depression'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Tropical Storm ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/tropical_storm'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/tropical_storm'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)

# Typhoon ----
domain <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')
ref_extent <- domain[[1]]/domain[[1]]
input_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/typhoon'
output_directory <- 'V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/typhoon'
process_netcdf_seq(input_directory = input_directory, output_directory = output_directory, reference_extent = ref_extent)