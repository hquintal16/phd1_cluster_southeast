#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: resample clusters to counties
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
# here::set_here("V:/users/hquintal/phd1_cluster_southeast")
here::i_am("scripts/14_cluster_county.R")  # Adjust this file path if necessary
source(here::here("scripts", "01_library.R"))

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

# We'll use the vector version for extraction.
county_vect <- us.states.vect

process_nc_to_county <- function(cluster_dirs, county_vect, county_rast, output_dir) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create a numeric template from the county raster (replace values with NA_real_)
  county_template <- county_rast
  values(county_template) <- NA_real_
  
  # List all .nc files from the specified cluster directories
  nc_files <- unlist(lapply(cluster_dirs, function(d) {
    list.files(d, full.names = TRUE, pattern = "\\.nc$")
  }))
  
  # Initialize an overall progress bar for processing all files
  overall_progress <- txtProgressBar(min = 0, max = length(nc_files), style = 3)
  
  # Loop through each NetCDF file
  for (i in seq_along(nc_files)) {
    nc_file <- nc_files[i]
    
    # Attempt to read the .nc file
    r <- try(terra::rast(nc_file), silent = TRUE)
    if (inherits(r, "try-error")) {
      warning("Could not read file: ", nc_file)
      setTxtProgressBar(overall_progress, i)
      next
    }
    
    # Resample the cluster raster to the grid defined by county_template
    r_resampled <- terra::resample(r, county_template, method = "near")
    
    # Extract time metadata (each layer assumed to correspond to a date)
    dates <- terra::time(r_resampled)
    if (is.null(dates)) {
      warning("No time metadata found in file: ", nc_file, "; skipping.")
      setTxtProgressBar(overall_progress, i)
      next
    }
    
    # Process each time layer (inner loop runs silently)
    county_layers <- lapply(1:terra::nlyr(r_resampled), function(j) {
      r_layer <- r_resampled[[j]]
      
      # Extract cell values for each county polygon
      ext_vals <- terra::extract(r_layer, county_vect)
      
      # Aggregate by polygon ID: flag as 1 if any cell is > 0, else 0
      agg <- aggregate(ext_vals[, 2], by = list(ID = ext_vals[, 1]), 
                       FUN = function(x) if (any(x > 0, na.rm = TRUE)) 1 else 0)
      
      # Prepare a vector of binary values for all polygons (default 0)
      values_bin <- rep(0, nrow(county_vect))
      values_bin[agg$ID] <- agg$x
      
      # Rasterize the county vector with these binary values onto r_layer's grid
      terra::rasterize(county_vect, r_layer, field = values_bin, background = 0)
    })
    
    # Combine all layers into a multi-layer SpatRaster and attach time metadata
    county_stack <- terra::rast(county_layers)
    terra::time(county_stack) <- dates
    
    # Create an output file name based on the input file name
    file_base <- basename(nc_file)
    out_file <- file.path(output_dir, paste0("county_event_", file_base))
    
    # Save the processed SpatRaster as a NetCDF file
    terra::writeCDF(county_stack, filename = out_file, varname = "county_event", 
                    longname = "County-level event indicator (1 = event)", unit = "1",
                    overwrite=TRUE)
    
    # Update the overall progress bar
    setTxtProgressBar(overall_progress, i)
  }
  close(overall_progress)
}

# Create county boundaries using your code

## Heat Index Advisory ----

### 0.25 deg / day ----
input_dir <- here('data','output','03_cluster','02_cluster','advisory','points','0.25','heat_index','clean')
output_dir <- here('data','output','03_cluster','02_cluster','advisory','points','0.25','heat_index','county')
process_nc_to_county(cluster_dirs = list(input_dir), county_vect = county_vect, county_rast = us.states.rast, output_dir = output_dir)

### 0.3075 deg / day ----
input_dir <- here('data','output','03_cluster','02_cluster','advisory','points','0.3075','heat_index','clean')
output_dir <- here('data','output','03_cluster','02_cluster','advisory','points','0.3075','heat_index','county')
process_nc_to_county(cluster_dirs = list(input_dir), county_vect = county_vect, county_rast = us.states.rast, output_dir = output_dir)

### 0.39 deg / day ----
input_dir <- here('data','output','03_cluster','02_cluster','advisory','points','0.39','heat_index','clean')
output_dir <- here('data','output','03_cluster','02_cluster','advisory','points','0.39','heat_index','county')
process_nc_to_county(cluster_dirs = list(input_dir), county_vect = county_vect, county_rast = us.states.rast, output_dir = output_dir)

## Heat Index Warning ----

### 0.25 deg / day ----
input_dir <- here('data','output','03_cluster','02_cluster','warning','points','0.25','heat_index','clean')
output_dir <- here('data','output','03_cluster','02_cluster','warning','points','0.25','heat_index','county')
process_nc_to_county(cluster_dirs = list(input_dir), county_vect = county_vect, county_rast = us.states.rast, output_dir = output_dir)

### 0.3075 deg / day ----
input_dir <- here('data','output','03_cluster','02_cluster','warning','points','0.3075','heat_index','clean')
output_dir <- here('data','output','03_cluster','02_cluster','warning','points','0.3075','heat_index','county')
process_nc_to_county(cluster_dirs = list(input_dir), county_vect = county_vect, county_rast = us.states.rast, output_dir = output_dir)

### 0.39 deg / day ----
input_dir <- here('data','output','03_cluster','02_cluster','warning','points','0.39','heat_index','clean')
output_dir <- here('data','output','03_cluster','02_cluster','warning','points','0.39','heat_index','county')
process_nc_to_county(cluster_dirs = list(input_dir), county_vect = county_vect, county_rast = us.states.rast, output_dir = output_dir)

## Precipitation ----
process_nc_to_county <- function(cluster_dirs, county_vect, county_rast, output_dir) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create a numeric template from the county raster (replace values with NA_real_)
  county_template <- county_rast
  values(county_template) <- NA_real_

  # List all .nc files from the specified cluster directories
  nc_files <- unlist(lapply(cluster_dirs, function(d) {
    list.files(d, full.names = TRUE, pattern = "\\.nc$")
  }))

  # Initialize an overall progress bar for processing all files
  overall_progress <- txtProgressBar(min = 0, max = length(nc_files), style = 3)

  # Loop through each NetCDF file
  for (i in seq_along(nc_files)) {
    nc_file <- nc_files[i]

    # Attempt to read the .nc file
    r <- try(terra::rast(nc_file), silent = TRUE)
    if (inherits(r, "try-error")) {
      warning("Could not read file: ", nc_file)
      setTxtProgressBar(overall_progress, i)
      next
    }

    # get max daily
    r[is.na(r)] <- 0
    r <- terra::tapp(r,'days',fun=max)

    # Resample the cluster raster to the grid defined by county_template
    r_resampled <- terra::resample(r, county_template, method = "near")

    # Extract time metadata (each layer assumed to correspond to a date)
    dates <- terra::time(r_resampled)
    if (is.null(dates)) {
      warning("No time metadata found in file: ", nc_file, "; skipping.")
      setTxtProgressBar(overall_progress, i)
      next
    }

    # Process each time layer (inner loop runs silently)
    county_layers <- lapply(1:terra::nlyr(r_resampled), function(j) {
      r_layer <- r_resampled[[j]]

      # Extract cell values for each county polygon
      ext_vals <- terra::extract(r_layer, county_vect)

      # Aggregate by polygon ID: flag as 1 if any cell is > 0, else 0
      agg <- aggregate(ext_vals[, 2], by = list(ID = ext_vals[, 1]),
                       FUN = function(x) if (any(x > 0, na.rm = TRUE)) 1 else 0)

      # Prepare a vector of binary values for all polygons (default 0)
      values_bin <- rep(0, nrow(county_vect))
      values_bin[agg$ID] <- agg$x

      # Rasterize the county vector with these binary values onto r_layer's grid
      terra::rasterize(county_vect, r_layer, field = values_bin, background = 0)
    })

    # Combine all layers into a multi-layer SpatRaster and attach time metadata
    county_stack <- terra::rast(county_layers)
    terra::time(county_stack) <- dates

    # Create an output file name based on the input file name
    file_base <- basename(nc_file)
    out_file <- file.path(output_dir, paste0("county_event_", file_base))

    # Save the processed SpatRaster as a NetCDF file
    terra::writeCDF(county_stack, filename = out_file, varname = "county_event",
                    longname = "County-level event indicator (1 = event)", unit = "1",
                    overwrite=TRUE)

    # Update the overall progress bar
    setTxtProgressBar(overall_progress, i)
  }
  close(overall_progress)
}

### 0.25 deg / hour ----

# Define input and output directories using here()
input_dir <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
output_dir <- here('data','output','03_cluster','02_cluster','24hr1yr','points','county')

# Call the function
process_nc_to_county(cluster_dirs = list(input_dir), county_vect = county_vect, county_rast = us.states.rast, output_dir = output_dir)
