#Setup----
#Updated February 2025
#Linked to GitHub
#Hunter Quintal
#purpose: summarize NOAA events
# 1. daily extent of all events
# 2. spatial extent and frequency of all events
# 3. temporal duration of all events
#study area: Southeast

# Load Libraries & Functions ----
source("V:/users/hquintal/phd2_southeast/scripts/01_library.R")

# Load spatraster with correct crs 
region.crs <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/07_event_final/stm1/heat_index/1940-07-21_event_0001.nc')

# Create US land mask
us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
us.states.rast <- stars::st_rasterize(us.states %>% dplyr::select(1, geom))
us.states.rast <- terra::rast(us.states.rast)
us.states.rast <- us.states.rast / us.states.rast
terra::crs(us.states.rast) <- terra::crs(region.crs)
us.states.rast <- terra::crop(us.states.rast, region.crs)
us.states.rast <- terra::extend(us.states.rast, region.crs)
plot(us.states.rast)

# UPDATED 2025-03-05 ----
process_noaa_events <- function(noaa_dirs, output_path, hazard_name) {
  total_start_time <- Sys.time()
  
  cat("\n[Step 1] Listing & Grouping NetCDF Files by Year...\n")
  step_start <- Sys.time()
  
  noaa_files <- unlist(lapply(noaa_dirs, list.files, full.names = TRUE, pattern = "\\.nc$"))
  
  # Extract Year (First 4 Digits) from Filenames
  file_years <- as.integer(str_extract(basename(noaa_files), "^(\\d{4})"))
  valid_indices <- !is.na(file_years)
  
  noaa_files <- noaa_files[valid_indices]
  file_years <- file_years[valid_indices]
  
  # Split Files by Year into Named List
  yearly_files_list <- split(noaa_files, file_years)
  
  step_end <- Sys.time()
  cat("[Step 1 Completed] Time Taken:", difftime(step_end, step_start, units = "mins"), "minutes\n\n")
  
  # ---- Process Each Year Separately ----
  for (year in names(yearly_files_list)) {
    cat("\nProcessing Year:", year, "\n")
    yearly_files <- yearly_files_list[[year]]
    
    # ---- Step 2: Event Extent ----
    final_raster <- NULL
    pb <- txtProgressBar(min = 0, max = length(yearly_files), style = 3)
    
    for (i in seq_along(yearly_files)) {
      file <- yearly_files[i]
      r <- try(rast(file), silent = TRUE)
      if (inherits(r, "try-error")) next
      
      r_binary <- app(r, function(x) as.integer(x > 0))
      r_extent <- max(r_binary, na.rm = TRUE)
      
      if (is.null(final_raster)) {
        final_raster <- r_extent
      } else {
        final_raster <- final_raster + r_extent
      }
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    # Save yearly event extent raster
    save.cdf(raster = final_raster,
             folder.path = output_path,
             file.name = paste0('NOAA_', hazard_name, '_Event_Extent_', year),
             var.name = 'event',
             long.name = 'Maximum extent of events',
             unit = 'event')
    
    # ---- Step 3: Daily Extent ----
    cat("\n[Step 3] Computing Daily Extent for Year", year, "...\n")
    step_start <- Sys.time()
    
    daily_rasters <- rast(yearly_files)
    daily_rasters <- daily_rasters / daily_rasters
    daily_rasters[is.na(daily_rasters)] <- 0
    
    if (!is.null(terra::time(daily_rasters))) {
      daily_rasters <- tapp(daily_rasters, 'day', fun = max)
    } else {
      warning("Skipping tapp step for year", year, "due to missing time metadata.")
    }
    
    step_end <- Sys.time()
    cat("[Step 3 Completed] Time Taken:", difftime(step_end, step_start, units = "mins"), "minutes\n\n")
    
    save.cdf(raster = daily_rasters,
             folder.path = output_path,
             file.name = paste0('NOAA_', hazard_name, '_Daily_Extent_', year),
             var.name = 'event',
             long.name = 'Daily extent of events',
             unit = 'event')
    
    # ---- Step 4: Event Duration ----
    cat("\n[Step 4] Computing Event Duration for Year", year, "...\n")
    step_start <- Sys.time()
    
    daily_rasters_sum <- sum(daily_rasters, na.rm = TRUE)
    
    step_end <- Sys.time()
    cat("[Step 4 Completed] Time Taken:", difftime(step_end, step_start, units = "mins"), "minutes\n\n")
    
    save.cdf(raster = daily_rasters_sum,
             folder.path = output_path,
             file.name = paste0('NOAA_', hazard_name, '_Event_Duration_', year),
             var.name = 'event',
             long.name = 'Duration (days) and extent of events',
             unit = 'days')
    
    gc()
  }
  
  total_end_time <- Sys.time()
  cat("\n✅ [All Steps Completed] Total Processing Time:", difftime(total_end_time, total_start_time, units = "mins"), "minutes\n")
}

# ---- All Heat ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_summary2"
noaa_dirs <- c("V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/heat/",
               "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/excess_heat/")
process_noaa_events(noaa_dirs, output_path, hazard_name = 'All_Heat')
gc()

# ---- All Precipitation ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_summary2"
noaa_dirs <- c("V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/flash_flood/",
               "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/flood/",
               "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/heavy_rain/",
               "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/hurricane/",
               "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/tropical_depression/",
               "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/tropical_storm/",
               "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/typhoon/")
process_noaa_events(noaa_dirs, output_path, hazard_name = 'All_Precipitation')
gc()

# UPDATED 2025-03-10 ----
# Calculate # NOAA events SE US ----

# Define function to check for non-zero, non-NA values in NetCDF files
check_nonzero_netcdf <- function(folder, mask_rast) {
  # List all NetCDF files in the folder
  nc_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  
  count_nonzero <- 0  # Counter for files with non-zero values
  
  for (nc_file in nc_files) {
    # Load the NetCDF file as a SpatRaster
    nc_rast <- try(terra::rast(nc_file), silent = TRUE)
    
    if (inherits(nc_rast, "try-error")) {
      warning(paste("Skipping file due to error:", nc_file))
      next
    }
    
    # Ensure CRS matches
    terra::crs(nc_rast) <- terra::crs(mask_rast)
    
    # Align extent & resolution
    nc_rast <- terra::extend(nc_rast, mask_rast)  
    nc_rast <- terra::crop(nc_rast, mask_rast)    
    nc_rast <- terra::resample(nc_rast, mask_rast, method = "bilinear")  
    
    # Mask and check for non-zero values
    nc_rast <- terra::mask(nc_rast, mask_rast)
    values <- values(nc_rast, na.rm = TRUE)
    
    if (any(values != 0, na.rm = TRUE)) {
      count_nonzero <- count_nonzero + 1
    }
  }
  
  return(count_nonzero)
}

# Function to process multiple folders with a progress bar
check_folders_nonzero_netcdf <- function(folders, mask_rast) {
  results <- pblapply(folders, function(folder) {
    count <- check_nonzero_netcdf(folder, mask_rast)
    return(data.frame(Folder = folder, NonZeroFiles = count))
  })
  
  # Combine results into a dataframe
  results_df <- bind_rows(results)
  return(results_df)
}

# Example usage
folder_paths <-c("V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/heat/")

# # ,
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/excess_heat/",
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/flash_flood/",
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/flood/",
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/heavy_rain/",
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/hurricane/",
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/tropical_depression/",
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/tropical_storm/",
# "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/typhoon/"

num_files_per_folder <- check_folders_nonzero_netcdf(folder_paths, us.states.rast)

print(num_files_per_folder)

# HCQ RESTART ----
library(terra)
library(parallel)
library(dplyr)

# Define function to check for non-zero, non-NA values in NetCDF files
check_nonzero_netcdf <- function(folder, mask_rast) {
  # List all NetCDF files in the folder
  nc_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  
  count_nonzero <- 0  # Counter for files with non-zero values
  
  for (nc_file in nc_files) {
    # Load the NetCDF file as a SpatRaster
    nc_rast <- try(terra::rast(nc_file), silent = TRUE)
    
    if (inherits(nc_rast, "try-error")) {
      warning(paste("Skipping file due to error:", nc_file))
      next
    }
    
    # Ensure CRS matches
    terra::crs(nc_rast) <- terra::crs(mask_rast)
    
    # Align extent & resolution
    nc_rast <- terra::extend(nc_rast, mask_rast)  
    nc_rast <- terra::crop(nc_rast, mask_rast)    
    nc_rast <- terra::resample(nc_rast, mask_rast, method = "bilinear")  
    
    # Mask and check for non-zero values
    nc_rast <- terra::mask(nc_rast, mask_rast)
    values <- values(nc_rast, na.rm = TRUE)
    
    if (any(values != 0, na.rm = TRUE)) {
      count_nonzero <- count_nonzero + 1
    }
  }
  
  return(count_nonzero)
}

# Function to process multiple folders with parallelization
check_folders_nonzero_netcdf <- function(folders, mask_rast, num_cores = 4) {
  # Using mclapply for parallel processing
  results <- mclapply(folders, function(folder) {
    count <- check_nonzero_netcdf(folder, mask_rast)
    return(data.frame(Folder = folder, NonZeroFiles = count))
  }, mc.cores = num_cores)
  
  # Combine results into a dataframe
  results_df <- bind_rows(results)
  return(results_df)
}

# Example usage
folder_paths <- c("V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/heat/")

# You can adjust the number of cores based on your system's capacity
num_files_per_folder <- check_folders_nonzero_netcdf(folder_paths, us.states.rast, num_cores = 4)

# View the results
print(num_files_per_folder)

# GET SUBSET OF NOAA EVENTS IN SOUTHEAST USING PARALLEL ----

process_noaa_events_southeast <- function(input_dir,output_dir,ref_raster,num_cores = 19) {
  
  # Troubleshoot
  input_dir <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/hurricane/"
  output_dir <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/hurricane/"
  ref_raster <- us.states.rast
  
  # Start time
  start_time <- Sys.time()
  
  # List all NetCDF files
  nc_files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)
  
  # Setup progress bar
  progressr::handlers(global = TRUE)
  progressr::with_progress({
    p <- progressr::progressor(along = nc_files)
    
    # Set up parallel processing
    plan(multisession, workers = 19)  # Use one less than available cores
    
    results <- future_lapply(seq_along(nc_files), function(i) {
      
      # Update progress bar
      p(sprintf("Processing %d/%d: %s", i, length(nc_files), basename(nc_files)))
      
      i <- 1
      
      # Get current file
      nc_file <- nc_files[[i]]
      
      # Read NetCDF file as raster
      nc_rast <- terra::rast(nc_file)
        
      # Ensure CRS matches
      terra::crs(nc_rast) <- terra::crs(ref_raster)
      
      # Align extent & resolution
      nc_rast <- terra::extend(nc_rast, ref_raster)  
      nc_rast <- terra::crop(nc_rast, ref_raster)    
      nc_rast <- terra::resample(nc_rast, ref_raster, method = "max")  
      
      # Mask and check for non-zero values
      nc_rast <- terra::mask(nc_rast, ref_raster)
      values <- values(nc_rast, na.rm = TRUE)
      
      if (any(values != 0, na.rm = TRUE)) {
        
        # save cdf to new location
        terra::writeCDF(x = nc_rast, filename = paste0(output_dir,basename(nc_files[[i]])),overwrite=TRUE, varname="NOAA event", 
                        longname="NOAA event ID", unit="ID")

      }
      
      gc()
    }, future.seed = TRUE)  # Ensure reproducibility in parallel
    
  })
  
  # Stop time and elapsed time
  stop_time <- Sys.time()
  elapsed_time <- stop_time - start_time
  
  print(paste("Processing started at:", start_time))
  print(paste("Processing completed at:", stop_time))
  print(paste("Total elapsed time:", elapsed_time))
  
}

process_noaa_events_southeast(input_dir <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/hurricane/",
                              output_dir <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/hurricane/",
                              ref_raster <- us.states.rast)

# Trial # 2 ----
process_noaa_events_southeast <- function(input_dir, output_dir, ref_raster, num_cores = 19) {
  
  # Start time
  start_time <- Sys.time()
  
  # List all NetCDF files
  nc_files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)
  
  # Set up parallel processing plan
  plan(multisession, workers = num_cores)  # Adjusted workers to number of cores
  
  # Setup progress bar
  progressr::handlers(global = TRUE)
  progressr::with_progress({
    p <- progressr::progressor(along = nc_files)
    
    # Use future_lapply for parallel processing
    results <- future_lapply(nc_files, function(nc_file) {
      
      # Update progress bar
      p(paste("Processing:", basename(nc_file)))
      
      # Read NetCDF file as raster inside the worker
      nc_rast <- try(terra::rast(nc_file), silent = TRUE)
      
      if (inherits(nc_rast, "try-error")) {
        warning(paste("Error reading file:", nc_file))
        return(NULL)  # Skip this file if error occurs
      }
      
      # Ensure CRS matches (ref_raster is passed explicitly)
      terra::crs(nc_rast) <- terra::crs(ref_raster)
      
      # Align extent & resolution
      nc_rast <- terra::extend(nc_rast, ref_raster)  
      nc_rast <- terra::crop(nc_rast, ref_raster)    
      nc_rast <- terra::resample(nc_rast, ref_raster, method = "max")  
      
      # Mask and check for non-zero values
      nc_rast <- terra::mask(nc_rast, ref_raster)
      values <- values(nc_rast, na.rm = TRUE)
      
      if (any(values != 0, na.rm = TRUE)) {
        
        # Write to output
        output_file <- file.path(output_dir, basename(nc_file))
        terra::writeCDF(x = nc_rast, filename = output_file, overwrite = TRUE, 
                        varname = "NOAA event", longname = "NOAA event ID", unit = "ID")
      }
      
      gc()
      
    }, future.seed = TRUE)  # Ensuring reproducibility
    
  })
  
  # Stop time and elapsed time
  stop_time <- Sys.time()
  elapsed_time <- stop_time - start_time
  
  print(paste("Processing started at:", start_time))
  print(paste("Processing completed at:", stop_time))
  print(paste("Total elapsed time:", elapsed_time))
}

# Example usage
process_noaa_events_southeast(input_dir = "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA/hurricane/",
                              output_dir = "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/hurricane/",
                              ref_raster = us.states.rast)
