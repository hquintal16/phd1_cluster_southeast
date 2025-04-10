#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: summarize Cluster events
# 1. daily extent of all events
# 2. spatial extent and frequency of all events
# 3. temporal duration of all events
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/15_cluster_summary.R")  # Adjust this file path if necessary
source(here::here("scripts", "01_library.R"))

process_county_events <- function(processed_dirs, output_path, hazard_name) {
  total_start_time <- Sys.time()
  
  cat("\n[Step 1] Listing & Grouping Processed County NetCDF Files by Year...\n")
  step_start <- Sys.time()
  
  # List all processed .nc files from the provided directories
  processed_files <- unlist(lapply(processed_dirs, list.files, full.names = TRUE, pattern = "\\.nc$"))
  
  # Extract the first occurrence of a 4-digit number from each file name
  file_years <- as.integer(str_extract(basename(processed_files), "(\\d{4})"))
  valid_indices <- !is.na(file_years)
  processed_files <- processed_files[valid_indices]
  file_years <- file_years[valid_indices]
  
  # Group files by year
  yearly_files_list <- split(processed_files, file_years)
  
  step_end <- Sys.time()
  cat("[Step 1 Completed] Time Taken:", difftime(step_end, step_start, units = "mins"), "minutes\n\n")
  
  # Loop over each year and compute summaries
  for (year in names(yearly_files_list)) {
    cat("\nProcessing Year:", year, "\n")
    yearly_files <- yearly_files_list[[year]]
    
    # Step 2: Compute Event Extent (cumulative county events)
    final_raster <- NULL
    pb <- txtProgressBar(min = 0, max = length(yearly_files), style = 3)
    
    for (i in seq_along(yearly_files)) {
      file <- yearly_files[i]
      r <- try(terra::rast(file), silent = TRUE)
      if (inherits(r, "try-error")) next
      
      # Convert the raster to binary: 1 for event, 0 for no event
      r_binary <- terra::app(r, function(x) as.integer(x > 0))
      # Compute the per-cell maximum across layers using terra::app() with max
      r_extent <- terra::app(r_binary, fun = max, na.rm = TRUE)
      
      if (is.null(final_raster)) {
        final_raster <- r_extent
      } else {
        final_raster <- final_raster + r_extent
      }
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    # Save yearly event extent raster (overwrite if exists)
    terra::writeCDF(final_raster,
                    filename = file.path(output_path, paste0('County_', hazard_name, '_Event_Extent_', year, ".nc")),
                    varname = 'event',
                    longname = 'Maximum extent of county events',
                    unit = 'event',
                    overwrite = TRUE)
    
    # Step 3: Compute Daily Extent across the year
    cat("\n[Step 3] Computing Daily Extent for Year", year, "...\n")
    step_start <- Sys.time()
    
    # Read all files for the year as a multi-layer raster
    daily_rasters <- terra::rast(yearly_files)
    daily_rasters <- daily_rasters / daily_rasters  # binarize: 1 for event, 0 for no event
    daily_rasters[is.na(daily_rasters)] <- 0
    
    # Aggregate by day if time metadata is available
    if (!is.null(terra::time(daily_rasters))) {
      daily_rasters <- terra::tapp(daily_rasters, 'day', fun = max)
    } else {
      warning("Skipping time aggregation for year", year, "due to missing time metadata.")
    }
    
    step_end <- Sys.time()
    cat("[Step 3 Completed] Time Taken:", difftime(step_end, step_start, units = "mins"), "minutes\n\n")
    
    terra::writeCDF(daily_rasters,
                    filename = file.path(output_path, paste0('County_', hazard_name, '_Daily_Extent_', year, ".nc")),
                    varname = 'event',
                    longname = 'Daily extent of county events',
                    unit = 'event',
                    overwrite = TRUE)
    
    # Step 4: Compute Event Duration (sum over daily extents)
    cat("\n[Step 4] Computing Event Duration for Year", year, "...\n")
    step_start <- Sys.time()
    
    daily_rasters_sum <- terra::app(daily_rasters, fun = function(x) sum(x, na.rm = TRUE))
    
    step_end <- Sys.time()
    cat("[Step 4 Completed] Time Taken:", difftime(step_end, step_start, units = "mins"), "minutes\n\n")
    
    terra::writeCDF(daily_rasters_sum,
                    filename = file.path(output_path, paste0('County_', hazard_name, '_Event_Duration_', year, ".nc")),
                    varname = 'event',
                    longname = 'Duration (days) and extent of county events',
                    unit = 'days',
                    overwrite = TRUE)
    
    gc()
  }
  
  total_end_time <- Sys.time()
  cat("\n✅ [All Steps Completed] Total Processing Time:", difftime(total_end_time, total_start_time, units = "mins"), "minutes\n")
}

## Heat index ----

### 0.25 / day ----

processed_dirs <- list(here('data','output','03_cluster','02_cluster','points','0.25','heat_index','county'))
output_path <- here('data','output','03_cluster','02_cluster','points','0.25','heat_index','summary')
hazard_name <- "heat_index"

# Run the function to process the county events
process_county_events(processed_dirs, output_path, hazard_name)

### 0.3075 / day ----

processed_dirs <- list(here('data','output','03_cluster','02_cluster','points','0.3075','heat_index','county'))
output_path <- here('data','output','03_cluster','02_cluster','points','0.3075','heat_index','summary')
hazard_name <- "heat_index"

# Run the function to process the county events
process_county_events(processed_dirs, output_path, hazard_name)

### 0.39 / day ----

processed_dirs <- list(here('data','output','03_cluster','02_cluster','points','0.39','heat_index','county'))
output_path <- here('data','output','03_cluster','02_cluster','points','0.39','heat_index','summary')
hazard_name <- "heat_index"

# Run the function to process the county events
process_county_events(processed_dirs, output_path, hazard_name)

## Precipitation ----

### 0.25 deg / hour ----

processed_dirs <- list(here('data','output','03_cluster','02_cluster','points','stm1','precipitation','county'))
output_path <- here('data','output','03_cluster','02_cluster','points','stm1','precipitation','summary')
hazard_name <- "precipitation"

# Run the function to process the county events
process_county_events(processed_dirs, output_path, hazard_name)
