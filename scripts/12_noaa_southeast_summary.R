#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: summarize NOAA events
# 1. daily extent of all events
# 2. spatial extent and frequency of all events
# 3. temporal duration of all events
#outputs saved in folder: V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast
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

# UPDATED 2025-03-14 ----
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

# Heat ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/heat/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/heat/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Heat')
gc()

# Excess Heat ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/excess_heat/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/excess_heat/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Excess_Heat')
gc()

# Flood ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/flood/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/flood/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Flood')
gc()

# Flash Flood ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/flash_flood/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/flash_flood/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Flash_Flood')
gc()

# Heavy Rain ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/heavy_rain/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/heavy_rain/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Heavy_Rain')
gc()

# Hurricane ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/hurricane/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/hurricane/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Hurricane')
gc()

# Tropical Depression ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/tropical_depression/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/tropical_depression/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Tropical_Depression')
gc()

# Tropical Storm ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/tropical_storm/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/tropical_storm/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Tropical_Storm')
gc()

# Typhoon ----
output_path <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/summary/typhoon/"
noaa_dirs <- "V:/users/hquintal/phd2_southeast/data/output/08_noaa_event/NOAA_southeast/typhoon/"
process_noaa_events(noaa_dirs, output_path, hazard_name = 'Typhoon')
gc()