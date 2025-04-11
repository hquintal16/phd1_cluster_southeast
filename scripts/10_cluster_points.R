#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: Convert gridded time series into space time cube points using 
# space time metric (record median), then compare against thresholds to get 
# extreme points
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/10_cluster_points.R")
source(here::here("scripts", "01_library.R"))

# Heat Index ----
# process_heat_index_files <- function(input_dir, output_dir, 
#                                      threshold_shapefile, 
#                                      space_time_metric,
#                                      num_cores = 19) {
#   
#   # Start time
#   start_time <- Sys.time()
#   
#   # Local helper: create.st.cube defined within the main function
#   create.st.cube <- function(target.raster, space.time.metric) {
#     # Create a temporary raster with adjusted resolution based on space.time.metric
#     temp <- terra::rast(crs = terra::crs(target.raster),
#                         res = terra::res(target.raster) * space.time.metric,
#                         ext = terra::ext(target.raster),
#                         nlyr = terra::nlyr(target.raster))
#     terra::values(temp) <- 0
#     
#     # Resample the target raster to the temporary raster using the max method
#     result <- terra::resample(target.raster, temp, method = 'max')
#     names(result) <- terra::time(target.raster)
#     
#     return(result)
#   }
#   
#   # List all NetCDF files
#   nc_files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)
#   
#   # Read in threshold shapefile as an sf object
#   threshold_sf <- sf::st_as_sf(sf::read_sf(threshold_shapefile))
#   
#   # Determine which space-time metric to use
#   stm.relavent <- switch(space_time_metric,
#                          "annual" = year.heat,
#                          "record" = record.heat,
#                          "month" = month.heat,
#                          "seasonal" = season.heat,
#                          space_time_metric)
#   
#   # Setup progress bar
#   progressr::handlers(global = TRUE)
#   progressr::with_progress({
#     p <- progressr::progressor(along = nc_files)
#     
#     # Set up multisession parallel plan
#     future::plan(future::multisession, workers = num_cores)
#     
#     results <- future.apply::future_lapply(seq_along(nc_files), function(i) {
#       
#       # Load necessary libraries within each worker
#       library(terra)
#       library(sf)
#       library(dplyr)
#       library(tidyr)
#       
#       # Update progress bar
#       p(sprintf("Processing %d/%d: %s", i, length(nc_files), basename(nc_files)))
#       
#       # Get current file
#       nc_file <- nc_files[[i]]
#       
#       # Read NetCDF file as a raster using terra
#       var <- terra::rast(nc_file)
#       
#       # Determine current time slice (assumes stm is defined in your global environment)
#       timeslice <- stm[which(nc_files == nc_file), ]
#       
#       # Select the appropriate space-time metric based on the available columns
#       stm.final <- if ("year" %in% colnames(stm.relavent)) {
#         stm.relavent[stm.relavent$year == timeslice$year, ]$median
#       } else if ("month" %in% colnames(stm.relavent)) {
#         stm.relavent[stm.relavent$month == timeslice$month, ]$median
#       } else if ("season" %in% colnames(stm.relavent)) {
#         stm.relavent[stm.relavent$season == timeslice$season, ]$median
#       } else {
#         stm.relavent
#       }
#       
#       # Create space-time cube using the embedded function
#       cube <- create.st.cube(target.raster = var, space.time.metric = stm.final)
#       # message(paste0('Space Time Cube resolution = ',terra::res(cube)))
#       
#       # Convert the cube to a data frame and then to a long format
#       cube.df <- as.data.frame(cube, xy = TRUE)
#       cube_long <- cube.df %>%
#         pivot_longer(-c(x, y), names_to = "date", values_to = "heat_index") %>%
#         mutate(date = as.Date(date))
#       
#       # Convert long format into an sf object using NAD83 CRS
#       cube_sf <- sf::st_as_sf(cube_long, coords = c("x", "y"), crs = 4269)
#       
#       # Spatial join with the threshold data and manage missing threshold values
#       cube_sf <- sf::st_join(cube_sf, threshold_sf, left = TRUE) %>%
#         mutate(ADVISORY = ifelse(is.na(ADVISORY), min(threshold_sf$ADVISORY, na.rm = TRUE), ADVISORY))
#       
#       # Flag extreme heat indices and set coordinates as lat-long columns
#       cube_sf <- cube_sf %>%
#         mutate(extreme = ifelse(heat_index >= ADVISORY, 1, 0)) %>%
#         mutate(lat = sf::st_coordinates(.)[, 2],
#                long = sf::st_coordinates(.)[, 1]) %>%
#         mutate(threshold = ADVISORY,
#                observation = heat_index) %>%
#         select(lat, long, date, observation, threshold, extreme)
#       
#       # Filter to only extreme values (i.e., extreme == 1)
#       cube_sf_filtered <- dplyr::filter(cube_sf, extreme == 1)
#       
#       # Only write out file if there are extreme observations
#       if (nrow(cube_sf_filtered) > 0) {
#         output_file <- file.path(output_dir, paste0("extreme_", 
#                                                     substr(basename(nc_file), 1, nchar(basename(nc_file))-3), 
#                                                     ".csv"))
#         write.csv(cube_sf_filtered, output_file, row.names = FALSE)
#       }
#       
#       gc()
#     }, future.seed = TRUE)  # Ensure reproducibility in parallel
#   })
#   
#   # Stop time and elapsed time calculations
#   stop_time <- Sys.time()
#   elapsed_time <- stop_time - start_time
#   
#   print(paste("Processing started at:", start_time))
#   print(paste("Processing completed at:", stop_time))
#   print(paste("Total elapsed time:", elapsed_time))
# }

process_heat_index_files <- function(input_dir, output_dir, 
                                     threshold_shapefile, 
                                     space_time_metric,
                                     num_cores = 19) {
  
  # Start time
  start_time <- Sys.time()
  
  # Local helper: create.st.cube defined within the main function
  create.st.cube <- function(target.raster, space.time.metric) {
    # Create a temporary raster with adjusted resolution based on space.time.metric
    temp <- terra::rast(crs = terra::crs(target.raster),
                        res = terra::res(target.raster) * space.time.metric,
                        ext = terra::ext(target.raster),
                        nlyr = terra::nlyr(target.raster))
    terra::values(temp) <- 0
    
    # Resample the target raster to the temporary raster using the max method
    result <- terra::resample(target.raster, temp, method = 'max')
    names(result) <- terra::time(target.raster)
    
    return(result)
  }
  
  # List all NetCDF files
  nc_files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)
  
  # Read in threshold shapefile as an sf object
  threshold_sf <- sf::st_as_sf(sf::read_sf(threshold_shapefile))
  
  # Setup progress bar
  progressr::handlers(global = TRUE)
  progressr::with_progress({
    p <- progressr::progressor(along = nc_files)
    
    # Set up multisession parallel plan
    future::plan(future::multisession, workers = num_cores)
    
    results <- future.apply::future_lapply(seq_along(nc_files), function(i) {
      
      # Load necessary libraries within each worker
      library(terra)
      library(sf)
      library(dplyr)
      library(tidyr)
      
      # Update progress bar
      p(sprintf("Processing %d/%d: %s", i, length(nc_files), basename(nc_files)))
      
      # Get current file
      nc_file <- nc_files[[i]]
      
      # Read NetCDF file as a raster using terra
      var <- terra::rast(nc_file)
      
      # Directly use the provided numeric space_time_metric in the create.st.cube function
      cube <- create.st.cube(target.raster = var, space.time.metric = space_time_metric)
      
      # Convert the cube to a data frame and then reshape to long format
      cube.df <- as.data.frame(cube, xy = TRUE)
      cube_long <- cube.df %>%
        pivot_longer(-c(x, y), names_to = "date", values_to = "heat_index") %>%
        mutate(date = as.Date(date))
      
      # Convert long format into an sf object using NAD83 CRS
      cube_sf <- sf::st_as_sf(cube_long, coords = c("x", "y"), crs = 4269)
      
      # Spatial join with the threshold data and manage missing threshold values
      cube_sf <- sf::st_join(cube_sf, threshold_sf, left = TRUE) %>%
        mutate(ADVISORY = ifelse(is.na(ADVISORY), min(threshold_sf$ADVISORY, na.rm = TRUE), ADVISORY))
      
      # Flag extreme heat indices and add coordinate columns
      cube_sf <- cube_sf %>%
        mutate(extreme = ifelse(heat_index >= ADVISORY, 1, 0)) %>%
        mutate(lat = sf::st_coordinates(.)[, 2],
               long = sf::st_coordinates(.)[, 1]) %>%
        mutate(threshold = ADVISORY,
               observation = heat_index) %>%
        select(lat, long, date, observation, threshold, extreme)
      
      # Filter to only extreme values (i.e., extreme == 1)
      cube_sf_filtered <- dplyr::filter(cube_sf, extreme == 1)
      
      # Only write out file if there are extreme observations
      if (nrow(cube_sf_filtered) > 0) {
        output_file <- file.path(output_dir, paste0("extreme_", 
                                                    substr(basename(nc_file), 1, nchar(basename(nc_file))-3), 
                                                    ".csv"))
        write.csv(cube_sf_filtered, output_file, row.names = FALSE)
      }
      
      gc()
    }, future.seed = TRUE)  # Ensure reproducibility in parallel
  })
  
  # Stop time and elapsed time calculations
  stop_time <- Sys.time()
  elapsed_time <- stop_time - start_time
  
  print(paste("Processing started at:", start_time))
  print(paste("Processing completed at:", stop_time))
  print(paste("Total elapsed time:", elapsed_time))
}


## Covariance ----
stm <- read.csv(here::here("data", "output", "02_covariance", "03_space_time_metric", "heat_index", "month", "heat_index_space_time_metric_optimal.txt"))

# add year, season, decade, date
stm$date <- as.Date(paste0(as.character(stm$year_mo), '01'), format = '%Y%m%d')
stm$year <- lubridate::year(stm$date)
stm$decade <- floor_decade(stm$year)
stm$month <- lubridate::month(stm$date)
stm$season <- ifelse(stm$month %in% c(12, 1, 2), "Winter",
                     ifelse(stm$month %in% c(3, 4, 5), "Spring",
                            ifelse(stm$month %in% c(6, 7, 8), "Summer", "Autumn")))
stm$month <- ifelse(stm$month %in% 12, "Dec",
                    ifelse(stm$month %in% 1, "Jan",
                           ifelse(stm$month %in% 2, "Feb",
                                  ifelse(stm$month %in% 3, "Mar",
                                         ifelse(stm$month %in% 4, "Apr",
                                                ifelse(stm$month %in% 5, "May",
                                                       ifelse(stm$month %in% 6, "Jun",
                                                              ifelse(stm$month %in% 7, "Jul",
                                                                     ifelse(stm$month %in% 8, "Aug",
                                                                            ifelse(stm$month %in% 9, "Sep",
                                                                                   ifelse(stm$month %in% 10, "Oct", "Nov")))))))))))

# Summarize the data
decade.heat <- stm %>%
  group_by(decade) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

year.heat <- stm %>%
  group_by(year) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

season.heat <- stm %>%
  group_by(season) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

month.heat <- stm %>%
  group_by(month) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

### 0.25 deg / day ----
# stm:1 * res:0.25 = res: 0.25
process_heat_index_files(
  input_dir = here::here("data", "output", "01_era5", "daily", "heat_index"),
  output_dir = here::here("data", "output", "03_cluster", "01_extreme_points", "0.25", "heat_index"),
  threshold_shapefile = here::here("data", "output","03_cluster", "00_threshold", "wfo_southeast_expansion_dissolved.shp"),
  # threshold_shapefile = here::here("data", "input", "threshold", "wfo_usa.shp"),
  space_time_metric = 1
)

### 0.3075 deg / day ----
# stm:1.23 * res:0.25 = res: 0.3075
process_heat_index_files(
  input_dir = here::here("data", "output", "01_era5", "daily", "heat_index"), 
  output_dir = here::here("data", "output", "03_cluster", "01_extreme_points", "0.3075", "heat_index"), 
  threshold_shapefile = here::here("data", "output","03_cluster", "00_threshold", "wfo_southeast_expansion_dissolved.shp"),
  space_time_metric = 1.23
)

### 0.39 deg / day ----
# stm:1.56 * res:0.25 = res: 0.39
process_heat_index_files(
  input_dir = here::here("data", "output", "01_era5", "daily", "heat_index"), 
  output_dir = here::here("data", "output", "03_cluster", "01_extreme_points", "0.39","heat_index"), 
  threshold_shapefile = here::here("data", "output","03_cluster", "00_threshold", "wfo_southeast_expansion_dissolved.shp"),
  space_time_metric = 1.56
)

# Precipitation ----
process_precipitation_files <- function(input_dir, output_dir, space_time_metric, atlas_sf) {
  
  # Start time
  start_time <- Sys.time()
  
  # List all NetCDF files
  nc_files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)
  
  # # Subset if necessary
  # nc_files <- nc_files[c(492:length(nc_files))]
  
  # Progress bar setup
  handlers(global = TRUE)
  p <- progressor(along = nc_files)
  
  # Use parallel processing
  plan(multicore, workers = parallel::detectCores() - 1)  # Use one less than max cores
  
  future_lapply(seq_along(nc_files), function(i) {
    
    nc_file <- nc_files[[i]]
    p(sprintf("Processing %d/%d: %s", i, length(nc_files), basename(nc_file)))
    
    # Read NetCDF file as raster
    var <- terra::rast(nc_file)
    
    # Create space-time cube for hourly data
    cube <- create.st.cube(target.raster = var, space.time.metric = space_time_metric)
    
    # Convert to data frame
    cube.df <- as.data.frame(cube, xy = TRUE)
    
    # Convert to long format
    cube_long <- cube.df %>%
      pivot_longer(-c(x, y), names_to = "datetime", values_to = "precipitation") %>%
      mutate(datetime = ifelse(nchar(datetime) == 10, paste0(datetime, " 00:00:00"), datetime)) %>%  # Ensure midnight timestamps have time component
      mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))  # Convert to POSIXct
    
    # Convert to sf object
    cube_sf <- st_as_sf(cube_long, coords = c("x", "y"), crs = 4269)  # NAD83 CRS
    
    # Perform spatial join with atlas_sf
    cube_sf <- st_join(cube_sf, atlas_sf, left = TRUE) %>%
      mutate(ne1yr24ha = ifelse(is.na(ne1yr24ha), min(atlas_sf$ne1yr24ha, na.rm = TRUE), ne1yr24ha))
    
    cube_sf <- cube_sf %>%
      mutate(extreme = ifelse(precipitation >= ne1yr24ha, 1, 0)) %>%
      mutate(lat = st_coordinates(.)[, 2], long = st_coordinates(.)[, 1]) %>%
      mutate(threshold = ne1yr24ha) %>%
      mutate(observation = precipitation) %>%
      select(lat, long, datetime, observation, threshold, extreme)
    
    # Subset to only rows where extreme == 1
    cube_sf_filtered <- cube_sf %>% filter(extreme == 1)
    
    # Save processed file if it contains values
    if (nrow(cube_sf_filtered) > 0) {
      output_file <- file.path(output_dir, paste0("extreme_", substr(basename(nc_file), 1, nchar(basename(nc_file)) - 3), ".csv"))
      write.csv(cube_sf_filtered, output_file, row.names = FALSE)
    }
    
    gc()
  })
  
  # Stop time and elapsed time
  stop_time <- Sys.time()
  elapsed_time <- stop_time - start_time
  
  print(paste("Processing started at:", start_time))
  print(paste("Processing completed at:", stop_time))
  print(paste("Total elapsed time:", elapsed_time))
}

## Threshold ----
atlas_sf <- st_read(here::here("data", "input", "threshold", "atlas14_shapefile.shp"))

## Covariance ----
stm <- read.csv(here::here("data", "output", "02_covariance", "03_space_time_metric", "precipitation", "month", "precipitation_space_time_metric_optimal.txt"))

# add year, season, decade, date
stm$date <- as.Date(paste0(as.character(stm$year_mo), '01'), format = '%Y%m%d')
stm$year <- lubridate::year(stm$date)
stm$decade <- floor_decade(stm$year)
stm$month <- lubridate::month(stm$date)
stm$season <- ifelse(stm$month %in% c(12, 1, 2), "Winter",
                     ifelse(stm$month %in% c(3, 4, 5), "Spring",
                            ifelse(stm$month %in% c(6, 7, 8), "Summer", "Autumn")))
stm$month <- ifelse(stm$month %in% 12, "Dec",
                    ifelse(stm$month %in% 1, "Jan",
                           ifelse(stm$month %in% 2, "Feb",
                                  ifelse(stm$month %in% 3, "Mar",
                                         ifelse(stm$month %in% 4, "Apr",
                                                ifelse(stm$month %in% 5, "May",
                                                       ifelse(stm$month %in% 6, "Jun",
                                                              ifelse(stm$month %in% 7, "Jul",
                                                                     ifelse(stm$month %in% 8, "Aug",
                                                                            ifelse(stm$month %in% 9, "Sep",
                                                                                   ifelse(stm$month %in% 10, "Oct", "Nov")))))))))))

# Summarize the data
decade.precipitation <- stm %>%
  group_by(decade) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

year.precipitation <- stm %>%
  group_by(year) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

season.precipitation <- stm %>%
  group_by(season) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

month.precipitation <- stm %>%
  group_by(month) %>%
  summarize(
    mean = mean(stm2, na.rm = TRUE),  # Calculate the mean
    median = median(stm2, na.rm = TRUE)  # Calculate the median
  ) %>%
  ungroup()

record.precipitation <- median(stm$stm2)

## 0.25 deg / hour ----
process_precipitation_files(
  input_dir = here::here("data", "output", "01_era5", "hourly", "precipitation"),
  output_dir = here::here("data", "output", "03_cluster", "stm1", "precipitation"),
  space_time_metric = 1,
  atlas_sf = atlas_sf
)