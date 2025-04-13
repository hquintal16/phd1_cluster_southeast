#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: calculate recall for a user-specified year range
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
# here::set_here("V:/users/hquintal/phd1_cluster_southeast")
here::i_am("scripts/17_validation_raster.R")  # Adjust this file path if necessary
source(here::here("scripts", "01_library.R"))

aggregate_validation_results <- function(years, validation_dir, output_dir) {
  
  message("Checkpoint 1: Listing gpkg files in ", validation_dir)
  pattern <- paste0("(", paste(years, collapse = "|"), ")_County_.*\\.gpkg$")
  gpkg_files <- list.files(validation_dir, pattern = pattern, full.names = TRUE)
  message("Found ", length(gpkg_files), " files.")
  
  if (length(gpkg_files) == 0) {
    stop("No gpkg files found for the specified years in ", validation_dir)
  }
  
  message("Checkpoint 2: Reading gpkg files with progress bar...")
  sf_list <- pblapply(gpkg_files, function(f) {
    message("Reading file: ", f)
    st_read(f, quiet = TRUE)
  })
  
  combined_sf <- do.call(rbind, sf_list)
  message("Checkpoint 3: Combined sf has ", nrow(combined_sf), " features.")
  
  message("Checkpoint 4: Aggregating numeric confusion matrix values by county (dropping geometry).")
  aggregated_numeric <- combined_sf %>%
    st_set_geometry(NULL) %>%   # Drop geometry for numeric aggregation
    group_by(ID_new) %>%
    summarise(
      TP = sum(TP, na.rm = TRUE),
      TN = sum(TN, na.rm = TRUE),
      FP = sum(FP, na.rm = TRUE),
      FN = sum(FN, na.rm = TRUE),
      cluster_count = sum(cluster_count, na.rm = TRUE),
      noaa_count = sum(noaa_count, na.rm = TRUE)
    ) %>%
    ungroup()
  message("Aggregated numeric data has ", nrow(aggregated_numeric), " rows.")
  
  message("Checkpoint 5: Extracting unique geometry for each county using first occurrence.")
  # Determine the name of the geometry column.
  geom_col <- attr(combined_sf, "sf_column")
  # Extract unique geometry rows (first occurrence per county)
  unique_geom <- combined_sf[!duplicated(combined_sf$ID_new), c("ID_new", geom_col)]
  message("Unique geometry extracted for ", nrow(unique_geom), " counties.")
  
  message("Checkpoint 6: Joining aggregated numeric data with unique geometries.")
  # Join the aggregated numeric data with the unique geometry (which is an sf object).
  aggregated <- left_join(unique_geom, aggregated_numeric, by = "ID_new")
  message("Checkpoint 7: Aggregated sf has ", nrow(aggregated), " features.")
  
  message("Checkpoint 8: Calculating recall (TP/(TP+FN)).")
  aggregated$recall <- with(aggregated, ifelse((TP + FN) > 0, TP/(TP + FN), NA))
  message("Aggregation complete.")
  
  #### Rasterization Steps
  message("Checkpoint 9: Loading reference raster for target CRS and extent.")
  region.crs <- rast(here('data','output','01_era5','daily','heat_index','heat_index_daily_maximum_194001.nc'))
  
  message("Checkpoint 10: Loading Koppen-Geiger raster for resolution.")
  directory <- here('data','input','regional_aggregation','koppen_geiger','1991_2020','koppen_geiger_0p1.tif')
  res_rast <- rast(directory)
  res_rast <- res_rast / res_rast  # Create a binary raster for resolution
  
  message("Checkpoint 11: Loading US county boundaries via maps.")
  us_states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
  us_states.vect <- vect(us_states)
  
  message("Checkpoint 12: Rasterizing US counties using Koppen-Geiger resolution.")
  us_states_rast <- rasterize(us_states.vect, res_rast, field = "ID")
  us_states_rast <- crop(us_states_rast, ext(region.crs))
  
  message("Checkpoint 13: Converting aggregated sf to SpatVector and rasterizing recall field.")
  aggregated_vect <- vect(aggregated)
  recall_raster <- rasterize(aggregated_vect, us_states_rast, field = "recall", fun = "mean")
  recall_raster <- crop(recall_raster, ext(region.crs))
  
  message("Checkpoint 14: Building dynamic output file name based on validation_dir.")
  dir_parts <- unlist(strsplit(validation_dir, split = .Platform$file.sep))
  dir_parts <- dir_parts[dir_parts != ""]
  dynamic_part <- paste(tail(dir_parts, 3), collapse = "_")
  file_name <- paste0(min(years), "_", max(years), "_", dynamic_part, ".nc")
  output_file <- file.path(output_dir, file_name)
  
  message("Checkpoint 15: Saving recall raster as NetCDF using writeCDF().")
  writeCDF(recall_raster, filename = output_file, overwrite = TRUE)
  
  message("Aggregated county recall raster saved as: ", output_file)
}


# Heat Index Advisory ----
## 0.25 deg ----
years_to_process <- 2000:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.25','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2010:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.25','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2019:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.25','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

## 0.3075 deg ----
years_to_process <- 2000:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.3075','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2010:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.3075','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2019:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.3075','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

## 0.39 deg ----
years_to_process <- 2000:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.39','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2010:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.39','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2019:2023
validation_directory <- here('data','output','05_validation','recall','advisory','day','0.39','excess_heat')
output_directory <- here('data','output','05_validation','recall','advisory','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

# Heat Index Warning ----
## 0.25 deg ----
years_to_process <- 2000:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.25','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2010:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.25','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2019:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.25','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

## 0.3075 deg ----
years_to_process <- 2000:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.3075','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2010:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.3075','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2019:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.3075','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

## 0.39 deg ----
years_to_process <- 2000:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.39','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2010:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.39','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

years_to_process <- 2019:2023
validation_directory <- here('data','output','05_validation','recall','warning','day','0.39','excess_heat')
output_directory <- here('data','output','05_validation','recall','warning','raster')
aggregate_validation_results(years_to_process, validation_directory, output_directory)

# ### Heat ----
# years_to_process <- 1996:2023
# validation_directory <- here('data','output','05_validation','recall','day','record','heat')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','record','heat')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','record','heat')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Flash Flood ----
# years_to_process <- 2000:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','flash_flood')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','flash_flood')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','flash_flood')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Flood ----
# years_to_process <- 2000:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','flood')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','flood')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','flood')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Heat ----
# years_to_process <- 1996:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','heat')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','heat')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','heat')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Heavy Rain ----
# years_to_process <- 1996:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','heavy_rain')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','heavy_rain')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','heavy_rain')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Hurricane ----
# years_to_process <- 1996:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','hurricane')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','hurricane')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','hurricane')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Tropical Depression ----
# years_to_process <- 1996:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','tropical_depression')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','tropical_depression')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','tropical_depression')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Tropical Storm ----
# years_to_process <- 1996:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','tropical_storm')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','tropical_storm')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','tropical_storm')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# ### Typhoon ----
# years_to_process <- 1996:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','typhoon')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2010:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','typhoon')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
# 
# years_to_process <- 2019:2023
# validation_directory <- here('data','output','05_validation','recall','day','stm1','typhoon')
# output_directory <- here('data','output','05_validation','recall','raster')
# aggregate_validation_results(years_to_process, validation_directory, output_directory)
