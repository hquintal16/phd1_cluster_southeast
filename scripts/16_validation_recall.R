#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: calculate recall for a user-specified year range
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
# here::set_here("V:/users/hquintal/phd1_cluster_southeast")
here::i_am("scripts/16_validation_recall.R")  # Adjust this file path if necessary
source(here::here("scripts", "01_library.R"))

process_validation_day <- function(year, cluster_dir, noaa_dir, output_dir, hazard_name) {
  
  # Load the Koppen-Geiger raster (to define resolution)
  resolution <- rast("V:/users/hquintal/phd2_southeast/data/input/regional_aggregation/koppen_geiger/1991_2020/koppen_geiger_0p1.tif")
  resolution <- resolution / resolution
  
  # Load county boundaries from the maps package and fix geometries
  us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
  us.states <- st_make_valid(us.states)
  us.states <- us.states[!st_is_empty(us.states), ]
  us.states.vect <- vect(us.states)
  
  # Rasterize counties to the defined resolution and crop to CONUS extent
  us.states.rast <- terra::rasterize(us.states.vect, resolution, field = 'ID')
  us.states.rast <- crop(us.states.rast, ext(-130, -65, 24, 50))
  cat("\n🔄 Processing Year:", year, "\n")
  
  #### Step 1: Read and Process Cluster Data 
  # Expecting filenames like: "2019-..-.._cluster_..."
  cluster_files <- list.files(cluster_dir, pattern = paste0(year, "-..-.._cluster_"), full.names = TRUE)
  if (length(cluster_files) == 0) {
    cat("⚠️ No cluster files found for year:", year, "\n")
    return(NULL)
  }
  
  cluster_stack <- rast(cluster_files)
  cluster_stack <- cluster_stack / cluster_stack  # Ensures binary values (0/1)
  us.states.rast <- crop(us.states.rast, ext(cluster_stack))
  us.states.rast.normal <- us.states.rast * 0
  cluster_stack <- terra::resample(cluster_stack, us.states.rast.normal, method = 'max')
  cluster_stack <- sum(cluster_stack, us.states.rast.normal, na.rm = TRUE)
  cluster_stack <- tapp(cluster_stack, "day", fun = max)
  
  # Remove days with no events (cells with global max not equal to 1)
  cluster_stack <- cluster_stack[[terra::global(cluster_stack, fun = "max", na.rm = TRUE) == 1]]
  terra::crs(cluster_stack) <- terra::crs(us.states.vect)
  # Explicitly set layer names as "YYYY-MM-DD"
  names(cluster_stack) <- format(as.Date(terra::time(cluster_stack)), "%Y-%m-%d")
  
  # Also compute the total (summed) count of cluster events per county
  cluster_count_sum <- terra::extract(cluster_stack, vect(us.states), fun = sum, na.rm = TRUE)
  
  #### Step 2: Read and Process NOAA Data
  # Expecting filename like: "NOAA_<hazard_name>_Daily_Extent_<year>.nc"
  noaa_file <- file.path(noaa_dir, paste0("NOAA_", hazard_name, "_Daily_Extent_", year, ".nc"))
  if (!file.exists(noaa_file)) {
    cat("⚠️ No NOAA file found for year:", year, "\n")
    return(NULL)
  }
  
  noaa_rast <- rast(noaa_file)
  noaa_rast <- tapp(noaa_rast, "day", fun = max)
  noaa_rast <- noaa_rast[[terra::global(noaa_rast, fun = "max", na.rm = TRUE) == 1]]
  terra::crs(noaa_rast) <- terra::crs(us.states.vect)
  names(noaa_rast) <- format(as.Date(terra::time(noaa_rast)), "%Y-%m-%d")
  
  # Also compute the total (summed) count of NOAA events per county
  noaa_count_sum <- terra::extract(noaa_rast, vect(us.states), fun = sum, na.rm = TRUE)
  
  #### Step 3: Extract County-Level Data for Confusion Matrix
  # Use terra::extract() with bylayer = FALSE (default) returns a data.frame where:
  #   - The first column is the county ID
  #   - Subsequent columns are named by the layer (our "YYYY-MM-DD" strings)
  cluster_county <- terra::extract(cluster_stack, vect(us.states), fun = max, na.rm = TRUE)
  noaa_county    <- terra::extract(noaa_rast, vect(us.states), fun = max, na.rm = TRUE)
  
  nan_county <- sum(is.nan(cluster_county[, 2]))
  
  if (is.null(cluster_county) || is.null(noaa_county) ||
      nrow(cluster_county) == 0 || nrow(noaa_county) == 0) {
    cat("⚠️ Extraction failed for year:", year, "\n")
    return(NULL)
  }
  
  #### Step 4: Compute Daily Confusion Matrix
  all_dates <- seq.Date(as.Date(paste0(year, "-01-01")),
                        as.Date(paste0(year, "-12-31")), by = "day")
  results <- data.frame(Date = all_dates, TP = 0, TN = 0, FP = 0, FN = 0)
  
  # Initialize county-level confusion matrix storage
  county_results <- data.frame(ID = cluster_county$ID, TP = 0, TN = 0, FP = 0, FN = 0)
  
  pb <- txtProgressBar(min = 0, max = length(all_dates), style = 3)
  
  for (i in seq_along(all_dates)) {
    setTxtProgressBar(pb, i)
    date <- all_dates[i]
    # Convert date to character using same format as layer names
    date_str <- format(date, "%Y-%m-%d")
    
    cluster_subset <- which(colnames(cluster_county) %in% date_str)
    noaa_subset    <- which(colnames(noaa_county) %in% date_str)
    
    cluster_day <- rep(0, nrow(cluster_county))
    noaa_day    <- rep(0, nrow(noaa_county))
    
    if (length(cluster_subset) > 0) {
      cluster_day <- cluster_county[, cluster_subset]
    }
    if (length(noaa_subset) > 0) {
      noaa_day <- noaa_county[, noaa_subset]
    }
    
    # Ensure only valid values (0, 1 or NA) remain
    cluster_day[!cluster_day %in% c(0, 1, NA)] <- NA
    noaa_day[!noaa_day %in% c(0, 1, NA)] <- NA
    
    tp <- cluster_day == 1 & noaa_day == 1
    tn <- cluster_day == 0 & noaa_day == 0
    fp <- cluster_day == 1 & noaa_day == 0
    fn <- cluster_day == 0 & noaa_day == 1
    
    county_results$TP <- county_results$TP + as.numeric(tp)
    county_results$TN <- county_results$TN + as.numeric(tn)
    county_results$FP <- county_results$FP + as.numeric(fp)
    county_results$FN <- county_results$FN + as.numeric(fn)
    
    results[i, 2:5] <- c(sum(tp, na.rm = TRUE), sum(tn, na.rm = TRUE),
                         sum(fp, na.rm = TRUE), sum(fn, na.rm = TRUE))
  }
  
  close(pb)
  
  true_county <- 3076 - nan_county
  results[results == 3076] <- true_county
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  write.csv(results, file.path(output_dir, paste0(year, "_validation_results.csv")), row.names = FALSE)
  
  # Replace NA's with 0 in county_results
  county_results[is.na(county_results)] <- 0
  
  # Add the count of events per county from the extractions
  county_results$cluster_count <- cluster_count_sum[, 2]
  county_results$noaa_count    <- noaa_count_sum[, 2]
  
  #### Step 5: Merge and Save County Results
  us.states.new <- us.states
  us.states.new$ID_new <- seq(1, nrow(us.states.new))
  
  us.states.new <- merge(us.states.new, county_results, by.x = "ID_new", by.y = "ID", all.x = TRUE)
  us.states.new[is.na(us.states.new)] <- 0
  
  st_write(us.states.new, file.path(output_dir, paste0(year, "_County_Validation.gpkg")), 
           layer = "county_results", driver = "GPKG", delete_layer = TRUE, quiet = TRUE)
  
  cat("✅ Finished processing year:", year, "\n")
  gc()
}

# Heat Index ----
# Excess Heat ----

## Heat Advisory ----

### 0.25 deg ----
years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','advisory','points','0.25','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall','advisory', 'day','0.25','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.3075 deg ----
years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','advisory','points','0.3075','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall','advisory', 'day','0.3075','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.39 deg ----
years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','advisory','points','0.39','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall','advisory', 'day','0.39','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

## Heat Warning ----

### 0.25 deg ----
years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','warning','points','0.25','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall','warning', 'day','0.25','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.3075 deg ----
years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','warning','points','0.3075','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall','warning', 'day','0.3075','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.39 deg ----
years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','warning','points','0.39','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall','warning', 'day','0.39','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

# Heat ----

## Heat Advisory ----

### 0.25 deg ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','advisory','points','0.25','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "heat"
output_directory <- here('data','output','05_validation','recall','advisory', 'day','0.25','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.3075 deg ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','advisory','points','0.3075','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "heat"
output_directory <- here('data','output','05_validation','recall','advisory', 'day','0.3075','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.39 deg ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','advisory','points','0.39','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "heat"
output_directory <- here('data','output','05_validation','recall','advisory', 'day','0.39','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

## Heat Warning ----

### 0.25 deg ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','warning','points','0.25','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "heat"
output_directory <- here('data','output','05_validation','recall','warning', 'day','0.25','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.3075 deg ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','warning','points','0.3075','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "heat"
output_directory <- here('data','output','05_validation','recall','warning', 'day','0.3075','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### 0.39 deg ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','warning','points','0.39','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "heat"
output_directory <- here('data','output','05_validation','recall','warning', 'day','0.39','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)


# Precipitation ----
# 0.25 deg 

### Flash Flood ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','flash_flood')
hazard <- "Flash_Flood"
output_directory <- here('data','output','05_validation','recall','24hr1yr','flash_flood')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Flood ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','flood')
hazard <- "Flood"
output_directory <- here('data','output','05_validation','recall','24hr1yr','flood')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Heavy Rain ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heavy_rain')
hazard <- "Heavy_Rain"
output_directory <- here('data','output','05_validation','recall','24hr1yr','heavy_rain')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Hurricane ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','hurricane')
hazard <- "Hurricane"
output_directory <- here('data','output','05_validation','recall','24hr1yr','hurricane')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Tropical Depression ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','tropical_depression')
hazard <- "Tropical_Depression"
output_directory <- here('data','output','05_validation','recall','24hr1yr','tropical_depression')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Tropical Storm ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','tropical_storm')
hazard <- "Tropical_Storm"
output_directory <- here('data','output','05_validation','recall','24hr1yr','tropical_storm')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Typhoon ----
years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','24hr1yr','points','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','typhoon')
hazard <- "Typhoon"
output_directory <- here('data','output','05_validation','recall','24hr1yr','typhoon')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)
