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

# Day ----

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

## 0.25 deg ----
### Excess Heat ----

years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm1','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall', 'day','stm1','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Heat ----

years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm1','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "Heat"
output_directory <- here('data','output','05_validation','recall', 'day','stm1','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

## 0.39 deg ----
### Excess Heat ----

years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','record','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall', 'day','record','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Heat ----

years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','record','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "Heat"
output_directory <- here('data','output','05_validation','recall', 'day','record','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

## 1.00 deg ----
### Excess Heat ----

years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm4','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall', 'day','stm4','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Heat ----

years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm4','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "Heat"
output_directory <- here('data','output','05_validation','recall', 'day','stm4','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_day, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

# Week ----

process_validation_week <- function(year, cluster_dir, noaa_dir, output_dir, hazard_name) {

  # Helper function: Fix week date if it falls in January but belongs to a previous ISO year
  fix_isoweek_date <- function(d, target_year) {
    if (month(d) == 1 && year(d) < target_year) {
      # Force to Monday of week 1 of the target year
      return(ISOweek2date(sprintf("%04d-W01-1", target_year)))
    } else {
      return(d)
    }
  }

  # Load the Koppen-Geiger raster (used to define resolution)
  resolution <- rast("V:/users/hquintal/phd2_southeast/data/input/regional_aggregation/koppen_geiger/1991_2020/koppen_geiger_0p1.tif")
  resolution <- resolution / resolution

  # Load county boundaries, fix geometries, and remove empty geometries
  us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
  us.states <- st_make_valid(us.states)
  us.states <- us.states[!st_is_empty(us.states), ]
  us.states.vect <- vect(us.states)

  # Rasterize counties to the defined resolution and crop to CONUS extent
  us.states.rast <- rasterize(us.states.vect, resolution, field = 'ID')
  us.states.rast <- crop(us.states.rast, ext(-130, -65, 24, 50))

  cat("\n🔄 Processing Year:", year, "\n")

  #### Step 1: Process Cluster Data (Weekly Aggregation)
  # Expect filenames like "2019-..-.._cluster_..."
  cluster_files <- list.files(cluster_dir, pattern = paste0(year, "-..-.._cluster_"), full.names = TRUE)
  if (length(cluster_files) == 0) {
    cat("⚠️ No cluster files found for year:", year, "\n")
    return(NULL)
  }

  # Read daily cluster data and force binary values (0/1)
  cluster_daily <- rast(cluster_files)
  cluster_daily <- cluster_daily / cluster_daily

  # Crop and resample to match the county raster
  us.states.rast <- crop(us.states.rast, ext(cluster_daily))
  us.states.rast.normal <- us.states.rast * 0
  cluster_daily <- terra::resample(cluster_daily, us.states.rast.normal, method = 'max')
  cluster_daily <- sum(cluster_daily, us.states.rast.normal, na.rm = TRUE)

  # Aggregate daily cluster data by week (using max for binary events)
  cluster_weekly <- tapp(cluster_daily, "week", fun = max)

  # Get week labels (these are typically in the form "week_XX" from tapp)
  week_labels <- names(cluster_weekly)
  # For each unique week number, get the Monday date using ISOweek2date:
  unique_weeks <- sort(unique(as.integer(str_extract(week_labels, "\\d+$"))))
  week_dates_cluster <- sapply(unique_weeks, function(w) {
    # Construct an ISO week string assuming target year; e.g., "2019-W01-1"
    d <- ISOweek2date(sprintf("%04d-W%02d-1", year, w))
    fix_isoweek_date(d, target_year = year)
  })
  # Assign these dates to the weekly raster (order by week number)
  terra::time(cluster_weekly) <- as.Date(week_dates_cluster)
  names(cluster_weekly) <- as.character(as.Date(week_dates_cluster))
  # Retain only weeks with events (global max equals 1)
  cluster_weekly <- cluster_weekly[[terra::global(cluster_weekly, fun = "max", na.rm = TRUE) == 1]]
  terra::crs(cluster_weekly) <- terra::crs(us.states.vect)

  # Also compute the total (summed) count of cluster events per county
  cluster_count_sum <- terra::extract(cluster_weekly, vect(us.states), fun = sum, na.rm = TRUE)

  #### Step 2: Process NOAA Data (Weekly Aggregation)
  # Expect filename like "NOAA_<hazard_name>_Daily_Extent_<year>.nc"
  noaa_file <- file.path(noaa_dir, paste0("NOAA_", hazard_name, "_Daily_Extent_", year, ".nc"))
  if (!file.exists(noaa_file)) {
    cat("⚠️ No NOAA file found for year:", year, "\n")
    return(NULL)
  }

  noaa_daily <- rast(noaa_file)
  noaa_daily <- tapp(noaa_daily, "day", fun = max)
  noaa_daily <- noaa_daily[[terra::global(noaa_daily, fun = "max", na.rm = TRUE) == 1]]
  terra::crs(noaa_daily) <- terra::crs(us.states.vect)

  dts_noaa <- as.Date(terra::time(noaa_daily))
  iso_weeks_noaa <- lubridate::isoweek(dts_noaa)
  unique_weeks_noaa <- sort(unique(iso_weeks_noaa))

  noaa_weekly_list <- lapply(unique_weeks_noaa, function(w) {
    idx <- which(iso_weeks_noaa == w)
    week_rast <- noaa_daily[[idx]]
    app(week_rast, fun = max)
  })
  noaa_weekly <- do.call(c, noaa_weekly_list)
  week_dates_noaa <- sapply(unique_weeks_noaa, function(w) {
    d <- ISOweek2date(sprintf("%04d-W%02d-1", year, w))
    fix_isoweek_date(d, target_year = year)
  })
  terra::time(noaa_weekly) <- as.Date(week_dates_noaa)
  names(noaa_weekly) <- as.character(as.Date(week_dates_noaa))
  noaa_weekly <- noaa_weekly[[terra::global(noaa_weekly, fun = "max", na.rm = TRUE) == 1]]
  terra::crs(noaa_weekly) <- terra::crs(us.states.vect)

  # Also compute the total (summed) count of NOAA events per county
  noaa_count_sum <- terra::extract(noaa_weekly, vect(us.states), fun = sum, na.rm = TRUE)

  #### Step 3: Extract County-Level Data for Confusion Matrix
  cluster_county <- terra::extract(cluster_weekly, vect(us.states), fun = max, na.rm = TRUE)
  noaa_county    <- terra::extract(noaa_weekly, vect(us.states), fun = max, na.rm = TRUE)
  nan_county <- sum(is.nan(cluster_county[, 2]))

  if (is.null(cluster_county) || is.null(noaa_county) ||
      nrow(cluster_county) == 0 || nrow(noaa_county) == 0) {
    cat("⚠️ Extraction failed for year:", year, "\n")
    return(NULL)
  }

  #### Step 4: Compute Weekly Confusion Matrix
  week_dates <- as.Date(terra::time(noaa_weekly))  # should match cluster_weekly
  results <- data.frame(Date = week_dates, TP = 0, TN = 0, FP = 0, FN = 0)
  county_results <- data.frame(ID = cluster_county$ID, TP = 0, TN = 0, FP = 0, FN = 0)

  pb <- txtProgressBar(min = 0, max = length(week_dates), style = 3)
  for (i in seq_along(week_dates)) {
    setTxtProgressBar(pb, i)
    date <- week_dates[i]
    date_str <- as.character(date)

    cluster_subset <- which(colnames(cluster_county) %in% date_str)
    noaa_subset <- which(colnames(noaa_county) %in% date_str)

    cluster_week <- rep(0, nrow(cluster_county))
    noaa_week <- rep(0, nrow(noaa_county))

    if (length(cluster_subset) > 0) {
      cluster_week <- cluster_county[, cluster_subset]
    }
    if (length(noaa_subset) > 0) {
      noaa_week <- noaa_county[, noaa_subset]
    }

    cluster_week[!cluster_week %in% c(0, 1, NA)] <- NA
    noaa_week[!noaa_week %in% c(0, 1, NA)] <- NA

    tp <- cluster_week == 1 & noaa_week == 1
    tn <- cluster_week == 0 & noaa_week == 0
    fp <- cluster_week == 1 & noaa_week == 0
    fn <- cluster_week == 0 & noaa_week == 1

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

  county_results[is.na(county_results)] <- 0
  county_results$cluster_count <- cluster_count_sum[, 2]
  county_results$noaa_count <- noaa_count_sum[, 2]

  #### Step 5: Merge County Results with Boundaries and Save as GeoPackage
  us.states.new <- us.states
  us.states.new$ID_new <- seq(1, nrow(us.states.new))

  us.states.new <- merge(us.states.new, county_results, by.x = "ID_new", by.y = "ID", all.x = TRUE)
  us.states.new[is.na(us.states.new)] <- 0

  st_write(us.states.new, file.path(output_dir, paste0(year, "_County_Validation.gpkg")),
           layer = "county_results", driver = "GPKG", delete_layer = TRUE, quiet = TRUE)

  cat("✅ Finished processing year:", year, "\n")
  gc()
}

## 0.25 deg ----
### Excess Heat ----

years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm1','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall', 'week','stm1','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_week, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Heat ----

years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm1','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "Heat"
output_directory <- here('data','output','05_validation','recall', 'week','stm1','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_week, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

## 0.39 deg ----
### Excess Heat ----

years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','record','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall', 'week','record','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_week, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Heat ----

years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','record','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "Heat"
output_directory <- here('data','output','05_validation','recall', 'week','record','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_week, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

## 1.00 deg ----
### Excess Heat ----

years_to_process <- 2000:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm4','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','excess_heat')
hazard <- "Excess_Heat"
output_directory <- here('data','output','05_validation','recall', 'week','stm4','excess_heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_week, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)

### Heat ----

years_to_process <- 1996:2023
cluster_directory <- here('data','output','03_cluster','02_cluster','points','stm4','heat_index','clean')
noaa_directory <- here('data','output','04_noaa','southeast','summary','heat')
hazard <- "Heat"
output_directory <- here('data','output','05_validation','recall', 'week','stm4','heat')
koppen_raster_path <- here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")

lapply(years_to_process, 
       process_validation_week, 
       cluster_directory, 
       noaa_directory, 
       output_directory, 
       hazard)
