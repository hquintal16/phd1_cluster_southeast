#Setup----
#Updated April 2025
#Linked to GitHub
#Hunter Quintal
#purpose: summarize clustered events into a df with intensity, extent, duration, NOAA-reported losses
#study area: Southeast

# Load Libraries & Functions ----
library(here)
here::i_am("scripts/19_dataframe_cluster.R")
source(here::here("scripts", "01_library.R"))

process_cluster_noaa_parallel <- function(cluster_folder, era5_folder, 
                                          noaa_excess_heat_folder, noaa_heat_folder, 
                                          noaa_impacts_folder, output_csv) {
  start_time_overall <- Sys.time()
  message(sprintf("Cluster processing (parallel per file) started at: %s", start_time_overall))
  
  # --- Load NOAA summary CSVs from the impacts folder ---
  noaa_excess_summary_file <- file.path(noaa_impacts_folder, "NOAA_excess_heat_summary.csv")
  noaa_heat_summary_file   <- file.path(noaa_impacts_folder, "NOAA_heat_summary.csv")
  
  if(!file.exists(noaa_excess_summary_file)) {
    stop("NOAA excess heat summary file not found: ", noaa_excess_summary_file)
  }
  if(!file.exists(noaa_heat_summary_file)) {
    stop("NOAA heat summary file not found: ", noaa_heat_summary_file)
  }
  
  noaa_excess_summary <- read.csv(noaa_excess_summary_file, stringsAsFactors = FALSE)
  noaa_heat_summary   <- read.csv(noaa_heat_summary_file, stringsAsFactors = FALSE)
  
  # Convert the date fields from mm/dd/YYYY to Date objects.
  noaa_excess_summary$start_time <- as.Date(noaa_excess_summary$start_time, format = "%m/%d/%Y")
  noaa_excess_summary$end_time   <- as.Date(noaa_excess_summary$end_time,   format = "%m/%d/%Y")
  noaa_heat_summary$start_time   <- as.Date(noaa_heat_summary$start_time,   format = "%m/%d/%Y")
  noaa_heat_summary$end_time     <- as.Date(noaa_heat_summary$end_time,     format = "%m/%d/%Y")
  
  # --- List all cluster .nc files and filter to those with a date >= 1996-01-01 ---
  nc_files <- list.files(cluster_folder, pattern = "\\.nc$", full.names = TRUE)
  if(length(nc_files) == 0){
    stop("No .nc files found in ", cluster_folder)
  }
  
  filtered_nc_files <- nc_files[sapply(nc_files, function(file) {
    file_base <- basename(file)
    m <- regexec("county_event_(\\d{4}-\\d{2}-\\d{2})_cluster_([0-9]+)\\.nc", file_base)
    parts <- regmatches(file_base, m)
    if(length(parts[[1]]) >= 3){
      event_date <- as.Date(parts[[1]][2])
      return(event_date >= as.Date("1996-01-01"))
    } else {
      message("Filename does not match expected pattern: ", file_base)
      return(FALSE)
    }
  })]
  
  if(length(filtered_nc_files) == 0){
    stop("No cluster files on or after 1996-01-01 found in ", cluster_folder)
  }
  
  # Set up progress handler (messages via message()) and parallel processing.
  handlers(global = TRUE)
  
  file_results <- future_lapply(filtered_nc_files, function(cluster_file) {
    message(sprintf("Processing cluster file: %s", basename(cluster_file)))
    
    # --- Read Cluster Raster ---
    cluster_rast <- try(terra::rast(cluster_file), silent = TRUE)
    if(inherits(cluster_rast, "try-error")){
      message("Error reading cluster file: ", cluster_file)
      return(NULL)
    }
    
    # Extract cluster date and id from filename.
    file_base <- basename(cluster_file)
    m <- regexec("county_event_(\\d{4}-\\d{2}-\\d{2})_cluster_([0-9]+)\\.nc", file_base)
    parts <- regmatches(file_base, m)
    if(length(parts[[1]]) < 3){
      message("Filename does not match expected pattern: ", file_base)
      return(NULL)
    }
    cluster_date_str <- parts[[1]][2]
    cluster_date <- as.Date(cluster_date_str)
    cluster_id <- parts[[1]][3]
    
    # --- Event Times and Exposed Area ---
    times <- terra::time(cluster_rast)
    if(is.null(times) || length(times) == 0){
      times <- as.POSIXct(cluster_date_str, tz = "UTC")
    }
    start_time_event <- min(times, na.rm = TRUE)
    end_time_event   <- max(times, na.rm = TRUE)
    duration_days <- as.numeric(difftime(end_time_event, start_time_event, units = "days")) + 1
    
    cluster_sum <- terra::app(cluster_rast, fun = sum, na.rm = TRUE)
    event_mask_binary <- cluster_sum != 0
    res_vals <- terra::res(cluster_rast)
    if(terra::is.lonlat(cluster_rast)) {
      cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
    } else {
      cell_area <- (res_vals[1] * res_vals[2]) / 1e6
    }
    exposed_area <- sum(terra::values(event_mask_binary), na.rm = TRUE) * cell_area
    
    # --- ERA5 Statistics ---
    era5_values <- c()
    yearmonths <- unique(format(times, "%Y%m"))
    for(ym in yearmonths) {
      era5_file <- here::here(era5_folder, paste0("heat_index_daily_maximum_", ym, ".nc"))
      if(!file.exists(era5_file)){
        message("ERA5 file not found for yearmonth: ", ym)
        next
      }
      era5_rast <- try(terra::rast(era5_file), silent = TRUE)
      if(inherits(era5_rast, "try-error")){
        message("Error reading ERA5 file: ", era5_file)
        next
      }
      cluster_ref <- cluster_rast[[1]]
      era5_rast_resampled <- terra::resample(era5_rast, cluster_ref, method = "near")
      era5_times <- terra::time(era5_rast_resampled)
      if(!is.null(era5_times)){
        idx <- which(era5_times >= start_time_event & era5_times <= end_time_event)
        if(length(idx) == 0) next
        era5_rast_resampled <- era5_rast_resampled[[idx]]
      }
      for(layer in 1:terra::nlyr(era5_rast_resampled)) {
        era5_layer <- era5_rast_resampled[[layer]]
        era5_masked <- terra::mask(era5_layer, event_mask_binary, maskvalue = FALSE)
        vals <- terra::values(era5_masked, mat = FALSE)
        era5_values <- c(era5_values, vals[!is.na(vals)])
      }
    }
    if(length(era5_values) > 0){
      era5_mean <- mean(era5_values, na.rm = TRUE)
      era5_median <- median(era5_values, na.rm = TRUE)
      era5_max <- max(era5_values, na.rm = TRUE)
    } else {
      era5_mean <- era5_median <- era5_max <- NA
    }
    
    # --- NOAA Processing Helper Function ---
    # This function processes a NOAA folder (for one hazard type),
    # checks if each NOAA file is within ±365 days of the cluster event,
    # verifies spatial overlap, and then looks up the impacts from the NOAA summary.
    process_noaa_folder <- function(noaa_folder, hazard_type, noaa_summary_df) {
      noaa_files <- list.files(noaa_folder, pattern = "\\.nc$", full.names = TRUE)
      # Filter NOAA files based on date difference from cluster_date.
      noaa_files <- noaa_files[sapply(noaa_files, function(nc) {
        file_base <- basename(nc)
        m <- regexec("^(\\d{4}-\\d{2}-\\d{2})_NOAA_.*?_(\\d+)_?\\.nc$", file_base)
        parts <- regmatches(file_base, m)
        if(length(parts[[1]]) >= 3){
          noaa_date <- as.Date(parts[[1]][2])
          return(abs(as.numeric(difftime(noaa_date, cluster_date, units = "days"))) <= 365)
        } else {
          message("NOAA filename does not match expected pattern: ", file_base)
          return(FALSE)
        }
      })]
      
      results <- list(ids = c(), impacts = list(injuries_direct = 0,
                                                injuries_indirect = 0,
                                                deaths_direct = 0,
                                                deaths_indirect = 0,
                                                damage_property = 0,
                                                damage_crops = 0))
      
      for(nc_file in noaa_files) {
        file_base <- basename(nc_file)
        m <- regexec("^(\\d{4}-\\d{2}-\\d{2})_NOAA_.*?_(\\d+)_?\\.nc$", file_base)
        parts <- regmatches(file_base, m)
        if(length(parts[[1]]) < 3) next
        
        noaa_date_str <- parts[[1]][2]
        noaa_date <- as.Date(noaa_date_str)
        noaa_id <- parts[[1]][3]
        
        # Only process files matching the hazard type.
        if(!grepl(hazard_type, file_base, ignore.case = TRUE)) next
        
        # Read NOAA raster.
        noaa_rast <- try(terra::rast(nc_file), silent = TRUE)
        if(inherits(noaa_rast, "try-error")){
          message("Error reading NOAA file: ", nc_file)
          next
        }
        cluster_ref <- cluster_rast[[1]]
        noaa_rast_resampled <- terra::resample(noaa_rast, cluster_ref, method = "near")
        
        # Create binary masks.
        noaa_mask <- noaa_rast_resampled > 0
        cluster_mask <- cluster_rast[[1]] > 0
        
        # Check for spatial overlap.
        if(sum(terra::values(noaa_mask * cluster_mask), na.rm = TRUE) > 0){
          results$ids <- c(results$ids, noaa_id)
          
          # Look up the NOAA impacts from the summary data.
          summary_row <- noaa_summary_df[as.character(noaa_summary_df$episode_id) == noaa_id, ]
          if(nrow(summary_row) > 0){
            results$impacts$injuries_direct   <- results$impacts$injuries_direct   + sum(as.numeric(summary_row$injuries_direct), na.rm = TRUE)
            results$impacts$injuries_indirect <- results$impacts$injuries_indirect + sum(as.numeric(summary_row$injuries_indirect), na.rm = TRUE)
            results$impacts$deaths_direct     <- results$impacts$deaths_direct     + sum(as.numeric(summary_row$deaths_direct), na.rm = TRUE)
            results$impacts$deaths_indirect   <- results$impacts$deaths_indirect   + sum(as.numeric(summary_row$deaths_indirect), na.rm = TRUE)
            results$impacts$damage_property   <- results$impacts$damage_property   + sum(as.numeric(summary_row$damage_property), na.rm = TRUE)
            results$impacts$damage_crops      <- results$impacts$damage_crops      + sum(as.numeric(summary_row$damage_crops), na.rm = TRUE)
          }
        }
      }
      return(results)
    }
    
    # --- Process NOAA Folders for Excess Heat and Heat ---
    excess_heat_results <- process_noaa_folder(noaa_excess_heat_folder, "excess_heat", noaa_excess_summary)
    heat_results        <- process_noaa_folder(noaa_heat_folder, "heat", noaa_heat_summary)
    
    noaa_excess_ids <- paste(unique(excess_heat_results$ids), collapse = ";")
    noaa_heat_ids   <- paste(unique(heat_results$ids), collapse = ";")
    
    # Sum impacts from both NOAA sources.
    injuries_direct_total   <- excess_heat_results$impacts$injuries_direct   + heat_results$impacts$injuries_direct
    injuries_indirect_total <- excess_heat_results$impacts$injuries_indirect + heat_results$impacts$injuries_indirect
    deaths_direct_total     <- excess_heat_results$impacts$deaths_direct     + heat_results$impacts$deaths_direct
    deaths_indirect_total   <- excess_heat_results$impacts$deaths_indirect   + heat_results$impacts$deaths_indirect
    damage_property_total   <- excess_heat_results$impacts$damage_property   + heat_results$impacts$damage_property
    damage_crops_total      <- excess_heat_results$impacts$damage_crops      + heat_results$impacts$damage_crops
    
    # Optionally, merge NOAA ids from both hazards.
    all_noaa_ids <- paste(c(noaa_excess_ids, noaa_heat_ids), collapse = ";")
    
    # For this example, we'll leave exposed_counties as NA.
    exposed_counties <- NA
    
    res_row <- data.frame(
      cluster_id = cluster_id,
      noaa_excess_heat_id = noaa_excess_ids,
      noaa_heat_id = noaa_heat_ids,
      start_time = start_time_event,
      end_time = end_time_event,
      duration = duration_days,
      exposed_counties = exposed_counties,
      exposed_area = exposed_area,
      era5_mean = era5_mean,
      era5_median = era5_median,
      era5_max = era5_max,
      injuries_direct = injuries_direct_total,
      injuries_indirect = injuries_indirect_total,
      deaths_direct = deaths_direct_total,
      deaths_indirect = deaths_indirect_total,
      damage_property = damage_property_total,
      damage_crops = damage_crops_total,
      stringsAsFactors = FALSE
    )
    
    return(res_row)
  })
  
  file_results <- file_results[!sapply(file_results, is.null)]
  results <- do.call(rbind, file_results)
  
  end_time_overall <- Sys.time()
  elapsed_time <- end_time_overall - start_time_overall
  message(sprintf("Processing started at: %s", start_time_overall))
  message(sprintf("Processing ended at: %s", end_time_overall))
  message(sprintf("Elapsed time: %s", elapsed_time))
  
  write.csv(results, output_csv, row.names = FALSE)
  message("Results saved to: ", output_csv)
  return(results)
}

# Heat Index ----
# Define folder paths.
cluster_folder <- here::here("data", "output", "03_cluster", "02_cluster", "points", "stm1", "heat_index", "county")
era5_folder <- here::here("data", "output", "01_era5", "daily", "heat_index")
noaa_excess_heat_folder <- here::here("data", "output", "04_noaa", "southeast", "excess_heat")
noaa_heat_folder <- here::here("data", "output", "04_noaa", "southeast", "heat")
noaa_impacts_folder <- here::here("data", "output", "05_validation", "summary")

# Define the output CSV path.
output_csv <- here::here("data", "output", "05_validation", "summary", "Cluster_heat_index_summary.csv")

# Run the new function that processes the cluster files along with NOAA and ERA5 data.
cluster_noaa_results <- process_cluster_noaa_parallel(
  cluster_folder = cluster_folder,
  era5_folder = era5_folder,
  noaa_excess_heat_folder = noaa_excess_heat_folder,
  noaa_heat_folder = noaa_heat_folder,
  noaa_impacts_folder = noaa_impacts_folder,
  output_csv = output_csv
)

# Optionally, combine with other results or further process.
message("Cluster NOAA summary saved to: ", output_csv)