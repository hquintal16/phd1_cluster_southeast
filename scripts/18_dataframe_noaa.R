#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: summarize NOAA southeast events into a df with intensity, extent, duration, losses
#study area: Southeast

# Load Libraries & Functions ----
library(here)
here::i_am("scripts/18_dataframe_noaa.R")  # Adjust the file path as needed
source(here::here("scripts", "01_library.R"))

# Heat Index ----

process_noaa_hazard_heat_parallel <- function(hazard_folder, era5_folder, noaa_impacts_folder, output_csv) {
  start_time <- Sys.time()
  message(sprintf("Heat processing (parallel per file) started at: %s", start_time))
  
  # List all NOAA .nc files in the hazard folder
  nc_files <- list.files(hazard_folder, pattern = "\\.nc$", full.names = TRUE)
  if(length(nc_files) == 0){
    stop("No .nc files found in ", hazard_folder)
  }
  
  # Set up progress handler (progress messages via message())
  handlers(global = TRUE)
  
  # Process each NOAA file in parallel using future_lapply.
  file_results <- future_lapply(nc_files, function(nc_file) {
    message(sprintf("Processing file: %s", basename(nc_file)))
    
    noaa_rast <- try(terra::rast(nc_file), silent = TRUE)
    if(inherits(noaa_rast, "try-error")){
      message("Error reading file: ", nc_file)
      return(NULL)
    }
    
    # Parse filename using updated regex to allow an optional underscore before .nc.
    file_base <- basename(nc_file)
    m <- regexec("^(\\d{4}-\\d{2}-\\d{2})_NOAA_(.+?)_([0-9]+)_?\\.nc$", file_base)
    parts <- regmatches(file_base, m)
    if(length(parts[[1]]) < 4){
      message("Filename does not match expected pattern: ", file_base)
      return(NULL)
    }
    first_day_str <- parts[[1]][2]
    hazard_name <- parts[[1]][3]
    episode_id <- parts[[1]][4]
    
    # Get NOAA event times; if missing, use the first day string.
    times <- terra::time(noaa_rast)
    if(is.null(times) || length(times) == 0){
      times <- as.POSIXct(first_day_str, tz = "UTC")
    }
    start_time_event <- min(times, na.rm = TRUE)
    end_time_event <- max(times, na.rm = TRUE)
    duration_days <- as.numeric(difftime(end_time_event, start_time_event, units = "days")) + 1
    
    # --- Exposed Area Calculation ---
    noaa_sum <- terra::app(noaa_rast, fun = sum, na.rm = TRUE)
    event_mask_binary <- noaa_sum != 0
    res_vals <- terra::res(noaa_rast)
    if(terra::is.lonlat(noaa_rast)) {
      cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
    } else {
      cell_area <- (res_vals[1] * res_vals[2]) / 1e6
    }
    exposed_area <- sum(terra::values(event_mask_binary), na.rm = TRUE) * cell_area
    
    # --- ERA5 Statistics (Heat) ---
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
      noaa_ref <- noaa_rast[[1]]
      era5_rast_resampled <- terra::resample(era5_rast, noaa_ref, method = "near")
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
    
    # --- NOAA Impacts ---
    impacts_csv1 <- file.path(here::here(noaa_impacts_folder), paste0("NOAA_events_", hazard_name, ".csv"))
    impacts_csv2 <- file.path(here::here(noaa_impacts_folder), paste0("NOAA_events_", hazard_name, "_2009_2015.csv"))
    impacts_data <- data.frame()
    if(file.exists(impacts_csv1)){
      impacts_data <- read.csv(impacts_csv1, stringsAsFactors = FALSE)
    }
    if(file.exists(impacts_csv2)){
      impacts_data2 <- read.csv(impacts_csv2, stringsAsFactors = FALSE)
      impacts_data <- rbind(impacts_data, impacts_data2)
    }
    impacts_filtered <- impacts_data %>% dplyr::filter(as.character(EPISODE_ID) == episode_id)
    
    injuries_direct <- sum(as.numeric(impacts_filtered$INJURIES_DIRECT), na.rm = TRUE)
    injuries_indirect <- sum(as.numeric(impacts_filtered$INJURIES_INDIRECT), na.rm = TRUE)
    deaths_direct <- sum(as.numeric(impacts_filtered$DEATHS_DIRECT), na.rm = TRUE)
    deaths_indirect <- sum(as.numeric(impacts_filtered$DEATHS_INDIRECT), na.rm = TRUE)
    damage_property <- sum(as.numeric(impacts_filtered$DAMAGE_PROPERTY), na.rm = TRUE)
    damage_crops <- sum(as.numeric(impacts_filtered$DAMAGE_CROPS), na.rm = TRUE)
    exposed_counties <- if(nrow(impacts_filtered) > 0) {
      paste(unique(impacts_filtered$EVENT_LOCATION), collapse = ";")
    } else {
      NA
    }
    
    res_row <- data.frame(
      episode_id = episode_id,
      hazard = hazard_name,
      start_time = start_time_event,
      end_time = end_time_event,
      duration = duration_days,
      exposed_counties = exposed_counties,
      exposed_area = exposed_area,
      era5_mean = era5_mean,
      era5_median = era5_median,
      era5_max = era5_max,
      injuries_direct = injuries_direct,
      injuries_indirect = injuries_indirect,
      deaths_direct = deaths_direct,
      deaths_indirect = deaths_indirect,
      damage_property = damage_property,
      damage_crops = damage_crops,
      stringsAsFactors = FALSE
    )
    
    return(res_row)
  })
  
  file_results <- file_results[!sapply(file_results, is.null)]
  results <- do.call(rbind, file_results)
  
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  message(sprintf("Processing started at: %s", start_time))
  message(sprintf("Processing ended at: %s", end_time))
  message(sprintf("Elapsed time: %s", elapsed_time))
  
  write.csv(results, output_csv, row.names = FALSE)
  return(results)
}

## Implementation ----

# Define heat hazard folders (update file root to "phd1_cluster_southeast")
heat_hazard_folders <- c(
  here::here("data", "output", "04_noaa", "southeast", "excess_heat"),
  here::here("data", "output", "04_noaa", "southeast", "heat")
)

era5_folder_heat <- here::here("data", "output", "01_era5", "daily", "heat_index")
noaa_impacts_folder <- here::here("data", "output", "05_validation", "summary")

all_heat_results <- list()
for (h_folder in heat_hazard_folders) {
  hazard_name <- basename(h_folder)
  message("Processing heat hazard folder: ", hazard_name)
  
  output_csv <- file.path(h_folder, paste0("NOAA_", hazard_name, "_summary.csv"))
  
  res <- process_noaa_hazard_heat_parallel(
    hazard_folder = h_folder,
    era5_folder = era5_folder_heat,
    noaa_impacts_folder = noaa_impacts_folder,
    output_csv = output_csv
  )
  
  all_heat_results[[hazard_name]] <- res
}
names(all_heat_results) <- sapply(heat_hazard_folders, basename)
combined_heat_results <- do.call(rbind, all_heat_results)
combined_csv <- here::here("data", "output", "05_validation", "summary", "southeast_heat_combined_summary.csv")
write.csv(combined_heat_results, combined_csv, row.names = FALSE)
message("Combined heat summary saved to: ", combined_csv)

# Precipitation ----

process_noaa_hazard_precip_folder <- function(hazard_folder, era5_folder, noaa_impacts_folder) {
  message(sprintf("Processing folder: %s", hazard_folder))
  
  nc_files <- list.files(hazard_folder, pattern = "\\.nc$", full.names = TRUE)
  if(length(nc_files) == 0){
    stop("No .nc files found in ", hazard_folder)
  }
  
  results <- data.frame(
    episode_id = character(),
    hazard = character(),
    start_time = as.POSIXct(character()),
    end_time = as.POSIXct(character()),
    duration = numeric(),
    exposed_counties = character(),
    exposed_area = numeric(),
    era5_mean = numeric(),
    era5_median = numeric(),
    era5_max = numeric(),
    injuries_direct = numeric(),
    injuries_indirect = numeric(),
    deaths_direct = numeric(),
    deaths_indirect = numeric(),
    damage_property = numeric(),
    damage_crops = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(nc_file in nc_files) {
    message(sprintf("Processing file: %s", basename(nc_file)))
    
    noaa_rast <- try(terra::rast(nc_file), silent = TRUE)
    if(inherits(noaa_rast, "try-error")){
      message("Error reading file: ", nc_file)
      next
    }
    
    file_base <- basename(nc_file)
    m <- regexec("^(\\d{4}-\\d{2}-\\d{2})_NOAA_(.+?)_([0-9]+)_?\\.nc$", file_base)
    parts <- regmatches(file_base, m)
    if(length(parts[[1]]) < 4){
      message("Filename does not match expected pattern: ", file_base)
      next
    }
    first_day_str <- parts[[1]][2]
    hazard_name <- parts[[1]][3]
    episode_id <- parts[[1]][4]
    
    times <- terra::time(noaa_rast)
    if(is.null(times) || length(times) == 0){
      times <- as.POSIXct(first_day_str, tz = "UTC")
    }
    start_time_event <- min(times, na.rm = TRUE)
    end_time_event <- max(times, na.rm = TRUE)
    duration_days <- as.numeric(difftime(end_time_event, start_time_event, units = "days")) + 1
    
    noaa_sum <- terra::app(noaa_rast, fun = sum, na.rm = TRUE)
    event_mask_binary <- noaa_sum != 0
    res_vals <- terra::res(noaa_rast)
    if(terra::is.lonlat(noaa_rast)) {
      cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
    } else {
      cell_area <- (res_vals[1] * res_vals[2]) / 1e6
    }
    exposed_area <- sum(terra::values(event_mask_binary), na.rm = TRUE) * cell_area
    
    accumulated_era5 <- NULL
    yearmonths <- unique(format(times, "%Y_%m"))
    for(ym in yearmonths) {
      era5_file <- here::here(era5_folder, paste0(ym, "_era5_precip_tot_conus.nc"))
      if(!file.exists(era5_file)){
        message("ERA5 file not found for yearmonth: ", ym)
        next
      }
      era5_rast <- try(terra::rast(era5_file), silent = TRUE)
      if(inherits(era5_rast, "try-error")){
        message("Error reading ERA5 file: ", era5_file)
        next
      }
      noaa_ref <- noaa_rast[[1]]
      era5_rast_resampled <- terra::resample(era5_rast, noaa_ref, method = "near")
      era5_times <- terra::time(era5_rast_resampled)
      if(!is.null(era5_times)) {
        idx <- which(era5_times >= start_time_event & era5_times <= end_time_event)
        if(length(idx) == 0) next
        era5_rast_resampled <- era5_rast_resampled[[idx]]
      }
      monthly_sum <- terra::app(era5_rast_resampled, fun = sum, na.rm = TRUE)
      if(is.null(accumulated_era5)) {
        accumulated_era5 <- monthly_sum
      } else {
        accumulated_era5 <- accumulated_era5 + monthly_sum
      }
    }
    
    if(is.null(accumulated_era5)) {
      era5_mean <- era5_median <- era5_max <- NA
    } else {
      era5_masked <- terra::mask(accumulated_era5, event_mask_binary, maskvalue = FALSE)
      vals <- terra::values(era5_masked, mat = FALSE)
      if(length(vals[!is.na(vals)]) > 0) {
        era5_mean <- mean(vals[!is.na(vals)], na.rm = TRUE)
        era5_median <- median(vals[!is.na(vals)], na.rm = TRUE)
        era5_max <- max(vals[!is.na(vals)], na.rm = TRUE)
      } else {
        era5_mean <- era5_median <- era5_max <- NA
      }
    }
    
    impacts_csv1 <- file.path(here::here(noaa_impacts_folder), paste0("NOAA_events_", hazard_name, ".csv"))
    impacts_csv2 <- file.path(here::here(noaa_impacts_folder), paste0("NOAA_events_", hazard_name, "_2009_2015.csv"))
    impacts_data <- data.frame()
    if(file.exists(impacts_csv1)){
      impacts_data <- read.csv(impacts_csv1, stringsAsFactors = FALSE)
    }
    if(file.exists(impacts_csv2)){
      impacts_data2 <- read.csv(impacts_csv2, stringsAsFactors = FALSE)
      impacts_data <- rbind(impacts_data, impacts_data2)
    }
    impacts_filtered <- impacts_data %>% dplyr::filter(as.character(EPISODE_ID) == episode_id)
    
    injuries_direct <- sum(as.numeric(impacts_filtered$INJURIES_DIRECT), na.rm = TRUE)
    injuries_indirect <- sum(as.numeric(impacts_filtered$INJURIES_INDIRECT), na.rm = TRUE)
    deaths_direct <- sum(as.numeric(impacts_filtered$DEATHS_DIRECT), na.rm = TRUE)
    deaths_indirect <- sum(as.numeric(impacts_filtered$DEATHS_INDIRECT), na.rm = TRUE)
    damage_property <- sum(as.numeric(impacts_filtered$DAMAGE_PROPERTY), na.rm = TRUE)
    damage_crops <- sum(as.numeric(impacts_filtered$DAMAGE_CROPS), na.rm = TRUE)
    exposed_counties <- if(nrow(impacts_filtered) > 0) {
      paste(unique(impacts_filtered$EVENT_LOCATION), collapse = ";")
    } else {
      NA
    }
    
    res_row <- data.frame(
      episode_id = episode_id,
      hazard = hazard_name,
      start_time = start_time_event,
      end_time = end_time_event,
      duration = duration_days,
      exposed_counties = exposed_counties,
      exposed_area = exposed_area,
      era5_mean = era5_mean,
      era5_median = era5_median,
      era5_max = era5_max,
      injuries_direct = injuries_direct,
      injuries_indirect = injuries_indirect,
      deaths_direct = deaths_direct,
      deaths_indirect = deaths_indirect,
      damage_property = damage_property,
      damage_crops = damage_crops,
      stringsAsFactors = FALSE
    )
    
    results <- rbind(results, res_row)
  }
  
  return(results)
}

## Implementation ----

precip_hazard_folders <- c(
  here::here("data", "output", "04_noaa", "southeast", "flash_flood")
  # here::here("data", "output", "04_noaa", "southeast", "flood"),
  # here::here("data", "output", "04_noaa", "southeast", "heavy_rain"),
  # here::here("data", "output", "04_noaa", "southeast", "hurricane"),
  # here::here("data", "output", "04_noaa", "southeast", "tropical_depression"),
  # here::here("data", "output", "04_noaa", "southeast", "tropical_storm"),
  # here::here("data", "output", "04_noaa", "southeast", "typhoon")
)

folder_sizes <- sapply(precip_hazard_folders, function(folder) {
  length(list.files(folder, pattern = "\\.nc$", full.names = TRUE))
})
sorted_folders <- precip_hazard_folders[order(folder_sizes, decreasing = TRUE)]
precip_hazard_folders <- rev(sorted_folders)

era5_folder_precip <- here::here("data", "output", "01_era5", "hourly", "precipitation")
# noaa_impacts_folder <- here::here("data", "output", "05_validation", "summary")
noaa_impacts_folder <- here::here("data", "output", "04_noaa")

all_precip_results <- future_lapply(precip_hazard_folders, function(h_folder) {
  hazard_name <- basename(h_folder)
  message("Processing hazard folder: ", hazard_name)
  
  res <- process_noaa_hazard_precip_folder(
    hazard_folder = h_folder,
    era5_folder = era5_folder_precip,
    noaa_impacts_folder = noaa_impacts_folder
  )
  
  output_csv <- file.path(h_folder, paste0("NOAA_", hazard_name, "_summary.csv"))
  write.csv(res, output_csv, row.names = FALSE)
  message("Saved CSV for: ", hazard_name)
  
  return(res)
})

names(all_precip_results) <- sapply(precip_hazard_folders, basename)
combined_precip_results <- do.call(rbind, all_precip_results)
combined_csv <- here::here("data", "output", "05_validation", "summary", "southeast_precip_combined_summary.csv")
write.csv(combined_precip_results, combined_csv, row.names = FALSE)
message("Combined precipitation summary saved to: ", combined_csv)

end_time <- Sys.time()
message(sprintf("Overall processing ended at: %s", end_time))

# # Flash flood is special ----
# process_noaa_hazard_precip_folder <- function(hazard_folder, era5_folder, noaa_impacts_folder) {
#   message(sprintf("Processing folder: %s", hazard_folder))
#   
#   nc_files <- list.files(hazard_folder, pattern = "\\.nc$", full.names = TRUE)
#   if(length(nc_files) == 0){
#     stop("No .nc files found in ", hazard_folder)
#   }
#   
#   results <- data.frame(
#     episode_id = character(),
#     hazard = character(),
#     start_time = as.POSIXct(character()),
#     end_time = as.POSIXct(character()),
#     duration = numeric(),
#     exposed_counties = character(),
#     exposed_area = numeric(),
#     era5_mean = numeric(),
#     era5_median = numeric(),
#     era5_max = numeric(),
#     injuries_direct = numeric(),
#     injuries_indirect = numeric(),
#     deaths_direct = numeric(),
#     deaths_indirect = numeric(),
#     damage_property = numeric(),
#     damage_crops = numeric(),
#     stringsAsFactors = FALSE
#   )
#   
#   for(nc_file in nc_files) {
#     message(sprintf("Processing file: %s", basename(nc_file)))
#     
#     noaa_rast <- try(terra::rast(nc_file), silent = TRUE)
#     if(inherits(noaa_rast, "try-error")){
#       message("Error reading file: ", nc_file)
#       next
#     }
#     
#     file_base <- basename(nc_file)
#     m <- regexec("^(\\d{4}-\\d{2}-\\d{2})_NOAA_(.+?)_([0-9]+)_?\\.nc$", file_base)
#     parts <- regmatches(file_base, m)
#     if(length(parts[[1]]) < 4){
#       message("Filename does not match expected pattern: ", file_base)
#       next
#     }
#     first_day_str <- parts[[1]][2]
#     hazard_name <- parts[[1]][3]
#     episode_id <- parts[[1]][4]
#     
#     times <- terra::time(noaa_rast)
#     if(is.null(times) || length(times) == 0){
#       times <- as.POSIXct(first_day_str, tz = "UTC")
#     }
#     start_time_event <- min(times, na.rm = TRUE)
#     end_time_event <- max(times, na.rm = TRUE)
#     duration_days <- as.numeric(difftime(end_time_event, start_time_event, units = "days")) + 1
#     
#     noaa_sum <- terra::app(noaa_rast, fun = sum, na.rm = TRUE)
#     event_mask_binary <- noaa_sum != 0
#     res_vals <- terra::res(noaa_rast)
#     if(terra::is.lonlat(noaa_rast)) {
#       cell_area <- (111.32 * res_vals[1]) * (111.32 * res_vals[2])
#     } else {
#       cell_area <- (res_vals[1] * res_vals[2]) / 1e6
#     }
#     exposed_area <- sum(terra::values(event_mask_binary), na.rm = TRUE) * cell_area
#     
#     accumulated_era5 <- NULL
#     yearmonths <- unique(format(times, "%Y_%m"))
#     for(ym in yearmonths) {
#       # For precipitation, ERA5 files are named: "precipitation_24-hr_<yyyymm>.nc"
#       era5_file <- file.path(era5_folder, paste0("precipitation_24-hr_", ym, ".nc"))
#       if(!file.exists(era5_file)){
#         message("ERA5 file not found for yearmonth: ", ym)
#         next
#       }
#       era5_rast <- try(terra::rast(era5_file), silent = TRUE)
#       if(inherits(era5_rast, "try-error")){
#         message("Error reading ERA5 file: ", era5_file)
#         next
#       }
#       noaa_ref <- noaa_rast[[1]]
#       era5_rast_resampled <- terra::resample(era5_rast, noaa_ref, method = "near")
#       era5_times <- terra::time(era5_rast_resampled)
#       if(!is.null(era5_times)) {
#         idx <- which(era5_times >= start_time_event & era5_times <= end_time_event)
#         if(length(idx) == 0) next
#         era5_rast_resampled <- era5_rast_resampled[[idx]]
#       }
#       monthly_sum <- terra::app(era5_rast_resampled, fun = sum, na.rm = TRUE)
#       if(is.null(accumulated_era5)) {
#         accumulated_era5 <- monthly_sum
#       } else {
#         accumulated_era5 <- accumulated_era5 + monthly_sum
#       }
#     }
#     
#     if(is.null(accumulated_era5)) {
#       era5_mean <- era5_median <- era5_max <- NA
#     } else {
#       era5_masked <- terra::mask(accumulated_era5, event_mask_binary, maskvalue = FALSE)
#       vals <- terra::values(era5_masked, mat = FALSE)
#       if(length(vals[!is.na(vals)]) > 0) {
#         era5_mean <- mean(vals[!is.na(vals)], na.rm = TRUE)
#         era5_median <- median(vals[!is.na(vals)], na.rm = TRUE)
#         era5_max <- max(vals[!is.na(vals)], na.rm = TRUE)
#       } else {
#         era5_mean <- era5_median <- era5_max <- NA
#       }
#     }
#     
#     impacts_csv1 <- file.path(here::here(noaa_impacts_folder), paste0("NOAA_events_", hazard_name, ".csv"))
#     impacts_csv2 <- file.path(here::here(noaa_impacts_folder), paste0("NOAA_events_", hazard_name, "_2009_2015.csv"))
#     impacts_data <- data.frame()
#     if(file.exists(impacts_csv1)){
#       impacts_data <- read.csv(impacts_csv1, stringsAsFactors = FALSE)
#     }
#     if(file.exists(impacts_csv2)){
#       impacts_data2 <- read.csv(impacts_csv2, stringsAsFactors = FALSE)
#       impacts_data <- rbind(impacts_data, impacts_data2)
#     }
#     if(ncol(impacts_data) > 0) {
#       colnames(impacts_data) <- toupper(trimws(colnames(impacts_data)))
#     }
#     impacts_filtered <- impacts_data %>% dplyr::filter(as.character(.data$EPISODE_ID) == episode_id)
#     
#     injuries_direct <- sum(as.numeric(impacts_filtered$INJURIES_DIRECT), na.rm = TRUE)
#     injuries_indirect <- sum(as.numeric(impacts_filtered$INJURIES_INDIRECT), na.rm = TRUE)
#     deaths_direct <- sum(as.numeric(impacts_filtered$DEATHS_DIRECT), na.rm = TRUE)
#     deaths_indirect <- sum(as.numeric(impacts_filtered$DEATHS_INDIRECT), na.rm = TRUE)
#     damage_property <- sum(as.numeric(impacts_filtered$DAMAGE_PROPERTY), na.rm = TRUE)
#     damage_crops <- sum(as.numeric(impacts_filtered$DAMAGE_CROPS), na.rm = TRUE)
#     exposed_counties <- if(nrow(impacts_filtered) > 0) {
#       paste(unique(impacts_filtered$EVENT_LOCATION), collapse = ";")
#     } else {
#       NA
#     }
#     
#     res_row <- data.frame(
#       episode_id = episode_id,
#       hazard = hazard_name,
#       start_time = start_time_event,
#       end_time = end_time_event,
#       duration = duration_days,
#       exposed_counties = exposed_counties,
#       exposed_area = exposed_area,
#       era5_mean = era5_mean,
#       era5_median = era5_median,
#       era5_max = era5_max,
#       injuries_direct = injuries_direct,
#       injuries_indirect = injuries_indirect,
#       deaths_direct = deaths_direct,
#       deaths_indirect = deaths_indirect,
#       damage_property = damage_property,
#       damage_crops = damage_crops,
#       stringsAsFactors = FALSE
#     )
#     
#     results <- rbind(results, res_row)
#   }
#   
#   return(results)
# }
# ## Implementation ----
# 
# precip_hazard_folders <- c(
#   here::here("data", "output", "04_noaa", "southeast", "flash_flood")
# )
# 
# folder_sizes <- sapply(precip_hazard_folders, function(folder) {
#   length(list.files(folder, pattern = "\\.nc$", full.names = TRUE))
# })
# sorted_folders <- precip_hazard_folders[order(folder_sizes, decreasing = TRUE)]
# precip_hazard_folders <- rev(sorted_folders)
# 
# era5_folder_precip <- here::here("data", "output", "01_era5", "hourly", "precipitation")
# noaa_impacts_folder <- here::here("data", "output", "05_validation", "summary")
# 
# all_precip_results <- future_lapply(precip_hazard_folders, function(h_folder) {
#   hazard_name <- basename(h_folder)
#   message("Processing hazard folder: ", hazard_name)
#   
#   res <- process_noaa_hazard_precip_folder(
#     hazard_folder = h_folder,
#     era5_folder = era5_folder_precip,
#     noaa_impacts_folder = noaa_impacts_folder
#   )
#   
#   output_csv <- file.path(h_folder, paste0("NOAA_", hazard_name, "_summary.csv"))
#   write.csv(res, output_csv, row.names = FALSE)
#   message("Saved CSV for: ", hazard_name)
#   
#   return(res)
# })
# names(all_precip_results) <- sapply(precip_hazard_folders, basename)
# combined_precip_results <- do.call(rbind, all_precip_results)
# combined_csv <- here::here("data", "output", "04_noaa", "southeast_precip_combined_summary.csv")
# write.csv(combined_precip_results, combined_csv, row.names = FALSE)
# message("Combined precipitation summary saved to: ", combined_csv)
# 
# end_time <- Sys.time()
# message(sprintf("Overall processing ended at: %s", end_time))