#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: create cluster rasters
#outputs saved in folder: V:\users\hquintal\phd2_southeast\data\output\z_02_cluster\points
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
  
# Read NetCDF file as raster
var <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/heat_index_daily_maximum_194001.nc')

# Heat Index ----

process_clusters <- function(heat_data, reference_raster, output_dir) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Extract unique cluster IDs from heat_data
  unique_clusters <- unique(heat_data$cluster)
  
  # Set up progress handlers and run within a progressr block
  progressr::handlers(global = TRUE)
  progressr::with_progress({
    p <- progressr::progressor(along = unique_clusters)
    
    # Process each cluster sequentially
    for (cluster_id in unique_clusters) {
      gc()  # Clean memory before processing each cluster
      tryCatch({
        # Filter heat_data for the current cluster
        heat_cluster <- heat_data %>% dplyr::filter(cluster == cluster_id)
        if (nrow(heat_cluster) == 0) next
        
        # Extract unique dates from the data
        unique_dates <- sort(unique(heat_cluster$date))
        if (length(unique_dates) == 0) next
        
        # Create an empty raster (using reference_raster dimensions, extent, CRS) with one layer per unique date
        temp_cluster <- terra::rast(nrows = terra::nrow(reference_raster),
                                    ncols = terra::ncol(reference_raster),
                                    ext   = terra::ext(reference_raster),
                                    crs   = terra::crs(reference_raster),
                                    nlyr  = length(unique_dates))
        temp_cluster <- terra::setValues(temp_cluster, NA)
        terra::time(temp_cluster) <- as.Date(unique_dates)
        names(temp_cluster) <- as.character(unique_dates)
        
        # For each date layer, assign the observation value to grid cells based on the heat event coordinates.
        for (i in seq_along(unique_dates)) {
          date_subset <- heat_cluster %>% dplyr::filter(date == unique_dates[i])
          if (nrow(date_subset) > 0) {
            # Compute cell indices using actual longitude and latitude columns.
            cell_indices <- terra::cellFromXY(temp_cluster[[i]],
                                              cbind(date_subset$long, date_subset$lat))
            vals <- terra::values(temp_cluster[[i]])
            # Instead of writing the cluster id, write the observation value.
            vals[cell_indices] <- date_subset$observation
            temp_cluster[[i]] <- terra::setValues(temp_cluster[[i]], vals)
          }
        }
        
        # Save the resulting raster as a NetCDF file.
        output_path <- file.path(output_dir, paste0(unique_dates[[1]], "_cluster_", cluster_id, ".nc"))
        try({
          terra::writeCDF(temp_cluster, output_path, overwrite = TRUE)
        }, silent = TRUE)
        
        gc()  # Clean memory after processing each cluster
        
      }, error = function(e) {
        message(sprintf("Error processing cluster %d: %s", cluster_id, e$message))
      })
      
      p()  # Update progress bar after processing each cluster
    }
  })
}

## 0.25 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv("V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/heat_index_stm1_clustered_extremes.csv" ))
heat <- heat %>%
  dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat,
  reference_raster = cube,
  output_dir = "V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/points/stm1/heat_index"
)

## 0.39 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv("V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/heat_index_record_clustered_extremes.csv"))
heat <- heat %>%
  dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1.564182)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat, 
  reference_raster = cube, 
  output_dir = "V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/points/record/heat_index"
)

## 1.00 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv("V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/heat_index_stm4_clustered_extremes.csv"))
heat <- heat %>%
  dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 4)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat, 
  reference_raster = cube, 
  output_dir = "V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/points/stm4/heat_index"
)

test <- terra::rast('V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/points/stm4/heat_index/1944-06-03_cluster_1.nc')
plot(test)
# Rename clusters chronologically to contain 0001 - 9999 event id's


# Precipitation ----

# Precipitation Hazard Function
process_clusters_precip <- function(precip_data, reference_raster, output_dir) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract unique clusters from precip_data
  unique_clusters <- unique(precip_data$cluster)
  
  # Set up progress handler
  handlers(global = TRUE)
  with_progress({
    p <- progressor(along = unique_clusters)
    
    # Process each cluster sequentially
    for (cluster_id in unique_clusters) {
      gc()  # Clean memory before each iteration
      tryCatch({
        # Filter data for the current cluster
        precip_cluster <- precip_data %>% filter(cluster == cluster_id)
        if (nrow(precip_cluster) == 0) next
        
        # Extract unique datetimes (as character) from the data
        unique_datetimes <- sort(unique(precip_cluster$datetime))
        if (length(unique_datetimes) == 0) next
        
        unique_datetimes <- as.character(unique_datetimes)
        # If no time is provided, append " 00:00:00" to each date string
        updated_datetimes <- ifelse(grepl(" ", unique_datetimes),
                                    unique_datetimes,
                                    paste0(unique_datetimes, " 00:00:00"))
        # Convert to POSIXct
        date_times <- as.POSIXct(updated_datetimes, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        
        # Create an empty raster with the same dimensions, extent, and CRS as the reference,
        # with one layer per unique datetime.
        temp_cluster <- rast(nrows = nrow(reference_raster),
                             ncols = ncol(reference_raster),
                             ext   = ext(reference_raster),
                             crs   = crs(reference_raster),
                             nlyr  = length(date_times))
        temp_cluster <- setValues(temp_cluster, NA)
        terra::time(temp_cluster) <- as.Date(date_times)
        names(temp_cluster) <- format(date_times, "%Y-%m-%d %H:%M:%S")
        
        # For each datetime, assign the "observation" values to grid cells corresponding to event coordinates.
        for (i in seq_along(unique_datetimes)) {
          # Use the "datetime" column for filtering
          date_subset <- precip_cluster %>% filter(datetime == unique_datetimes[i])
          if (nrow(date_subset) > 0) {
            # Compute cell indices using the actual coordinates (assumes 'long' and 'lat' are in the same CRS as reference_raster)
            cell_indices <- cellFromXY(temp_cluster[[i]], cbind(date_subset$long, date_subset$lat))
            vals <- values(temp_cluster[[i]])
            # Replace the NA values at these cell indices with the corresponding observation values.
            # (If multiple points fall in the same cell, the last one will be used.)
            vals[cell_indices] <- date_subset$observation
            temp_cluster[[i]] <- setValues(temp_cluster[[i]], vals)
          }
        }
        
        # Save the resulting raster as a NetCDF file
        output_path <- file.path(output_dir, paste0(as.Date(unique_datetimes[[1]]), "_cluster_", cluster_id, ".nc"))
        try({
          writeCDF(temp_cluster, output_path, overwrite = TRUE)
        }, silent = TRUE)
        
        gc()  # Clean memory after processing each cluster
      }, error = function(e) {
        message(sprintf("Error processing cluster %d: %s", cluster_id, e$message))
      })
      p()  # Update progress bar after processing each cluster
    }
  })
}

## 0.25 deg / hour ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv("V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/precipitation_stm1_clustered_extremes.csv"))
heat <- heat %>%
  dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1)
cube <- cube[[1]]
values(cube) <- NA

process_clusters_precip(
  precip_data = heat, 
  reference_raster = cube, 
  output_dir = "V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/points/stm1/precipitation"
)

## 1.00 deg / hour ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv("V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/precipitation_stm4_clustered_extremes.csv"))
heat <- heat %>%
  dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 4)
cube <- cube[[1]]
values(cube) <- NA

process_clusters_precip(
  precip_data = heat, 
  reference_raster = cube, 
  output_dir = "V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/points/stm4/precipitation"
)

# DEFUNCT ----
# process_clusters <- function(heat_data, reference_raster, output_dir) {
#   # Ensure output directory exists
#   if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
#   
#   # Extract unique clusters
#   unique_clusters <- unique(heat_data$cluster)
#   
#   # Set up progress handlers and run within with_progress
#   progressr::handlers(global = TRUE)
#   progressr::with_progress({
#     p <- progressr::progressor(along = unique_clusters)
#     
#     # Sequential Processing
#     for (cluster_id in unique_clusters) {
#       gc()  # Clear memory before each iteration
#       # message(sprintf("Starting cluster %d", cluster_id))
#       
#       tryCatch({
#         heat_cluster <- heat_data %>% dplyr::filter(cluster == cluster_id)
#         if (nrow(heat_cluster) == 0) next  # Skip empty clusters
#         
#         unique_dates <- sort(unique(heat_cluster$date))
#         if (length(unique_dates) == 0) next
#         
#         # Create a new empty raster
#         temp_cluster <- terra::rast(nrows = terra::nrow(reference_raster),
#                                     ncols = terra::ncol(reference_raster),
#                                     ext   = terra::ext(reference_raster),
#                                     crs   = terra::crs(reference_raster),
#                                     nlyr  = length(unique_dates))
#         temp_cluster <- terra::setValues(temp_cluster, NA)
#         terra::time(temp_cluster) <- as.Date(unique_dates)
#         names(temp_cluster) <- as.character(unique_dates)
#         
#         for (i in seq_along(unique_dates)) {
#           date_subset <- heat_cluster %>% dplyr::filter(date == unique_dates[i])
#           
#           if (nrow(date_subset) > 0) {
#             cell_indices <- terra::cellFromRowCol(temp_cluster[[1]], 
#                                                   date_subset$lat.norm, 
#                                                   date_subset$long.norm)
#             vals <- terra::values(temp_cluster[[i]])
#             vals[cell_indices] <- cluster_id
#             temp_cluster[[i]] <- terra::setValues(temp_cluster[[i]], vals)
#           }
#         }
#         
#         # Save raster as NetCDF
#         output_path <- file.path(output_dir, paste0(unique_dates[[1]], "_cluster_", cluster_id, ".nc"))
#         try({
#           terra::writeCDF(temp_cluster, output_path, overwrite = TRUE)
#         }, silent = TRUE)
#         
#         gc()  # Clean memory after each cluster
#         
#       }, error = function(e) {
#         message(sprintf("Error processing cluster %d: %s", cluster_id, e$message))
#       })
#       
#       p()  # Update progress bar after processing each cluster
#     }
#   })
# }
# # Heat Index Processing Function (Parallelizable version)
# process_clusters <- function(heat_data, reference_raster, output_dir) {
#   # Ensure output directory exists
#   if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
#   
#   # Extract unique cluster IDs from heat_data
#   unique_clusters <- unique(heat_data$cluster)
#   
#   # Set up progress handlers and run within a progressr block
#   progressr::handlers(global = TRUE)
#   progressr::with_progress({
#     p <- progressr::progressor(along = unique_clusters)
#     
#     # Process each cluster sequentially
#     for (cluster_id in unique_clusters) {
#       gc()  # Clean memory before processing each cluster
#       tryCatch({
#         # Filter heat_data for the current cluster
#         heat_cluster <- heat_data %>% dplyr::filter(cluster == cluster_id)
#         if (nrow(heat_cluster) == 0) next
#         
#         # Extract unique dates from the data
#         unique_dates <- sort(unique(heat_cluster$date))
#         if (length(unique_dates) == 0) next
#         
#         # Create an empty raster (using reference_raster's dimensions, extent, CRS) with one layer per date
#         temp_cluster <- terra::rast(nrows = terra::nrow(reference_raster),
#                                     ncols = terra::ncol(reference_raster),
#                                     ext   = terra::ext(reference_raster),
#                                     crs   = terra::crs(reference_raster),
#                                     nlyr  = length(unique_dates))
#         temp_cluster <- terra::setValues(temp_cluster, NA)
#         terra::time(temp_cluster) <- as.Date(unique_dates)
#         names(temp_cluster) <- as.character(unique_dates)
#         
#         # For each date, mark the grid cells corresponding to the event coordinates with the cluster ID
#         for (i in seq_along(unique_dates)) {
#           date_subset <- heat_cluster %>% dplyr::filter(date == unique_dates[i])
#           if (nrow(date_subset) > 0) {
#             # Use cellFromXY to compute cell indices from coordinates
#             cell_indices <- terra::cellFromXY(temp_cluster[[i]], 
#                                               cbind(date_subset$long.norm, date_subset$lat.norm))
#             vals <- terra::values(temp_cluster[[i]])
#             vals[cell_indices] <- cluster_id
#             temp_cluster[[i]] <- terra::setValues(temp_cluster[[i]], vals)
#           }
#         }
#         
#         # Save the resulting raster as a NetCDF file
#         output_path <- file.path(output_dir, paste0(unique_dates[[1]], "_cluster_", cluster_id, ".nc"))
#         try({
#           terra::writeCDF(temp_cluster, output_path, overwrite = TRUE)
#         }, silent = TRUE)
#         
#         gc()  # Clean memory after processing each cluster
#         
#       }, error = function(e) {
#         message(sprintf("Error processing cluster %d: %s", cluster_id, e$message))
#       })
#       
#       p()  # Update progress bar after each cluster
#     }
#   })
# }

# process_clusters <- function(heat_data, reference_raster, output_dir) {
#   # Ensure output directory exists
#   if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
#   
#   # Extract unique cluster IDs
#   unique_clusters <- unique(heat_data$cluster)
#   
#   # Set up progress handlers and run within a progressr block
#   progressr::handlers(global = TRUE)
#   progressr::with_progress({
#     p <- progressr::progressor(along = unique_clusters)
#     
#     # Process each cluster sequentially
#     for (cluster_id in unique_clusters) {
#       gc()  # Clear memory before processing each cluster
#       tryCatch({
#         # Filter heat_data for the current cluster
#         heat_cluster <- heat_data %>% dplyr::filter(cluster == cluster_id)
#         if (nrow(heat_cluster) == 0) next
#         
#         # Extract unique dates from the data
#         unique_dates <- sort(unique(heat_cluster$date))
#         if (length(unique_dates) == 0) next
#         
#         # Create an empty raster (using reference_raster's dimensions, extent, CRS)
#         # with one layer per unique date
#         temp_cluster <- terra::rast(nrows = terra::nrow(reference_raster),
#                                     ncols = terra::ncol(reference_raster),
#                                     ext   = terra::ext(reference_raster),
#                                     crs   = terra::crs(reference_raster),
#                                     nlyr  = length(unique_dates))
#         temp_cluster <- terra::setValues(temp_cluster, NA)
#         terra::time(temp_cluster) <- as.Date(unique_dates)
#         names(temp_cluster) <- as.character(unique_dates)
#         
#         # For each date layer, assign the cluster ID to cells corresponding to event coordinates.
#         # Use the actual 'long' and 'lat' columns.
#         for (i in seq_along(unique_dates)) {
#           date_subset <- heat_cluster %>% dplyr::filter(date == unique_dates[i])
#           if (nrow(date_subset) > 0) {
#             # Compute cell indices using cellFromXY, with x=long and y=lat
#             cell_indices <- terra::cellFromXY(temp_cluster[[i]], 
#                                               cbind(date_subset$long, date_subset$lat))
#             vals <- terra::values(temp_cluster[[i]])
#             vals[cell_indices] <- cluster_id
#             temp_cluster[[i]] <- terra::setValues(temp_cluster[[i]], vals)
#           }
#         }
#         
#         # Save the resulting raster as a NetCDF file
#         output_path <- file.path(output_dir, paste0(unique_dates[[1]], "_cluster_", cluster_id, ".nc"))
#         try({
#           terra::writeCDF(temp_cluster, output_path, overwrite = TRUE)
#         }, silent = TRUE)
#         
#         gc()  # Clean memory after processing each cluster
#         
#       }, error = function(e) {
#         message(sprintf("Error processing cluster %d: %s", cluster_id, e$message))
#       })
#       
#       p()  # Update progress bar after processing each cluster
#     }
#   })
# }

# process_clusters <- function(precip_data, reference_raster, output_dir) {
#   
#   # debug
#   # precip_data = heat
#   # reference_raster = cube
#   # output_dir = "V:/users/hquintal/phd2_southeast/data/output/z_02_cluster/points/stm4/precipitation"
#   
#   # Ensure output directory exists
#   if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
#   
#   # Extract unique clusters
#   unique_clusters <- unique(precip_data$cluster)
#   
#   # Set up progress handlers and run within a with_progress block
#   progressr::handlers(global = TRUE)
#   progressr::with_progress({
#     p <- progressr::progressor(along = unique_clusters)
#     
#     # Sequential processing for each cluster
#     for (cluster_id in unique_clusters) {
#       gc()  # Clear memory before each iteration
#       
#       # debug
#       # cluster_id <- unique_clusters[[1]]
#       
#       message(sprintf("Starting cluster %d", cluster_id))
#       
#       tryCatch({
#         precip_cluster <- precip_data %>% dplyr::filter(cluster == cluster_id)
#         if (nrow(precip_cluster) == 0) next  # Skip empty clusters
#         
#         unique_dates <- sort(unique(precip_cluster$datetime))
#         if (length(unique_dates) == 0) next
#         
#         # Ensure unique_dates is a character vector
#         unique_dates <- as.character(unique_dates)
#         
#         # If no time is provided, append " 00:00:00" to each date
#         updated_dates <- ifelse(grepl(" ", unique_dates),
#                                 unique_dates,
#                                 paste0(unique_dates, " 00:00:00"))
#         
#         # Convert to POSIXct using the correct format
#         date_times <- as.POSIXct(updated_dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#         
#         # Create the empty SpatRaster
#         temp_cluster <- terra::rast(nrows = terra::nrow(reference_raster),
#                                     ncols = terra::ncol(reference_raster),
#                                     ext   = terra::ext(reference_raster),
#                                     crs   = terra::crs(reference_raster),
#                                     nlyr  = length(date_times))
#         temp_cluster <- terra::setValues(temp_cluster, NA)
#         terra::time(temp_cluster) <- date_times
#         names(temp_cluster) <- format(date_times, "%Y-%m-%d %H:%M:%S")
#         
#         for (i in seq_along(unique_dates)) {
#           date_subset <- precip_cluster %>% dplyr::filter(date == unique_dates[i])
#           
#           if (nrow(date_subset) > 0) {
#             cell_indices <- terra::cellFromRowCol(temp_cluster[[1]], 
#                                                   date_subset$lat.norm, 
#                                                   date_subset$long.norm)
#             vals <- terra::values(temp_cluster[[i]])
#             vals[cell_indices] <- cluster_id
#             temp_cluster[[i]] <- terra::setValues(temp_cluster[[i]], vals)
#           }
#         }
#         
#         # Save raster as NetCDF
#         output_path <- file.path(output_dir, paste0(as.Date(unique_dates[[1]]), "_cluster_", cluster_id, ".nc"))
#         try({
#           terra::writeCDF(temp_cluster, output_path, overwrite = TRUE)
#         }, silent = TRUE)
#         
#         gc()  # Clean memory after each cluster
#         
#       }, error = function(e) {
#         message(sprintf("Error processing cluster %d: %s", cluster_id, e$message))
#       })
#       
#       p()  # Update progress bar after processing each cluster
#     }
#   })
# }