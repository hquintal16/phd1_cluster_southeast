#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: rasterize clusters
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/12_cluster_rasterize.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Load spatraster with correct CRS 
region.crs <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))

# Create US land mask
us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
us.states.rast <- stars::st_rasterize(us.states %>% dplyr::select(1, geom))
us.states.rast <- terra::rast(us.states.rast)
us.states.rast <- us.states.rast / us.states.rast
terra::crs(us.states.rast) <- terra::crs(region.crs)
us.states.rast <- terra::crop(us.states.rast, region.crs)
us.states.rast <- terra::extend(us.states.rast, region.crs)

# Read NetCDF file as raster
var <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))

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

# Heat Index Advisory ----

## 0.25 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv(here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.25_clustered_extremes.csv")))
heat <- heat %>% dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat,
  reference_raster = cube,
  output_dir = here::here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.25", "heat_index","raw")
)

## 0.3075 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv(here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.3075_clustered_extremes.csv")))
heat <- heat %>% dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1.23)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat, 
  reference_raster = cube, 
  output_dir = here::here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.3075", "heat_index","raw")
)

## 0.39 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv(here::here("data", "output", "03_cluster", "02_cluster","advisory", "heat_index_0.39_clustered_extremes.csv")))
heat <- heat %>% dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1.56)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat, 
  reference_raster = cube, 
  output_dir = here::here("data", "output", "03_cluster", "02_cluster","advisory", "points", "0.39", "heat_index","raw")
)


# Heat Index Warning ----

## 0.25 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv(here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.25_clustered_extremes.csv")))
heat <- heat %>% dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat,
  reference_raster = cube,
  output_dir = here::here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.25", "heat_index","raw")
)

## 0.3075 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv(here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.3075_clustered_extremes.csv")))
heat <- heat %>% dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1.23)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat, 
  reference_raster = cube, 
  output_dir = here::here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.3075", "heat_index","raw")
)

## 0.39 deg / day ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv(here::here("data", "output", "03_cluster", "02_cluster","warning", "heat_index_0.39_clustered_extremes.csv")))
heat <- heat %>% dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1.56)
cube <- cube[[1]]
values(cube) <- NA

process_clusters(
  heat_data = heat, 
  reference_raster = cube, 
  output_dir = here::here("data", "output", "03_cluster", "02_cluster","warning", "points", "0.39", "heat_index","raw")
)

# Precipitation ----
process_clusters_precip <- function(precip_data, reference_raster, output_dir) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Extract unique clusters from precip_data
  unique_clusters <- unique(precip_data$cluster)

  # Set up progress handler with progressr
  progressr::handlers(global = TRUE)
  progressr::with_progress({
    p <- progressr::progressor(along = unique_clusters)

    # Process each cluster sequentially
    for (cluster_id in unique_clusters) {
      gc()  # Clean memory before each iteration

      tryCatch({
        # Filter data for the current cluster
        precip_cluster <- precip_data %>% dplyr::filter(cluster == cluster_id)
        if (nrow(precip_cluster) == 0) next

        # Convert the datetime column into a standardized string.
        # If no time is provided, append " 00:00:00" to each date string.
        precip_cluster$datetime <- as.character(precip_cluster$datetime)
        precip_cluster$datetime <- ifelse(grepl(" ", precip_cluster$datetime),
                                          precip_cluster$datetime,
                                          paste0(precip_cluster$datetime, " 00:00:00"))
        # Convert to POSIXct
        dt_all <- as.POSIXct(precip_cluster$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

        # Extract unique, sorted datetimes for this cluster
        unique_datetimes <- sort(unique(dt_all))
        if (length(unique_datetimes) == 0) next

        # Create an empty raster with the same dimensions, extent, and CRS as the reference,
        # with one layer per unique datetime.
        temp_cluster <- terra::rast(nrows = terra::nrow(reference_raster),
                                    ncols = terra::ncol(reference_raster),
                                    ext   = terra::ext(reference_raster),
                                    crs   = terra::crs(reference_raster),
                                    nlyr  = length(unique_datetimes))
        temp_cluster <- terra::setValues(temp_cluster, NA)
        terra::time(temp_cluster) <- as.Date(unique_datetimes)
        names(temp_cluster) <- format(unique_datetimes, "%Y-%m-%d %H:%M:%S")

        # For each datetime, assign the observation values to grid cells where events occurred.
        for (i in seq_along(unique_datetimes)) {
          this_dt <- unique_datetimes[i]
          # Filter the cluster data for the current datetime
          date_subset <- precip_cluster[as.POSIXct(precip_cluster$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") == this_dt, ]
          if (nrow(date_subset) > 0) {
            # Compute cell indices using the coordinates (assuming 'long' and 'lat' match reference_raster CRS)
            cell_indices <- terra::cellFromXY(temp_cluster[[i]], cbind(date_subset$long, date_subset$lat))
            vals <- terra::values(temp_cluster[[i]])
            # Replace NAs with the observation values at the computed cell indices.
            # (If multiple points fall in the same cell, the last one will override.)
            vals[cell_indices] <- date_subset$observation
            temp_cluster[[i]] <- terra::setValues(temp_cluster[[i]], vals)
          }
        }

        # Save the resulting raster as a NetCDF file
        output_path <- file.path(output_dir, paste0(as.Date(unique_datetimes[1]), "_cluster_", cluster_id, ".nc"))
        terra::writeCDF(temp_cluster, output_path, overwrite = TRUE)

        gc()  # Clean memory after processing each cluster
      }, error = function(e) {
        message(sprintf("Error processing cluster %s: %s", cluster_id, e$message))
      })

      p()  # Update progress bar after processing each cluster
    }
  })
}

## 0.25 deg / hour ----
options(pillar.sigfig = 15)
heat <- as_tibble(read.csv(here::here("data", "output", "03_cluster", "02_cluster","24hr1yr", "0.25_24hr1yr_clustered_extremes.csv")))
heat <- heat %>% dplyr::filter(cluster > 0)

cube <- create.st.cube(target.raster = var, space.time.metric = 1)
cube <- cube[[1]]
values(cube) <- NA

process_clusters_precip(
  precip_data = heat,
  reference_raster = cube,
  output_dir = here::here("data", "output", "03_cluster", "02_cluster","24hr1yr", "points","raw")
)
