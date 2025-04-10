#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: compile all extreme points, conduct kNN analysis to calculate epsilon, cluster extreme points
#outputs saved in folder: V:\users\hquintal\phd2_southeast\data\output\z_02_cluster
#study area: Southeast

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/11_cluster_dbscan.R")  # Adjust this path as needed
source(here::here("scripts", "01_library.R"))

process_extremes_data <- function(input_dir, output_dir) {
  
  # Define subfolder names for output file naming
  subfolder_name <- basename(input_dir)
  subsubfolder_name <- basename(dirname(input_dir))
  
  # Load region raster (with its CRS)
  region.crs <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))
  
  # Create US land mask from states
  us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  us.states.rast <- st_rasterize(us.states %>% dplyr::select(1, geom))
  us.states.rast <- terra::rast(us.states.rast)
  us.states.rast <- us.states.rast / us.states.rast
  terra::crs(us.states.rast) <- terra::crs(region.crs)
  us.states.rast <- terra::crop(us.states.rast, region.crs)
  us.states.rast <- terra::extend(us.states.rast, region.crs)
  
  # Convert the raster mask to a polygon and buffer it
  us_states_poly <- terra::as.polygons(us.states.rast, dissolve = TRUE)
  us_states_sf <- st_as_sf(us_states_poly)
  
  # Buffer by ~1 degree (111000 m, approximate for geographic CRS)
  buffered_us_states_sf <- st_buffer(us_states_sf, dist = 111000)
  
  # Clip the buffered polygon to the extent of the original raster
  rast_extent <- st_as_sfc(st_bbox(us.states.rast))
  final_buffered_polygon <- st_intersection(buffered_us_states_sf, rast_extent)
  
  # Transform to EPSG:4326 (WGS84) to match CSV lat/long values
  final_buffered_polygon_fixed <- st_transform(final_buffered_polygon, crs = 4326)
  
  # Read and compile CSV files
  csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    message("No CSV files found in ", input_dir, ". Skipping.")
    stop()
  }
  
  plan(multicore, workers = 19)
  compiled_data <- with_progress({
    p <- progressor(along = csv_files)
    data_list <- future_lapply(csv_files, function(file) {
      p()
      suppressMessages(read_csv(file, progress = FALSE))
    })
    bind_rows(data_list)
  })
  
  # If precipitation data, use datetime; otherwise use date (for Heat Index)
  if (grepl("precipitation", subfolder_name, ignore.case = TRUE)) {
    
    # Convert compiled_data to an sf object without altering original lat/long
    compiled_data_clean <- compiled_data %>% dplyr::select(lat, long, datetime, observation, threshold)
    points_sf <- st_as_sf(compiled_data_clean, coords = c("long", "lat"), crs = 4326)
    
    # Subset points within the buffered polygon
    inside_logical <- st_within(points_sf, final_buffered_polygon_fixed, sparse = FALSE)[, 1]
    points_inside <- points_sf[inside_logical, ]
    
    # Snap each point to the grid cell center from region.crs without updating original lat/long
    pts_coords <- st_coordinates(points_inside)
    cell_idx <- terra::cellFromXY(region.crs, pts_coords)
    grid_centers <- terra::xyFromCell(region.crs, cell_idx)
    
    # Replace the coordinates with the grid center (the original CSV values remain available in the data)
    points_inside$long <- grid_centers[, 1]
    points_inside$lat  <- grid_centers[, 2]
    
    # Prepare final data for clustering
    points_final_df <- as.data.frame(st_drop_geometry(points_inside))
    extremes <- as_tibble(points_final_df)
    
    # Create normalization data frames for latitudes and longitudes
    lat.norm <- data.frame(values = sort(unique(extremes$lat)),
                           position = seq_along(unique(extremes$lat)))
    long.norm <- data.frame(values = sort(unique(extremes$long)),
                            position = seq_along(unique(extremes$long)))
    
    # Merge normalization values into extremes data
    extremes.normalized <- merge(extremes, lat.norm, by.x = "lat", by.y = "values", all = TRUE)
    colnames(extremes.normalized)[ncol(extremes.normalized)] <- "lat.norm"
    extremes.normalized <- merge(extremes.normalized, long.norm, by.x = "long", by.y = "values", all = TRUE)
    colnames(extremes.normalized)[ncol(extremes.normalized)] <- "long.norm"
    
    # Normalize the date variable (convert to numeric)
    extremes.normalized$date.norm <- as.numeric(extremes.normalized$date)
    extremes.normalized$date.norm <- extremes.normalized$date.norm / 3600
    extremes.normalized$lat.norm <- as.numeric(extremes.normalized$lat.norm)
    extremes.normalized$long.norm <- as.numeric(extremes.normalized$long.norm)
    
    # Create space–time cube and perform clustering
    st.cube <- extremes.normalized
    gc()
    
    # Calculate kNN for DBSCAN (using k = 3 and expecting mu = 4)
    kNN <- cluster.kNN.2(var.st.cube = st.cube[, 6:8], neighbors = 3)
    gc()
    
    # Sort by normalized lat, long, and date
    st.cube <- arrange(st.cube, lat.norm, long.norm, date.norm)
    
    # Perform DBSCAN clustering
    clusters <- dbscan::dbscan(st.cube[, 6:8], eps = kNN$epsilon, minPts = kNN$mu)
    st.cube$cluster <- clusters$cluster
    st.cube$mu <- clusters$minPts
    st.cube$eps <- clusters$eps
    st.cube <- as_tibble(st.cube)
    
    # Write output CSV file
    output_file <- file.path(output_dir,
                             paste0(subfolder_name, "_", subsubfolder_name, "_clustered_extremes.csv"))
    write.csv(st.cube, output_file, row.names = FALSE)
    message("Output written to: ", output_file)
    
    gc()
    
  } else { # HEAT INDEX
    
    # Convert compiled_data to an sf object without altering original lat/long
    compiled_data_clean <- compiled_data %>% dplyr::select(lat, long, date, observation, threshold)
    points_sf <- st_as_sf(compiled_data_clean, coords = c("long", "lat"), crs = 4326)
    
    # Subset points within the buffered polygon
    inside_logical <- st_within(points_sf, final_buffered_polygon_fixed, sparse = FALSE)[, 1]
    points_inside <- points_sf[inside_logical, ]
    
    # Snap each point to the grid cell center from region.crs without updating original lat/long
    pts_coords <- st_coordinates(points_inside)
    cell_idx <- terra::cellFromXY(region.crs, pts_coords)
    grid_centers <- terra::xyFromCell(region.crs, cell_idx)
    
    # Replace the coordinates with the grid center (the original CSV values remain available in the data)
    points_inside$long <- grid_centers[, 1]
    points_inside$lat  <- grid_centers[, 2]
    
    # Prepare final data for clustering
    points_final_df <- as.data.frame(st_drop_geometry(points_inside))
    extremes <- as_tibble(points_final_df)
    
    # Create normalization data frames for latitudes and longitudes
    lat.norm <- data.frame(values = sort(unique(extremes$lat)),
                           position = seq_along(unique(extremes$lat)))
    long.norm <- data.frame(values = sort(unique(extremes$long)),
                            position = seq_along(unique(extremes$long)))
    
    # Merge normalization values into extremes data
    extremes.normalized <- merge(extremes, lat.norm, by.x = "lat", by.y = "values", all = TRUE)
    colnames(extremes.normalized)[ncol(extremes.normalized)] <- "lat.norm"
    extremes.normalized <- merge(extremes.normalized, long.norm, by.x = "long", by.y = "values", all = TRUE)
    colnames(extremes.normalized)[ncol(extremes.normalized)] <- "long.norm"
    
    # Normalize the date variable (convert to numeric)
    extremes.normalized$date.norm <- as.numeric(extremes.normalized$date)
    extremes.normalized$lat.norm <- as.numeric(extremes.normalized$lat.norm)
    extremes.normalized$long.norm <- as.numeric(extremes.normalized$long.norm)
    
    # Create space–time cube and perform clustering
    st.cube <- extremes.normalized
    gc()
    
    # Calculate kNN for DBSCAN (using k = 3 and expecting mu = 4)
    kNN <- cluster.kNN.2(var.st.cube = st.cube[, 6:8], neighbors = 3)
    gc()
    
    # Sort by normalized lat, long, and date
    st.cube <- arrange(st.cube, lat.norm, long.norm, date.norm)
    
    # Perform DBSCAN clustering
    clusters <- dbscan::dbscan(st.cube[, 6:8], eps = kNN$epsilon, minPts = kNN$mu)
    st.cube$cluster <- clusters$cluster
    st.cube$mu <- clusters$minPts
    st.cube$eps <- clusters$eps
    st.cube <- as_tibble(st.cube)
    
    # Write output CSV file
    output_file <- file.path(output_dir,
                             paste0(subfolder_name, "_", subsubfolder_name, "_clustered_extremes.csv"))
    write.csv(st.cube, output_file, row.names = FALSE)
    message("Output written to: ", output_file)
    
    gc()  
    
  }
  
}

# Calc Clusters ----
input_dirs <- c(
  here::here("data", "output", "03_cluster", "01_extreme_points", "0.25", "heat_index"),
  here::here("data", "output", "03_cluster", "01_extreme_points", "0.3075", "heat_index"),
  here::here("data", "output", "03_cluster", "01_extreme_points", "0.39", "heat_index")
)

# Define the common output directory
output_dir <- here::here("data", "output", "03_cluster", "02_cluster")

# Loop over each input directory and process the extremes data.
for (in_dir in input_dirs) {
  message("Processing input directory: ", in_dir)
  process_extremes_data(in_dir, output_dir)
}

# Summary Info ----
heat_index_0.25_deg_day <- read.csv(here::here("data", "output", "03_cluster", "02_cluster", "heat_index_0.25_clustered_extremes.csv"))
heat_index_0.39_deg_day <- read.csv(here::here("data", "output", "03_cluster", "02_cluster", "heat_index_0.39_clustered_extremes.csv"))
heat_index_0.3075_deg_day <- read.csv(here::here("data", "output", "03_cluster", "02_cluster", "heat_index_0.3075_clustered_extremes.csv"))
precipitation_0.25_deg_hour <- read.csv(here::here("data", "output", "03_cluster", "02_cluster", "precipitation_stm1_clustered_extremes.csv"))

cluster_results <- data.frame(
  hazard = c('Heat','Heat','Heat','Precipitation'),
  resolution = c(0.25, 0.3075, 0.39, 0.25),
  mu = c(heat_index_0.25_deg_day$mu[1],
         heat_index_0.3075_deg_day$mu[1],
         heat_index_0.39_deg_day$mu[1],
         precipitation_0.25_deg_hour$mu[1]),
  eps = c(heat_index_0.25_deg_day$eps[1],
          heat_index_0.3075_deg_day$eps[1],
          heat_index_0.39_deg_day$eps[1],
          precipitation_0.25_deg_hour$eps[1]),
  points = c(nrow(heat_index_0.25_deg_day),
             nrow(heat_index_0.3075_deg_day),
             nrow(heat_index_0.39_deg_day),
             nrow(precipitation_0.25_deg_hour)),
  noise = c(sum(heat_index_0.25_deg_day$cluster == 0),
            sum(heat_index_0.3075_deg_day$cluster == 0),
            sum(heat_index_0.39_deg_day$cluster == 0),
            sum(precipitation_0.25_deg_hour$cluster == 0)),
  clusters = c(max(unique(heat_index_0.25_deg_day$cluster)),
               max(unique(heat_index_0.3075_deg_day$cluster)),
               max(unique(heat_index_0.39_deg_day$cluster)),
               max(unique(precipitation_0.25_deg_hour$cluster)))
)

write.csv(cluster_results, here::here("data", "output", "03_cluster", "02_cluster", "summary.csv"))
