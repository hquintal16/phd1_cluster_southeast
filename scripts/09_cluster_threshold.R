#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: convert thresholds to shapefiles 
#study area: Southeast

# Load Libraries & Functions ----
library(here)
here::i_am("scripts/09_cluster_threshold.R")  # Adjust the file path if needed
source(here::here("scripts", "01_library.R"))

# first get the code from original script
## Read in koppen-geiger geotiffs
directory <- "V:/users/hquintal/phd2_southeast/data/input/regional_aggregation/koppen_geiger/1991_2020/koppen_geiger_0p1.tif"
regions <- terra::rast(directory)

# Load US states data using the maps package and convert it to an sf object
us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Convert us.states to raster
us.states.rast <- stars::st_rasterize(us.states %>% dplyr::select(1,geom))
us.states.rast <- terra::rast(us.states.rast)

# Normalize raster as 1 = USA land, NA = ocean / Not USA land
us.states.rast <- us.states.rast / us.states.rast

# Give correct crs as var.1
terra::crs(us.states.rast) <- terra::crs(regions)

# Resample usa outline to era5 resolution
us.states.rast <- terra::resample(us.states.rast,regions)

# Heat Index ----

## Define WFO areas ----
cwa <- c('CAR','GYX','BOX','OKX','BIS','FGF','DLH','MOT','UNR','ABR','FSD','MPX','ARX',
         'MKX','APX','GRB','GRR','DTX','LBF','DVN','IWX','BTV','ALY','BUF','BGM','CTP',
         'CLE','ILN','RLX','PBZ','AMA','LUB','SJT','FWD','TSA','SHV','LZK','JAN','MEG',
         'OHX','HUN','BMX','MRX','FFC','GLD','GID','OAX','DMX','LOT','TOP','EAX','ILX',
         'IND','DDC','ICT','SGF','LSX','PAH','LMK','JKL','AKQ','RNK','RAH','GSP','MHX',
         'ILM','HGX','LCH','LIX','MOB','TAE','JAX','TBW','MFL','MLB','CRP','CAE','CHS',
         'BRO','OUN')

# Define WFO areas with complex reporting
troublemakers <- c('ABQ','EPZ','MAF','EWX','RIW','CYS','BOU','GJT','PUB','LWX','PHI')

# Read in shapefile
wfo <- sf::st_read('V:/users/hquintal/phd2_southeast/data/input/threshold/z_05mr24/z_05mr24.shp')
wfo <- wfo[wfo$STATE %in% c('AL','KY','OH','LA','OK','AZ','ME','OR','AR','MD',
                            'PA','MA','CA','MI','RI','CO','MN','SC','CT','MS',
                            'SD','DE','MO','TN','DC','MT','TX','FL','NE','GA',
                            'NV','UT','NH','VT','NJ','VA','ID','NM','IL','NY',
                            'WA','IN','NC','WV','IA','ND','WI','KS','WY'), ]

# Subset
troublemakers <- wfo %>% dplyr::filter(CWA %in% troublemakers)

# Deal with troublemakers
# Subset
ABQ <- troublemakers %>% dplyr::filter(CWA %in% 'ABQ')
EPZ <- troublemakers %>% dplyr::filter(CWA %in% 'EPZ')
MAF <- troublemakers %>% dplyr::filter(CWA %in% 'MAF')
EWX <- troublemakers %>% dplyr::filter(CWA %in% 'EWX')
RIW <- troublemakers %>% dplyr::filter(CWA %in% 'RIW')
CYS <- troublemakers %>% dplyr::filter(CWA %in% 'CYS')
GJT <- troublemakers %>% dplyr::filter(CWA %in% 'GJT')
PUB <- troublemakers %>% dplyr::filter(CWA %in% 'PUB')
LWX <- troublemakers %>% dplyr::filter(CWA %in% 'LWX')
PHI <- troublemakers %>% dplyr::filter(CWA %in% 'PHI')
BOU <- troublemakers %>% dplyr::filter(CWA %in% 'BOU')

# LWX 
LWX.DC <- LWX %>% dplyr::filter(STATE %in% 'DC')
LWX.VA <- LWX %>% dplyr::filter(STATE %in% 'VA')
LWX.MD <- LWX %>% dplyr::filter(STATE %in% 'MD')
LWX.WV <- LWX %>% dplyr::filter(STATE %in% 'WV')

# VA
LWX.VA.west <- LWX.VA %>% dplyr::filter(ZONE %in% c(paste0('0',c(028,031,030,029,027,026,025)),c(503,504,507,508)))
LWX.VA.east <- LWX.VA %>% dplyr::filter(! ZONE %in% c(paste0('0',c(028,031,030,029,027,026,025)),c(503,504,507,508)))

# MD
LWX.MD.west <- LWX.MD %>% dplyr::filter(ZONE %in% c(paste0('00',c(001,003)),c(501,502)))
LWX.MD.east <- LWX.MD %>% dplyr::filter(! ZONE %in% c(paste0('00',c(001,003)),c(501,502)))

# define criteria
LWX.DC$CRITERIA <- 105
LWX.WV$CRITERIA <- 100
LWX.VA.west$CRITERIA <- 100
LWX.VA.east$CRITERIA <- 105
LWX.MD.west$CRITERIA <- 100
LWX.MD.east$CRITERIA <- 105

# combine
LWX <- rbind(LWX.DC,LWX.WV,LWX.VA.west,LWX.VA.east,LWX.MD.west,LWX.MD.east)

# PHI 
PHI.DE <- PHI %>% dplyr::filter(STATE %in% 'DE')
PHI.MD <- PHI %>% dplyr::filter(STATE %in% 'MD')
PHI.NJ <- PHI %>% dplyr::filter(STATE %in% 'NJ')
PHI.PA <- PHI %>% dplyr::filter(STATE %in% 'PA')

# NJ
PHI.NJ.north <- PHI.NJ %>% dplyr::filter(ZONE %in% c(paste0('00',c(001,007,008,009)),
                                                     paste0('0',c(010,012,015,013,014,026,019,020,027))))
PHI.NJ.south <- PHI.NJ %>% dplyr::filter(! ZONE %in% c(paste0('00',c(001,007,008,009)),
                                                       paste0('0',c(010,012,015,013,014,026,019,020,027))))

# define criteria
PHI.DE$CRITERIA <- 105
PHI.MD$CRITERIA <- 105
PHI.PA$CRITERIA <- 100
PHI.NJ.north$CRITERIA <- 100
PHI.NJ.south$CRITERIA <- 105

# combine
PHI <- rbind(PHI.DE,PHI.MD,PHI.PA,PHI.NJ.north,PHI.NJ.south)

# Single WFO Criteria 
# Subset to only wfo's with criteria
wfo <- wfo %>% dplyr::filter(CWA %in% cwa)

# Define ADVISORY
advisory.095 <- c('CAR','GYX','BOX','OKX')
advisory.100 <- c('BIS','FGF','DLH','MOT','UNR','ABR','FSD','MPX','ARX','MKX',
                  'APX','GRB','GRR','DTX','LBF','DVN','IWX','BTV','ALY','BUF',
                  'BGM','CTP','CLE','ILN','RLX','PBZ')
advisory.105 <- c('AMA','LUB','SJT','FWD','TSA','SHV','LZK','JAN','MEG','OHX',
                  'HUN','BMX','MRX','FFC','GLD','GID','OAX','DMX','LOT','TOP',
                  'EAX','ILX','IND','DDC','ICT','SGF','LSX','PAH','LMK','JKL',
                  'AKQ','RNK','RAH','GSP','MHX','ILM','OUN')
advisory.108 <- c('HGX','LCH','LIX','MOB','TAE','JAX','TBW','MFL','MLB')
advisory.110 <- c('CRP','CAE','CHS')
advisory.111 <- c('BRO')

# Define WARNING
warning.105 <- c('BIS','FGF','DLH','MOT','UNR','ABR','FSD','MPX','ARX','MKX',
                 'APX','GRB','GRR','DTX','LBF','DVN','IWX','CAR','GYX','BTV',
                 'ALY','BOX','OKX','BGM','BUF','CTP','PBZ','CLE','ILN','RLX')
warning.110 <- c('AMA','LUB','SJT','FWD','TSA','SHV','LZK','JAN','MEG','OHX',
                 'HUN','BMX','MRX','FFC','GLD','GID','OAX','DMX','LOT','TOP',
                 'EAX','ILX','IND','DDC','ICT','SGF','LSX','PAH','LMK','JKL')
warning.113 <- c('HGX','LCH','LIX','MOB','TAE','JAX','TBW','MFL','AKQ','RNK',
                 'RAH','GSP','MHX','ILM','MLB','OUN')
warning.115 <- c('CRP','CAE','CHS')
warning.116 <- c('BRO')

# Subset to relevant offices
# wfos
wfo.adv.095 <- wfo %>% dplyr::filter(CWA %in% advisory.095)
wfo.adv.100 <- wfo %>% dplyr::filter(CWA %in% advisory.100)
wfo.adv.105 <- wfo %>% dplyr::filter(CWA %in% advisory.105)
wfo.adv.108 <- wfo %>% dplyr::filter(CWA %in% advisory.108)
wfo.adv.110 <- wfo %>% dplyr::filter(CWA %in% advisory.110)
wfo.adv.111 <- wfo %>% dplyr::filter(CWA %in% advisory.111)

wfo.warn.105 <- wfo %>% dplyr::filter(CWA %in% warning.105)
wfo.warn.110 <- wfo %>% dplyr::filter(CWA %in% warning.110)
wfo.warn.113 <- wfo %>% dplyr::filter(CWA %in% warning.113)
wfo.warn.115 <- wfo %>% dplyr::filter(CWA %in% warning.115)
wfo.warn.116 <- wfo %>% dplyr::filter(CWA %in% warning.116)

# Deal with troublemakers
# 095 
maf <- c(paste0('0',c(027)),c(270,271,275,280,277))
riw <- c(paste0('00',c(001,002,008,009)),paste0('0',c(012,024,014,015)))
gjt <- c(paste0('00',c(004,009)),paste0('0',c(013,012,010,017,018,019)))
bou <- c(paste0('0',c(035,033,031,034)))
pub <- c(paste0('0',c(067,068,066,065,073,063,061,060,082,075,074)))

advisory.095 <- rbind( # green, mountains
  ABQ %>% dplyr::filter(ZONE %in% c(202,206,208,210,211,212,213,214,215,221,226,227)),
  EPZ %>% dplyr::filter(ZONE %in% c(415,426,425)),
  MAF %>% dplyr::filter(ZONE %in% maf),
  RIW %>% dplyr::filter(ZONE %in% riw),
  CYS %>% dplyr::filter(ZONE %in% c(112,103,114)),
  GJT %>% dplyr::filter(ZONE %in% gjt),
  BOU %>% dplyr::filter(ZONE %in% bou),
  PUB %>% dplyr::filter(ZONE %in% pub)
)

# 100
advisory.100 <- rbind( # blue, plains/range/valleys
  ABQ %>% dplyr::filter(ZONE %in% c(201,203,204,205,207,216,217,218,219,222,223,224,228,229,241)),
  EPZ %>% dplyr::filter(ZONE %in% c(414,416,401,408,403)),
  MAF %>% dplyr::filter(ZONE %in% c(276,279,278)),
  RIW %>% dplyr::filter(! ZONE %in% riw),
  CYS %>% dplyr::filter(! ZONE %in% c(112,103,114)),
  GJT %>% dplyr::filter(! ZONE %in% gjt),
  BOU %>% dplyr::filter(! ZONE %in% bou),
  PUB %>% dplyr::filter(! ZONE %in% pub)
)

# 105
maf <- c(paste0('0',c(028,075,082,052,048,063,062,061,068,060,029,033,034,045,050,046,051,047,053,070,69)),c(272,273))

advisory.105 <- rbind( # orange
  ABQ %>% dplyr::filter(ZONE %in% c(209,220,225,230,231,232,233,234,235,236,237,238,239,240)),
  EPZ %>% dplyr::filter(ZONE %in% c(409,411,404,405,406,418,420,421,422,424,417,423,407,419,410,429,428,427)),
  MAF %>% dplyr::filter(ZONE %in% maf)
)

advisory.108 <- rbind( # yellow
  EWX %>% dplyr::filter(! ZONE %in% c(183,202,217,218,228))
)

# 110 
maf <- c(paste0('0',c(067,059)),c(281,282,274))

advisory.110 <- rbind( # pink
  MAF %>% dplyr::filter(ZONE %in% maf),
  EWX %>% dplyr::filter(ZONE %in% c(183,202,217,218,228))
)

# combine troublemakers
wfo.adv.095 <- rbind(wfo.adv.095,advisory.095)
wfo.adv.100 <- rbind(wfo.adv.100,advisory.100)
wfo.adv.105 <- rbind(wfo.adv.105,advisory.105)
wfo.adv.108 <- rbind(wfo.adv.108,advisory.108)
wfo.adv.110 <- rbind(wfo.adv.110,advisory.110)

# assign values
wfo.adv.095$CRITERIA <- 95
wfo.adv.100$CRITERIA <- 100
wfo.adv.105$CRITERIA <- 105
wfo.adv.108$CRITERIA <- 108
wfo.adv.110$CRITERIA <- 110
wfo.adv.111$CRITERIA <- 111

# combine
wfo <- rbind(wfo.adv.095,wfo.adv.100,wfo.adv.105,wfo.adv.108,wfo.adv.110,
             wfo.adv.111,LWX,PHI)

# update column name
names(wfo)[names(wfo) == 'CRITERIA'] <- 'ADVISORY'

# create warning column
wfo$WARNING <- wfo$ADVISORY + 5

# convert F to C
wfo$ADVISORY <- (wfo$ADVISORY-32)*5/9
wfo$WARNING <- (wfo$WARNING-32)*5/9

expand_shapefile_nearest_parallel <- function(shp, 
                                              lat_min, lat_max, lon_min, lon_max, 
                                              resolution, 
                                              cols_to_expand = c(), 
                                              n_workers = 19,
                                              simplify_tolerance = NULL) {
  
  # Verify columns exist
  if (!all(cols_to_expand %in% names(shp))) {
    stop("Some cols_to_expand are not present in the shapefile.")
  }
  
  # Optional geometry simplification
  if (!is.null(simplify_tolerance)) {
    shp <- st_simplify(shp, dTolerance = simplify_tolerance, preserveTopology = TRUE)
  }
  
  # Set future globals max size to avoid serialization error
  options(future.globals.maxSize = +Inf)
  plan(multisession, workers = n_workers)
  
  # Create the grid
  bbox_grid <- st_bbox(c(xmin = lon_min, ymin = lat_min, xmax = lon_max, ymax = lat_max), 
                       crs = st_crs(shp))
  expanded_grid <- st_make_grid(
    st_as_sfc(bbox_grid),
    cellsize = resolution,
    what = "polygons"
  ) %>% st_as_sf()
  
  # Identify centroids
  grid_centroids <- st_centroid(expanded_grid)
  
  # Check intersections
  within_original <- st_intersects(grid_centroids, shp, sparse = FALSE)[, 1]
  
  grid_within <- expanded_grid[within_original, ]
  grid_outside <- expanded_grid[!within_original, ]
  
  # Assign attributes to inside grid cells
  grid_within <- st_join(grid_within, shp[, cols_to_expand], join = st_intersects, left = TRUE)
  
  # For outside, split into manageable chunks
  if (nrow(grid_outside) > 0) {
    grid_chunks <- split(grid_outside, cut(seq_len(nrow(grid_outside)), n_workers * 4, labels = FALSE))
    
    handlers(global = TRUE)
    p <- progressor(steps = length(grid_chunks))
    
    # Use centroid coordinates to reduce serialization overhead
    shp_centroids <- st_centroid(shp)
    shp_attrs <- st_drop_geometry(shp[, cols_to_expand])
    shp_points <- cbind(st_coordinates(shp_centroids), shp_attrs)
    
    nearest_outside_parallel <- future_lapply(grid_chunks, function(chunk) {
      chunk_centroids <- st_centroid(chunk)
      chunk_coords <- st_coordinates(chunk_centroids)
      
      # Use simple nearest neighbor search
      nn_index <- RANN::nn2(data = shp_points[,1:2], query = chunk_coords, k = 1)$nn.idx[,1]
      
      nearest_attrs <- shp_attrs[nn_index, ]
      
      p()  # Update progress
      
      bind_cols(chunk, nearest_attrs)
    })
    
    grid_outside_final <- bind_rows(nearest_outside_parallel)
  } else {
    grid_outside_final <- grid_outside
  }
  
  result <- bind_rows(grid_within, grid_outside_final)
  
  # Reset plan and global size
  plan(sequential)
  options(future.globals.maxSize = 500 * 1024^2)  # reset to default ~500MB
  
  return(result)
}

# Define the new extent
lat_min <- 24
lat_max <- 40
lon_min <- -95
lon_max <- -75
resolution <- 0.05

# Columns you wish to expand
cols_to_expand <- c("ADVISORY", "WARNING")

# Expand the shapefile
expanded_shp <- expand_shapefile_nearest_parallel(
  shp = wfo,
  lat_min = lat_min,
  lat_max = lat_max,
  lon_min = lon_min,
  lon_max = lon_max,
  resolution = resolution,
  cols_to_expand = cols_to_expand,
  n_workers = 19
  
)

## Save output 
st_write(expanded_shp, here::here("data", "output","03_cluster", "00_threshold", "wfo_southeast_expansion.shp"), delete_layer = TRUE)

# Read output
expanded_shp <- sf::st_read(here::here("data", "output","03_cluster", "00_threshold", "wfo_southeast_expansion.shp"))

# dissolve
# Dissolve polygons by attributes (removing internal boundaries)
simplified_shp <- expanded_shp %>%
  group_by(ADVISORY, WARNING) %>%   # group by your attribute columns
  summarise(geometry = st_union(geometry), .groups = 'drop')

plot(simplified_shp$geometry,col = simplified_shp$ADVISORY)

## Save output 
st_write(simplified_shp, here::here("data", "output","03_cluster", "00_threshold", "wfo_southeast_expansion_dissolved.shp"), delete_layer = TRUE)

# Precipitation ----

## Read in koppen-geiger geotiffs
directory <- here::here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")
regions <- terra::rast(directory)

# Read in domain of interest
domain <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))
domain <- domain[[1]] / domain[[1]]

united_states <- us.states.rast

## 1-year 24-hr ----
atlas.14 <- threshold.compile(
  ne.dir = here::here("data", "input", "threshold", "ne1yr24ha", "ne1yr24ha.asc"),
  orb.dir = here::here("data", "input", "threshold", "orb1yr24ha", "orb1yr24ha.asc"),
  se.dir  = here::here("data", "input", "threshold", "se1yr24ha", "se1yr24ha.asc"),
  sw.dir  = here::here("data", "input", "threshold", "sw1yr24ha", "sw1yr24ha.asc"),
  tx.dir  = here::here("data", "input", "threshold", "tx1yr24ha", "tx1yr24ha.asc"),
  mw.dir  = here::here("data", "input", "threshold", "mw1yr24ha", "mw1yr24ha.asc")
)

expand_spatraster_nearest_parallel <- function(raster_obj,
                                               lon_min, lon_max,
                                               lat_min, lat_max,
                                               resolution,
                                               band_name = "koppen_geiger_0p1",
                                               n_workers = 4,
                                               simplify_tolerance = NULL) {
  
  #### Step 1. Crop the Raster and Force Continuous Numeric Values ####
  
  # Define the desired extent (xmin, xmax, ymin, ymax)
  desired_ext <- ext(lon_min, lon_max, lat_min, max = lat_max)
  
  # Crop the original raster
  atlas_cropped <- crop(raster_obj, desired_ext)
  
  # Remove any levels (to avoid categorical interpretation) and force numeric conversion:
  levels(atlas_cropped) <- NULL
  atlas_cropped <- atlas_cropped * 1.0
  
  # Check: cell values should be continuous
  print("Cropped raster values (continuous):")
  print(head(values(atlas_cropped, mat = FALSE)))
  
  #### Step 2. Rebuild the Raster (to ensure no RAT persists) ####
  
  atlas_cropped_num <- rast(atlas_cropped)
  levels(atlas_cropped_num) <- NULL
  values(atlas_cropped_num) <- as.numeric(values(atlas_cropped))
  
  print("Rebuilt raster values (continuous):")
  print(head(values(atlas_cropped_num, mat = FALSE)))
  
  #### Step 3. Convert the Clean Raster to Polygons ####
  
  # Use dissolve = FALSE and values = TRUE so each cell becomes a polygon with its raw value
  atlas_poly <- as.polygons(atlas_cropped_num, dissolve = FALSE, values = TRUE)
  names(atlas_poly) <- band_name
  
  # Convert the SpatVector to an sf object
  atlas_poly_sf <- st_as_sf(atlas_poly)
  
  # Ensure the attribute is numeric (using as.character to break any factor conversion)
  atlas_poly_sf[[band_name]] <- as.numeric(as.character(atlas_poly_sf[[band_name]]))
  
  # Optionally simplify the geometry to speed up further spatial operations
  if (!is.null(simplify_tolerance)) {
    atlas_poly_sf <- st_simplify(atlas_poly_sf, dTolerance = simplify_tolerance, preserveTopology = TRUE)
  }
  
  print("Polygon attribute values (should be continuous):")
  print(head(atlas_poly_sf[[band_name]]))
  
  #### Step 4. Create a Regular Grid over the Target Extent ####
  
  # Build a grid (sf polygons) that covers the entire desired extent
  bbox_grid <- st_bbox(c(xmin = lon_min, ymin = lat_min, xmax = lon_max, ymax = lat_max),
                       crs = st_crs(atlas_poly_sf))
  grid_sf <- st_make_grid(st_as_sfc(bbox_grid), cellsize = resolution, what = "polygons") %>% st_as_sf()
  
  #### Step 5. Determine Grid Cells that Are "Inside" the Original Polygon and Those That Are Outside ####
  
  # Identify grid cell centroids for efficient spatial matching
  grid_centroids <- st_centroid(grid_sf)
  inside_idx <- st_intersects(grid_centroids, atlas_poly_sf, sparse = FALSE)[, 1]
  
  grid_inside <- grid_sf[inside_idx, ]
  grid_outside <- grid_sf[!inside_idx, ]
  
  # For grid cells that intersect the original polygons, join directly the attribute values.
  grid_inside <- st_join(grid_inside, atlas_poly_sf[, band_name], join = st_intersects, left = TRUE)
  
  #### Step 6. For Grid Cells Outside, Assign Values via Nearest Neighbor Search ####
  
  if (nrow(grid_outside) > 0) {
    # Split the grid for parallel processing (arbitrarily split into n_workers * 4 chunks)
    n_chunks <- n_workers * 4
    grid_chunks <- split(grid_outside, cut(seq_len(nrow(grid_outside)), n_chunks, labels = FALSE))
    
    # Set up the parallel plan and memory options
    options(future.globals.maxSize = +Inf)
    plan(multisession, workers = n_workers)
    
    # Precompute centroids and coordinates from the polygonized raster
    poly_centroids <- st_centroid(atlas_poly_sf)
    poly_coords <- st_coordinates(poly_centroids)
    poly_values <- as.data.frame(atlas_poly_sf)[, band_name, drop = FALSE]
    
    # Process each chunk in parallel using nearest neighbor search
    nn_vals_list <- future_lapply(grid_chunks, function(chunk) {
      chunk_centroids <- st_centroid(chunk)
      chunk_coords <- st_coordinates(chunk_centroids)
      
      # Find nearest polygon centroid for each chunk cell
      nn_index <- RANN::nn2(data = poly_coords, query = chunk_coords, k = 1)$nn.idx[, 1]
      nearest_attrs <- poly_values[nn_index, , drop = FALSE]
      
      cbind(chunk, nearest_attrs)
    })
    
    grid_outside_final <- do.call(rbind, nn_vals_list)
    
    # Reset parallel plan and options
    plan(sequential)
    options(future.globals.maxSize = 500 * 1024^2)
  } else {
    grid_outside_final <- grid_outside
  }
  
  #### Step 7. Combine the Grids and Finalize the Output ####
  
  expanded_sf <- bind_rows(grid_inside, grid_outside_final)
  expanded_sf[[band_name]] <- as.numeric(as.character(expanded_sf[[band_name]]))
  
  return(expanded_sf)
}


# Example usage:
# Suppose atlas.14 is your SpatRaster. To subset and expand over c(-95, -75, 24, 40)
# with a 0.1 resolution grid, you would call:
expanded_sf <- expand_spatraster_nearest_parallel(
  raster_obj = atlas.14,
  lon_min = -95, lon_max = -75,
  lat_min = 24, lat_max = 40,
  resolution = 0.05,
  band_name = "koppen_geiger_0p1",
  n_workers = 19,
  simplify_tolerance = NULL
)

ggplot(expanded_sf) +
  geom_sf(aes(fill = koppen_geiger_0p1), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "mm") +
  theme_minimal()

head(unique(atlas.14$ne1yr24ha))
head(unique(expanded_sf$koppen_geiger_0p1))

# st_write(expanded_sf, here::here("data", "input", "threshold", "atlas14_shapefile.shp"), delete_layer = TRUE)
st_write(expanded_sf, here::here("data", "output","03_cluster", "00_threshold", "atlas14_southeast_expansion_dissolved.shp"), delete_layer = TRUE)
