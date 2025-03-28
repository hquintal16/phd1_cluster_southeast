#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: Calculate maximum daily Heat Index derived from Surface Temperature and Surface Dew Point Temperature
#purpose: Calculate 24-hour accumulated precipitation hourly
#study area: Southeast

# Download Weather Forecast Zones CONUS
# https://www.weather.gov/abq/forecasts-local-zonedescription

# Download Southeast Criteria
# https://noaa.maps.arcgis.com/apps/MapJournal/index.html?appid=964c64e07e2d43629d6d7e17facd85fc

# Set the project root explicitly using here
library(here)
here::i_am("scripts/02_clean_variables.R")

# Load Libraries and Functions ----
source(here("scripts", "01_library.R"))

start.year <- 1940
end.year <- 2023

## Hourly Heat Index ----

for (year in start.year:end.year) {
  
  gc()  # clear memory
  
  message(paste0('--------------------- Calculating ', year, ' Hourly Heat Index ---------------------'))
  
  start.time <- Sys.time()
  message("Start time: ", start.time)
  
  # Load Rasters 
  temperature <- raster.compile(year = as.character(year), directory = here("data", "input", "temperature"))
  dewpoint    <- raster.compile(year = as.character(year), directory = here("data", "input", "dewpoint"))
  
  # Convert Kelvin to Celsius
  temperature <- temperature - 273.15 
  dewpoint    <- dewpoint - 273.15 
  
  # Calculate Heat Index 
  heat.index <- calc.heat.index(ambient.temperature = temperature, dewpoint.temperature = dewpoint)
  
  # Fill NA values if necessary
  heat.index.NA <- freq(is.na(heat.index))
  heat.index.NA <- heat.index.NA %>% group_by(count) %>% summarize()
  
  if (nrow(heat.index.NA) != 1) {
    heat.index.fill <- raster.fill.NA(heat.index)
    heat.index.fill <- raster.crop.mask(raster.target = heat.index.fill, raster.source = heat.index)
    heat.index.fill <- raster.fill.nearest.neighbor(heat.index.fill)
    heat.index <- heat.index.fill
  } else {
    message('No NA values exist')
  }
  
  heat.index.NA <- freq(is.na(heat.index))
  heat.index.NA <- heat.index.NA %>% group_by(count) %>% summarize()
  
  if (nrow(heat.index.NA) == 1) {
    message('No NA values remain')
  } else {
    message('ERROR! NA values remain')
  }
  
  # Save Hourly Heat Index Raster as NetCDF
  save.cdf(raster = heat.index,
           folder.path = here("data", "output", "01_era5", "hourly", "heat_index"),
           file.name = paste0('heat_index_', year),
           var.name = 'heat_index',
           long.name = 'heat_index',
           unit = 'C')
  
  end.time <- Sys.time()
  message("Time taken: ", end.time - start.time)
}


## Daily Heat Index ----

for (year in start.year:end.year) {
  
  gc()  # clear memory
  
  message(paste0('--------------------- Calculating ', year, ' Maximum Daily Heat Index ---------------------'))
  
  start.time <- Sys.time()
  message("Start time: ", start.time)
  
  # Load Hourly Heat Index Raster for the year
  heat.index <- raster.compile(year = as.character(year),
                               directory = here("data", "output", "01_era5", "hourly", "heat_index"))
  
  # Calculate Max Daily Value from hourly data
  heat.index <- terra::tapp(heat.index, 'days', max)
  
  # Loop over each month and save the monthly subset
  for (month in sprintf('%02d', 1:12)) {
    start.date <- lubridate::floor_date(lubridate::ym(paste0(as.character(year), month)), 'month') 
    end.date <- lubridate::ceiling_date(lubridate::ym(paste0(as.character(year), month)), 'month') - lubridate::days(1)
    
    heat.index.month <- heat.index[[terra::time(heat.index) >= start.date & terra::time(heat.index) <= end.date]]
    
    save.cdf(raster = heat.index.month,
             folder.path = here("data", "output", "01_era5", "daily", "heat_index"),
             file.name = paste0('heat_index_daily_maximum_', year, month),
             var.name = 'heat_index',
             long.name = 'heat_index_daily_maximum',
             unit = 'C')
  }
  
  end.time <- Sys.time()
  message("Time taken: ", end.time - start.time)
}

## 24-hr Precipitation ----

for (year in start.year:end.year) {
  
  gc()  # clear memory
  
  message(paste0('--------------------- Calculating ', year, ' Hourly Precipitation ---------------------'))
  
  start.time <- Sys.time()
  message("Start time: ", start.time)
  
  # Load Rasters using the here package to build the path
  precipitation <- raster.compile(year = as.character(year), 
                                  directory = here("data", "input", "precipitation"))
  
  # Calculate a 24-hr rolling sum of precipitation
  minus.one <- 23
  precipitation.roll <- terra::roll(precipitation, 24, 'sum', type = 'to')
  
  # For the first 23 layers, use the original values
  precipitation.roll[[1:minus.one]] <- precipitation[[1:minus.one]]
  
  # Overwrite the precipitation variable with the rolling sum
  precipitation <- precipitation.roll
  
  # Convert units: assume original units in meters → convert to mm
  precipitation <- precipitation * 1000
  
  # Fill NAs if needed 
  precipitation.NA <- freq(is.na(precipitation))
  precipitation.NA <- precipitation.NA %>% group_by(count) %>% summarize()
  
  if (nrow(precipitation.NA) != 1) {
    precipitation.fill <- raster.fill.NA(precipitation)
    precipitation.fill <- raster.crop.mask(raster.target = precipitation.fill, raster.source = precipitation)
    precipitation.fill <- raster.fill.nearest.neighbor(precipitation.fill)
    precipitation <- precipitation.fill
  } else {
    message('No NA values exist')
  }
  
  precipitation.NA <- freq(is.na(precipitation))
  precipitation.NA <- precipitation.NA %>% group_by(count) %>% summarize()
  
  if (nrow(precipitation.NA) == 1) {
    message('No NA values remain')
  } else {
    message('ERROR! NA values remain')
  }
  
  # For each month, subset the raster and save a NetCDF file
  for (month in sprintf('%02d', 1:12)) {
    start.date <- lubridate::floor_date(lubridate::ym(paste0(as.character(year), month)), 'month')
    end.date <- lubridate::ceiling_date(lubridate::ym(paste0(as.character(year), month)), 'month') - lubridate::days(1)
    
    precipitation.month <- precipitation[[terra::time(precipitation) >= start.date & terra::time(precipitation) <= end.date]]
    
    save.cdf(raster = precipitation.month,
             folder.path = here("data", "output", "01_era5", "hourly", "precipitation"),
             file.name = paste0('precipitation_24-hr_', year, month),
             var.name = 'precipitation',
             long.name = 'precipitation',
             unit = 'mm')
  }
  
  end.time <- Sys.time()
  message("Time taken for year ", year, ": ", end.time - start.time)
  
}