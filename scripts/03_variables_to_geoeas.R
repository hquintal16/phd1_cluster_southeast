#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Reformat heat index variable into geoeas format for input into MatLab to calculate Space Time Metric

library(here)
here::i_am("scripts/03_variables_to_geoeas.R")

# Load Libraries & Functions ----
source(here("scripts", "01_library.R"))

start.year <- 1940
end.year <- 2023

## Heat Index ----
year_month <- yearmo(start.year = 1940, end.year = 2023)

for (year in year_month) {
  
  gc()  # clear memory
  
  message(paste0('--------------------- Creating and Saving Heat Index GeoEAS for ', year, ' ---------------------'))
  
  start.time <- Sys.time()
  message("Start time: ", start.time)
  
  # Load Rasters from the daily heat index output directory
  heat.index <- raster.compile(year = as.character(year),
                               directory = here("data", "output", "01_era5", "daily", "heat_index"))
  
  # Compile into GeoEAS format using the bme.compile function
  heat.index.bme <- bme.compile(raster.var = heat.index,
                                raster.name = 'Heat_Index',
                                raster.units = 'C',
                                time.unit = 'day')
  
  # Save the GeoEAS file to the monthly folder
  save.geoeas(df = heat.index.bme,
              directory = here("data", "output", "02_covariance", "01_geoeas", "heat_index", "month"),
              file.name = 'heat_index')
  
  end.time <- Sys.time()
  message("Time taken for year ", year, ": ", end.time - start.time)
}

## Precipitation ----
# Create a vector of year-month combinations using your yearmo function
year_month <- yearmo(start.year = 1940, end.year = 2023)

for (year in year_month) {
  
  gc()  # clear memory
  
  message(paste0('-------------------- Create and Save Precipitation GeoEAS for ', year, ' --------------------'))
  
  start.time.all <- Sys.time()
  message("Start time: ", start.time.all)
  
  # Load Rasters: using here() to build the path relative to the project root
  precipitation <- raster.compile(year = as.character(year),
                                  directory = here("data", "output", "01_variables", "hourly", "precipitation"))
  
  # Compile into GeoEAS format using the bme.compile function
  precipitation.bme <- bme.compile(raster.var = precipitation,
                                   raster.name = 'Precipitation',
                                   raster.units = 'mm',
                                   time.unit = 'hour')
  
  # Save GeoEAS files using here() for the output directory
  save.geoeas(df = precipitation.bme,
              directory = here("data", "output", "02_covariance", "01_geoeas", "precipitation", "month"),
              file.name = 'precipitation')
  
  end.time.all <- Sys.time()
  message("Time taken: ", end.time.all - start.time.all)
}
