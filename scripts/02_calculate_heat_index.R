#Setup----
#Updated January 2025
#Linked to GitHub
#Hunter Quintal
#purpose: Calculate maximum daily Heat Index derived from Surface Temperature and Surface Dew Point Temperature
#outputs saved in folder: V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index
#outputs saved in folder: V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/heat_index
#study area: Southeast

# Download Weather Forecast Zones CONUS
# https://www.weather.gov/abq/forecasts-local-zonedescription

# Download Southeast Criteria
# https://noaa.maps.arcgis.com/apps/MapJournal/index.html?appid=964c64e07e2d43629d6d7e17facd85fc

# Load Libraries and Functions ----
source("V:/users/hquintal/phd2_southeast/scripts/01_library.R")

start.year <- 1940
end.year <- 2023

# Calc Hourly Heat Index ----
for (year in c(start.year:end.year)){
  
  # clear
  gc()
  
  # debug
  # year <- 1940
  
  print(paste0('--------------------- Calculating ',year,' Hourly Heat Index ---------------------'))
  
  # time
  start.time <- Sys.time()
  print(start.time)

  # Load Rasters 
  temperature <- raster.compile(year = as.character(year),directory = "V:/users/hquintal/phd2_southeast/data/input/temperature/")
  dewpoint <- raster.compile(year = as.character(year),directory = "V:/users/hquintal/phd2_southeast/data/input/dewpoint/")
  
  # Convert K to C
  temperature <- temperature - 273.15 
  dewpoint <- dewpoint - 273.15 
  
  # Calc HI 
  heat.index <- calc.heat.index(ambient.temperature = temperature,dewpoint.temperature = dewpoint)
  
  # Fill NAs
  
  # Calc NA values 
  heat.index.NA <- freq(is.na(heat.index))
  heat.index.NA <- heat.index.NA %>% group_by(count) %>% summarize()
  
  # For loop
  if (nrow(heat.index.NA) != 1){
    
    # Replace NA values with focal mean
    heat.index.fill <- raster.fill.NA(heat.index)
    
    # Crop and mask to original raster
    heat.index.fill <- raster.crop.mask(raster.target = heat.index.fill,raster.source = heat.index)
    
    # Fill remaining NA values (6 min)
    heat.index.fill <- raster.fill.nearest.neighbor(heat.index.fill)
    
    # update original variable
    heat.index <- heat.index.fill
    
  } else {print('No NA values exist')}
  
  # Calc NA values 
  heat.index.NA <- freq(is.na(heat.index))
  heat.index.NA <- heat.index.NA %>% group_by(count) %>% summarize()
  
  # Check if any remaining NA values
  if (nrow(heat.index.NA) == 1){
    
    print('No NA values remain')
    
  } else {print('ERROR! NA values remain')}
  
  # Save Heat Index Raster
  save.cdf(raster = heat.index,
           folder.path = 'V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/heat_index/',
           file.name = paste0('heat_index_',year),
           var.name = 'heat_index',
           long.name = 'heat_index',
           unit = 'C')
  
  # time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}

# Calc Daily Heat Index ----
for (year in c(start.year:end.year)){
  
  # clear
  gc()
  
  # debug
  # year <- 1940
  
  # Print statement
  print(paste0('--------------------- Calculating ',year,' Maximum Daily Heat Index ---------------------'))
  
  # time
  start.time <- Sys.time()
  print(start.time)
  
  # Load Raster
  heat.index <- raster.compile(year = as.character(year),
                        directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/heat_index/")
  
  # Calculate Max Daily Value
  heat.index <- terra::tapp(heat.index,'days',max)
  
  # fill list with each month
  for (month in sprintf('%02d',1:12)){
    
    # Create time range per month
    start.date <- lubridate::floor_date(lubridate::ym(paste0(as.character(year),month)),'month') 
    end.date <- lubridate::ceiling_date(lubridate::ym(paste0(as.character(year),month)),'month') - lubridate::days(1)
    
    # subset year by month
    heat.index.month <- heat.index[[terra::time(heat.index) >= start.date & terra::time(heat.index) <= end.date]]
    
    # Save precipitation raster 
    save.cdf(raster = heat.index.month,
             folder.path = 'V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/',
             file.name = paste0('heat_index_daily_maximum_',year,month),
             var.name = 'heat_index',
             long.name = 'heat_index_daily_maximum',
             unit = 'C')
    
  }
  
  # time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}
