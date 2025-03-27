#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: Calculate maximum daily 24-hour accumulated precipitation
#study area: Southeast

# Set the project root explicitly using here
library(here)
here::i_am("scripts/03_calculate_precipitation.R")

# Load Libraries and Functions ----
source(here("scripts", "01_library.R"))

start.year <- 1940
end.year <- 2023

#------------------------------------------------------------
# Produce Record of Precipitation (Hourly) ----
#------------------------------------------------------------
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
  
  # Fill NAs if needed ----
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

# #------------------------------------------------------------
# # Calculate Daily Record of Max 24-hr Accumulation Precipitation ----
# #------------------------------------------------------------
# for (year in start.year:end.year) {
#   
#   gc()  # clear memory
#   
#   message(paste0('--------------------- Calculating ', year, ' Maximum Daily 24-hr Precipitation ---------------------'))
#   
#   start.time <- Sys.time()
#   message("Start time: ", start.time)
#   
#   # Load Raster from the appropriate directory
#   precipitation <- raster.compile(year = as.character(year), 
#                                   directory = here("data", "output", "01_variables", "hourly", "precipitation"))
#   
#   # Calculate the maximum daily precipitation from the hourly record
#   precipitation <- terra::tapp(precipitation, 'days', max)
#   
#   save.cdf(raster = precipitation,
#            folder.path = here("data", "output", "01_variables", "daily", "precipitation"),
#            file.name = paste0('precipitation_24-hr_daily_maximum_', year),
#            var.name = 'precipitation',
#            long.name = 'precipitation_24-hr_daily_maximum',
#            unit = 'mm')
#   
#   end.time <- Sys.time()
#   message("Time taken for year ", year, ": ", end.time - start.time)
# }


# # Load Libraries and Functions ----
# source("V:/users/hquintal/phd1_cluster_southeast/scripts/01_library.R")
# 
# start.year <- 1940
# end.year <- 2023
# 
# # Produce Record of Precipitation ----
# for (year in c(start.year:end.year)){
#   
#   # clear
#   gc()
#   
#   # debug
#   # year <- 1940
#   
#   print(paste0('--------------------- Calculating ',year,' Hourly Precipitation ---------------------'))
#   
#   # time
#   start.time <- Sys.time()
#   print(start.time)
#   
#   # Load Rasters 
#   precipitation <- raster.compile(year = as.character(year),directory = "V:/users/hquintal/phd1_cluster_southeast/data/input/precipitation/")
#   
#   # Calculate multi-hour sum, fill NA values ----
#   # using https://stackoverflow.com/questions/74518464/r-terra-rolling-sum-across-layers
#   minus.one <- 23
#   
#   # Calc x-hr sum ----
#   precipitation.roll <- terra::roll(precipitation,24,'sum',type='to')
#   
#   # Add regular values for first two time stamps
#   precipitation.roll[[1:minus.one]] <- precipitation[[1:minus.one]]
#   
#   # Write as precipitation again
#   precipitation <- precipitation.roll
#   
#   # Convert m to mm
#   precipitation <- precipitation * 1000
#   
#   # Fill NAs 
#   
#   # Calc NA values 
#   precipitation.NA <- freq(is.na(precipitation))
#   precipitation.NA <- precipitation.NA %>% group_by(count) %>% summarize()
#   
#   # For loop
#   if (nrow(precipitation.NA) != 1){
#     
#     # Replace NA values with focal mean
#     precipitation.fill <- raster.fill.NA(precipitation)
#     
#     # Crop and mask to original raster
#     precipitation.fill <- raster.crop.mask(raster.target = precipitation.fill,raster.source = precipitation)
#     
#     # Fill remaining NA values (6 min)
#     precipitation.fill <- raster.fill.nearest.neighbor(precipitation.fill)
#     
#     # update original variable
#     precipitation <- precipitation.fill
#     
#   } else {print('No NA values exist')}
#   
#   # Calc NA values 
#   precipitation.NA <- freq(is.na(precipitation))
#   precipitation.NA <- precipitation.NA %>% group_by(count) %>% summarize()
#   
#   # Check remaining NA values
#   if (nrow(precipitation.NA) == 1){
#     
#     print('No NA values remain')
#     
#   } else {print('ERROR! NA values remain')}
#   
#   # fill list with each month
#   for (month in sprintf('%02d',1:12)){
#     
#     # Create time range per month
#     start.date <- lubridate::floor_date(lubridate::ym(paste0(as.character(year),month)),'month') 
#     end.date <- lubridate::ceiling_date(lubridate::ym(paste0(as.character(year),month)),'month') - lubridate::days(1)
#     
#     # subset year by month
#     precipitation.month <- precipitation[[terra::time(precipitation) >= start.date & terra::time(precipitation) <= end.date]]
#     
#     # Save precipitation raster 
#     save.cdf(raster = precipitation.month,
#              folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/01_era5/hourly/precipitation/',
#              file.name = paste0('precipitation_24-hr_',year,month),
#              var.name = 'precipitation',
#              long.name = 'precipitation',
#              unit = 'mm')
#     
#   }
#   
#   # time
#   start.time <- Sys.time()
#   print(start.time)
#   
# }
# 
# # Calculate Daily Record of Max 24-hr accumulation Precipitation ----
# for (year in c(start.year:end.year)){
#   
#   # debug
#   # year <- 1940
#   
#   # Print statement
#   print(paste0('--------------------- Calculating ',year,' Maximum Daily 24-hr Precipitation ---------------------'))
#   
#   # time
#   start.time <- Sys.time()
#   print(start.time)
#   
#   # Load Raster
#   precipitation <- raster.compile(year = as.character(year),directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/precipitation/")
#   
#   # Calculate Max Daily Value
#   precipitation <- terra::tapp(precipitation,'days',max)
#   
#   # Save Heat Index Raster
#   save.cdf(raster = precipitation,
#            folder.path = 'V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/precipitation/',
#            file.name = paste0('precipitation_24-hr_daily_maximum_',year),
#            var.name = 'precipitation',
#            long.name = 'precipitation_24-hr_daily_maximum',
#            unit = 'mm')
#   
#   # time
#   end.time <- Sys.time()
#   time.taken <- end.time - start.time
#   print(time.taken)
#   
# }
