#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Reformat heat index variable into geoeas format for input into MatLab to calculate Space Time Metric

# Set project root explicitly (assuming the script is located at "V:/users/hquintal/phd1_cluster_southeast/scripts/02_calculate_heat_index.R")
library(here)
here::i_am("scripts/04_geoeas_heat_index")

# Load Libraries & Functions ----
source(here("scripts", "01_library.R"))

start.year <- 1940
end.year <- 2023

# Monthly Aggregation ----
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

# # Annual Aggregation ----
# for (year in start.year:end.year) {
#   
#   gc()  # clear memory
#   
#   message(paste0('--------------------- Creating and Saving Heat Index GeoEAS for ', year, ' ---------------------'))
#   
#   start.time <- Sys.time()
#   message("Start time: ", start.time)
#   
#   # Load the daily heat index raster for the year
#   heat.index <- raster.compile(year = as.character(year),
#                                directory = here("data", "output", "01_variables", "daily", "heat_index"))
#   
#   # Calculate the maximum daily heat index from the hourly record
#   heat.index <- terra::tapp(heat.index, 'days', max)
#   
#   # Save the resulting raster to the annual folder
#   save.geoeas(raster = heat.index,
#               directory = here("data", "output", "02_geoeas", "01_geoeas", "heat_index", "year"),
#               file.name = paste0('heat_index_daily_maximum_', year),
#               var.name = 'heat_index',
#               long.name = 'heat_index_daily_maximum',
#               unit = 'C')
#   
#   end.time <- Sys.time()
#   message("Time taken for year ", year, ": ", end.time - start.time)
# }


# # Load Libraries & Functions ----
# source("V:/users/hquintal/phd1_cluster_southeast/scripts/01_library.R")
# 
# # Monthly Aggregation ----
# year_month <- yearmo(start.year = 1940,
#                      end.year = 2023)
# 
# # Load Rasters 
# for (year in year_month){
#   
#   # clear
#   gc()
#   
#   # debug
#   # year <- 1940
#   
#   # print statement
#   print(paste0('-------------------- Create and Save Heat Index GeoEAS for ',year,' --------------------'))
#   
#   # time
#   start.time.all <- Sys.time()
#   print(start.time.all)
#   
#   # load year
#   heat.index <- raster.compile(year = as.character(year),
#                                   directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/")
#   
#   # compile into geoeas format
#   heat.index.bme <- bme.compile(raster.var = heat.index,raster.name = 'Heat_Index',
#                                 raster.units = 'C',time.unit = 'day')
#   
#   # Save geoeas files
#   save.geoeas(df = heat.index.bme,
#               directory = 'V:/users/hquintal/phd2_southeast/data/output/02_geoeas/01_geoeas/heat_index/month/',
#               file.name = 'heat_index')
#   
#   # time
#   end.time.all <- Sys.time()
#   time.taken <- end.time.all - start.time.all
#   print(time.taken)
#   
# }
# 
# # Annual Aggregation ----
# 
# start.year <- 1940
# end.year <- 2023
# 
# # Load Rasters
# for (year in c(start.year:end.year)){
# 
#   # clear
#   gc()
# 
#   # debug
#   # year <- 1940
# 
#   # print statement
#   print(paste0('-------------------- Create and Save Heat Index GeoEAS for ',year,' --------------------'))
# 
#   # time
#   start.time.all <- Sys.time()
#   print(start.time.all)
# 
#   # load year
#   heat.index <- raster.compile(year = as.character(year),
#                         directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/daily/heat_index/")
# 
#   # compile into geoeas format
#   heat.index.bme <- bme.compile(raster.var = heat.index,raster.name = 'Heat_Index',
#                          raster.units = 'C',time.unit = 'day')
# 
#   # Save geoeas files
#   save.geoeas(df = heat.index.bme,
#               directory = 'V:/users/hquintal/phd2_southeast/data/output/02_geoeas/01_geoeas/heat_index/year/',
#               file.name = 'heat_index')
# 
#   # time
#   end.time.all <- Sys.time()
#   time.taken <- end.time.all - start.time.all
#   print(time.taken)
# 
# }
