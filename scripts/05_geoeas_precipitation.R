#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Reformat precipitation variable into geoeas format for input into MatLab to calculate Space Time Metric

# Set the project root explicitly using here
library(here)
here::i_am("scripts/05_geoeas_precipitation.R")

# Load Libraries & Functions ----
source(here("scripts", "01_library.R"))

# Create a vector of year-month combinations using your yearmo function
year_month <- yearmo(start.year = 1940, end.year = 2023)

# Monthly Aggregation ----
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


# # Load Libraries & Functions ----
# source("V:/users/hquintal/phd2_southeast/scripts/01_library.R")
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
#   print(paste0('-------------------- Create and Save Precipitation GeoEAS for ',year,' --------------------'))
#   
#   # time
#   start.time.all <- Sys.time()
#   print(start.time.all)
#   
#   # load year
#   precipitation <- raster.compile(year = as.character(year),
#                                directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/precipitation/")
#   
#   # compile into geoeas format
#   precipitation.bme <- bme.compile(raster.var = precipitation,raster.name = 'Precipitation',
#                          raster.units = 'mm',time.unit = 'hour')
#   
#   # Save geoeas files
#   save.geoeas(df = precipitation.bme,
#               directory = 'V:/users/hquintal/phd2_southeast/data/output/02_geoeas/01_geoeas/precipitation/month/',
#               file.name = 'precipitation')
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
#   print(paste0('-------------------- Create and Save Precipitation GeoEAS for ',year,' --------------------'))
#   
#   # time
#   start.time.all <- Sys.time()
#   print(start.time.all)
#   
#   # load year
#   precipitation <- raster.compile(year = as.character(year),
#                                   directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/precipitation/")
#   
#   # compile into geoeas format
#   precipitation.bme <- bme.compile(raster.var = precipitation,raster.name = 'Precipitation',
#                                 raster.units = 'mm',time.unit = 'hour')
#   
#   # Save geoeas files
#   save.geoeas(df = heat.index.bme,
#               directory = 'V:/users/hquintal/phd2_southeast/data/output/02_geoeas/01_geoeas/precipitation/year/',
#               file.name = 'precipitation')
#   
#   # time
#   end.time.all <- Sys.time()
#   time.taken <- end.time.all - start.time.all
#   print(time.taken)
#   
# }
