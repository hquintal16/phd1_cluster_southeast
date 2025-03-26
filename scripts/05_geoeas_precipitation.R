#Setup----
#Updated January 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Reformat precipitation variable into geoeas format for input into MatLab to calculate Space Time Metric
#outputs saved in folder: V:\users\hquintal\phd2_southeast\data\output\02_geoeas\precipitation

# Load Libraries & Functions ----
source("V:/users/hquintal/phd2_southeast/scripts/01_library.R")

# Monthly Aggregation ----
year_month <- yearmo(start.year = 1940,
                     end.year = 2023)

# Load Rasters
for (year in year_month){
  
  # clear
  gc()
  
  # debug
  # year <- 1940
  
  # print statement
  print(paste0('-------------------- Create and Save Precipitation GeoEAS for ',year,' --------------------'))
  
  # time
  start.time.all <- Sys.time()
  print(start.time.all)
  
  # load year
  precipitation <- raster.compile(year = as.character(year),
                               directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/precipitation/")
  
  # compile into geoeas format
  precipitation.bme <- bme.compile(raster.var = precipitation,raster.name = 'Precipitation',
                         raster.units = 'mm',time.unit = 'hour')
  
  # Save geoeas files
  save.geoeas(df = precipitation.bme,
              directory = 'V:/users/hquintal/phd2_southeast/data/output/02_geoeas/01_geoeas/precipitation/month/',
              file.name = 'precipitation')
  
  # time
  end.time.all <- Sys.time()
  time.taken <- end.time.all - start.time.all
  print(time.taken)
  
}

# Annual Aggregation ----

start.year <- 1940
end.year <- 2023

# Load Rasters
for (year in c(start.year:end.year)){
  
  # clear
  gc()
  
  # debug
  # year <- 1940
  
  # print statement
  print(paste0('-------------------- Create and Save Precipitation GeoEAS for ',year,' --------------------'))
  
  # time
  start.time.all <- Sys.time()
  print(start.time.all)
  
  # load year
  precipitation <- raster.compile(year = as.character(year),
                                  directory = "V:/users/hquintal/phd2_southeast/data/output/01_variables/hourly/precipitation/")
  
  # compile into geoeas format
  precipitation.bme <- bme.compile(raster.var = precipitation,raster.name = 'Precipitation',
                                raster.units = 'mm',time.unit = 'hour')
  
  # Save geoeas files
  save.geoeas(df = heat.index.bme,
              directory = 'V:/users/hquintal/phd2_southeast/data/output/02_geoeas/01_geoeas/precipitation/year/',
              file.name = 'precipitation')
  
  # time
  end.time.all <- Sys.time()
  time.taken <- end.time.all - start.time.all
  print(time.taken)
  
}
