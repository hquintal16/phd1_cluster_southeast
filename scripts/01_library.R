#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: all libraries and functions required for PhD 21analysis

# Download older R versions at: https://cran.r-project.org/bin/windows/base/old/
# Download older Rtools packages at: https://cran.r-project.org/bin/windows/Rtools/history.html

##Current date
date <- format(Sys.Date(),'%Y%m%d')

#Read in packages R 4.4.1----

# install CoinCalc package:
# download package version 2.0 from https://github.com/JonatanSiegmund/CoinCalc 
# in RStudio banner, go to Tools -> Install Packages -> change directory location
# CoinCalc package saved to C:\Users\hquintal.AD\OneDrive - University of North Carolina at Chapel Hill\Documents\2022_2026_NC_UNC_PHD\RESEARCH\1_CODING\R\CoinCalc

# install.packages('')
# install_github('')
# remove.packages('')

libs <- c('SpecsVerification','terra','ggplot2','stringr','tigris','sf','raster',
          'dplyr','remote','lubridate','data.table','MetBrewer','tigris','maps',
          'hydroGOF','openxlsx','remote','spatialEco','sp','ggpubr','tidyterra',
          'rasterVis','dplyr','cowplot','devtools','fasterize','ggtext','plotly',
          'grid','scales','viridis','ggthemes','factoextra','weathermetrics',
          'exactextractr','stars','here','DescTools','ncdf4','installr','dbscan',
          'magick','ggpattern','tidyr','anytime','CoinCalc','berryFunctions',
          'biscale','sf','classInt','reshape2','eulerr','gsubfn','digest',
          'gganimate','transformr','magick','gifski','pbapply','purrr','parallel',
          'future.apply','progress','gridExtra','ggpattern','readr','ISOweek',
          'progressr','furrr','future','callr','ggnewscale','doSNOW','foreach',
          'patchwork','nngeo','RANN')

# sapply(libs,install.packages,character.only=T)
sapply(libs, require, character.only = T)

#Write functions----
raster.compile <- function(directory,year){
  
  # create raster
  raster.names <- list.files(path=directory,pattern=year)
  raster.names <- paste0(directory,'/',raster.names)
  raster.var <- terra::rast(raster.names) 
  
  # Change extent to Southeast
  extent.southeast <- terra::ext(-95,-75,24,40) #(west,east,south,north)
  raster.var <- terra::crop(raster.var,extent.southeast)
  
  # return result
  return(raster.var)
  
}

calc.heat.index <- function(ambient.temperature,dewpoint.temperature){
  
  # # debug
  # ambient.temperature <- var.1
  # dewpoint.temperature <- var.3
  
  # https://earthscience.stackexchange.com/questions/16570/how-to-calculate-relative-humidity-from-temperature-dew-point-and-pressure?newreg=86cf2a3bb5bb47c5afd5fcaccfea796a
  
  # Confirm similar raster resolutions, extents
  terra::crs(dewpoint.temperature) == terra::crs(ambient.temperature)
  terra::ext(dewpoint.temperature) == terra::ext(ambient.temperature)
  terra::res(dewpoint.temperature) == terra::res(ambient.temperature)
  
  # convert to df
  temp <- as.data.frame(ambient.temperature,xy=T)
  dewp <- as.data.frame(dewpoint.temperature,xy=T)
  
  # store lat long
  lat.long <- temp[1:nrow(temp),1:2]
  
  # remove lat long
  temp <- temp[1:nrow(temp),-c(1:2)]
  dewp <- dewp[1:nrow(dewp),-c(1:2)]
  
  # transpose for cols = space, rows = time
  temp <- t(temp)
  dewp <- t(dewp)
  
  # set up empty matrix
  rh <- matrix(nrow=nrow(temp),ncol=ncol(temp))
  
  # nested for loop to calculate relative humidity
  for (i in 1:ncol(rh)){ # space
    
    # print(i)
    # get single values
    loc.temp <- temp[,i]
    loc.dewp <- dewp[,i]
    
    # calculate heat index
    rh[,i] <- weathermetrics::heat.index(t = loc.temp,
                                         dp = loc.dewp,
                                         temperature.metric = 'celsius',
                                         output.metric = 'celsius',
                                         round = 2)
    
  }
  
  # convert to raster 
  # fill out dataframe of col1 = long, col2 = lat, col3:length(ncol) = relative humidity
  rh.layer <- as.data.frame(cbind(lat.long,rh[1,]))
  
  for (i in 2:nrow(rh)){
    # print(i)
    rh.layer[,i+2] <- rh[i,]
  }
  
  # create raster layers
  rh.list <- list()
  
  for (i in 3:ncol(rh.layer)){
    j <- i-2
    # print(j)
    raster.layer <- raster::rasterFromXYZ(rh.layer[,c(1,2,i)])
    rh.list[[j]]<- raster.layer
  }
  
  # stack list of rasters
  rh <- raster::stack(rh.list)
  
  # add back crs, res, ext
  rh.crs.res.ext <- raster.crs.ext.res(rh,ambient.temperature)
  
  # convert from raster::stack to terra::rast 
  rh.terra <- as(rh.crs.res.ext, "SpatRaster")
  
  # confirm crs, ext, res
  terra::crs(rh.terra) == terra::crs(ambient.temperature)
  terra::ext(rh.terra) == terra::ext(ambient.temperature)
  terra::res(rh.terra) == terra::res(ambient.temperature)
  
  # add back time
  terra::time(rh.terra) <- terra::time(ambient.temperature)
  
  # return result
  return(rh.terra)
  
}

raster.fill.NA <- function(raster.var){
  
  # fill NA values with focal mean
  raster.var <- terra::focal(raster.var,w=9,fun=mean,na.policy='only',na.rm=T)
  
  # fill remaining NA values with linear interpolation
  raster.var <- terra::approximate(raster.var,rule=2)
  
  # return result
  return(raster.var)
}

raster.crop.mask <- function(raster.target,raster.source){
  
  if (class(raster.target) == 'SpatRaster'){
    # print('Target is Terra')
  } else {
    # convert to terra object
    raster.target <- terra::rast(raster.target)
    # print('Target to Terra')
  }
  
  if (class(raster.source) == 'SpatRaster'){
    # print('Source is Terra')
  } else {
    # convert to terra object
    raster.source <- terra::rast(raster.source)
    # print('Source to Terra')
  }
  
  # crop and mask
  raster.target <- terra::crop(raster.target,raster.source)
  raster.target <- terra::mask(raster.target,raster.source)
  
  # return result
  return(raster.target)
  
}

raster.fill.nearest.neighbor <- function(raster.var){
  
  # # debug
  # raster.var <- var.1.fill
  
  # convert to df
  df <- as.data.frame(raster.var,xy=T)
  
  # store lat long
  lat.long <- df[1:nrow(df),1:2]
  
  # remove lat long
  df <- df[1:nrow(df),-c(1:2)]
  
  # transpose for cols = space, rows = time
  df <- t(df)
  
  # set up empty matrix
  result <- matrix(nrow=nrow(df),ncol=ncol(df))
  
  # replace NA with value of nearest neighbor
  for(i in 1:ncol(df)){
    # print(i)
    result[,i] <- replace.nearest.neighbor(df[,i])
  }
  
  # convert to raster 
  # fill out dataframe of col1 = long, col2 = lat, col3:length(ncol) = relative humidity
  result.df <- as.data.frame(cbind(lat.long,result[1,]))
  
  for (i in 2:nrow(result)){
    result.df[,i+2] <- result[i,]
  }
  
  # create raster layers
  result.list <- list()
  
  for (i in 3:ncol(result.df)){
    j <- i-2
    # print(j)
    raster.layer <- raster::rasterFromXYZ(result.df[,c(1,2,i)])
    result.list[[j]]<- raster.layer
  }
  
  # stack list of rasters
  result <- raster::stack(result.list)
  
  # convert to terra
  result <- terra::rast(result)
  
  # crs 
  terra::crs(result) <- terra::crs(raster.var)
  
  # add back time
  terra::time(result) <- terra::time(raster.var)
  
  # return result
  return(result)
  
}

save.cdf <- function(raster,folder.path,file.name,var.name,long.name,unit){
  
  # save file
  terra::writeCDF(raster, 
                  filename=paste0(folder.path,"/",file.name,'.nc'),
                  varname=var.name,
                  longname=long.name,
                  unit=unit, 
                  overwrite = TRUE)
  
}

bme.compile <- function(raster.var,raster.name,raster.units,time.unit){
  
  # # debug
  # raster.var <- var.1
  # raster.name <- 'Heat_Index'
  # raster.units <- 'C'
  # time.unit <- 'hour'
  
  # print finished statement
  # print('Begin: BMElib Script Writing')
  
  # Convert to useful format for BME analysis
  df <- as.data.frame(raster.var,xy=T) 
  
  # Initialize BME lib file
  df.cols <- c('x','y',paste0(time.unit,'.1'))
  
  # for loop to name columns of BME lib file
  for (i in 4:ncol(df)){
    df.cols <- append(df.cols,paste0(time.unit,'.',i-2))
  }
  colnames(df) <- df.cols
  
  #Create character string to append to beginning of text file to become a GeoEAS text file
  char <- paste0('Measurements of ',raster.name,' (',raster.units,'), space/time grid format\n',
                 ncol(df),
                 '\nx_deg (long)\ny_deg (lat)')
  
  for (i in 3:ncol(df)){
    char <- append(char,paste0(raster.name,'_',time.unit,'_',i-2))
  }
  
  char <- paste(char,collapse='\n')
  
  # Turn df into a string
  df.text <- paste(as.character(df[1,]),collapse = ' ')
  
  for (i in 2:nrow(df)){
    new.line <- paste(as.character(df[i,]),collapse = ' ')
    df.text <- append(df.text,new.line)
  }
  
  df.text <- paste(df.text,collapse = '\n')
  
  # Append char and df
  df <- rbind(char,df.text)
  
  # print finished statement
  # print('Finished: BMElib Script Written')
  
  # Return result
  return(df)
  
}

save.geoeas <- function(df,directory,file.name){
  
  # save file
  writeLines(df,paste0(directory,file.name,'_',year,'.txt'))
  
}

format.figure <- function(raster.var){
  
  # fortify
  spdf <- fortify(as.data.frame(as(raster::raster(raster.var),'SpatialPixelsDataFrame')))
  
  # return result
  return(spdf)
  
}

save.figure.png <- function(figure,figure.number,figure.name,pixels.x,pixels.y,resolution){
  
  # open file
  png(file=paste0("V:/users/hquintal/phd2_southeast/figures/",
                  "Fig_",figure.number,"_",figure.name,'_',date,".png"),
      width = pixels.x,height = pixels.y,res = resolution)
  
  # plot file
  plot(figure)
  
  # close file
  dev.off()
  
}

save.figure.svg <- function(figure,figure.number,figure.name,pixels.x,pixels.y){
  
  ggplot2::ggsave(file=paste0("V:/users/hquintal/phd2_southeast/figures/",
                              "Fig_",figure.number,"_",figure.name,'_',date,".svg"),
                  plot=figure,width=pixels.x,height = pixels.y)
  
}

# Define the function to compute the sum of squared errors (SSE) for two structure (nested) exp cov model
sse_function_exp_exp <- function(params) {
  
  # params
  at1 <- params[1]
  ar1 <- params[2]  
  at2 <- params[3]  
  ar2 <- params[4]
  
  # First model predictions
  space.pred <- sill * (alpha1 * exp(-3 * space$lag / ar1) + 
                           alpha2 * exp(-3 * space$lag / ar2))

  # Second model predictions
  time.pred <- sill * (alpha1 * exp(-3 * time$lag / at1) +
                         alpha2 * exp(-3 * time$lag / at2))
  
  # Sum of squared errors for both models
  sse1 <- sum((space$cov.experimental - space.pred)^2)
  sse2 <- sum((time$cov.experimental - time.pred)^2)
  
  # Total SSE
  sse1 + sse2
}

# Define the function to compute the sum of squared errors (SSE) for single structure exp cov model
sse_function_exp <- function(params) {
  
  # params
  at1 <- params[1]
  ar1 <- params[2]
  
  # First model predictions
  space.pred <- sill * exp(-3 * space$lag / ar1)
  
  # Second model predictions
  time.pred <- sill * exp(-3 * time$lag / at1)
  
  # Sum of squared errors for both models
  sse1 <- sum((space$cov.experimental - space.pred)^2)
  sse2 <- sum((time$cov.experimental - time.pred)^2)
  
  # Total SSE
  sse1 + sse2

}

# Define the function to compute the sum of squared errors (SSE) for single structure gauss cov model
sse_function_gau <- function(params) {
  
  # params
  at1 <- params[1]
  ar1 <- params[2]
  
  # First model predictions
  space.pred <- sill * exp((-3 * (space$lag)^2) / (3 * (ar1)^2))
  
  # Second model predictions
  time.pred <- sill * exp((-3 * (time$lag)^2) / (3 * (at1)^2))
  
  # Add a penalty if any predicted value is non-positive
  # penalty <- sum((space.pred <= 0) * abs(space.pred)) + sum((time.pred <= 0) * abs(time.pred))
  
  # Sum of squared errors for both models
  sse1 <- sum((space$cov.experimental - space.pred)^2)
  sse2 <- sum((time$cov.experimental - time.pred)^2)
  
  # Return Total SSE with large penalty for negative values
  # sse1 + sse2 + 1e6 * penalty 
  sse1 + sse2
  
}

# Define the function to compute the sum of squared errors (SSE) for two structure (nested) gauss cov model
sse_function_gau_exp <- function(params) {
  
  # params
  at1 <- params[1]
  ar1 <- params[2]  
  at2 <- params[3]  
  ar2 <- params[4]
  
  # First model predictions
  space.pred <- sill * (alpha1 * exp((-3 * (space$lag)^2) / (3 * (ar1)^2)) + 
                          alpha2 * exp((-3 * space$lag) / ar2))
  
  # Second model predictions
  time.pred <- sill * (alpha1 * exp((-3 * (time$lag)^2) / (3 * (at1)^2)) +
                         alpha2 * exp((-3 * time$lag) / at2))
  
  # Sum of squared errors for both models
  sse1 <- sum((space$cov.experimental - space.pred)^2)
  sse2 <- sum((time$cov.experimental - time.pred)^2)
  
  # Total SSE
  sse1 + sse2
}


cov.ratio <- function(cov.parameters.df){
  result <- data.frame(ratio.variance.weighted = (cov.parameters.df$alpha[1]*cov.parameters.df$range[1]+
                                                    cov.parameters.df$alpha[2]*cov.parameters.df$range[2]) /
                       (cov.parameters.df$alpha[1]*cov.parameters.df$tau[1]+
                           cov.parameters.df$alpha[2]*cov.parameters.df$tau[2]),
                       variance.weighted.average = cov.parameters.df$alpha[1]*
                         (cov.parameters.df$range[1]/cov.parameters.df$tau[1])+ cov.parameters.df$alpha[2]*
                         (cov.parameters.df$range[2]/cov.parameters.df$tau[2]))
}

# create function that takes a given space time metric and a given gridded time series, and if the:
create.st.cube <- function(target.raster,space.time.metric){

  # # debug
  # target.raster <- heat.index
  # space.time.metric <- stm.heat.index[[1]]

  # create temporary raster
  temp <- terra::rast(crs = terra::crs(target.raster),
                      res = terra::res(target.raster) * space.time.metric,
                      ext = terra::ext(target.raster),
                      nlyr = terra::nlyr(target.raster))
  terra::values(temp) <- 0

  # resample dataset to temp
  result <- terra::resample(target.raster,temp,method = 'max')
  names(result) <- terra::time(target.raster)

  # return result
  return(result)

}

# Function to find the nearest value for a vector
find_nearest <- function(x, target) {
  sapply(x, function(value) {
    if (is.na(value)) return(NA) # Preserve NA values
    return(target[which.min(abs(target - value))])
  })
}

# find index of the center of a square matrix
center.matrix.index <- function(side.length){
  center <- round((side.length+1)/2,0)
  print(center)
  index <- (center - 1) + ((center - 1) * side.length) + 1
  
  return(index)
}

# perform nearest neighbor interpolation
# Function to fill NA using the nearest non-NA value
fill_na <- function(x) {
  if (is.na(x[center.index])) { # The center cell in a  matrix
    return(mean(x, na.rm = TRUE)) # Use the mean of neighbors
  } else {
    return(x[center.index]) # Keep original value if it's not NA
  }
}

threshold.binary <- function(raster.var,raster.threshold){
  
  # debug
  # raster.var <- raster.compile(year = as.character(2019),directory = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/")
  # raster.threshold <- terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_advisory_1_hour_20240915.nc')
  # raster.var <- raster.compile(year = as.character(2019),directory = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/6-hr/")
  # raster.threshold <- criteria.001
  # raster.var <- heat.index.stc
  # raster.threshold <- heat.index.crit
  
  # Crop and mask to original raster
  raster.var <- raster.crop.mask(raster.target = raster.var,raster.source = raster.threshold)
  
  # Convert var to dataframe
  raster.var.df <- as.data.frame(raster.var,xy=T) # col1: long, col2: lat, col3:ncol: values
  raster.var.df <- t(raster.var.df) # row1: long, row2: lat, row3:ncol: values
  
  # Save and remove coordinate information
  lat.long <- raster.var.df[1:2,]
  raster.var.df <- raster.var.df[-c(1:2),]
  
  # Create temporary raster to replace
  raster.var.temp <- raster.var.df # row1:nrow: values, cols = locations
  
  # Convert threshold to dataframe and remove coordinate information
  crit.temp <- as.data.frame(raster.threshold,xy=T) # col1: long, col2: lat, col3:ncol: values
  crit.temp <- t(crit.temp) # row1: long, row2: lat, row3:ncol: values
  crit.temp <- crit.temp[-c(1:2),]
  
  # Create boolean dataframe 
  for (i in 1:ncol(raster.var.df)){ # get space
    # print(i)
    for (j in 1:nrow(raster.var.df)){ # get time
      if (raster.var.df[j,i] >= crit.temp[i]){
        raster.var.temp[j,i] <- 1
      } else {raster.var.temp[j,i] <- 0}
    }
  }
  
  # Combine coordinate and boolean information
  raster.var.temp <- rbind(lat.long,raster.var.temp)
  
  # Transpose boolean information
  raster.var.temp <- t(raster.var.temp)
  
  # Create empty list to fill with each raster time slice
  raster.var.list <- list()
  
  # Create raster time slices and save to list
  for (i in 3:ncol(raster.var.temp)){
    j <- i - 2
    # print(i)
    raster.var.list[[j]] <- raster::rasterFromXYZ(raster.var.temp[,c(1,2,i)],
                                                  crs = terra::crs(raster.threshold))
  }
  
  # Stack raster list items and then convert to spatraster
  raster.var.rast <- raster::stack(raster.var.list)
  raster.var.rast <- terra::rast(raster.var.rast)
  terra::time(raster.var.rast) <- terra::time(raster.var)
  
  # Return result
  return(raster.var.rast)
  
}

yearmo <- function(start.year,end.year){
  
  # Define the start and end year-month
  start_year <- start.year
  start_month <- 1
  end_year <- end.year
  end_month <- 12
  
  # Generate a sequence of years and months
  years <- start_year:end_year
  months <- 1:12
  
  # Create all combinations of years and months
  year_month <- unlist(lapply(years, function(y) {
    sprintf("%04d%02d", y, months)
  }))
  
  # Filter to include only valid year-month combinations
  start_date <- as.integer(sprintf("%04d%02d", start_year, start_month))
  end_date <- as.integer(sprintf("%04d%02d", end_year, end_month))
  year_month <- as.integer(year_month[year_month >= start_date & year_month <= end_date])
  
  return(year_month)
  
}

# Function to replace NA with the closest non-NA value
fill_closest <- function(x) {
  center_value <- x[5]  # Center cell
  if (is.na(center_value)) {
    non_na_values <- x[!is.na(x)]  # Non-NA values in the window
    non_na_distances <- dist_matrix[!is.na(x)]  # Corresponding distances
    if (length(non_na_values) > 0) {
      return(non_na_values[which.min(non_na_distances)])  # Closest value
    } else {
      return(NA)  # No non-NA neighbors
    }
  } else {
    return(center_value)  # Keep the original value if not NA
  }
}

try_catch <- function(exprs) {!inherits(try(eval(exprs)), "try-error")}

# # Function to extract the "YYYY-MM-DD" date from filenames
# extract_date <- function(filename) {
#   date_match <- str_extract(basename(filename), "[0-9]{4}-[0-9]{2}-[0-9]{2}")
#   return(ifelse(!is.na(date_match), date_match, NA))
# }

align_rasters <- function(raster1, raster2) {
  # Ensure both are SpatRaster objects
  if (is.null(raster1) || is.null(raster2)) return(NULL)
  
  # Resample raster2 to match raster1's resolution and extent
  raster2 <- tryCatch(resample(raster2, raster1, method = "near"), error = function(e) NULL)
  
  return(raster2)
}

# Function to check for shared numeric values in any layer
share_numeric_values <- function(raster1, raster2) {
  raster2 <- align_rasters(raster1, raster2)
  
  if (is.null(raster1) || is.null(raster2)) return(FALSE)  # Skip if either is NULL
  
  vals1 <- values(raster1)
  vals2 <- values(raster2)
  
  # Ensure arrays are of equal length
  if (length(vals1) != length(vals2)) return(FALSE)
  
  shared_values <- !is.na(vals1) & !is.na(vals2) & vals1 == vals2
  return(any(shared_values))  # TRUE if any overlap exists
}

# Function to determine which raster has more non-NA values
choose_better_raster <- function(raster1, raster2) {
  vals1 <- values(raster1)
  vals2 <- values(raster2)
  
  non_na_count1 <- sum(!is.na(vals1))
  non_na_count2 <- sum(!is.na(vals2))
  
  return(ifelse(non_na_count1 >= non_na_count2, raster1, raster2))
}

# Function to extract date from filename (assuming "YYYY-MM-DD" format at start)
extract_date <- function(filename) {
  match <- str_extract(basename(filename), "\\d{4}-\\d{2}-\\d{2}")
  if (!is.na(match)) return(match) else return(NULL)
}

# Process NOAA NetCDFs: Resample, crop, and conditionally modify values
process_noaa <- function(file) {
  r <- terra::rast(file)  # Load as SpatRaster
  r <- terra::resample(r, us.states.rast, method = "max")  # Resample to match reference raster
  terra::crs(r) <- terra::crs(us.states.rast)  # Ensure CRS is the same
  r <- terra::crop(r, us.states.rast)  # Match spatial extent
  
  # Apply conditions:
  r <- ifel(r > 1, 1, us.states.rast - 1)  # Convert >1 to 1, 1 in us.states.rast to 0, NA remains NA
  
  # Extract date for the NetCDF
  date_str <- extract_date(file)
  
  # Assign dates to layers if available
  if (!is.null(date_str)) {
    num_layers <- nlyr(r)
    time_seq <- as.Date(date_str) + 0:(num_layers - 1)  # Assuming daily time steps
    terra::time(r) <- time_seq
    names(r) <- time_seq
  }
  
  return(r)
}

# Function to filter out rasters that do not contain any values of 1
filter_noaa_list <- function(raster_list) {
  filtered_list <- list()  # Create empty list to store valid rasters
  index <- 1
  
  for (r in raster_list) {
    has_ones <- any(terra::global(r, fun = "max", na.rm = TRUE) == 1)  # Check if any layer contains a 1
    if (has_ones) {
      filtered_list[[index]] <- r
      index <- index + 1
    }
  }
  
  return(filtered_list)
}

merge_spatrasters <- function(r1, r2) {
  # Ensure both rasters have time attributes
  if (is.null(terra::time(r1)) | is.null(terra::time(r2))) {
    stop("One or both rasters do not have valid time attributes.")
  }
  
  time1 <- as.character(terra::time(r1))
  time2 <- as.character(terra::time(r2))
  
  # Identify common and unique dates
  common_dates <- intersect(time1, time2)
  unique_r1 <- setdiff(time1, time2)
  unique_r2 <- setdiff(time2, time1)
  
  # Extract unique layers
  r1_unique <- if (length(unique_r1) > 0) subset(r1, which(time1 %in% unique_r1)) else NULL
  r2_unique <- if (length(unique_r2) > 0) subset(r2, which(time2 %in% unique_r2)) else NULL
  
  # Process overlapping layers with a progress bar
  merged_layers <- pblapply(common_dates, function(date) {
    layer1_idx <- which(time1 == date)
    layer2_idx <- which(time2 == date)
    
    if (length(layer1_idx) == 0 | length(layer2_idx) == 0) {
      return(NULL)  # Skip if either layer is missing
    }
    
    layer1 <- subset(r1, layer1_idx)
    layer2 <- subset(r2, layer2_idx)
    
    return(layer1 + layer2)  # Add overlapping layers
  })
  
  # Remove NULL values (in case any dates were missing)
  merged_layers <- Filter(Negate(is.null), merged_layers)
  
  # Combine all layers into a single SpatRaster
  final_raster <- do.call(c, c(list(r1_unique, r2_unique), merged_layers))
  
  # Assign correct time attributes
  new_time <- c(unique_r1, unique_r2, common_dates)
  if (!is.null(terra::time(final_raster))) {
    terra::time(final_raster) <- as.Date(new_time)
  }
  
  return(final_raster)
}

# Function to extract the date range of a SpatRaster
get_date_range <- function(r) {
  dates <- time(r)  # Extract dates from SpatRaster
  return(range(dates, na.rm = TRUE))  # Return min and max dates
}

# Function to process a single SpatRaster from cluster_heat_index_list
# # process_cluster <- function(cluster_raster, noaa_list) {
#   
#   # Extract date range from cluster raster
#   cluster_dates <- terra::time(cluster_raster)
#   if (is.null(cluster_dates) || all(is.na(cluster_dates))) {
#     cluster_dates <- as.Date(names(cluster_raster), format = "%Y-%m-%d")
#   }
#   
#   # Initialize results list
#   result_list <- list()
#   
#   for (noaa_raster in noaa_list) {
#     
#     # Extract date range from NOAA raster
#     noaa_dates <- terra::time(noaa_raster)
#     if (is.null(noaa_dates) || all(is.na(noaa_dates))) {
#       noaa_dates <- as.Date(names(noaa_raster), format = "%Y-%m-%d")
#     }
#     
#     # Determine overlapping date range
#     min_date <- min(c(cluster_dates, noaa_dates), na.rm = TRUE)
#     max_date <- max(c(cluster_dates, noaa_dates), na.rm = TRUE)
#     full_date_range <- seq(min_date, max_date, by = "day")
#     
#     # Create an empty template raster if needed
#     template_raster <- cluster_raster[[1]]
#     template_raster <- terra::rast(nlyr = length(full_date_range), ext = terra::ext(cluster_raster),
#                                    crs = terra::crs(cluster_raster), resolution = terra::res(cluster_raster))
#     
#     # Assign the full date range as layer names
#     names(template_raster) <- as.character(full_date_range)
#     
#     # Align cluster raster
#     aligned_cluster <- template_raster
#     matched_cluster_indices <- match(cluster_dates, full_date_range, nomatch = 0)
#     aligned_cluster[[which(matched_cluster_indices > 0)]] <- cluster_raster[[which(matched_cluster_indices > 0)]]
#     
#     # Align NOAA raster
#     aligned_noaa <- template_raster
#     matched_noaa_indices <- match(noaa_dates, full_date_range, nomatch = 0)
#     aligned_noaa[[which(matched_noaa_indices > 0)]] <- noaa_raster[[which(matched_noaa_indices > 0)]]
#     
#     # Assign time metadata
#     terra::time(aligned_cluster) <- full_date_range
#     terra::time(aligned_noaa) <- full_date_range
#     
#     # Sum rasters (values of 2 indicate overlap)
#     summed_raster <- aligned_cluster + aligned_noaa
#     
#     # Store in result list
#     result_list[[length(result_list) + 1]] <- summed_raster
#   }
#   
#   return(result_list)
# }
# process_and_save_clusters <- function(cluster_list, noaa_list, output_dir) {
#   for (i in seq_along(cluster_list)) {
#     cluster_raster <- cluster_list[[i]]
#     cluster_dates <- terra::time(cluster_raster)
#     date_range <- range(cluster_dates, na.rm = TRUE)
#     
#     matching_noaa <- lapply(noaa_list, function(noaa_raster) {
#       noaa_dates <- terra::time(noaa_raster)
#       if (any(noaa_dates >= (date_range[1] - 3) & noaa_dates <= (date_range[2] + 3))) {
#         return(noaa_raster)
#       } else {
#         return(NULL)
#       }
#     })
#     matching_noaa <- Filter(Negate(is.null), matching_noaa)
#     
#     if (length(matching_noaa) > 0) {
#       combined_raster <- Reduce(
#         function(x, y) {
#           common_dates <- intersect(terra::time(x), terra::time(y))
#           if (length(common_dates) > 0) {
#             x_common <- x[[which(terra::time(x) %in% common_dates)]]
#             y_common <- y[[which(terra::time(y) %in% common_dates)]]
#             summed <- x_common + y_common
#             terra::time(summed) <- common_dates
#             return(summed)
#           } else {
#             return(NULL)
#           }
#         },
#         matching_noaa,
#         init = cluster_raster
#       )
#       
#       if (!is.null(combined_raster)) {
#         first_date <- format(min(terra::time(cluster_raster), na.rm = TRUE), "%Y-%m-%d")
#         file_name <- sprintf("%s_validated_cluster_%04d.nc", first_date, i)
#         file_path <- file.path(output_dir, file_name)
#         terra::writeRaster(combined_raster, file_path, overwrite = TRUE)
#       }
#     }
#   }
# }
# 
# process_single_cluster <- function(cluster_raster, cluster_index, noaa_list, output_dir) {
#   cluster_dates <- terra::time(cluster_raster)
#   date_range <- range(cluster_dates, na.rm = TRUE)
#   
#   matching_noaa <- lapply(noaa_list, function(noaa_raster) {
#     noaa_dates <- terra::time(noaa_raster)
#     if (any(noaa_dates >= (date_range[1] - 3) & noaa_dates <= (date_range[2] + 3))) {
#       return(noaa_raster)
#     } else {
#       return(NULL)
#     }
#   })
#   matching_noaa <- Filter(Negate(is.null), matching_noaa)
#   
#   if (length(matching_noaa) > 0) {
#     combined_raster <- Reduce(
#       function(x, y) {
#         common_dates <- intersect(terra::time(x), terra::time(y))
#         if (length(common_dates) > 0) {
#           x_common <- x[[which(terra::time(x) %in% common_dates)]]
#           y_common <- y[[which(terra::time(y) %in% common_dates)]]
#           summed <- x_common + y_common
#           terra::time(summed) <- common_dates
#           return(summed)
#         } else {
#           return(NULL)
#         }
#       },
#       matching_noaa,
#       init = cluster_raster
#     )
#     
#     if (!is.null(combined_raster)) {
#       first_date <- format(min(terra::time(cluster_raster), na.rm = TRUE), "%Y-%m-%d")
#       file_name <- sprintf("%s_validated_cluster_%04d.nc", first_date, cluster_index)
#       file_path <- file.path(output_dir, file_name)
#       terra::writeRaster(combined_raster, file_path, overwrite = TRUE)
#     }
#   }
# }

# process_single_cluster <- function(cluster_raster, cluster_index, noaa_list, output_dir) {
#   cluster_dates <- terra::time(cluster_raster)
#   if (is.null(cluster_dates)) return(NULL)  # Skip if no dates are available
#   
#   date_range <- range(cluster_dates, na.rm = TRUE)
#   
#   # Find matching NOAA rasters within ±3 days
#   matching_noaa <- lapply(noaa_list, function(noaa_raster) {
#     noaa_dates <- terra::time(noaa_raster)
#     if (!is.null(noaa_dates) && any(noaa_dates >= (date_range[1] - 3) & noaa_dates <= (date_range[2] + 3))) {
#       return(noaa_raster)
#     } else {
#       return(NULL)
#     }
#   })
#   matching_noaa <- Filter(Negate(is.null), matching_noaa)
#   
#   # Skip if no matching NOAA rasters found
#   if (length(matching_noaa) == 0) return(NULL)
#   
#   # Initialize the combined raster with the cluster raster
#   combined_raster <- cluster_raster
#   
#   # Add overlapping NOAA rasters
#   for (noaa_raster in matching_noaa) {
#     common_dates <- intersect(terra::time(combined_raster), terra::time(noaa_raster))
#     if (length(common_dates) > 0) {
#       x_common <- combined_raster[[which(terra::time(combined_raster) %in% common_dates)]]
#       y_common <- noaa_raster[[which(terra::time(noaa_raster) %in% common_dates)]]
#       summed <- x_common + y_common
#       terra::time(summed) <- common_dates
#       combined_raster <- summed  # Update the combined raster
#     }
#   }
#   
#   # Skip saving if no valid combined raster exists
#   if (is.null(combined_raster) || terra::nlyr(combined_raster) == 0) return(NULL)
#   
#   # Generate file name based on the first date
#   first_date <- format(min(cluster_dates, na.rm = TRUE), "%Y-%m-%d")
#   file_name <- sprintf("%s_validated_cluster_%04d.nc", first_date, cluster_index)
#   file_path <- file.path(output_dir, file_name)
#   
#   # Save the raster
#   terra::writeRaster(combined_raster, file_path, overwrite = TRUE)
# }
# 

# Old ----

correct.criteria <- function(raster.target,neighbor.range) {
 
  # run through each raster position
  for (i in 1:terra::nrow(us.states.rast)){
    
    for (j in 1:terra::ncol(us.states.rast)){
      
      # debug
      # print(paste0('Raster position: [',i,',',j,']'))
      # i <- 50
      # j <- 10
      # raster.target <- heat_index
      # neighbor.range <- 2
      
      # if there is a value in both rasters, do nothing
      if (!is.na(us.states.rast[i,j])[1] && !is.na(raster.target[i,j])[1]) {
        
        # if there is an NA in both rasters, do nothing
      } else if (is.na(us.states.rast[i,j])[1] && is.na(raster.target[i,j])[1]) {
        
        # if there is a value in us.states.rast but not in raster.target, add a value that is the nearest neighbor
      } else if (!is.na(us.states.rast[i,j])[1] && is.na(raster.target[i,j])[1]) {
        
        nearest.neighbor <- unlist(c(raster.target[i-1,j+1],
                                     raster.target[i,j+1],
                                     raster.target[i+1,j+1],
                                     raster.target[i-1,j],
                                     raster.target[i+1,j],
                                     raster.target[i-1,j-1],
                                     raster.target[i,j-1],
                                     raster.target[i+1,j-1]))
        
        # give nearest neighbor value to raster.target raster
        raster.target[i,j] <- max(nearest.neighbor)
        
        # if there is a value in raster.target but not in us.states.rast, change that value to NA
      } else {
        
        raster.target[i,j] <- NA
        
      }
      
    }
    
  }
  
  # fill remaining gaps
  for (i in 1:terra::nrow(raster.target)){
    for (j in 1:terra::ncol(raster.target)){
      if (is.na(raster.target[i,j])){
      } else if (raster.target[i,j] == 1){
        
        print(paste0('Fix Location ',i,':',j))
        
        # get row col indices for radius around current cell
        temp.df <- data.frame(
          row.range = seq(i-neighbor.range,i+neighbor.range,1),
          col.range = seq(j-neighbor.range,j+neighbor.range,1)
        )
        
        # get all possible combinations
        temp.df <- expand.grid(temp.df)
        
        # create empty list to fill with all neighbors
        temp.list <- list()
        
        # fill list with all neighbors
        for (k in 1:nrow(temp.df)){
          indexing <- temp.df[k,]
          temp.row <- indexing[1]
          temp.col <- indexing[2]
          temp.list[[k]] <- raster.target[temp.row,temp.col]
        }
        
        # calc most often occuring nearest neighbor
        nearest.neighbor <- DescTools::Mode(na.omit(as.vector(unlist(temp.list))))
        
        # replace 1 value with nearest neighbor
        raster.target[i,j] <- nearest.neighbor[1]
        
      } else {}
    }
  }
  
  # return result
  return(raster.target)
  
}

buffer.criteria <- function(raster.target,neighbor.range){
  
  # debug
  # raster.target <- heat_index
  # neighbor.range <- 2
  
  # fill remaining gaps
  for (i in 1:terra::nrow(raster.target)){
    for (j in 1:terra::ncol(raster.target)){
      if (is.na(raster.target[i,j])){
      } else if (raster.target[i,j] == 1){
        
        print(paste0('Fix Location ',i,':',j))
        
        # get row col indices for radius around current cell
        temp.df <- data.frame(
          row.range = seq(i-neighbor.range,i+neighbor.range,1),
          col.range = seq(j-neighbor.range,j+neighbor.range,1)
        )
        
        # get all possible combinations
        temp.df <- expand.grid(temp.df)
        
        # create empty list to fill with all neighbors
        temp.list <- list()
        
        # fill list with all neighbors
        for (k in 1:nrow(temp.df)){
          indexing <- temp.df[k,]
          temp.row <- indexing[1]
          temp.col <- indexing[2]
          temp.list[[k]] <- raster.target[temp.row,temp.col]
        }
        
        # Unlist result, remove NA values
        temp.list <- na.omit(as.vector(unlist(temp.list)))
        
        # Remove 1's
        temp.list <- temp.list[!temp.list == 1]
        
        # calc most often occuring nearest neighbor
        nearest.neighbor <- DescTools::Mode(temp.list)
        
        # replace 1 value with nearest neighbor
        raster.target[i,j] <- nearest.neighbor[1]
        
      } else {}
    }
  }
  
  # return result
  return(raster.target)
  
}

buffer.criteria.precip <- function(raster.target,neighbor.range){
  
  # debug
  # raster.target <- heat_index
  # neighbor.range <- 2
  
  # fill remaining gaps
  for (i in 1:terra::nrow(raster.target)){
    for (j in 1:terra::ncol(raster.target)){
      if (is.na(raster.target[i,j])){
      } else if (raster.target[i,j] == 1){
        
        print(paste0('Fix Location ',i,':',j))
        
        # get row col indices for radius around current cell
        temp.df <- data.frame(
          row.range = seq(i-neighbor.range,i+neighbor.range,1),
          col.range = seq(j-neighbor.range,j+neighbor.range,1)
        )
        
        # get all possible combinations
        temp.df <- expand.grid(temp.df)
        
        # create empty list to fill with all neighbors
        temp.list <- list()
        
        # fill list with all neighbors
        for (k in 1:nrow(temp.df)){
          indexing <- temp.df[k,]
          temp.row <- indexing[1]
          temp.col <- indexing[2]
          temp.list[[k]] <- raster.target[temp.row,temp.col]
        }
        
        # Unlist result, remove NA values
        temp.list <- na.omit(as.vector(unlist(temp.list)))
        
        # Remove 1's
        temp.list <- temp.list[!temp.list == 1]
        
        # calc most often occuring nearest neighbor
        nearest.neighbor <- DescTools::Mean(temp.list)
        
        # replace 1 value with nearest neighbor
        raster.target[i,j] <- nearest.neighbor[1]
        
      } else {}
    }
  }
  
  # return result
  return(raster.target)
  
}

threshold.compile <- function(ne.dir,orb.dir,se.dir,sw.dir,tx.dir,mw.dir){
 
  # # debug
  # ne <- terra::rast('V:/users/hquintal/NC_Quintal_Manuscript_Data_Scripts/data/out/shapefiles/ne50yr60ma/ne50yr60ma.asc')
  # orb <- terra::rast('V:/users/hquintal/NC_Quintal_Manuscript_Data_Scripts/data/out/shapefiles/orb50yr60ma/orb50yr60ma.asc')
  # se <- terra::rast('V:/users/hquintal/NC_Quintal_Manuscript_Data_Scripts/data/out/shapefiles/se50yr60ma/se50yr60ma.asc')
  # sw <- terra::rast('V:/users/hquintal/NC_Quintal_Manuscript_Data_Scripts/data/out/shapefiles/sw50yr60ma/sw50yr60ma.asc')
  # tx <- terra::rast('V:/users/hquintal/NC_Quintal_Manuscript_Data_Scripts/data/out/shapefiles/tx50yr60ma/tx50yr60ma.asc')
  # mw <- terra::rast('V:/users/hquintal/NC_Quintal_Manuscript_Data_Scripts/data/out/shapefiles/mw50yr60ma/mw50yr60ma.asc')
  
  # Read in NOAA Atlas 14 intensities (in inches) from https://hdsc.nws.noaa.gov/pfds/index.html
  ne <- terra::rast(ne.dir)
  orb <- terra::rast(orb.dir)
  se <- terra::rast(se.dir)
  sw <- terra::rast(sw.dir)
  tx <- terra::rast(tx.dir)
  mw <- terra::rast(mw.dir)
  
  # resample
  ne <- terra::resample(ne,united_states)
  orb <- terra::resample(orb,united_states)
  se <- terra::resample(se,united_states)
  sw <- terra::resample(sw,united_states)
  tx <- terra::resample(tx,united_states)
  mw <- terra::resample(mw,united_states)

  # mosaic
  atlas.14 <- terra::mosaic(ne,orb)
  atlas.14 <- terra::mosaic(atlas.14,se)
  atlas.14 <- terra::mosaic(atlas.14,sw)
  atlas.14 <- terra::mosaic(atlas.14,tx)
  atlas.14 <- terra::mosaic(atlas.14,mw)
  
  # convert to mm
  atlas.14 <- atlas.14 / 1000 * 25.4
 
  # return result
  return(atlas.14)
  
}

raster.clip <- function(raster.file,shapefile){
 
  # confirm same crs
  shp <- sf::st_transform(shapefile,terra::crs(raster.file))
  
  # Crop and mask to land mass
  raster.file <- terra::crop(raster.file,shp)
  raster.file <- terra::mask(raster.file,shp)

  # return result
  return(raster.file)
  
}





replace.nearest.neighbor <- function(dat) {
  # https://stackoverflow.com/questions/10077415/replacing-nas-in-r-with-nearest-value
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}





raster.crs.ext.res <- function(raster.target,raster.source){
  
  # convert to either raster::raster or raster::stack, set crs, ext, res, 
  # then convert back to terra::rast
  
  # # debug
  # raster.target <- var.3
  # raster.source <- var.1
  
  # raster target
  if (class(raster.target) == 'RasterLayer' ||
      class(raster.target) == 'RasterStack' ||
      class(raster.target) == 'RasterBrick' ){
    # print('Target is Raster')
  } else {
    # convert to raster object
    if (terra::nlyr(raster.target) > 1){
      raster.target <- raster::stack(raster.target)
      # print('Target to Raster')
    } else {
      raster.target <- raster::raster(raster.target)
      # print('Target to Raster')
    }
    
  }
  
  # raster source
  if (class(raster.source) == 'RasterLayer' ||
      class(raster.source) == 'RasterStack' ||
      class(raster.source) == 'RasterBrick' ){
    raster.source <- terra::rast(raster.source)
    # print('Source to Terra')
  } else {
    # already terra object
    # print('Source is Terra')
  } 
  
  # add back crs, res, ext
  raster::crs(raster.target) <- terra::crs(raster.source)
  raster::res(raster.target) <- terra::res(raster.source)
  bb <- terra::ext(raster.source)[1:4]
  raster.target <- raster::setExtent(raster.target,bb)
  
  # convert to terra::rast
  raster.target <- terra::rast(raster.target)

  # return result
  return(raster.target)
  
}



threshold.boolean <- function(raster.var,raster.threshold){
  
  # # debug
  # raster.var <- raster.compile(year = as.character(2023),directory = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/") 
  # raster.threshold <- terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_advisory_1_hour_20240915.nc')
  # plot(raster.var[[1]])
  # plot(raster.threshold)

  # update extents
  terra::ext(raster.var) <- terra::ext(raster.threshold)
  
  # create new raster
  raster.bool <- list()
  # 
  # # raster.threshold as terra object
  # raster.threshold <- terra::rast(raster.threshold)
  
  # conditional indexing
  # https://stackoverflow.com/questions/49674917/reclassify-a-raster-based-on-2-rasters
  for (i in 1:terra::nlyr(raster.var)){
    # i <- 1
    # print(i)
    
    # go by layer
    raster.bool.layer <- raster.threshold
    raster.var.layer <- raster.var[[i]]
    
    # set values to 0
    terra::values(raster.bool.layer) <- NA
    
    # create bool
    raster.bool.layer[raster.var.layer >= raster.threshold] <- 1
    raster.bool.layer[raster.var.layer < raster.threshold] <- 0
    
    # if NA value, give it a 0
    raster.bool.layer[is.na(raster.var.layer)] <- 0
    
    # save to list
    raster.bool[[i]] <- raster.bool.layer
  }
  
  # print('conditional indexing')
  
  # stack list of rasters
  bool <- terra::rast(raster.bool)
  # print('stack rasters')
  
  # add back time
  terra::time(bool) <- terra::time(raster.var)
  
  # crop and mask to threshold
  bool <- raster.crop.mask(bool,raster.threshold)

  # return result
  return(bool)
  
}

# threshold.binary <- function(raster.var,raster.threshold){
#   
#   # debug
#   # raster.var <- raster.compile(year = as.character(2019),directory = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/")
#   # raster.threshold <- terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_advisory_1_hour_20240915.nc')
#   # raster.var <- raster.compile(year = as.character(2019),directory = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/6-hr/")
#   # raster.threshold <- criteria.001
# 
#   # Crop and mask to original raster
#   raster.var <- raster.crop.mask(raster.target = raster.var,raster.source = raster.threshold)
#   
#   # Convert var to dataframe
#   raster.var.df <- as.data.frame(raster.var,xy=T) # col1: long, col2: lat, col3:ncol: values
#   raster.var.df <- t(raster.var.df) # row1: long, row2: lat, row3:ncol: values
#   
#   # Save and remove coordinate information
#   lat.long <- raster.var.df[1:2,]
#   raster.var.df <- raster.var.df[-c(1:2),]
#   
#   # Create temporary raster to replace
#   raster.var.temp <- raster.var.df # row1:nrow: values, cols = locations
#   
#   # Convert threshold to dataframe and remove coordinate information
#   crit.temp <- as.data.frame(raster.threshold,xy=T) # col1: long, col2: lat, col3:ncol: values
#   crit.temp <- t(crit.temp) # row1: long, row2: lat, row3:ncol: values
#   crit.temp <- crit.temp[-c(1:2),]
#   
#   # Create boolean dataframe 
#   for (i in 1:ncol(raster.var.df)){ # get space
#     # print(i)
#     for (j in 1:nrow(raster.var.df)){ # get time
#       if (raster.var.df[j,i] >= crit.temp[i]){
#         raster.var.temp[j,i] <- 1
#       } else {raster.var.temp[j,i] <- 0}
#     }
#   }
#   
#   # Combine coordinate and boolean information
#   raster.var.temp <- rbind(lat.long,raster.var.temp)
#   
#   # Transpose boolean information
#   raster.var.temp <- t(raster.var.temp)
#   
#   # Create empty list to fill with each raster time slice
#   raster.var.list <- list()
#   
#   # Create raster time slices and save to list
#   for (i in 3:ncol(raster.var.temp)){
#     j <- i - 2
#     # print(i)
#     raster.var.list[[j]] <- raster::rasterFromXYZ(raster.var.temp[,c(1,2,i)],
#                                                   crs = terra::crs(raster.threshold))
#   }
# 
#   # Stack raster list items and then convert to spatraster
#   raster.var.rast <- raster::stack(raster.var.list)
#   raster.var.rast <- terra::rast(raster.var.rast)
#   terra::time(raster.var.rast) <- terra::time(raster.var)
# 
#   # Return result
#   return(raster.var.rast)
#   
# }

# cov.ratio <- function(cov.parameters.df){
#   result <- data.frame(ratio.variance.weighted = (cov.parameters.df$alpha[1]*cov.parameters.df$tau[1]+
#                                                     cov.parameters.df$alpha[2]*cov.parameters.df$tau[2])/
#                          (cov.parameters.df$alpha[1]*cov.parameters.df$range[1]+
#                             cov.parameters.df$alpha[2]*cov.parameters.df$range[2]),
#                        variance.weighted.average = cov.parameters.df$alpha[1]*
#                          (cov.parameters.df$tau[1]/cov.parameters.df$range[1])+ cov.parameters.df$alpha[2]*
#                          (cov.parameters.df$tau[2]/cov.parameters.df$range[2]))
# }

cluster.lat.long.date <- function(raster.bool){
 
  # debug
  raster.bool <- var.bool
  
  # convert rasters into data.frames
  df <- as.data.frame(raster.bool,xy=T) 
  
  # store lat long information and transpose data.frames so that rows are time and cols are space
  lat.long <- df[1:nrow(df),1:2]
  df <- df[1:nrow(df),-c(1:2)]
  date.time <- terra::time(raster.bool)
    
  # combine data into space time cube representing observations
  long <- rep(lat.long$x,dim(df)[2])
  lat <- rep(lat.long$y,dim(df)[2])
  date <- rep(seq(1,length(date.time),1),each=dim(df)[1])
  extremes <- unlist(df,use.names = F)
  
  # combine
  st.cube <- data.frame(long = long, lat = lat, date = date, extremes = extremes)
  
  # remove non-extremes
  st.cube <- st.cube[st.cube$extremes %in% 1,]
  
  # remove extreme values
  st.cube <- st.cube[,-c(4)]
  
  # return result
  return(st.cube)
  
}

cluster.lat.long.date.2 <- function(raster.bool){
  
  # debug
  # raster.bool <- var.bool
  
  # convert rasters into data.frames
  df <- as.data.frame(raster.bool,xy=T)

  # store lat long information and transpose data.frames so that rows are time and cols are space
  lat.long <- df[1:nrow(df),1:2]
  # head(lat.long)
  df <- df[1:nrow(df),-c(1:2)]
  date.time <- terra::time(raster.bool)

  # combine data into space time cube representing observations
  long <- rep(lat.long$x,dim(df)[2])
  lat <- rep(lat.long$y,dim(df)[2])
  date <- rep(seq(1,length(date.time),1),each=dim(df)[1])
  extremes <- unlist(df,use.names = F)
  
  # join lat and lat position and also join long and long position
  lat.df <- data.frame(lat = unique(lat),lat.pos = seq(1,length(unique(lat)),1))
  lat.pos <- data.frame(lat = lat)
  lat.pos <- dplyr::left_join(lat.pos,lat.df,by='lat')

  # join long and long position
  long.df <- data.frame(long = unique(long),long.pos = seq(1,length(unique(long)),1))
  long.pos <- data.frame(long = long)
  long.pos <- dplyr::left_join(long.pos,long.df,by='long')
  
  # join date and date.time
  date.df <- data.frame(date = as.numeric(date.time),date.pos = seq(1,length(unique(date.time)),1))
  date.pos <- data.frame(date.pos = date)
  date.pos <- dplyr::left_join(date.pos,date.df,by='date.pos')
  date.pos$date <- lubridate::as_date(date.pos$date)

  # combine
  st.cube <- data.frame(long.pos, lat.pos, date.pos, extremes = extremes)
  # remove non-extremes
  st.cube <- st.cube[st.cube$extremes %in% 1,]
  
  # remove extreme values
  st.cube <- st.cube[,!colnames(st.cube)=='extremes']
  
  # return result
  return(st.cube)
  
}

cluster.st.cube <- function(cluster.3d.grid,cluster.st.metric){
 
  # debug
  cluster.3d.grid <- var.cube
  cluster.st.metric <- var.2.st.metric[1]
    
  # Round metric to nearest whole number
  cluster.st.metric <- round(cluster.st.metric,0)
  
  # subset 3d.grid by st.metric
  date <- seq(1,max(cluster.3d.grid$date),cluster.st.metric$ratio.variance.weighted)
  date.old <- seq(1,max(cluster.3d.grid$date),cluster.st.metric$ratio.variance.weighted)
  date.new <- seq(1,max(cluster.3d.grid$date)/cluster.st.metric$ratio.variance.weighted,1)
  
  if(length(date.new) < length(date.old)){
    # print('Change Date')
    date.new <- c(date.new,max(date.new)+cluster.st.metric$ratio.variance.weighted)
  }
  
  # combine timestamps
  cube.subset <- data.frame(date,date.new,date.old)
  
  #  join common timestamps between cluster.3d.grid and cube.subset
  result <- merge(x=cluster.3d.grid,y=cube.subset,by='date') 
  # date.old is the actual hour of the year
  # date.new is the reformatted hour of the year for clustering
  
  # return result
  return(result)
  
}

cluster.st.cube.2 <- function(cluster.3d.grid,cluster.st.metric,row.start){
  
  # # debug
  # cluster.3d.grid <- var.cube
  # cluster.st.metric <- var.st.metric[1]
  # row.start <- 1
  
  # subset 3d.grid by st.metric
  date <- seq(row.start,max(cluster.3d.grid$date.pos),cluster.st.metric$ratio.variance.weighted)
  date.original <- seq(row.start,max(cluster.3d.grid$date.pos),cluster.st.metric$ratio.variance.weighted)
  date.compress <- seq(1,max(cluster.3d.grid$date.pos)/cluster.st.metric$ratio.variance.weighted,1)
  
  if(length(date.compress) < length(date.original)){
    
    date.compress <- c(date.compress,max(date.compress)+cluster.st.metric$ratio.variance.weighted)
  
  }
  
  # combine timestamps
  cube.subset <- data.frame(date,date.compress,date.original)
  
  #  join common timestamps between cluster.3d.grid and cube.subset
  result <- merge(x=cluster.3d.grid,y=cube.subset,by.x='date.pos',by.y='date.original') 
  
  # update column names
  colnames(result) <- c('date.pos','long','long.pos','lat','lat.pos','date','date.original','date.compress')
  # date and date.pos and date.original is the actual day of the year
  # date.compress is the reformatted day of the year for clustering
  
  # return result
  return(result)
  
}

cluster.kNN <- function(raster.bool.df,min.extremes){ 
  # 2024-08-30 added min.extremes, which is an input to cluster.workflow function
 
  # try catch 
  tryCatch({
    
    # kNN assessment
    st.cube <- raster.bool.df
    
    # define k 
    epsilon <- list()
    
    
    # for (k in seq(1,100,1)){ # 2024-08-30 changed 100 to min.extremes, which is an input to cluster.workflow function
    for (k in seq(1,min.extremes,1)){
      # print(paste0('k = ',k))
      # k = 2
      # dbscan process
      points.by.dist = 1:length(dbscan::kNNdist(st.cube, k = k))
      nn.dist = sort(dbscan::kNNdist(st.cube, k =  k))
      df <- data.frame(points.by.dist = 1:length(dbscan::kNNdist(st.cube, k = k)),
                       nn.dist = sort(dbscan::kNNdist(st.cube, k = k)))
      
      # find the elbow in the kNN graph
      elbow.point <- pathviewr::find_curve_elbow(df,plot_curve = T)
      
      # # example plots for advisors
      # ##Access directory to save figs
      # save_folder_path <- 'V:/users/hquintal/NC_Quintal_Manuscript_Data_Scripts/PhD2_Cluster_Parameters/figures/Fig_6/'
      # png(file=paste0(save_folder_path,"/",date.now,"_Fig_06_UTCI_kNN_",k,"_",year,".png"),
      #     width = 6000,height = 4200,res = 600)
      # plot(df,main=c('Threshold = 99.9%','Year = 1940',paste0('k = ',k)))
      # abline(v=elbow.point)
      # abline(h=df[elbow.point,2])
      # dev.off()
      # # end example
      
      # nn distance
      nn.distance <- df[elbow.point,]
      
      # return kNN distance
      # print(paste0('k-NN Distance = ',round(nn.distance[2],2)))
      epsilon[k] <- round(nn.distance[2],2)
    }
    
    # df <- data.frame(mu = seq(1,100,1),epsilon = unlist(epsilon)) # 2024-08-30 changed 100 to min.extremes, which is an input to cluster.workflow function
    df <- data.frame(mu = seq(1,min.extremes,1),epsilon = unlist(epsilon))
    df <- na.omit(df)
    
    # identify whether by increasing the value k, we begin to include points that would otherwise be included in other clusters
    elbow.point <- pathviewr::find_curve_elbow(df,plot_curve = T)
    
    # # example plots for advisors
    # png(file=paste0(save_folder_path,"/",date.now,"_Fig_06_UTCI_mu_epsilon_",year,".png"),
    #     width = 6000,height = 4200,res = 600)
    # plot(df,main=c('Threshold = 99.9%','Year = 1940'),ylab='Epsilon (Search radius)',
    #      xlab='Mu (Minimum # of points needed to form a cluster)')
    # abline(v=elbow.point)
    # abline(h=df[elbow.point,2])
    # dev.off()
    
    # shorten df
    df <- df[1:which(df$mu==elbow.point),]
    
    # return result
    return(df)
    
  })
  
}

# cluster.plausible <- function(dbscan.params.df,difference.value,difference.operator){
 
#   # #debug
#   # dbscan.params.df <- var.kNN
#   # difference.value <- 10
#   # difference.operator <- 'add'
#   
#   # try catch 
#   tryCatch({
#     
#     # read in values
#     df <- dbscan.params.df
#     
#     # initiate
#     params <- data.frame(iter = df[1,1],
#                          plausible = 1)
#     
#     # start comparing
#     for (i in 2:nrow(df)){
#       
#       # addition option
#       if (difference.operator == 'add') {
#         j <- i-1
#         if (df[i,2] > (df[j,2]+difference.value)) {
#           result <- data.frame(iter = df[i,1],
#                                plausible = 0)
#         } else {
#           result <- data.frame(iter = df[i,1],
#                                plausible = 1)
#           
#         } 
#         # print(result)
#       }
#       
#       # multiplication option
#       if (difference.operator == 'multiply') {
#         j <- i-1
#         if (df[i,2] > (df[i-1,2]*difference.value)) {
#           result <- data.frame(iter = df[i,1],
#                                plausible = 0)
#         } else {
#           result <- data.frame(iter = df[i,1],
#                                plausible = 1)
#           
#         } 
#         # print(result)
#       }
#       
#       # append
#       params <- rbind(params,result)
#       
#     }
#     
#     # subset to new plausible range
#     params.range <- params[match(unique(params$plausible),params$plausible),]
#     
#     if (length(params.range$iter) > 1){
#       params.max <- params.range[2,1]-1
#       params <- df[1:params.max,]
#     } else {
#       params <- df[1,]
#     }
#     
#     # return result
#     return(params)
#     
#   })
#   
# }

cluster.kNN.2 <- function(var.st.cube,neighbors){
  
  # debug
  # var.st.cube <- temp
  
  # where k = # of neighbors to identify, which must be 
  # at least 4 in a 3D array
  
  # sort points by distance
  points.sorted.by.distance = 1:length(dbscan::kNNdist(var.st.cube, k = neighbors))
  nearest.neighbor.distance = sort(dbscan::kNNdist(var.st.cube, k = neighbors))
  
  # store as dataframe
  df <- data.frame(points.sorted.by.distance,nearest.neighbor.distance)
  
  # find the elbow in the kNN graph
  elbow.point <- pathviewr::find_curve_elbow(df,plot_curve = T)
  
  # save epsilon distance
  epsilon <- df[elbow.point,2]
  
  # store mu minpts
  mu <- 4
  
  # save as dataframe
  dbscan.parameters <- data.frame(mu,epsilon)
  
  # return result
  return(dbscan.parameters)
  
}

cluster.kNN.3 <- function(var.st.cube,mu){
  
  # debug
  # var.st.cube <- temp
  
  # sort points by distance
  points.sorted.by.distance = 1:length(dbscan::kNNdist(var.st.cube, k = 4))
  nearest.neighbor.distance = sort(dbscan::kNNdist(var.st.cube, k = 4))
  
  # store as dataframe
  df <- data.frame(points.sorted.by.distance,nearest.neighbor.distance)
  
  # find the elbow in the kNN graph
  elbow.point <- pathviewr::find_curve_elbow(df,plot_curve = T)
  
  # save epsilon distance
  epsilon <- df[elbow.point,2]
  
  # store mu minpts
  mu <- mu
  
  # save as dataframe
  dbscan.parameters <- data.frame(mu,epsilon)
  
  # return result
  return(dbscan.parameters)
  
}

cluster.kNN.4 <- function(var.st.cube,mu,neighbor){
  
  # debug
  # var.st.cube <- temp
  
  # sort points by distance
  points.sorted.by.distance = 1:length(dbscan::kNNdist(var.st.cube, k = neighbor))
  nearest.neighbor.distance = sort(dbscan::kNNdist(var.st.cube, k = neighbor))
  
  # store as dataframe
  df <- data.frame(points.sorted.by.distance,nearest.neighbor.distance)
  
  # find the elbow in the kNN graph
  elbow.point <- pathviewr::find_curve_elbow(df,plot_curve = T)
  
  # save epsilon distance
  epsilon <- df[elbow.point,2]
  
  # store mu minpts
  mu <- mu
  
  # save as dataframe
  dbscan.parameters <- data.frame(mu,epsilon)
  
  # return result
  return(dbscan.parameters)
  
}

cluster.plausible <- function(dbscan.params.df,difference.value,difference.operator){

  # #debug
  # dbscan.params.df <- var.kNN
  # difference.value <- 10
  # difference.operator <- 'add'
  
  # read in values
  df <- dbscan.params.df
  
  # initiate
  params <- data.frame(iter = df[1,1],
                       plausible = 1)
  
  # start comparing
  for (i in 2:nrow(df)){
    # i <- 2
    # addition option
    if (difference.operator == 'add') {
      j <- i-1
      if (df[i,2] > (df[j,2]+difference.value)) {
        result <- data.frame(iter = df[i,1],
                             plausible = 0)
      } else {
        result <- data.frame(iter = df[i,1],
                             plausible = 1)
        
      } 
      # print(result)
    } else if (difference.operator == 'multiply') {
      j <- i-1
      if (df[i,2] > (df[i-1,2]*difference.value)) {
        result <- data.frame(iter = df[i,1],
                             plausible = 0)
      } else {
        result <- data.frame(iter = df[i,1],
                             plausible = 1)
        
      } 
      # print(result)
    }
    
    # append
    params <- rbind(params,result)
    
  }
  
  # test whether all parameters are plausible, then subset to new plausible range
  if (length(which(params$plausible != 1)) == 0){
    params.range <- params
  } else {params.range <- params[match(unique(params$plausible),params$plausible),]}

  # get range of mu and epsilon
  if (length(params.range$iter) > 1){
    # params.max <- params.range[2,1]-1
    
    # get max mu
    params.max <- max(params.range[,1])
    
    # get position of max mu
    index.position <- which(df$mu == params.max)
    
    # get range of plausible 
    params <- df[1:index.position,]
    
  } else {
    
    # if no additional plausible values, just return first row
    params <- df[1,]
    
  }
  
  # return result
  return(params)
  
}


cluster.extremes <- function(raster.cube.df,raster.kNN.df,raster.bool,raster.var,param.summary){

  # try catch 
  tryCatch({
    
    # define variables
    st.cube <- raster.cube.df
    df <- raster.kNN.df
    
    # choose method for dbscan
    if (param.summary == 'max'){
      clusters <- dbscan::dbscan(st.cube[,1:2],eps = max(df$epsilon),minPts = max(df$mu))
    }
    if (param.summary == 'min'){
      clusters <- dbscan::dbscan(st.cube[,1:2],eps = min(df$epsilon),minPts = min(df$mu))
    }
    if (param.summary == 'mean'){
      clusters <- dbscan::dbscan(st.cube[,1:2],eps = mean(df$epsilon),minPts = mean(df$mu))
    }
    if (param.summary == 'median'){
      clusters <- dbscan::dbscan(st.cube[,1:2],eps = median(df$epsilon),minPts = median(df$mu))
    }
    
    # append cluster to st.cube
    st.cube$cluster <- clusters$cluster
    
    # return date time dimension
    st.cube$date.time <- terra::time(raster.var[[st.cube$date]])
    
    # results
    results <- list()
    
    # append results
    results[[1]] <- st.cube
    results[[2]] <- clusters
    
    # return result
    return(results)
    
  })
  
}

sum_with_na <- function(..., na.rm = FALSE) {
  # Use rowSums to calculate the sum across layers, setting na.rm = TRUE
  # but ensuring NA is returned if any NA is encountered and not removed
  layers <- list(...)
  sum_result <- rowSums(do.call(cbind, layers), na.rm = na.rm)
  
  # Restore NA values where all layers are NA
  all_na <- Reduce(`&`, lapply(layers, is.na))
  sum_result[all_na] <- NA
  
  return(sum_result)
}

cluster.extremes.2 <- function(raster.cube.df,raster.bool,raster.var,raster.kNN.df){
  
  # # debug
  # raster.cube.df <- var.st.cube
  # raster.bool <- var.bool
  # raster.var <- var
  # raster.kNN.df <- var.kNN
  
  # define variables
  st.cube <- raster.cube.df[,c(2,4,7)]
  
  # cluster using dbscan
  # clusters <- dbscan::dbscan(st.cube[,1:2],eps = raster.kNN.df$epsilon,minPts = raster.kNN.df$mu)
  clusters <- dbscan::dbscan(st.cube,eps = raster.kNN.df$epsilon,minPts = raster.kNN.df$mu)
  
  # append cluster to st.cube
  # st.cube$cluster <- clusters$cluster
  raster.cube.df$cluster <- clusters$cluster
  
  # return date time dimension
  # st.cube$date.time <- terra::time(raster.var[[st.cube$date]])
  
  # results
  results <- list()
  
  # append results
  results[[1]] <- raster.cube.df
  results[[2]] <- clusters
  
  # return result
  return(results)
  
}

cluster.rasters <- function(clusters.df,raster.var,cluster.st.metric){
 
  # debug
  # clusters.df <- var.cluster[[1]]
  # raster.var <- var
  # cluster.st.metric <- var.st.metric[1]
  
  # lat long 
  raster.df <- as.data.frame(raster.var,xy=T)
  raster.df <- raster.df[,c(1,2)]
  lat <- unique(raster.df$y)
  long <- rev(unique(raster.df$x))
  
  # make lat long position dfs
  lat <- data.frame(lat = lat,lat.pos = seq(1,length(lat),1))
  long <- data.frame(long = long,long.pos = seq(1,length(long),1))
  
  # merge position with cluster df
  cluster.df <- merge(clusters.df,lat,by='lat')
  cluster.df <- merge(cluster.df,long,by='long')
  
  cluster.df %>%
    arrange(cluster)
  
  # create empty list
  cluster.list <- list()
  
  # create raster for each cluster
  print(paste0('There are ',max(cluster.df$cluster),' Clusters'))
  
  # note that cluster == 0 are noise points and are ignored, so we start at 1
  
  for (i in 1:max(cluster.df$cluster)){
    
    # print(paste0('Cluster # ',i))
    # i <- 1
    
    # get df for each cluster
    temp.df <- cluster.df[cluster.df$cluster==i,] # change 1 to i
    
    # subset to all unique date, lat.pos, long.pos
    raster.index <- data.frame(lat = temp.df$lat.pos,long = temp.df$long.pos,time = temp.df$date)
    
    # Then add back time slices before and after cluster for half of the value of the st metric
    # this accounts for decompressing the cluster results when examining the real dataset
    # This also happily deals with the issues of multiple time slices in the original raster.index df
    # By filling in half of the hourly observations on either side of a cluster point before reaching 
    # the next cluster point in time. Will therefore need to go +1 farther than half way in case
    # values round down and would accidentally miss a point between two cluster points in time.
    # Therefore, will need to remove repeated rows in the case of double counting when rounded up.
    cluster.st.metric <- round(cluster.st.metric/2,0)+1
    
    # include additional time slices around each cluster point
    for (h in 1:cluster.st.metric[1,1]){
      
      # print(paste0('Time slice # ',h))
      # h <- 1
      
      # create new data.frames for each new time slice
      time.earlier <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time-h)
      time.later <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time+h)
      
      # remove 0 and negative values
      time.earlier <- time.earlier[!time.earlier$time < 1,]
      time.later <- time.later[!time.later$time < 1,]
      
      # append to existing raster.index
      raster.index <- rbind(raster.index,time.earlier,time.later)
      
    }
    
    # remove duplicate rows 
    raster.index <- raster.index[!duplicated(raster.index),]
    
    # https://stackoverflow.com/questions/39375629/how-to-subset-a-raster-by-cell-number-in-r
    # make new raster and fill with NAs, will append later
    raster.new <- raster.var
    values(raster.new) <- NA
    
    # get original raster for raster multiplication
    raster.orig <- raster.var

    # remove layers with entirely NA values
    raster.subset <- seq(min(raster.index$time),max(raster.index$time),1) # HCQ here's the problem
    raster.new <- raster.new[[raster.subset]]
    raster.orig <- raster.orig[[raster.subset]]
    
    # subset time column to new raster dimensions
    raster.index$time <- raster.index$time - min(raster.index$time) + 1
    
    # change raster values to 1 that are associated with the cluster
    for (j in 1:nrow(raster.index)){
      
      # print(j)
      # j <- 1
      
      # append NA with 1
      raster.new[raster.index[j,1],raster.index[j,2],raster.index[j,3]] <- 1
      
    }
    
    # create outline of all cluster points per layer and then fill in with 1's
    temp.cluster <- list()
    
    # clear memory
    # gc()
    
    for (k in 1:nlyr(raster.new)){ # UNCOMMENT THIS
      
      # print(k)
      # k <-1
      
      # convert spatraster to spatvector of points
      vector.new <- terra::as.points(raster.new[[k]],values=T) 
      
      # create polygon from points
      polygon.new <- terra::convHull(vector.new)
      
      # give polygon value of 1
      values(polygon.new) <- 1
      
      # rasterize polygon
      temp.cluster[[k]] <- terra::rasterize(polygon.new,raster.new[[k]])
      
    } # UNCOMMENT THIS
    
    # stack result
    raster.filled <- terra::rast(temp.cluster)
    
    # give time 
    terra::time(raster.filled) <- terra::time(raster.new)
    
    # give layer name 
    names(raster.filled) <- terra::time(raster.new)

    # change NA values to 0
    raster.filled[is.na(raster.filled)] <- 0

    # multiply 0 and 1 raster with raster.var
    raster.filled <- raster.filled * raster.orig

    # append to cluster.list
    cluster.list[[i]] <- raster.filled

  }
 
  # return result
  return(cluster.list)
  
}

cluster.rasters.2 <- function(clusters.df,raster.var,cluster.st.metric){
  
  # # debug
  # clusters.df <- var.cluster[[1]]
  # raster.var <- var
  # cluster.st.metric <- var.st.metric[1]
  
  # lat long 
  raster.df <- as.data.frame(raster.var,xy=T)
  raster.df <- raster.df[,c(1,2)]
  lat <- unique(raster.df$y)
  long <- rev(unique(raster.df$x))
  
  # make lat long position dfs
  lat <- data.frame(lat = lat,lat.pos = seq(1,length(lat),1))
  long <- data.frame(long = long,long.pos = rev(seq(1,length(long),1)))
  
  # merge position with cluster df
  cluster.df <- merge(clusters.df,lat,by='lat.pos')
  cluster.df <- merge(cluster.df,long,by='long.pos')
  
  # create empty list
  cluster.list <- list()
  
  # create raster for each cluster
  print(paste0('There are ',max(cluster.df$cluster),' Clusters'))
  
  # note that cluster == 0 are noise points and are ignored, so we start at 1
  
  for (i in 1:max(cluster.df$cluster)){
    
    # print(paste0('Cluster # ',i))
    # i <- 1
    
    # get df for each cluster
    temp.df <- cluster.df[cluster.df$cluster==i,] # change 1 to i
    
    # subset to all unique date, lat.pos, long.pos
    raster.index <- data.frame(lat = temp.df$lat.pos,long = temp.df$long.pos,time = temp.df$date.original)
    
    # Then add back time slices before and after cluster for half of the value of the st metric
    # this accounts for decompressing the cluster results when examining the real dataset
    # This also happily deals with the issues of multiple time slices in the original raster.index df
    # By filling in half of the hourly observations on either side of a cluster point before reaching 
    # the next cluster point in time. Will therefore need to go +1 farther than half way in case
    # values round down and would accidentally miss a point between two cluster points in time.
    # Therefore, will need to remove repeated rows in the case of double counting when rounded up.
    cluster.st.metric <- round(cluster.st.metric[1]/2,0)+1
    
    # include additional time slices around each cluster point
    for (h in 1:cluster.st.metric[1,1]){
      
      # print(paste0('Time slice # ',h))
      # h <- 1
      
      # create new data.frames for each new time slice
      time.earlier <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time-h)
      time.later <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time+h)
      
      # remove 0 and negative values
      time.earlier <- time.earlier[!time.earlier$time < 1,]
      time.later <- time.later[!time.later$time < 1,]
      
      # append to existing raster.index
      raster.index <- rbind(raster.index,time.earlier,time.later)
      
    }
    
    # remove duplicate rows 
    raster.index <- raster.index[!duplicated(raster.index),]
    
    # https://stackoverflow.com/questions/39375629/how-to-subset-a-raster-by-cell-number-in-r
    # make new raster and fill with NAs, will append later
    raster.new <- raster.var
    values(raster.new) <- NA
    
    # get original raster for raster multiplication
    raster.orig <- raster.var
    
    # remove layers with entirely NA values
    raster.subset <- seq(min(raster.index$time),max(raster.index$time),1) # HCQ here's the problem
    raster.new <- raster.new[[raster.subset]]
    raster.orig <- raster.orig[[raster.subset]]
    
    # subset time column to new raster dimensions
    raster.index$time <- raster.index$time - min(raster.index$time) + 1
    
    # change raster values to 1 that are associated with the cluster
    for (j in 1:nrow(raster.index)){
      
      # print(j)
      # j <- 1
      
      # append NA with 1
      raster.new[raster.index[j,1],raster.index[j,2],raster.index[j,3]] <- 1
      
    }
    
    # create outline of all cluster points per layer and then fill in with 1's
    temp.cluster <- list()
    
    # clear memory
    # gc()
    
    for (k in 1:nlyr(raster.new)){ # UNCOMMENT THIS
      
      # print(k)
      # k <-1
      
      # convert spatraster to spatvector of points
      vector.new <- terra::as.points(raster.new[[k]],values=T) 
      
      # create polygon from points
      polygon.new <- terra::convHull(vector.new)
      
      # give polygon value of 1
      values(polygon.new) <- 1
      
      # rasterize polygon
      temp.cluster[[k]] <- terra::rasterize(polygon.new,raster.new[[k]])
      
    } # UNCOMMENT THIS
    
    # stack result
    raster.filled <- terra::rast(temp.cluster)
    
    # give time 
    terra::time(raster.filled) <- terra::time(raster.new)
    
    # give layer name 
    names(raster.filled) <- terra::time(raster.new)
    
    # change NA values to 0
    raster.filled[is.na(raster.filled)] <- 0
    
    # multiply 0 and 1 raster with raster.var
    raster.filled <- raster.filled * raster.orig
    
    # append to cluster.list
    cluster.list[[i]] <- raster.filled
    
  }
  
  # return result
  return(cluster.list)
  
}

cluster.rasters.3 <- function(clusters.df,raster.var,cluster.st.metric){
  
  # # debug
  # clusters.df <- var.cluster[[1]]
  # raster.var <- var
  # cluster.st.metric <- var.st.metric[1]
  
  # lat long 
  raster.df <- as.data.frame(raster.var,xy=T)
  raster.df <- raster.df[,c(1,2)]
  lat <- unique(raster.df$y)
  long <- rev(unique(raster.df$x))
  
  # make lat long position dfs
  lat <- data.frame(lat = lat,lat.pos = seq(1,length(lat),1))
  long <- data.frame(long = long,long.pos = rev(seq(1,length(long),1)))
  
  # merge position with cluster df
  cluster.df <- merge(clusters.df,lat,by='lat.pos')
  cluster.df <- merge(cluster.df,long,by='long.pos')
  
  # create empty list
  cluster.list <- list()
  
  # create raster for each cluster
  print(paste0('There are ',max(cluster.df$cluster),' Clusters'))
  
  # note that cluster == 0 are noise points and are ignored, so we start at 1
  
  for (i in 1:max(cluster.df$cluster)){
    
    # i <- 33
    # print(paste0('Cluster # ',i))
    
    # get df for each cluster
    temp.df <- cluster.df[cluster.df$cluster==i,] # change 1 to i
    
    # subset to all unique date, lat.pos, long.pos
    raster.index <- data.frame(lat = temp.df$lat.pos,long = temp.df$long.pos,time = temp.df$date.compress)
    
    # Then add back time slices before and after cluster for half of the value of the st metric
    # this accounts for decompressing the cluster results when examining the real dataset
    # This also happily deals with the issues of multiple time slices in the original raster.index df
    # By filling in half of the hourly observations on either side of a cluster point before reaching 
    # the next cluster point in time. Will therefore need to go +1 farther than half way in case
    # values round down and would accidentally miss a point between two cluster points in time.
    # Therefore, will need to remove repeated rows in the case of double counting when rounded up.
    cluster.st.metric <- round(cluster.st.metric[1]/2,0)+1
    
    # include additional time slices around each cluster point
    for (h in 1:cluster.st.metric[1,1]){
      
      # print(paste0('Time slice # ',h))
      # h <- 1
      
      # create new data.frames for each new time slice
      time.earlier <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time-h)
      time.later <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time+h)
      
      # remove values smaller than time range of that year
      time.earlier <- time.earlier[!time.earlier$time < 1,]
      time.later <- time.later[!time.later$time < 1,]
      
      # remove values greater than time range of that year
      time.later <- time.later[!time.later$time > terra::nlyr(raster.var),]
      
      # append to existing raster.index
      raster.index <- rbind(raster.index,time.earlier,time.later)
      
    }
    
    # remove duplicate rows 
    raster.index <- raster.index[!duplicated(raster.index),]
    
    # https://stackoverflow.com/questions/39375629/how-to-subset-a-raster-by-cell-number-in-r
    # make new raster and fill with NAs, will append later
    raster.new <- raster.var
    values(raster.new) <- NA
    
    # get original raster for raster multiplication
    raster.orig <- raster.var
    
    # remove layers with entirely NA values
    raster.subset <- seq(min(raster.index$time),max(raster.index$time),1) # HCQ here's the problem
    raster.new <- raster.new[[raster.subset]]
    raster.orig <- raster.orig[[raster.subset]]
    
    # subset time column to new raster dimensions
    raster.index$time <- raster.index$time - min(raster.index$time) + 1
    
    # change raster values to 1 that are associated with the cluster
    for (j in 1:nrow(raster.index)){
      
      # print(j)
      # j <- 1
      
      # append NA with 1
      raster.new[raster.index[j,1],raster.index[j,2],raster.index[j,3]] <- 1
      
    }
    
    # create outline of all cluster points per layer and then fill in with 1's
    temp.cluster <- list()
    
    # clear memory
    # gc()
    
    for (k in 1:nlyr(raster.new)){ # UNCOMMENT THIS
      
      # print(k)
      # k <-1
      
      # convert spatraster to spatvector of points
      vector.new <- terra::as.points(raster.new[[k]],values=T) 
      
      # create polygon from points
      polygon.new <- terra::convHull(vector.new)
      
      # give polygon value of 1
      values(polygon.new) <- 1
      
      # rasterize polygon
      temp.cluster[[k]] <- terra::rasterize(polygon.new,raster.new[[k]])
      
    } # UNCOMMENT THIS
    
    # stack result
    raster.filled <- terra::rast(temp.cluster)
    
    # give time 
    terra::time(raster.filled) <- terra::time(raster.new)
    
    # give layer name 
    names(raster.filled) <- terra::time(raster.new)
    
    # change NA values to 0
    raster.filled[is.na(raster.filled)] <- 0
    
    # multiply 0 and 1 raster with raster.var
    raster.filled <- raster.filled * raster.orig
    
    # append to cluster.list
    cluster.list[[i]] <- raster.filled
    
  }
  
  # return result
  return(cluster.list)
  
}

cluster.rasters.4 <- function(clusters.df,raster.var,cluster.st.metric){
  
  # # debug
  # clusters.df <- var.cluster[[1]]
  # raster.var <- var
  # cluster.st.metric <- var.st.metric[1]
  
  # lat long 
  raster.df <- as.data.frame(raster.var,xy=T)
  raster.df <- raster.df[,c(1,2)]
  lat <- unique(raster.df$y)
  long <- rev(unique(raster.df$x))
  
  # make lat long position dfs
  lat <- data.frame(lat = lat,lat.pos = seq(1,length(lat),1))
  long <- data.frame(long = long,long.pos = rev(seq(1,length(long),1)))
  
  # merge position with cluster df
  cluster.df <- merge(clusters.df,lat,by='lat.pos')
  cluster.df <- merge(cluster.df,long,by='long.pos')
  
  # create empty list
  cluster.list <- list()
  
  # create raster for each cluster
  print(paste0('There are ',max(cluster.df$cluster),' Clusters'))
  
  # note that cluster == 0 are noise points and are ignored, so we start at 1
  
  # Test if there are any clusters
  if (max(cluster.df$cluster) > 0){
    
    for (i in 1:max(cluster.df$cluster)){
      
      # i <- 33
      # print(paste0('Cluster # ',i))
      
      # get df for each cluster
      temp.df <- cluster.df[cluster.df$cluster==i,] # change 1 to i
      
      # subset to all unique date, lat.pos, long.pos
      raster.index <- data.frame(lat = temp.df$lat.pos,long = temp.df$long.pos,time = temp.df$date.original) # time = temp.df$date.compress
      
      # Then add back time slices before and after cluster for half of the value of the st metric
      # this accounts for decompressing the cluster results when examining the real dataset
      # This also happily deals with the issues of multiple time slices in the original raster.index df
      # By filling in half of the hourly observations on either side of a cluster point before reaching 
      # the next cluster point in time. Will therefore need to go +1 farther than half way in case
      # values round down and would accidentally miss a point between two cluster points in time.
      # Therefore, will need to remove repeated rows in the case of double counting when rounded up.
      cluster.st.metric <- round(cluster.st.metric[1]/2,0)+1
      
      # include additional time slices around each cluster point
      for (h in 1:cluster.st.metric[1,1]){
        
        # print(paste0('Time slice # ',h))
        # h <- 1
        
        # create new data.frames for each new time slice
        time.earlier <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time-h)
        time.later <- data.frame(lat = raster.index$lat,long = raster.index$long,time = raster.index$time+h)
        
        # remove values smaller than time range of that year
        time.earlier <- time.earlier[!time.earlier$time < 1,]
        time.later <- time.later[!time.later$time < 1,]
        
        # remove values greater than time range of that year
        time.later <- time.later[!time.later$time > terra::nlyr(raster.var),]
        
        # append to existing raster.index
        raster.index <- rbind(raster.index,time.earlier,time.later)
        
      }
      
      # remove duplicate rows 
      raster.index <- raster.index[!duplicated(raster.index),]
      
      # https://stackoverflow.com/questions/39375629/how-to-subset-a-raster-by-cell-number-in-r
      # make new raster and fill with NAs, will append later
      raster.new <- raster.var
      values(raster.new) <- NA
      
      # get original raster for raster multiplication
      raster.orig <- raster.var
      
      # remove layers with entirely NA values
      raster.subset <- seq(min(raster.index$time),max(raster.index$time),1) # HCQ here's the problem
      raster.new <- raster.new[[raster.subset]]
      raster.orig <- raster.orig[[raster.subset]]
      
      # subset time column to new raster dimensions
      raster.index$time <- raster.index$time - min(raster.index$time) + 1
      
      # change raster values to 1 that are associated with the cluster
      for (j in 1:nrow(raster.index)){
        
        # print(j)
        # j <- 1
        
        # append NA with 1
        raster.new[raster.index[j,1],raster.index[j,2],raster.index[j,3]] <- 1
        
      }
      
      # create outline of all cluster points per layer and then fill in with 1's
      temp.cluster <- list()
      
      # clear memory
      # gc()
      
      for (k in 1:nlyr(raster.new)){ # UNCOMMENT THIS
        
        # print(k)
        # k <-1
        
        # convert spatraster to spatvector of points
        vector.new <- terra::as.points(raster.new[[k]],values=T) 
        
        # create polygon from points
        polygon.new <- terra::convHull(vector.new)
        
        # give polygon value of 1
        values(polygon.new) <- 1
        
        # rasterize polygon
        temp.cluster[[k]] <- terra::rasterize(polygon.new,raster.new[[k]])
        
      } # UNCOMMENT THIS
      
      # stack result
      raster.filled <- terra::rast(temp.cluster)
      
      # give time 
      terra::time(raster.filled) <- terra::time(raster.new)
      
      # give layer name 
      names(raster.filled) <- terra::time(raster.new)
      
      # change NA values to 0
      raster.filled[is.na(raster.filled)] <- 0
      
      # multiply 0 and 1 raster with raster.var
      raster.filled <- raster.filled * raster.orig
      
      # append to cluster.list
      cluster.list[[i]] <- raster.filled
      
    }
    
    # return result
    return(cluster.list)
    
  } else {}
  
}

cluster.subset <- function(cluster.raster.list){
  
  # debug
  cluster.raster.list <- var.cluster.raster
  plot(cluster.raster.list[[1]])
  
  # set up new list
  cluster.raster.list.subset <- list()
  
  # initiate list element
  j <- 1
  
  # Find list elements with values
  for (i in 1:length(cluster.raster.list)){
    # print(i)
    if(terra::minmax(sum(cluster.raster.list[[i]]))[2] > 0){
      
      # print('Cluster')
      
      # store cluster list element
      cluster.raster.list.subset[[j]] <- cluster.raster.list[[i]]
      
      # append list element
      j <- j + 1
      
    }else{
      # print('No Cluster')}
      # cluster.raster.list.subset[[j]] <- cluster.raster.list[[i]]
    }

    # return result
    return(cluster.raster.list.subset)
    # if (length(cluster.raster.list.subset) > 0){
    #   return(cluster.raster.list.subset)
    # } else {}
    
  }
  
}

cluster.metadata <- function(cluster.raster.list,raster.source){

  # # debug
  cluster.raster.list <- var.cluster.raster
  raster.source <- var
  
  # convert to spatraster if necessary
  # raster.source <- terra::rast(raster.source)
  
  # initiate first layer
  x <- cluster.raster.list[[1]]
  
  # get domain cells
  total.cells <- terra::cells(raster.source)
  total.cells <- length(total.cells)
  
  # remove layers without extremes
  nil.layers <- c()
  counter <- 1
  
  for (i in 1:terra::nlyr(x)){
    if (terra::minmax(x[[i]])[2] == 0){
      nil.layers[[counter]] <- i
      counter <- counter + 1
    }
  }
  
  nil.layers <- unlist(nil.layers)
  
  # remove layers only if necessary
  if (!is.null(nil.layers)) {
    x <- x[[-nil.layers]]
  }
  
  # force NAs
  x[x == 0] <- NA
  
  # summary statistics
  cluster.mean <- terra::mean(x,na.rm=T)
  cluster.minmax <- terra::minmax(x)
  cluster.median <- terra::median(x,na.rm=T)
  cluster.sd <- terra::stdev(x,na.rm=T)
  cluster.range <- range(terra::time(x))
  cluster.duration <- diff(cluster.range)
  domain.area.km2 <- terra::expanse(raster.source,unit='km')
  cluster.area.km2 <- terra::expanse(x,unit='km')
  
  # compile summary statistics
  summary.df <- data.frame(start.time = cluster.range[1],
                           end.time = cluster.range[2],
                           duration = cluster.duration,
                           area = max(cluster.area.km2[,2]),
                           fraction = max(cluster.area.km2[,2])/domain.area.km2[1,2]*100,
                           max.lower = min(cluster.minmax[2,]),
                           max.upper = max(cluster.minmax[2,]),
                           min.lower = min(cluster.minmax[1,]),
                           min.upper = max(cluster.minmax[1,]),
                           mean.lower = terra::minmax(cluster.mean)[1],
                           mean.upper = terra::minmax(cluster.mean)[2],
                           med.lower = terra::minmax(cluster.median)[1],
                           med.upper = terra::minmax(cluster.median)[2],
                           sd.lower = terra::minmax(cluster.sd)[1],
                           sd.upper = terra::minmax(cluster.sd)[2])
  
  # test whether there are more than 1 clusters 
  if (length(cluster.raster.list) > 1) {
    
    for (h in 2:length(cluster.raster.list)){
      
      # initiate first layer
      x <- cluster.raster.list[[h]]
      
      # get domain cells
      total.cells <- freq(!is.na(raster.source))
      total.cells <- total.cells[2,2]
      
      # remove layers without extremes
      nil.layers <- c()
      counter <- 1
      
      for (i in 1:terra::nlyr(x)){
        if (terra::minmax(x[[i]])[2] == 0){
          nil.layers[[counter]] <- i
          counter <- counter + 1
        }
      }
      
      
      nil.layers <- unlist(nil.layers)
      
      # remove layers only if necessary
      if (!is.null(nil.layers)) {
        x <- x[[-nil.layers]]
      }
      
      # force NAs
      x[x == 0] <- NA
      
      # summary statistics
      cluster.mean <- terra::mean(x,na.rm=T)
      cluster.minmax <- terra::minmax(x)
      cluster.median <- terra::median(x,na.rm=T)
      cluster.sd <- terra::stdev(x,na.rm=T)
      cluster.range <- range(terra::time(x))
      cluster.duration <- diff(cluster.range)
      domain.area.km2 <- terra::expanse(raster.source,unit='km')
      cluster.area.km2 <- terra::expanse(x,unit='km')
      
      # compile summary statistics
      summary.df.temp <- data.frame(start.time = cluster.range[1],
                                    end.time = cluster.range[2],
                                    duration = cluster.duration,
                                    area = max(cluster.area.km2[,2]),
                                    fraction = max(cluster.area.km2[,2])/domain.area.km2[1,2]*100,
                                    max.lower = min(cluster.minmax[2,]),
                                    max.upper = max(cluster.minmax[2,]),
                                    min.lower = min(cluster.minmax[1,]),
                                    min.upper = max(cluster.minmax[1,]),
                                    mean.lower = terra::minmax(cluster.mean)[1],
                                    mean.upper = terra::minmax(cluster.mean)[2],
                                    med.lower = terra::minmax(cluster.median)[1],
                                    med.upper = terra::minmax(cluster.median)[2],
                                    sd.lower = terra::minmax(cluster.sd)[1],
                                    sd.upper = terra::minmax(cluster.sd)[2])
      
      # append to existing df
      summary.df <- rbind(summary.df,summary.df.temp)
      
    }
    
  } else {}

  # return result
  return(summary.df)
  
}

cluster.metadata.2 <- function(cluster.raster.list,raster.source){
  
  # # debug
  # cluster.raster.list <- var.cluster.raster.list.combined.final
  # raster.source <- var
  
  # convert to spatraster if necessary
  # raster.source <- terra::rast(raster.source)
  
  # initiate first layer
  x <- cluster.raster.list[[1]]
  
  # test if any values above 0
  if (max(terra::minmax(x)) > 0) {
    
    # get domain cells
    total.cells <- terra::cells(raster.source)
    total.cells <- length(total.cells)
    
    # remove layers without extremes
    nil.layers <- c()
    counter <- 1
    
    for (i in 1:terra::nlyr(x)){
      if (terra::minmax(x[[i]])[2] == 0){
        nil.layers[[counter]] <- i
        counter <- counter + 1
      }
    }
    
    nil.layers <- unlist(nil.layers)
    
    # remove layers only if necessary
    if (!is.null(nil.layers)) {
      x <- x[[-nil.layers]]
    }
    
    # force NAs
    x[x == 0] <- NA
    
    # summary statistics
    cluster.mean <- terra::mean(x,na.rm=T)
    cluster.minmax <- terra::minmax(x)
    cluster.median <- terra::median(x,na.rm=T)
    cluster.sd <- terra::stdev(x,na.rm=T)
    cluster.range <- range(terra::time(x))
    cluster.duration <- diff(cluster.range)
    domain.area.km2 <- terra::expanse(raster.source,unit='km')
    cluster.area.km2 <- terra::expanse(x,unit='km')
    
    # compile summary statistics
    summary.df <- data.frame(start.time = cluster.range[1],
                             end.time = cluster.range[2],
                             duration = cluster.duration,
                             area = max(cluster.area.km2[,2]),
                             fraction = max(cluster.area.km2[,2])/domain.area.km2[1,2]*100,
                             max.lower = min(cluster.minmax[2,]),
                             max.upper = max(cluster.minmax[2,]),
                             min.lower = min(cluster.minmax[1,]),
                             min.upper = max(cluster.minmax[1,]),
                             mean.lower = terra::minmax(cluster.mean)[1],
                             mean.upper = terra::minmax(cluster.mean)[2],
                             med.lower = terra::minmax(cluster.median)[1],
                             med.upper = terra::minmax(cluster.median)[2],
                             sd.lower = terra::minmax(cluster.sd)[1],
                             sd.upper = terra::minmax(cluster.sd)[2])
    
    # test whether there are more than 1 clusters 
    if (length(cluster.raster.list) > 1) {
      
      for (h in 2:length(cluster.raster.list)){
        
        # initiate first layer
        x <- cluster.raster.list[[h]]
        
        # test if any values above 0
        if (max(terra::minmax(x)) > 0) {
          
          # get domain cells
          total.cells <- freq(!is.na(raster.source))
          total.cells <- total.cells[2,2]
          
          # remove layers without extremes
          nil.layers <- c()
          counter <- 1
          
          for (i in 1:terra::nlyr(x)){
            if (terra::minmax(x[[i]])[2] == 0){
              nil.layers[[counter]] <- i
              counter <- counter + 1
            }
          }
          
          
          nil.layers <- unlist(nil.layers)
          
          # remove layers only if necessary
          if (!is.null(nil.layers)) {
            x <- x[[-nil.layers]]
          }
          
          # force NAs
          x[x == 0] <- NA
          
          # summary statistics
          cluster.mean <- terra::mean(x,na.rm=T)
          cluster.minmax <- terra::minmax(x)
          cluster.median <- terra::median(x,na.rm=T)
          cluster.sd <- terra::stdev(x,na.rm=T)
          cluster.range <- range(terra::time(x))
          cluster.duration <- diff(cluster.range)
          domain.area.km2 <- terra::expanse(raster.source,unit='km')
          cluster.area.km2 <- terra::expanse(x,unit='km')
          
          # compile summary statistics
          summary.df.temp <- data.frame(start.time = cluster.range[1],
                                        end.time = cluster.range[2],
                                        duration = cluster.duration,
                                        area = max(cluster.area.km2[,2]),
                                        fraction = max(cluster.area.km2[,2])/domain.area.km2[1,2]*100,
                                        max.lower = min(cluster.minmax[2,]),
                                        max.upper = max(cluster.minmax[2,]),
                                        min.lower = min(cluster.minmax[1,]),
                                        min.upper = max(cluster.minmax[1,]),
                                        mean.lower = terra::minmax(cluster.mean)[1],
                                        mean.upper = terra::minmax(cluster.mean)[2],
                                        med.lower = terra::minmax(cluster.median)[1],
                                        med.upper = terra::minmax(cluster.median)[2],
                                        sd.lower = terra::minmax(cluster.sd)[1],
                                        sd.upper = terra::minmax(cluster.sd)[2])
          
          # append to existing df
          summary.df <- rbind(summary.df,summary.df.temp)
          
        } else {}
        
      }
      
    } else {return(summary.df)}
    
    # return result
    return(summary.df)
    
  } else {print(paste0('There are no cluster events to summarize'))}
  
}

cluster.compile <- function(raster.var,cluster.raster.list){

  # debug
  raster.var <- var
  cluster.raster.list <- var.cluster.raster
  
  # give time to spatraster names
  names(raster.var) <- terra::time(raster.var)
  
  # start with empty raster with time range
  compiled <- raster.var
  zeros <- raster.var

  # set values to NA
  values(compiled) <- NA
  
  # set values to 0
  zeros <- zeros / zeros
  zeros <- zeros - zeros
  
  # # append raster with each cluster list item
  # for (i in 1:length(cluster.raster.list)){
  #   compiled <- terra::merge(cluster.raster.list[[i]],compiled)
  # }
  
  # go through each list item containing a cluster
  for (i in 1:length(cluster.raster.list)){
    
    # get time range of list item
    time.range <- terra::time(cluster.raster.list[[1]])
    
    # create empty list
    time.index <- list()
    
    # remove last four characters of time.range (removing UTC)
    # time.range <- gsub(',{4}$','',time.range)
    for (j in time.range){
      time.index[[j]] <- which(terra::time(compiled) == time.range[j])
    }
    # time.index <- which(terra::time(compiled) == time.range) 
    
    # get same time subset of original raster
    reference <- compiled[[time.range]]
    
  }
  
  # get only extremes
  compiled <- terra::subst(compiled,0,NA)
  
  # convert to 1's
  compiled <- compiled/compiled
  
  # give remaining values 0
  compiled <- terra::mosaic(compiled, zeros,fun='sum')
  
  # give time back
  terra::time(compiled) <- terra::time(raster.var)

  # return result
  return(compiled)
  
}

cluster.compile.2 <- function(raster.threshold.criteria,vector.year.range,
                              string.hazard.name,string.directory.variable.raw,
                              string.directory.variable.boolean,string.variable.unit,
                              numeric.space.time.metric,string.directory.cluster){
  
  # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_warning_1_hour_20240925.nc')
  # vector.year.range = seq(2019,2023,1)
  # string.hazard.name = "Excessive_Heat_Warning"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/heat_index/warning/"
  # numeric.space.time.metric = data.frame(alpha = c(0.75,0.25),range = c(50,1000),tau = c(1000,20))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/"
  # string.variable.unit = "C"
  
  # code
  for (year in vector.year.range) {
    
    # year <- 2019
    
    # print statement
    print(paste0('-------------------- ',string.hazard.name,' ',year,' --------------------'))
    
    # time
    start.time <- Sys.time()
    print(start.time)
    
    # Load regular Raster
    var <- raster.compile(year = as.character(year),directory = string.directory.variable.raw)
    
    # Load Boolean Raster
    var.bool <- raster.compile(year = as.character(year),directory = string.directory.variable.boolean)
    plot(sum(var.bool))
    
    # Crop
    var <- raster.crop.mask(var,raster.threshold.criteria)
    
    # Correct Extent
    terra::ext(var) <- terra::ext(raster.threshold.criteria)
    terra::ext(var.bool) <- terra::ext(raster.threshold.criteria)
    
    # Cluster (10 min)
    # DBSCAN - covariance model parameters obtained from BMElib Matlab code
    var.cov.model <- numeric.space.time.metric
    
    # DBSCAN - space time metric: result is in (# hours / 0.25 spatial degree)
    # no longer need to multiply by 4 to get # hours / 0.25 spatial degree (ERA5 units)
    var.st.metric <- cov.ratio(var.cov.model) 
    
    # round to nearest whole number
    # var.st.metric <- round(var.st.metric,0)
    
    # change st.metric to 1 if below 1
    if (var.st.metric[1] < 1){
      var.st.metric[1] <- 1
    }
    
    if (var.st.metric[2] < 1){
      var.st.metric[2] <- 1
    }
    
    # var.st.metric[1] <- 1
    # var.st.metric[2] <- 1
    
    # DBSCAN - create space time cube (1 min)
    var.cube <- cluster.lat.long.date(var.bool)

    # # recursively cluster for each compressed row
    # for (compressed in 1:var.st.metric[1]){
    #   
    #   
    #   
    # }
    
    # DBSCAN - incorporate space time metric
    var.st.cube <- cluster.st.cube(var.cube,var.st.metric[1])

    # Test if enough points to continue
    if (nrow(var.st.cube) >= 4){
      
      # DBSCAN - kth nearest neighbor analysis to return epsilon and mu
      var.kNN <- cluster.kNN.2(var.cube) # var.st.cube
      
      # DBSCAN - cluster for defined mu and epsilon
      var.cluster <- cluster.extremes.2(raster.cube.df = var.cube, # var.st.cube
                                        raster.bool = raster.bool,
                                        raster.var = var,
                                        raster.kNN.df = var.kNN)
      
      # DBSCAN - get raster for each cluster
      var.cluster.raster <- cluster.rasters.2(clusters.df = var.cluster[[1]],
                                            raster.var = var,
                                            cluster.st.metric = var.st.metric[1])
      
      # plot
      # plot(var.cluster.raster[[7]])
      
      # Get max extent per cluster event 
      event.extent <- list()
      event.count <- list()
      
      # get each spatraster list item
      for (cluster.index in 1:length(var.cluster.raster)) {
        
        # subset
        event.extent.temp <- var.cluster.raster[[cluster.index]]
        
        # binarize
        event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
        
        # give new values, which are 'year.event_number' format
        event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
        
        # loop through time steps of one event 
        for (q in 1:terra::nlyr(event.extent.temp)) {
          
          # give final extent and value to time range 
          event.extent.temp[[q]] <- event.extent.binary
          
        }
        
        # give time back
        terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
        
        # rename layers
        names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
        
        # update list
        event.extent[[cluster.index]] <- event.extent.temp
        
        # update list with first spatial extent per event
        event.count[[cluster.index]] <- event.extent.temp[[1]]
        
      }
      
      # unlist
      event.extent <- terra::rast(unlist(event.extent))
      
      # give time as names to clusters
      names(event.extent) <- terra::time(event.extent)
      
      # calculate daily extent, in cases where multiple clusters occur at the same time
      event.extent <- terra::tapp(event.extent,'days',fun=max)
      
      # normalize extent as binary when overlapping produces 2+ extents in a location
      event.extent <- event.extent / event.extent
      
      # Save output
      save.cdf(raster = event.extent,
               folder.path = paste0(string.directory.cluster,'extent/'),
               file.name = paste0(string.hazard.name,'_extent_',year),
               var.name = 'event_id',
               long.name = 'event_id',
               unit = 'event_id')
      
      # unlist
      event.count <- terra::rast(unlist(event.count))
      
      # give time as names to clusters
      names(event.count) <- terra::time(event.count)
      
      # calculate daily extent, in cases where multiple clusters occur at the same time
      event.count <- terra::tapp(event.count,'days',fun=max)
      
      # normalize extent as binary when overlapping produces 2+ extents in a location
      event.count <- event.count / event.count
      
      # Save output
      save.cdf(raster = event.count,
               folder.path = paste0(string.directory.cluster,'count/'),
               file.name = paste0(string.hazard.name,'_count_',year),
               var.name = 'event_id',
               long.name = 'event_id',
               unit = 'event_id')
      
      # unlist and stack result 
      var.intensity <- terra::rast(unlist(var.cluster.raster))
      
      # remove layers without values
      # 2. Identify layers that are entirely NaN
      # Use sapply to check each layer for all NaN values
      nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
        all(is.nan(values(var.intensity[[i]])))
      })
      
      if (sum(nan_layers) > 0){
        
        # 3. Keep only layers that are not entirely NaN
        # Subset the SpatRaster to keep non-NaN layers
        var.intensity <- var.intensity[[!nan_layers]]
        
      } else {}
      
      # Save output
      save.cdf(raster = var.intensity,
               folder.path = paste0(string.directory.cluster,'intensity/'),
               file.name = paste0(string.hazard.name,'_intensity_',year),
               var.name = string.hazard.name,
               long.name = string.hazard.name,
               unit = string.variable.unit)
      
      # Convert intensity to binary raster
      var.binary <- var.intensity / var.intensity
      
      # Save output
      save.cdf(raster = var.binary,
               folder.path = paste0(string.directory.cluster,'binary/'),
               file.name = paste0(string.hazard.name,'_binary_',year),
               var.name = string.hazard.name,
               long.name = string.hazard.name,
               unit = 'boolean')
      
      # Compile dbscan parameter dataframe
      dbscan.parameters.range <- data.frame(year = year,
                                            mu = var.kNN$mu,
                                            epsilon = var.kNN$epsilon,
                                            event.count = length(var.cluster.raster))

      # remove na rows
      dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]

      # Save resulting dataframe
      write.csv(dbscan.parameters.range,
                file = paste0(string.directory.cluster,'dbscan_parameters_',year,
                              '.csv'))
      
    } else {print(paste0('No clusters in ',year))}
    
    # time
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
  }
  
}

cluster.compile.3 <- function(raster.threshold.criteria,vector.year.range,
                              string.hazard.name,string.directory.variable.raw,
                              string.directory.variable.boolean,string.variable.unit,
                              numeric.space.time.metric,string.directory.cluster){
  
  # debug
  raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_warning_1_hour_20240925.nc')
  vector.year.range = seq(2019,2023,1)
  string.hazard.name = "Excessive_Heat_Warning"
  string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/1-day/"
  string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/heat_index/warning/"
  numeric.space.time.metric = data.frame(alpha = c(0.75,0.25),range = c(50,1000),tau = c(1000,20))
  string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/"
  string.variable.unit = "C"
  
  # # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/precipitation_001_year_24_hour_20240925.nc')
  # vector.year.range = seq(1963,1963,1)
  # string.hazard.name = "Precipitation"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/precipitation/001/"
  # numeric.space.time.metric = data.frame(alpha = c(0.99,0.01),range = c(12,20),tau = c(4,4))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/precipitation/001/"
  # string.variable.unit = "mm"

  # code
  for (year in vector.year.range) {
    
    year <- 1961
    
    # print statement
    print(paste0('-------------------- ',string.hazard.name,' ',year,' --------------------'))
    
    # time
    start.time <- Sys.time()
    print(start.time)
    
    # Load regular Raster
    var <- raster.compile(year = as.character(year),directory = string.directory.variable.raw)
    
    # Load Boolean Raster
    var.bool <- raster.compile(year = as.character(year),directory = string.directory.variable.boolean)
    # plot(sum(var.bool))
    
    # Crop
    var <- raster.crop.mask(var,raster.threshold.criteria)
    
    # Correct Extent
    terra::ext(var) <- terra::ext(raster.threshold.criteria)
    terra::ext(var.bool) <- terra::ext(raster.threshold.criteria)
    
    # Cluster (10 min)
    # DBSCAN - covariance model parameters obtained from BMElib Matlab code
    var.cov.model <- numeric.space.time.metric
    
    # DBSCAN - space time metric: result is in (# hours / 0.25 spatial degree)
    # no longer need to multiply by 4 to get # hours / 0.25 spatial degree (ERA5 units)
    var.st.metric <- cov.ratio(var.cov.model) 
    
    # round to nearest whole number
    var.st.metric <- round(var.st.metric,0)
    
    # change st.metric to 1 if below 1
    if (var.st.metric[1] < 1){
      var.st.metric[1] <- 1
    }
    
    if (var.st.metric[2] < 1){
      var.st.metric[2] <- 1
    }
    
    # DBSCAN - create space time cube (1 min)
    var.cube <- cluster.lat.long.date.2(var.bool)
    
    # create empty list
    var.cluster.raster.list <- list()
    
    # recursively cluster for each compressed row
    for (compressed in 1:var.st.metric[1,1]){
      
      compressed <- 1
      
      # DBSCAN - incorporate space time metric
      var.st.cube <- cluster.st.cube.2(var.cube,var.st.metric[1],row.start = compressed)
      
      # DBSCAN - correct order of var.st.cube df
      var.st.cube <- var.st.cube[,-1]
      
      # Test if enough points to continue
      if (nrow(var.st.cube) >= 4){
        
        # DBSCAN - kth nearest neighbor analysis to return epsilon and mu
        var.kNN <- cluster.kNN.2(var.cube[,c(2,4,5)]) # var.st.cube
        
        # DBSCAN - cluster for defined mu and epsilon
        var.cluster <- cluster.extremes.2(raster.cube.df = var.st.cube, # var.st.cube
                                          raster.bool = raster.bool,
                                          raster.var = var,
                                          raster.kNN.df = var.kNN)
        
        # DBSCAN - get raster for each cluster
        var.cluster.raster <- cluster.rasters.4(clusters.df = var.cluster[[1]],
                                                raster.var = var,
                                                cluster.st.metric = var.st.metric[1])
        
        # See if any clusters
        if (!is.null(var.cluster.raster)){
          
          # store in list object
          var.cluster.raster.list[[compressed]] <- var.cluster.raster 
          
        } else {}
        
        
        
      } else {print(paste0('No clusters in ',year,' for compressed time slice # ',compressed))}
      
    }
    
    if (length(var.cluster.raster.list) > 0){
      
      # unlist 
      var.cluster.raster <- unlist(var.cluster.raster.list)
      
      # calculate metadata
      var.cluster.raster.metadata <- cluster.metadata.2(var.cluster.raster,var)
      
      # save metadata
      write.csv(var.cluster.raster.metadata,
                file = paste0(string.directory.cluster,'metadata/event_metadata_',year,
                              '.csv'))
      
      # Get max extent per cluster event 
      event.extent <- list()
      event.count <- list()
      
      # get each spatraster list item
      for (cluster.index in 1:length(var.cluster.raster)) {
        
        # subset
        event.extent.temp <- var.cluster.raster[[cluster.index]]
        
        # binarize
        event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
        
        # give new values, which are 'year.event_number' format
        event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
        
        # loop through time steps of one event 
        for (q in 1:terra::nlyr(event.extent.temp)) {
          
          # give final extent and value to time range 
          event.extent.temp[[q]] <- event.extent.binary
          
        }
        
        # give time back
        terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
        
        # rename layers
        names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
        
        # update list
        event.extent[[cluster.index]] <- event.extent.temp
        
        # update list with first spatial extent per event
        event.count[[cluster.index]] <- event.extent.temp[[1]]
        
      }
      
      # unlist
      event.extent <- terra::rast(unlist(event.extent))
      
      # give time as names to clusters
      names(event.extent) <- terra::time(event.extent)
      
      # calculate daily extent, in cases where multiple clusters occur at the same time
      event.extent <- terra::tapp(event.extent,'days',fun=max)
      
      # normalize extent as binary when overlapping produces 2+ extents in a location
      event.extent <- event.extent / event.extent
      
      # Save output
      save.cdf(raster = event.extent,
               folder.path = paste0(string.directory.cluster,'extent/'),
               file.name = paste0(string.hazard.name,'_extent_',year),
               var.name = 'event_id',
               long.name = 'event_id',
               unit = 'event_id')
      
      # unlist
      event.count <- terra::rast(unlist(event.count))
      
      # give time as names to clusters
      names(event.count) <- terra::time(event.count)
      
      # calculate daily extent, in cases where multiple clusters occur at the same time
      event.count <- terra::tapp(event.count,'days',fun=max)
      
      # normalize extent as binary when overlapping produces 2+ extents in a location
      event.count <- event.count / event.count
      
      # Save output
      save.cdf(raster = event.count,
               folder.path = paste0(string.directory.cluster,'count/'),
               file.name = paste0(string.hazard.name,'_count_',year),
               var.name = 'event_id',
               long.name = 'event_id',
               unit = 'event_id')
      
      # unlist and stack result 
      var.intensity <- terra::rast(unlist(var.cluster.raster))
      
      # remove layers without values
      # 2. Identify layers that are entirely NaN
      # Use sapply to check each layer for all NaN values
      nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
        all(is.nan(values(var.intensity[[i]])))
      })
      
      if (sum(nan_layers) > 0){
        
        # 3. Keep only layers that are not entirely NaN
        # Subset the SpatRaster to keep non-NaN layers
        var.intensity <- var.intensity[[!nan_layers]]
        
      } else {}
      
      # Save output
      save.cdf(raster = var.intensity,
               folder.path = paste0(string.directory.cluster,'intensity/'),
               file.name = paste0(string.hazard.name,'_intensity_',year),
               var.name = string.hazard.name,
               long.name = string.hazard.name,
               unit = string.variable.unit)
      
      # Convert intensity to binary raster
      var.binary <- var.intensity / var.intensity
      
      # Save output
      save.cdf(raster = var.binary,
               folder.path = paste0(string.directory.cluster,'binary/'),
               file.name = paste0(string.hazard.name,'_binary_',year),
               var.name = string.hazard.name,
               long.name = string.hazard.name,
               unit = 'boolean')
      
      # Compile dbscan parameter dataframe
      dbscan.parameters.range <- data.frame(year = year,
                                            mu = var.kNN$mu,
                                            epsilon = var.kNN$epsilon,
                                            event.count = length(var.cluster.raster))
      
      # remove na rows
      dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]
      
      # Save resulting dataframe
      write.csv(dbscan.parameters.range,
                file = paste0(string.directory.cluster,'parameters/dbscan_parameters_',year,
                              '.csv'))
      
    } else {print(paste0('No clusters in ',year))}
    
    # time
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
  }
  
}

cluster.compile.4 <- function(raster.threshold.criteria,vector.year.range,
                              string.hazard.name,string.directory.variable.raw,
                              string.directory.variable.boolean,string.variable.unit,
                              numeric.space.time.metric,string.directory.cluster){
  
  # # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_warning_1_hour_20240925.nc')
  # vector.year.range = seq(2019,2023,1)
  # string.hazard.name = "Excessive_Heat_Warning"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/heat_index/warning/"
  # numeric.space.time.metric = data.frame(alpha = c(0.75,0.25),range = c(50,1000),tau = c(1000,20))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/"
  # string.variable.unit = "C"
  
  # # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/precipitation_001_year_24_hour_20240925.nc')
  # vector.year.range = seq(1963,1963,1)
  # string.hazard.name = "Precipitation"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/precipitation/001/"
  # numeric.space.time.metric = data.frame(alpha = c(0.99,0.01),range = c(12,20),tau = c(4,4))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/precipitation/001/"
  # string.variable.unit = "mm"
  
  # code
  for (year in vector.year.range) {
    
    # year <- 1961
    
    # print statement
    print(paste0('-------------------- ',string.hazard.name,' ',year,' --------------------'))
    
    # time
    start.time <- Sys.time()
    print(start.time)
    
    # Load regular Raster
    var <- raster.compile(year = as.character(year),directory = string.directory.variable.raw)
    
    # Load Boolean Raster
    var.bool <- raster.compile(year = as.character(year),directory = string.directory.variable.boolean)
    # plot(sum(var.bool))
    
    # Crop
    var <- raster.crop.mask(var,raster.threshold.criteria)
    
    # Correct Extent
    terra::ext(var) <- terra::ext(raster.threshold.criteria)
    terra::ext(var.bool) <- terra::ext(raster.threshold.criteria)
    
    # Cluster (10 min)
    # DBSCAN - covariance model parameters obtained from BMElib Matlab code
    var.cov.model <- numeric.space.time.metric
    
    # DBSCAN - space time metric: result is in (# hours / 0.25 spatial degree)
    # no longer need to multiply by 4 to get # hours / 0.25 spatial degree (ERA5 units)
    var.st.metric <- cov.ratio(var.cov.model) 
    
    # round to nearest whole number
    var.st.metric <- round(var.st.metric,0)
    
    # change st.metric to 1 if below 1
    if (var.st.metric[1] < 1){
      var.st.metric[1] <- 1
    }
    
    if (var.st.metric[2] < 1){
      var.st.metric[2] <- 1
    }
    
    # DBSCAN - create space time cube (1 min)
    var.cube <- cluster.lat.long.date.2(var.bool)
    
    # Only continue if enough extreme points
    if (nrow(var.cube >= 4)){
      
      # create empty list
      var.cluster.raster.list <- list()
      
      # recursively cluster for each compressed row
      for (compressed in 1:var.st.metric[1,1]){
        
        # compressed <- 1
        
        # DBSCAN - incorporate space time metric
        var.st.cube <- cluster.st.cube.2(var.cube,var.st.metric[1],row.start = compressed)
        
        # DBSCAN - correct order of var.st.cube df
        var.st.cube <- var.st.cube[,-1]
        
        # Test if enough points to continue
        if (nrow(var.st.cube) >= 4){
          
          # DBSCAN - kth nearest neighbor analysis to return epsilon and mu
          var.kNN <- cluster.kNN.2(var.cube[,c(2,4,5)]) # var.st.cube
          
          # DBSCAN - cluster for defined mu and epsilon
          var.cluster <- cluster.extremes.2(raster.cube.df = var.st.cube, # var.st.cube
                                            raster.bool = raster.bool,
                                            raster.var = var,
                                            raster.kNN.df = var.kNN)
          
          # DBSCAN - get raster for each cluster
          var.cluster.raster <- cluster.rasters.4(clusters.df = var.cluster[[1]],
                                                  raster.var = var,
                                                  cluster.st.metric = var.st.metric[1])
          
          # See if any clusters
          if (!is.null(var.cluster.raster)){
            
            # store in list object
            var.cluster.raster.list[[compressed]] <- var.cluster.raster 
            
          } else {}
          
          
          
        } else {print(paste0('No clusters in ',year,' for compressed time slice # ',compressed))}
        
      }
      
      if (length(var.cluster.raster.list) > 0){
        
        # unlist 
        var.cluster.raster <- unlist(var.cluster.raster.list)
        
        # calculate metadata
        var.cluster.raster.metadata <- cluster.metadata.2(var.cluster.raster,var)
        
        # save metadata
        write.csv(var.cluster.raster.metadata,
                  file = paste0(string.directory.cluster,'metadata/event_metadata_',year,
                                '.csv'))
        
        # Get max extent per cluster event 
        event.extent <- list()
        event.count <- list()
        
        # get each spatraster list item
        for (cluster.index in 1:length(var.cluster.raster)) {
          
          # subset
          event.extent.temp <- var.cluster.raster[[cluster.index]]
          
          # binarize
          event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
          
          # give new values, which are 'year.event_number' format
          event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
          
          # loop through time steps of one event 
          for (q in 1:terra::nlyr(event.extent.temp)) {
            
            # give final extent and value to time range 
            event.extent.temp[[q]] <- event.extent.binary
            
          }
          
          # give time back
          terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
          
          # rename layers
          names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
          
          # update list
          event.extent[[cluster.index]] <- event.extent.temp
          
          # update list with first spatial extent per event
          event.count[[cluster.index]] <- event.extent.temp[[1]]
          
        }
        
        # unlist
        event.extent <- terra::rast(unlist(event.extent))
        
        # give time as names to clusters
        names(event.extent) <- terra::time(event.extent)
        
        # calculate daily extent, in cases where multiple clusters occur at the same time
        event.extent <- terra::tapp(event.extent,'days',fun=max)
        
        # normalize extent as binary when overlapping produces 2+ extents in a location
        event.extent <- event.extent / event.extent
        
        # Save output
        save.cdf(raster = event.extent,
                 folder.path = paste0(string.directory.cluster,'extent/'),
                 file.name = paste0(string.hazard.name,'_extent_',year),
                 var.name = 'event_id',
                 long.name = 'event_id',
                 unit = 'event_id')
        
        # unlist
        event.count <- terra::rast(unlist(event.count))
        
        # give time as names to clusters
        names(event.count) <- terra::time(event.count)
        
        # calculate daily extent, in cases where multiple clusters occur at the same time
        event.count <- terra::tapp(event.count,'days',fun=max)
        
        # normalize extent as binary when overlapping produces 2+ extents in a location
        event.count <- event.count / event.count
        
        # Save output
        save.cdf(raster = event.count,
                 folder.path = paste0(string.directory.cluster,'count/'),
                 file.name = paste0(string.hazard.name,'_count_',year),
                 var.name = 'event_id',
                 long.name = 'event_id',
                 unit = 'event_id')
        
        # unlist and stack result 
        var.intensity <- terra::rast(unlist(var.cluster.raster))
        
        # remove layers without values
        # 2. Identify layers that are entirely NaN
        # Use sapply to check each layer for all NaN values
        nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
          all(is.nan(values(var.intensity[[i]])))
        })
        
        if (sum(nan_layers) > 0){
          
          # 3. Keep only layers that are not entirely NaN
          # Subset the SpatRaster to keep non-NaN layers
          var.intensity <- var.intensity[[!nan_layers]]
          
        } else {}
        
        # Save output
        save.cdf(raster = var.intensity,
                 folder.path = paste0(string.directory.cluster,'intensity/'),
                 file.name = paste0(string.hazard.name,'_intensity_',year),
                 var.name = string.hazard.name,
                 long.name = string.hazard.name,
                 unit = string.variable.unit)
        
        # Convert intensity to binary raster
        var.binary <- var.intensity / var.intensity
        
        # Save output
        save.cdf(raster = var.binary,
                 folder.path = paste0(string.directory.cluster,'binary/'),
                 file.name = paste0(string.hazard.name,'_binary_',year),
                 var.name = string.hazard.name,
                 long.name = string.hazard.name,
                 unit = 'boolean')
        
        # Compile dbscan parameter dataframe
        dbscan.parameters.range <- data.frame(year = year,
                                              mu = var.kNN$mu,
                                              epsilon = var.kNN$epsilon,
                                              event.count = length(var.cluster.raster))
        
        # remove na rows
        dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]
        
        # Save resulting dataframe
        write.csv(dbscan.parameters.range,
                  file = paste0(string.directory.cluster,'parameters/dbscan_parameters_',year,
                                '.csv'))
        
      } else {print(paste0('No clusters in ',year))}
      
    } else {print(paste0('No clusters in ',year))}
    
    # time
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
  }
  
}

cluster.compile.5 <- function(raster.threshold.criteria,vector.year.range,
                              string.hazard.name,string.directory.variable.raw,
                              string.directory.variable.boolean,string.variable.unit,
                              numeric.space.time.metric,string.directory.cluster){
  
  # debug
  raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_warning_1_hour_20240925.nc')
  vector.year.range = seq(2019,2023,1)
  string.hazard.name = "Excessive_Heat_Warning"
  string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/1-day/"
  string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/heat_index/warning/"
  numeric.space.time.metric = data.frame(alpha = c(0.75,0.25),range = c(50,1000),tau = c(1000,20))
  string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/"
  string.variable.unit = "C"
  
  # # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/precipitation_001_year_24_hour_20240925.nc')
  # vector.year.range = seq(1963,1963,1)
  # string.hazard.name = "Precipitation"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/precipitation/001/"
  # numeric.space.time.metric = data.frame(alpha = c(0.99,0.01),range = c(12,20),tau = c(4,4))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/precipitation/001/"
  # string.variable.unit = "mm"
  
  # code
  for (year in vector.year.range) {
    
    year <- 1953
    
    # print statement
    print(paste0('-------------------- ',string.hazard.name,' ',year,' --------------------'))
    
    # time
    start.time <- Sys.time()
    print(start.time)
    
    # Load regular Raster
    var <- raster.compile(year = as.character(year),directory = string.directory.variable.raw)
    
    # Load Boolean Raster
    var.bool <- raster.compile(year = as.character(year),directory = string.directory.variable.boolean)
    
    # Crop
    var <- raster.crop.mask(var,raster.threshold.criteria)
    
    # Correct Extent
    terra::ext(var) <- terra::ext(raster.threshold.criteria)
    terra::ext(var.bool) <- terra::ext(raster.threshold.criteria)
    
    # Cluster (10 min)
    # DBSCAN - covariance model parameters obtained from BMElib Matlab code
    var.cov.model <- numeric.space.time.metric
    
    # DBSCAN - space time metric: result is in (# hours / 0.25 spatial degree)
    # no longer need to multiply by 4 to get # hours / 0.25 spatial degree (ERA5 units)
    var.st.metric <- cov.ratio(var.cov.model) 
    
    # round to nearest whole number
    var.st.metric <- round(var.st.metric,0)
    
    # change st.metric to 1 if below 1
    if (var.st.metric[1] < 1){
      var.st.metric[1] <- 1
    }
    
    if (var.st.metric[2] < 1){
      var.st.metric[2] <- 1
    }
    
    # DBSCAN - create space time cube (1 min)
    var.cube <- cluster.lat.long.date.2(var.bool)
    
    # Only continue if enough extreme points
    if (nrow(var.cube >= 4)){
      
      # create empty list
      var.cluster.raster.list <- list()
      
      # recursively cluster for each compressed row
      for (compressed in 1:var.st.metric[1,1]){
        
        # compressed <- 1
        
        # DBSCAN - incorporate space time metric
        var.st.cube <- cluster.st.cube.2(var.cube,var.st.metric[1],row.start = compressed)
        
        # DBSCAN - correct order of var.st.cube df
        var.st.cube <- var.st.cube[,-1]
        
        # Test if enough points to continue
        if (nrow(var.st.cube) >= 4){
          
          # DBSCAN - kth nearest neighbor analysis to return epsilon and mu
          var.kNN <- cluster.kNN.2(var.cube[,c(2,4,5)]) # var.st.cube
          
          # DBSCAN - cluster for defined mu and epsilon
          var.cluster <- cluster.extremes.2(raster.cube.df = var.st.cube, # var.st.cube
                                            raster.bool = raster.bool,
                                            raster.var = var,
                                            raster.kNN.df = var.kNN)
          
          # DBSCAN - get raster for each cluster
          var.cluster.raster <- cluster.rasters.4(clusters.df = var.cluster[[1]],
                                                  raster.var = var,
                                                  cluster.st.metric = var.st.metric[1])
          
          # See if any clusters
          if (!is.null(var.cluster.raster)){
            
            # store in list object
            var.cluster.raster.list[[compressed]] <- var.cluster.raster 
            
          } else {}
          
        } else {print(paste0('No clusters in ',year,' for compressed time slice # ',compressed))}
        
      }
      
      # Only continue if list has clusters in it, as opposed to only noise points 
      if (length(var.cluster.raster.list) > 0){
        
        # unlist 
        var.cluster.raster <- unlist(var.cluster.raster.list)
        
        # 2024-10-16 ---
        # Create new list
        var.cluster.raster.list.boolean <- list()
        
        for (list.index in 1:length(var.cluster.raster)){
          
          # print(list.index)
          
          # create temporary raster
          var.temp <- var.cluster.raster[[list.index]]
          
          # convert 0 to na
          var.temp[var.temp == 0] <- NA
          
          # create boolean
          var.cluster.raster.list.boolean[[list.index]] <- var.temp / var.temp
          
        }
        
        # 2024-10-17 ---
        # Tried ChatGPT to no avail
        # 2024-10-17 ---
        
        # special case for single cluster
        if (length(var.cluster.raster.list.boolean) > 1){
          
          # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 1 ---
          # HCQ EVENTUALLY NEEDS TO TURN THIS SECTION INTO A FUNCTION BASED ON STM INPUT VALUE
          
          # Create new list
          var.cluster.raster.list.combined <- list()
          combined.index <- 1
          
          # Combine overlapping clusters
          # Test whether sum of two boolean clusters > 1
          for (list.index in 1:length(var.cluster.raster.list.boolean)){
            
            # create new list
            confirmation.list <- list()
            confirmation.index <- 1
            
            # print(list.index)
            
            # test if sum of two rasters > 1
            var.temp.test <- var.cluster.raster.list.boolean[[list.index]]
            
            # look through other list items for comparison to var.temp.test
            for (temp.index in 1:length(var.cluster.raster.list.boolean)){
              
              if (temp.index != list.index){
                
                # get var.temp.compare
                var.temp.compare <- var.cluster.raster.list.boolean[[temp.index]]
                
                # test whether overlapping date strings
                var.temp.test.time <- lubridate::yday(terra::time(var.temp.test))
                var.temp.compare.time <- lubridate::yday(terra::time(var.temp.compare))
                
                if (any(var.temp.test.time %in% var.temp.compare.time)){
                  
                  # merge the two rasters
                  var.temp.combine <- terra::sds(var.temp.test,var.temp.compare) # HCQ leftoff here
                  var.temp.combine <- terra::mergeTime(var.temp.combine,fun=sum)
                  
                  # test if max of sum of two rasters is not NA
                  if (!is.na(max(terra::minmax(var.temp.combine)))){
                    
                    # test if max of sum of two rasters > 1
                    if (max(terra::minmax(var.temp.combine)) > 1){
                      
                      # return to boolean
                      var.temp.combine <- var.temp.combine / var.temp.combine
                      
                      # subset native raster to newly merged time string
                      raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine))),
                                           max(lubridate::yday(terra::time(var.temp.combine))),1)
                      var.temp.subset <- var[[raster.subset]]
                      
                      # multiply by native raster
                      var.temp.combine <- var.temp.combine * var.temp.subset
                      
                      # append to list item
                      confirmation.list[[confirmation.index]] <- var.temp.combine
                      
                      # update index
                      confirmation.index <- confirmation.index + 1
                      
                    } else {}
                    
                  } else {}
                  
                } else {}
                
              } else {}
              
            }
            
                        # take newly created list, convert to sds, repeat combination assessment, put one final raster into final list
            if (length(confirmation.list) > 0){
              
              if (length(confirmation.list) > 1){
                
                # merge the x# of rasters
                var.temp.combine.final <- terra::sds(confirmation.list)
                var.temp.combine.final <- terra::mergeTime(var.temp.combine.final,fun=max)
                
                # append to list
                var.cluster.raster.list.combined[[combined.index]] <- var.temp.combine.final
                
                # update counter
                combined.index <- combined.index + 1
                
              } else {
                
                # append to list
                var.cluster.raster.list.combined[[combined.index]] <- confirmation.list[[1]]
                
                # update counter
                combined.index <- combined.index + 1
                
              }
              
            } else {}
            
          }
          
          # append unaltered clusters to list
          var.cluster.raster.list.combined <- append(var.cluster.raster.list.combined,
                                                     var.cluster.raster.list.boolean,
                                                     after=length(var.cluster.raster.list.combined))
          
          # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 2 ---
          
          # only do this part if multiple clusters
          
          # Create new list
          var.cluster.raster.list.combined.2 <- list()
          combined.index <- 1
          
          # Combine overlapping clusters
          # Test whether sum of two boolean clusters > 1
          for (list.index in 1:length(var.cluster.raster.list.combined)){
            
            # create new list
            confirmation.list <- list()
            confirmation.index <- 1
            
            # print(list.index)
            
            # test if sum of two rasters > 1
            var.temp.test <- var.cluster.raster.list.combined[[list.index]]
            
            # look through other list items for comparison to var.temp.test
            for (temp.index in 1:length(var.cluster.raster.list.combined)){
              
              # temp.index <- 1
              
              if (temp.index != list.index){
                
                # get var.temp.compare
                var.temp.compare <- var.cluster.raster.list.combined[[temp.index]]
                
                # test whether overlapping date strings
                var.temp.test.time <- lubridate::yday(terra::time(var.temp.test))
                var.temp.compare.time <- lubridate::yday(terra::time(var.temp.compare))
                
                if (any(var.temp.test.time %in% var.temp.compare.time)){
                  
                  # merge the two rasters
                  var.temp.combine <- terra::sds(var.temp.test,var.temp.compare) 
                  var.temp.combine <- terra::mergeTime(var.temp.combine,fun=sum)
                  
                  # test if max of sum of two rasters is not NA
                  if (!is.na(max(terra::minmax(var.temp.combine)))){
                    
                    # test if max of sum of two rasters > 1
                    if (max(terra::minmax(var.temp.combine)) > 1){
                      
                      # return to boolean
                      var.temp.combine <- var.temp.combine / var.temp.combine
                      
                      # subset native raster to newly merged time string
                      raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine))),
                                           max(lubridate::yday(terra::time(var.temp.combine))),1)
                      var.temp.subset <- var[[raster.subset]]
                      
                      # multiply by native raster
                      var.temp.combine <- var.temp.combine * var.temp.subset
                      
                      # append to list item
                      confirmation.list[[confirmation.index]] <- var.temp.combine
                      
                      # update index
                      confirmation.index <- confirmation.index + 1
                      
                    } else {}
                    
                  } else {}
                  
                } else {}
                
              } else {}
              
            }
            
            # take newly created list, convert to sds, repeat combination assessment, put one final raster into final list
            if (length(confirmation.list) > 0){
              
              if (length(confirmation.list) > 1){
                
                # merge the x# of rasters
                var.temp.combine.final <- terra::sds(confirmation.list)
                var.temp.combine.final <- terra::mergeTime(var.temp.combine.final,fun=max)
                
                # append to list
                var.cluster.raster.list.combined.2[[combined.index]] <- var.temp.combine.final
                
                # update counter
                combined.index <- combined.index + 1
                
              } else {
                
                # append to list
                var.cluster.raster.list.combined.2[[combined.index]] <- confirmation.list[[1]]
                
                # update counter
                combined.index <- combined.index + 1
                
              }
              
            } else {}
            
          }
          
          # append unaltered clusters to list
          var.cluster.raster.list.combined.2 <- append(var.cluster.raster.list.combined.2,
                                                     var.cluster.raster.list.boolean,
                                                     after=length(var.cluster.raster.list.combined.2))
          
          # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 1 ---
          
          # test if two list items are equal, if so, remove additional
          var.cluster.raster.list.combined.clean <- var.cluster.raster.list.combined.2
          
          # go through list, remove another layer if it is equal to layer in question
          for (are.equal in 1:length(var.cluster.raster.list.combined.2)){
            
            for (perhaps in 1:length(var.cluster.raster.list.combined.2)){
              
              if (are.equal != perhaps){
                
                if (perhaps > are.equal){
                  
                  if (terra::nlyr(var.cluster.raster.list.combined.2[[are.equal]]) == 
                      terra::nlyr(var.cluster.raster.list.combined.2[[perhaps]])) {
                    
                    result <- terra::all.equal(var.cluster.raster.list.combined.2[[are.equal]],
                                               var.cluster.raster.list.combined.2[[perhaps]]) != TRUE
                    
                    if (result[1] == T){}
                    
                    else {
                      
                      # remove perhaps layer
                      var.cluster.raster.list.combined.clean <- var.cluster.raster.list.combined.clean[-perhaps]
                      
                    }
                    
                  } else {}
                  
                } else {}
                
              } else {}
              
            }
            
          }
          
          # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 2 ---
          
         # test if two list items are equal, if so, remove additional
          var.cluster.raster.list.combined.clean.2 <- var.cluster.raster.list.combined.clean
          
          # go through list, remove another layer if it is equal to layer in question
          for (are.equal in 1:length(var.cluster.raster.list.combined.clean)){
            
            for (perhaps in 1:length(var.cluster.raster.list.combined.clean)){
              
              if (are.equal != perhaps){
                
                if (perhaps > are.equal){
                  
                  if (terra::nlyr(var.cluster.raster.list.combined.clean[[are.equal]]) == 
                      terra::nlyr(var.cluster.raster.list.combined.clean[[perhaps]])) {
                    
                    result <- terra::all.equal(var.cluster.raster.list.combined.clean[[are.equal]],
                                               var.cluster.raster.list.combined.clean[[perhaps]]) != TRUE
                    
                    if (result[1] == T){}
                    
                    else {
                      
                      # remove perhaps layer
                      var.cluster.raster.list.combined.clean.2 <- var.cluster.raster.list.combined.clean[-perhaps]
                      
                    }
                    
                  } else {}
                  
                } else {}
                
              } else {}
              
            }
            
          }
          
          # rename list
          var.cluster.raster <- var.cluster.raster.list.combined.clean.2
          
          
          # give back actual values
          for (final.layer in 1:length(var.cluster.raster)){
            
            # get layer
            var.temp.combine <- var.cluster.raster[[final.layer]]
            
            # subset native raster to newly merged time string
            raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine))),
                                 max(lubridate::yday(terra::time(var.temp.combine))),1)
            var.temp.subset <- var[[raster.subset]]
            
            # multiply by native raster
            var.cluster.raster[[final.layer]] <- var.temp.combine * var.temp.subset
            
          }
          
          # 2024-10-16 ---
          
          # calculate metadata
          var.cluster.raster.metadata <- cluster.metadata.2(var.cluster.raster,var)
          
          # save metadata
          write.csv(var.cluster.raster.metadata,
                    file = paste0(string.directory.cluster,'metadata/event_metadata_',year,
                                  '.csv'))
          
          # Get max extent per cluster event 
          event.extent <- list()
          event.count <- list()
          
          # get each spatraster list item
          for (cluster.index in 1:length(var.cluster.raster)) {
            
            # subset
            event.extent.temp <- var.cluster.raster[[cluster.index]]
            
            # binarize
            event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
            
            # give new values, which are 'year.event_number' format
            event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
            
            # loop through time steps of one event 
            for (q in 1:terra::nlyr(event.extent.temp)) {
              
              # give final extent and value to time range 
              event.extent.temp[[q]] <- event.extent.binary
              
            }
            
            # give time back
            terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
            
            # rename layers
            names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
            
            # update list
            event.extent[[cluster.index]] <- event.extent.temp
            
            # update list with first spatial extent per event
            event.count[[cluster.index]] <- event.extent.temp[[1]]
            
          }
          
          # unlist
          event.extent <- terra::rast(unlist(event.extent))
          
          # give time as names to clusters
          names(event.extent) <- terra::time(event.extent)
          
          # calculate daily extent, in cases where multiple clusters occur at the same time
          event.extent <- terra::tapp(event.extent,'days',fun=max)
          
          # normalize extent as binary when overlapping produces 2+ extents in a location
          event.extent <- event.extent / event.extent
          
          # Save output
          save.cdf(raster = event.extent,
                   folder.path = paste0(string.directory.cluster,'extent/'),
                   file.name = paste0(string.hazard.name,'_extent_',year),
                   var.name = 'event_id',
                   long.name = 'event_id',
                   unit = 'event_id')
          
          # unlist
          event.count <- terra::rast(unlist(event.count))
          
          # give time as names to clusters
          names(event.count) <- terra::time(event.count)
          
          # calculate daily extent, in cases where multiple clusters occur at the same time
          event.count <- terra::tapp(event.count,'days',fun=max)
          
          # normalize extent as binary when overlapping produces 2+ extents in a location
          event.count <- event.count / event.count
          
          # Save output
          save.cdf(raster = event.count,
                   folder.path = paste0(string.directory.cluster,'count/'),
                   file.name = paste0(string.hazard.name,'_count_',year),
                   var.name = 'event_id',
                   long.name = 'event_id',
                   unit = 'event_id')
          
          # unlist and stack result 
          var.intensity <- terra::rast(unlist(var.cluster.raster))
          
          # remove layers without values
          # 2. Identify layers that are entirely NaN
          # Use sapply to check each layer for all NaN values
          nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
            all(is.nan(values(var.intensity[[i]])))
          })
          
          if (sum(nan_layers) > 0){
            
            # 3. Keep only layers that are not entirely NaN
            # Subset the SpatRaster to keep non-NaN layers
            var.intensity <- var.intensity[[!nan_layers]]
            
          } else {}
          
          # Save output
          save.cdf(raster = var.intensity,
                   folder.path = paste0(string.directory.cluster,'intensity/'),
                   file.name = paste0(string.hazard.name,'_intensity_',year),
                   var.name = string.hazard.name,
                   long.name = string.hazard.name,
                   unit = string.variable.unit)
          
          # Convert intensity to binary raster
          var.binary <- var.intensity / var.intensity
          
          # Save output
          save.cdf(raster = var.binary,
                   folder.path = paste0(string.directory.cluster,'binary/'),
                   file.name = paste0(string.hazard.name,'_binary_',year),
                   var.name = string.hazard.name,
                   long.name = string.hazard.name,
                   unit = 'boolean')
          
          # Compile dbscan parameter dataframe
          dbscan.parameters.range <- data.frame(year = year,
                                                mu = var.kNN$mu,
                                                epsilon = var.kNN$epsilon,
                                                event.count = length(var.cluster.raster))
          
          # remove na rows
          dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]
          
          # Save resulting dataframe
          write.csv(dbscan.parameters.range,
                    file = paste0(string.directory.cluster,'parameters/dbscan_parameters_',year,
                                  '.csv'))
          
        } else {
          
          # Just multiply bool by native raster subset and then calculate everything
          # return to boolean
          var.temp.combine <- var.cluster.raster.list.boolean[[1]] / var.cluster.raster.list.boolean[[1]]
          
          # subset native raster to newly merged time string
          raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine[[1]]))),
                               max(lubridate::yday(terra::time(var.temp.combine[[1]]))),1)
          var.temp.subset <- var[[raster.subset]]
          
          # multiply by native raster
          var.temp.combine <- var.temp.combine * var.temp.subset
          
          # Move below to else statement
          
          # Do everything you would normally do
          # calculate metadata
          var.cluster.raster.metadata <- cluster.metadata.2(var.cluster.raster,var)
          
          # save metadata
          write.csv(var.cluster.raster.metadata,
                    file = paste0(string.directory.cluster,'metadata/event_metadata_',year,
                                  '.csv'))
          
          # Get max extent per cluster event 
          event.extent <- list()
          event.count <- list()
          
          # get each spatraster list item
          for (cluster.index in 1:length(var.cluster.raster)) {
            
            # subset
            event.extent.temp <- var.cluster.raster[[cluster.index]]
            
            # binarize
            event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
            
            # give new values, which are 'year.event_number' format
            event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
            
            # loop through time steps of one event 
            for (q in 1:terra::nlyr(event.extent.temp)) {
              
              # give final extent and value to time range 
              event.extent.temp[[q]] <- event.extent.binary
              
            }
            
            # give time back
            terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
            
            # rename layers
            names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
            
            # update list
            event.extent[[cluster.index]] <- event.extent.temp
            
            # update list with first spatial extent per event
            event.count[[cluster.index]] <- event.extent.temp[[1]]
            
          }
          
          # unlist
          event.extent <- terra::rast(unlist(event.extent))
          
          # give time as names to clusters
          names(event.extent) <- terra::time(event.extent)
          
          # calculate daily extent, in cases where multiple clusters occur at the same time
          event.extent <- terra::tapp(event.extent,'days',fun=max)
          
          # normalize extent as binary when overlapping produces 2+ extents in a location
          event.extent <- event.extent / event.extent
          
          # Save output
          save.cdf(raster = event.extent,
                   folder.path = paste0(string.directory.cluster,'extent/'),
                   file.name = paste0(string.hazard.name,'_extent_',year),
                   var.name = 'event_id',
                   long.name = 'event_id',
                   unit = 'event_id')
          
          # unlist
          event.count <- terra::rast(unlist(event.count))
          
          # give time as names to clusters
          names(event.count) <- terra::time(event.count)
          
          # calculate daily extent, in cases where multiple clusters occur at the same time
          event.count <- terra::tapp(event.count,'days',fun=max)
          
          # normalize extent as binary when overlapping produces 2+ extents in a location
          event.count <- event.count / event.count
          
          # Save output
          save.cdf(raster = event.count,
                   folder.path = paste0(string.directory.cluster,'count/'),
                   file.name = paste0(string.hazard.name,'_count_',year),
                   var.name = 'event_id',
                   long.name = 'event_id',
                   unit = 'event_id')
          
          # unlist and stack result 
          var.intensity <- terra::rast(unlist(var.cluster.raster))
          
          # remove layers without values
          # 2. Identify layers that are entirely NaN
          # Use sapply to check each layer for all NaN values
          nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
            all(is.nan(values(var.intensity[[i]])))
          })
          
          if (sum(nan_layers) > 0){
            
            # 3. Keep only layers that are not entirely NaN
            # Subset the SpatRaster to keep non-NaN layers
            var.intensity <- var.intensity[[!nan_layers]]
            
          } else {}
          
          # Save output
          save.cdf(raster = var.intensity,
                   folder.path = paste0(string.directory.cluster,'intensity/'),
                   file.name = paste0(string.hazard.name,'_intensity_',year),
                   var.name = string.hazard.name,
                   long.name = string.hazard.name,
                   unit = string.variable.unit)
          
          # Convert intensity to binary raster
          var.binary <- var.intensity / var.intensity
          
          # Save output
          save.cdf(raster = var.binary,
                   folder.path = paste0(string.directory.cluster,'binary/'),
                   file.name = paste0(string.hazard.name,'_binary_',year),
                   var.name = string.hazard.name,
                   long.name = string.hazard.name,
                   unit = 'boolean')
          
          # Compile dbscan parameter dataframe
          dbscan.parameters.range <- data.frame(year = year,
                                                mu = var.kNN$mu,
                                                epsilon = var.kNN$epsilon,
                                                event.count = length(var.cluster.raster))
          
          # remove na rows
          dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]
          
          # Save resulting dataframe
          write.csv(dbscan.parameters.range,
                    file = paste0(string.directory.cluster,'parameters/dbscan_parameters_',year,
                                  '.csv'))
          
          }
        
      } else {print(paste0('No clusters in ',year))}
      
    } else {print(paste0('No clusters in ',year))}
    
    # time
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
  }
  
}

# cluster.compile.6 <- function(raster.threshold.criteria,vector.year.range,
#                               string.hazard.name,string.directory.variable.raw,
#                               string.directory.variable.boolean,string.variable.unit,
#                               numeric.space.time.metric,string.directory.cluster){
#   
#   # debug
#   raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_warning_1_hour_20240925.nc')
#   vector.year.range = seq(2019,2023,1)
#   string.hazard.name = "Excessive_Heat_Warning"
#   string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/1-day/"
#   string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/heat_index/warning/"
#   numeric.space.time.metric = data.frame(alpha = c(0.75,0.25),range = c(50,1000),tau = c(1000,20))
#   string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/"
#   string.variable.unit = "C"
#   
#   # # debug
#   # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/precipitation_001_year_24_hour_20240925.nc')
#   # vector.year.range = seq(1963,1963,1)
#   # string.hazard.name = "Precipitation"
#   # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/1-day/"
#   # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/precipitation/001/"
#   # numeric.space.time.metric = data.frame(alpha = c(0.99,0.01),range = c(12,20),tau = c(4,4))
#   # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/precipitation/001/"
#   # string.variable.unit = "mm"
#   
#   # code
#   for (year in vector.year.range) {
#     
#     year <- 1953
#     
#     # print statement
#     print(paste0('-------------------- ',string.hazard.name,' ',year,' --------------------'))
#     
#     # time
#     start.time <- Sys.time()
#     print(start.time)
#     
#     # Load regular Raster
#     var <- raster.compile(year = as.character(year),directory = string.directory.variable.raw)
#     
#     # Load Boolean Raster
#     var.bool <- raster.compile(year = as.character(year),directory = string.directory.variable.boolean)
#     
#     # Crop
#     var <- raster.crop.mask(var,raster.threshold.criteria)
#     
#     # Correct Extent
#     terra::ext(var) <- terra::ext(raster.threshold.criteria)
#     terra::ext(var.bool) <- terra::ext(raster.threshold.criteria)
#     
#     # Cluster (10 min)
#     # DBSCAN - covariance model parameters obtained from BMElib Matlab code
#     var.cov.model <- numeric.space.time.metric
#     
#     # DBSCAN - space time metric: result is in (# hours / 0.25 spatial degree)
#     # no longer need to multiply by 4 to get # hours / 0.25 spatial degree (ERA5 units)
#     var.st.metric <- cov.ratio(var.cov.model) 
#     
#     # round to nearest whole number
#     var.st.metric <- round(var.st.metric,0)
#     
#     # change st.metric to 1 if below 1
#     if (var.st.metric[1] < 1){
#       var.st.metric[1] <- 1
#     }
#     
#     if (var.st.metric[2] < 1){
#       var.st.metric[2] <- 1
#     }
#     
#     # DBSCAN - create space time cube (1 min)
#     var.cube <- cluster.lat.long.date.2(var.bool)
#     
#     # Only continue if enough extreme points
#     if (nrow(var.cube) < 4){print(paste0('No clusters in ',year))}
#     
#     # Able to continue
#     else {
#       
#       # create empty list
#       var.cluster.raster.list <- list()
#       
#       # recursively cluster for each compressed row
#       for (compressed in 1:var.st.metric[1,1]){
#         
#         # compressed <- 1
#         
#         # DBSCAN - incorporate space time metric
#         var.st.cube <- cluster.st.cube.2(var.cube,var.st.metric[1],row.start = compressed)
#         
#         # DBSCAN - correct order of var.st.cube df
#         var.st.cube <- var.st.cube[,-1]
#         
#         # Test if enough points to continue
#         if (nrow(var.st.cube) >= 4){
#           
#           # DBSCAN - kth nearest neighbor analysis to return epsilon and mu
#           var.kNN <- cluster.kNN.2(var.cube[,c(2,4,5)]) # var.st.cube
#           
#           # DBSCAN - cluster for defined mu and epsilon
#           var.cluster <- cluster.extremes.2(raster.cube.df = var.st.cube, # var.st.cube
#                                             raster.bool = raster.bool,
#                                             raster.var = var,
#                                             raster.kNN.df = var.kNN)
#           
#           # DBSCAN - get raster for each cluster
#           var.cluster.raster <- cluster.rasters.4(clusters.df = var.cluster[[1]],
#                                                   raster.var = var,
#                                                   cluster.st.metric = var.st.metric[1])
#           
#           # See if any clusters
#           if (!is.null(var.cluster.raster)){
#             
#             # store in list object
#             var.cluster.raster.list[[compressed]] <- var.cluster.raster 
#             
#           } else {}
#           
#         } else {print(paste0('No clusters in ',year,' for compressed time slice # ',compressed))}
#         
#       }
#       
#       # Only continue if list has clusters in it, as opposed to only noise points 
#       if (length(var.cluster.raster.list) == 0){
#         
#         # print statement
#         print(paste0('There are no clusters in ',year))
#         
#       } else {
#         
#         # print statement
#         print(paste0('Converting to boolean'))
#         
#         # unlist 
#         var.cluster.raster <- unlist(var.cluster.raster.list)
#         
#         # Create new list
#         var.cluster.raster.list.boolean <- list()
#         
#         # convert clusters to booleans for combining
#         for (list.index in 1:length(var.cluster.raster)){
#           
#           # print(list.index)
#           
#           # create temporary raster
#           var.temp <- var.cluster.raster[[list.index]]
#           
#           # convert 0 to na
#           var.temp[var.temp == 0] <- NA
#           
#           # create boolean
#           var.cluster.raster.list.boolean[[list.index]] <- var.temp / var.temp
#           
#         }
#         
#         # print statement
#         print(paste0('There are ',length(var.cluster.raster.list.boolean),' cluster(s) in ',
#                      year,', but some may be duplicates or may need to be merged'))
#         
#         # this is a special case for single cluster
#         if (length(var.cluster.raster.list.boolean) == 1){} 
#         
#         else {
#           # this is a case where we have multiple clusters, and need to see 
#           # whether there are duplicates or clusters needing to be merged
#           
#           # Create new list
#           var.cluster.raster.list.combined <- list()
#           combined.index <- 1
#           
#           # Combine overlapping clusters
#           # Test whether sum of two boolean clusters > 1
#           for (list.index in 1:length(var.cluster.raster.list.boolean)){
#             
#             # list.index <- 1
#             
#             # create new list
#             confirmation.list <- list()
#             confirmation.index <- 1
#             
#             # print(list.index)
#             
#             # test if sum of two rasters > 1
#             var.temp.test <- var.cluster.raster.list.boolean[[list.index]]
#             
#             # look through other list items for comparison to var.temp.test
#             for (temp.index in 1:length(var.cluster.raster.list.boolean)){
#               
#               if (temp.index != list.index){
#                 
#                 # get var.temp.compare
#                 var.temp.compare <- var.cluster.raster.list.boolean[[temp.index]]
#                 
#                 # test whether overlapping date strings
#                 var.temp.test.time <- lubridate::yday(terra::time(var.temp.test))
#                 var.temp.compare.time <- lubridate::yday(terra::time(var.temp.compare))
#                 
#                 if (any(var.temp.test.time %in% var.temp.compare.time)){
#                   
#                   # merge the two rasters
#                   var.temp.combine <- terra::sds(var.temp.test,var.temp.compare) # HCQ leftoff here
#                   var.temp.combine <- terra::mergeTime(var.temp.combine,fun=sum)
#                   
#                   # test if max of sum of two rasters is not NA
#                   if (!is.na(max(terra::minmax(var.temp.combine)))){
#                     
#                     # test if max of sum of two rasters > 1
#                     if (max(terra::minmax(var.temp.combine)) > 1){
#                       
#                       # return to boolean
#                       var.temp.combine <- var.temp.combine / var.temp.combine
#                       
#                       # subset native raster to newly merged time string
#                       raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine))),
#                                            max(lubridate::yday(terra::time(var.temp.combine))),1)
#                       var.temp.subset <- var[[raster.subset]]
#                       
#                       # multiply by native raster
#                       var.temp.combine <- var.temp.combine * var.temp.subset
#                       
#                       # append to list item
#                       confirmation.list[[confirmation.index]] <- var.temp.combine
#                       
#                       # update index
#                       confirmation.index <- confirmation.index + 1
#                       
#                     } else {}
#                     
#                   } else {}
#                   
#                 } else {}
#                 
#               } else {}
#               
#             }
#             
#             # if nothing in confirmation list, that means there were no duplicates, 
#             # so we need to fill this position back in with the original value 
#             if (length(confirmation.list) == 0){
#               
#               # append with original value
#               confirmation.list[[1]] <- var.temp.test
#               
#             } else {}
#             
#             # take newly created list, convert to sds, repeat combination assessment, 
#             # put one final raster into final list
#             if (length(confirmation.list) == 1){
#               
#               # fill new list position with this value
#               var.cluster.raster.list.combined[[combined.index]] <- confirmation.list
#               
#             } else {
#               # This is the case where there are multiple overlapping clusters 
#               # that need to be combined
#               
#               # merge the x# of rasters
#               var.temp.combine.final <- terra::sds(confirmation.list)
#               var.temp.combine.final <- terra::mergeTime(var.temp.combine.final,fun=max)
#               
#               # append to list
#               var.cluster.raster.list.combined[[combined.index]] <- var.temp.combine.final
#               
#             }
#             
#           }
#           
#         }
#             
#             # Now let's update the index
#             combined.index <- combined.index + 1
#             
#             ----
#             
#             # take newly created list, convert to sds, repeat combination assessment, put one final raster into final list
#             if (length(confirmation.list) > 0){
#               
#               if (length(confirmation.list) > 1){
#                 
#                 
#                 
#                 # update counter
#                 combined.index <- combined.index + 1
#                 
#               } else {
#                 
#                 # append to list
#                 var.cluster.raster.list.combined[[combined.index]] <- confirmation.list[[1]]
#                 
#                 # update counter
#                 combined.index <- combined.index + 1
#                 
#               }
#               
#             } else {}
#             
#           }
#           
#         }
#         
#         # Turn booleans into measurements
#         
#         # Save metadata, final rasters
#         
#       }
#       
#     }
#     
# 
#     -------------------------------------------------------------------
#     
#     # Only continue if list has clusters in it, as opposed to only noise points 
#       if (length(var.cluster.raster.list) > 0){
#         
#                 # special case for single cluster
#         if (length(var.cluster.raster.list.boolean) > 1){
#           
#           # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 1 ---
#           # HCQ EVENTUALLY NEEDS TO TURN THIS SECTION INTO A FUNCTION BASED ON STM INPUT VALUE
#           
#           # Create new list
#           var.cluster.raster.list.combined <- list()
#           combined.index <- 1
#           
#           # Combine overlapping clusters
#           # Test whether sum of two boolean clusters > 1
#           for (list.index in 1:length(var.cluster.raster.list.boolean)){
#             
#             # create new list
#             confirmation.list <- list()
#             confirmation.index <- 1
#             
#             # print(list.index)
#             
#             # test if sum of two rasters > 1
#             var.temp.test <- var.cluster.raster.list.boolean[[list.index]]
#             
#             # look through other list items for comparison to var.temp.test
#             for (temp.index in 1:length(var.cluster.raster.list.boolean)){
#               
#               if (temp.index != list.index){
#                 
#                 # get var.temp.compare
#                 var.temp.compare <- var.cluster.raster.list.boolean[[temp.index]]
#                 
#                 # test whether overlapping date strings
#                 var.temp.test.time <- lubridate::yday(terra::time(var.temp.test))
#                 var.temp.compare.time <- lubridate::yday(terra::time(var.temp.compare))
#                 
#                 if (any(var.temp.test.time %in% var.temp.compare.time)){
#                   
#                   # merge the two rasters
#                   var.temp.combine <- terra::sds(var.temp.test,var.temp.compare) # HCQ leftoff here
#                   var.temp.combine <- terra::mergeTime(var.temp.combine,fun=sum)
#                   
#                   # test if max of sum of two rasters is not NA
#                   if (!is.na(max(terra::minmax(var.temp.combine)))){
#                     
#                     # test if max of sum of two rasters > 1
#                     if (max(terra::minmax(var.temp.combine)) > 1){
#                       
#                       # return to boolean
#                       var.temp.combine <- var.temp.combine / var.temp.combine
#                       
#                       # subset native raster to newly merged time string
#                       raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine))),
#                                            max(lubridate::yday(terra::time(var.temp.combine))),1)
#                       var.temp.subset <- var[[raster.subset]]
#                       
#                       # multiply by native raster
#                       var.temp.combine <- var.temp.combine * var.temp.subset
#                       
#                       # append to list item
#                       confirmation.list[[confirmation.index]] <- var.temp.combine
#                       
#                       # update index
#                       confirmation.index <- confirmation.index + 1
#                       
#                     } else {}
#                     
#                   } else {}
#                   
#                 } else {}
#                 
#               } else {}
#               
#             }
#             
#             # take newly created list, convert to sds, repeat combination assessment, put one final raster into final list
#             if (length(confirmation.list) > 0){
#               
#               if (length(confirmation.list) > 1){
#                 
#                 # merge the x# of rasters
#                 var.temp.combine.final <- terra::sds(confirmation.list)
#                 var.temp.combine.final <- terra::mergeTime(var.temp.combine.final,fun=max)
#                 
#                 # append to list
#                 var.cluster.raster.list.combined[[combined.index]] <- var.temp.combine.final
#                 
#                 # update counter
#                 combined.index <- combined.index + 1
#                 
#               } else {
#                 
#                 # append to list
#                 var.cluster.raster.list.combined[[combined.index]] <- confirmation.list[[1]]
#                 
#                 # update counter
#                 combined.index <- combined.index + 1
#                 
#               }
#               
#             } else {}
#             
#           }
#           
#           # append unaltered clusters to list
#           var.cluster.raster.list.combined <- append(var.cluster.raster.list.combined,
#                                                      var.cluster.raster.list.boolean,
#                                                      after=length(var.cluster.raster.list.combined))
#           
#           # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 2 ---
#           
#           # only do this part if multiple clusters
#           
#           # Create new list
#           var.cluster.raster.list.combined.2 <- list()
#           combined.index <- 1
#           
#           # Combine overlapping clusters
#           # Test whether sum of two boolean clusters > 1
#           for (list.index in 1:length(var.cluster.raster.list.combined)){
#             
#             # create new list
#             confirmation.list <- list()
#             confirmation.index <- 1
#             
#             # print(list.index)
#             
#             # test if sum of two rasters > 1
#             var.temp.test <- var.cluster.raster.list.combined[[list.index]]
#             
#             # look through other list items for comparison to var.temp.test
#             for (temp.index in 1:length(var.cluster.raster.list.combined)){
#               
#               # temp.index <- 1
#               
#               if (temp.index != list.index){
#                 
#                 # get var.temp.compare
#                 var.temp.compare <- var.cluster.raster.list.combined[[temp.index]]
#                 
#                 # test whether overlapping date strings
#                 var.temp.test.time <- lubridate::yday(terra::time(var.temp.test))
#                 var.temp.compare.time <- lubridate::yday(terra::time(var.temp.compare))
#                 
#                 if (any(var.temp.test.time %in% var.temp.compare.time)){
#                   
#                   # merge the two rasters
#                   var.temp.combine <- terra::sds(var.temp.test,var.temp.compare) 
#                   var.temp.combine <- terra::mergeTime(var.temp.combine,fun=sum)
#                   
#                   # test if max of sum of two rasters is not NA
#                   if (!is.na(max(terra::minmax(var.temp.combine)))){
#                     
#                     # test if max of sum of two rasters > 1
#                     if (max(terra::minmax(var.temp.combine)) > 1){
#                       
#                       # return to boolean
#                       var.temp.combine <- var.temp.combine / var.temp.combine
#                       
#                       # subset native raster to newly merged time string
#                       raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine))),
#                                            max(lubridate::yday(terra::time(var.temp.combine))),1)
#                       var.temp.subset <- var[[raster.subset]]
#                       
#                       # multiply by native raster
#                       var.temp.combine <- var.temp.combine * var.temp.subset
#                       
#                       # append to list item
#                       confirmation.list[[confirmation.index]] <- var.temp.combine
#                       
#                       # update index
#                       confirmation.index <- confirmation.index + 1
#                       
#                     } else {}
#                     
#                   } else {}
#                   
#                 } else {}
#                 
#               } else {}
#               
#             }
#             
#             # take newly created list, convert to sds, repeat combination assessment, put one final raster into final list
#             if (length(confirmation.list) > 0){
#               
#               if (length(confirmation.list) > 1){
#                 
#                 # merge the x# of rasters
#                 var.temp.combine.final <- terra::sds(confirmation.list)
#                 var.temp.combine.final <- terra::mergeTime(var.temp.combine.final,fun=max)
#                 
#                 # append to list
#                 var.cluster.raster.list.combined.2[[combined.index]] <- var.temp.combine.final
#                 
#                 # update counter
#                 combined.index <- combined.index + 1
#                 
#               } else {
#                 
#                 # append to list
#                 var.cluster.raster.list.combined.2[[combined.index]] <- confirmation.list[[1]]
#                 
#                 # update counter
#                 combined.index <- combined.index + 1
#                 
#               }
#               
#             } else {}
#             
#           }
#           
#           # append unaltered clusters to list
#           var.cluster.raster.list.combined.2 <- append(var.cluster.raster.list.combined.2,
#                                                        var.cluster.raster.list.boolean,
#                                                        after=length(var.cluster.raster.list.combined.2))
#           
#           # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 1 ---
#           
#           # test if two list items are equal, if so, remove additional
#           var.cluster.raster.list.combined.clean <- var.cluster.raster.list.combined.2
#           
#           # go through list, remove another layer if it is equal to layer in question
#           for (are.equal in 1:length(var.cluster.raster.list.combined.2)){
#             
#             for (perhaps in 1:length(var.cluster.raster.list.combined.2)){
#               
#               if (are.equal != perhaps){
#                 
#                 if (perhaps > are.equal){
#                   
#                   if (terra::nlyr(var.cluster.raster.list.combined.2[[are.equal]]) == 
#                       terra::nlyr(var.cluster.raster.list.combined.2[[perhaps]])) {
#                     
#                     result <- terra::all.equal(var.cluster.raster.list.combined.2[[are.equal]],
#                                                var.cluster.raster.list.combined.2[[perhaps]]) != TRUE
#                     
#                     if (result[1] == T){}
#                     
#                     else {
#                       
#                       # remove perhaps layer
#                       var.cluster.raster.list.combined.clean <- var.cluster.raster.list.combined.clean[-perhaps]
#                       
#                     }
#                     
#                   } else {}
#                   
#                 } else {}
#                 
#               } else {}
#               
#             }
#             
#           }
#           
#           # HCQ NEEDS TO REPEAT THIS SECTION BY THE NUMBER OF STM minus 1: 2 ---
#           
#           # test if two list items are equal, if so, remove additional
#           var.cluster.raster.list.combined.clean.2 <- var.cluster.raster.list.combined.clean
#           
#           # go through list, remove another layer if it is equal to layer in question
#           for (are.equal in 1:length(var.cluster.raster.list.combined.clean)){
#             
#             for (perhaps in 1:length(var.cluster.raster.list.combined.clean)){
#               
#               if (are.equal != perhaps){
#                 
#                 if (perhaps > are.equal){
#                   
#                   if (terra::nlyr(var.cluster.raster.list.combined.clean[[are.equal]]) == 
#                       terra::nlyr(var.cluster.raster.list.combined.clean[[perhaps]])) {
#                     
#                     result <- terra::all.equal(var.cluster.raster.list.combined.clean[[are.equal]],
#                                                var.cluster.raster.list.combined.clean[[perhaps]]) != TRUE
#                     
#                     if (result[1] == T){}
#                     
#                     else {
#                       
#                       # remove perhaps layer
#                       var.cluster.raster.list.combined.clean.2 <- var.cluster.raster.list.combined.clean[-perhaps]
#                       
#                     }
#                     
#                   } else {}
#                   
#                 } else {}
#                 
#               } else {}
#               
#             }
#             
#           }
#           
#           # rename list
#           var.cluster.raster <- var.cluster.raster.list.combined.clean.2
#           
#           
#           # give back actual values
#           for (final.layer in 1:length(var.cluster.raster)){
#             
#             # get layer
#             var.temp.combine <- var.cluster.raster[[final.layer]]
#             
#             # subset native raster to newly merged time string
#             raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine))),
#                                  max(lubridate::yday(terra::time(var.temp.combine))),1)
#             var.temp.subset <- var[[raster.subset]]
#             
#             # multiply by native raster
#             var.cluster.raster[[final.layer]] <- var.temp.combine * var.temp.subset
#             
#           }
#           
#           # 2024-10-16 ---
#           
#           # calculate metadata
#           var.cluster.raster.metadata <- cluster.metadata.2(var.cluster.raster,var)
#           
#           # save metadata
#           write.csv(var.cluster.raster.metadata,
#                     file = paste0(string.directory.cluster,'metadata/event_metadata_',year,
#                                   '.csv'))
#           
#           # Get max extent per cluster event 
#           event.extent <- list()
#           event.count <- list()
#           
#           # get each spatraster list item
#           for (cluster.index in 1:length(var.cluster.raster)) {
#             
#             # subset
#             event.extent.temp <- var.cluster.raster[[cluster.index]]
#             
#             # binarize
#             event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
#             
#             # give new values, which are 'year.event_number' format
#             event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
#             
#             # loop through time steps of one event 
#             for (q in 1:terra::nlyr(event.extent.temp)) {
#               
#               # give final extent and value to time range 
#               event.extent.temp[[q]] <- event.extent.binary
#               
#             }
#             
#             # give time back
#             terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
#             
#             # rename layers
#             names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
#             
#             # update list
#             event.extent[[cluster.index]] <- event.extent.temp
#             
#             # update list with first spatial extent per event
#             event.count[[cluster.index]] <- event.extent.temp[[1]]
#             
#           }
#           
#           # unlist
#           event.extent <- terra::rast(unlist(event.extent))
#           
#           # give time as names to clusters
#           names(event.extent) <- terra::time(event.extent)
#           
#           # calculate daily extent, in cases where multiple clusters occur at the same time
#           event.extent <- terra::tapp(event.extent,'days',fun=max)
#           
#           # normalize extent as binary when overlapping produces 2+ extents in a location
#           event.extent <- event.extent / event.extent
#           
#           # Save output
#           save.cdf(raster = event.extent,
#                    folder.path = paste0(string.directory.cluster,'extent/'),
#                    file.name = paste0(string.hazard.name,'_extent_',year),
#                    var.name = 'event_id',
#                    long.name = 'event_id',
#                    unit = 'event_id')
#           
#           # unlist
#           event.count <- terra::rast(unlist(event.count))
#           
#           # give time as names to clusters
#           names(event.count) <- terra::time(event.count)
#           
#           # calculate daily extent, in cases where multiple clusters occur at the same time
#           event.count <- terra::tapp(event.count,'days',fun=max)
#           
#           # normalize extent as binary when overlapping produces 2+ extents in a location
#           event.count <- event.count / event.count
#           
#           # Save output
#           save.cdf(raster = event.count,
#                    folder.path = paste0(string.directory.cluster,'count/'),
#                    file.name = paste0(string.hazard.name,'_count_',year),
#                    var.name = 'event_id',
#                    long.name = 'event_id',
#                    unit = 'event_id')
#           
#           # unlist and stack result 
#           var.intensity <- terra::rast(unlist(var.cluster.raster))
#           
#           # remove layers without values
#           # 2. Identify layers that are entirely NaN
#           # Use sapply to check each layer for all NaN values
#           nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
#             all(is.nan(values(var.intensity[[i]])))
#           })
#           
#           if (sum(nan_layers) > 0){
#             
#             # 3. Keep only layers that are not entirely NaN
#             # Subset the SpatRaster to keep non-NaN layers
#             var.intensity <- var.intensity[[!nan_layers]]
#             
#           } else {}
#           
#           # Save output
#           save.cdf(raster = var.intensity,
#                    folder.path = paste0(string.directory.cluster,'intensity/'),
#                    file.name = paste0(string.hazard.name,'_intensity_',year),
#                    var.name = string.hazard.name,
#                    long.name = string.hazard.name,
#                    unit = string.variable.unit)
#           
#           # Convert intensity to binary raster
#           var.binary <- var.intensity / var.intensity
#           
#           # Save output
#           save.cdf(raster = var.binary,
#                    folder.path = paste0(string.directory.cluster,'binary/'),
#                    file.name = paste0(string.hazard.name,'_binary_',year),
#                    var.name = string.hazard.name,
#                    long.name = string.hazard.name,
#                    unit = 'boolean')
#           
#           # Compile dbscan parameter dataframe
#           dbscan.parameters.range <- data.frame(year = year,
#                                                 mu = var.kNN$mu,
#                                                 epsilon = var.kNN$epsilon,
#                                                 event.count = length(var.cluster.raster))
#           
#           # remove na rows
#           dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]
#           
#           # Save resulting dataframe
#           write.csv(dbscan.parameters.range,
#                     file = paste0(string.directory.cluster,'parameters/dbscan_parameters_',year,
#                                   '.csv'))
#           
#         } else {
#           
#           # Just multiply bool by native raster subset and then calculate everything
#           # return to boolean
#           var.temp.combine <- var.cluster.raster.list.boolean[[1]] / var.cluster.raster.list.boolean[[1]]
#           
#           # subset native raster to newly merged time string
#           raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine[[1]]))),
#                                max(lubridate::yday(terra::time(var.temp.combine[[1]]))),1)
#           var.temp.subset <- var[[raster.subset]]
#           
#           # multiply by native raster
#           var.temp.combine <- var.temp.combine * var.temp.subset
#           
#           # Move below to else statement
#           
#           # Do everything you would normally do
#           # calculate metadata
#           var.cluster.raster.metadata <- cluster.metadata.2(var.cluster.raster,var)
#           
#           # save metadata
#           write.csv(var.cluster.raster.metadata,
#                     file = paste0(string.directory.cluster,'metadata/event_metadata_',year,
#                                   '.csv'))
#           
#           # Get max extent per cluster event 
#           event.extent <- list()
#           event.count <- list()
#           
#           # get each spatraster list item
#           for (cluster.index in 1:length(var.cluster.raster)) {
#             
#             # subset
#             event.extent.temp <- var.cluster.raster[[cluster.index]]
#             
#             # binarize
#             event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
#             
#             # give new values, which are 'year.event_number' format
#             event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
#             
#             # loop through time steps of one event 
#             for (q in 1:terra::nlyr(event.extent.temp)) {
#               
#               # give final extent and value to time range 
#               event.extent.temp[[q]] <- event.extent.binary
#               
#             }
#             
#             # give time back
#             terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
#             
#             # rename layers
#             names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
#             
#             # update list
#             event.extent[[cluster.index]] <- event.extent.temp
#             
#             # update list with first spatial extent per event
#             event.count[[cluster.index]] <- event.extent.temp[[1]]
#             
#           }
#           
#           # unlist
#           event.extent <- terra::rast(unlist(event.extent))
#           
#           # give time as names to clusters
#           names(event.extent) <- terra::time(event.extent)
#           
#           # calculate daily extent, in cases where multiple clusters occur at the same time
#           event.extent <- terra::tapp(event.extent,'days',fun=max)
#           
#           # normalize extent as binary when overlapping produces 2+ extents in a location
#           event.extent <- event.extent / event.extent
#           
#           # Save output
#           save.cdf(raster = event.extent,
#                    folder.path = paste0(string.directory.cluster,'extent/'),
#                    file.name = paste0(string.hazard.name,'_extent_',year),
#                    var.name = 'event_id',
#                    long.name = 'event_id',
#                    unit = 'event_id')
#           
#           # unlist
#           event.count <- terra::rast(unlist(event.count))
#           
#           # give time as names to clusters
#           names(event.count) <- terra::time(event.count)
#           
#           # calculate daily extent, in cases where multiple clusters occur at the same time
#           event.count <- terra::tapp(event.count,'days',fun=max)
#           
#           # normalize extent as binary when overlapping produces 2+ extents in a location
#           event.count <- event.count / event.count
#           
#           # Save output
#           save.cdf(raster = event.count,
#                    folder.path = paste0(string.directory.cluster,'count/'),
#                    file.name = paste0(string.hazard.name,'_count_',year),
#                    var.name = 'event_id',
#                    long.name = 'event_id',
#                    unit = 'event_id')
#           
#           # unlist and stack result 
#           var.intensity <- terra::rast(unlist(var.cluster.raster))
#           
#           # remove layers without values
#           # 2. Identify layers that are entirely NaN
#           # Use sapply to check each layer for all NaN values
#           nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
#             all(is.nan(values(var.intensity[[i]])))
#           })
#           
#           if (sum(nan_layers) > 0){
#             
#             # 3. Keep only layers that are not entirely NaN
#             # Subset the SpatRaster to keep non-NaN layers
#             var.intensity <- var.intensity[[!nan_layers]]
#             
#           } else {}
#           
#           # Save output
#           save.cdf(raster = var.intensity,
#                    folder.path = paste0(string.directory.cluster,'intensity/'),
#                    file.name = paste0(string.hazard.name,'_intensity_',year),
#                    var.name = string.hazard.name,
#                    long.name = string.hazard.name,
#                    unit = string.variable.unit)
#           
#           # Convert intensity to binary raster
#           var.binary <- var.intensity / var.intensity
#           
#           # Save output
#           save.cdf(raster = var.binary,
#                    folder.path = paste0(string.directory.cluster,'binary/'),
#                    file.name = paste0(string.hazard.name,'_binary_',year),
#                    var.name = string.hazard.name,
#                    long.name = string.hazard.name,
#                    unit = 'boolean')
#           
#           # Compile dbscan parameter dataframe
#           dbscan.parameters.range <- data.frame(year = year,
#                                                 mu = var.kNN$mu,
#                                                 epsilon = var.kNN$epsilon,
#                                                 event.count = length(var.cluster.raster))
#           
#           # remove na rows
#           dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]
#           
#           # Save resulting dataframe
#           write.csv(dbscan.parameters.range,
#                     file = paste0(string.directory.cluster,'parameters/dbscan_parameters_',year,
#                                   '.csv'))
#           
#         }
#         
#       } else {print(paste0('No clusters in ',year))}
#       
#     } else {print(paste0('No clusters in ',year))}
#     
#     # time
#     end.time <- Sys.time()
#     time.taken <- end.time - start.time
#     print(time.taken)
#     
#   }
#   
# }


cluster.compile.7 <- function(raster.threshold.criteria,vector.year.range,
                              string.hazard.name,string.directory.variable.raw,
                              string.directory.variable.boolean,string.variable.unit,
                              numeric.space.time.metric,string.directory.cluster){
  
  # debug
  raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_warning_1_hour_20240925.nc')
  vector.year.range = seq(2019,2023,1)
  string.hazard.name = "Excessive_Heat_Warning"
  string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/1-day/"
  string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/heat_index/warning/"
  numeric.space.time.metric = data.frame(alpha = c(0.75,0.25),range = c(50,1000),tau = c(1000,20))
  string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/"
  string.variable.unit = "C"
  # 
  # # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/precipitation_001_year_24_hour_20240925.nc')
  # vector.year.range = seq(1963,1963,1)
  # string.hazard.name = "Precipitation"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/precipitation/001/"
  # numeric.space.time.metric = data.frame(alpha = c(0.99,0.01),range = c(12,20),tau = c(4,4))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/precipitation/001/"
  # string.variable.unit = "mm"
  
  # code
  for (year in vector.year.range) {
    
    year <- 1952
    
    # print statement
    print(paste0('-------------------- ',string.hazard.name,' ',year,' --------------------'))
    
    # time
    start.time <- Sys.time()
    print(start.time)
    
    # Load regular Raster
    var <- raster.compile(year = as.character(year),directory = string.directory.variable.raw)
    
    # Load Boolean Raster
    var.bool <- raster.compile(year = as.character(year),directory = string.directory.variable.boolean)
    
    # Crop
    var <- raster.crop.mask(var,raster.threshold.criteria)
    
    # Correct Extent
    terra::ext(var) <- terra::ext(raster.threshold.criteria)
    terra::ext(var.bool) <- terra::ext(raster.threshold.criteria)
    
    # Cluster (10 min)
    # DBSCAN - covariance model parameters obtained from BMElib Matlab code
    var.cov.model <- numeric.space.time.metric
    
    # DBSCAN - space time metric: result is in (# hours / 0.25 spatial degree)
    # no longer need to multiply by 4 to get # hours / 0.25 spatial degree (ERA5 units)
    var.st.metric <- cov.ratio(var.cov.model) 
    
    # round to nearest whole number
    var.st.metric <- round(var.st.metric,0)
    
    # change st.metric to 1 if below 1
    if (var.st.metric[1] < 1){
      var.st.metric[1] <- 1
    }
    
    if (var.st.metric[2] < 1){
      var.st.metric[2] <- 1
    }
    
    # DBSCAN - create space time cube (1 min)
    var.cube <- cluster.lat.long.date.2(var.bool)
    
    # Only continue if enough extreme points
    if (nrow(var.cube) >= 4){
      
      # create empty list
      var.cluster.raster.list <- list()
      
      # recursively cluster for each compressed row
      for (compressed in 1:var.st.metric[1,1]){
        
        # DBSCAN - incorporate space time metric
        var.st.cube <- cluster.st.cube.2(var.cube,var.st.metric[1],row.start = compressed)
        
        # DBSCAN - correct order of var.st.cube df
        var.st.cube <- var.st.cube[,-1]
        
        # Test if enough points to continue
        if (nrow(var.st.cube) >= 4){
          
          # DBSCAN - kth nearest neighbor analysis to return epsilon and mu
          var.kNN <- cluster.kNN.2(var.cube[,c(2,4,5)]) # var.st.cube
          
          # DBSCAN - cluster for defined mu and epsilon
          var.cluster <- cluster.extremes.2(raster.cube.df = var.st.cube, # var.st.cube
                                            raster.bool = raster.bool,
                                            raster.var = var,
                                            raster.kNN.df = var.kNN)
          
          # DBSCAN - get raster for each cluster
          var.cluster.raster <- cluster.rasters.4(clusters.df = var.cluster[[1]],
                                                  raster.var = var,
                                                  cluster.st.metric = var.st.metric[1])
          
          # See if any clusters
          if (!is.null(var.cluster.raster)){
            
            # store in list object
            var.cluster.raster.list[[compressed]] <- var.cluster.raster 
            
          } else {}
          
        } else {print(paste0('No clusters in ',year,' for compressed time slice # ',compressed))}
        
      }
      
      # Only continue if list has clusters in it, as opposed to only noise points 
      if (length(var.cluster.raster.list) > 0){

        # unlist 
        var.cluster.raster <- unlist(var.cluster.raster.list)
        
        # Create new list
        var.cluster.raster.list.boolean <- list()
        
        # convert clusters to booleans for combining
        for (list.index in 1:length(var.cluster.raster)){
          
          # list.index <- 1
          
          # create temporary raster
          var.temp <- var.cluster.raster[[list.index]]
          
          # convert 0 to na
          var.temp[var.temp == 0] <- NA
          
          # create boolean
          var.cluster.raster.list.boolean[[list.index]] <- var.temp / var.temp
          
        }
        
        # print statement
        print(paste0('There are ',length(var.cluster.raster.list.boolean),' clusters in ',
                     year,', but some may be duplicates or may need to be merged'))
        
        # Create new list
        var.cluster.raster.list.combined <- list()
        combined.index <- 1
        
        # this is a case where we have multiple clusters, and need to see 
        # whether there are duplicates or clusters needing to be merged
        if (length(var.cluster.raster.list.boolean) > 1){
          
          # Combine overlapping clusters
          # Test whether sum of two boolean clusters > 1
          for (list.index.new in 1:length(var.cluster.raster.list.boolean)){
            
            # create new list
            confirmation.list <- list()
            confirmation.index <- 1
            
            # test if sum of two rasters > 1
            var.temp.test <- var.cluster.raster.list.boolean[[list.index.new]]
            
            # look through other list items for comparison to var.temp.test
            for (temp.index in 1:length(var.cluster.raster.list.boolean)){
              
              # get var.temp.compare
              var.temp.compare <- var.cluster.raster.list.boolean[[temp.index]]
              
              # test whether overlapping date strings
              var.temp.test.time <- lubridate::yday(terra::time(var.temp.test))
              var.temp.compare.time <- lubridate::yday(terra::time(var.temp.compare))
              
              if (any(var.temp.test.time %in% var.temp.compare.time)){
                
                # merge the two rasters
                var.temp.combine <- terra::sds(var.temp.test,var.temp.compare) # HCQ leftoff here
                var.temp.combine <- terra::mergeTime(var.temp.combine,fun=sum)
                
                # test if max of sum of two rasters is not NA
                if (!is.na(max(terra::minmax(var.temp.combine)))){
                  
                  # test if max of sum of two rasters > 1
                  if (max(terra::minmax(var.temp.combine)) > 1){
                    
                    # return to boolean
                    var.temp.combine <- var.temp.combine / var.temp.combine
                    
                    # append to list item
                    confirmation.list[[confirmation.index]] <- var.temp.combine
                    
                    # update index
                    confirmation.index <- confirmation.index + 1
                    
                  } else {}
                  
                } else {}
                
              } else {}
              
            }
            
            # take newly created list, convert to sds, repeat combination assessment, 
            # put one final raster into final list
            if (length(confirmation.list) > 1){
              
              # This is the case where there are multiple overlapping clusters 
              # that need to be combined
              
              # Combine any potentially overlapping boolean rasters
              var.temp.combine.final <- terra::sds(confirmation.list)
              var.temp.combine.final <- terra::mergeTime(var.temp.combine.final,fun=sum)
              
              # convert 0 to na
              var.temp.combine.final[var.temp.combine.final == 0] <- NA
              
              # create boolean
              var.temp.combine.final <- var.temp.combine.final / var.temp.combine.final
              
              # append to list
              var.cluster.raster.list.combined[[combined.index]] <- var.temp.combine.final
              
              # update index
              combined.index <- combined.index + 1
              
            } else {
              
              # If the confirmation list only has one entry, fill new list position with this value
              var.cluster.raster.list.combined[[combined.index]] <- confirmation.list[[1]]
              
              # update index
              combined.index <- combined.index + 1
              
            }
            
          } 
          
        } else {
          
          # If there's only one entry in the boolean list, save the original boolean result
          var.cluster.raster.list.combined[[combined.index]] <- var.cluster.raster.list.boolean
          
          # update index
          combined.index <- combined.index + 1
          
        }
        
        # Remove duplicated layers
        # HCQ leftoff here, need to massage data a bit more
        plot(var.cluster.raster.list.combined[[1]])
        plot(var.cluster.raster.list.combined[[2]])
        plot(var.cluster.raster.list.combined[[3]])
        plot(var.cluster.raster.list.combined[[4]])
        plot(var.cluster.raster.list.combined[[5]])
        plot(var.cluster.raster.list.combined[[6]])
        plot(var.cluster.raster.list.combined[[7]])
        plot(var.cluster.raster.list.combined[[8]])
        plot(var.cluster.raster.list.combined[[9]])
        plot(var.cluster.raster.list.combined[[10]])
        plot(var.cluster.raster.list.combined[[11]])
        plot(var.cluster.raster.list.combined[[12]])
        plot(var.cluster.raster.list.combined[[13]])
        plot(var.cluster.raster.list.combined[[14]])
        plot(var.cluster.raster.list.combined[[15]])
        
        # unlist 
        var.cluster.raster.list.combined <- unlist(var.cluster.raster.list.combined)
        
        # make empty list
        var.cluster.raster.list.combined.final <- list()
        combined.final.index <- 1
        
        # Turn booleans into measurements
        for (layer.subset in 1:length(var.cluster.raster.list.combined)){
          
          # get layer
          var.temp.combine.clean <- var.cluster.raster.list.combined[[layer.subset]]
          
          # subset native raster to newly merged time string
          raster.subset <- seq(min(lubridate::yday(terra::time(var.temp.combine.clean))),
                               max(lubridate::yday(terra::time(var.temp.combine.clean))),1)
          var.temp.subset.clean <- var[[raster.subset]]
          
          # multiply by native raster
          var.temp.combine.clean <- var.temp.combine.clean * var.temp.subset.clean
          
          # append to list item
          var.cluster.raster.list.combined.final[[combined.final.index]] <- var.temp.combine.clean
          
          # update index
          combined.final.index <- combined.final.index + 1
          
        }

        # HCQ leftoff here...
        # Still not quite removing duplicates fully, and apparently getting a few layers with NaN values...
        # see if any list items have entirely NA values
        plot(var.cluster.raster.list.combined.final[[8]])
        
        # print statement 
        print(paste0('Calculating metadata for a total of ',
                     length(var.cluster.raster.list.combined.final),
                     ' final clusters'))
        
        # calculate metadata
        var.cluster.raster.metadata <- cluster.metadata.2(var.cluster.raster.list.combined.final,var)
        
        # save metadata
        write.csv(var.cluster.raster.metadata,
                  file = paste0(string.directory.cluster,'metadata/event_metadata_',year,
                                '.csv'))
        
        # Get max extent per cluster event 
        event.extent <- list()
        event.count <- list()
        
        # get each spatraster list item
        for (cluster.index in 1:length(var.cluster.raster)) {
          
          # subset
          event.extent.temp <- var.cluster.raster[[cluster.index]]
          
          # binarize
          event.extent.binary <- sum(event.extent.temp) / sum(event.extent.temp)
          
          # give new values, which are 'year.event_number' format
          event.extent.binary <- (event.extent.binary * year) + (cluster.index / 100)
          
          # loop through time steps of one event 
          for (q in 1:terra::nlyr(event.extent.temp)) {
            
            # give final extent and value to time range 
            event.extent.temp[[q]] <- event.extent.binary
            
          }
          
          # give time back
          terra::time(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
          
          # rename layers
          names(event.extent.temp) <- terra::time(var.cluster.raster[[cluster.index]])
          
          # update list
          event.extent[[cluster.index]] <- event.extent.temp
          
          # update list with first spatial extent per event
          event.count[[cluster.index]] <- event.extent.temp[[1]]
          
        }
        
        # unlist
        event.extent <- terra::rast(unlist(event.extent))
        
        # give time as names to clusters
        names(event.extent) <- terra::time(event.extent)
        
        # calculate daily extent, in cases where multiple clusters occur at the same time
        event.extent <- terra::tapp(event.extent,'days',fun=max)
        
        # normalize extent as binary when overlapping produces 2+ extents in a location
        event.extent <- event.extent / event.extent
        
        # Save output
        save.cdf(raster = event.extent,
                 folder.path = paste0(string.directory.cluster,'extent/'),
                 file.name = paste0(string.hazard.name,'_extent_',year),
                 var.name = 'event_id',
                 long.name = 'event_id',
                 unit = 'event_id')
        
        # unlist
        event.count <- terra::rast(unlist(event.count))
        
        # give time as names to clusters
        names(event.count) <- terra::time(event.count)
        
        # calculate daily extent, in cases where multiple clusters occur at the same time
        event.count <- terra::tapp(event.count,'days',fun=max)
        
        # normalize extent as binary when overlapping produces 2+ extents in a location
        event.count <- event.count / event.count
        
        # Save output
        save.cdf(raster = event.count,
                 folder.path = paste0(string.directory.cluster,'count/'),
                 file.name = paste0(string.hazard.name,'_count_',year),
                 var.name = 'event_id',
                 long.name = 'event_id',
                 unit = 'event_id')
        
        # unlist and stack result 
        var.intensity <- terra::rast(unlist(var.cluster.raster))
        
        # remove layers without values
        # 2. Identify layers that are entirely NaN
        # Use sapply to check each layer for all NaN values
        nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
          all(is.nan(values(var.intensity[[i]])))
        })
        
        if (sum(nan_layers) > 0){
          
          # 3. Keep only layers that are not entirely NaN
          # Subset the SpatRaster to keep non-NaN layers
          var.intensity <- var.intensity[[!nan_layers]]
          
        } else {}
        
        # Save output
        save.cdf(raster = var.intensity,
                 folder.path = paste0(string.directory.cluster,'intensity/'),
                 file.name = paste0(string.hazard.name,'_intensity_',year),
                 var.name = string.hazard.name,
                 long.name = string.hazard.name,
                 unit = string.variable.unit)
        
        # Convert intensity to binary raster
        var.binary <- var.intensity / var.intensity
        
        # Save output
        save.cdf(raster = var.binary,
                 folder.path = paste0(string.directory.cluster,'binary/'),
                 file.name = paste0(string.hazard.name,'_binary_',year),
                 var.name = string.hazard.name,
                 long.name = string.hazard.name,
                 unit = 'boolean')
        
        # Compile dbscan parameter dataframe
        dbscan.parameters.range <- data.frame(year = year,
                                              mu = var.kNN$mu,
                                              epsilon = var.kNN$epsilon,
                                              event.count = length(var.cluster.raster))
        
        # remove na rows
        dbscan.parameters.range <- dbscan.parameters.range[complete.cases(dbscan.parameters.range),]
        
        # Save resulting dataframe
        write.csv(dbscan.parameters.range,
                  file = paste0(string.directory.cluster,'parameters/dbscan_parameters_',year,
                                '.csv'))
        
      } else {print(paste0('No clusters were modeled in ',year))}
      
    } else {print(paste0('There are not enough extreme observations to cluster in ',year))}
    
  }
  
  # time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}

cluster.compile.val <- function(raster.threshold.criteria,vector.year.range,
                              string.hazard.name,string.directory.variable.raw,
                              string.directory.variable.boolean,string.variable.unit,
                              numeric.space.time.metric,string.directory.cluster,
                              string.directory.storage){
  
  # # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/heat_index_warning_1_hour_20240925.nc')
  # vector.year.range = seq(2019,2023,1)
  # string.hazard.name = "Excessive_Heat_Warning"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/heat_index/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/heat_index/warning/"
  # numeric.space.time.metric = data.frame(alpha = c(0.75,0.25),range = c(50,1000),tau = c(1000,20))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/"
  # string.variable.unit = "C"
  
  # # debug
  # raster.threshold.criteria = terra::rast('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/threshold_criteria/precipitation_001_year_24_hour_20240925.nc')
  # vector.year.range = seq(1963,1963,1)
  # string.hazard.name = "Precipitation"
  # string.directory.variable.raw = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/prcp_tot/1-day/"
  # string.directory.variable.boolean = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/boolean/precipitation/001/"
  # numeric.space.time.metric = data.frame(alpha = c(0.99,0.01),range = c(12,20),tau = c(4,4))
  # string.directory.cluster = "V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/precipitation/001/"
  # string.variable.unit = "mm"
  
  # code
  for (year in vector.year.range) {
    
    # year <- 1950
    
    # print statement
    print(paste0('-------------------- ',string.hazard.name,' ',year,' --------------------'))
    
    # time
    start.time <- Sys.time()
    print(start.time)
    
    # Load regular Raster
    var <- raster.compile(year = as.character(year),directory = string.directory.variable.raw)
    
    # Load Boolean Raster
    var.bool <- raster.compile(year = as.character(year),directory = string.directory.variable.boolean)
    # plot(sum(var.bool))
    
    # Crop
    var <- raster.crop.mask(var,raster.threshold.criteria)
    
    # Correct Extent
    terra::ext(var) <- terra::ext(raster.threshold.criteria)
    terra::ext(var.bool) <- terra::ext(raster.threshold.criteria)
    
    # Cluster (10 min)
    # DBSCAN - covariance model parameters obtained from BMElib Matlab code
    var.cov.model <- numeric.space.time.metric
    
    # DBSCAN - space time metric: result is in (# hours / 0.25 spatial degree)
    # no longer need to multiply by 4 to get # hours / 0.25 spatial degree (ERA5 units)
    var.st.metric <- cov.ratio(var.cov.model) 
    
    # round to nearest whole number
    var.st.metric <- round(var.st.metric,0)
    
    # change st.metric to 1 if below 1
    if (var.st.metric[1] < 1){
      var.st.metric[1] <- 1
    }
    
    if (var.st.metric[2] < 1){
      var.st.metric[2] <- 1
    }
    
    # DBSCAN - create space time cube (1 min)
    var.cube <- cluster.lat.long.date.2(var.bool)
    
    # Only continue if enough extreme points
    if (nrow(var.cube >= 4)){
      
      # create empty list
      var.cluster.raster.list <- list()
      
      # recursively cluster for each compressed row
      for (compressed in 1:var.st.metric[1,1]){
        
        # compressed <- 1
        
        # DBSCAN - incorporate space time metric
        var.st.cube <- cluster.st.cube.2(var.cube,var.st.metric[1],row.start = compressed)
        
        # DBSCAN - correct order of var.st.cube df
        var.st.cube <- var.st.cube[,-1]
        
        # Test if enough points to continue
        if (nrow(var.st.cube) >= 4){
          
          # DBSCAN - kth nearest neighbor analysis to return epsilon and mu
          var.kNN <- cluster.kNN.2(var.cube[,c(2,4,5)]) # var.st.cube
          
          # DBSCAN - cluster for defined mu and epsilon
          var.cluster <- cluster.extremes.2(raster.cube.df = var.st.cube, # var.st.cube
                                            raster.bool = raster.bool,
                                            raster.var = var,
                                            raster.kNN.df = var.kNN)
          
          # DBSCAN - get raster for each cluster
          var.cluster.raster <- cluster.rasters.4(clusters.df = var.cluster[[1]],
                                                  raster.var = var,
                                                  cluster.st.metric = var.st.metric[1])
          
          # See if any clusters
          if (!is.null(var.cluster.raster)){
            
            # store in list object
            var.cluster.raster.list[[compressed]] <- var.cluster.raster 
            
          } else {}
          
          
          
        } else {print(paste0('No clusters in ',year,' for compressed time slice # ',compressed))}
        
      }
      
      if (length(var.cluster.raster.list) > 0){
        
        # unlist 
        var.cluster.raster <- unlist(var.cluster.raster.list)
        
        # # unlist and stack result 
        # var.intensity <- terra::rast(unlist(var.cluster.raster))
        # 
        # # remove layers without values
        # # 2. Identify layers that are entirely NaN
        # # Use sapply to check each layer for all NaN values
        # nan_layers <- sapply(1:nlyr(var.intensity), function(i) {
        #   all(is.nan(values(var.intensity[[i]])))
        # })
        # 
        # if (sum(nan_layers) > 0){
        #   
        #   # 3. Keep only layers that are not entirely NaN
        #   # Subset the SpatRaster to keep non-NaN layers
        #   var.intensity <- var.intensity[[!nan_layers]]
        #   
        # } else {}
        
        # save each cluster with max extent and duration as separate spatraster
        for (intensity in 1:length(var.cluster.raster)){
          
          # # Save output
          # save.cdf(raster = var.cluster.raster[[intensity]],
          #          folder.path = paste0('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
          #                               string.hazard.name,'/'),
          #          file.name = paste0(string.hazard.name,'_intensity_',year,'_event_',intensity),
          #          var.name = string.hazard.name,
          #          long.name = string.hazard.name,
          #          unit = string.variable.unit)
          
          # Save output
          save.cdf(raster = var.cluster.raster[[intensity]],
                   folder.path = paste0(string.directory.cluster,'/raw/'),
                   file.name = paste0(string.hazard.name,'_intensity_',year,'_event_',intensity),
                   var.name = string.hazard.name,
                   long.name = string.hazard.name,
                   unit = string.variable.unit)
          
        }
        
      } else {print(paste0('No clusters in ',year))}
      
    } else {print(paste0('No clusters in ',year))}
    
    # time
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
  }
  
}







cluster.workflow <- function(st.cube,return.period,raster.var,difference.value,
                             difference.operator,param.summary,raster.bool,st.metric,min.extremes){
  
  # time
  start.time.all <- Sys.time()
  print(start.time.all)
  
  # # debug
  # st.cube <- var.cube
  # return.period <- '001'
  # raster.var <- var
  # difference.value <- 10
  # difference.operator <- 'add'
  # param.summary <- 'min'
  # raster.bool <- var.bool
  # st.metric <- var.st.metric
  # min.extremes <- 100
  
  # Only conduct clustering if the number of extreme observations are greater than the dimensions of your dataset (lat, long, time = 3. 3 + 1 = 4) 
  if (nrow(st.cube) < 4) {
    
    print(paste0('There are ',nrow(st.cube),' Extreme Observations, which is not enough to Form a single Cluster'))
    
  } else {
    
    # DBSCAN - incorporate space time metric
    print('DBSCAN - incorporate space time metric')
    var.st.cube <- cluster.st.cube(st.cube,st.metric[1])
    
    # Only conduct clustering if number of extreme observations exceeds the number of nearest neighbors (min.extremes) you are interested in 
    if (min.extremes >= nrow(var.st.cube)) { 
      # from https://rdrr.io/cran/dbscan/src/R/kNN.R 
      # if (k >= nrow(x))
      # stop("Not enough neighbors in data set!")
      
      print(paste0('There are ',nrow(var.st.cube),' Extreme Observations, which is not enough to Form a Cluster at a min.extremes values of ',min.extremes,'. We recommend lowering your value of min.extremes.'))
      
    } else {
      
      print(paste0('There are ',nrow(var.st.cube),' Extreme Observations, which are now being clustered'))
      
      # DBSCAN - kth nearest neighbor analysis to return plausible epsilon and mu
      print('DBSCAN - kth nearest neighbor analysis to return plausible epsilon and mu')
      var.kNN <- cluster.kNN(var.st.cube[,1:3],min.extremes = min.extremes)
      
      # DBSCAN - subset plausible values
      print('DBSCAN - subset plausible values')
      var.plausible <- cluster.plausible(dbscan.params.df = var.kNN,
                                         difference.value = difference.value,
                                         difference.operator = difference.operator)
      
      # DBSCAN - cluster for defined mu and epsilon
      print('DBSCAN - cluster for defined mu and epsilon')
      var.cluster <- cluster.extremes(raster.cube.df = var.st.cube,
                                      raster.kNN.df = var.plausible,
                                      raster.bool = raster.bool,
                                      raster.var = raster.var,
                                      param.summary = param.summary)
      
      # DBSCAN - get raster for each cluster 
      print('DBSCAN - get raster for each cluster')
      var.cluster.raster <- cluster.rasters(clusters.df = var.cluster[[1]],
                                            raster.var = raster.var,
                                            cluster.st.metric = st.metric[1])
      
      # DBSCAN - remove NA layers
      print('DBSCAN - remove NA layers')
      var.cluster.raster <- cluster.subset(var.cluster.raster)
      
      # troubleshoot
      var.cluster.raster
      
      # DBSCAN - calculate metadata for each cluster as dataframe 
      print('DBSCAN - calculate metadata for each cluster as dataframe')
      var.cluster.metadata <- cluster.metadata(var.cluster.raster,raster.var)
      
      # DBSCAN - create single binary raster of extremes for the year (4 min)
      print('DBSCAN - create single binary raster of extremes for the year')
      var.cluster.compiled <- cluster.compile(raster.var,var.cluster.raster)
      
      # save as either precip or heat index results depending on input
      if (return.period == '001' || return.period == '002' || return.period == '005' || 
          return.period == '010' || return.period == '025' || return.period == '050' || 
          return.period == '100' ) {
        
        # DBSCAN - save binary rasters
        print('DBSCAN - save binary rasters')
        save.cdf(raster = var.cluster.compiled,
                 folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/',
                 file.name = paste0('precipitation_',return.period,'_binary_',year),
                 var.name = 'precipitation_binary',
                 long.name = 'precipitation_binary',unit = 'boolean')
        
        # DBSCAN - save intensity rasters
        print('DBSCAN - save intensity rasters')
        var.cluster.compiled.intensity <- var.cluster.compiled * raster.var
        
        save.cdf(raster = var.cluster.compiled.intensity,
                 folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/',
                 file.name = paste0('precipitation_',return.period,'_intensity_',year),
                 var.name = 'precipitation_intensity',
                 long.name = 'precipitation_intensity',unit = 'boolean')
        
        # DBSCAN - save cluster metadata dataframes
        print('DBSCAN - save cluster metadata dataframes')
        write.csv(var.cluster.metadata,
                  paste0('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/',
                         'precipitation_',return.period,'_metadata_',year,'.txt'))
        
      } else {
        
        # DBSCAN - save binary rasters
        print('DBSCAN - save binary rasters')
        save.cdf(raster = var.cluster.compiled,
                 folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/',
                 file.name = paste0('heat_index_',return.period,'_binary_',year),
                 var.name = 'heat_index_binary',
                 long.name = 'heat_index_binary',unit = 'boolean')
        
        # DBSCAN - save intensity rasters
        print('DBSCAN - save intensity rasters')
        var.cluster.compiled.intensity <- var.cluster.compiled * raster.var
        
        save.cdf(raster = var.cluster.compiled.intensity,
                 folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/',
                 file.name = paste0('heat_index_',return.period,'_intensity_',year),
                 var.name = 'heat_index_intensity',
                 long.name = 'heat_index_intensity',unit = 'boolean')
        
        # DBSCAN - save cluster metadata dataframes
        print('DBSCAN - save cluster metadata dataframes')
        write.csv(var.cluster.metadata,
                  paste0('V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/',
                         'heat_index_',return.period,'_metadata_',year,'.txt'))
        
      }
      
    } 
    
  }
    
  # time
  end.time.all <- Sys.time()
  time.taken <- end.time.all - start.time.all
  print(time.taken)
  
  # print statement
  print('Finished Clustering')
  
}

cluster.eca.delT <- function(binary.var.1,binary.var.2,max.time,raster.var){
 
  # # debug
  # binary.var.1
  # binary.var.2
  max.time <- 7 * 24
  year <- 2023
  raster.var <- raster.compile(year = as.character(year),directory = "V:/users/hquintal/PhD2_Cluster_Parameters_2/data/in/temp/southeast/") # temp
  
  # convert binary raster into df
  binary.var.1.df <- as.data.frame(binary.var.1,xy=T)
  binary.var.2.df <- as.data.frame(binary.var.2,xy=T)
  lat.long <- binary.var.1.df[1:nrow(binary.var.1.df),1:2]
  
  # create vars
  var1 <- t(binary.var.1.df[,3:ncol(binary.var.1.df)])
  var2 <- t(binary.var.2.df[,3:ncol(binary.var.2.df)])
  
  # make open lists
  eca.coin.days.precursor.terra.delT <- list()
  eca.coin.days.trigger.terra.delT <- list()

  # rename max time
  max.days <- max.time
  
  # delT
  for (j in 0:max.days){
    
    # print(paste0('Start delT of ',j,' days at ',Sys.time()))
    
    # set up results matrix
    eca.results <- matrix(nrow=6,ncol=ncol(var1))
    
    # setup eca for-loop
    for (i in 1:ncol(var1)){
      
      #eca
      print(paste0('ECA on row ',i))
      ca.out <- CoinCalc::CC.eca.ts(var1[,i],var2[,i],
                                    sigtest = 'poisson',
                                    alpha = .05,delT = j,tau = 0) 
      
      #read to matrix
      print('Write to Matrix')
      eca.results[1,i] <- ca.out$`NH precursor`[1]
      eca.results[2,i] <- ca.out$`NH trigger`[1]
      eca.results[3,i] <- ca.out$`p-value precursor`[1]
      eca.results[4,i] <- ca.out$`p-value trigger`[1]
      eca.results[5,i] <- ca.out$`precursor coincidence rate`[1]
      eca.results[6,i] <- ca.out$`trigger coincidence rate`[1]
    }
    
    print('Transpose')
    eca.results <- t(eca.results)
    
    # convert to dfs
    print('Convert to dfs')
    eca.p.val.precursor <- as.data.frame(cbind(lat.long,eca.results[,3]))
    eca.p.val.trigger <- as.data.frame(cbind(lat.long,eca.results[,4]))
    eca.coin.rate.precursor <- as.data.frame(cbind(lat.long,eca.results[,5]))
    eca.coin.rate.trigger <- as.data.frame(cbind(lat.long,eca.results[,6]))
    
    # binarize p-vals
    print('Binarize p-values')
    eca.p.val.precursor[,3] <- ifelse(eca.p.val.precursor[,3] < 0.05,1,0)
    eca.p.val.trigger[,3] <- ifelse(eca.p.val.trigger[,3] < 0.05,1,0)
    
    # make rasters
    print('Make rasters')
    eca.p.val.precursor.raster <- raster::rasterFromXYZ(eca.p.val.precursor)
    eca.p.val.trigger.raster <- raster::rasterFromXYZ(eca.p.val.trigger)
    eca.coin.rate.precursor.raster <- raster::rasterFromXYZ(eca.coin.rate.precursor)
    eca.coin.rate.trigger.raster <- raster::rasterFromXYZ(eca.coin.rate.trigger)
    
    # covert from raster::stack to terra::rast for demeaning
    print('Convert to terra')
    eca.p.val.precursor.terra <- as(eca.p.val.precursor.raster, "SpatRaster")
    eca.p.val.trigger.terra <- as(eca.p.val.trigger.raster, "SpatRaster")
    eca.coin.rate.precursor.terra <- as(eca.coin.rate.precursor.raster, "SpatRaster")
    eca.coin.rate.trigger.terra <- as(eca.coin.rate.trigger.raster, "SpatRaster")
    
    # add back a crs
    print('Add back crs')
    terra::crs(eca.p.val.precursor.terra) <- terra::crs(var.1)
    terra::crs(eca.p.val.trigger.terra) <- terra::crs(var.1)
    terra::crs(eca.coin.rate.precursor.terra) <- terra::crs(var.1)
    terra::crs(eca.coin.rate.trigger.terra) <- terra::crs(var.1)
    
    # quantify coincidence rate as # of days out of 30660
    print('Quantify coincidence rate as # of hours')
    eca.coin.rate.precursor.terra <- eca.coin.rate.precursor.terra*dim(var.1)[3]
    eca.coin.rate.trigger.terra <- eca.coin.rate.trigger.terra*dim(var.1)[3]
    
    # multiply rasters to get statistically significant coincidence rates
    print('Calculate significant coincidence rates')
    eca.coin.days.precursor.terra <- round(eca.p.val.precursor.terra * eca.coin.rate.precursor.terra,0)
    eca.coin.days.trigger.terra <- round(eca.p.val.trigger.terra * eca.coin.rate.trigger.terra,0)
    
    # store in list item
    print('Write to list')
    eca.coin.days.precursor.terra.delT[[j+1]] <- eca.coin.days.precursor.terra
    eca.coin.days.trigger.terra.delT[[j+1]] <- eca.coin.days.trigger.terra
  }
  
  # stack list of rasters
  print('Stack list of rasters')
  eca.coin.days.precursor.terra.delT <- terra::rast(eca.coin.days.precursor.terra.delT)
  eca.coin.days.trigger.terra.delT <- terra::rast(eca.coin.days.trigger.terra.delT)
  
  # determine  max (max rate of eca)
  print('Determine max')
  eca.coin.days.precursor.terra.delT.max <- max(eca.coin.days.precursor.terra.delT)
  eca.coin.days.trigger.terra.delT.max <- max(eca.coin.days.trigger.terra.delT)
  
  # determine which max (day of max eca)
  print('Determine which max')
  eca.coin.days.precursor.terra.delT.max.which <- which.max(eca.coin.days.precursor.terra.delT)
  eca.coin.days.trigger.terra.delT.max.which <- which.max(eca.coin.days.trigger.terra.delT)

  # return result
  return()
  
}

validate.county <- function(raster.var,event.df,raster.ref,event.type){

  # debug
  # var <- raster.var
  # event.df <- flash.flood
  # raster.ref <- criteria.low
  # event.type <- 'Flash_Flood'
  
  # code
  var <- raster.var
  
  # Create lists
  event.id.list <- list()
  death.direct.list <- list()
  injury.direct.list <- list()
  damage.property.dollar.list <- list()
  damage.crop.dollar.list <- list()
  injury.indirect.list <- list()
  death.indirect.list <- list()
  
  # Start an index for the list object
  j <- 1
  
  # Loop through time slices of raster
  for (i in 1:terra::nlyr(var)){
    
    # Get time slice
    time.slice <- terra::time(var[[i]])
    
    # Identify active events during time slice
    active.event <- event.df %>%
      filter(START.TIMESTAMP <= time.slice & END.TIMESTAMP >= time.slice)
    
    # If there is an event, give event ID to county shapefile
    
    # If there is an active event, 
    if (nrow(active.event) > 0){
      
      # State when and how many events occurred 
      print(paste0('---------- ',nrow(active.event),' event(s) occurred on ',time.slice,' ----------'))
      
      # Create temporary shapefile of counties
      shp <- county_shapefile
      
      # Set all episode ID's to zero for time slice
      shp$EPISODE_ID <- 0
      shp$DEATH_DIRECT <- 0
      shp$INJURY_DIRECT <- 0
      shp$DAMAGE_PROPERTY_DOLLAR <- 0
      shp$DAMAGE_CROP_DOLLAR <- 0
      shp$INJURY_INDIRECT <- 0
      shp$DEATH_INDIRECT <- 0
      
      # For each active event during the time slice,
      for (l in 1:nrow(active.event)){
        active.event.temp <- active.event[l,]
        
        # Match the NOAA event location (State Abbr, County) with the corresponding location in the County shapefile
        for (k in 1:nrow(shp)){
          if (shp$STATE_ABBR[k] == active.event.temp$STATE_ABBR && shp$COUNTY[k] == active.event.temp$COUNTY){
            
            # Then, store the Event ID at that spatial location
            shp$EPISODE_ID[k] <- active.event.temp$EPISODE_ID
            shp$DEATH_DIRECT[k] <- active.event.temp$DEATHS_DIRECT
            shp$INJURY_DIRECT[k] <- active.event.temp$INJURIES_DIRECT
            shp$DAMAGE_PROPERTY_DOLLAR[k] <- active.event.temp$DAMAGE_PROPERTY_NUM
            shp$DAMAGE_CROP_DOLLAR[k] <- active.event.temp$DAMAGE_CROPS_NUM
            shp$INJURY_INDIRECT[k] <- active.event.temp$INJURIES_INDIRECT 
            shp$DEATH_INDIRECT[k] <- active.event.temp$DEATHS_INDIRECT
            
          } else {}
          
        }
        
      }
      
      # Once all locations are identified during the time slice, rasterize the resulting shapefile
      test.episode.id <- stars::st_rasterize(shp %>% dplyr::select(geom,EPISODE_ID))
      test.death.direct <- stars::st_rasterize(shp %>% dplyr::select(geom,DEATH_DIRECT))
      test.injury.direct <- stars::st_rasterize(shp %>% dplyr::select(geom,INJURY_DIRECT))
      test.damage.property <- stars::st_rasterize(shp %>% dplyr::select(geom,DAMAGE_PROPERTY_DOLLAR))
      test.damage.crop <- stars::st_rasterize(shp %>% dplyr::select(geom,DAMAGE_CROP_DOLLAR))
      test.injury.indirect <- stars::st_rasterize(shp %>% dplyr::select(geom,INJURY_INDIRECT))
      test.death.indirect <- stars::st_rasterize(shp %>% dplyr::select(geom,DEATH_INDIRECT))
      
      # Then, convert the raster into a terra spatraster object
      test.episode.id <- terra::rast(test.episode.id)
      test.death.direct <- terra::rast(test.death.direct)
      test.injury.direct <- terra::rast(test.injury.direct)
      test.damage.property <- terra::rast(test.damage.property)
      test.damage.crop <- terra::rast(test.damage.crop)
      test.injury.indirect <- terra::rast(test.injury.indirect)
      test.death.indirect <- terra::rast(test.death.indirect)
      
      # Then, resample the spatraster to the original study region 
      test.episode.id <- terra::resample(x=test.episode.id,y=criteria.low,method='near')
      test.death.direct <- terra::resample(x=test.death.direct,y=criteria.low,method='near')
      test.injury.direct <- terra::resample(x=test.injury.direct,y=criteria.low,method='near')
      test.damage.property <- terra::resample(x=test.damage.property,y=criteria.low,method='near')
      test.damage.crop <- terra::resample(x=test.damage.crop,y=criteria.low,method='near')
      test.injury.indirect <- terra::resample(x=test.injury.indirect,y=criteria.low,method='near')
      test.death.indirect <- terra::resample(x=test.death.indirect,y=criteria.low,method='near')
      
      # Then, add back the date time information to the new spatraster
      terra::time(test.episode.id) <- time.slice
      terra::time(test.death.direct) <- time.slice
      terra::time(test.injury.direct) <- time.slice
      terra::time(test.damage.property) <- time.slice
      terra::time(test.damage.crop) <- time.slice
      terra::time(test.injury.indirect) <- time.slice
      terra::time(test.death.indirect) <- time.slice
      
      # Then, store the new spatraster in a list object
      event.id.list[[j]] <- test.episode.id
      death.direct.list[[j]] <- test.death.direct
      injury.direct.list[[j]] <- test.injury.direct
      damage.property.dollar.list[[j]] <- test.damage.property
      damage.crop.dollar.list[[j]] <- test.damage.crop
      injury.indirect.list[[j]] <- test.injury.indirect
      death.indirect.list[[j]] <- test.death.indirect
      
      # Finally, update the list index
      j <- j + 1
      
    }
    
  }
  
  # Compile the list object of spatrasters into a single spatraster
  event.id.raster <- terra::rast(event.id.list)
  death.direct.raster <- terra::rast(death.direct.list)
  injury.direct.raster <- terra::rast(injury.direct.list)
  damage.property.dollar.raster <- terra::rast(damage.property.dollar.list)
  damage.crop.dollar.raster <- terra::rast(damage.crop.dollar.list)
  injury.indirect.raster <- terra::rast(injury.indirect.list)
  death.indirect.raster <- terra::rast(death.indirect.list)
  
  # Once the list object is finalized, remove all of the layers without any events. 
  # This corresponds to events that happened outside of the study region domain.
  
  # Create a new empty list
  event.id.list <- list()
  death.direct.list <- list()
  injury.direct.list <- list()
  damage.property.dollar.list <- list()
  damage.crop.dollar.list <- list()
  injury.indirect.list <- list()
  death.indirect.list <- list()
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(event.id.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(event.id.raster[[i]])) > 0){
      # Save that list item to the new list
      event.id.list[[j]] <- event.id.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(death.direct.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(death.direct.raster[[i]])) > 0){
      # Save that list item to the new list
      death.direct.list[[j]] <- death.direct.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(injury.direct.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(injury.direct.raster[[i]])) > 0){
      # Save that list item to the new list
      injury.direct.list[[j]] <- injury.direct.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(damage.property.dollar.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(damage.property.dollar.raster[[i]])) > 0){
      # Save that list item to the new list
      damage.property.dollar.list[[j]] <- damage.property.dollar.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(damage.crop.dollar.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(damage.crop.dollar.raster[[i]])) > 0){
      # Save that list item to the new list
      damage.crop.dollar.list[[j]] <- damage.crop.dollar.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(injury.indirect.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(injury.indirect.raster[[i]])) > 0){
      # Save that list item to the new list
      injury.indirect.list[[j]] <- injury.indirect.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(death.indirect.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(death.indirect.raster[[i]])) > 0){
      # Save that list item to the new list
      death.indirect.list[[j]] <- death.indirect.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Compile the list object of spatrasters into a single spatraster
  event.id.raster <- terra::rast(event.id.list)
  death.direct.raster <- terra::rast(death.direct.list)
  injury.direct.raster <- terra::rast(injury.direct.list)
  damage.property.dollar.raster <- terra::rast(damage.property.dollar.list)
  damage.crop.dollar.raster <- terra::rast(damage.crop.dollar.list)
  injury.indirect.raster <- terra::rast(injury.indirect.list)
  death.indirect.raster <- terra::rast(death.indirect.list)
  
  # Finally, save the NOAA Events spatraster as a netcdf
  save.cdf(raster = event.id.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Event_ID'),
           var.name = 'event_id',
           long.name = 'event_id',
           unit = 'event_id')
  
  save.cdf(raster = death.direct.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Direct_Death'),
           var.name = 'direct_death',
           long.name = 'direct_death',
           unit = 'count')
  
  save.cdf(raster = injury.direct.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Direct_Injury'),
           var.name = 'direct_injury',
           long.name = 'direct_injury',
           unit = 'count')
  
  save.cdf(raster = damage.property.dollar.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Damage_Property'),
           var.name = 'damage_property',
           long.name = 'damage_property',
           unit = 'dollar')
  
  save.cdf(raster = damage.crop.dollar.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Damage_Crop'),
           var.name = 'damage_crop',
           long.name = 'damage_crop',
           unit = 'dollar')
  
  save.cdf(raster = injury.indirect.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Indirect_Injury'),
           var.name = 'indirect_injury',
           long.name = 'indirect_injury',
           unit = 'count')
  
  save.cdf(raster = death.indirect.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Indirect_Death'),
           var.name = 'indirect_death',
           long.name = 'indirect_death',
           unit = 'count')

}

validate.event <- function(raster.var,event.df,raster.ref,event.type){
  
  # debug
  # var <- raster.var
  # event.df <- flash.flood
  # raster.ref <- criteria.low
  # event.type <- 'Flash_Flood'
  
  # code
  var <- raster.var
  
  # Create lists
  event.id.list <- list()
  
  # Start an index for the list object
  j <- 1
  
  # Loop through time slices of raster
  for (i in 1:terra::nlyr(var)){
    
    # Get time slice
    time.slice <- terra::time(var[[i]])
    
    # Identify active events during time slice
    active.event <- event.df %>%
      filter(START.TIMESTAMP <= time.slice & END.TIMESTAMP >= time.slice)
    
    # If there is an event, give event ID to county shapefile
    
    # If there is an active event, 
    if (nrow(active.event) > 0){
      
      # State when and how many events occurred 
      print(paste0('---------- ',nrow(active.event),' event(s) occurred on ',time.slice,' ----------'))
      
      # Create temporary shapefile of counties
      shp <- county_shapefile
      
      # Set all episode ID's to zero for time slice
      shp$EPISODE_ID <- 0
      
      # For each active event during the time slice,
      for (l in 1:nrow(active.event)){
        active.event.temp <- active.event[l,]
        
        # Match the NOAA event location (State Abbr, County) with the corresponding location in the County shapefile
        for (k in 1:nrow(shp)){
          if (shp$STATE_ABBR[k] == active.event.temp$STATE_ABBR && shp$COUNTY[k] == active.event.temp$COUNTY){
            
            # Then, store the Event ID at that spatial location
            shp$EPISODE_ID[k] <- active.event.temp$EPISODE_ID
            
          } else {}
          
        }
        
      }
      
      # Once all locations are identified during the time slice, rasterize the resulting shapefile
      test.episode.id <- stars::st_rasterize(shp %>% dplyr::select(geom,EPISODE_ID))
      
      # Then, convert the raster into a terra spatraster object
      test.episode.id <- terra::rast(test.episode.id)
      
      # Then, resample the spatraster to the original study region 
      test.episode.id <- terra::resample(x=test.episode.id,y=criteria.low,method='near')
      
      # Then, add back the date time information to the new spatraster
      terra::time(test.episode.id) <- time.slice
      
      # Then, store the new spatraster in a list object
      event.id.list[[j]] <- test.episode.id
      
      # Finally, update the list index
      j <- j + 1
      
    }
    
  }
  
  # Compile the list object of spatrasters into a single spatraster
  event.id.raster <- terra::rast(event.id.list)
  
  # Once the list object is finalized, remove all of the layers without any events. 
  # This corresponds to events that happened outside of the study region domain.
  
  # Create a new empty list
  event.id.list <- list()
  
  # Start an index for the list object
  j <- 1
  
  # Loop through list items
  for (i in 1:terra::nlyr(event.id.raster)){
    # If there is at least one event in a given list item
    if (max(minmax(event.id.raster[[i]])) > 0){
      # Save that list item to the new list
      event.id.list[[j]] <- event.id.raster[[i]]
      # Then, update the list index
      j <- j + 1
    } else {}
  }
  
  # Compile the list object of spatrasters into a single spatraster
  event.id.raster <- terra::rast(event.id.list)
  
  # Finally, save the NOAA Events spatraster as a netcdf
  save.cdf(raster = event.id.raster,
           folder.path = 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/validation/',
           file.name = paste0(event.type,'_Event_ID'),
           var.name = 'event_id',
           long.name = 'event_id',
           unit = 'event_id')
  
}

validate.figure <- function(model.raster,observed.raster,region.shapefile,figure.title,units){
 
  # debug
  # model.raster <- binary.var.2
  # observed.raster <- flood.injury.dir
  # region.shapefile <- us.states
  # figure.title <- 'Flood Injury Direct (2019-2023)'
  # units <- 'Count'
  
  # code
  # get time of events in each dataset
  date.time.cluster <- terra::time(model.raster)
  date.time.noaa <- terra::time(observed.raster)
  
  # create raster of clusters with a noaa event
  date.time <- match(date.time.cluster,date.time.noaa)
  date.time <- which(!is.na(date.time))
  
  # only continue if there are coinciding time slices
  if (length(date.time) > 0) {
    
    # subset cluster raster 
    raster.cluster <- model.raster[[date.time]]
    
    # create raster of noaa events with a cluster
    date.time <- match(date.time.noaa,date.time.cluster)
    date.time <- which(!is.na(date.time))
    
    # subset noaa raster
    raster.noaa <- observed.raster[[date.time]]
    
    # Multiply binary cluster raster against each NOAA event raster
    observed.raster <- raster.cluster * raster.noaa
    
    # Subset
    observed.raster <- terra::app(observed.raster,fun='sum',na.rm=T)
    
    # Count exceedances and convert to figure
    observed.raster <- format.figure(observed.raster)
    
    # Flash Flood
    observed.raster.plot <- ggplot() +
      geom_tile(data=observed.raster,aes(x=x,y=y,fill=sum)) +
      geom_sf(data=us.states,fill=NA) +
      scale_fill_distiller(limits=c(1,max(observed.raster$sum)),
                           breaks=round(seq(1,
                                            max(observed.raster$sum),
                                            ((max(observed.raster$sum)-1)/1)),0),
                           type='seq',palette='YlOrRd',direction = 1) +
      theme_bw() +
      ggtitle(figure.title) +
      labs(fill= units) +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
            plot.title = element_markdown(),legend.position = "bottom") +
      coord_sf(ylim = c(25,40),xlim = c(-95,-75)) +
      annotate(geom = 'text',label = 'Cluster & NOAA Events',x=-88,y=25)
    
    # return result
    return(observed.raster.plot)
    
  } else {print('No Coinciding Events')}
  
}

validate.figure.fraction <- function(model.raster,observed.raster,region.shapefile,figure.title,units,palette){
  
  # # # debug
  # model.raster <- clust.low
  # observed.raster <- excess.heat.event.id
  # region.shapefile <- us.states
  # figure.title <- 'Excessive Heat Event ID (2019-2023)'
  # units <- 'Day'
  
  # code
  # get time of events in each dataset
  date.time.cluster <- terra::time(model.raster)
  date.time.noaa <- terra::time(observed.raster)
  
  # create raster of clusters with a noaa event
  date.time <- match(date.time.cluster,date.time.noaa)
  date.time <- which(!is.na(date.time))
  
  # only continue if there are coinciding time slices
  if (length(date.time) > 0) {
    
    # subset cluster raster 
    raster.cluster <- model.raster[[date.time]]
    
    # 2. Identify layers that are entirely NaN
    # Use sapply to check each layer for all NaN values
    nan_layers <- sapply(1:nlyr(raster.cluster), function(i) {
      all(is.nan(values(raster.cluster[[i]])))
    })
    
    # 3. Keep only layers that are not entirely NaN
    # Subset the SpatRaster to keep non-NaN layers
    raster.cluster <- raster.cluster[[!nan_layers]]

    # create raster of noaa events with a cluster
    date.time <- match(date.time.noaa,date.time.cluster)
    date.time <- which(!is.na(date.time))
    
    # subset noaa raster
    raster.noaa <- observed.raster[[date.time]]

    # Convert noaa raster to binary
    raster.noaa <- raster.noaa / raster.noaa

    # Calculate Fraction of observed events that cluster model predicts
    observed.raster <- raster.cluster * raster.noaa

    # calculate total number of overlapping events
    observed.raster <- terra::app(observed.raster,fun='sum',na.rm=T) #
    # observed.raster <- terra::app(observed.raster, sum_with_na)

    # calculate total number of noaa events 
    raster.noaa <- terra::app(raster.noaa,fun='sum',na.rm=T) #
    # raster.noaa <- terra::app(raster.noaa, sum_with_na)

    # calculate fraction 
    observed.raster <- observed.raster / raster.noaa
    # plot(observed.raster)
    # Count exceedances and convert to figure
    observed.raster <- format.figure(observed.raster)
    
    # Flash Flood
    observed.raster.plot <- ggplot() +
      geom_tile(data=observed.raster,aes(x=x,y=y,fill=sum)) +
      geom_sf(data=us.states,fill=NA) +
      scale_fill_distiller(limits=c(0,1),
                           breaks=seq(0,1,.5),
                           type='seq',palette=palette,direction = 1) +
      theme_bw() +
      ggtitle(figure.title) +
      labs(fill= units) +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
            plot.title = element_markdown(),legend.position = "bottom") +
      coord_sf(ylim = c(25,40),xlim = c(-95,-75)) +
      annotate(geom = 'text',label = 'Cluster Validation',x=-88,y=25)
    
    # return result
    return(observed.raster.plot)
    
  } else {print('No Coinciding Events')}
  
}

noaa.figure <- function(observed.raster,region.shapefile,figure.title,units,palette){
  
  # debug
  # observed.raster <- flash.flood.damage.prop
  # region.shapefile <- us.states
  # figure.title <- 'Flash Flood Property Damage (2019-2023)'
  # units <- '$'
  
  # code
  observed.raster <- terra::app(observed.raster,fun='sum',na.rm=T)
  
  # Count exceedances and convert to figure
  observed.raster <- format.figure(observed.raster)
  
  # Flash Flood
  observed.raster.plot <- ggplot() +
    geom_tile(data=observed.raster,aes(x=x,y=y,fill=sum)) +
    geom_sf(data=us.states,fill=NA) +
    scale_fill_distiller(limits=c(1,max(observed.raster$sum)),
                         breaks=round(seq(1,
                                          max(observed.raster$sum),
                                          ((max(observed.raster$sum)-1)/1)),0),
                         type='seq',palette=palette,direction = 1) +
    theme_bw() +
    ggtitle(figure.title) +
    labs(fill= units) +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          plot.title = element_markdown(),legend.position = "bottom") +
    coord_sf(ylim = c(25,40),xlim = c(-95,-75)) +
    annotate(geom = 'text',label = 'NOAA Events',x=-90,y=25)
  
  # return result
  return(observed.raster.plot)
  
}

cluster.figure <- function(model.raster,region.shapefile,figure.title,units,palette){
  
  # debug
  # model.raster <- binary.var.2
  # region.shapefile <- us.states
  # figure.title <- 'Flash Flood Property Damage (2019-2023)'
  # units <- 'Hour'
  
  # code
  model.raster <- terra::app(model.raster,fun='sum',na.rm=T)
  
  # Count exceedances and convert to figure
  model.raster <- format.figure(model.raster)
  
  # Flash Flood
  model.raster.plot <- ggplot() +
    geom_tile(data=model.raster,aes(x=x,y=y,fill=sum)) +
    geom_sf(data=us.states,fill=NA) +
    scale_fill_distiller(limits=c(1,max(model.raster$sum)),
                         breaks=round(seq(1,
                                          max(model.raster$sum),
                                          ((max(model.raster$sum)-1)/1)),0),
                         type='seq',palette=palette,direction = 1) +
    theme_bw() +
    ggtitle(figure.title) +
    labs(fill= units) +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          plot.title = element_markdown(),legend.position = "bottom") +
    coord_sf(ylim = c(25,40),xlim = c(-95,-75)) +
    annotate(geom = 'text',label = 'Cluster',x=-90,y=25)
  
  # return result
  return(model.raster.plot)
  
}

figure.3 <- function(formatted.figure,figure.title,shapefile,figure.units,figure.palette){
  
  # create figure
  figure.plot <- ggplot() +
    geom_tile(data=formatted.figure,aes(x=x,y=y,fill=sum)) +
    geom_sf(data=shapefile,fill=NA) +
    scale_fill_distiller(limits=c(1,max(formatted.figure$sum)),
                         breaks=round(seq(1,
                                          max(formatted.figure$sum),
                                          ((max(formatted.figure$sum)-1)/3)),0),
                         type='seq',palette=figure.palette,direction = 1) +
    theme_bw() + 
    ggtitle(figure.title) +
    labs(fill= figure.units) +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          plot.title = element_markdown(),legend.position = "bottom") +
    coord_sf(ylim = c(25,40),xlim = c(-95,-75))
  
  # return result
  return(figure.plot)
  
}

trend.analysis.heat <- function(directory){
  
  # # debug
  # directory <- 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/heat_index/warning/'
  
  # get directories
  directory.metadata <- paste0(directory,'metadata')
  directory.parameters <- paste0(directory,'parameters')
  
  # Read in all metadata 
  filenames <- list.files(directory.metadata, pattern="*.csv", full.names=TRUE)
  metadata <- lapply(filenames, read.csv)
  
  # Read in all parameters
  filenames <- list.files(directory.parameters, pattern="*.csv", full.names=TRUE)
  epsilon <- lapply(filenames, read.csv)
  
  # Combine metadata and parameters
  cluster.info <- list()
  
  # append epsilon columns to end of metadata columns
  for (i in 1:length(metadata)){
    
    # i <- 1
    
    # get temp files
    metadata.temp <- metadata[[i]]
    epsilon.temp <- epsilon[[i]]
    
    # edit event.count column with nrow of metadata.temp
    epsilon.temp$event.count <- nrow(metadata.temp)
    
    # repeat epsilon.temp to be equal nrows as metadata.temp
    epsilon.temp <- cbind(epsilon.temp, rep(row.names(epsilon.temp), each = nrow(metadata.temp)))
    
    # remove X columns from both dataframes
    metadata.temp <- metadata.temp[,-1]
    epsilon.temp <- epsilon.temp[,-c(1,6)]
    
    # rename dataframe columns
    colnames(epsilon.temp) <- c('year','mu','epsilon','event.count')
    
    # combine and append to cluster.info list
    cluster.info[[i]] <- cbind(metadata.temp,epsilon.temp)
    
  }
  
  ## Cluster.info
  
  # unlist
  cluster.info <- data.table::rbindlist(cluster.info)

  # order time
  cluster.info$year <- as.ordered(cluster.info$year)
  
  # plot duration over time
  plot.duration <- ggplot(data = cluster.info, aes(x=year,y=duration)) +
    geom_boxplot() + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Event Duration (Days)',x='Year') 
  
  # plot fraction of domain over time
  plot.fraction <- ggplot(data = cluster.info, aes(x=year,y=fraction)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Exposed Area (% of Domain)',x='Year') 
  
  # plot intensity (max) over time
  plot.max <- ggplot(data = cluster.info, aes(x=year,y=max.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Maximum Heat Index (°C)',x='Year') 
  
  # plot intensity (mean) over time
  plot.mean <- ggplot(data = cluster.info, aes(x=year,y=mean.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Mean Heat Index (°C)',x='Year') 
  
  # plot intensity (median) over time
  plot.median <- ggplot(data = cluster.info, aes(x=year,y=med.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Median Heat Index (°C)',x='Year') 
  
  # plot intensity (mean) over time
  plot.min <- ggplot(data = cluster.info, aes(x=year,y=min.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Minimum Heat Index (°C)',x='Year')
  
  ## Epsilon and # Events
  # unlist
  epsilon <- data.table::rbindlist(epsilon)
  
  # sum(epsilon$event.count)
  
  ## Calculate validation event count
  # epsilon.val <- epsilon[year >= 2019,]
  # sum(epsilon.val$event.count)
  
  # order time
  # epsilon$year <- as.ordered(epsilon$year)
  
  # plot epsilon over time
  plot.epsilon <- ggplot(data = epsilon, aes(x=year,y=epsilon)) +
    geom_point() + 
    geom_smooth(method=lm,se=TRUE,color='red') + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Epsilon (Search Radius)',x='Year') 
  
  # plot events over time
  plot.events <- ggplot(data = epsilon, aes(x=year,y=event.count)) +
    geom_point() + 
    geom_smooth(method=lm,se=TRUE,color='red') + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Count of Events',x='Year')
  
  # Combine Results
  figure.organization <- list(plot.events,plot.duration,plot.fraction,
                              plot.epsilon,plot.median,plot.mean)
  
  figure.organization <- ggpubr::ggarrange(plotlist=figure.organization,nrow=2,ncol = 3)
  
  # return result
  return(figure.organization)
  
}

trend.analysis.rain <- function(directory){
  
  # debug
  # directory <- 'V:/users/hquintal/PhD2_Cluster_Parameters/data/out/cluster/precipitation/001/'
  
  # get directories
  directory.metadata <- paste0(directory,'metadata')
  directory.parameters <- paste0(directory,'parameters')
  
  # Read in all metadata 
  filenames <- list.files(directory.metadata, pattern="*.csv", full.names=TRUE)
  metadata <- lapply(filenames, read.csv)
  
  # Read in all parameters
  filenames <- list.files(directory.parameters, pattern="*.csv", full.names=TRUE)
  epsilon <- lapply(filenames, read.csv)
  
  # Combine metadata and parameters
  cluster.info <- list()
  
  # append epsilon columns to end of metadata columns
  for (i in 1:length(metadata)){
    
    # i <- 1
    
    # get temp files
    metadata.temp <- metadata[[i]]
    epsilon.temp <- epsilon[[i]]
    
    # edit event.count column with nrow of metadata.temp
    epsilon.temp$event.count <- nrow(metadata.temp)
    
    # repeat epsilon.temp to be equal nrows as metadata.temp
    epsilon.temp <- cbind(epsilon.temp, rep(row.names(epsilon.temp), each = nrow(metadata.temp)))
    
    # remove X columns from both dataframes
    metadata.temp <- metadata.temp[,-1]
    epsilon.temp <- epsilon.temp[,-c(1,6)]
    
    # rename dataframe columns
    colnames(epsilon.temp) <- c('year','mu','epsilon','event.count')
    
    # combine and append to cluster.info list
    cluster.info[[i]] <- cbind(metadata.temp,epsilon.temp)
    
  }
  
  # delete list items with fewer than 19 columns
  final.info <- list()
  counter <- 1
  
  for (h in 1:length(cluster.info)){
    
    # h <- 1
    
    # get temp files
    cluster.temp <- cluster.info[[h]]
    
    if (ncol(cluster.temp) == 19){
      
      # update new list
      final.info[[counter]] <- cluster.temp
      
      # update counter
      counter <- counter + 1
      
    } else {}
    
  }
  
  # rename final.info
  cluster.info <- final.info
  
  ## Cluster.info
  
  # unlist
  cluster.info <- data.table::rbindlist(cluster.info)
  
  # order time
  cluster.info$year <- as.ordered(cluster.info$year)
  
  # plot duration over time
  plot.duration <- ggplot(data = cluster.info, aes(x=year,y=duration)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Event Duration (Days)',x='Year') 
  
  # plot fraction of domain over time
  plot.fraction <- ggplot(data = cluster.info, aes(x=year,y=fraction)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Exposed Area (% of Domain)',x='Year') 
  
  # plot intensity (max) over time
  plot.max <- ggplot(data = cluster.info, aes(x=year,y=max.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Maximum Precipitation (mm)',x='Year') 
  
  # plot intensity (mean) over time
  plot.mean <- ggplot(data = cluster.info, aes(x=year,y=mean.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Mean Precipitation (mm)',x='Year') 
  
  # plot intensity (mean) over time
  plot.median <- ggplot(data = cluster.info, aes(x=year,y=med.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Median Precipitation (mm)',x='Year')
  
  # plot intensity (mean) over time
  plot.min <- ggplot(data = cluster.info, aes(x=year,y=min.upper)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Minimum Precipitation (mm)',x='Year') 
  
  ## Epsilon and # Events
  # unlist
  epsilon <- data.table::rbindlist(epsilon)
  
  # sum(epsilon$event.count)
  
  ## Calculate validation event count
  # epsilon.val <- epsilon[year >= 2019,]
  # sum(epsilon.val$event.count)
  
  # plot epsilon over time
  plot.epsilon <- ggplot(data = epsilon, aes(x=year,y=epsilon)) +
    geom_point() + 
    geom_smooth(method=lm,se=TRUE,color='red') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Epsilon (Search Radius)',x='Year')
  
  # plot events over time
  plot.events <- ggplot(data = epsilon, aes(x=year,y=event.count)) +
    geom_point() + 
    geom_smooth(method=lm,se=TRUE,color='red') + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)) +
    labs(y='Count of Events',x='Year') 

  # Combine Results
  figure.organization <- list(plot.events,plot.duration,plot.fraction,
                              plot.epsilon,plot.median,plot.mean)
  
  figure.organization <- ggpubr::ggarrange(plotlist=figure.organization,nrow=2,ncol = 3)
  
  # return result
  return(figure.organization)
  
}

# Function to set future occurrences of values to NA
mask_future_layers <- function(raster) {
  # Get the total number of layers
  n_layers <- nlyr(raster)
  
  # Loop over each layer except the last one
  for (i in 1:(n_layers - 1)) {
    print(i)
    
    # Extract the current layer's values
    current_values <- values(raster[[i]])
    
    # Identify the positions with non-NA values
    valid_positions <- which(!is.na(current_values))
    
    # Loop through subsequent layers to set these positions to NA
    for (j in (i + 1):n_layers) {
      # Extract the subsequent layer's values
      layer_values <- values(raster[[j]])
      
      # Set the identified positions to NA
      layer_values[valid_positions] <- NA
      
      # Update the layer with the modified values
      values(raster[[j]]) <- layer_values
    }
  }
  
  return(raster)
}

floor_decade <- function(value){ return(value - value %% 10) }

# https://stackoverflow.com/questions/28819761/how-to-split-a-decimal-number-from-a-string-in-r
getNumberPart <- function(x) {
  pat <- "(-?(\\d*\\.*\\d+|\\d+\\.))"
  strapply(x, pattern=pat, FUN=as.numeric, simplify=TRUE, empty=NA)
}