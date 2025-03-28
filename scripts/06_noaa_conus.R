#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: rasterize NOAA Events Database by county for validation of clustered events

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/06_noaa_conus.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Download bulk files manually ----
# (Files available at: https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/)

## Read in Koppen-Geiger geotiff ----
directory <- here::here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")
resolution <- terra::rast(directory)
resolution <- resolution / resolution

# Load US states data using the maps package and convert to an sf object
us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
# Convert to spatvector
us.states.vect <- terra::vect(us.states)
# Rasterize using the resolution from the Koppen-Geiger file
us.states.rast <- terra::rasterize(us.states.vect, resolution, field = 'ID')
# Crop to CONUS extent (approximate)
us.states.rast <- terra::crop(us.states.rast, terra::ext(-130, -65, 24, 50))

# Load NOAA events CSV ----
files <- list.files(path = here::here("data", "input", "noaa_events"), full.names = TRUE)
files <- files[2:length(files)]
events <- do.call(rbind, lapply(files, function(x) data.table::fread(x, stringsAsFactors = FALSE)))
# Remove event narrative columns
events$EVENT_NARRATIVE <- NULL
events$EPISODE_NARRATIVE <- NULL
# Keep only relevant event types (update indices as needed)
relevant <- unique(events$EVENT_TYPE)
relevant <- relevant[c(5,8,14,15,16,18,23,30,31,42,44,54,62,65,67,69,70)]
events <- events[events$EVENT_TYPE %in% relevant]
# Keep only events with EPISODE_ID
events <- events[!is.na(events$EPISODE_ID)]

# Update NOAA events date ----
# Process BEGIN_DAY to ensure two-digit day values
BEGIN_DAY <- events$BEGIN_DAY
BEGIN_DAY_TEMP <- vector("list", length(BEGIN_DAY))
for (i in 1:length(BEGIN_DAY)){
  print(i)
  if ((floor(log10(BEGIN_DAY[[i]])) + 1) < 2){
    BEGIN_DAY_TEMP[[i]] <- paste0("0", BEGIN_DAY[[i]])
  } else {
    BEGIN_DAY_TEMP[[i]] <- as.character(BEGIN_DAY[[i]])
  }
}
BEGIN_DATE <- paste0(events$BEGIN_YEARMONTH, BEGIN_DAY_TEMP)
events$BEGIN_DATE <- as.Date(BEGIN_DATE, '%Y%m%d')

# Process END_DAY similarly
END_DAY <- events$END_DAY
END_DAY_TEMP <- vector("list", length(END_DAY))
for (i in 1:length(END_DAY)){
  print(i)
  if ((floor(log10(END_DAY[[i]])) + 1) < 2){
    END_DAY_TEMP[[i]] <- paste0("0", END_DAY[[i]])
  } else {
    END_DAY_TEMP[[i]] <- as.character(END_DAY[[i]])
  }
}
gc()
END_DATE <- paste0(events$END_YEARMONTH, END_DAY_TEMP)
events$END_DATE <- as.Date(END_DATE, '%Y%m%d')

# Update NOAA events time ----
# Process BEGIN_TIME
BEGIN_TIME <- events$BEGIN_TIME
BEGIN_TIME_TEMP <- vector("list", length(BEGIN_TIME))
for (i in 1:length(BEGIN_TIME)){
  print(i)
  count <- nchar(BEGIN_TIME[[i]])
  if (count < 4){
    if (count == 1){
      BEGIN_TIME_TEMP[[i]] <- paste0('000', BEGIN_TIME[[i]], '00')
    } else if (count == 2){
      BEGIN_TIME_TEMP[[i]] <- paste0('00', BEGIN_TIME[[i]], '00')
    } else {
      BEGIN_TIME_TEMP[[i]] <- paste0('0', BEGIN_TIME[[i]], '00')
    }
  } else {
    BEGIN_TIME_TEMP[[i]] <- paste0(BEGIN_TIME[[i]], '00')
  }
}
gc()
BEGIN_TIME_TEMP <- unlist(BEGIN_TIME_TEMP)
BEGIN_DATE_TIME <- paste(BEGIN_DATE, BEGIN_TIME_TEMP)
BEGIN_DATE_TIME <- lubridate::ymd_hms(BEGIN_DATE_TIME)
events$BEGIN_DATE_TIME_COMPLETE <- BEGIN_DATE_TIME

# Process END_TIME similarly
END_TIME <- events$END_TIME
END_TIME_TEMP <- vector("list", length(END_TIME))
for (i in 1:length(END_TIME)){
  print(i)
  count <- nchar(END_TIME[[i]])
  if (count < 4){
    if (count == 1){
      END_TIME_TEMP[[i]] <- paste0('000', END_TIME[[i]], '00')
    } else if (count == 2){
      END_TIME_TEMP[[i]] <- paste0('00', END_TIME[[i]], '00')
    } else {
      END_TIME_TEMP[[i]] <- paste0('0', END_TIME[[i]], '00')
    }
  } else {
    END_TIME_TEMP[[i]] <- paste0(END_TIME[[i]], '00')
  }
}
gc()
END_TIME_TEMP <- unlist(END_TIME_TEMP)
END_DATE_TIME <- paste(END_DATE, END_TIME_TEMP)
END_DATE_TIME <- lubridate::ymd_hms(END_DATE_TIME)
events$END_DATE_TIME_COMPLETE <- END_DATE_TIME

# Subset to only CONUS FIPS
unique.fips <- sort(unique(events$STATE_FIPS))
events <- events[events$STATE_FIPS %in% 1:56]
# (Assuming a temporary object 'temp' exists; adjust as needed)
events <- temp

# Define time zones and update events with corrected time zones
tz_combine <- data.table(tz = unique(events$CZ_TIMEZONE),
                         region = c("America/Chicago","America/New_York",
                                    "America/Los_Angeles","America/Denver",
                                    "America/Anguilla","America/Adak",
                                    "Pacific/Samoa","America/Chicago",
                                    "America/New_York","America/Los_Angeles",
                                    "America/Anguilla","America/Adak",
                                    "America/Denver","America/Anchorage",
                                    "Pacific/Samoa","Asia/Muscat",
                                    "America/Los_Angeles","America/Chicago",
                                    "America/New_York"))
time.zone.list <- vector("list", length(events$CZ_TIMEZONE))
for (i in 1:length(events$CZ_TIMEZONE)){
  print(i)
  temp.pos <- tz_combine == events$CZ_TIMEZONE[[i]]
  temp.pos <- which(temp.pos == TRUE)
  time.zone.list[[i]] <- tz_combine$region[[temp.pos]]
}
events$TIME.ZONE <- unlist(time.zone.list)
events <- temp

# Update events start and end times with the correct time zones
events[, LOCAL_TIME := as.character(BEGIN_DATE_TIME_COMPLETE)]
events[, LOCAL_TIME := purrr::map2(LOCAL_TIME, TIME.ZONE, ~ as.POSIXct(.x, tz = .y))]
events[, LOCAL_TIME := unlist(LOCAL_TIME)]
events[, START.TIMESTAMP.UTC := as.POSIXct(LOCAL_TIME, tz = "UTC")]
events[, LOCAL_TIME2 := as.character(END_DATE_TIME_COMPLETE)]
events[, LOCAL_TIME2 := purrr::map2(LOCAL_TIME2, TIME.ZONE, ~ as.POSIXct(.x, tz = .y))]
events[, LOCAL_TIME2 := unlist(LOCAL_TIME2)]
events[, END.TIMESTAMP.UTC := as.POSIXct(LOCAL_TIME2, tz = "UTC")]

events <- as.data.table(events)
# Remove superfluous columns
events <- events %>%
  select(-c(BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME, END_YEARMONTH, END_DAY, END_TIME,
            YEAR, MONTH_NAME, CZ_TYPE, CZ_FIPS, BEGIN_DATE_TIME, CZ_TIMEZONE,
            END_DATE_TIME, SOURCE, MAGNITUDE, MAGNITUDE_TYPE, FLOOD_CAUSE, CATEGORY,
            TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, TOR_OTHER_WFO, TOR_OTHER_CZ_STATE,
            TOR_OTHER_CZ_FIPS, TOR_OTHER_CZ_NAME, BEGIN_RANGE, BEGIN_AZIMUTH,
            BEGIN_LOCATION, END_RANGE, END_AZIMUTH, END_LOCATION, BEGIN_LAT,
            BEGIN_LON, END_LAT, END_LON, DATA_SOURCE, BEGIN_DATE, END_DATE,
            BEGIN_DATE_TIME_COMPLETE, END_DATE_TIME_COMPLETE, TIME.ZONE, LOCAL_TIME,
            LOCAL_TIME2))
# Save result
write.csv(events, here::here("data", "output", "04_noaa", "NOAA_events.csv"))

# Read result
events <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events.csv")))

# Process damage columns: assign 0.00K or 0 where needed
events$DAMAGE_PROPERTY[nchar(events$DAMAGE_PROPERTY) < 2] <- '0.00K'
events$DAMAGE_CROPS[nchar(events$DAMAGE_CROPS) < 2] <- '0.00K'
events$INJURIES_DIRECT[nchar(events$INJURIES_DIRECT) < 1] <- 0
events$INJURIES_INDIRECT[nchar(events$INJURIES_INDIRECT) < 1] <- 0
events$DEATHS_DIRECT[nchar(events$DEATHS_DIRECT) < 1] <- 0
events$DEATHS_INDIRECT[nchar(events$DEATHS_INDIRECT) < 1] <- 0

# Separate numbers and letters for damage fields
events <- events %>%
  mutate(DAMAGE_PROPERTY_INT = getNumberPart(DAMAGE_PROPERTY)) %>%
  mutate(DAMAGE_PROPERTY_CHAR = gsub("[^a-zA-Z]", "", DAMAGE_PROPERTY))
events <- events %>%
  mutate(DAMAGE_CROPS_INT = getNumberPart(DAMAGE_CROPS)) %>%
  mutate(DAMAGE_CROPS_CHAR = gsub("[^a-zA-Z]", "", DAMAGE_CROPS))

# Save result (first time)
write.csv(events, here::here("data", "output", "04_noaa", "NOAA_events.csv"))
# Read result again
events <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events.csv")))

# Convert damage fields into dollars
events$DAMAGE_PROPERTY_CHAR <- as.numeric(gsub('K', 1000,
                                               gsub('M', 1000000,
                                                    gsub('B', 1000000000,
                                                         events$DAMAGE_PROPERTY_CHAR))))
events$DAMAGE_CROPS_CHAR <- as.numeric(gsub('K', 1000,
                                            gsub('M', 1000000,
                                                 gsub('B', 1000000000,
                                                      events$DAMAGE_CROPS_CHAR))))
events$DAMAGE_PROPERTY <- events$DAMAGE_PROPERTY_CHAR * events$DAMAGE_PROPERTY_INT
events$DAMAGE_CROPS <- events$DAMAGE_CROPS_CHAR * events$DAMAGE_CROPS_INT
# Remove remaining NAs and superfluous columns
events[is.na(events$DAMAGE_PROPERTY)] <- 0
events[is.na(events$DAMAGE_CROPS)] <- 0
events <- events %>%
  select(-c(DAMAGE_PROPERTY_INT, DAMAGE_PROPERTY_CHAR, DAMAGE_CROPS_INT, DAMAGE_CROPS_CHAR, X.1, X))
# Remove events without date times
events <- events[complete.cases(events),]

# Update event location to state,county format to match CONUS county raster
state.county <- unique(us.states.rast$ID)
events <- events %>%
  mutate(EVENT_LOCATION = paste0(tolower(STATE), ',', tolower(CZ_NAME)))
events.list <- list()
county.noaa <- sub("^.*?,", "", events$EVENT_LOCATION)
county.noaa <- gsub("[[:punct:]]", " ", county.noaa)
county.noaa <- stringr::str_squish(county.noaa)
for (i in 1:nrow(state.county)){
  temp.county <- sub("^.*?,", "", state.county[i,])
  temp.county <- gsub("[[:punct:]]", " ", temp.county)
  temp <- events[county.noaa %like% temp.county,]
  temp.state.noaa <- temp[temp$EVENT_LOCATION == state.county[i,]]
  print(paste0(i, ' ', state.county[i,], ' had ', nrow(temp.state.noaa), ' events'))
  events.list[[i]] <- temp.state.noaa
}
events <- dplyr::bind_rows(events.list, .id = 'column_label')
events <- events %>% select(-c(column_label, STATE, STATE_FIPS, CZ_NAME))
# Save final NOAA events result
write.csv(events, here::here("data", "output", "04_noaa", "NOAA_events.csv"))
events <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events.csv")))
events$START.TIMESTAMP.UTC <- as.Date(events$START.TIMESTAMP.UTC)
events$END.TIMESTAMP.UTC <- as.Date(events$END.TIMESTAMP.UTC)

# Subset by event type
excess.heat <- events[which(events$EVENT_TYPE == "Excessive Heat"),] 
flash.flood <- events[which(events$EVENT_TYPE == "Flash Flood"),] 
flood <- events[which(events$EVENT_TYPE == "Flood"),] 
heat <- events[which(events$EVENT_TYPE == "Heat"),] 
heavy.rain <- events[which(events$EVENT_TYPE == "Heavy Rain"),]
hurricane <- events[which(events$EVENT_TYPE == "Hurricane"),]
typhoon <- events[which(events$EVENT_TYPE == "Hurricane (Typhoon)"),]
tropical.storm <- events[which(events$EVENT_TYPE == "Tropical Storm"),]
tropical.depression <- events[which(events$EVENT_TYPE == "Tropical Depression"),]
noaa.heat <- rbind(excess.heat, heat)
noaa.precipitation <- rbind(flash.flood, flood, heavy.rain, hurricane, typhoon, tropical.storm, tropical.depression)

# Save separate event type files
write.csv(excess.heat, here::here("data", "output", "04_noaa", "NOAA_events_excess_heat.csv"))
write.csv(flash.flood, here::here("data", "output", "04_noaa", "NOAA_events_flash_flood.csv"))
write.csv(flood, here::here("data", "output", "04_noaa", "NOAA_events_flood.csv"))
write.csv(heat, here::here("data", "output", "04_noaa", "NOAA_events_heat.csv"))
write.csv(heavy.rain, here::here("data", "output", "04_noaa", "NOAA_events_heavy_rain.csv"))
write.csv(hurricane, here::here("data", "output", "04_noaa", "NOAA_events_hurricane.csv"))
write.csv(typhoon, here::here("data", "output", "04_noaa", "NOAA_events_typhoon.csv"))
write.csv(tropical.storm, here::here("data", "output", "04_noaa", "NOAA_events_tropical_storm.csv"))
write.csv(tropical.depression, here::here("data", "output", "04_noaa", "NOAA_events_tropical_depression.csv"))
write.csv(noaa.heat, here::here("data", "output", "04_noaa", "NOAA_events_all_heat.csv"))
write.csv(noaa.precipitation, here::here("data", "output", "04_noaa", "NOAA_events_all_precipitation.csv"))
write.csv(events, here::here("data", "output", "04_noaa", "NOAA_events.csv"))

# Read results for further processing
excess.heat <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_excess_heat.csv")))
flash.flood <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_flash_flood.csv")))
flood <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_flood.csv")))
heat <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_heat.csv")))
heavy.rain <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_heavy_rain.csv")))
hurricane <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_hurricane.csv")))
typhoon <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_typhoon.csv")))
tropical.storm <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_tropical_storm.csv")))
tropical.depression <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_tropical_depression.csv")))
noaa.heat <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_all_heat.csv")))
noaa.precipitation <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events_all_precipitation.csv")))
events <- as.data.table(read.csv(here::here("data", "output", "04_noaa", "NOAA_events.csv")))
events$START.TIMESTAMP.UTC <- as.Date(events$START.TIMESTAMP.UTC)
events$END.TIMESTAMP.UTC <- as.Date(events$END.TIMESTAMP.UTC)


# Combine NOAA events (by episode) with CONUS county raster for each event type

# 1. NOAA: Excessive Heat ---- 
excess.heat.episode <- unique(excess.heat$EPISODE_ID)
for (i in 1:length(excess.heat.episode)){
  print(paste0('NOAA Excess Heat Episode # ', excess.heat.episode[[i]], ' ', round(i/length(excess.heat.episode),2)*100, ' % Complete'))
  temp <- excess.heat[excess.heat$EPISODE_ID == excess.heat.episode[[i]]]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)){
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = excess.heat.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)){
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "excess_heat"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_excess_heat_', excess.heat.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Excess Heat Episode ID',
           unit = 'ID')
  gc()
}

# 2. NOAA: Flash Flood ---- 
flash.flood.episode <- unique(flash.flood$EPISODE_ID)
for (i in 1:length(flash.flood.episode)){
  print(paste0('NOAA Flash Flood Episode # ', flash.flood.episode[[i]], ' ', round(i/length(flash.flood.episode),2)*100, ' % Complete'))
  temp <- flash.flood[flash.flood$EPISODE_ID == flash.flood.episode[[i]]]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)){
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = flash.flood.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)){
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "flash_flood"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_flash_flood_', flash.flood.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Flash Flood Episode ID',
           unit = 'ID')
  gc()
}

# 3. NOAA: Flood ---- 
flood.episode <- unique(flood$EPISODE_ID)
for (i in 1:length(flood.episode)){
  print(paste0('NOAA Flood Episode # ', flood.episode[[i]], ' ', round(i/length(flood.episode),2)*100, ' % Complete'))
  temp <- flood[flood$EPISODE_ID == flood.episode[[i]]]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)){
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = flood.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)){
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "flood"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_flood_', flood.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Flood Episode ID',
           unit = 'ID')
  gc()
}

# 4. NOAA: Heat ---- 
heat.episode <- unique(heat$EPISODE_ID)
for (i in 1:length(heat.episode)){
  print(paste0('NOAA Heat Episode # ', heat.episode[[i]], ' ', round(i/length(heat.episode),2)*100, ' % Complete'))
  temp <- heat[heat$EPISODE_ID == heat.episode[[i]]]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)){
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = heat.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)){
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "heat"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_heat_', heat.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Heat Episode ID',
           unit = 'ID')
  gc()
}

# 5. NOAA: Heavy Rain ---- 
heavy.rain.episode <- unique(heavy.rain$EPISODE_ID)
for (i in 1:length(heavy.rain.episode)){
  print(paste0('NOAA Heavy Rain Episode # ', heavy.rain.episode[[i]], ' ', round(i/length(heavy.rain.episode),2)*100, ' % Complete'))
  temp <- heavy.rain[heavy.rain$EPISODE_ID == heavy.rain.episode[[i]]]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)){
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = heavy.rain.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)){
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "heavy_rain"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_heavy_rain_', heavy.rain.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Heavy Rain Episode ID',
           unit = 'ID')
  gc()
}

# 6. NOAA: Hurricane ---- 
hurricane.episode <- unique(hurricane$EPISODE_ID)
for (i in 1:length(hurricane.episode)){
  print(paste0('NOAA Hurricane Episode # ', hurricane.episode[[i]], ' ', round(i/length(hurricane.episode),2)*100, ' % Complete'))
  temp <- hurricane[hurricane$EPISODE_ID == hurricane.episode[[i]]]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)){
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = hurricane.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)){
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "hurricane"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_hurricane_', hurricane.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Hurricane Episode ID',
           unit = 'ID')
  gc()
}

# 7. NOAA: Typhoon ---- 
typhoon.episode <- unique(typhoon$EPISODE_ID)
for (i in 1:length(typhoon.episode)){
  print(paste0('NOAA Typhoon Episode # ', typhoon.episode[[i]], ' ', round(i/length(typhoon.episode),2)*100, ' % Complete'))
  temp <- typhoon[typhoon$EPISODE_ID == typhoon.episode[[i]]]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)){
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = typhoon.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)){
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "typhoon"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_typhoon_', typhoon.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Typhoon Episode ID',
           unit = 'ID')
  gc()
}

# 8. NOAA: Tropical Storm ---- 
tropical.storm.episode <- unique(tropical.storm$EPISODE_ID)
for (i in 1:length(tropical.storm.episode)) {
  print(paste0('NOAA Tropical Storm Episode # ', tropical.storm.episode[[i]], ' ', 
               round(i/length(tropical.storm.episode), 2) * 100, ' % Complete'))
  temp <- tropical.storm[tropical.storm$EPISODE_ID == tropical.storm.episode[[i]], ]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)) {
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = tropical.storm.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)) {
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "tropical_storm"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_tropical_storm_', tropical.storm.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Tropical Storm Episode ID',
           unit = 'ID')
  gc()
}

# 9. NOAA: Tropical Depression ---- 
tropical.depression.episode <- unique(tropical.depression$EPISODE_ID)
for (i in 1:length(tropical.depression.episode)) {
  print(paste0('NOAA Tropical Depression Episode # ', tropical.depression.episode[[i]], ' ', 
               round(i/length(tropical.depression.episode), 2) * 100, ' % Complete'))
  temp <- tropical.depression[tropical.depression$EPISODE_ID == tropical.depression.episode[[i]], ]
  temp.counties <- unique(temp$EVENT_LOCATION)
  temp.list <- list()
  pb <- txtProgressBar(min = 0, max = length(temp.counties), style = 3)
  for (j in 1:length(temp.counties)) {
    setTxtProgressBar(pb, j)
    temp.rast <- terra::subst(us.states.rast, from = temp.counties[j], to = temp.counties[j], others = NA)
    temp.rast <- terra::subst(temp.rast, from = temp.counties[j], to = tropical.depression.episode[[i]], others = NA)
    temp.rast.values <- temp.rast
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]), by = 1)
    temp.times <- as.Date(temp.times)
    terra::nlyr(temp.rast) <- length(temp.times)
    terra::time(temp.rast) <- temp.times
    for (k in 1:terra::nlyr(temp.rast)) {
      temp.rast[[k]] <- temp.rast.values
    }
    names(temp.rast) <- temp.times
    temp.list[[j]] <- temp.rast
  }
  close(pb)
  temp <- terra::rast(temp.list)
  terra::time(temp) <- as.Date(names(temp))
  temp[is.na(temp)] <- 0
  temp <- terra::tapp(temp, 'days', fun = max)
  names(temp) <- terra::time(temp)
  save.cdf(raster = temp,
           folder.path = here::here("data", "output", "04_noaa", "conus", "tropical_depression"),
           file.name = paste0(min(terra::time(temp)), '_NOAA_tropical_depression_', tropical.depression.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Tropical Depression Episode ID',
           unit = 'ID')
  gc()
}