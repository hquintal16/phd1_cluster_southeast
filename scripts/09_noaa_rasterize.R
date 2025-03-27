#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: rasterize NOAA Events Database by county for validation of clustered events
#outputs saved in folder: V:\users\hquintal\phd2_southeast\data\output\08_noaa_event\

# Download relevant files from
# https://www.ncdc.noaa.gov/stormevents/listevents.jsp?eventType=%28Z%29+Excessive+Heat&eventType=%28C%29+Flash+Flood&eventType=%28Z%29+Heat&eventType=%28C%29+Heavy+Rain&beginDate_mm=01&beginDate_dd=01&beginDate_yyyy=2023&endDate_mm=01&endDate_dd=01&endDate_yyyy=2024&county=ALL&hailfilter=0.00&tornfilter=0&windfilter=000&sort=DT&submitbutton=Search&statefips=37%2CNORTH+CAROLINA

# Load Libraries & Functions ----
source("V:/users/hquintal/phd1_cluster_southeast/scripts/01_library.R")

# Download bulk files manually ----
# https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/

## Read in koppen-geiger geotiffs ----
directory <- "V:/users/hquintal/phd1_cluster_southeast/data/input/regional_aggregation/koppen_geiger/1991_2020/koppen_geiger_0p1.tif"
resolution <- terra::rast(directory)
resolution <- resolution/resolution

# Load US states data using the maps package and convert it to an sf object
us.states <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))

# Convert to spatvector
us.states.vect <- terra::vect(us.states)

# Convert to spatraster
us.states.rast <- terra::rasterize(us.states.vect, resolution, field = 'ID')

# Crop out regions
us.states.rast <- terra::crop(us.states.rast,terra::ext(-130,-65,24,50))

# Load NOAA events csv ----
# CONUS 1950-2023
files <- list.files(path = 'V:/users/hquintal/phd1_cluster_southeast/data/input/noaa_events/',full.names = T)

# 2025-02-28 Repeat for 2009 and 2015, somehow I missed these years
# files <- files[c(60,66)]

files <- files[2:length(files)]
events <- do.call(rbind, lapply(files, function(x) data.table::fread(x, stringsAsFactors = FALSE)))
# temp <- do.call(rbind, lapply(files, function(x) data.table::fread(x, stringsAsFactors = FALSE)))

# Remove event and episode narrative columns
events$EVENT_NARRATIVE = NULL
events$EPISODE_NARRATIVE = NULL

# ONLY KEEP RELEVANT EVENT TYPES
relevant <- unique(events$EVENT_TYPE)
relevant <- relevant[c(5,8,14,15,16,18,23,30,31,42,44,54,62,65,67,69,70)]
# relevant <- relevant[c(10,13,19,37,39,41,44,46)]
events <- events[events$EVENT_TYPE %in% relevant]

# ONLY KEEP EPISODE ID's, where each EPISODE contains multiple Storms
events <- events[!is.na(events$EPISODE_ID)]
table(events$EVENT_TYPE)
# Update NOAA events date ----
# BEGIN
BEGIN_DAY <- events$BEGIN_DAY
BEGIN_DAY_TEMP <- list()

for (i in 1:length(BEGIN_DAY)){
  print(i)
  
  if ((floor(log10(BEGIN_DAY[[i]])) + 1) < 2){
    
    BEGIN_DAY_TEMP[[i]] <- as.character(paste0(0,BEGIN_DAY[[i]]))
    
  } else {
    
    BEGIN_DAY_TEMP[[i]] <- as.character(BEGIN_DAY[[i]])
    
  }
  
}

# Combine year month day
BEGIN_DATE <- paste0(events$BEGIN_YEARMONTH,BEGIN_DAY_TEMP)
events$BEGIN_DATE <- as.Date(BEGIN_DATE,'%Y%m%d')

# END
END_DAY <- events$END_DAY
END_DAY_TEMP <- list()

for (i in 1:length(END_DAY)){
  print(i)
  
  if ((floor(log10(END_DAY[[i]])) + 1) < 2){
    
    END_DAY_TEMP[[i]] <- as.character(paste0(0,END_DAY[[i]]))
    
  } else {
    
    END_DAY_TEMP[[i]] <- as.character(END_DAY[[i]])
    
  }
  
}

gc()

# Combine year month day
END_DATE <- paste0(events$END_YEARMONTH,END_DAY_TEMP)
events$END_DATE <- as.Date(END_DATE,'%Y%m%d')

# Update NOAA events time ----
# BEGIN
BEGIN_TIME <- events$BEGIN_TIME
BEGIN_TIME_TEMP <- list()

for (i in 1:length(BEGIN_TIME)){
  
  print(i)
  
  count <- nchar(BEGIN_TIME[[i]])
  
  if (count < 4){
    
    if (count == 1){
      
      BEGIN_TIME_TEMP[[i]] <- as.character(paste0('000',as.character(BEGIN_TIME[[i]]),'00'))
      
    } else if (count == 2){
      
      BEGIN_TIME_TEMP[[i]] <- as.character(paste0('00',as.character(BEGIN_TIME[[i]]),'00'))
      
    } else {
      
      BEGIN_TIME_TEMP[[i]] <- as.character(paste0('0',as.character(BEGIN_TIME[[i]]),'00'))
      
    } 
    
  } else {
    
    BEGIN_TIME_TEMP[[i]] <- as.character(paste0(BEGIN_TIME[[i]],'00'))
    
  }
  
}

gc()

# Unlist result
BEGIN_TIME_TEMP <- unlist(BEGIN_TIME_TEMP)

# Combine year month day
BEGIN_DATE_TIME <- paste(BEGIN_DATE,BEGIN_TIME_TEMP)
BEGIN_DATE_TIME <- lubridate::ymd_hms(BEGIN_DATE_TIME)

# Combine year month day
events$BEGIN_DATE_TIME_COMPLETE <- BEGIN_DATE_TIME

# END
END_TIME <- events$END_TIME
END_TIME_TEMP <- list()

for (i in 1:length(END_TIME)){
  
  print(i)
  
  count <- nchar(END_TIME[[i]])
  
  if (count < 4){
    
    if (count == 1){
      
      END_TIME_TEMP[[i]] <- as.character(paste0('000',as.character(END_TIME[[i]]),'00'))
      
    } else if (count == 2){
      
      END_TIME_TEMP[[i]] <- as.character(paste0('00',as.character(END_TIME[[i]]),'00'))
      
    } else {
      
      END_TIME_TEMP[[i]] <- as.character(paste0('0',as.character(END_TIME[[i]]),'00'))
      
    } 
    
  } else {
    
    END_TIME_TEMP[[i]] <- as.character(paste0(END_TIME[[i]],'00'))
    
  }
  
}

# Unlist result
END_TIME_TEMP <- unlist(END_TIME_TEMP)

# Combine year month day
END_DATE_TIME <- paste(END_DATE,END_TIME_TEMP)
END_DATE_TIME <- lubridate::ymd_hms(END_DATE_TIME)

# Combine year month day
events$END_DATE_TIME_COMPLETE <- END_DATE_TIME

# subset to only conus fips...
unique.fips <- sort(unique(events$STATE_FIPS))
events <- events[events$STATE_FIPS %in% 1:56]
# temp <- events
events <- temp

# Define time zones
tz_combine <- data.table(tz = unique(events$CZ_TIMEZONE),
                         region = c("America/Chicago","America/New_York",
                                    "America/Los_Angeles","America/Denver",
                                    "America/Anguilla","America/Adak",
                                    "Pacific/Samoa","America/Chicago", # Samoa = Midway
                                    "America/New_York","America/Los_Angeles",
                                    "America/Anguilla","America/Adak",
                                    "America/Denver","America/Anchorage",
                                    "Pacific/Samoa","Asia/Muscat",
                                    "America/Los_Angeles","America/Chicago",
                                    "America/New_York"))

# # 2025-02-28 temporary solution for 2009 and 2015
# tz_combine <- data.table(tz = unique(events$CZ_TIMEZONE),
#                          region = c("America/Chicago","America/New_York",
#                                     "America/Los_Angeles","America/Adak",
#                                     "America/Denver","America/Anchorage"))
# table(events$STATE)
# Add time-zones back in 
time.zone.list <- list()

for (i in 1:length(events$CZ_TIMEZONE)){
  
  print(i)
  # i <- 1
  
  temp.pos <- tz_combine == events$CZ_TIMEZONE[[i]]
  temp.pos <- which(temp.pos == T)
  
  time.zone.list[[i]] <- tz_combine$region[[temp.pos]]
  
}

# Add corrected time zone to events data frame 
events$TIME.ZONE <- unlist(time.zone.list)
# temp <- events
events <- temp

# Update events start and end time with tz
# Create date time object from character date time and time zone columns
# https://stackoverflow.com/questions/74597695/how-to-create-datetime-object-from-character-datetime-and-timezone-columns
# https://stackoverflow.com/questions/25422954/combining-datetime-and-timezone-columns-to-get-a-date-time-object
# THIS STEP TAKES A VERY LONG TIME

# Start time
# Step 1: Convert to character to remove incorrect UTC assignment
events[, LOCAL_TIME := as.character(BEGIN_DATE_TIME_COMPLETE)]

# Step 2: Reassign the correct time zone row-wise
events[, LOCAL_TIME := purrr::map2(LOCAL_TIME, TIME.ZONE, ~ as.POSIXct(.x, tz=.y))]

# Step 3: Unlist LOCAL_TIME and convert to UTC
events[, LOCAL_TIME := unlist(LOCAL_TIME)]
events[, START.TIMESTAMP.UTC := as.POSIXct(LOCAL_TIME, tz="UTC")]

# End time
# Step 1: Convert to character to remove incorrect UTC assignment
events[, LOCAL_TIME2 := as.character(END_DATE_TIME_COMPLETE)]

# Step 2: Reassign the correct time zone row-wise
events[, LOCAL_TIME2 := purrr::map2(LOCAL_TIME2, TIME.ZONE, ~ as.POSIXct(.x, tz=.y))]

# Step 3: Unlist LOCAL_TIME and convert to UTC
events[, LOCAL_TIME2 := unlist(LOCAL_TIME2)]
events[, END.TIMESTAMP.UTC := as.POSIXct(LOCAL_TIME2, tz="UTC")]

# events <- events %>% 
#   rowwise() %>%
#   mutate(START.TIMESTAMP = lubridate::ymd_hms(BEGIN_DATE_TIME_COMPLETE)) %>% 
#   group_by(TIME.ZONE) %>%
#   mutate(START.TIMESTAMP.UTC = lubridate::with_tz(START.TIMESTAMP,TIME.ZONE)) %>%
#   select(-START.TIMESTAMP) 
# events <- events %>%
#   mutate(BEGIN_DATE_TIME_COMPLETE = as.character(BEGIN_DATE_TIME_COMPLETE)) %>% # Ensure it's character
#   mutate(START.TIMESTAMP = lubridate::ymd_hms(BEGIN_DATE_TIME_COMPLETE)) %>%
#   mutate(START.TIMESTAMP.UTC = lubridate::with_tz(START.TIMESTAMP, tzone = "UTC")) %>%
#   select(-START.TIMESTAMP)
# 
# # test
# events$START.TIMESTAMP.UTC
# 
# events <- events %>% 
#   rowwise() %>%
#   mutate(END.TIMESTAMP = lubridate::ymd_hms(END_DATE_TIME_COMPLETE)) %>% 
#   group_by(TIME.ZONE) %>%
#   mutate(END.TIMESTAMP.UTC = lubridate::with_tz(END.TIMESTAMP,TIME.ZONE)) %>%
#   select(-END.TIMESTAMP) 

events <- as.data.table(events)
# events$TIME.ZONE

# Remove superfluous columns
events <- events %>%
  select(-c(BEGIN_YEARMONTH,BEGIN_DAY,BEGIN_TIME,END_YEARMONTH,END_DAY,END_TIME,
            YEAR,MONTH_NAME,CZ_TYPE,CZ_FIPS,BEGIN_DATE_TIME,CZ_TIMEZONE,
            END_DATE_TIME,SOURCE,MAGNITUDE,MAGNITUDE_TYPE,FLOOD_CAUSE,CATEGORY,
            TOR_F_SCALE,TOR_LENGTH,TOR_WIDTH,TOR_OTHER_WFO,TOR_OTHER_CZ_STATE,
            TOR_OTHER_CZ_FIPS,TOR_OTHER_CZ_NAME,BEGIN_RANGE,BEGIN_AZIMUTH,
            BEGIN_LOCATION,END_RANGE,END_AZIMUTH,END_LOCATION,BEGIN_LAT,
            BEGIN_LON,END_LAT,END_LON,DATA_SOURCE,BEGIN_DATE,END_DATE,
            BEGIN_DATE_TIME_COMPLETE,END_DATE_TIME_COMPLETE,TIME.ZONE,LOCAL_TIME,
            LOCAL_TIME2))
# events <- events %>%
#   select(-c(BEGIN_DATE_TIME_UTC))

# Save result
write.csv(events,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv')

# Read result
events <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv'))

# Give 0 dollars to "" or "NA" entries
events$DAMAGE_PROPERTY[nchar(events$DAMAGE_PROPERTY) < 2] <- '0.00K'
events$DAMAGE_CROPS[nchar(events$DAMAGE_CROPS) < 2] <- '0.00K'
events$INJURIES_DIRECT[nchar(events$INJURIES_DIRECT) < 1] <- 0
events$INJURIES_INDIRECT[nchar(events$INJURIES_INDIRECT) < 1] <- 0
events$DEATHS_DIRECT[nchar(events$DEATHS_DIRECT) < 1] <- 0
events$DEATHS_INDIRECT[nchar(events$DEATHS_INDIRECT) < 1] <- 0

# Separate numbers and letters
events <- events %>%
  mutate(DAMAGE_PROPERTY_INT = getNumberPart(DAMAGE_PROPERTY)) %>%
  mutate(DAMAGE_PROPERTY_CHAR = gsub("[^a-zA-Z]", "", DAMAGE_PROPERTY))

events <- events %>%
  mutate(DAMAGE_CROPS_INT = getNumberPart(DAMAGE_CROPS)) %>%
  mutate(DAMAGE_CROPS_CHAR = gsub("[^a-zA-Z]", "", DAMAGE_CROPS))

# Save result
write.csv(events,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv')

# Read result
events <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv'))

# Convert damages into dollars
events$DAMAGE_PROPERTY_CHAR <- as.numeric(gsub('K',1000,
                                               gsub('M',1000000,
                                                    gsub('B',1000000000,
                                                         events$DAMAGE_PROPERTY_CHAR))))

events$DAMAGE_CROPS_CHAR <- as.numeric(gsub('K',1000,
                                            gsub('M',1000000,
                                                 gsub('B',1000000000,
                                                      events$DAMAGE_CROPS_CHAR))))

# calculate damages
events$DAMAGE_PROPERTY <- events$DAMAGE_PROPERTY_CHAR * events$DAMAGE_PROPERTY_INT
events$DAMAGE_CROPS <- events$DAMAGE_CROPS_CHAR * events$DAMAGE_CROPS_INT

# remove remaining NA's
events[is.na(events$DAMAGE_PROPERTY)] <- 0
events[is.na(events$DAMAGE_CROPS)] <- 0

# remove superfluous columns
events <- events %>%
  select(-c(DAMAGE_PROPERTY_INT,DAMAGE_PROPERTY_CHAR,DAMAGE_CROPS_INT,DAMAGE_CROPS_CHAR,
            X.1,X))

# remove events without date times
events <- events[complete.cases(events),]

# Update location to be state,county format to match conus county raster
state.county <- unique(us.states.rast$ID)

events <- events %>%
  mutate(EVENT_LOCATION = paste0(tolower(STATE),',',tolower(CZ_NAME)))

events.list <- list()

county.noaa <- sub("^.*?,", "", events$EVENT_LOCATION)
county.noaa <- gsub("[[:punct:]]", " ", county.noaa)
county.noaa <- stringr::str_squish(county.noaa)

for (i in 1:nrow(state.county)){
  
  temp.county <- sub("^.*?,", "", state.county[i,])
  temp.county <- gsub("[[:punct:]]", " ", temp.county)
  
  temp <- events[county.noaa %like% temp.county,]
  
  temp.state.noaa <- temp[temp$EVENT_LOCATION == state.county[i,]]
  
  print(paste0(i,' ',state.county[i,],' had ',nrow(temp.state.noaa),' events'))
  
  events.list[[i]] <- temp.state.noaa
  
}

events <- dplyr::bind_rows(events.list, .id = 'column_label')

# remove superfluous columns
events <- events %>%
  select(-c(column_label,STATE,STATE_FIPS,CZ_NAME))

# Save result
write.csv(events,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv')

# Read result
events <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv'))

# order by start date time
events <- events[order(rank(EPISODE_ID))]

# convert date time as character to date
events$START.TIMESTAMP.UTC <- as.Date(events$START.TIMESTAMP.UTC)
events$END.TIMESTAMP.UTC <- as.Date(events$END.TIMESTAMP.UTC)

# convert datetime into posct format
# events$START.TIMESTAMP.UTC <- as.POSIXct(events$START.TIMESTAMP.UTC,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# events$END.TIMESTAMP.UTC <- as.POSIXct(events$END.TIMESTAMP.UTC,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Subset to each event type
excess.heat <- events[which(events$EVENT_TYPE == "Excessive Heat"),] 
flash.flood <- events[which(events$EVENT_TYPE == "Flash Flood"),] 
flood <- events[which(events$EVENT_TYPE == "Flood"),] 
heat <- events[which(events$EVENT_TYPE == "Heat"),] 
heavy.rain <- events[which(events$EVENT_TYPE == "Heavy Rain"),]
hurricane <- events[which(events$EVENT_TYPE == "Hurricane"),]
typhoon <- events[which(events$EVENT_TYPE == "Hurricane (Typhoon)"),]
tropical.storm <- events[which(events$EVENT_TYPE == "Tropical Storm"),]
tropical.depression <- events[which(events$EVENT_TYPE == "Tropical Depression"),]

noaa.heat <- rbind(excess.heat,heat)
noaa.precipitation <- rbind(flash.flood,flood,heavy.rain,hurricane,typhoon,tropical.storm,tropical.depression)

# Save result
write.csv(excess.heat,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_excess_heat.csv')
write.csv(flash.flood,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_flash_flood.csv')
write.csv(flood,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_flood.csv')
write.csv(heat,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_heat.csv')
write.csv(heavy.rain,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_heavy_rain.csv')
write.csv(hurricane,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_hurricane.csv')
write.csv(typhoon,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_typhoon.csv')
write.csv(tropical.storm,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_tropical_storm.csv')
write.csv(tropical.depression,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_tropical_depression.csv')

write.csv(noaa.heat,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_all_heat.csv')
write.csv(noaa.precipitation,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_all_precipitation.csv')

write.csv(events,'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv')

# Read result
excess.heat <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_excess_heat.csv'))
flash.flood <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_flash_flood.csv'))
flood <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_flood.csv'))
heat <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_heat.csv'))
heavy.rain <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_heavy_rain.csv'))
hurricane <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_hurricane.csv'))
typhoon <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_typhoon.csv'))
tropical.storm <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_tropical_storm.csv'))
tropical.depression <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_tropical_depression.csv'))

noaa.heat <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_all_heat.csv'))
noaa.precipitation <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events_all_precipitation.csv'))

events <- as.data.table(read.csv('V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/NOAA_events.csv'))

events$START.TIMESTAMP.UTC <- as.Date(events$START.TIMESTAMP.UTC)
events$END.TIMESTAMP.UTC <- as.Date(events$END.TIMESTAMP.UTC)

# Combine noaa.heat episode id with us.states.rast and associated date time duration into single raster

# 1. NOAA: Excessive Heat ---- 
excess.heat.episode <- unique(excess.heat$EPISODE_ID)

for (i in 1:length(excess.heat.episode)){
  
  print(paste0('NOAA Excess Heat Episode # ',excess.heat.episode[[i]],' ',
               round(i/length(excess.heat.episode),2)*100,' % Complete'))
  
  temp <- excess.heat[excess.heat$EPISODE_ID == excess.heat.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=excess.heat.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/excess_heat/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_excess_heat_',
                              excess.heat.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Excess Heat Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 2. NOAA: Flash Flood ---- 

flash.flood.episode <- unique(flash.flood$EPISODE_ID)

for (i in 1:length(flash.flood.episode)){
  
  print(paste0('NOAA Flash Flood Episode # ',flash.flood.episode[[i]],' ',
               round(i/length(flash.flood.episode),2)*100,' % Complete'))
  
  temp <- flash.flood[flash.flood$EPISODE_ID == flash.flood.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=flash.flood.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/flash_flood/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_flash_flood_',
                              flash.flood.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Flash Flood Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 3. NOAA: Flood ---- 

flood.episode <- unique(flood$EPISODE_ID)

for (i in 1:length(flood.episode)){
  
  print(paste0('NOAA Flood Episode # ',flood.episode[[i]],' ',
               round(i/length(flood.episode),2)*100,' % Complete'))
  
  temp <- flood[flood$EPISODE_ID == flood.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=flood.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/flood/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_flood_',
                              flood.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Flood Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 4. NOAA: Heat ---- 

heat.episode <- unique(heat$EPISODE_ID)

for (i in 1:length(heat.episode)){
  
  print(paste0('NOAA Heat Episode # ',heat.episode[[i]],' ',
               round(i/length(heat.episode),2)*100,' % Complete'))
  
  temp <- heat[heat$EPISODE_ID == heat.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=heat.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/heat/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_heat_',
                              heat.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Heat Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 5. NOAA: Heavy Rain ---- 

heavy.rain.episode <- unique(heavy.rain$EPISODE_ID)

for (i in 1:length(heavy.rain.episode)){
  
  print(paste0('NOAA Heavy Rain Episode # ',heavy.rain.episode[[i]],' ',
               round(i/length(heavy.rain.episode),2)*100,' % Complete'))
  
  temp <- heavy.rain[heavy.rain$EPISODE_ID == heavy.rain.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=heavy.rain.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/heavy_rain/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_heavy_rain_',
                              heavy.rain.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Heavy Rain Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 6. NOAA: Hurricane ---- 

hurricane.episode <- unique(hurricane$EPISODE_ID)

for (i in 1:length(hurricane.episode)){
  
  print(paste0('NOAA Hurricane Episode # ',hurricane.episode[[i]],' ',
               round(i/length(hurricane.episode),2)*100,' % Complete'))
  
  temp <- hurricane[hurricane$EPISODE_ID == hurricane.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=hurricane.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/hurricane/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_hurricane_',
                              hurricane.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Hurricane Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 7. NOAA: Typhoon ----

typhoon.episode <- unique(typhoon$EPISODE_ID)

for (i in 1:length(typhoon.episode)){
  
  print(paste0('NOAA Typhoon Episode # ',typhoon.episode[[i]],' ',
               round(i/length(typhoon.episode),2)*100,' % Complete'))
  
  temp <- typhoon[typhoon$EPISODE_ID == typhoon.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=typhoon.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/typhoon/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_typhoon_',
                              typhoon.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Typhoon Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 8. NOAA: Tropical Storm ---- 

tropical.storm.episode <- unique(tropical.storm$EPISODE_ID)

for (i in 1:length(tropical.storm.episode)){
  
  print(paste0('NOAA Tropical Storm Episode # ',tropical.storm.episode[[i]],' ',
               round(i/length(tropical.storm.episode),2)*100,' % Complete'))
  
  temp <- tropical.storm[tropical.storm$EPISODE_ID == tropical.storm.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=tropical.storm.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/tropical_storm/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_tropical_storm_',
                              tropical.storm.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Tropical Storm Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}

# 9. NOAA: Tropical Depression ----

tropical.depression.episode <- unique(tropical.depression$EPISODE_ID)

for (i in 1:length(tropical.depression.episode)){
  
  print(paste0('NOAA Tropical Depression Episode # ',tropical.depression.episode[[i]],' ',
               round(i/length(tropical.depression.episode),2)*100,' % Complete'))
  
  temp <- tropical.depression[tropical.depression$EPISODE_ID == tropical.depression.episode[[i]]]
  
  temp.counties <- unique(temp$EVENT_LOCATION)
  
  temp.list <- list()
  pb = txtProgressBar(min = 0, max = length(temp.counties), initial = 0) 
  
  for (j in 1:length(temp.counties)){
    
    setTxtProgressBar(pb,j)
    
    temp.rast <- terra::subst(us.states.rast,from=temp.counties[j],to=temp.counties[j],others=NA)
    temp.rast <- terra::subst(temp.rast,from=temp.counties[j],to=tropical.depression.episode[[i]],others=NA)
    temp.rast.values <- temp.rast
    
    temp.times <- seq.Date(as.Date(temp$START.TIMESTAMP.UTC[j]),
                           as.Date(temp$END.TIMESTAMP.UTC[j]),by = 1)
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
  
  temp <- terra::tapp(temp,'days',fun=max)
  
  names(temp) <- terra::time(temp)
  
  # save result
  save.cdf(raster = temp,
           folder.path = 'V:/users/hquintal/phd1_cluster_southeast/data/output/04_noaa/conus/tropical_depression/',
           file.name = paste0(min(terra::time(temp)),'_NOAA_tropical_depression_',
                              tropical.depression.episode[[i]]),
           var.name = 'Episode ID',
           long.name = 'NOAA Tropical Depression Episode ID',
           unit = 'ID')
  
  # clear memory
  gc()
  
}
