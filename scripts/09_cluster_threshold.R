#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: convert thresholds to shapefiles 
#study area: Southeast

# Load Libraries & Functions ----
library(here)
here::i_am("scripts/09_cluster_threshold.R")  # Adjust the file path if needed
source(here::here("scripts", "01_library.R"))

## Read in koppen-geiger geotiffs
directory <- here::here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")
regions <- terra::rast(directory)

# Load US states data using the maps package and convert it to an sf object
us.states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Convert us.states to raster
us.states.rast <- stars::st_rasterize(us.states %>% dplyr::select(1, geom))
us.states.rast <- terra::rast(us.states.rast)

# Normalize raster as 1 = USA land, NA = ocean / Not USA land
us.states.rast <- us.states.rast / us.states.rast

# Give correct CRS using the regions raster CRS
terra::crs(us.states.rast) <- terra::crs(regions)

# Resample USA outline to the resolution of regions
us.states.rast <- terra::resample(us.states.rast, regions)

# Heat Index ----

## Define WFO areas ----
cwa <- c('CAR','GYX','BOX','OKX','BIS','FGF','DLH','MOT','UNR','ABR','FSD','MPX','ARX',
         'MKX','APX','GRB','GRR','DTX','LBF','DVN','IWX','BTV','ALY','BUF','BGM','CTP',
         'CLE','ILN','RLX','PBZ','AMA','LUB','SJT','FWD','TSA','SHV','LZK','JAN','MEG',
         'OHX','HUN','BMX','MRX','FFC','GLD','GID','OAX','DMX','LOT','TOP','EAX','ILX',
         'IND','DDC','ICT','SGF','LSX','PAH','LMK','JKL','AKQ','RNK','RAH','GSP','MHX',
         'ILM','HGX','LCH','LIX','MOB','TAE','JAX','TBW','MFL','MLB','CRP','CAE','CHS',
         'BRO','OUN')

troublemakers <- c('ABQ','EPZ','MAF','EWX','RIW','CYS','BOU','GJT','PUB','LWX','PHI')

# Read in shapefile for WFO boundaries
wfo <- sf::st_read(here::here("data", "input", "threshold", "z_05mr24", "z_05mr24.shp"))
wfo <- wfo[wfo$STATE %in% c('AL','KY','OH','LA','OK','AZ','ME','OR','AR','MD',
                            'PA','MA','CA','MI','RI','CO','MN','SC','CT','MS',
                            'SD','DE','MO','TN','DC','MT','TX','FL','NE','GA',
                            'NV','UT','NH','VT','NJ','VA','ID','NM','IL','NY',
                            'WA','IN','NC','WV','IA','ND','WI','KS','WY'), ]

# Subset troublemakers from wfo
troublemakers <- wfo %>% dplyr::filter(CWA %in% troublemakers)

# Deal with troublemakers
ABQ <- troublemakers %>% dplyr::filter(CWA %in% 'ABQ')
EPZ <- troublemakers %>% dplyr::filter(CWA %in% 'EPZ')
MAF <- troublemakers %>% dplyr::filter(CWA %in% 'MAF')
EWX <- troublemakers %>% dplyr::filter(CWA %in% 'EWX')
RIW <- troublemakers %>% dplyr::filter(CWA %in% 'RIW')
CYS <- troublemakers %>% dplyr::filter(CWA %in% 'CYS')
GJT <- troublemakers %>% dplyr::filter(CWA %in% 'GJT')
PUB <- troublemakers %>% dplyr::filter(CWA %in% 'PUB')
LWX <- troublemakers %>% dplyr::filter(CWA %in% 'LWX')

# LWX subdivisions
LWX.DC <- LWX %>% dplyr::filter(STATE %in% 'DC')
LWX.VA <- LWX %>% dplyr::filter(STATE %in% 'VA')
LWX.MD <- LWX %>% dplyr::filter(STATE %in% 'MD')
LWX.WV <- LWX %>% dplyr::filter(STATE %in% 'WV')

LWX.VA.west <- LWX.VA %>% dplyr::filter(ZONE %in% c(paste0('0', c(028,031,030,029,027,026,025)), c(503,504,507,508)))
LWX.VA.east <- LWX.VA %>% dplyr::filter(!ZONE %in% c(paste0('0', c(028,031,030,029,027,026,025)), c(503,504,507,508)))

LWX.MD.west <- LWX.MD %>% dplyr::filter(ZONE %in% c(paste0('00', c(001,003)), c(501,502)))
LWX.MD.east <- LWX.MD %>% dplyr::filter(!ZONE %in% c(paste0('00', c(001,003)), c(501,502)))

LWX.DC$CRITERIA <- 105
LWX.WV$CRITERIA <- 100
LWX.VA.west$CRITERIA <- 100
LWX.VA.east$CRITERIA <- 105
LWX.MD.west$CRITERIA <- 100
LWX.MD.east$CRITERIA <- 105

LWX <- rbind(LWX.DC, LWX.WV, LWX.VA.west, LWX.VA.east, LWX.MD.west, LWX.MD.east)

PHI <- troublemakers %>% dplyr::filter(CWA %in% 'PHI')
PHI.DE <- PHI %>% dplyr::filter(STATE %in% 'DE')
PHI.MD <- PHI %>% dplyr::filter(STATE %in% 'MD')
PHI.NJ <- PHI %>% dplyr::filter(STATE %in% 'NJ')
PHI.PA <- PHI %>% dplyr::filter(STATE %in% 'PA')

PHI.NJ.north <- PHI.NJ %>% dplyr::filter(ZONE %in% c(paste0('00', c(001,007,008,009)),
                                                     paste0('0', c(010,012,015,013,014,026,019,020,027))))
PHI.NJ.south <- PHI.NJ %>% dplyr::filter(!ZONE %in% c(paste0('00', c(001,007,008,009)),
                                                      paste0('0', c(010,012,015,013,014,026,019,020,027))))

PHI.DE$CRITERIA <- 105
PHI.MD$CRITERIA <- 105
PHI.PA$CRITERIA <- 100
PHI.NJ.north$CRITERIA <- 100
PHI.NJ.south$CRITERIA <- 105

PHI <- rbind(PHI.DE, PHI.MD, PHI.PA, PHI.NJ.north, PHI.NJ.south)

# Single WFO Criteria 
wfo <- wfo %>% dplyr::filter(CWA %in% cwa)

advisory.095 <- c('CAR','GYX','BOX','OKX')
advisory.100 <- c('BIS','FGF','DLH','MOT','UNR','ABR','FSD','MPX','ARX')
advisory.105 <- c('AMA','LUB','SJT','FWD','TSA','SHV','LZK','JAN','MEG','OHX',
                  'HUN','BMX','MRX','FFC','GLD','GID','OAX','DMX','LOT','TOP','EAX','ILX',
                  'IND','DDC','ICT','SGF','LSX','PAH','LMK','JKL','AKQ','RNK','RAH','GSP','MHX',
                  'ILM','OUN')
advisory.108 <- c('HGX','LCH','LIX','MOB','TAE','JAX','TBW','MFL','MLB')
advisory.110 <- c('CRP','CAE','CHS')
advisory.111 <- c('BRO')

wfo.adv.095 <- wfo %>% dplyr::filter(CWA %in% advisory.095)
wfo.adv.100 <- wfo %>% dplyr::filter(CWA %in% advisory.100)
wfo.adv.105 <- wfo %>% dplyr::filter(CWA %in% advisory.105)
wfo.adv.108 <- wfo %>% dplyr::filter(CWA %in% advisory.108)
wfo.adv.110 <- wfo %>% dplyr::filter(CWA %in% advisory.110)

# (Assume warning groups are defined similarly elsewhere if needed)

wfo <- rbind(wfo.adv.095, wfo.adv.100, wfo.adv.105, wfo.adv.108, wfo.adv.110,
             wfo.adv.111, LWX, PHI)
names(wfo)[names(wfo) == 'CRITERIA'] <- 'ADVISORY'
wfo$WARNING <- wfo$ADVISORY + 5

# Convert F to C
wfo$ADVISORY <- (wfo$ADVISORY - 32) * 5/9
wfo$WARNING <- (wfo$WARNING - 32) * 5/9

## Save output ----
st_write(wfo, here::here("data", "input", "threshold", "atlas14_shapefile.shp"), delete_layer = TRUE)

# Precipitation ----

## Read in koppen-geiger geotiffs
directory <- here::here("data", "input", "regional_aggregation", "koppen_geiger", "1991_2020", "koppen_geiger_0p1.tif")
regions <- terra::rast(directory)

# Read in domain of interest
domain <- terra::rast(here::here("data", "output", "01_era5", "daily", "heat_index", "heat_index_daily_maximum_194001.nc"))
domain <- domain[[1]] / domain[[1]]

# Read in United States counties using tigris
united_states <- tigris::counties(state = c('AL','AR','CO','CT','DE','DC','FL','GA','AZ',
                                            'IL','IN','IA','KS','KY','LA','ME','MD','MA',
                                            'MI','MN','MS','MO','NE','NH','NJ','NM','UT',
                                            'NY','NC','ND','OH','OK','PA','RI','SC','SD',
                                            'TN','TX','VT','VA','WV','WI','NV','CA'), 
                                  cb = TRUE) %>% st_as_sf()

united_states <- stars::st_rasterize(united_states %>% dplyr::select(AFFGEOID, geometry))
united_states <- terra::rast(united_states)
terra::res(united_states) <- c(0.008333333, 0.008333333)

## 1-year 24-hr ----
atlas.14 <- threshold.compile(
  ne.dir = here::here("data", "input", "threshold", "ne1yr24ha", "ne1yr24ha.asc"),
  orb.dir = here::here("data", "input", "threshold", "orb1yr24ha", "orb1yr24ha.asc"),
  se.dir  = here::here("data", "input", "threshold", "se1yr24ha", "se1yr24ha.asc"),
  sw.dir  = here::here("data", "input", "threshold", "sw1yr24ha", "sw1yr24ha.asc"),
  tx.dir  = here::here("data", "input", "threshold", "tx1yr24ha", "tx1yr24ha.asc"),
  mw.dir  = here::here("data", "input", "threshold", "mw1yr24ha", "mw1yr24ha.asc")
)

atlas_vector <- as.polygons(atlas.14, dissolve = FALSE)
atlas_sf <- st_as_sf(atlas_vector)
colnames(atlas_sf)[which(names(atlas_sf) == "layer")] <- "threshold"

## Save output ----
st_write(atlas_sf, here::here("data", "input", "threshold", "atlas14_shapefile.shp"), delete_layer = TRUE)
