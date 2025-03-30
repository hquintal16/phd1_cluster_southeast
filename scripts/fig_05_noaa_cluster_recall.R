#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 9 panel figures of noaa events, clusters, and recall by county

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_04_thresholds.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

noaa <- terra::rast(here('data','output','04_noaa','southeast','summary','excess_heat','NOAA_Excess_Heat_Daily_Extent_2023.nc'))
cluster <- terra::rast(here('data','output','03_cluster','02_cluster','points','stm4','heat_index','summary','County_heat_index_Daily_Extent_1940.nc'))

plot(noaa)
plot(cluster)
