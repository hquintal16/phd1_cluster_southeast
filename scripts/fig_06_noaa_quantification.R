#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 7 panel figures of noaa events, intensity, duration, exposed area, impacts

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_06_noaa_quantification.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# plot_excess_heat <- function(dir_path) {
#   
#   # Identify all CSV files in the specified directory
#   csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
#   
#   # Initialize an empty list to store plots per file
#   plots_list <- list()
#   
#   # Loop over each file
#   for (file in csv_files) {
#     # Read the data
#     df <- read_csv(file)
#     
#     # Convert start_time to a Date object (adjust format if necessary)
#     df <- df %>% 
#       mutate(start_time = as.Date(start_time, format = "%d/%m/%Y"))
#     
#     ## Create individual plots
#     
#     # Plot 1: Color by exposed_area
#     p1 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          size = duration, color = exposed_area)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Exposed Area") +
#       theme_minimal()
#     
#     # Plot 2: Color by injuries_direct
#     p2 <- ggplot(df, aes(x = start_time, y = era5_median, color = injuries_direct)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Injuries Direct") +
#       theme_minimal()
#     
#     # Plot 3: Color by injuries_indirect
#     p3 <- ggplot(df, aes(x = start_time, y = era5_median, color = injuries_indirect)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Injuries Indirect") +
#       theme_minimal()
#     
#     # Plot 4: Color by deaths_direct
#     p4 <- ggplot(df, aes(x = start_time, y = era5_median, color = deaths_direct)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Deaths Direct") +
#       theme_minimal()
#     
#     # Plot 5: Color by deaths_direct (again; adjust if needed)
#     p5 <- ggplot(df, aes(x = start_time, y = era5_median, color = deaths_direct)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Deaths Direct") +
#       theme_minimal()
#     
#     # Plot 6: Color by damage_property
#     p6 <- ggplot(df, aes(x = start_time, y = era5_median, color = damage_property)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Damage Property") +
#       theme_minimal()
#     
#     # Plot 7: Color by damage_crops
#     p7 <- ggplot(df, aes(x = start_time, y = era5_median, color = damage_crops)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Damage Crops") +
#       theme_minimal()
#     
#     ## Create patchwork panel for plots p2 to p7
#     patch_panel <- (p2 + p3 + p4 + p5 + p6 + p7) + 
#       plot_layout(ncol = 2, nrow = 3)
#     
#     ## Save the individual p1 and the patchwork panel in the list,
#     ## using the file basename as the key for later retrieval.
#     plots_list[[basename(file)]] <- list("p1" = p1, "patch" = patch_panel)
#   }
#   
#   # Return the list of plots
#   return(plots_list)
# }
# plot_excess_heat <- function(dir_path) {
#   
#   # Identify all CSV files in the specified directory
#   csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
#   
#   # Initialize an empty list to store plots per file
#   plots_list <- list()
#   
#   # Loop over each file
#   for (file in csv_files) {
#     # Read the data
#     df <- read_csv(file)
#     
#     # Convert start_time to a Date object (adjust format if necessary)
#     df <- df %>% 
#       mutate(start_time = as.Date(start_time, format = "%d/%m/%Y"))
#     
#     ## Create individual plots
#     
#     # Plot 1: Color by exposed_area
#     p1 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          size = duration, color = exposed_area)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Exposed Area") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     # Plot 2: Color by injuries_direct
#     p2 <- ggplot(df, aes(x = start_time, y = era5_median, color = injuries_direct)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Injuries Direct") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     # Plot 3: Color by injuries_indirect
#     p3 <- ggplot(df, aes(x = start_time, y = era5_median, color = injuries_indirect)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Injuries Indirect") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     # Plot 4: Color by deaths_direct
#     p4 <- ggplot(df, aes(x = start_time, y = era5_median, color = deaths_direct)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Deaths Direct") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     # Plot 5: Color by deaths_direct (again; adjust if needed)
#     p5 <- ggplot(df, aes(x = start_time, y = era5_median, color = deaths_direct)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Deaths Direct") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     # Plot 6: Color by damage_property
#     p6 <- ggplot(df, aes(x = start_time, y = era5_median, color = damage_property)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Damage Property") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     # Plot 7: Color by damage_crops
#     p7 <- ggplot(df, aes(x = start_time, y = era5_median, color = damage_crops)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Damage Crops") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     ## Create patchwork panel for plots p2 to p7
#     patch_panel <- (p2+p3+p4+p5+p6+p7) + 
#                         
#       plot_layout(ncol = 2, nrow = 3)
#     
#     ## Save the individual p1 and the patchwork panel in the list,
#     ## using the file basename as the key for later retrieval.
#     plots_list[[basename(file)]] <- list("p1" = p1, "patch" = patch_panel)
#   }
#   
#   # Return the list of plots
#   return(plots_list)
# }

# plot_excess_heat <- function(dir_path) {
#   
#   # Identify all CSV files in the specified directory
#   csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
#   
#   # Initialize an empty list to store plots per file
#   plots_list <- list()
#   
#   # Loop over each file
#   for (file in csv_files) {
#     # Read the data
#     df <- read_csv(file)
#     
#     # Convert start_time to a Date object (adjust format if necessary)
#     df <- df %>% 
#       mutate(start_time = as.Date(start_time, format = "%d/%m/%Y"))
#     
#     ## Create individual plots
#     
#     # Plot 1: Color by exposed_area (keeping original mapping)
#     p1 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          size = duration, color = exposed_area)) +
#       geom_point() +
#       labs(x = "Start Time", y = "ERA5 Median", color = "Exposed Area") +
#       scale_color_gradient(low = "grey", high = "black") +
#       theme_bw()
#     
#     # For plots p2 to p7, use shape 21 and fill only when the variable > 0.
#     # Plot 2: Injuries Direct
#     p2 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          fill = if_else(injuries_direct > 0, injuries_direct, NA_real_))) +
#       geom_point(shape = 21, size = 3, color = "black") +
#       labs(x = "Start Time", y = "ERA5 Median", fill = "Injuries Direct") +
#       scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
#       theme_bw()
#     
#     # Plot 3: Injuries Indirect
#     p3 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          fill = if_else(injuries_indirect > 0, injuries_indirect, NA_real_))) +
#       geom_point(shape = 21, size = 3, color = "black") +
#       labs(x = "Start Time", y = "ERA5 Median", fill = "Injuries Indirect") +
#       scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
#       theme_bw()
#     
#     # Plot 4: Deaths Direct
#     p4 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          fill = if_else(deaths_direct > 0, deaths_direct, NA_real_))) +
#       geom_point(shape = 21, size = 3, color = "black") +
#       labs(x = "Start Time", y = "ERA5 Median", fill = "Deaths Direct") +
#       scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
#       theme_bw()
#     
#     # Plot 5: Deaths Direct (again; adjust if needed)
#     p5 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          fill = if_else(deaths_direct > 0, deaths_direct, NA_real_))) +
#       geom_point(shape = 21, size = 3, color = "black") +
#       labs(x = "Start Time", y = "ERA5 Median", fill = "Deaths Direct") +
#       scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
#       theme_bw()
#     
#     # Plot 6: Damage Property
#     p6 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          fill = if_else(damage_property > 0, damage_property, NA_real_))) +
#       geom_point(shape = 21, size = 3, color = "black") +
#       labs(x = "Start Time", y = "ERA5 Median", fill = "Damage Property") +
#       scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
#       theme_bw()
#     
#     # Plot 7: Damage Crops
#     p7 <- ggplot(df, aes(x = start_time, y = era5_median,
#                          fill = if_else(damage_crops > 0, damage_crops, NA_real_))) +
#       geom_point(shape = 21, size = 3, color = "black") +
#       labs(x = "Start Time", y = "ERA5 Median", fill = "Damage Crops") +
#       scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
#       theme_bw()
#     
#     ## Create patchwork panel for plots p2 to p7
#     patch_panel <- (p2 + p3 + p4 + p5 + p6 + p7) + 
#       plot_layout(ncol = 2, nrow = 3)
#     
#     ## Save the individual p1 and the patchwork panel in the list,
#     ## using the file basename as the key for later retrieval.
#     plots_list[[basename(file)]] <- list("p1" = p1, "patch" = patch_panel)
#   }
#   
#   # Return the list of plots
#   return(plots_list)
# }

plot_excess_heat <- function(dir_path) {
  
  # Identify all CSV files in the specified directory
  csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store plots per file
  plots_list <- list()
  
  # Loop over each file
  for (file in csv_files) {
    # Read the data
    df <- read_csv(file)
    
    # Convert start_time to a Date object (adjust format if necessary)
    df <- df %>% 
      mutate(start_time = as.Date(start_time, format = "%d/%m/%Y"))
    
    ## Create individual plots
    
    # Plot 1: Color by exposed_area (unchanged)
    p1 <- ggplot(df, aes(x = start_time, y = era5_max,
                         size = duration, color = exposed_area)) +
      geom_point() +
      labs(x = "Start Time", y = "ERA5 max", color = "Exposed Area") +
      scale_color_gradient(low = "grey", high = "black") +
      theme_bw()
    
    # Plot 2: Injuries Direct
    p2 <- ggplot(df, aes(x = start_time, y = era5_max)) +
      # First, plot hollow points for injuries_direct <= 0
      geom_point(data = df %>% filter(injuries_direct <= 0),
                 shape = 21, size = 3, color = "black", fill = NA) +
      # Then, plot filled points for injuries_direct > 0
      geom_point(data = df %>% filter(injuries_direct > 0),
                 aes(fill = injuries_direct),
                 shape = 21, size = 3, color = "black") +
      labs(x = "Start Time", y = "ERA5 max", fill = "Injuries Direct") +
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
    
    # Plot 3: Injuries Indirect
    p3 <- ggplot(df, aes(x = start_time, y = era5_max)) +
      geom_point(data = df %>% filter(injuries_indirect <= 0),
                 shape = 21, size = 3, color = "black", fill = NA) +
      geom_point(data = df %>% filter(injuries_indirect > 0),
                 aes(fill = injuries_indirect),
                 shape = 21, size = 3, color = "black") +
      labs(x = "Start Time", y = "ERA5 max", fill = "Injuries Indirect") +
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
    
    # Plot 4: Deaths Direct
    p4 <- ggplot(df, aes(x = start_time, y = era5_max)) +
      geom_point(data = df %>% filter(deaths_direct <= 0),
                 shape = 21, size = 3, color = "black", fill = NA) +
      geom_point(data = df %>% filter(deaths_direct > 0),
                 aes(fill = deaths_direct),
                 shape = 21, size = 3, color = "black") +
      labs(x = "Start Time", y = "ERA5 max", fill = "Deaths Direct") +
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
    
    # Plot 5: Deaths Direct (again; adjust if needed)
    p5 <- ggplot(df, aes(x = start_time, y = era5_max)) +
      geom_point(data = df %>% filter(deaths_indirect <= 0),
                 shape = 21, size = 3, color = "black", fill = NA) +
      geom_point(data = df %>% filter(deaths_indirect > 0),
                 aes(fill = deaths_indirect),
                 shape = 21, size = 3, color = "black") +
      labs(x = "Start Time", y = "ERA5 max", fill = "Deaths Indirect") +
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
    
    # Plot 6: Damage Property
    p6 <- ggplot(df, aes(x = start_time, y = era5_max)) +
      geom_point(data = df %>% filter(damage_property <= 0),
                 shape = 21, size = 3, color = "black", fill = NA) +
      geom_point(data = df %>% filter(damage_property > 0),
                 aes(fill = damage_property),
                 shape = 21, size = 3, color = "black") +
      labs(x = "Start Time", y = "ERA5 max", fill = "Damage Property") +
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
    
    # Plot 7: Damage Crops
    p7 <- ggplot(df, aes(x = start_time, y = era5_max)) +
      geom_point(data = df %>% filter(damage_crops <= 0),
                 shape = 21, size = 3, color = "black", fill = NA) +
      geom_point(data = df %>% filter(damage_crops > 0),
                 aes(fill = damage_crops),
                 shape = 21, size = 3, color = "black") +
      labs(x = "Start Time", y = "ERA5 max", fill = "Damage Crops") +
      scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
    
    ## Create patchwork panel for plots p2 to p7
    patch_panel <- (p2 + p3 + p4 + p5 + p6 + p7) + 
      plot_layout(ncol = 2, nrow = 3)
    
    ## Save the individual p1 and the patchwork panel in the list,
    ## using the file basename as the key for later retrieval.
    plots_list[[basename(file)]] <- list("p1" = p1, "patch" = patch_panel)
  }
  
  # Return the list of plots
  return(plots_list)
}

# Assume final_plots is the list returned by plot_excess_heat()
final_plots <- plot_excess_heat(here::here("data", "output", "05_validation", "summary"))

for(name in names(final_plots)){
  plot_obj <- final_plots[[name]]
  
  # Create file names (modify as needed)
  png_path_p1   <- here("figures", paste0("06_",name, "_p1.png"))
  svg_path_p1   <- here("figures", paste0("06_",name, "_p1.svg"))
  png_path_patch <- here("figures", paste0("06_",name, "_patch.png"))
  svg_path_patch <- here("figures", paste0("06_",name, "_patch.svg"))
  
  # Save p1
  ggsave(filename = png_path_p1, plot = plot_obj$p1, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_p1, plot = plot_obj$p1, width = 9, height = 6, device = "svg")
  
  # Save patch panel (p2 to p7)
  ggsave(filename = png_path_patch, plot = plot_obj$patch, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_patch, plot = plot_obj$patch, width = 9, height = 6, device = "svg")
}
