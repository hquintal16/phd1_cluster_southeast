#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce multiple 7 panel figures of noaa events, intensity, duration, exposed area, impacts

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s04_cluster_noaa_idf_impacts.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

plot_excess_heat <- function(dir_path, date_format = "%m/%d/%Y") {
  
  # Identify all CSV files in the specified directory
  csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store plots per file
  plots_list <- list()
  
  # Loop over each file
  for (file in csv_files) {
    # Read the data
    df <- read_csv(file)
    
    # Standardize date/time columns: Check for start_time alternatives
    if (!"start_time" %in% names(df)) {
      if ("start_datetime" %in% names(df)) {
        df <- df %>% rename(start_time = start_datetime)
      } else if ("start_date" %in% names(df)) {
        df <- df %>% rename(start_time = start_date)
      }
    }
    
    # Standardize date/time columns: Check for end_time alternatives
    if (!"end_time" %in% names(df)) {
      if ("end_date_time" %in% names(df)) {
        df <- df %>% rename(end_time = end_date_time)
      } else if ("end_date" %in% names(df)) {
        df <- df %>% rename(end_time = end_date)
      }
    }
    
    # Standardize duration: Look for duration_hours if duration is missing.
    if (!"duration" %in% names(df)) {
      if ("duration_hours" %in% names(df)) {
        df <- df %>% rename(duration = duration_hours)
        # Convert duration from hours to days
        df <- df %>% mutate(duration = duration / 24)
      }
    }
    
    # Convert start_time to a Date object using the specified format
    df <- df %>% mutate(start_time = as.Date(start_time, format = date_format))
    
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
      geom_point(data = df %>% filter(injuries_direct <= 0),
                 shape = 21, size = 3, color = "black", fill = NA) +
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
    
    # Plot 5: Deaths Indirect
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
final_plots <- plot_excess_heat(here::here("data", "output", "05_validation", "summary","cluster"))

for(name in names(final_plots)){
  plot_obj <- final_plots[[name]]
  
  # Create file names (modify as needed)
  png_path_p1   <- here("figures", paste0("05_",name, "_p1.png"))
  svg_path_p1   <- here("figures", paste0("05_",name, "_p1.svg"))
  png_path_patch <- here("figures", paste0("05_",name, "_patch.png"))
  svg_path_patch <- here("figures", paste0("05_",name, "_patch.svg"))
  
  # Save p1
  ggsave(filename = png_path_p1, plot = plot_obj$p1, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_p1, plot = plot_obj$p1, width = 9, height = 6, device = "svg")
  
  # Save patch panel (p2 to p7)
  ggsave(filename = png_path_patch, plot = plot_obj$patch, width = 9, height = 6, dpi = 300)
  ggsave(filename = svg_path_patch, plot = plot_obj$patch, width = 9, height = 6, device = "svg")
}

# Assume final_plots is the list returned by plot_excess_heat()
final_plots <- plot_excess_heat(here::here("data", "output", "05_validation", "summary","noaa"))

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
