#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce recall time series plots

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s05_recall_time_series.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# # Time series recall ----
# combine_validation_daily <- function(input_folder, year_range) {
#   
#   # input_folder <- here("data", "output", "04_validation", "recall", "day", "record", "excess_heat")
#   # year_range <- 1996:2023
#   
#   message("Listing CSV files in folder: ", input_folder)
#   csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
#   message("Found ", length(csv_files), " CSV files.")
#   
#   if(length(csv_files) == 0) {
#     stop("No CSV files found in ", input_folder)
#   }
#   
#   # Extract a 4-digit year from each filename (e.g., "2000_validation_results.csv")
#   extracted_years <- as.numeric(str_extract(basename(csv_files), "\\d{4}"))
#   message("Extracted years from filenames: ", paste(extracted_years, collapse = ", "))
#   
#   # Subset to files whose extracted year is in the specified year_range
#   valid_files <- csv_files[extracted_years %in% year_range]
#   message("Using ", length(valid_files), " files for years: ", paste(year_range, collapse = ", "))
#   
#   if(length(valid_files) == 0) {
#     stop("No CSV files found in ", input_folder, " for years: ", paste(year_range, collapse = ", "))
#   }
#   
#   # Read each CSV file into a data frame
#   df_list <- lapply(valid_files, function(f) {
#     message("Reading file: ", f)
#     read.csv(f, stringsAsFactors = FALSE)
#   })
#   
#   # Combine all data frames into one
#   combined_df <- bind_rows(df_list)
#   message("Combined data has ", nrow(combined_df), " rows.")
#   
#   # Check that a Date column exists and convert it to Date format.
#   if(!"Date" %in% names(combined_df)) {
#     stop("The combined data does not have a 'Date' column.")
#   }
#   # combined_df$Date <- as.Date(combined_df$Date, format = "%d/%m/%Y")
#   
#   # Optionally, filter to only include dates within the specified year_range
#   combined_df <- combined_df %>% filter(year(Date) %in% as.numeric(year_range))
#   
#   # Convert TP and FN to numeric in case they were read as characters.
#   combined_df$TP <- as.numeric(combined_df$TP)
#   combined_df$FN <- as.numeric(combined_df$FN)
#   
#   # Calculate recall for each row (if TP+FN > 0, else NA)
#   combined_df <- combined_df %>% mutate(recall = ifelse((TP + FN) > 0, TP / (TP + FN), NA))
#   
#   message("Aggregation complete. Returning combined data with daily recall per row.")
#   return(combined_df)
# }
# ## 0.25 ----
# 
# ### Excess Heat ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "excess_heat")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_excess_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Flash Flood ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "flash_flood")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_flash_flood <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Flood ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "flood")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_flood <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Heat ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "heat")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_heat  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Heavy Rain ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "heavy_rain")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_heavy_rain  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Hurricane ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "hurricane")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_hurricane  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Tropical Depression ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "tropical_depression")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_tropical_depression <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Hurricane ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "tropical_storm")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_stm1_tropical_storm  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ## 0.39 ----
# ### Excess Heat ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "record", "excess_heat")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# # Ensure the Date column is of class Date
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_record_excess_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### Heat ----
# input_folder <- here("data", "output", "04_validation", "recall", "day", "record", "heat")
# year_range <- 1996:2023
# 
# daily_recall <- combine_validation_daily(input_folder, year_range)
# daily_recall <- daily_recall[complete.cases(daily_recall),]
# # Ensure the Date column is of class Date
# daily_recall$Date <- as.Date(daily_recall$Date)
# 
# p_record_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
#   geom_point(color = "black", alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "black") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = "Year", y = "Recall") +
#   ylim(0,1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# # Heat Plots ----
# final_plot <- (p_stm1_heat + p_record_heat) / (p_stm1_excess_heat + p_record_excess_heat) +
#   plot_annotation(tag_levels = "a")
# 
# # Define file paths using here
# png_path <- here("figures","s06_recall_heat_time_series.png")
# svg_path <- here("figures","s06_recall_heat_time_series.svg")
# 
# # Save the plot as a PNG + SVG file
# ggsave(filename = png_path, plot = final_plot, width = 9, height = 6, dpi = 300)
# ggsave(filename = svg_path, plot = final_plot, width = 9, height = 6, device = "svg")
# 
# # Precip Plots ----
# final_plot <- (p_stm1_flash_flood + p_stm1_flood) / 
#   (p_stm1_heavy_rain + p_stm1_hurricane) / 
#   (p_stm1_tropical_depression + p_stm1_tropical_storm) + 
#   plot_annotation(tag_levels = "a")
# 
# # Define file paths using here
# png_path <- here("figures","s06_recall_precip_time_series.png")
# svg_path <- here("figures","s06_recall_precip_time_series.svg")
# 
# # Save the plot as a PNG + SVG file
# ggsave(filename = png_path, plot = final_plot, width = 9, height = 8, dpi = 300)
# ggsave(filename = svg_path, plot = final_plot, width = 9, height = 8, device = "svg")
# 
# 
# library(dplyr)
# library(ggplot2)
# library(lubridate)
# library(purrr)
# library(scales)    # for hue_pal()

# Excess Heat ----

combine_validation_daily <- function(input_dirs, resolutions, year_range) {
  library(dplyr); library(ggplot2); library(lubridate); library(purrr); library(scales)
  
  # read & combine as before…
  df_list <- map2(input_dirs, resolutions, function(dir, res) {
    csvs <- list.files(dir, "\\.csv$", full.names = TRUE)
    map_dfr(csvs, function(f) {
      df <- read.csv(f, stringsAsFactors=FALSE)
      df %>%
        mutate(Date = as.Date(Date),
               TP   = as.numeric(TP),
               FN   = as.numeric(FN)) %>%
        filter(year(Date) %in% year_range) %>%
        mutate(recall = ifelse((TP+FN)>0, TP/(TP+FN), NA),
               resolution = res) %>%
        filter(!is.na(recall))
    })
  })
  combined_df <- bind_rows(df_list)
  
  # grab default ggplot hues & name them by the actual resolution values
  default_pal <- hue_pal()(3)
  names(default_pal) <- as.character(resolutions)
  
  # override the middle label
  new_labels <- c("0.25", "0.31", "0.39")
  
  # plot
  ggplot(combined_df,
         aes(x = Date, y = recall,
             color = factor(resolution),
             fill  = factor(resolution))) +
    geom_smooth(method = "loess", se = TRUE, size = 1, alpha = 0.5) +
    scale_color_manual(
      name   = "Resolution",
      values = default_pal,
      labels = new_labels
    ) +
    scale_fill_manual(
      name   = "Resolution",
      values = default_pal,
      labels = new_labels
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = "Year", y = "Recall") +
    ylim(0, 1) +
    theme_bw() +
    theme(
      axis.text.x     = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
}

dirs <- c(
  here("data", "output", "05_validation", "recall", "advisory","day", "0.25", "excess_heat"),
  here("data", "output", "05_validation", "recall", "advisory","day", "0.3075", "excess_heat"),
  here("data", "output", "05_validation", "recall", "advisory","day", "0.39", "excess_heat")
)

res <- c(0.25, 0.3075, 0.39)
yrs  <- 1996:2023

p_all <- combine_validation_daily(dirs, res, yrs)

# Define file paths using here
png_path <- here("figures","s06_recall_heat_time_series.png")
svg_path <- here("figures","s06_recall_heat_time_series.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = p_all, width = 9, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = p_all, width = 9, height = 6, device = "svg")

# Precipitation ----

# combine_validation_loess_multihazard <- function(input_dirs,
#                                                  hazard_names,
#                                                  year_range) {
#   # input_dirs   : character vector of length 7 (folders of CSVs)
#   # hazard_names : character vector of length 7 (labels for legend)
#   # year_range   : integer vector of years to include
#   
#   if (length(input_dirs) != 7 || length(hazard_names) != 7) {
#     stop("Please supply exactly seven input_dirs and seven hazard_names.")
#   }
#   
#   # 1. Read & process all CSVs, tagging by hazard
#   df_list <- map2(input_dirs, hazard_names, function(dir, hz) {
#     files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
#     if (length(files) == 0) stop("No .csv files found in ", dir)
#     
#     map_dfr(files, function(f) {
#       message("Reading: ", f)
#       df <- read.csv(f, stringsAsFactors = FALSE)
#       if (!"Date" %in% names(df)) {
#         stop("File ", f, " is missing a Date column.")
#       }
#       df %>%
#         mutate(
#           Date = as.Date(Date),
#           TP   = as.numeric(TP),
#           FN   = as.numeric(FN)
#         ) %>%
#         filter(year(Date) %in% year_range) %>%
#         mutate(
#           recall = ifelse((TP + FN) > 0, TP / (TP + FN), NA),
#           hazard = hz
#         ) %>%
#         filter(!is.na(recall))
#     })
#   })
#   
#   combined_df <- bind_rows(df_list)
#   
#   # 2. Build a 7‑hue palette & name it by hazard
#   pal7 <- hue_pal()(7)
#   names(pal7) <- hazard_names
#   
#   # 3. Plot LOESS curves with alpha = 0.5 on both line & ribbon
#   ggplot(combined_df,
#          aes(x = Date, y = recall,
#              color = hazard,
#              fill  = hazard)) +
#     geom_smooth(method = "loess",
#                 se     = TRUE,
#                 size   = 1,
#                 alpha  = 0.5) +
#     scale_color_manual(name   = "Hazard",
#                        values = pal7) +
#     scale_fill_manual(name   = "Hazard",
#                       values = pal7) +
#     scale_x_date(date_breaks = "1 year",
#                  date_labels = "%Y") +
#     labs(x = "Year",
#          y = "Recall") +
#     ylim(0, 1) +
#     theme_bw() +
#     theme(
#       axis.text.x     = element_text(angle = 90, hjust = 1),
#       legend.position = "bottom"
#     )
# }
# 
# dirs <- c(
#   here("data", "output", "05_validation", "recall", "24hr1yr","flash_flood"),
#   here("data", "output", "05_validation", "recall", "24hr1yr","flood"),
#   here("data", "output", "05_validation", "recall", "24hr1yr","heavy_rain"),
#   here("data", "output", "05_validation", "recall", "24hr1yr","hurricane"),
#   here("data", "output", "05_validation", "recall", "24hr1yr","tropical_depression"),
#   here("data", "output", "05_validation", "recall", "24hr1yr","tropical_storm"),
#   here("data", "output", "05_validation", "recall", "24hr1yr","typhoon")
# )
# 
# hazards <- c(
#   "FlashFlood",
#   "Flood",
#   "HeavyRain",
#   "Hurricane",
#   "TropicalDepression",
#   "TropicalStorm",
#   "Typhoon"
# )
# 
# yrs <- 1996:2023
# 
# p_hazards <- combine_validation_loess_multihazard(dirs, hazards, yrs)
# 
# # Define file paths using here
# png_path <- here("figures","s06_recall_precipitation_time_series.png")
# svg_path <- here("figures","s06_recall_precipitation_time_series.svg")
# 
# # Save the plot as a PNG + SVG file
# ggsave(filename = png_path, plot = p_hazards, width = 9, height = 6, dpi = 300)
# ggsave(filename = svg_path, plot = p_hazards, width = 9, height = 6, device = "svg")

plot_recall_grouped_tropical <- function(input_dirs,
                                         hazard_names,
                                         year_range) {

  
  # Validate inputs
  if (length(input_dirs) != 7 || length(hazard_names) != 7) {
    stop("Please supply exactly seven input_dirs and seven hazard_names.")
  }
  
  # 1. Read every CSV and tag with its hazard
  all_raw <- map2_dfr(input_dirs, hazard_names, function(dir, hz) {
    files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(files) == 0) stop("No .csv files found in ", dir)
    
    map_dfr(files, function(f) {
      df <- read.csv(f, stringsAsFactors = FALSE)
      if (!"Date" %in% names(df)) {
        stop("File ", f, " is missing a Date column.")
      }
      df %>%
        mutate(
          Date   = as.Date(Date),
          TP     = as.numeric(TP),
          FN     = as.numeric(FN),
          hazard = hz
        ) %>%
        filter(year(Date) %in% year_range)
    })
  })
  
  # 2. Re‑group tropical hazards into one category
  summary_df <- all_raw %>%
    mutate(
      hazard_group = if_else(
        hazard %in% c("Hurricane",
                      "TropicalDepression",
                      "TropicalStorm",
                      "Typhoon"),
        "TropicalEvents",
        hazard
      )
    ) %>%
    group_by(Date, hazard_group) %>%
    summarize(
      TP_sum = sum(TP, na.rm = TRUE),
      FN_sum = sum(FN, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(recall = TP_sum / (TP_sum + FN_sum)) %>%
    filter(!is.na(recall))
  
  # 3. Build a 4‑hue palette for the four groups
  pal4 <- hue_pal()(4)
  names(pal4) <- c("FlashFlood", "Flood", "HeavyRain", "TropicalEvents")
  
  # 4. Plot with LOESS and y-axis limited to [0, 0.25] via coord_cartesian
  ggplot(summary_df,
         aes(x = Date, y = recall,
             color = hazard_group,
             fill  = hazard_group)) +
    geom_smooth(method = "loess",
                se     = TRUE,
                size   = 1,
                alpha  = 0.5) +
    scale_color_manual(name   = "Hazard",
                       values = pal4) +
    scale_fill_manual(name   = "Hazard",
                      values = pal4) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    labs(x = "Year", y = "Recall") +
    coord_cartesian(ylim = c(0, 0.25)) +
    theme_bw() +
    theme(
      axis.text.x     = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
}


dirs <- c(
  here("data/output/05_validation/recall/24hr1yr","flash_flood"),
  here("data/output/05_validation/recall/24hr1yr","flood"),
  here("data/output/05_validation/recall/24hr1yr","heavy_rain"),
  here("data/output/05_validation/recall/24hr1yr","hurricane"),
  here("data/output/05_validation/recall/24hr1yr","tropical_depression"),
  here("data/output/05_validation/recall/24hr1yr","tropical_storm"),
  here("data/output/05_validation/recall/24hr1yr","typhoon")
)

hazards <- c(
  "FlashFlood",
  "Flood",
  "HeavyRain",
  "Hurricane",
  "TropicalDepression",
  "TropicalStorm",
  "Typhoon"
)

yrs <- 1996:2023

p <- plot_recall_grouped_tropical(dirs, hazards, yrs)
print(p)

# And to save:
ggsave(here("figures","s06_recall_precipitation_time_series.png"),
       p, width = 9, height = 6, dpi = 300)
ggsave(here("figures","s06_recall_precipitation_time_series.svg"),
       p, width = 9, height = 6, device = "svg")
