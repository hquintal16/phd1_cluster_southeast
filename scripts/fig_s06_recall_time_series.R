#Setup----
#Updated March 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce recall time series plots

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s06_recall_time_series.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

# Time series recall ----
combine_validation_daily <- function(input_folder, year_range) {
  
  # input_folder <- here("data", "output", "04_validation", "recall", "day", "record", "excess_heat")
  # year_range <- 1996:2023
  
  message("Listing CSV files in folder: ", input_folder)
  csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
  message("Found ", length(csv_files), " CSV files.")
  
  if(length(csv_files) == 0) {
    stop("No CSV files found in ", input_folder)
  }
  
  # Extract a 4-digit year from each filename (e.g., "2000_validation_results.csv")
  extracted_years <- as.numeric(str_extract(basename(csv_files), "\\d{4}"))
  message("Extracted years from filenames: ", paste(extracted_years, collapse = ", "))
  
  # Subset to files whose extracted year is in the specified year_range
  valid_files <- csv_files[extracted_years %in% year_range]
  message("Using ", length(valid_files), " files for years: ", paste(year_range, collapse = ", "))
  
  if(length(valid_files) == 0) {
    stop("No CSV files found in ", input_folder, " for years: ", paste(year_range, collapse = ", "))
  }
  
  # Read each CSV file into a data frame
  df_list <- lapply(valid_files, function(f) {
    message("Reading file: ", f)
    read.csv(f, stringsAsFactors = FALSE)
  })
  
  # Combine all data frames into one
  combined_df <- bind_rows(df_list)
  message("Combined data has ", nrow(combined_df), " rows.")
  
  # Check that a Date column exists and convert it to Date format.
  if(!"Date" %in% names(combined_df)) {
    stop("The combined data does not have a 'Date' column.")
  }
  # combined_df$Date <- as.Date(combined_df$Date, format = "%d/%m/%Y")
  
  # Optionally, filter to only include dates within the specified year_range
  combined_df <- combined_df %>% filter(year(Date) %in% as.numeric(year_range))
  
  # Convert TP and FN to numeric in case they were read as characters.
  combined_df$TP <- as.numeric(combined_df$TP)
  combined_df$FN <- as.numeric(combined_df$FN)
  
  # Calculate recall for each row (if TP+FN > 0, else NA)
  combined_df <- combined_df %>% mutate(recall = ifelse((TP + FN) > 0, TP / (TP + FN), NA))
  
  message("Aggregation complete. Returning combined data with daily recall per row.")
  return(combined_df)
}
## 0.25 ----

### Excess Heat ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "excess_heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_excess_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Flash Flood ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "flash_flood")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_flash_flood <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Flood ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "flood")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_flood <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Heat ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_heat  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Heavy Rain ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "heavy_rain")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_heavy_rain  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Hurricane ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "hurricane")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_hurricane  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Tropical Depression ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "tropical_depression")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_tropical_depression <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Hurricane ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "stm1", "tropical_storm")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
daily_recall$Date <- as.Date(daily_recall$Date)

p_stm1_tropical_storm  <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## 0.39 ----
### Excess Heat ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "record", "excess_heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
# Ensure the Date column is of class Date
daily_recall$Date <- as.Date(daily_recall$Date)

p_record_excess_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Heat ----
input_folder <- here("data", "output", "04_validation", "recall", "day", "record", "heat")
year_range <- 1996:2023

daily_recall <- combine_validation_daily(input_folder, year_range)
daily_recall <- daily_recall[complete.cases(daily_recall),]
# Ensure the Date column is of class Date
daily_recall$Date <- as.Date(daily_recall$Date)

p_record_heat <- ggplot(daily_recall, aes(x = Date, y = recall)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Recall") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Heat Plots ----
final_plot <- (p_stm1_heat + p_record_heat) / (p_stm1_excess_heat + p_record_excess_heat) +
  plot_annotation(tag_levels = "a")

# Define file paths using here
png_path <- here("figures","s06_recall_heat_time_series.png")
svg_path <- here("figures","s06_recall_heat_time_series.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = final_plot, width = 9, height = 6, dpi = 300)
ggsave(filename = svg_path, plot = final_plot, width = 9, height = 6, device = "svg")

# Precip Plots ----
final_plot <- (p_stm1_flash_flood + p_stm1_flood) / 
  (p_stm1_heavy_rain + p_stm1_hurricane) / 
  (p_stm1_tropical_depression + p_stm1_tropical_storm) + 
  plot_annotation(tag_levels = "a")

# Define file paths using here
png_path <- here("figures","s06_recall_precip_time_series.png")
svg_path <- here("figures","s06_recall_precip_time_series.svg")

# Save the plot as a PNG + SVG file
ggsave(filename = png_path, plot = final_plot, width = 9, height = 8, dpi = 300)
ggsave(filename = svg_path, plot = final_plot, width = 9, height = 8, device = "svg")
