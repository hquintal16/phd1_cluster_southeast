#Setup----
#Updated March 2025
#Linked to GitHub
#Hunter Quintal
#purpose: update cluster id's 
#study area: Southeast

# Load Libraries & Functions ----
library(here)
here::i_am("scripts/13_cluster_id.R")  # Adjust the file path if needed
source(here::here("scripts", "01_library.R"))

# Define the function to copy and rename .nc files
rename_and_copy_files <- function(input_directory, output_directory) {
  # Resolve directories using here::here
  in_dir <- here::here(input_directory)
  out_dir <- here::here(output_directory)
  
  # Create the output directory if it does not exist
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # List all .nc files with the naming convention yyyy-mm-dd_cluster_x.nc
  files <- list.files(
    path = in_dir,
    pattern = "^\\d{4}-\\d{2}-\\d{2}_cluster_\\d+\\.nc$",
    full.names = TRUE
  )
  
  # If no files match, print a message and exit the function
  if (length(files) == 0) {
    message("No matching .nc files found in ", in_dir)
    return(NULL)
  }
  
  # Extract the date from the filename (assumes date is the first 10 characters)
  file_dates <- as.Date(substr(basename(files), 1, 10))
  
  # Order files by date (oldest first)
  sorted_order <- order(file_dates)
  files_sorted <- files[sorted_order]
  
  # Loop over each file, copy it, and rename with sequential numbering (0001, 0002, ...)
  for (i in seq_along(files_sorted)) {
    original_filename <- basename(files_sorted[i])
    # Extract the date string from the filename
    date_str <- substr(original_filename, 1, 10)
    # Create a new index padded to 4 digits
    new_index <- sprintf("%04d", i)
    # Create the new filename
    new_filename <- paste0(date_str, "_cluster_", new_index, ".nc")
    new_filepath <- file.path(out_dir, new_filename)
    
    # Copy the file from the input to the output directory
    file.copy(from = files_sorted[i], to = new_filepath)
  }
}

# Example usage: loop across five input/output directory pairs
input_dirs <- c(here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm1', 'heat_index', 'raw'),
                here('data', 'output', '03_cluster', '02_cluster', 'points', 'record', 'heat_index', 'raw'),
                here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm4', 'heat_index', 'raw'),
                here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm1', 'precipitation', 'raw'),
                here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm4', 'precipitation', 'raw'))

output_dirs <- c(here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm1', 'heat_index', 'clean'),
                 here('data', 'output', '03_cluster', '02_cluster', 'points', 'record', 'heat_index', 'clean'),
                 here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm4', 'heat_index', 'clean'),
                 here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm1', 'precipitation', 'clean'),
                 here('data', 'output', '03_cluster', '02_cluster', 'points', 'stm4', 'precipitation', 'clean'))

for(i in seq_along(input_dirs)) {
  rename_and_copy_files(input_dirs[i], output_dirs[i])
}
