




# test read data 




setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory






# Load required libraries
library(dplyr)

# Define the path to the main folder where all yearly folders are stored
main_path <- "Data/Tankerkoenig/prices"  # Update this to your actual folder path

# List all CSV files in the directory and subdirectories
# Recursive = TRUE allows us to look into subdirectories
file_list <- list.files(path = main_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Read all files and store them in a list
data_list <- lapply(file_list, function(file) {
  # Read each file
  tryCatch({
    read.csv(file)
  }, error = function(e) {
    NULL  # In case of an error, return NULL for that file
  })
})

# Remove any NULL values from the list (in case some files couldn't be read)
data_list <- data_list[!sapply(data_list, is.null)]

# Combine all data frames into one
final_data <- bind_rows(data_list)










# more efficient approach:


library(dplyr)

# Define the path to the main folder
main_path <- "Data/Tankerkoenig/prices"  # Update this path accordingly

# List all CSV files in the directory and subdirectories
file_list <- list.files(path = main_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Initialize an empty data frame to store combined results
final_data <- data.frame()

# Process files in batches to avoid memory overload
batch_size <- 100  # Set an appropriate batch size, e.g., 100 files at a time

for (i in seq(1, length(file_list), by = batch_size)) {
  # Select a batch of files
  batch_files <- file_list[i:min(i + batch_size - 1, length(file_list))]
  
  # Read each file in the batch and combine them
  batch_data <- lapply(batch_files, function(file) {
    tryCatch({
      read.csv(file)
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })
  })
  
  # Remove any NULL values
  batch_data <- batch_data[!sapply(batch_data, is.null)]
  
  # Combine the batch and add to final_data
  final_data <- bind_rows(final_data, bind_rows(batch_data))
  
  # Clear memory after each batch
  rm(batch_data)
  gc()  # Run garbage collection to free up memory
}

# final_data now contains all combined data
















# more efficient approach


library(dplyr)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory

# Define the path to the main folder
main_path <- "Data/Tankerkoenig/prices"  # Update this path accordingly

# List all CSV files in the directory and subdirectories
file_list <- list.files(path = main_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Initialize an empty data frame to store combined results
final_data1 <- data.frame()


# Initialize an empty data table to store combined results
final_data <- NULL  # Start with NULL for memory efficiency

# Process files in batches to avoid memory overload
batch_size <- 100  # Set an appropriate batch size, e.g., 100 files at a time


for (i in seq(1, length(file_list), by = batch_size)) {
  # Select a batch of files
  batch_files <- file_list[i:min(i + batch_size - 1, length(file_list))]
  
  # Read and bind each batch
  batch_data <- rbindlist(lapply(batch_files, function(file) {
    tryCatch({
      fread(file)  # Use fread for faster reading
    }, error = function(e) {
      NULL
    })
  }), use.names = TRUE, fill = TRUE)
  
  # Append batch data to final data
  final_data <- rbindlist(list(final_data, batch_data), use.names = TRUE, fill = TRUE)
  
  # Clear memory after each batch
  rm(batch_data)
  gc()
}


















library(dplyr)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory

# Define the path to the main folder
main_path <- "Data/Tankerkoenig/prices"  # Update this path accordingly

# List all CSV files in the directory and subdirectories
file_list <- list.files(path = main_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Check if file_list is empty to prevent further issues
if (length(file_list) == 0) {
  stop("No CSV files found in the specified directory.")
}

# Initialize an empty data table to store combined results
final_data <- NULL  # Start with NULL for memory efficiency

# Experiment with a smaller batch size
batch_size <- 25  # Try smaller values if memory issues persist

# Process files in smaller batches
for (i in seq(1, length(file_list), by = batch_size)) {
  # Select a batch of files
  batch_files <- file_list[i:min(i + batch_size - 1, length(file_list))]
  
  # Read and bind each batch
  batch_data <- rbindlist(lapply(batch_files, function(file) {
    tryCatch({
      fread(file)  # Use fread for faster reading
    }, error = function(e) {
      NULL
    })
  }), use.names = TRUE, fill = TRUE)
  
  # Append batch data to final data
  final_data <- rbindlist(list(final_data, batch_data), use.names = TRUE, fill = TRUE)
  
  # Clear memory after each batch
  rm(batch_data)
  gc()  # Run garbage collection to free up memory
}

# final_data now contains all combined data
















