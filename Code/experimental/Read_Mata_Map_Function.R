




# Test of reading data with map function


library(tidyverse)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory

# Define the path to the main folder
main_path <- "Data/Tankerkoenig/prices/2021/01"  # Update this path accordingly


df <- dir(main_path, full.names = TRUE) %>% map_df(read_csv)





























library(tidyverse)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory

# Define the path to the main folder
main_path <- "Data/Tankerkoenig/prices/1/01"  # Update this path accordingly

# Function to safely read CSV files
safe_read_csv <- safely(read_csv)

# List all CSV files in the directory
file <- list.files(path = main_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Initialize an empty list to store combined results
results <- list()

# Read files in chunks to avoid memory issues
batch_size <- 10  # Start with a smaller batch size
for (i in seq(1, length(file_list), by = batch_size)) {
  # Select a batch of files
  batch_files <- file_list[i:min(i + batch_size - 1, length(file_list))]
  
  # Read each file in the batch and store the results
  batch_results <- map(batch_files, ~ possibly(read_csv, otherwise = NULL)(.x)) %>%
    compact()  # Remove any errors (NULLs)
  
  # Append batch results to the results
  results <- c(results, batch_results)
  
  # Clear memory after each batch
  gc()
}

# Combine all data frames into one
final_data <- bind_rows(results)

# Check the structure of the final combined data
(final_data)







