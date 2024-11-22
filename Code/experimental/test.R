



# Small Example -----------------------------------------------------------


data <- read.csv("https://dev.azure.com/tankerkoenig/362e70d1-bafa-4cf7-a346-1f3613304973/_apis/git/repositories/0d6e7286-91e4-402c-af56-fa75be1f223d/items?path=/prices/2021/01/2021-01-01-prices.csv&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0&download=true")

data2 <- read.csv("https://dev.azure.com/tankerkoenig/362e70d1-bafa-4cf7-a346-1f3613304973/_apis/git/repositories/0d6e7286-91e4-402c-af56-fa75be1f223d/items?path=/prices/2021/01/2021-01-02-prices.csv&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0&download=true")

data3 <- read.csv("https://dev.azure.com/tankerkoenig/362e70d1-bafa-4cf7-a346-1f3613304973/_apis/git/repositories/0d6e7286-91e4-402c-af56-fa75be1f223d/items?path=/prices/2021/01/2021-01-03-prices.csv&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0&download=true")


# This actually gets the data correctly.

# Now I want to create a for-loop that iterates over the years, starting from 2021-01-01 to 2021-01-03

# I want to create a list of dataframes, where each dataframe is a day of the year.

# the code:

data_list <- list()

for (i in 1:3) {
  data_list[[i]] <- read.csv(paste0("https://dev.azure.com/tankerkoenig/362e70d1-bafa-4cf7-a346-1f3613304973/_apis/git/repositories/0d6e7286-91e4-402c-af56-fa75be1f223d/items?path=/prices/2021/01/2021-01-0", i, "-prices.csv&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0&download=true"))
}

# Now I want to combine this into one dataframe

data_list <- do.call(rbind, data_list)






# Big Example -------------------------------------------------------------





# 
# 
# 
# # Load required library for date manipulation
# library(lubridate)
# 
# # Define the start and end dates
# start_date <- as.Date("2021-01-01")
# end_date <- as.Date("2023-12-31")
# 
# # Create a sequence of dates from start to end
# date_sequence <- seq(start_date, end_date, by = "day")
# 
# # Initialize an empty list to store daily data frames
# data_list <- list()
# 
# # Loop through each date in the sequence
# for (i in seq_along(date_sequence)) {
#   
#   # Extract year, month, and day components for the URL
#   year <- format(date_sequence[i], "%Y")
#   month <- format(date_sequence[i], "%m")
#   day <- format(date_sequence[i], "%d")
#   
#   # Construct the URL for each date
#   url <- paste0("https://dev.azure.com/tankerkoenig/362e70d1-bafa-4cf7-a346-1f3613304973/_apis/git/repositories/0d6e7286-91e4-402c-af56-fa75be1f223d/items?path=/prices/",
#                 year, "/", month, "/", year, "-", month, "-", day,
#                 "-prices.csv&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0&download=true")
#   
#   # Try to read the CSV from the constructed URL
#   # Using tryCatch to handle any potential errors if the URL doesn't exist for a certain date
#   data_list[[i]] <- tryCatch({
#     read.csv(url)
#   }, error = function(e) {
#     NULL  # If the URL is invalid or data is missing, return NULL
#   })
# }
# 
# # Filter out any NULL values in the list (in case some dates had no data)
# data_list <- data_list[!sapply(data_list, is.null)]
# 
# # Combine all data frames into one data frame
# final_data <- do.call(rbind, data_list)
# 
# 








library(bonn)




geographies <- get_geographies()


# ID: GEM


themes <- get_themes(geography = "GEM")


# ID: 060 (Private Einkommen und Schulden)



variables <- get_variables(theme = "060", geography = "GEM")


# KurznamePlus: Kaufkraft 



# get_metadata(variable = "Kaufkraft")


get_data(variable = "246", geography = "GEM", time = NULL)








get_variables(theme, geography)








get_variables()

data <- get_data()






data <- get_data(variable="011", geography="KRE")



