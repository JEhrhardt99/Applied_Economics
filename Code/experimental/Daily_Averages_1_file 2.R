



library(data.table)

library(tidyverse)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory


df <- fread("Data/Tankerkoenig/prices/2021/01/2021-01-01-prices.csv")



# How many stations were active on that day?

unique(df$station_uuid) %>% length()

str(df)






# Load necessary library
library(dplyr)

# Example dataframe spanning a two-day period
df <- data.frame(
  datetime = as.POSIXct(c('2024-11-07 23:05:00', '2024-11-07 23:30:00', 
                          '2024-11-08 00:30:00', '2024-11-08 10:15:00',
                          '2024-11-08 15:00:00', '2024-11-08 22:59:00'),
                        tz = "UTC")
)

# View the original dataframe
print("Original Data:")
print(df)

# Adjust the date to reflect the second day only for times between 23:00 and 23:59
df <- df %>%
  mutate(date = ifelse(format(datetime, "%H:%M") >= "23:00", 
                       as.Date(datetime) + 1, 
                       as.Date(datetime))) %>%
  mutate(date = as.character(date))  # Ensure date is stored as a string (YYYY-MM-DD)

# View the updated dataframe
print("Updated Data with Second Day Date (Only for 23:00-23:59) in YYYY-MM-DD format:")
print(df)

str(d)
















































# Load the data.table package
library(data.table)

# Assuming your data is already a data.table named 'df'

# Step 1: Add a new column for the date only, excluding the time component
df[, date_only := as.Date(date)]

# Step 2: Calculate daily averages per station for each fuel type
# Group by station_uuid and date_only, then compute mean for diesel, e5, and e10
daily_averages <- df[, .(
  avg_diesel = mean(diesel, na.rm = TRUE),
  avg_e5 = mean(e5, na.rm = TRUE),
  avg_e10 = mean(e10, na.rm = TRUE)
), by = .(station_uuid)]

# View the result
head(daily_averages)








# Load the data.table package
library(data.table)

# Assuming 'df' is already a data.table

# Adjust the date to treat 23:00-23:59 as the next day
df[, effective_date := as.Date(date + lubridate::hours(1))]

# Now calculate daily averages based on this adjusted date
daily_averages <- df[, .(
  avg_diesel = mean(diesel, na.rm = TRUE),
  avg_e5 = mean(e5, na.rm = TRUE),
  avg_e10 = mean(e10, na.rm = TRUE)
), by = .(station_uuid, effective_date)]

# View the result
head(daily_averages)





