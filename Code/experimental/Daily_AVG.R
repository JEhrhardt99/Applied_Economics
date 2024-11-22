
library(ggplot2)
library(ggthemes)
library(data.table)
library(tidyverse)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory


df <- fread("Data/Tankerkoenig/prices/2021/01/2021-01-01-prices.csv")

# Convert the 'date' column to POSIXct and handle time zone
df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S%z", tz = "Europe/Berlin")


# How many stations were active on that day?

unique(df$station_uuid) %>% length()

str(df)


unique(df$dieselchange)



summary(df$diesel)





# Group by station_uuid and date_only, then compute mean for diesel, e5, and e10
daily_averages <- df[, .(
  avg_diesel = mean(diesel, na.rm = TRUE),
  avg_e5 = mean(e5, na.rm = TRUE),
  avg_e10 = mean(e10, na.rm = TRUE)
), by = .(station_uuid)]

# View the result
head(daily_averages)

# count the number of observations of daily_averages

nrow(daily_averages)

# should be the same as the number of unique station_uuids

unique(df$station_uuid) %>% length()

# nice, it is the same


# Now we want to add the location data to the daily_averages data frame

# read the station data

