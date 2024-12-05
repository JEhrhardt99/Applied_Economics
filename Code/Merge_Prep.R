# General Information -----------------------------------------------------


# This Script will be used to merged the preprocessed data from the different 
# sources and create a final dataset for the analysis.

# Auhtor: Jannes Ehrhardt
# Topic: Applied Economics - Fuel Tx Discount GER 2022
# Date: 2024-11-24

rm(list = ls())

# Load Libraries ----------------------------------------------------------

library(modelsummary)
library(estimatr)
library(lmtest)
library(sandwich)
library(haven)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(tidyverse)
library(data.table)
library(purrr)
library(xml2)
library(XML)
library(microbenchmark)
library(lubridate)
library(visdat)
library(zoo)

# Set WD ------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# check if wd is the root directory
getwd() 


# Load Data ---------------------------------------------------------------

df_FRA <- fread("../../Data/Prix-des-carburants/2022/preprocessed_data_FRA.csv.gz")

df_GERp <- fread("../../Data/Tankerkoenig/prices_avg/Combined/daily_avg_prices_GER.csv.gz")

df_GERs <- fread("../../Data/Tankerkoenig/stations/2022/preprocessed/stations_2022.csv.gz")



# Data Inspection ---------------------------------------------------------

str(df_FRA)

str(df_GERp)

str(df_GERs)

# In df_FRA the id variable ist stored as int -> make it chr to be able to merge later

df_FRA$station_id <- as.character(df_FRA$station_id)

# Rename station_id to station_uuid to match the column name in df_GERp

colnames(df_FRA)[colnames(df_FRA) == "station_id"] <- "station_uuid"

# why so many observations in df_GERs?

unique(df_GERs$uuid) |> length()


# Merge Data -------------------------------------------------------------

# Merge df_GERs (crossectional part) and df_GERp (time series part) on station_id

# Set keys for both data.tables
setkey(df_GERp, station_uuid, date_only)
setkey(df_GERs, uuid, date)

# Merge the two tables
merged_GER <- merge(
  df_GERp,
  df_GERs,
  by.x = c("station_uuid", "date_only"),
  by.y = c("uuid", "date"),
  all.x = TRUE,  # Keep all rows from df_GERp (time series data)
  all.y = FALSE  # Only keep matching rows from df_GERs
)


str(merged_GER)

str(df_FRA)


# Add the GER dummy variable
merged_GER[, dummy_GER := 1]
df_FRA[, dummy_GER := 0]


setnames(df_FRA, old = c("date", "E10", "Gazole"), 
         new = c("date_only", "avg_e10", "avg_diesel"))


# Bind the rows of the two data.tables
df <- rbind(merged_GER, df_FRA, fill = TRUE)



# how many stations in total?
unique(df$station_uuid) |> length()



# see if there are any missings among longitude and latitude


df[is.na(longitude) | is.na(latitude)] |> nrow()
df[is.na(longitude) | is.na(latitude)]

df[is.na(longitude)]
df[is.na(latitude)]

df_weird <- df[is.na(longitude) | is.na(latitude)]

# which stations?
unique(df_weird$station_uuid)

# okay so sometimes, for some stations, longitude and latitude are randomly misssing. 
# This can be solved. The stations are still in the data df, so we can just fill them in with the correct values.
# Stations will not move in space, so the NAs can be filled by a value of the same station. This is the task at hand:

# Group by station_uuid and fill missing values with non-NA ones
df[, latitude := ifelse(all(is.na(latitude)),
                        NA,  # Keep it as NA if all values are missing
                        ifelse(is.na(latitude),
                               max(latitude, na.rm = TRUE),
                               latitude)), 
   by = station_uuid]


df[, longitude := ifelse(all(is.na(longitude)),
                        NA,  # Keep it as NA if all values are missing
                        ifelse(is.na(longitude),
                               max(longitude, na.rm = TRUE),
                               longitude)), 
   by = station_uuid]

# Check if there are any remaining missing values
missing_lat_lon <- df[is.na(longitude) | is.na(latitude)]
nrow(missing_lat_lon)






# how many observations of the station 00061087-0010-4444-8888-acdc00000010 are there in df?
df[station_uuid == "00061087-0010-4444-8888-acdc00000010"] |> nrow()

test <- df[station_uuid == "00061087-0010-4444-8888-acdc00000010"]

# Here the unbalanced panel structure kicks in. This seems to be a station that was only for a short amount
# of time on the market. The station count is rather low, so I will drop all stations that still have no longitude and
# latitude values after imputation.


df <- df[!station_uuid %in% missing_lat_lon$station_uuid]



# Dummies -----------------------------------------------------------------

# Create a dummy for the FTD period (dummy_FTD = 1 if date_only is between 2022-06-01 and 2022-08-31)

df[, dummy_FTD := ifelse(date_only >= "2022-06-01" & date_only <= "2022-08-31", 1, 0)]


# Spatial Operations --------------------------------------------------------




# Save Preprocessed Data --------------------------------------------------

fwrite(df, "../../Data/Preprocessed/final_data.csv.gz",
       row.names = FALSE)







