# General Information -----------------------------------------------------


# This Script will be used to merged the preprocessed data from the different 
# sources and create a final dataset for the analysis.

# Auhtor: Jannes Ehrhardt
# Topic: Applied Economics - Fuel Tx Discount GER 2022
# Date: 2024-11-24


# Load Libraries ----------------------------------------------------------

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









