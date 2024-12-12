# General Information -----------------------------------------------------


# This Script will be used to merged the preprocessed data from the different 
# sources and create a final dataset for the analysis.

# Auhtor: Jannes Ehrhardt
# Topic: Applied Economics - Fuel Tx Discount GER 2022
# Date: 2024-11-24

# rm(list = ls())

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

# Rename the columns in df_FRA to match the columns in merged_GER
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
# latitude values after imputation. This corresponds to stations that are not only on the market for a short time and do not have spatial data.


df <- df[!station_uuid %in% missing_lat_lon$station_uuid]





# Dummies -----------------------------------------------------------------

# Create a dummy for the FTD period (dummy_FTD = 1 if date_only is between 2022-06-01 and 2022-08-31)
df[, dummy_FTD := ifelse(date_only >= "2022-06-01" & date_only <= "2022-08-31", 1, 0)]


# Spatial Operations --------------------------------------------------------

# clear the environment except for df
rm(list = ls()[!ls() %in% "df"])

# special libraries:
library(sf) # Simple features
library(terra) # Raster data
library(geodata) # For spatial data
library(exactextractr) # Extract raster data


## Extract Cross Sectional Stations -----------------------------------

# How many unique stations?
unique(df$station_uuid) |> length()

# Get unique stations and keep longitude and latitude
stations <- df |> 
  dplyr::select(station_uuid, longitude, latitude) |> 
  dplyr::distinct()

# Turn into SF
stations_sf <- stations |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Plot Stations
ggplot()+
  geom_sf(data = stations_sf)

# there seem to be some outliers
# => from visual inspection, one can see that stations with should be omitted that have longitude < 40 or latitude > 20
# This will be done in df directly
df <- df |> 
  dplyr::filter(longitude < 20, latitude > 40)

# How many unique stations?
unique(df$station_uuid) |> length()

# Get unique stations and keep longitude and latitude
stations <- df |> 
  dplyr::select(station_uuid, longitude, latitude) |> 
  dplyr::distinct()

# Turn into SF
stations_sf <- stations |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Plot Stations
ggplot()+
  geom_sf(data = stations_sf)


## 5km Buffer ---------------------------------------------------------------

# create 5 km buffer around each station
stations_buffer <- stations_sf |> 
  st_buffer(dist = units::as_units(5, "km")) 


# Join Stations and buffers:
nb_list <- st_within(stations_sf, stations_buffer)

# See number of neighboring fuel stations
number_neighbors <- nb_list |> 
  sapply(length)

# make a histogram of the number of neighbors
hist(number_neighbors)


# Join Stations and Buffers
stations_join <- stations_sf |>
  sf::st_join(stations_buffer)

head(stations_join, 15)


# Perform a spatial join of stations with their 5 km buffers
stations_with_neighbors <- st_join(stations_sf, stations_buffer,
                                   join = st_within)


# Count the number of neighbors for each station
station_neighbor_counts <- stations_with_neighbors %>%
  group_by(station_uuid.x) %>%
  summarise(neighbors_count = n())


# Rename the columns to match as before
colnames(station_neighbor_counts) <- c("station_uuid", "neighbors_count", "geometry")

str(station_neighbor_counts)

# Convert station_neighbor_counts to a data.table 
setDT(station_neighbor_counts)

# drop geometry
station_neighbor_counts <- station_neighbor_counts %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-geometry)


str(station_neighbor_counts)
str(df)



# Perform the join: Add neighbors_count to df
df <- merge(df, station_neighbor_counts,
                 by = "station_uuid",
                 all.x = TRUE)

# Check the result
str(df)

summary(df$neighbors_count)

# The way this new variable was calculated, if a station has no competitors inside the radius, it will be shown as 1, since itself is inside the buffer
# Therefore, I will subtract 1 from the neighbors_count variable to get the actual number of competitors in the 5 km radius
df$neighbors_count <- df$neighbors_count - 1

summary(df$neighbors_count)

# create a nice historamm that shows the distribution of the number of competitors using ggplot 2 and the ggthemes package (grouped by station_uuid)
ggplot(station_neighbor_counts, aes(x = neighbors_count - 1)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "grey") +
  theme_few() +
  labs(title = "Distribution of the Number of Competitors in a 5 km Radius",
       x = "Number of Competitors",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(neighbors_count)), color = "red", linetype = "dashed")


# Calculate frequencies for labels
label_data <- station_neighbor_counts |>
  dplyr::mutate(neighbors_count_adjusted = neighbors_count - 1) |>
  dplyr::count(neighbors_count_adjusted) # Count occurrences of each value

# Create the histogram with labels
ggplot(station_neighbor_counts, aes(x = neighbors_count - 1)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "grey") +
  geom_text(data = label_data, aes(x = neighbors_count_adjusted, y = n, label = neighbors_count_adjusted), 
            vjust = -0.5, size = 3) +
  theme_few() +
  labs(title = "Distribution of the Number of Competitors in a 5 km Radius",
       x = "Number of Competitors",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(neighbors_count)), color = "red", linetype = "dashed")





# Save Preprocessed Data --------------------------------------------------

fwrite(df, "../../Data/Preprocessed/final_data.csv.gz",
       row.names = FALSE)



# Save Preprocessed Data small --------------------------------------------------

# the Dynamic DiD Approach is very memory intensive, so I will save a smaller version of the data for that

str(df)

# create a new station_uuid variable that is still a unique identifier for the stations, however,
# one that is saved as an integer to save memory space later on
df[, station_id := as.integer(factor(station_uuid))]

# drop the columns that are not needed for the analysis using data.table syntax
df <- df[, .(station_id,
             date_only,
             avg_e10,
             avg_diesel,
             dummy_GER,
             dummy_FTD,
             neighbors_count,
             longitude,
             latitude)]

# save the smaller version of the data
fwrite(df, "../../Data/Preprocessed/final_data_small.csv.gz",
       row.names = FALSE)




































































































































































































































































