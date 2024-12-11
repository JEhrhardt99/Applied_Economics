# General Information -----------------------------------------------------


# This Script contains the assesment of the parallel trends assumption for the DID analysis

# Auhtor: Jannes Ehrhardt
# Topic: Applied Economics - Fuel Tx Discount GER 2022
# Date: 2024-12-11

# clear environment
rm(list = ls())

# Load Libraries ----------------------------------------------------------

library(modelsummary)
library(estimatr)
library(lmtest)
library(sandwich)
library(haven)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(data.table)
library(purrr)
library(microbenchmark)
library(lubridate)
library(visdat)
library(zoo)
library(fixest)
library(Hmisc)
library(tseries)
library(forecast)

# Set WD ------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# check if wd is the root directory
getwd() 


# Load Data ---------------------------------------------------------------


df <- fread("../../Data/Preprocessed/final_data.csv.gz")


str(df)



label(df$dummy_GER) <- "$GER_{i}$"
label(df$dummy_FTD) <- "$FTD_{t}$"
label(df$neighbors_count) <- "$X_{i}$"

# create a dummy variable for competition
df$neighbors_dummy <- ifelse(df$neighbors_count > 0, 1, 0)

label(df$neighbors_dummy) <- "$D_{X_{i}}$"

# how many stations have at least one competitor
df |> 
  dplyr::select(station_uuid, neighbors_dummy) |> 
  dplyr::distinct() |>
  dplyr::count(neighbors_dummy)



# Country Averages --------------------------------------------------------

df <- df %>%
  mutate(country = ifelse(dummy_GER == 1, "GER", "FRA"))

# check
unique(df$country) 

# how many german and french stations are in the data
df |> 
  dplyr::select(station_uuid, country) |> 
  dplyr::distinct() |>
  dplyr::count(country)

# create subset for the country averages diesel
df_avg_diesel <- df %>%
  group_by(country, date_only) %>%
  summarise(
    avg_diesel = mean(avg_diesel, na.rm = TRUE),
    .groups = "drop"  # Um die Gruppierung zu entfernen nach der Zusammenfassung
  )

# transform from long to wide
df_avg_diesel_wide <- df_avg_diesel %>%
  pivot_wider(names_from = country, values_from = avg_diesel)

# create subset for the country averages E10
df_avg_e10 <- df %>%
  group_by(country, date_only) %>%
  summarise(
    avg_e10 = mean(avg_e10, na.rm = TRUE),
    .groups = "drop"  # Um die Gruppierung zu entfernen nach der Zusammenfassung
  )

# transform from long to wide
df_avg_e10_wide <- df_avg_e10 %>%
  pivot_wider(names_from = country, values_from = avg_e10)


# # set as time series object
# df_avg_diesel_wide <- ts(df_avg_diesel_wide[, -1],
#                   start = c(2022, 1),
#                   frequency = 365)
# 
# # set as time series object
# df_avg_e10_wide <- ts(df_avg_e10_wide[, -1],
#                          start = c(2022, 1),
#                          frequency = 365)



# # plot the time series
# autoplot(df_avg_diesel_wide)
# autoplot(df_avg_e10_wide)


# Plot the development of the average prices using ggplot2


df_avg_diesel_wide |>
  ggplot(aes(x = date_only)) +
  geom_line(aes(y = FRA, color = "France")) +
  geom_line(aes(y = GER, color = "Germany")) +
  geom_vline(xintercept = as.Date(c("2022-04-01", "2022-06-01", "2022-08-31")),
             linetype = "dashed") +
  labs(title = "Average Diesel Prices in Germany and France",
       x = "Date",
       y = "Price in €/L",
       color = "Country:") +
  scale_color_manual(values = c("France" = "blue", "Germany" = "red")) +  
  theme_few() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10,
                                    face = "bold"))




df_avg_e10_wide |>
  ggplot(aes(x = date_only)) +
  geom_line(aes(y = FRA, color = "France")) +
  geom_line(aes(y = GER, color = "Germany")) +
  geom_vline(xintercept = as.Date(c("2022-04-01", "2022-06-01", "2022-08-31")),
             linetype = "dashed") +
  labs(title = "Average E10 Prices in Germany and France",
       x = "Date",
       y = "Price in €/L",
       color = "Country:") +
  scale_color_manual(values = c("France" = "blue", "Germany" = "red")) +  
  theme_few() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10,
                                    face = "bold"))














































































































































































































































































































































































































































































































































































































