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




















