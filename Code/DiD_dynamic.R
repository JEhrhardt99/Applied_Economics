# General Information -----------------------------------------------------


# This Script contains the main analysis in the dynamic DiD Model

# Auhtor: Jannes Ehrhardt
# Topic: Applied Economics - Fuel Tx Discount GER 2022
# Date: 2024-12-13

# clear environment
# rm(list = ls()) # !!!Haram!!!

# Load Libraries ----------------------------------------------------------


library(modelsummary)
library(estimatr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(data.table)
library(lubridate)
library(xtable)
library(fixest)
library(Hmisc)

# Set WD ------------------------------------------------------------------

# check if wd is the code folder of the project
getwd() 

# Load Data ---------------------------------------------------------------

# load the data only for the relevant time
df <- fread("../../Data/Preprocessed/final_data_small.csv.gz")[date_only >= "2022-04-01" & date_only <= "2022-08-31"]

# repeat the data labeling
Hmisc::label(df$dummy_GER) <- "$GER_{i}$"
Hmisc::label(df$dummy_FTD) <- "$FTD_{t}$"
Hmisc::label(df$neighbors_count) <- "$X_{i}$"

# competition dummy
df$neighbors_dummy <- ifelse(df$neighbors_count > 0, 1, 0)
Hmisc::label(df$neighbors_dummy) <- "$D_{X_{i}}$"

df <- df %>%
  dplyr::rename(Station = station_id)



# Dynamic DiD -------------------------------------------------------

# Define the treatment date
treatment_date <- as.IDate("2022-05-31")

str(df)
str(treatment_date)

# Create the centered_t variable
df[, centered_t := as.integer(date_only - treatment_date)]












# Dynamic DiD Model for Diesel

t0 <- Sys.time() # start time

DiD_dy_diesel <- feols(
  avg_diesel ~ i(centered_t, dummy_GER, ref = 0) | Station + centered_t,
  data = df
)

t1 <- Sys.time()  # end time
print(t1 - t0)    # how long did it take

# Summarize results
# summary(DiD_dy_diesel)

# Plot the dynamic treatment effects
coefplot(DiD_dy_diesel,
         ci.width = "1%")


rm(DiD_dy_diesel)








# Dynamic DiD Model for E10

t0 <- Sys.time() # start time

# Dynamic DiD Model
DiD_dy_e10 <- feols(
  avg_e10 ~ i(centered_t, dummy_GER, ref = 0) | Station + centered_t,
  data = df
)

t1 <- Sys.time()  # end time
print(t1 - t0)    # how long did it take



# Plot the dynamic treatment effects
coefplot(DiD_dy_e10,
         ci.width = "1%")

rm(DiD_dy_e10)







# Dynamic DiD Comp -------------------------------------------------------

# Dynamic DiD Model with interaction of competition dummy
t0 <- Sys.time()

DiD_dy_comp_diesel <- feols(
  avg_diesel ~ i(centered_t, dummy_GER, ref = 0) +
    i(centered_t, dummy_GER, ref = 0):neighbors_dummy | Station + centered_t,
  data = df
)

t1 <- Sys.time()
print(t1 - t0)



###





































































































































































































































































































































































































