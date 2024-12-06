# General Information -----------------------------------------------------


# This Script will contains the main analysis

# Auhtor: Jannes Ehrhardt
# Topic: Applied Economics - Fuel Tx Discount GER 2022
# Date: 2024-12-05

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
library(fixest)

# Set WD ------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# check if wd is the root directory
getwd() 


# Load Data ---------------------------------------------------------------


df <- fread("../../Data/Preprocessed/final_data.csv.gz")



# 1. DiD basic ------------------------------------------------------------

## Test ---------------------------------------------------------------

DiD_basic <- lm_robust(avg_diesel ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                       data = df)

# Custom labels for Regression Coefficients
var_labels <- c(
  "(Intercept)" = "$\\beta_{0}$",
  "avg_diesel" = "$p_{it}$",
  "avg_e10" = "$p_{it}$",
  "dummy_GER" = "$GER_{i}$",
  "dummy_FTD" = "$FTD_{t}$",
  "station_uuid" = "Station ID",
  "dummy_GER:dummy_FTD" = "$GER_{i} \\times FTD_{t}$"
)

modelsummary(DiD_basic,
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
             gof_omit = "^(?!R2|Num)",
             coef_map = var_labels)


# Custom GoF labels: Include all desired GoF statistics
gof_labels <- tribble(
  ~raw,            ~clean,                ~fmt,      # Specify raw names, desired label, and format
  "within_r2",     "$R^2_{\\text{within}}$", 3,
  "r.squared",     "$R^2$",               3,         # Format to 2 decimal places
  "adj.r.squared", "Adjusted $R^2$",      3,
  "nobs",          "$N$", 0       # No decimals for nobs
)

# Generate modelsummary table
modelsummary(
  DiD_basic,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_map = var_labels, # Apply custom coefficient labels
  gof_map = gof_labels
)

### Diesel ---------------------------------------------------------------

DiD_basic_d <- lm_robust(avg_diesel ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                       data = df)

### E10 ---------------------------------------------------------------

DiD_basic_e10 <- lm_robust(avg_e10 ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                         data = df)


# list of models
models <- list()

models[["$p_{it}$ (Diesel) Year"]] <- DiD_basic_d
models[["$p_{it}$ (E10) Year"]] <- DiD_basic_e10



# Generate modelsummary table
modelsummary(
  models,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_map = var_labels, # Apply custom coefficient labels
  gof_map = gof_labels
)


## Correct Period ---------------------------------------------------------------

# France started a TFD on 2022-04-01, Parallel Trends can only be assumed since this period

df_period <- df[date_only >= "2022-04-01" & date_only <= "2022-08-31"]


### Diesel ---------------------------------------------------------------

DiD_basic_d_1 <- lm_robust(avg_diesel ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                         data = df_period)

### E10 ---------------------------------------------------------------

DiD_basic_e10_1 <- lm_robust(avg_e10 ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                           data = df_period)



models[["$p_{it}$ (Diesel)"]] <- DiD_basic_d_1
models[["$p_{it}$ (E10)"]] <- DiD_basic_e10_1


# Generate modelsummary table
modelsummary(
  models,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_map = var_labels, # Apply custom coefficient labels
  gof_map = gof_labels
)




# 2. DiD fixest ---------------------------------------------------

## No FE ---------------------------------------------------------------

DiD_feols <- feols(avg_diesel ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                   data = df_period)


summary(DiD_feols)

## FE ---------------------------------------------------------------

# rename station_uuid to Station so it looks nicer in the output later when adding clustered Standard Errors
df_period <- df_period %>%
  rename(Station = station_uuid)

DiD_FE_d <- feols(avg_diesel ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD |
                    Station + date_only,
                  data = df_period)

summary(DiD_FE_d)

# perfect multicollinearity -> only the interaction term needed

DiD_FE_d <- feols(
  avg_diesel ~ dummy_GER:dummy_FTD | Station + date_only,
  data = df_period,
  vcov = ~Station
)

summary(DiD_FE_d)



DiD_FE_E10 <- feols(
  avg_e10 ~ dummy_GER:dummy_FTD | Station + date_only,
  data = df_period,
  vcov = ~Station
)


summary(DiD_FE_E10)




models_FE <- list()
models_FE[["$p_{it}$ (Diesel) FE"]] <- DiD_FE_d
models_FE[["$p_{it}$ (E10) FE"]] <- DiD_FE_E10


# Get available GoF keys
get_gof(DiD_FE_E10)


# Check clustering information
attr(DiD_FE_E10, "fixef_vars")

# Standard Errors are not clustered yet

# Custom GoF labels: Include all desired GoF statistics
gof_labels <- tribble(
  ~raw,            ~clean,                ~fmt,      # Specify raw names, desired label, and format
  "r.squared",     "$R^2$",               3,         # Format to 2 decimal places
  "adj.r.squared", "$R^2$ (adjusted)",      3,
  "r2.within",     "$R^2_{\\text{within}}$", 3,
  "r2.within.adjusted",     "$R^2_{\\text{within}}$ (adjusted)", 3,
  "rmse",          "RMSE", 3,
  "FE: Station", "$\\alpha_{i}$", NA,
  "FE: date_only", "$\\tau_{t}$", NA,
  "vcov.type",     "Clustered SE",   NA,
  "nobs",          "$N$", 0      
)




# Generate modelsummary table
modelsummary(
  models_FE,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_map = var_labels, # Apply custom coefficient labels
  gof_map = gof_labels
)


# delete wrong models that used the hole year from the models list
models <- models[-c(1,2)]

# Generate modelsummary table comparing fixed effects vsno fixed effects
modelsummary(
  c(models, models_FE),
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_map = var_labels, # Apply custom coefficient labels
  gof_map = gof_labels
)








# next steps: Display this nicely in a table
# include R2 within and look up the Meaning
# make a table to compare with and without fixed effects for each fuel respectively
# estimate FE model again with shorter time period arount the introduction point
# => see if the point estimates are higher... First indicator for decline in pass through rate

# look up how to cluster standard errors in fixest
# implement it

# Only than I can start with the competition metric and the dynamic DiD Approach























































































































































































































































































































































































































































































































































































