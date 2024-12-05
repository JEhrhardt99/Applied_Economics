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
  "dummy_GER:dummy_FTD" = "$GER_{i} \\times FTD_{t}$"
)

modelsummary(DiD_basic,
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
             gof_omit = "^(?!R2|Num)",
             coef_map = var_labels)


# Custom GoF labels: Include all desired GoF statistics
gof_labels <- tribble(
  ~raw,            ~clean,                ~fmt,      # Specify raw names, desired label, and format
  "r.squared",     "$R^2$",               2,         # Format to 2 decimal places
  "adj.r.squared", "Adjusted $R^2$",      2,
  "nobs",          "$N$", 0       # No decimals for nobs
)

# Generate modelsummary table
modelsummary(
  DiD_basic,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_map = var_labels, # Apply custom coefficient labels
  gof_map = gof_labels
)

## Diesel ---------------------------------------------------------------

DiD_basic_d <- lm_robust(avg_diesel ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                       data = df)

## E10 ---------------------------------------------------------------

DiD_basic_e10 <- lm_robust(avg_e10 ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                         data = df)


# list of models
models <- list()

models[["$p_{it}$ (Diesel)"]] <- DiD_basic_d
models[["$p_{it}$ (E10)"]] <- DiD_basic_e10



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


## Diesel ---------------------------------------------------------------

DiD_basic_d_1 <- lm_robust(avg_diesel ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                         data = df_period)

## E10 ---------------------------------------------------------------

DiD_basic_e10_1 <- lm_robust(avg_e10 ~ dummy_GER + dummy_FTD + dummy_GER:dummy_FTD,
                           data = df_period)



models[["$p_{it}$ (Diesel) New"]] <- DiD_basic_d_1
models[["$p_{it}$ (E10) New"]] <- DiD_basic_e10_1


# Generate modelsummary table
modelsummary(
  models,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_map = var_labels, # Apply custom coefficient labels
  gof_map = gof_labels
)
































































































































































































































































































































































































































































































































































































