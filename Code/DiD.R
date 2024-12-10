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
library(tidyverse)
library(data.table)
library(purrr)
library(microbenchmark)
library(lubridate)
library(visdat)
library(zoo)
library(fixest)
library(Hmisc)


# Set WD ------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# check if wd is the root directory
getwd() 


# Load Data ---------------------------------------------------------------


df <- fread("../../Data/Preprocessed/final_data.csv.gz")

## Data Prep ---------------------------------------------------------------

# most data preprocessing is done in Data_Prep.qmd or Merge_Prep.R
# still, some steps are included here to make the data ready for the analysis and a nice display of output

# add label to the dummy term variable for a nice display in the modelsummary later,
# since the var_labels can not be used if if coef_rename function is active

label(df$dummy_GER) <- "$GER_{i}$"
label(df$dummy_FTD) <- "$FTD_{t}$"
label(df$neighbors_count) <- "$X_{i}$"

# create a dummy variable for competition
df$neighbors_dummy <- ifelse(df$neighbors_count > 0, 1, 0)

# label for nice display
label(df$neighbors_dummy) <- "$D_{X_{i}}$"

# how many stations have at least one competitor
df |> 
  dplyr::select(station_uuid, neighbors_dummy) |> 
  dplyr::distinct() |>
  dplyr::count(neighbors_dummy)


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
  coef_map = var_labels, 
  gof_map = gof_labels
  # escape = FALSE,
  # output = "test1.tex"
)



# done:

# next steps: Display this nicely in a table
# include R2 within and look up the Meaning
# make a table to compare with and without fixed effects for each fuel respectively
# look up how to cluster standard errors in fixest
# implement it


# to do:

# estimate FE model again with shorter time period arount the introduction point
# => see if the point estimates are higher... First indicator for decline in pass through rate

# Only than I can start with the competition metric and the dynamic DiD Approach



## FE with shorter time period ---------------------------------------------------------------

# 2 Weeks around Introduction
df_weeks_2 <- df[date_only >= "2022-05-18" & date_only <= "2022-06-14"]

# rename station_uuid to Station so it looks nicer in the output later when adding clustered Standard Errors
df_weeks_2 <- df_weeks_2 %>%
  rename(Station = station_uuid)



# test: use fixest panel() function to let R now who the individuals are and what the time is
# this is needed since we have an unbalenced panel
# Mr. Huse said that this is necceaary to get the correct standard errors, but it does not change anything


panel(df_weeks_2, panel.id = c("Station", "date_only"))



# FE Model with 2 Weeks around the introduction of the FTD in Germany (Diesel)
DiD_FE_d_w_2 <- feols(
  avg_diesel ~ dummy_GER:dummy_FTD | Station + date_only,
  data = df_weeks_2,
  vcov = ~Station
)

summary(DiD_FE_d)

# FE Model with 2 Weeks around the introduction of the FTD in Germany (E10)
DiD_FE_E10_w_2 <- feols(
  avg_e10 ~ dummy_GER:dummy_FTD | Station + date_only,
  data = df_weeks_2,
  vcov = ~Station
)

summary(DiD_FE_E10)


# Add to FE models list
models_FE[["$p_{it}$ (Diesel) FE W2"]] <- DiD_FE_d_w_2
models_FE[["$p_{it}$ (E10) FE W2"]] <- DiD_FE_E10_w_2


options("modelsummary_format_numeric_latex" = "plain")

# create a label for the dummy term variable 



# Generate modelsummary table
modelsummary(
  models_FE,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  # coef_map = var_labels,
  gof_map = gof_labels,
  fmt = 4,
  latex = TRUE,
  coef_rename = TRUE # kann nicht in Verbindung mit coef_map gemacht werden
  # escape = FALSE,
  # output = "test1.tex"
)






# Competition Baseline ----------------------------------------------------

## Competition Dummy ----------------------------------------------------

### Period ----------------------------------------------------

# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (Diesel)
DiD_Comp_Dummy_d <- feols(
  avg_diesel ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_dummy | Station + date_only,
  data = df_period,
  vcov = ~Station
)

summary(DiD_Comp_Dummy_d)

# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (E10)
DiD_Comp_Dummy_e10 <- feols(
  avg_e10 ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_dummy | Station + date_only,
  data = df_period,
  vcov = ~Station
)

summary(DiD_Comp_Dummy_e10)


### 2 Weeks ----------------------------------------------------

# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (Diesel)
DiD_Comp_Dummy_d_w_2 <- feols(
  avg_diesel ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_dummy | Station + date_only,
  data = df_weeks_2,
  vcov = ~Station
)

summary(DiD_Comp_Dummy_d_w_2)



# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (E10)
DiD_Comp_Dummy_e10_w_2 <- feols(
  avg_e10 ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_dummy | Station + date_only,
  data = df_weeks_2,
  vcov = ~Station
)

summary(DiD_Comp_Dummy_e10_w_2)


# inizialize list
models_comp_dummy <- list()

# Models without comp dummy
models_comp_dummy[["$p_{it}$ (Diesel) FE"]] <- DiD_FE_d
models_comp_dummy[["$p_{it}$ (Diesel) FE 2 Weeks"]] <- DiD_FE_d_w_2
models_comp_dummy[["$p_{it}$ (E10) FE"]] <- DiD_FE_E10
models_comp_dummy[["$p_{it}$ (E10) FE 2 Weeks"]] <- DiD_FE_E10_w_2

# Models with comp dummy
models_comp_dummy[["$p_{it}$ (Diesel)"]] <- DiD_Comp_Dummy_d
models_comp_dummy[["$p_{it}$ (Diesel) 2 Weeks"]] <- DiD_Comp_Dummy_d_w_2
models_comp_dummy[["$p_{it}$ (E10)"]] <- DiD_Comp_Dummy_e10
models_comp_dummy[["$p_{it}$ (E10) 2 Weeks"]] <- DiD_Comp_Dummy_e10_w_2


# Generate modelsummary table
modelsummary(
  models_comp_dummy,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  # coef_map = var_labels,
  gof_map = gof_labels,
  fmt = 4,
  latex = TRUE,
  coef_rename = TRUE # kann nicht in Verbindung mit coef_map gemacht werden
  # escape = FALSE,
  # output = "test1.tex"
)





## Competition Integer ----------------------------------------------------



### Period ----------------------------------------------------

# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (Diesel)
DiD_Comp_d <- feols(
  avg_diesel ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_count | Station + date_only,
  data = df_period,
  vcov = ~Station
)

summary(DiD_Comp_d)

# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (E10)
DiD_Comp_e10 <- feols(
  avg_e10 ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_count | Station + date_only,
  data = df_period,
  vcov = ~Station
)

summary(DiD_Comp_e10)


### 2 Weeks ----------------------------------------------------

# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (Diesel)
DiD_Comp_d_w_2 <- feols(
  avg_diesel ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_count | Station + date_only,
  data = df_weeks_2,
  vcov = ~Station
)

summary(DiD_Comp_d_w_2)



# FE Model with Competition Dummy and 2 Weeks around the introduction of the FTD in Germany (E10)
DiD_Comp_e10_w_2 <- feols(
  avg_e10 ~ dummy_GER:dummy_FTD + dummy_GER:dummy_FTD:neighbors_count | Station + date_only,
  data = df_weeks_2,
  vcov = ~Station
)

summary(DiD_Comp_e10_w_2)


# Models with comp integer
models_comp_dummy[["$p_{it}$ (Diesel) Int"]] <- DiD_Comp_d
models_comp_dummy[["$p_{it}$ (Diesel) 2 Weeks Int"]] <- DiD_Comp_d_w_2
models_comp_dummy[["$p_{it}$ (E10) Int"]] <- DiD_Comp_e10
models_comp_dummy[["$p_{it}$ (E10) 2 Weeks Int"]] <- DiD_Comp_e10_w_2





# Custom GoF labels: Include all desired GoF statistics
# R2 and R2 adjusted are not needed when FE are implemented
gof_labels <- tribble(
  ~raw,            ~clean,                ~fmt,      # Specify raw names, desired label, and format
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
  models_comp_dummy,
  stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  # coef_map = var_labels,
  gof_map = gof_labels,
  fmt = 4,
  latex = TRUE,
  coef_rename = TRUE # kann nicht in Verbindung mit coef_map gemacht werden
  # escape = FALSE,
  # output = "test1.tex"
)




# Intuition of the positive sign of delta in the 2Weeks Diesel Specification:
# Since only 2 Weeks are used, this data reflects the pricing behavior right around the intro
# of the FTD. During this time, the medial attention was especially high. 
# It might be that especially stations with low competition thaught that they might be 
# obseved by media and politics and therefore reduced their prices strategically to signal that 
# they indeed pass through the tax reduction to the customers. Once this medial attention is gone,
# the pass through rate might decrease again. This is a first indicator for the decline in pass through rate
# and the importance of media attention for the pass through rate.

# Question for next week. Is there a possibility to include the media attention in the model?

# Next steps to do. Implement the dynamic DiD approach (base line and with the competition metric)


















































































































































































































































































































































































































































































































































