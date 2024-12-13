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




# Plot more beautiful

# Extract coefficients and confidence intervals
coef_data <- broom::tidy(DiD_dy_diesel, conf.int = TRUE) %>%
  filter(str_detect(term, "centered_t")) %>%                # Filter for dynamic effects
  mutate(
    centered_t = as.numeric(str_extract(term, "-?\\d+")),   # Extract the time variable
    date_only = treatment_date + centered_t                 # Map to actual dates
  )

# Subset for x-axis labels (e.g., every 10th day and treatment date)
date_mapping <- coef_data %>%
  filter(centered_t %% 10 == 0 | centered_t == 0) %>%  # Select every 10th day
  bind_rows(data.frame(centered_t = 0, date_only = as.IDate("2022-06-01")))  # Manually add treatment date

# Ensure date_mapping is ordered correctly
date_mapping <- date_mapping %>%
  arrange(centered_t)



ftd_indicator <- "FTD: -0.1404€"

# # Create the ggplot with added elements
# ggplot(coef_data, aes(x = centered_t, y = estimate)) +
#   # Shaded ribbon for confidence intervals
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.3) +
#   # Point estimates
#   geom_point(size = 1, color = "black") +
#   # Vertical line for treatment date
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
#   # Horizontal line at y = 0
#   geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
#   # Horizontal line at y = 0
#   geom_hline(yintercept = -0.1404, linetype = "dashed", color = "darkred", size = 0.2) +
#   # Annotate custom text
#   annotate(
#     "text", 
#     x = -45, y = -0.132,  # Set position manually (adjust as needed)
#     label = ftd_indicator, 
#     color = "darkred", 
#     size = 4, 
#     fontface = "italic"
#   ) +
#   # Custom x-axis with dates
#   scale_x_continuous(
#     breaks = date_mapping$centered_t,              # Custom breaks
#     labels = as.character(date_mapping$date_only)  # Corresponding dates
#   ) +
#   # Apply ggthemes' theme_few
#   ggthemes::theme_few(base_size = 12) +
#   # Labels
#   labs(
#     x = "Date (centered around treatment)",
#     y = "Estimated Coefficient",
#     title = "Dynamic Treatment Effects - Diesel",
#     subtitle = "Shaded confidence intervals and treatment start indicator"
#   ) +
#   # Rotate x-axis labels for better visibility
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )




ggplot(coef_data, aes(x = centered_t, y = estimate)) +
  # Shaded ribbon for confidence intervals
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = "Confidence Interval"), 
    alpha = 0.3
  ) +
  # Point estimates
  geom_point(aes(color = "Point Estimate"), size = 1) +
  # Vertical line for treatment date
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  # Horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  # Horizontal line for a reference value
  geom_hline(yintercept = -0.1404, linetype = "dashed", color = "darkred", size = 0.2) +
  # Annotate custom text
  annotate(
    "text", 
    x = -45, y = -0.132,  # Set position manually (adjust as needed)
    label = ftd_indicator, 
    color = "darkred", 
    size = 4, 
    fontface = "italic"
  ) +
  # Custom x-axis with dates
  scale_x_continuous(
    breaks = date_mapping$centered_t,              # Custom breaks
    labels = as.character(date_mapping$date_only)  # Corresponding dates
  ) +
  # Properly label legend entries
  scale_fill_manual(
    name = NULL,  # No title for the legend
    values = "blue",
    labels = "Confidence Intervals (99%)"
  ) +
  scale_color_manual(
    name = NULL,  # No title for the legend
    values = "black",
    labels = expression(paste("Point Estimates (", delta[t], ")"))
  ) +
  # Apply ggthemes' theme_few
  ggthemes::theme_few(base_size = 12) +
  # Labels
  labs(
    x = "Date (centered around treatment)",
    y = "Estimated Coefficient",
    title = "Dynamic Treatment Effects - Diesel",
    subtitle = "Shaded confidence intervals and treatment start indicator"
  ) +
  # Rotate x-axis labels for better visibility and adjust legend placement
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",  # Place legend below the plot
    legend.box = "horizontal"   # Arrange legend elements horizontally
  )












# Plot the dynamic treatment effects
coefplot(DiD_dy_diesel,
         ci.width = "1%",
         ci_level = 0.99)


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





































































































































































































































































































































































































