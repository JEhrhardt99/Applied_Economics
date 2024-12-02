
# Small working example for my problem with the data imputation


# Load necessary library
library(data.table)

# Create a small data.table to mimic the structure of 'final_data'
set.seed(42)

# Define a small set of station_ids and fuel types
station_ids <- c(101, 102, 103)
fuel_types <- c("Gazole", "E10")

# Create a sequence of dates
dates <- seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "day")

# Create the data.table
final_data_example <- data.table(
  station_id = rep(station_ids, each = length(dates) * length(fuel_types)),
  fuel_type = rep(fuel_types, times = length(station_ids) * length(dates)),
  date = rep(dates, times = length(station_ids) * length(fuel_types)),
  price_value = c(
    1.5, 1.4, NA, 1.45, 1.46, 1.48, 1.47, 1.49, 1.5, 1.52,  # Station 101, Gazole
    1.4, 1.35, NA, 1.42, NA, 1.45, 1.44, 1.46, 1.47, 1.5,    # Station 101, E10
    1.55, 1.57, NA, 1.56, 1.58, 1.6, NA, 1.63, 1.65, NA,     # Station 102, Gazole
    1.3, 1.28, 1.29, NA, 1.31, 1.32, NA, 1.34, 1.36, 1.37,    # Station 102, E10
    1.55, 1.56, 1.58, NA, 1.6, 1.62, NA, 1.65, 1.67, 1.69     # Station 103, Gazole
  )
)

# View the small dataset
print(final_data_example)
