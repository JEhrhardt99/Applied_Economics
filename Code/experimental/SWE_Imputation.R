
# Small working example for my problem with the data imputation


# Load necessary library
library(data.table)

# Set seed for reproducibility
set.seed(42)

# Create a small example data.table to mimic your structure
final_data_example <- data.table(
  station_id = c(101, 101, 102, 102, 103, 103, 101, 102, 103),  # 3 stations
  fuel_type = c("Gazole", "E10", "Gazole", "E10", "Gazole", "E10", "Gazole", "E10", "Gazole"),
  price_value = c(1.5, NA, 1.6, NA, 1.65, 1.5, 1.55, NA, 1.7),  # Some NAs for missing prices
  date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02", 
                   "2022-01-03", "2022-01-03", "2022-01-04", "2022-01-04", 
                   "2022-01-05")),  # Dates for each observation
  longitude = rep(c(2.0, 2.1, 2.2), each = 3),  # Longitude
  latitude = rep(c(48.0, 48.1, 48.2), each = 3)  # Latitude
)

final_data_example <- final_data_example %>% 
  arrange(station_id, fuel_type, date)  # Order by station_id and date








# Identify stations with at least 2 consecutive NAs
stations_with_na_streak_alt <- final_data_example %>%
  group_by(station_id, fuel_type) %>%
  arrange(station_id, fuel_type, date) %>%
  mutate(na_group = cumsum(!is.na(price_value))) %>%  # Create groups of consecutive NAs
  group_by(station_id, fuel_type, na_group) %>%       # Group by streaks
  summarize(
    streak_length = n(),                              # Count streak length
    is_na_streak = all(is.na(price_value)),           # Confirm streak is all NAs
    .groups = "drop"
  ) %>%
  filter(is_na_streak & streak_length >= 2) %>%      # Filter streaks of 14+ consecutive NAs
  distinct(station_id, fuel_type)                    # Keep unique station_id and fuel_type combinations



# Count stations with long NA streaks
num_stations_with_na_streak_alt <- nrow(stations_with_na_streak_alt)

cat("Number of stations with at least 14 consecutive NAs:", num_stations_with_na_streak_alt, "\n")

# Investigate their contribution to the dataset
stations_with_na_data_alt <- final_data_example %>%
  filter(station_id %in% stations_with_na_streak_alt$station_id)

cat("Total observations contributed by affected stations:", nrow(stations_with_na_data_alt), "\n")
cat("Proportion of total observations:", nrow(stations_with_na_data_alt) / nrow(final_data_example), "\n")



