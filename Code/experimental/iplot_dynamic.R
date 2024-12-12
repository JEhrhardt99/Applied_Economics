

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("..") # move up to the project root directory
getwd() # check if wd is the root directory

library(fixest)
library(data.table)

df <- fread("../../Data/Preprocessed/final_data.csv.gz")[date_only >= "2022-04-01" & date_only <= "2022-08-31"]





# try with this and it will work
# df_dynamic <- df_weeks_2

# Define the treatment date
treatment_date <- as.IDate("2022-05-31")

str(df)
str(treatment_date)

# Create the centered_t variable
df[, centered_t := as.integer(date_only - treatment_date)]

# Dynamic DiD Model
t0 <- Sys.time()
dynamic_did <- feols(
  avg_diesel ~ i(centered_t, dummy_GER, ref = 0) +
    i(centered_t, dummy_GER, ref = 0):I(neighbors_count == 0) | station_uuid + centered_t,
  data = df
)


t1 <- Sys.time()
print(t1 - t0)

# Summarize results
x <- summary(dynamic_did)

# Plot the dynamic treatment effects
coefplot(dynamic_did,
         ci.width = "1%")

group1 <- feols(
  avg_e10 ~  i(centered_t, dummy_GER, ref = 0) | station_uuid + centered_t,
  data = df[neighbors_count == 0, ]
)
group2 <- feols(
  avg_e10 ~  i(centered_t, dummy_GER, ref = 0) | station_uuid + centered_t,
  data = df[neighbors_count > 0, ]
)

iplot(list(group1, group2), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment')

legend("bottomleft", col = c(1, 2), pch = c(20, 17),
       legend = c("No Neighbors", "Neighbors."))
