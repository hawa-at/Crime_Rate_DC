library(tidyverse)
library(lubridate)

crime_2016 <- read.csv("../data/Crime_Incidents_in_2016.csv")
crime_2017 <- read.csv("../data/Crime_Incidents_in_2017.csv")
crime_2024 <- read.csv("../data/Crime_Incidents_in_2024.csv")
crime_2018_2023 <- read.csv("../data/Crime_2018-2023.csv")

# Standardize columns
crime_datasets <- list(crime_2016, crime_2017, crime_2024, crime_2018_2023)

for (i in seq_along(crime_datasets)) {
  crime_datasets[[i]]$Year <- year(as.Date(crime_datasets[[i]]$REPORT_DAT))
}

# Merge datasets
crime_data <- bind_rows(crime_datasets)

# Convert REPORT_DAT to Date format
crime_data <- crime_data %>% mutate(REPORT_DAT = as.Date(REPORT_DAT, format="%Y-%m-%d"))

# Replace NAs with "Unknown" for categorical variables 
categorical_cols <- c("SHIFT", "METHOD", "OFFENSE", "BLOCK", "WARD", "DISTRICT", "PSA")
crime_data[categorical_cols] <- lapply(crime_data[categorical_cols], function(x) ifelse(is.na(x), "Unknown", x))

# Handle missing values in latitude/longitude (remove or impute with median values)
crime_data <- crime_data %>% 
  mutate(
    LATITUDE = ifelse(is.na(LATITUDE), median(LATITUDE, na.rm = TRUE), LATITUDE),
    LONGITUDE = ifelse(is.na(LONGITUDE), median(LONGITUDE, na.rm = TRUE), LONGITUDE)
  )

# Save cleaned data
github_path <- "/Users/hawatoumbou/Documents/GitHub/Crime_Rate_DC/data"
write.csv(crime_data, file.path(github_path, "Cleaned_Crime_Incidents_2016_2024.csv"), row.names = FALSE)

# Summary of cleaned data
summary(crime_data)
