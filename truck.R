# Load required libraries
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)  # For color palettes

# Read the dataset
df_truckdata <- read_excel('NP_EX_1-2.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')

# Select relevant columns
df <- df_truckdata[, c(4:15)]
df <- subset(df, select = -c(...10))  # Drop unnecessary columns

# Convert date range
date_min <- min(df$Date, na.rm = TRUE)
date_max <- max(df$Date, na.rm = TRUE)
number_of_days_on_the_road <- as.numeric(difftime(date_max, date_min, units = "days"))

# Total hours calculation
total_hours <- sum(df$Hours, na.rm = TRUE)
number_of_days_recorded <- nrow(df)
avg_hrs_per_day_rec <- round(total_hours / number_of_days_recorded, digits = 3)

# Expense calculations
df$fuel_cost <- df$Gallons * df$Price.per.Gallon
df$other_expenses <- df$Misc + df$Tolls
df$total_expenses <- df$fuel_cost + df$other_expenses
df$total_gallons <- sum(df$Gallons, na.rm = TRUE)
df$total_miles <- sum(df$Miles, na.rm = TRUE)
df$miles_per_gallon <- df$total_miles / df$total_gallons
df$cost_per_mile <- df$total_expenses / df$total_miles

# Split 'Starting Location' into warehouse and state
df[c('warehouse', 'starting_city_state')] <- str_split_fixed(df$Starting.Location, ',', 2)
df$starting_city_state <- gsub(',', "", df$starting_city_state)

# Create pivot table for starting locations
df_starting_pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(
    count = n(),
    mean_size_hours = mean(Hours, na.rm = TRUE),
    sd_hours = sd(Hours, na.rm = TRUE),
    total_hours = sum(Hours, na.rm = TRUE),
    total_gallons = sum(Gallons, na.rm = TRUE)
  )

# **Bar chart for starting locations (Fixed `+` issue)**
ggplot(df_starting_pivot, aes(x = starting_city_state, y = count, fill = starting_city_state)) +
  geom_col() +
  scale_fill_viridis_d() +  # Automatically assigns distinct colors
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

# Split 'Delivery Location' into city and state
df[c('Delivery_City', 'Delivery_State')] <- str_split_fixed(df$Delivery.Location, ',', 2)
df$Delivery_State <- gsub(' ', "", df$Delivery_State)

# Create pivot table for delivery locations
df_delivery_pivot <- df %>%
  group_by(Delivery_State) %>%
  summarize(
    count = n(),
    mean_size_hours = mean(Hours, na.rm = TRUE),
    sd_hours = sd(Hours, na.rm = TRUE),
    total_hours = sum(Hours, na.rm = TRUE),
    total_gallons = sum(Gallons, na.rm = TRUE)
  ) %>%
  ungroup()

# **Bar chart for delivery locations (Fixed `+` issue)**
ggplot(df_delivery_pivot, aes(x = Delivery_State, y = count, fill = Delivery_State)) +
  geom_col() +
  scale_fill_viridis_d() +  # Uses Viridis color palette for distinct colors
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
