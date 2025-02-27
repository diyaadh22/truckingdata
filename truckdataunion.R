# Load required libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Set working directory
setwd("~/Documents/rproject")

# ------------------ 1️⃣ UNION ALL TRUCK DATA ------------------ #
# Read multiple trucking data files and combine them into one dataset
df_truck_0001 <- read_excel('truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369 <- read_excel('truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226 <- read_excel('truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442 <- read_excel('truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478 <- read_excel('truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539 <- read_excel('truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769 <- read_excel('truck data 1769 (1).xlsx', sheet = 2, skip = 3, .name_repair = 'universal')


df <- rbind(df_truck_0001, df_truck_0369, df_truck_1226, df_truck_1442, df_truck_1478, df_truck_1539, df_truck_1769)

# ------------------ 2️⃣ JOIN IN PAY DATA ------------------ #
# Read driver pay data
df_pay <- read_excel('Driver Pay Sheet.xlsx', .name_repair = 'universal')

# Perform a left join to include pay data for each truck
df <- left_join(df, df_pay, by = c('Truck.ID'))

# ------------------ 3️⃣ CALCULATE NUMBER OF LOCATIONS ------------------ #
# Group by truck ID to count the number of locations each truck operated from
df_starting_Pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

# ------------------ 4️⃣ CALCULATE PAY PER DRIVER ------------------ #
# Calculate expenses
df$fuel_cost <- df$Gallons * df$Price.per.Gallon
df$other_expenses <- df$Misc + df$Tolls
df$total_expenses <- df$fuel_cost + df$other_expenses

# Calculate total miles driven
df$total_miles_driven <- df$Odometer.Ending - df$Odometer.Beginning

# Calculate wages per driver based on miles driven
df$Wages_per_driver <- df$total_miles_driven * df$labor_per_mil

# Summarize salary per driver
df_Wages_per_driver <- df %>%
  group_by(Truck.ID) %>%
  summarize(
    count = n(),
    salary_by_driver = sum(Wages_per_driver, na.rm = TRUE)
  )

# ------------------ 5️⃣ PLOT WAGES PER DRIVER (COLORED BAR CHART) ------------------ #
ggplot(df_Wages_per_driver, aes(x = Truck.ID, y = salary_by_driver, fill = Truck.ID)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("red", "green", "blue", "yellow", "orange", "purple", "pink")) +  # Assign unique colors
  labs(title = "Wages per Driver", x = "Truck ID", y = "Salary") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = 1))
