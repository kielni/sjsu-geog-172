# Lab 1 - Kimberly Nicholls

# verify environment setup for Module 2
library(tidyverse)
library(sf)

park_acres <- c(12, 45, 10, 8, 32)

mean_park_size <- mean(park_acres)
print(paste("Mean park size:", mean_park_size))

summary_park_size <- summary(park_acres)
print("Summary of park sizes:")
print(summary_park_size)

#' Convert acres to square feet
to_square_feet <- function(acres) {
  acres * 43560
}

# test to_square_feet function
park_sizes_sqft <- to_square_feet(park_acres)
print("Park sizes in square feet:")
print(park_sizes_sqft)



# Check your version
version_check <- packageVersion("sf")
print(paste("My SF version is:", version_check))
