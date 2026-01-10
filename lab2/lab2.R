library(tidycensus)
library(dplyr)
library(sf)
readRenviron("~/.Renviron")
options(tigris_use_cache = TRUE)

# Setup
vars <- load_variables(2019, "acs5")
bay_counties <- c(
  "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
  "San Mateo", "Santa Clara", "Solano", "Sonoma"
)
bay_income <- get_acs(
  geography = "tract", # We request data at the census tract level
  state = "CA", # Target state is California
  county = bay_counties, # We limit the query to a vector of Bay Area counties
  # ACS variable B19019_001 represents median household income
  variables = c(hhincome = "B19019_001"),
  geometry = TRUE # Includes spatial geometry for mapping
)
bay_income <- bay_income |>
  # Removes columns not needed for analysis (tract name, variable label,
  # and margin of error)
  select(-NAME, -variable, -moe) |>
  # Renames the 'estimate' column to 'inc_19' for clarity
  rename(inc_19 = estimate) |>
  na.omit() # Removes rows with any missing values

bay_value <- get_acs(
  geography = "tract", # Data at census tract level
  state = "CA", # California
  county = bay_counties, # Focus on Bay Area counties
  variables = c(hval = "B25077_001"), # ACS variable for median housing value
  geometry = TRUE # Include geometry for mapping
)

bay_value <- bay_value |>
  select(-NAME, -variable, -moe) |> # Drop extra columns
  rename(HVal_19 = estimate) |> # Rename 'estimate' column to 'HVal_19'
  na.omit() # Remove rows with missing values

x <- bay_income$inc_19 # Define a vector 'x' containing the income estimates
mean(x) # Calculate the mean (average) household income
median(x)
# Define a function to compute the mode (most frequent value)
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
getmode(x) # Return the mode of income

# Plot a histogram to visualize the distribution of household income
hist(x,
  main = "Bay Area Household Income (ACS 2019)", # Main title
  xlab = "Income"
) # X-axis label

# Add vertical dashed lines at the 25th, 50th (median), and 75th percentiles
abline(
  v = quantile(x, probs = c(.25, .5, .75)),
  col = c("blue", "red", "green"),
  lty = 2, lwd = 2
)

# Define a vector 'y' with median housing values for each tract
y <- bay_value$HVal_19
mean(y) # Compute the mean (average) housing value
median(y)
getmode(y)

bay_income_0 <- bay_income |>
  mutate(
    quantile_group = cut( # Divide income into 4 quartiles
      inc_19,
      breaks = quantile(
        inc_19,
        probs = seq(0, 1, by = 0.25),
        na.rm = TRUE
      ) # Set quartile cut-points
    ),
    lower_cut = as.numeric( # Extract the lower bound from each quartile label
      sub("\\((.+),.*", "\\1", quantile_group)
    ),
    order = as.integer( # Convert lower bound to an ordered numeric value
      as.ordered(lower_cut)
    ),
    friendly_label = factor( # Assign readable labels (1st–4th Quantile)
      order,
      labels = c("1st Quantile", "2nd Quantile", "3rd Quantile", "4th Quantile")
    ),
    friendly_informative_label = paste(
      # Combine label with original range for clarity
      friendly_label,
      quantile_group
    )
  )

brk <- quantile(
  BayIncQ$inc_19,
  probs = seq(0, 1, 0.25)
) # Define quartile break points for the histogram

hist(BayIncQ$inc_19, # Plot histogram of household income
  main = "Bay Area Income by Quantile", # Title for the plot
  xlab = "Household Income", # X-axis label
  breaks = brk
) # Use quartile breaks as bin edges

abline(
  v = brk, # Draw vertical lines at quartile boundaries
  col = "grey", # Set line color to grey
  lty = 2, lwd = 2
) # Use dashed lines with medium thickness

# Assignment
# Use the Bay_val dataset to replicate the steps from the
# income quantile section.
# Create a new object (e.g., BayValQ) and apply the mutate() pipeline to:
# Cut HVal_19 into 4 quantiles.
# Extract lower bounds, create an ordered factor, and assign friendly labels.
# Generate a histogram with quantile-based bins.
# Write a short interpretation of the distribution you see.
# Submit both your histogram and summary through the discussion board.
# 💡 Bonus: Try using ggplot2 to recreate the histogram with
# labeled colors for each quantile!
