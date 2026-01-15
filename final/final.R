library(censusapi)
library(cowplot)
library(dplyr)
library(sf)
library(tidyr)
library(tidycensus)
library(tigris)

options(tigris_use_cache = TRUE)

# Commute times in the Bay Area, 2000 vs 2019 ----------------------------
# Data sources
#   - ACS: B08303	Travel Time To Work
#   - 2000 Decennial Census:P031 Travel time to work for workers 16 years & over

# Shared code ----------------------------

prep_data <- function(commute) {
  # prepare commute data with common steps for all years
  # return dataframe with mean commute time by tract

  # coerce to numeric and drop NAs
  commute <- commute |>
    mutate(
      mean_travel_time = as.numeric(.data$mean_travel_time)
    ) |>
    filter(!is.na(.data$mean_travel_time))
}

plot_data <- function(commute_data, common_scale, year) {
  # create ggplot object for commute data
  print(paste("plotting year", year))
  plt <- ggplot(commute_data) +
    geom_sf(aes(fill = .data$mean_travel_time), color = NA) +
    scale_fill_viridis_c(limits = common_scale) +
    theme_minimal() +
    labs(
      title = paste(year),
      fill = "Mean Commute Time (minutes)"
    )
  print(plt)
  filename <- paste0("commute_", year, ".png")
  print(paste("writing", filename))
  ggsave(
    filename,
    path = "final",
    plot = plt,
    width = 10,
    height = 6
  )
  plt
}

# Setup ----------------------------
bay_counties <- c(
  "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
  "San Mateo", "Santa Clara", "Solano", "Sonoma"
)
year_latest <- 2019
year_prev <- 2000

# Prep 2019 data ----------------------------
# from ACS subject table
# S0801_C01_046 =
#   Estimate!!Total!!
#   Workers 16 years and over who did not work from home!!
#   TRAVEL TIME TO WORK!!
#   Mean travel time to work (minutes)
commute_latest <- get_acs(
  geography = "tract",
  state = "CA",
  county = bay_counties,
  survey = "acs5",
  variables = "S0801_C01_046",
  year = year_latest,
  geometry = TRUE,
  cache_table = TRUE,
) |>
  rename(mean_travel_time = estimate)

mean_commute_latest <- prep_data(commute_latest)

# Prep 2000 data ----------------------------
# from SF3 transportation profile
# P033001 =
#   Aggregate travel time to work (in minutes)
#   by travel time to work
#   by means of transportation to work
#   for workers 16 years and over
#   who did not work at home
# P031002 =
#   Total!!Did not work at home
#   TRAVEL TIME TO WORK FOR WORKERS 16 YEARS AND OVER
commute_prev <- get_decennial(
  geography = "tract",
  state = "CA",
  county = bay_counties,
  variables = c(
    total_time = "P033001",
    total_workers = "P031002"
  ),
  sumfile = "sf3",
  year = year_prev,
  geometry = TRUE,
  cache_table = TRUE,
  output = "wide"
) |>
  # calculate mean travel time
  mutate(
    mean_travel_time = total_time / total_workers
  )

mean_commute_prev <- prep_data(commute_prev)

# Create maps ----------------------------
# create a common scale so colors will be the same on both plots
common_scale <- range(
  c(
    mean_commute_prev$mean_travel_time,
    mean_commute_latest$mean_travel_time
  ),
  na.rm = TRUE
)
plot_prev <- plot_data(mean_commute_prev, common_scale, year_prev)
plot_latest <- plot_data(mean_commute_latest, common_scale, year_latest)
# plot side by side
combined_plot <- plot_grid(plot_prev, plot_latest, ncol = 2)
ggsave("commute_comparison.png",
  path = "final",
  plot = combined_plot,
  width = 16,
  height = 6
)
# TODO:
# single legend for both plots
# crop to urban area boundary

# visualization options
# simplest: mean commute time color scale by tract
# more complex: bivariate choropleth with commute time buckets and urban area
#   - don't use bubbles for mean commute time since it's hard to compare areas
