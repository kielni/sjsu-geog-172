library(tidycensus)
library(censusapi)

library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(tmap)
options(tigris_use_cache = TRUE)

# Commute times in the Bay Area, 2000 vs 2019 ----------------------------
# Data sources
#   - ACS: B08303	Travel Time To Work
#   - 2000 Decennial Census: P031

# Shared code ----------------------------
# clean <- function(df) {


# Setup ----------------------------
bay_counties <- c(
  "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
  "San Mateo", "Santa Clara", "Solano", "Sonoma"
)

# load data for 2019 census tracts and rename variables
commute_latest <- get_acs(
  geography = "tract",
  state = "CA",
  county = bay_counties,
  # the bucket labels are irregular and don't make good variable names,
  # so be explicit when renaming
  variables = c(
    range_0_5 = "B08303_002",
    range_5_9 = "B08303_003",
    range_10_14 = "B08303_004",
    range_15_19 = "B08303_005",
    range_20_24 = "B08303_006",
    range_25_29 = "B08303_007",
    range_30_34 = "B08303_008",
    range_35_39 = "B08303_009",
    range_40_44 = "B08303_010",
    range_45_59 = "B08303_011",
    range_60_89 = "B08303_012",
    range_90_plus = "B08303_013"
  ),
  year = 2019,
  geometry = TRUE,
  cache_table = TRUE,
  survey = "acs5",
)

# set range_0_5, range_5_9, ..., range_90_plus as numeric and drop NAs
commute_latest <- commute_latest |>
  mutate(
    estimate = as.numeric(estimate)
  ) |>
  filter(!is.na(estimate))

# want consistent categories between 2000 and 2019, so set cutoffs manually
commute_latest <- commute_latest |>
  mutate(
    category = case_when(
      # 0-14 minutes = short
      variable %in% c(
        "range_0_5", "range_5_9", "range_10_14"
      ) ~ "short",
      # 15-34 minutes = medium
      variable %in% c(
        "range_15_19", "range_20_24", "range_25_29", "range_30_34",
        "range_35_39"
      ) ~ "medium",
      # 35+ minutes = long
      variable %in% c(
        "range_40_44", "range_45_59",
        "range_60_89", "range_90_plus"
      ) ~ "long",
      TRUE ~ "total"
    )
  ) |>
  st_transform(3857)

# set midpoint 2.5 = for variable = range_0_5
# TODO: test values for 90+
midpoints <- c(
  range_0_5 = 2.5, range_5_9 = 7, range_10_14 = 12,
  range_15_19 = 17, range_20_24 = 22, range_25_29 = 27,
  range_30_34 = 32, range_35_39 = 37, range_40_44 = 42,
  range_45_59 = 52, range_60_89 = 75, range_90_plus = 100
)
# set midpoint based on variable
commute_latest <- commute_latest |>
  mutate(
    midpoint = midpoints[variable]
  )

# calculate mean commute time by tract
mean_commute <- commute_latest |>
  filter(variable != "total") |>
  group_by(GEOID, NAME, geometry) |>
  summarise(
    mean_minutes =
      sum(estimate * midpoint, na.rm = TRUE) / sum(estimate, na.rm = TRUE),
    .groups = "drop"
  )

# visualization options
# simplest: mean commute time color scale by tract
# more complex: bivariate choropleth with commute time buckets and urban area
#   - don't use bubbles for mean commute time since it's hard to compare areas
plt <- ggplot(mean_commute) +
  geom_sf(aes(fill = mean_minutes), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Mean Commute 2019",
    fill = "Mean Commute Time (minutes)"
  )
print(plt)
ggsave("commute_2019.png", path = "final", plot = plt, width = 10, height = 6)

