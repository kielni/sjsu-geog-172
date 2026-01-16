library(censusapi)
library(cowplot)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(sf)
library(tidyr)
library(tidycensus)
library(tigris)

options(tigris_use_cache = TRUE)

# Commute times in the Bay Area, 2000 vs 2019 ----------------------------
# Data sources
#   - ACS: B08303	Travel Time To Work
#   - 2000 Decennial Census:P031 Travel time to work for workers 16 years & over

# Functions ----------------------------

prep_data <- function(commute) {
  # prepare commute data with common steps for all years
  # coerce to numeric, filter NA, transform to Web Mercator
  commute |>
    mutate(
      mean_travel_time = as.numeric(.data$mean_travel_time)
    ) |>
    filter(!is.na(.data$mean_travel_time)) |>
    st_transform(3857) # Web Mercator for mapping
}

plot_data <- function(commute_data, common_scale, year,
                      draw_north = FALSE, draw_scale = FALSE) {
  # create ggplot object for commute data
  print(paste("plotting year", year))
  plt <- ggplot() +
    # annotation_map_tile(type = "cartolight", zoom = 10) +
    geom_sf(
      data = commute_data,
      aes(fill = .data$mean_travel_time),
      color = NA
    ) +
    scale_fill_viridis_c(limits = common_scale) +
    labs(fill = "Mean Commute Time\n(minutes)") +
    theme(
      # remove left and right margins
      # from cowplot shared legend
      plot.margin = margin(6, 0, 6, 0),
    )
  # draw year label in top left corner
  plt <- plt +
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = year,
      hjust = 0,
      vjust = 1,
      size = 4,
      fontface = "bold",
      color = "black"
    )
  if (draw_north) {
    plt <- plt +
      annotation_north_arrow(
        location = "bl",
        which_north = "true",
        height = unit(0.6, "cm"),
        width = unit(0.6, "cm")
      )
  }
  if (draw_scale) {
    plt <- plt +
      annotation_scale(
        location = "br",
        height = unit(0.2, "cm"),
      )
  }
  plt <- plt + theme_void()

  # write to file for debugging
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

# Prep 2019 data ----------------------------
# from ACS subject table
# S0801_C01_046 =
#   Estimate!!Total!!
#   Workers 16 years and over who did not work from home!!
#   TRAVEL TIME TO WORK!!
#   Mean travel time to work (minutes)
load_acs <- function(counties, year) {
  commute_latest <- get_acs(
    geography = "tract",
    state = "CA",
    county = bay_counties,
    survey = "acs5",
    variables = "S0801_C01_046",
    year = year,
    geometry = TRUE,
    cache_table = TRUE,
  ) |>
    rename(mean_travel_time = estimate)

  prep_data(commute_latest)
}

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
load_decennial <- function(counties, year) {
  commute_prev <- get_decennial(
    geography = "tract",
    state = "CA",
    county = bay_counties,
    variables = c(
      total_time = "P033001",
      total_workers = "P031002"
    ),
    sumfile = "sf3",
    year = year,
    geometry = TRUE,
    cache_table = TRUE,
    output = "wide"
  ) |>
    # calculate mean travel time
    mutate(
      mean_travel_time = total_time / total_workers
    )

  prep_data(commute_prev)
}

# Create single map ----------------------------
combine_plots <- function(plot_prev, plot_latest) {
  # combine two plots side by side
  plots <- plot_grid(
    plot_prev + theme(legend.position = "none"),
    plot_latest + theme(legend.position = "none"),
    align = "vh",
    nrow = 1
  )

  # single shared legend
  legend <- get_legend(
    plot_prev + theme(legend.box.margin = margin(0, 0, 0, 12))
  )

  # attribution in bottom right
  attribution <- ggdraw() +
    draw_label(
      "Basemap: © OpenStreetMap | Data: 2000 Census, 2019 ACS",
      size = 8,
      x = 0.5,
      hjust = 0.5
    )

  # overall title
  title <- ggdraw() +
    draw_label(
      "Mean Commute Times in the San Francisco Bay Area: 2000 vs 2019",
      fontface = "bold",
      x = 0.5,
      hjust = 0.5
    ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      #plot.margin = margin(0, 0, 0, 12)
    )

  # arrange elements
  # add legend to the right of the plots
  plots_with_legend <- plot_grid(plots, legend, rel_widths = c(2, .4))
  final_map <- plot_grid(
    title, plots_with_legend, attribution,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  # TODO:
  # add urban area boundary and crop
  final_map <- ggdraw() +
    draw_plot(final_map) +
    theme(plot.background = element_rect(fill = "white", color = NA))

  ggsave(
    "commute_comparison.png",
    path = "final",
    plot = final_map,
    width = 12,
    height = 6
  )
}

# Setup ----------------------------
bay_counties <- c(
  "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
  "San Mateo", "Santa Clara", "Solano", "Sonoma"
)
year_latest <- 2019
year_prev <- 2000

# mean_commute_latest <- load_acs(counties, year_latest)
# mean_commute_prev <- load_decennial(counties, year_prev)

# common scale so colors will have the same meaning on both plots
common_scale <- range(
  c(
    mean_commute_prev$mean_travel_time,
    mean_commute_latest$mean_travel_time
  ),
  na.rm = TRUE
)

# create plots with draw single north and scale
plot_prev <- plot_data(
  mean_commute_prev,
  common_scale,
  year_prev,
  draw_north = TRUE
)
plot_latest <- plot_data(
  mean_commute_latest,
  common_scale,
  year_latest,
  draw_scale = TRUE
)

combine_plots(plot_prev, plot_latest)

# TODO:
# draw urban area boundary
# crop to urban area boundary
