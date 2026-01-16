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

# TODO: update comment style, add params
# TODO: add path, create if not exists
# TODO: update title to include metro
# TODO: add line to legend
# TODO: scale
# TODO: color transparency
# TODO: legend label

# Commute times in the Bay Area, 2000 vs 2019 ----------------------------
# Data sources
#   - ACS: B08303	Travel Time To Work
#   - 2000 Decennial Census:P031 Travel time to work for workers 16 years & over
#   - 2019 urban area boundaries from tigris::urban_areas()

# Functions ----------------------------
clip_to_metro <- function(commute, metro) {
  #' Clip the rows in the commute data to overlap with the metro area.
  #' @param commute sf dataframe with census tract rows, in web mercator
  #' @param metro sf dataframe with metropolitan area polygons, in web mercator
  #' @return sf dataframe with census that overlap the metro by at least 2.5%
  metro_union <- st_union(metro)
  plt <- ggplot() +
    geom_sf(data = commute, fill = "lightblue") +
    geom_sf(data = metro_union, fill = NA, color = "black")
  ggsave("metro_initial.png", plot = plt, path = "final")

  # get all tracts that intersect with metro area and
  # calculate percentage area of census tract that overlaps with metro
  commute <- commute |>
    st_join(metro, join = st_intersects, left = FALSE) |>
    # add tract area
    mutate(tract_area = st_area(geometry)) |>
    # add tract x metro overlap
    mutate(overlap_area = st_area(st_intersection(geometry, metro_union))) |>
    mutate(overlap_pct = overlap_area / tract_area)

  ggplot() +
    geom_sf(data = commute, aes(fill = as.numeric(overlap_pct)), color = NA) +
    scale_fill_viridis_c()
  ggsave("metro_overlap_percent.png", path = "final")

  # 1% is 2% overlap, 5% is 38%
  # keep tracts that overlap at least 2.5% of their area
  min_overlap <- 0.025
  commute <- commute |>
    filter(as.numeric(overlap_pct) > min_overlap)
  ggplot() +
    geom_sf(data = commute, aes(fill = as.numeric(overlap_pct)), color = NA) +
    scale_fill_viridis_c()
  ggsave("metro_overlap_final.png", path = "final")

  commute
}

prep_data <- function(commute, metro) {
  #' Prepare commute data with common steps for all years.
  #' Coerce to numeric, filter NA, transform to Web Mercator.
  #' @param commute sf data frame with mean_travel_time column
  #' @param metro sf data frame with metro boundary, in Web Mercator
  #' @return cleaned sf data frame, in Web Mercator
  commute <- commute |>
    mutate(
      mean_travel_time = as.numeric(.data$mean_travel_time)
    ) |>
    filter(!is.na(.data$mean_travel_time)) |>
    # convert to web mercator for mapping
    st_transform(3857)

  clip_to_metro(commute, metro)
}

plot_data <- function(commute_data, common_scale, metro, year,
                      draw_north = FALSE, draw_scale = FALSE) {
  # create ggplot object for commute data
  print(paste("plotting year", year))
  plt <- ggplot() +
    annotation_map_tile(type = "cartolight") +
    geom_sf(
      data = commute_data,
      aes(fill = .data$mean_travel_time),
      color = NA
    ) +
    # scale_fill_viridis_c(limits = common_scale) +
    scale_fill_distiller(
      palette = "YlGnBu",
      direction = 1,
      limits = common_scale
    )
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
  # draw metro boundary
  plt <- plt +
    geom_sf(
      data = metro,
      fill = NA,
      color = "black",
      size = 0.5
    )
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

# Load data ----------------------------------

load_acs <- function(counties, metro, year) {
  #' Load 2019 data from ACS subject table
  #' S0801_C01_046 =
  #'   Estimate!!Total!!
  #'   Workers 16 years and over who did not work from home!!
  #'   TRAVEL TIME TO WORK!!
  # '  Mean travel time to work (minutes)
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

  prep_data(commute_latest, metro)
}

load_decennial <- function(counties, metro, year) {
  #' Load 2000 datafrom SF3 transportation profile
  #' P033001 =
  #'   Aggregate travel time to work (in minutes)
  #'   by travel time to work
  #'   by means of transportation to work
  #'   for workers 16 years and over
  #'   who did not work at home
  #' P031002 =
  #'   Total!!Did not work at home
  #'   TRAVEL TIME TO WORK FOR WORKERS 16 YEARS AND OVER
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

  prep_data(commute_prev, metro)
}

prep_metro <- function() {
  #' Load 2019 core based statistical area boundaries for
  #' San Francisco - San Jose corridor.
  #' This data is not currently available for years prior to 2011.
  ua <- urban_areas(year = 2019, cb = TRUE) |>
    filter(grepl(", CA", NAME10))

  # San Francisco--Oakland, CA
  sf_ua <- ua |>
    filter(NAME10 == "San Francisco--Oakland, CA")
  # San Jose, CA
  sj_ua <- ua |>
    filter(NAME10 == "San Jose, CA")
  rbind(sj_ua, sf_ua) |>
    st_transform(3857)
}

combine_plots <- function(plot_prev, plot_latest) {
  #' Create single map from two plots.
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

# metro <- prep_metro()

# mean_commute_latest <- load_acs(counties, metro, year_latest)
# mean_commute_prev <- load_decennial(counties, metro, year_prev)

# common scale so colors will have the same meaning on both plots
common_scale <- range(
  c(
    mean_commute_prev$mean_travel_time,
    mean_commute_latest$mean_travel_time
  ),
  na.rm = TRUE
)

# create plots; draw single north arrow and scale
plot_prev <- plot_data(
  mean_commute_prev,
  common_scale,
  metro,
  year_prev,
  draw_north = TRUE
)
plot_latest <- plot_data(
  mean_commute_latest,
  common_scale,
  metro,
  year_latest,
  draw_scale = TRUE
)

combine_plots(plot_prev, plot_latest)
