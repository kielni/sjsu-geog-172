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

# TODO: add path, create if not exists
# TODO: update title to include metro
# TODO: color transparency

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

  # 1st percentile is 2% overlap, 5th is 38%
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
  #' Clip to metro area.
  #' Create commute time categories.
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

  commute <- clip_to_metro(commute, metro)

  # create commute time categories
  # from values in ACS variable B08303:
  # (0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 60, 90, Inf)
  # from data
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  # 10.00   26.78   30.11   30.09   33.45   57.90
  breaks <- c(0, 25, 30, 35, 45, 60)
  labels <- c(
    "Less than 25 minutes",
    "25 to 29 minutes",
    "30 to 34 minutes",
    "35 to 44 minutes",
    "45 minutes or more"
  )
  commute <- commute |>
    mutate(
      commute_category = cut(
        mean_travel_time,
        breaks = breaks,
        labels = labels,
        right = FALSE
      )
    )
}

get_limits <- function(sf_object) {
  #' Get bounding box of sf object as named vector
  #' @param sf_object sf data frame
  #' @return named vector with xlim and ylim
  bbox <- st_bbox(sf_object)
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  bbox["xmin"] <- bbox["xmin"] - 0.05 * x_range
  bbox["xmax"] <- bbox["xmax"] + 0.05 * x_range
  bbox["ymin"] <- bbox["ymin"] - 0.05 * y_range
  bbox["ymax"] <- bbox["ymax"] + 0.05 * y_range
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])
  list(xlim = xlim, ylim = ylim)
}

plot_data <- function(commute_data, common_scale, metro, year,
                      draw_north = FALSE, draw_scale = FALSE) {
  #' Create a map for commute data
  #' @param commute_data sf data frame with mean_travel_time column
  #' @param common_scale numeric vector with min, max values for color scale
  #' @param metro sf data frame with metro boundary, in Web Mercator
  #' @param year integer year for labeling
  #' @param draw_north logical whether to draw north arrow
  #' @param draw_scale logical whether to draw scale bar
  #' @return ggplot object

  print(paste("plotting year", year))
  plt <- ggplot() +
    # base map
    annotation_map_tile(type = "cartolight", zoom = 11) +
    # commute data
    geom_sf(
      data = commute_data,
      aes(fill = commute_category)
    ) +
    scale_fill_brewer(
      # palette = "YlGnBu",
      palette = "RdYlGn",
      direction = -1,
      na.value = "transparent",
    ) +
    labs(fill = "Mean Commute Time\n(minutes)")
  # draw year label in top right corner
  plt <- plt +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = year,
      hjust = 1.2,
      vjust = 1.2,
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
    plt <- plt + annotation_scale(location = "br", height = unit(0.2, "cm"))
  }

  # draw metro boundary
  plt <- plt +
    geom_sf(
      data = metro,
      aes(color = "2019 Urban\nArea Boundary"),
      fill = NA,
      size = 0.5
    ) +
    scale_color_manual(
      values = c("2019 Urban\nArea Boundary" = "black"),
      name = NULL
    ) +
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2)
    )

  # set limits to metro area with some padding
  limits <- get_limits(metro)
  plt <- plt + coord_sf(xlim = limits$xlim, ylim = limits$ylim) +
    theme_void() +
    theme(
      legend.margin = margin(b = 10),
      legend.spacing.y = unit(15, "pt")
    )

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
  #' @param counties vector of county names
  #' @param metro sf data frame with metro boundary, in Web Mercator
  #' @param year integer year for ACS data
  #' @return sf data frame with mean_travel_time column
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
  #' @param counties vector of county names
  #' @param metro sf data frame with metro boundary, in Web Mercator
  #' @param year integer year for decennial data
  #' @return sf data frame with mean_travel_time column
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
    mutate(mean_travel_time = total_time / total_workers)

  prep_data(commute_prev, metro)
}

prep_metro <- function() {
  #' Load 2019 urban area boundaries for San Francisco and San Jose.
  #' This data is not currently available for years prior to 2011.
  #' @return sf data frame with metro boundary, in Web Mercator
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
  #' Create single map from two plots, with shared legend, title, attribution
  #' @param plot_prev ggplot object for previous year
  #' @param plot_latest ggplot object for latest year
  #' @return ggplot object

  # add side by side plots
  plots <- plot_grid(
    plot_prev +
      theme(legend.position = "none", plot.margin = margin(0, 6, 0, 0)),
    plot_latest +
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 6)),
    align = "vh",
    nrow = 1
  )

  # single shared legend
  legend <- get_legend(
    plot_prev + theme(legend.box.margin = margin(0, 0, 0, 1))
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
      paste(
        "Mean Commute Times in the San Francisco - San Jose Urban Area\n",
        "2000 vs 2019"
      ),
      fontface = "bold",
      x = 0.5,
      hjust = 0.5
    ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
    )

  # arrange elements
  # add legend to the right of the plots
  plots_with_legend <- plot_grid(plots, legend, rel_widths = c(0.85, 0.15))
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
    height = 9
  )
  final_map
}

# Script  ----------------------------
bay_counties <- c(
  "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
  "San Mateo", "Santa Clara", "Solano", "Sonoma"
)
year_latest <- 2019
year_prev <- 2000

# metro <- prep_metro()
mean_commute_latest <- load_acs(counties, metro, year_latest)
mean_commute_prev <- load_decennial(counties, metro, year_prev)

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
