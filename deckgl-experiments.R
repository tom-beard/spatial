# deckgl experiments

# quickstart --------------------------------------------------------------

# based on https://crazycapivara.github.io/deckgl/articles/deckgl.html

library(tidyverse)
library(deckgl)
library(sf)

deckgl() # starts a viewer pane

deckgl() %>% 
  add_basemap() # no basemap visible in viewer pane: need to click "Show in new window" to open in browser
# this might be an RStudio issue, solved in 1.3.1054: https://github.com/rstudio/rstudio/issues/7152

# Grid layer example
data("sf_bike_parking")

props <- list(
  extruded = TRUE,
  cellSize = 200,
  elevationScale = 4,
  getPosition = ~lng + lat,
  tooltip = "Count: {{count}}"
)

deckgl(zoom = 11, pitch = 45) %>%
  add_basemap() %>%
  add_grid_layer(
    data = sf_bike_parking,
    properties = props
  )

# custom deckgl() call
deckgl(zoom = 11, pitch = 45, latitude = -41.3, longitude = 174.77) %>% # Wellington
  add_basemap()


# layer example -----------------------------------------------------------

# from https://crazycapivara.github.io/deckgl/reference/add_arc_layer.html

data("bart_segments")

properties <- list(
  getWidth = 12,
  getSourcePosition = ~from_lng + from_lat,
  getTargetPosition = ~to_lng + to_lat,
  getSourceColor = JS("d => [Math.sqrt(d.inbound), 140, 0]"),
  getTargetColor = JS("d => [Math.sqrt(d.outbound), 140, 0]"),
  tooltip = use_tooltip(
    html = "{{from_name}} to {{to_name}}",
    style = "background: steelBlue; border-radius: 5px;"
  )
)

deck <- deckgl(zoom = 10, pitch = 35) %>%
  add_arc_layer(data = bart_segments, properties = properties) %>%
  add_control("Arc Layer", "top-left") %>%
  add_basemap()
deck


# basemap choice ----------------------------------------------------------

deckgl(zoom = 11, pitch = 45, latitude = -41.3, longitude = 174.77) %>% # Wellington
  add_basemap(style = use_carto_style("positron"))


# try a path layer --------------------------------------------------------


# example from https://github.com/crazycapivara/deckgl/blob/master/_examples/sf/sf_path-layer.R

features <- st_as_sf(bart_stations, coords = c("lng", "lat"), crs = 4326)[6:9, ]
path <- st_geometry(features) %>%
  st_coordinates() %>%
  st_linestring() %>%
  st_sfc(crs = 4326) %>%
  st_sf()

deckgl(zoom = 9.5, pitch = 35) %>%
  add_scatterplot_layer(
    data = features,
    getPosition = ~geometry,
    radiusScale = 6,
    getRadius = 100,
    getFillColor = c(240, 140, 20),
    getTooltip = ~name
  ) %>%
  add_path_layer(
    data = path,
    getPath = ~geometry,
    widthScale = 20,
    widthMinPixels = 2,
    getWidth = 5
  ) %>%
  add_basemap() # rather than add_mapbox_basemap()

# this won't render, due to a bug in the deckgl package
# for a workaround: https://github.com/crazycapivara/deckgl/issues/178

deckgl(zoom = 9.5, pitch = 35) %>%
  add_scatterplot_layer(
    data = features,
    getPosition = JS("d => d.geometry.coordinates"),
    radiusScale = 6,
    getRadius = 100,
    getFillColor = c(240, 140, 20),
    getTooltip = ~name
  ) %>%
  add_path_layer(
    data = path,
    getPath = JS("d => d.geometry.coordinates"),
    getColor = "red",
    widthScale = 20,
    widthMinPixels = 2,
    getWidth = 5
  ) %>%
  add_basemap()

# test with 3D paths

test_bart_df <- tribble(
  ~x, ~y, ~z,
  -122.3535851, 37.936051, 100,
  -122.3179784, 37.9249513, 7000,
  -122.300284, 37.902646, 1000,
  -122.2843653, 37.8735039, 5000
)

test_bart_path <- test_bart_df %>% 
  add_column(name = "howdy", .before = 1) %>% 
  group_by(name) %>% 
  st_as_sf(coords = c("x", "y", "z"), crs = 4326) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

deckgl(zoom = 9.5, pitch = 35) %>%
  add_path_layer(
    data = test_bart_path,
    getPath = JS("d => d.geometry.coordinates"),
    getColor = "red",
    widthScale = 20,
    widthMinPixels = 2,
    getTooltip = ~name,
    getWidth = 5
  ) %>%
  add_basemap()

# it works!
# not perfect visually, since it's hard to see elevation, but it's a start
