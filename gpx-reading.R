# test reading Motion X GPX data

library(tidyverse)
library(fs)
library(lubridate)
library(here)
library(hms)
library(glue)
library(skimr)
library(janitor)
library(sf)
library(leaflet)
library(plotly)
library(deckgl)
# library(ggmap)
# library(xml2)
# library(rgdal)
# library(rvest)

# define path -------------------------------------------------------------

test_dir <- "D:/Downloads/gpx"
filename <- "Track_367.gpx"
test_file <- path(test_dir, filename)

# get layers --------------------------------------------------------------

gpx_layers <- st_layers(test_file)
gpx_layers %>% str()

# we want layer_name = "tracks", thought "track_points" could also be interesting

# read tracks layer -------------------------------------------------------

gpx_tracks_layer <- st_read(test_file, layer = "tracks", stringsAsFactors = FALSE)
gpx_tracks_layer %>% str()
gpx_tracks_layer %>% glimpse()

# much more sparse description info than kml: the "desc" columns just contains the timestamp,
# with no summaries such as distance, avg speed, max altitude etc
# geometry has no time stamps or elevations, but these are in track_points below

gpx_tracks_timestamps <- gpx_tracks_layer$desc

# read track_points layer -------------------------------------------------------

gpx_track_points_layer <- st_read(test_file, layer = "track_points", stringsAsFactors = FALSE)
gpx_track_points_layer %>% str()
gpx_track_points_layer %>% glimpse()

gpx_track_points_layer %>% 
  as_tibble() %>% 
  count(track_fid, track_seg_id)

# useful cols: track_seg_id, track_seg_point_id, ele, time
# this means that unlike kml, both elevation and time are recorded for each point

# test vis ----------------------------------------------------------------

gpx_track_points_layer %>% 
  leaflet() %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "TopoMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addCircleMarkers(group = "track points",
                     label = ~glue("{time}: {ele}"),
                     # labelOptions = labelOptions(noHide = T, textsize = "8px"),
                     radius = 3, stroke = FALSE, color = "blue", fillColor = "blue", opacity = 0.5, fill = TRUE,
                     popup = ~glue("{time}: {ele}")) %>% 
    addLayersControl(baseGroups = c("Terrain", "TopoMap", "Aerial", "OSM", "Toner Lite"),
                     overlayGroups = c("track points"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")
    
gpx_track_points_layer %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = time, y = ele, colour = factor(track_seg_id)), size = 0.5, alpha = 0.8) +
  geom_point(aes(x = time, y = ele, colour = factor(track_seg_id)), size = 0.5, alpha = 0.8) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# this shows elevation vs time, but elevation vs cumulative distance would be more useful


# gpx reading functions ---------------------------------------------------

st_empty <- function(type = "POINT", crs = 4326) {
  sf::st_as_sfc(paste0(toupper(type), "(EMPTY)"), crs = crs)
}

read_gpx <- function(file_to_read, as_list = TRUE) {
  tracks_layer <- st_read(file_to_read, layer = "tracks", stringsAsFactors = FALSE)
  tracks_timestamps <- tracks_layer$desc
  track_points_layer <- st_read(file_to_read, layer = "track_points", stringsAsFactors = FALSE) %>% 
    select(track_fid, track_seg_id, track_seg_point_id, ele, time, geometry)
  if (st_crs(tracks_layer)$input == "WGS 84") {
    tracks_layer = st_set_crs(tracks_layer, 4326)
  } # essentially the same, but this ensures compatiblity with other objects
  if (st_crs(track_points_layer)$input == "WGS 84") {
    track_points_layer = st_set_crs(track_points_layer, 4326)
  } # essentially the same, but this ensures compatiblity with other objects
  list(
    tracks = tracks_layer,
    timestamps = tracks_layer$desc,
    track_points = track_points_layer %>% 
      mutate(distance = st_distance(geometry, lag(geometry, default = st_empty()), by_element = TRUE)) %>% 
      tidyr::replace_na(list(distance = units::as_units(0, "m"))) %>% 
      mutate(cume_distance = cumsum(distance))
  )
}

gpx_obj <- read_gpx(test_file)
str(gpx_obj)

read_gpx_track_points <- function(file_to_read, file_label = NULL) {
  if (is.null(file_label)) {file_label <- file_to_read}
  read_gpx(file_to_read)$track_points %>% 
    add_column(file_label = file_label, .before = 1)
}

multitrack_sf <- dir_ls(test_dir, glob = "*.gpx") %>%
  tail(10) %>% # for testing
  map_dfr(read_gpx_track_points) %>% 
  mutate(name = path_ext_remove(path_file(file_label)), .befor = 1L)

# visualise walks over time & distance ---------------------------------------------------------

elev_by_time_plot <- multitrack_sf %>% 
  as_tibble() %>% 
  mutate(walk = factor(name)) %>% 
  group_by(walk) %>% 
  mutate(elapsed_time = as.numeric(difftime(time, min(time), units = "hours"))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = elapsed_time, y = ele, colour = walk), size = 0.5, alpha = 0.8) +
  geom_point(aes(x = elapsed_time, y = ele, colour = walk), size = 0.5, alpha = 0.8) +
  labs(y = "elevation (m amsl)", x = "hours", title = "Elevation over time") +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank())

elev_by_dist_plot <- multitrack_sf %>% 
  as_tibble() %>% 
  mutate(walk = factor(name), km = as.numeric(cume_distance) / 1000) %>% 
  ggplot() +
  geom_line(aes(x = km, y = ele, colour = walk), size = 0.5, alpha = 0.8) +
  geom_point(aes(x = km, y = ele, colour = walk), size = 0.5, alpha = 0.8) +
  labs(y = "elevation (m amsl)", x = "distance walked (km)", title = "Elevation by distance") +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank())

ggplotly(elev_by_dist_plot)

# use deck.gl for 3d vis --------------------------------------------------

vertical_exaggeration <- 1

# add elevation to points (there has to be a nicer way of doing this!)
path_3d_sf <- multitrack_sf %>% 
  transmute(name, z = ele * vertical_exaggeration, geometry) %>% 
  mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>% 
  as_tibble() %>%
  group_by(name) %>% 
  st_as_sf(coords = c("x", "y", "z"), crs = 4326) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

paths_centre <- path_3d_sf %>%
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_centroid() %>% 
  st_coordinates()

view_widget <- function(widget, widget_path = fs::path(tempdir(), "widget.html")) {
  htmlwidgets::saveWidget(widget, file = widget_path)
  browseURL(widget_path)
}

deckgl(zoom = 9.5, pitch = 45,
       longitude = paths_centre[1], latitude = paths_centre[2],
       width = "100%", height = 900) %>%
  add_path_layer(
    data = path_3d_sf,
    getPath = JS("d => d.geometry.coordinates"),
    getColor = "firebrick",
    widthScale = 1,
    widthMinPixels = 2,
    getTooltip = ~name,
    getWidth = 2
  ) %>%
  add_basemap(style = use_carto_style("positron")) %>% 
  add_control(
    str_glue("<h2>Last {nrow(path_3d_sf)} walks</h2>"),
    pos = "top-right",
    style = "background: steelblue; color: white;"
  ) %>%
  view_widget()
