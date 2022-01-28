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

read_gpx <- function(file_to_read, as_list = TRUE) {
  tracks_layer <- st_read(file_to_read, layer = "tracks", stringsAsFactors = FALSE)
  tracks_timestamps <- tracks_layer$desc
  track_points_layer <- st_read(file_to_read, layer = "track_points", stringsAsFactors = FALSE) %>% 
    select(track_fid, track_seg_id, track_seg_point_id, ele, time, geometry)
  list(
    tracks = tracks_layer,
    timestamps = tracks_layer$desc,
    track_points = track_points_layer
    )
}

gpx_obj <- read_gpx(test_file)
str(gpx_obj)

# next step: write function that wraps this to just get the track points sf, with file name column

