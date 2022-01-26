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

# much more sparse info than kml: the "desc" columns just contains the timestamp
# geometry has no time stamps or elevations

gpx_tracks_timestamps <- gpx_tracks_layer$desc
