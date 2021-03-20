# Functions to style OSM streetmaps for basemap use

# inspiration from:
# http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# https://www.dshkol.com/2018/better-maps-with-vector-tiles/
# http://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/
# https://taraskaduk.com/posts/2021-01-18-print-street-maps/

library(tidyverse)
library(sf)
library(glue)
library(fs)

local_osm <- opq(bbox = 'whangarei nz') %>%
  add_osm_feature(key = 'highway', value = 'trunk') %>%
  osmdata_sf()

local_osm$osm_lines %>% class() # sf and data.frame, but not tbl!
local_osm$osm_lines %>% glimpse()

local_osm$osm_lines %>%
  mutate(maxspeed = as.integer(maxspeed), lanes = as.integer(lanes)) %>% 
  ggplot() +
  geom_sf(aes(colour = maxspeed), size = 2) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
