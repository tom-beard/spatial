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
library(osmdata) # for local use, read from local files instead
library(janitor)


# get and examine OSM data ------------------------------------------------------------

local_osm <- opq(bbox = 'whangarei nz') %>%
  osmdata_sf()
# takes ~1 min on laptop. resulting object ~1.2GB, but we want to get water objects etc as well as streets

local_osm$osm_lines %>% class() # sf and data.frame, but not tbl!
local_osm$osm_lines %>% glimpse()

highway_sizes <- tibble::tribble(
  ~highway, ~highway_group, ~size,
  "motorway",        "large",   0.5,
  "motorway_link",        "large",   0.3,
  "trunk",        "large",   0.5,
  "trunk_link",        "large",   0.3,
  "primary",        "large",   0.5,
  "primary_link",        "large",   0.3,
  "secondary",       "medium",   0.3,
  "secondary_link",       "medium",   0.3,
  "tertiary",       "medium",   0.3,
  "tertiary_link",       "medium",   0.3,
  "residential",        "small",   0.2,
  "living_street",        "small",   0.2,
  "unclassified",        "small",   0.2,
  "service",        "small",   0.2,
  "steps",        "small",   0.2,
  "pedestrian",        "small",   0.2,
  "footway",        "small",   0.2
)

local_osm$osm_lines %>%
  as.data.frame() %>%
  count(highway, sort = TRUE) %>% 
  left_join(highway_sizes, by = "highway")

# prepare layers ----------------------------------------------------------

small_roads <- local_osm$osm_lines %>%
  filter(highway %in%
           (highway_sizes %>%
              filter(highway_group %in% c("small")) %>%
              pull(highway))
  ) %>%
  select(name, highway) %>% 
  mutate(road_length = st_length(.) %>% units::set_units(km) %>% as.numeric())
# useful tips: st_ calculations can take "." as an argument in a pipe; units:set_units is your friend!

urban_bbox <- st_bbox(small_roads) %>%
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_transform(crs = 2193) %>% 
  st_buffer(dist = 500) %>%
  st_transform(crs = 4326) %>% 
  st_bbox()

small_roads %>% 
  ggplot() +
  geom_sf(colour = "grey80", size = .5) +
  geom_sf(data = local_osm$osm_lines %>% filter(highway == "trunk") %>% st_crop(urban_bbox),
          colour = "firebrick") +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
