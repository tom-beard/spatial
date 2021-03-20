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

# manual bbox instead
urban_bbox <- st_bbox(c(xmin = 174.25, xmax = 174.4, ymax = -35.65, ymin = -35.8), crs = st_crs(4326))

local_osm$osm_lines %>% class() # sf and data.frame, but not tbl!
local_osm$osm_lines %>% glimpse()
all_lines <- local_osm$osm_lines %>% st_crop(urban_bbox)
all_polygons <- local_osm$osm_polygons %>% st_crop(urban_bbox)
all_multipolygons <- local_osm$osm_multipolygons
# all_multipolygons <- local_osm$osm_multipolygons %>% st_crop(urban_bbox)
# Error in CPL_geos_op2(op, x, y) : 
#Evaluation error: TopologyException: Input geom 0 is invalid: Self-intersection at or near point 
# 174.27898149999999 -35.744872299999997 at 174.27898149999999 -35.744872299999997.

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

all_lines %>%
  as.data.frame() %>%
  count(highway, sort = TRUE) %>% 
  left_join(highway_sizes, by = "highway")

# polygons

all_residential <- local_osm$osm_multipolygons %>% 
  filter(landuse == "residential") %>% 
  select(name) %>% bind_rows(
    local_osm$osm_polygons %>% 
                               filter(landuse == "residential") %>% 
                               select(name)
  )

all_polygons %>% as_tibble() %>% select(landuse, man_made, natural, water, waterway) %>% skimr::skim()
all_polygons %>% as_tibble() %>% count(landuse)
all_polygons %>% as_tibble() %>% count(man_made)
all_polygons %>% as_tibble() %>% count(natural)
all_polygons %>% as_tibble() %>% count(water)
all_polygons %>% as_tibble() %>% count(waterway)

all_multipolygons %>% as_tibble() %>% count(landuse)
all_multipolygons %>% as_tibble() %>% count(man_made)
all_multipolygons %>% as_tibble() %>% count(natural)
all_multipolygons %>% as_tibble() %>% count(water)
all_multipolygons %>% as_tibble() %>% count(waterway)

all_water <- all_polygons %>% select(name, natural, water, waterway) %>% 
  bind_rows(all_multipolygons %>% select(name, natural, water, waterway)) %>% 
  filter(natural %in% c("water", "coastline", "bay") |
           water %in% c("lake", "reservoir", "river") |
           waterway %in% c("dam", "river", "riverbank")) %>% 
  st_crop(urban_bbox) %>% 
  st_union()

# prepare layers ----------------------------------------------------------

filter_highways <- function(input_sf, highway_groups = c("small")) {
  input_sf %>%
    filter(highway %in%
             (highway_sizes %>%
                filter(highway_group %in% highway_groups) %>%
                pull(highway))
    ) %>%
    select(name, highway)
}

small_roads <- all_lines %>%
  filter_highways("small") %>% 
  mutate(road_length = st_length(.) %>% units::set_units(km) %>% as.numeric())
# useful tips: st_ calculations can take "." as an argument in a pipe; units:set_units is your friend!

# tried bbox based on extent of smaller roads, and it didn't work
# but code can make a useful function
st_bbox_with_buffer <- function(input_sf, buffer_m = 10) {
  st_bbox(input_sf) %>%
    st_as_sfc() %>% 
    st_as_sf() %>% 
    st_transform(crs = 2193) %>% 
    st_buffer(dist = buffer_m) %>%
    st_transform(crs = 4326) %>% 
    st_bbox()
}

ggplot() +
  geom_sf(data = all_water, fill = "steelblue", colour = NA, alpha = 0.5) +
  geom_sf(data = filter_highways(all_lines, "small"), colour = "grey70", size = .1) +
  geom_sf(data = filter_highways(all_lines, "medium"), colour = "grey50", size = .3) +
  geom_sf(data = filter_highways(all_lines, "large"), colour = "grey40", size = .5) +
  labs(x = "", y = "", title = "") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey90", colour = "grey90")
  )
