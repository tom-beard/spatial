# simple active modes accessibility

library(tidyverse)
library(sf)
library(glue)
library(fs)
library(osmdata) # for local use, read from local files instead
library(janitor)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(igraph)
library(leaflet)

# base paths --------------------------------------------------------------

gis_path <- path("D:", "GIS")
census_path <- path(gis_path, "census", "Census 2018")

path_to_geo_areas <- path(gis_path, "geo-areas", "geographic-areas-file-2019.csv")
path_to_sa_geom <- path(census_path, "sa1", "statistical-area-1-2018-generalised.csv")
path_to_sa_data <- path(census_path, "Individual_part1_totalNZ_updated_16-7-20",
                        "Census_usually_resident_pop_count-long_format_updated_16-7-20.csv")

# get sa1 data ------------------------------------------------------------

geo_area_df <- read_csv(path_to_geo_areas, cols(
  .default = col_character(),
  SA12019_code = col_double(),
  SA12018_code = col_double(),
  SA22019_code = col_double(),
  SA22018_code = col_double(),
  UR2019_code = col_double(),
  IUR2019_code = col_double(),
  UR2018_code = col_double(),
  IUR2018_code = col_double(),
  MED2014_code = col_double(),
  AU2017_code = col_double(),
  AU2013_code = col_double()
))

sa_df <- read_csv(path_to_sa_data, col_types = cols(
  Area_code_and_description = col_skip(),
  Area_code = col_character(),
  Area_description = col_skip(),
  Year = col_integer(),
  Usual_resident_count = col_integer()
)) %>% 
  clean_names() %>%
  rename(sa1_id = area_code) %>% 
  filter(startsWith(sa1_id, "7"))

sa_geom_sf <- st_read(path_to_sa_geom, quiet = TRUE) %>% 
  rename(sa1_id = SA12018_V1_00) %>% 
  clean_names()

ta_name <- "Wellington City"
local_name <-  "Oriental Bay"

area_census_df <- geo_area_df %>% 
  mutate(sa1_id = as.character(SA12018_code)) %>% 
  select(sa1_id, SA22018_name, UR2018_name, TA2019_name) %>% 
  filter(TA2019_name == ta_name) %>% 
  distinct() %>% 
  left_join(sa_df, by = "sa1_id") %>% 
  filter(year == 2018)


# test vis ----------------------------------------------------------------

sa_geom_sf %>% 
  right_join(area_census_df %>% filter(SA22018_name == local_name),
             by = "sa1_id") %>% 
  st_transform(crs = 4326) %>% 
  leaflet() %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "TopoMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addPolygons(group = "sa1",
                     label = ~glue("{sa1_id}"),
                     labelOptions = labelOptions(noHide = T, textsize = "8px"),
                     stroke = TRUE, color = "blue", opacity = 1, weight = 2, fill = TRUE,
                     popup = ~glue("{sa1_id}")) %>% 
    addLayersControl(baseGroups = c("Terrain", "TopoMap", "Aerial", "OSM", "Toner Lite"),
                     overlayGroups = c("sa1"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")

# get and process OSM data ------------------------------------------------------------

local_bbox <- getbb(paste("Oriental Bay", "NZ"))
local_bbox_st <- c(xmin = local_bbox["x", "min"], xmax = local_bbox["x", "max"],
                      ymin = local_bbox["y", "min"], ymax = local_bbox["y", "max"]) %>% 
  st_bbox(crs = 4326) %>% 
  st_as_sfc() %>% 
  st_buffer(dist = 2000, nQuadSegs = 1) %>% 
  st_bbox()
local_osm <- opq(bbox = local_bbox) %>%
  osmdata_sf()

local_lines <- local_osm$osm_lines
local_highways <- local_lines %>% filter(!is.na(highway)) %>%
  st_crop(local_bbox_st)

local_highways %>%
  select(where(~!all(is.na(.x)))) %>% 
  glimpse()

local_highways %>% 
  ggplot() +
  geom_sf() +
  labs(x = "", y = "", title = "") +
  theme_void()


# make sf network ---------------------------------------------------------

walking_speed <- 1.34

net <- local_highways %>%
  select(where(~!all(is.na(.x)))) %>% 
  filter(!highway %in% c("service", "motorway", "motorway_link"),
         !foot %in% c("private", "no")) %>% 
  st_cast("LINESTRING") %>% # loses part of one multilinestring
  as_sfnetwork() %>%
  activate("edges") %>%
  mutate(speed = units::set_units(walking_speed, "m/s")) %>% # default walking speed used by OpenTripPlanner
  mutate(time = edge_length() / speed)

# find target locations ---------------------------------------------------

target_street_name <- "Oriental Parade"

target_street_edges <- net %>%
  activate("edges") %>%
  filter(startsWith(name, target_street_name)) %>% 
  st_as_sf("edges")

target_intersections <- st_as_sf(net, "nodes") %>% 
  st_filter(target_street_edges, .predicate = st_intersects)

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), colour = "grey50") +
  geom_sf(data = st_as_sf(net, "nodes"), size = 1, colour = "blue") +
  geom_sf(data = target_intersections, size = 2, colour = "red")


# find isochrone node sets ------------------------------------------------

time_threshold <- 10 # minutes
buffer_time <- 2 # minutes
buffer_dist <- 2 * 60 * walking_speed

find_iso_nodes <- function(this_node, net, time_threshold) {
  net %>%
    activate("nodes") %>% 
    filter(node_distance_from(this_node, weights = time) <= 60 * (time_threshold - buffer_time)) %>% 
    st_as_sf("nodes")
}

iso_nodes <- st_nearest_feature(target_intersections, st_as_sf(net, "nodes")) %>% 
  map_dfr(find_iso_nodes, net, time_threshold) %>% 
  distinct()


# find isochrone ----------------------------------------------------------

isochrone_sf <- iso_nodes %>% 
  st_as_sf("edges") %>% 
  select(geometry) %>% 
  st_combine() %>% 
  st_buffer(dist = 2 * 60 * walking_speed)

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), colour = "grey80") +
  geom_sf(data = isochrone_sf, colour = "blue", fill = "steelblue", alpha = 0.3) +
  geom_sf(data = st_as_sf(iso_nodes, "nodes"), colour = "firebrick", size = 0.5) +
  theme_void()

# find intersecting sa1s --------------------------------------------------

intersecting_sa1s <- sa_geom_sf %>% 
  right_join(area_census_df, by = "sa1_id") %>% 
  st_transform(crs = 4326) %>% 
  st_filter(isochrone_sf, .predicate = st_intersects)

ggplot() +
  geom_sf(data = intersecting_sa1s, colour = "purple", fill = "purple", alpha = 0.5) +
  geom_sf(data = st_as_sf(net, "edges"), colour = "grey80") +
  geom_sf(data = isochrone_sf, colour = "steelblue", fill = "steelblue", alpha = 0.3) +
  geom_sf(data = st_as_sf(iso_nodes, "nodes"), colour = "firebrick", size = 0.5) +
  theme_void()

intersecting_sa1s %>% pull(usual_resident_count) %>% sum()
# ~ 10000 people
