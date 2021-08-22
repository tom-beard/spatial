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

net <- local_highways %>%
  select(where(~!all(is.na(.x)))) %>% 
  filter(!highway %in% c("service", "motorway", "motorway_link"),
         !foot %in% c("private", "no")) %>% 
  st_cast("LINESTRING") %>% # loses part of one multilinestring
  as_sfnetwork()

