library(tidyverse)
library(osmdata)
library(sf)

# get meshblock geometry --------------------------------------------------

# use meshblocks rather than SA1s for now

path_to_shapefile <- "D:/GIS/census/Census 2013/ESRI_Shapefile_Digital_Boundaries_2014_High_Def_Clipped/MB2014_HD_Clipped.shp"
(mb_geom <- st_read(path_to_shapefile,
                    quiet = TRUE))

focus_mb_geom <- mb_geom %>% 
  filter(UA2014_NAM == "Whangarei") %>% 
  st_transform(crs = 4326)

# get osm highways --------------------------------------------------------

focus_highways <- opq(bbox = 'whangarei nz') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

focus_highways$osm_lines %>%
  mutate(maxspeed = as.integer(maxspeed), lanes = as.integer(lanes)) %>% 
  ggplot() +
  geom_sf(data = focus_mb_geom, fill = "grey80", colour = "white", size = 0.2) +
  geom_sf(aes(colour = maxspeed), size = 1) +
  geom_sf(data = focus_highways$osm_polygons, fill = "red") +
  labs(x = "", y = "", title = "") +
  coord_sf() +
  theme_minimal()

# not sure what the polygons are: at least one is a loop track, that should be lines

# create highway buffer ---------------------------------------------------

focus_highways$osm_lines %>% glimpse()

highway_buffers <- focus_highways$osm_lines %>% 
  select(geometry) %>% 
  st_transform(crs = 2193) %>% 
  mutate(buffer = st_buffer(geometry, dist = 50)) %>% 
  st_set_geometry("buffer") %>% 
  select(-geometry) %>% 
  summarise()

buffers_by_mb <- focus_mb_geom %>% 
  select(MB2014) %>% 
  st_transform(crs = 2193) %>% 
  st_intersection(highway_buffers) %>% 
  left_join(focus_mb_geom %>% as_tibble() %>% select(MB2014, AU2014_NAM), by = "MB2014")

AU_subset <- c("Kensington", "Mairtown", "Regent")

buffers_by_mb %>% 
  filter(AU2014_NAM %in% AU_subset) %>% 
  ggplot() +
  geom_sf(data = focus_mb_geom %>% filter(AU2014_NAM %in% AU_subset),
          fill = "grey80", colour = "white", size = 0.2) +
  geom_sf(fill = "blue", colour = NA, alpha = 0.3) +
  labs(x = "", y = "", title = "") +
  coord_sf() +
  theme_minimal()

