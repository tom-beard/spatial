library(tidyverse)
library(osmdata)
library(sf)
library(tictoc)

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
  theme_void()

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
  theme_void()


# centroids and alternatives ----------------------------------------------

test_mb <- focus_mb_geom %>% 
  select(MB2014) %>% 
  st_transform(crs = 2193)

tic()
mb_centroids <- test_mb %>% mutate(centroid = st_centroid(geometry))
toc()
# ~0.1s, 492 features

tic()
mb_point_on_surface <- test_mb %>% mutate(centroid = st_point_on_surface(geometry))
toc()
# ~0.07s, 492 features

test_buffers <- buffers_by_mb %>% select(MB2014)

tic()
buffer_centroids <- test_buffers %>% mutate(centroid = st_centroid(geometry))
toc()
# ~0.1s, 492 features

tic()
buffer_point_on_surface <- test_buffers %>% mutate(centroid = st_point_on_surface(geometry))
toc()
# ~0.1s, 492 features

# note: st_point_on_surface() is supposed to be slow, but seems at least as fast as centroids?

prep_centroids <- function(centroid_sf) {
  centroid_sf %>% 
    st_set_geometry("centroid") %>% 
    select(MB2014) %>% 
    left_join(focus_mb_geom %>% as_tibble() %>% select(MB2014, AU2014_NAM), by = "MB2014") %>%
    filter(AU2014_NAM %in% AU_subset) %>% 
    select(MB2014)
}

buffers_by_mb %>% 
  filter(AU2014_NAM %in% AU_subset) %>% 
  ggplot() +
  geom_sf(data = focus_mb_geom %>% filter(AU2014_NAM %in% AU_subset),
          fill = "grey80", colour = "white", size = 0.2) +
  geom_sf(fill = "blue", colour = NA, alpha = 0.1) +
  geom_sf(data = prep_centroids(mb_centroids), colour = "red", alpha = 0.8) +
  geom_sf(data = prep_centroids(mb_point_on_surface), colour = "firebrick", alpha = 0.8) +
  geom_sf(data = prep_centroids(buffer_centroids), colour = "darkgreen", alpha = 0.8) +
  geom_sf(data = prep_centroids(buffer_point_on_surface), colour = "green", alpha = 0.8) +
  theme_void()
