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

# not sure what the polygons are!

