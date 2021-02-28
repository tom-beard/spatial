library(tidyverse)
library(osmdata)
library(sf)

# get meshblock geometry and data --------------------------------------------------

path_to_shapefile <- "D:/GIS/census/Census 2013/ESRI_Shapefile_Digital_Boundaries_2014_High_Def_Clipped/MB2014_HD_Clipped_Wellington_region.shp"
(mb_geom <- st_read(path_to_shapefile,
                    quiet = TRUE))

path_to_mb_data <- "D:/GIS/census/Census 2013/2013_mb_dataset_Total_New_Zealand_CSV/mb2013_indiv1.csv"

path_to_mb_data %>% glimpse()

mb_df <- read_csv(path_to_mb_data) %>% select(1:6)
mb_geom %>% 
  filter(WARD2014_N == "Lambton Ward") %>% 
  left_join(mb_df, by = c("MB2014" = "Code")) %>% 
  mutate(pop_density = `2013_Census_census_usually_resident_population_count(1)` / SHAPE_Area)
  

# get osm highways --------------------------------------------------------

local_osm <- opq(bbox = 'whangarei nz') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()


