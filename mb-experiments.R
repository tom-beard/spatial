# just a quick test of meshblock reading, joining and visualising

library(sf)
library(tidyverse)

test_sf <- st_read("D:/GIS/census/Census 2013/GEDs_clipped_simplified_3.shp",
                       quiet = TRUE)

ggplot(test_sf) +
  geom_sf(aes(fill = ged))

path_to_shapefile <- "D:/GIS/census/Census 2013/ESRI_Shapefile_Digital_Boundaries_2014_High_Def_Clipped/MB2014_HD_Clipped_Wellington_region.shp"
(mb_test <- st_read(path_to_shapefile,
                       quiet = TRUE))

ggplot(mb_test) +
  geom_sf(aes(fill = UA2014))

path_to_mb_data <- "D:/GIS/census/Census 2013/2013_mb_dataset_Total_New_Zealand_CSV/mb2013_indiv1.csv"

mb_df <- read_csv(path_to_mb_data) %>% select(1:6)
mb_test %>% 
  filter(WARD2014_N == "Lambton Ward") %>% 
  left_join(mb_df, by = c("MB2014" = "Code")) %>% 
  mutate(pop_density = `2013_Census_census_usually_resident_population_count(1)` / SHAPE_Area) %>% 
  ggplot() +
  geom_sf(aes(fill = pop_density)) +
  scale_fill_viridis_c(option = "A")

