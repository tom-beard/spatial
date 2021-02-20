# based on https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

library(osmdata)
library(tidyverse)

q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))

q <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = 'highway', value = 'motorway')

head(available_features()) # takes a few seconds

x <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf()

x$osm_lines %>% class()

x$osm_lines %>%
  ggplot() +
  geom_sf() +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
  
