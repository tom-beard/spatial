library(sf)
library(tidyverse)

path_to_mb_file <- "D:/R/GIS/tester.RDS"

mb_sf <- readRDS(path_to_mb_file)

attr(mb_sf, "sf_column")
mb_sf %>% st_geometry()

mb_sf %>% 
  st_set_crs(2193) %>%
  st_transform(4326) %>%
  st_bbox()
mb_sf %>% 
  st_bbox()

mb_sf %>% 
  st_set_crs(2193) %>%
  st_transform(4326) %>%
  st_is_longlat()

mb_sf %>% 
  st_set_crs(2193) %>%
  st_transform(4326) %>%
  ggplot() +
  geom_sf(aes(geometry = WKT))


# P -----------------------------------------------------------------------

newmb <-  st_set_crs(mb_sf,2193)
newmb <- st_transform(newmb,4326)
ggplot(newmb)+geom_sf(data = newmb, aes(geometry=newmb$WKT))

ggplot(newmb) + geom_sf(aes(geometry=WKT))
  
  geom_sf(data = newmb, aes(geometry=newmb$WKT))

