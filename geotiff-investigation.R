library(tidyverse)
library(rayshader)
library(fs)
library(raster)

# attempt reading LINZ DEM ------------------------------------------------

dem_path <- "D:/GIS/wellington/lds-wellington-lidar-1m-dem-2013-GTiff"
dem_file <- "DEM_BQ31_2013_1000_2139.tif"

localtif <- raster::raster(path(dem_path, dem_file))

elmat <- matrix(raster::extract(localtif, raster::extent(localtif), buffer = 1000),
                nrow = ncol(localtif), ncol = nrow(localtif))

dem_file2 <- "DEM_BQ31_2013_1000_2140.tif"
localtif2 <- raster::raster(path(dem_path, dem_file2))

elmat2 <- matrix(raster::extract(localtif2, raster::extent(localtif2), buffer = 1000),
                 nrow = ncol(localtif2), ncol = nrow(localtif2))

elmat_combined <- cbind(elmat, elmat2)

raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)

elmat %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(elmat), color = "steelblue") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  plot_map()

elmat %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(elmat), color = "steelblue") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  plot_3d(elmat, zscale = 1, fov = 0, theta = 135, zoom = 0.75, phi = 45,
          windowsize = c(1000, 800))

elmat %>% 
  as_tibble() %>% 
  rownames_to_column("row") %>% 
  pivot_longer(-row, names_to = "col", values_to = "value") %>% 
  ggplot() +
  geom_histogram(aes(x = value)) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

localtif %>% plot()  
