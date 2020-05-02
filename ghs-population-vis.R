# investigating the data and vis approach at http://www.statsmapsnpix.com/2020/04/population-density-in-europe.html

library(tidyverse)
library(rayshader)
library(fs)
library(raster)
library(viridis)

# common functions etc ----------------------------------------------------

sqkm_to_acre <- 247.105

tiff_to_matrix <- function(localtif) {
  matrix(raster::extract(localtif, raster::extent(localtif), buffer = 1000),
         nrow = ncol(localtif), ncol = nrow(localtif))
}

popmatrix_to_tibble <- function(pop_matrix) {
  pop_matrix %>% 
    as_tibble() %>% 
    rownames_to_column("row") %>% 
    pivot_longer(-row, names_to = "col", values_to = "value") %>% 
    drop_na(value)
}

# read GHS GeoTIFF ------------------------------------------------

# data from https://ghsl.jrc.ec.europa.eu/download.php?ds=pop, tile 35_12

geotiff_path <- "D:/GIS/ghs-pop"
geotiff_file <- "GHS_POP_E2015_GLOBE_R2019A_4326_9ss_V1_0_35_12.tif"
# geotiff_file <- "GHS_POP_E2015_GLOBE_R2019A_4326_9ss_V1_0_18_3.tif" # Europe for comparison

localtif <- raster::raster(path(geotiff_path, geotiff_file))

localtif %>% plot()

nz_matrix <- tiff_to_matrix(localtif)
nz_df <- popmatrix_to_tibble(nz_matrix)
nz_df %>% mutate(per_acre = value / sqkm_to_acre) %>% pull(per_acre) %>% max()

nz_df %>% 
  ggplot() +
  geom_histogram(aes(x = value)) +
  scale_x_log10(breaks = c(1/100, 1, 100, 1000), labels = c("1/100", 1, 100, 1000)) +
  labs(x = "population per square km", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

max_pop_value_nz <- max(nz_df$value)

export_3d_pop <- function(localtif, cropped_extent, output_filename) {
  cropped_tiff <- crop(localtif, cropped_extent)
  cropped_tiff %>% plot()
  
  cropped_matrix <- tiff_to_matrix(cropped_tiff)
  max_pop_value <- cropped_matrix %>% popmatrix_to_tibble() %>% pull(value) %>% max()
  print(max_pop_value)
  
  raymat <- ray_shade(cropped_matrix)
  ambmat <- ambient_shade(cropped_matrix)
  pop_overlay <- height_shade(raster_to_matrix(cropped_tiff),
                              texture = viridis(100, option = "A", begin = 0,
                                                end = max_pop_value / max_pop_value_nz))
  
  cropped_matrix %>%
    sphere_shade(texture = "imhof1") %>%
    add_shadow(raymat) %>%
    add_shadow(ambmat) %>%
    add_overlay(pop_overlay, alphalayer = 0.8, alphamethod = "multiply") %>% 
    plot_3d(cropped_matrix, solid = FALSE, shadowdepth = -5, lineantialias = TRUE, linewidth = 0,
            zscale = 10, fov = 0, theta = 0, zoom = 0.25, phi = 15,
            windowsize = c(100, 100, 1600, 700))
  render_snapshot(filename = here::here(output_filename), clear = TRUE, instant_capture = FALSE)
}

export_3d_pop(localtif, extent(174, 176.5, -38.5, -36.5), "auckland-waikato")
export_3d_pop(localtif, extent(174.3, 176.7, -41.8, -39.8), "wellington")
