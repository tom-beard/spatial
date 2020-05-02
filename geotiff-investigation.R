library(tidyverse)
library(fs)
library(raster)


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

# reading GHS GeoTIFF ------------------------------------------------

geotiff_path <- "D:/GIS/ghs-pop"
geotiff_file <- "GHS_POP_E2015_GLOBE_R2019A_4326_9ss_V1_0_35_12.tif"

localtif <- raster::raster(path(geotiff_path, geotiff_file))

pop_matrix <- tiff_to_matrix(localtif)
pop_df <- popmatrix_to_tibble(pop_matrix)

pop_df %>% 
  ggplot() +
  geom_histogram(aes(x = value)) +
  scale_x_log10() +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

localtif %>% plot()

pop_df %>% mutate(per_acre = value / sqkm_to_acre) %>% pull(per_acre) %>% max()

# crop to Auckland

ak_extent <- extent(174, 176.5, -38, -36.5)
ak_tiff <- crop(localtif, ak_extent)
ak_tiff %>% plot()

ak_matrix <- tiff_to_matrix(ak_tiff)
ak_df <- popmatrix_to_tibble(ak_matrix)

ak_df %>% 
  ggplot() +
  geom_histogram(aes(x = value)) +
  scale_x_log10(breaks = c(1/100, 1, 100, 1000), labels = c("1/100", 1, 100, 1000)) +
  labs(x = "population per square km", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
