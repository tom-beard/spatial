library(tidyverse)
library(fs)
# library(vroom)
library(lubridate)
library(here)
library(hms)
library(glue)
library(skimr)
library(janitor)
library(sf)
library(leaflet)
# library(ggmap)
library(xml2)
library(rgdal)
# library(rvest)
library(exifr)

img_path <- "D:/Pictures/iphone_DCIM/111APPLE"

all_files <- dir_ls(path = img_path)
all_file_paths <- tibble(path = all_files) %>% 
  mutate(ext = path_ext(path))
all_file_paths %>% count(ext)

img_file_paths <- all_file_paths %>% 
  filter(ext %in% c("JPG", "HEIC")) %>% 
  pull(path)

img_info_df <- read_exif(img_file_paths)
img_info_df %>%
  glimpse()
img_info_df %>%
  select(SourceFile, FileModifyDate, starts_with("GPS")) %>%
  glimpse()

img_info_df %>%
  filter(!is.na(GPSLongitude)) %>% 
  count(FileTypeExtension)
img_info_df %>%
  count(FileTypeExtension)
