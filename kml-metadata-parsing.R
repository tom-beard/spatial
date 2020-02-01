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
library(rvest)

# test import of a single Motion X GPS kml file ----------------------------------------


test_dir <- "D:/Downloads/Track 262"
filename <- "doc.kml"
test_file <- path(test_dir, filename)

kml_test <- st_read(test_file, stringsAsFactors = FALSE)
kml_desc <- kml_test$Description[3]
kml_table_list <- kml_desc %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table()
