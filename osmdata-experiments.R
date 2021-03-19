# basic vignette ----------------------------------------------------------

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
  
x_xml <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_xml()

# note: osmdata builds objects of type XML::xml_document, but doesn't install the XML package

x_xml %>% class()
x_xml %>% as.character()

# might be able to use xml2 instead
x_xml %>% xml2::as_list()


# try local examples ------------------------------------------------------

library(osmdata)
library(tidyverse)
library(sf)

local_osm <- opq(bbox = 'whangarei nz') %>%
  add_osm_feature(key = 'highway', value = 'trunk') %>%
  osmdata_sf()

local_osm$osm_lines %>% class() # sf and data.frame, but not tbl!
local_osm$osm_lines %>% glimpse()

local_osm$osm_lines %>%
  mutate(maxspeed = as.integer(maxspeed), lanes = as.integer(lanes)) %>% 
  ggplot() +
  geom_sf(aes(colour = maxspeed), size = 2) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# try animation ---------------------------------------------

library(gganimate)
# transformr is required to tween sf layers
# also, gifski is required for gifs

anim <- local_osm$osm_lines %>%
  mutate(maxspeed = as.integer(maxspeed), lanes = as.integer(lanes)) %>% 
  ggplot() +
  geom_sf(aes(colour = maxspeed, group = seq_along(maxspeed)), size = 2.5) +
  scale_color_viridis_c() +
  labs(x = "", y = "", title = "Max speed <= {closest_state}km/h") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  transition_states(maxspeed)

# shadow_mark() does the trick! Can even give a subtle fade to previous states
animate(anim + enter_fade() + exit_fade() + shadow_mark(size = 2),
        nframes = 50)

# highway styling

whangarei_base <- osm_base %>%
  st_join(summarise(pseudo_accessibility_sf), left = FALSE, join = st_within) %>% 
  mutate(hway_category = case_when(
    highway %in% c("trunk", "trunk_link", "primary", "secondary", "secondary_link") ~ "main",
    highway %in% c("residential") ~ "residential",
    TRUE ~ "other"
  ))

whangarei_base %>% 
  ggplot() +
  geom_sf(data = pseudo_accessibility_sf, aes(fill = reachable_area), alpha = 1) +
  geom_sf(aes(colour = factor(hway_category)),
          size = .5) +
  scale_fill_viridis_c(option = "A") +
  scale_color_manual(values = c("main" = "white", "residential" = "grey50", "other" = "grey20")) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.spacing = unit(2, "lines"))

