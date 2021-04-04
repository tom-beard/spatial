# Functions to style OSM streetmaps for basemap use

# inspiration from:
# http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# https://www.dshkol.com/2018/better-maps-with-vector-tiles/
# http://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/
# https://taraskaduk.com/posts/2021-01-18-print-street-maps/
# https://cran.r-project.org/web/packages/osmplotr/vignettes/maps-with-ocean.html

library(tidyverse)
library(sf)
library(glue)
library(fs)
library(osmdata) # for local use, read from local files instead
library(janitor)
# library(osmplotr)

this_area <- "Whangarei District"

# get and process OSM data ------------------------------------------------------------

district_bbox <- getbb(paste(this_area, "NZ")) # untested
# district_bbox <- getbb('whangarei district nz')
district_bbox_st <- c(xmin = district_bbox["x", "min"], xmax = district_bbox["x", "max"],
                      ymin = district_bbox["y", "min"], ymax = district_bbox["y", "max"]) %>% 
  st_bbox(crs = 4326)
district_osm <- opq(bbox = district_bbox) %>%
  osmdata_sf()
# takes ~2 mins on laptop. resulting object ~3.4GB, but we want to get water objects etc as well as streets

district_lines <- district_osm$osm_lines
district_highways <- district_lines %>% filter(!is.na(highway)) %>%
  st_crop(district_bbox_st)
district_polygons <- district_osm$osm_polygons
district_multipolygons <- unname_osmdata_sf(district_osm)$osm_multipolygons # fixed named geometry object issue
district_boundary <- district_multipolygons %>%
  filter(name == this_area) %>% 
  select(name, admin_level, population)

district_bbox_area <- district_bbox_st %>%
  st_as_sfc() %>%
  st_area() %>%
  units::set_units(km2) %>%
  as.numeric()
min_area <- (sqrt(district_bbox_area) / 100) ^ 2

district_water <- district_polygons %>% select(name, natural, water, waterway) %>% 
  bind_rows(district_multipolygons %>% select(name, natural, water, waterway)) %>% 
  filter(natural %in% c("water", "coastline", "bay") |
           water %in% c("lake", "reservoir", "river") |
           waterway %in% c("dam", "river", "riverbank")) %>% 
  # st_transform(2193) %>% 
  mutate(area = st_area(.) %>% units::set_units(km2) %>% as.numeric()) %>% 
  st_crop(district_bbox_st)

district_coast <- district_lines %>% 
  filter(natural == "coastline")
district_coast %>% janitor::remove_empty("cols") %>% glimpse()

all_residential <- district_multipolygons %>% 
  filter(landuse == "residential") %>% 
  select(name) %>% bind_rows(
    district_polygons %>%
      filter(landuse == "residential") %>% 
      select(name))

all_green <- district_polygons %>% select(name, natural, landuse) %>% 
  bind_rows(district_multipolygons %>% select(name, natural, landuse)) %>% 
  filter(landuse %in% c("forest", "grass", "recreation_ground") |
           natural %in% c("wood", "scrub", "grassland")) %>% 
  st_crop(district_bbox_st) %>%
  st_union()

all_urban <- district_polygons %>% select(name, landuse) %>% 
  bind_rows(district_multipolygons %>% select(name, landuse)) %>% 
  filter(landuse %in% c("residential", "commercial", "industrial", "retail")) %>% 
  st_union() %>% 
  st_crop(district_bbox_st)

district_polygons %>% as_tibble() %>% count(highway, junction)
# note: several highways appear as polygons
# a few walkways etc, but mostly roundabouts
# might just be able to use st_cast() to convert to linestring?

# prepare highway layers ----------------------------------------------------------

highway_sizes <- tibble::tribble(
  ~highway, ~highway_group, ~size,
  "motorway",        "large",   0.5,
  "motorway_link",        "large",   0.3,
  "trunk",        "large",   0.5,
  "trunk_link",        "large",   0.3,
  "primary",        "large",   0.5,
  "primary_link",        "large",   0.3,
  "secondary",       "medium",   0.3,
  "secondary_link",       "medium",   0.3,
  "tertiary",       "medium",   0.3,
  "tertiary_link",       "medium",   0.3,
  "residential",        "small",   0.2,
  "living_street",        "small",   0.2,
  "unclassified",        "small",   0.2,
  "service",        "small",   0.2,
  "steps",        "small",   0.2,
  "pedestrian",        "small",   0.2,
  "footway",        "small",   0.2
)

filter_highways <- function(input_sf, highway_groups = c("small")) {
  input_sf %>%
    filter(highway %in%
             (highway_sizes %>%
                filter(highway_group %in% highway_groups) %>%
                pull(highway))
    ) %>%
    select(name, highway)
}

# tried bbox based on extent of smaller roads, and it didn't work
# but code can make a useful function
st_bbox_with_buffer <- function(input_sf, buffer_m = 10) {
  st_bbox(input_sf) %>%
    st_as_sfc() %>% 
    st_as_sf() %>% 
    st_transform(crs = 2193) %>% 
    st_buffer(dist = buffer_m) %>%
    st_transform(crs = 4326) %>% 
    st_bbox()
}

# use function to add road layer ----------------------------------------------

geom_road <- function(road_type, ...) {
  geom_sf(data = filter_highways(district_highways, road_type), ...)
}

ggplot() +
  geom_sf(data = district_water, fill = "steelblue", colour = NA, alpha = 0.5) +
  geom_sf(data = all_green, fill = "darkgreen", colour = NA, alpha = 0.2) +
  geom_sf(data = all_urban, fill = "grey50", colour = NA, alpha = 0.2) +
  geom_road("small", colour = "grey70", size = .2) +
  geom_road("medium", colour = "grey50", size = .5) +
  geom_road("large", colour = "grey40", size = 1) +
  labs(x = "", y = "", title = "") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey90", colour = "grey90")
  )

# green land use colour sort of works as is, but probably won't as a basemap for choropleths


# prepare meshblocks for choropleth tests ---------------------------------

path_to_shapefile <- "D:/GIS/census/Census 2013/ESRI_Shapefile_Digital_Boundaries_2014_High_Def_Clipped/MB2014_HD_Clipped.shp"
path_to_mb_data <- "D:/GIS/census/Census 2013/2013_mb_dataset_Total_New_Zealand_CSV/mb2013_indiv1.csv"

mb_df <- read_csv(path_to_mb_data) %>% select(1:6)

mb_geom <- st_read(path_to_shapefile,
                    quiet = TRUE) %>% 
  filter(TA2014_NAM == this_area) %>% # when this_area is a TA name
  left_join(mb_df, by = c("MB2014" = "Code")) %>% 
  mutate(pop_density = `2013_Census_census_usually_resident_population_count(1)` / SHAPE_Area) %>% 
  st_transform(crs = 4326)

# mock up data for small multiples
mb_range <- mb_geom$MB2014 %>% as.numeric() %>% range()
density_range <- mb_geom$pop_density %>% as.numeric() %>% range()

mock_stats <- mb_geom %>% 
  filter(pop_density > 1e-04) %>% 
  as_tibble() %>% 
  select(MB2014, v_1 = pop_density) %>% 
  mutate(MB = as.numeric(MB2014)) %>% 
  mutate(v_2 = (density_range[2] - v_1) *.6,
         v_3 = v_1 * (MB - mb_range[1]) / diff(mb_range),
         v_4 = v_2 * (MB - mb_range[1]) / diff(mb_range)
  ) %>% 
  pivot_longer(starts_with("v_"), names_to = "threshold", names_prefix = "v_") %>% 
  select(-MB) %>% 
  mutate(threshold = as.numeric(threshold) * 15)

mock_stats_sf <- mb_geom %>% 
  inner_join(mock_stats, by = "MB2014")


# basemap with choropleth -------------------------------------------------

ggplot() +
  geom_sf(data = district_water, fill = "steelblue", colour = NA, alpha = 0.5) +
  # geom_sf(data = all_green, fill = "darkgreen", colour = NA, alpha = 0.2) +
  geom_sf(data = all_urban, fill = "grey50", colour = NA, alpha = 0.2) +
  geom_sf(data = mock_stats_sf %>% filter(threshold == 60),
          aes(fill = pop_density),
          colour = NA, alpha = 1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  geom_road("small", colour = "grey70", size = .2) +
  geom_road("medium", colour = "grey50", size = .5) +
  geom_road("large", colour = "grey40", size = 1) +
  labs(x = "", y = "", title = "") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey90", colour = "grey90")
  )

# notes:
# better without green areas
# urban areas redundant when pop-based choropleths are shown
# roads are better on top of choropleths
# palette should start with lighter colours

# viridis palettes work better against dark backgrounds, but lowest values are still too dark

# try plotting OSM coastline  -----------------------------------------------

# verdict: not worth trying when we have access to coastlines from LINZ!

if (FALSE) {
  coast_poly <- osm_line2poly(district_coast, district_bbox_st)
  # this fails, due to an open issue: https://github.com/ropensci/osmplotr/issues/29
  # (awaiting PR: https://github.com/ropensci/osmplotr/pull/38/files)
  
  coast_poly_direct <- district_coast$geometry %>% 
    st_combine() %>% 
    st_polygonize()
  
  ggplot() +
    geom_sf(data = coast_poly_direct, colour = "blue", fill = "steelblue", alpha = 1)
  
  # try some code from https://github.com/ropensci/osmplotr/pull/38/files
  m1 <- st_cast(st_line_merge(st_union(district_coast$geometry)), "LINESTRING")
  m2 <- st_polygonize(m1)
  k <- st_dimension(m2)
  st_is_empty(m2)
  islands <- st_cast(m2)
  
  ggplot() +
    geom_sf(data = m2, colour = "blue", fill = "steelblue", alpha = 1)
  # same as coast_poly_direct
  
  m1lines <- m1[is.na(k)]
  
  m1lines %>% st_combine() %>% st_polygonize() # empty: can't polygonize
  polys <- st_cast(st_polygonize(m1lines[2]))
  
  lines2_start_end <- m1lines[2] %>% 
    st_transform(2193) %>% 
    st_line_sample(sample = c(0, 1)) %>% 
    st_transform(4326)
  
  lines1_bbox <- st_bbox_with_buffer(m1lines[1], buffer_m = 50000)
  lines2_bbox <- st_bbox_with_buffer(lines2_start_end, buffer_m = 1000)
  
  ggplot() +
    geom_sf(data = m1lines[2], colour = "blue", alpha = 1) +
    geom_sf(data = m1lines[1], colour = "red", alpha = 1, size = 1) +
    coord_sf(xlim = c(lines1_bbox$xmin, lines1_bbox$xmax), ylim = c(lines1_bbox$ymin, lines1_bbox$ymax))
  # stupid little snippet, possible near the top of the WDC bbox. Part of an island?
  
  ggplot() +
    geom_sf(data = m1lines[2], colour = "blue", alpha = 1) +
    geom_sf(data = m1lines[1], colour = "red", alpha = 1, size = 10) +
    geom_sf(data = lines2_start_end, colour = "purple", alpha = 1, size = 2) +
    coord_sf(xlim = c(lines2_bbox$xmin, lines2_bbox$xmax), ylim = c(lines2_bbox$ymin, lines2_bbox$ymax))
  # tiny gap in upper Waitemata! Kingsway Rd causeway
  
  ggplot() +
    geom_sf(data = district_water %>% filter(area > min_area), fill = "steelblue", colour = NA, alpha = 0.8) +
    geom_sf(data = district_boundary, colour = "red", fill = NA) +
    # geom_sf(data = district_coast, colour = "blue", alpha = 1) +
    geom_sf(data = mock_stats_sf %>% filter(threshold == 60),
            aes(fill = pop_density),
            colour = NA, alpha = 1) +
    scale_fill_viridis_c(option = "B") +
    geom_sf(data = filter_highways(district_highways, "small"), colour = "grey30", size = 0.2) +
    geom_sf(data = filter_highways(district_highways, "medium"), colour = "grey40", size = 0.5) +
    geom_sf(data = filter_highways(district_highways, "large"), colour = "grey50", size = 1) +
    labs(x = "", y = "", title = "") +
    coord_sf(expand = FALSE) +
    # coord_sf(xlim = c(district_bbox["x", ]), ylim = c(district_bbox["y", ]), expand = FALSE) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "grey20", colour = "grey20")
    )
  # coast is for whole NI!
  # Also extends all SHs, and possibly any relations that continue beyond the boundary,
  # but we clipped these earlier
}


# LINZ coastlines ---------------------------------------------------------

land2 <- st_read("D:/GIS/Terralink_Oct_07/Hydro/Polygons/land2.shp")
land2 %>% 
  st_transform(4326) %>% 
  ggplot() +
  geom_sf()

district_land <- land2 %>% 
  st_transform(4326) %>% # would be better to transform bbox to 2193 first and crop with that
  st_crop(district_bbox_st)

ggplot() +
  geom_sf(data = district_land, fill = "grey30", colour = NA) +
  geom_sf(data = district_boundary, fill = "grey20") +
  geom_sf(data = district_water %>% filter(area > min_area),
          fill = "steelblue", colour = NA) +
  geom_sf(data = mock_stats_sf %>% filter(threshold == 60),
          aes(fill = pop_density),
          colour = NA, alpha = 1) +
  scale_fill_viridis_c(option = "B") +
  geom_sf(data = filter_highways(district_highways, "small"), colour = "grey30", size = 0.2) +
  geom_sf(data = filter_highways(district_highways, "medium"), colour = "grey40", size = 0.5) +
  geom_sf(data = filter_highways(district_highways, "large"), colour = "grey50", size = 1) +
  labs(x = "", y = "", title = "") +
  coord_sf(expand = FALSE) +
  # coord_sf(xlim = c(district_bbox["x", ]), ylim = c(district_bbox["y", ]), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "steelblue")
  )
# this colour scheme for district vs non-district looks nice, partly because
# the fill matches the small highway colour

# small multiples ---------------------------------------------------------

ggplot(mock_stats_sf) +
  geom_sf(data = district_land, fill = "grey30", colour = NA) +
  geom_sf(data = district_boundary, fill = "grey20") +
  geom_sf(data = district_water %>% filter(area > min_area),
          fill = "steelblue", colour = NA) +
  geom_sf(aes(fill = value), colour = NA, alpha = 1) +
  scale_fill_viridis_c(option = "B") +
  geom_road("small", colour = "grey30", size = .2) +
  geom_road("medium", colour = "grey40", size = .5) +
  geom_road("large", colour = "grey50", size = 1) +
  facet_wrap(~ threshold) +
  labs(x = "", y = "",
       title = glue("Simulated data for various thresholds for {this_area}"),
       caption = "Base map and data from OpenStreetMap and OpenStreetMap Foundation") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "steelblue"),
    panel.spacing = unit(0.5, "lines"),
    strip.text = element_text(margin = margin(b = 0.5, unit = "lines"))
  )

