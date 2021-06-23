library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(igraph)

# set up base network -----------------------------------------------------

roxel
net <- as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

# randomly generate walking speeds for each edge type
types <- net %>%
  activate("edges") %>%
  pull(type) %>%
  unique()

# Randomly define a walking speed in m/s for each type: between 3 and 7 km/hr.
set.seed(1)
speeds <- runif(length(types), 3 * 1000 / 60 / 60, 7 * 1000 / 60 / 60)

# Assign a speed to each edge based on its type.
# Calculate travel time for each edge based on that.
net <- net %>%
  activate("edges") %>%
  group_by(type) %>%
  mutate(speed = units::set_units(speeds[cur_group_id()], "m/s")) %>%
  mutate(time = weight / speed) %>%
  ungroup()

net %>% 
  activate("edges") %>% 
  as_tibble() %>% 
  transmute(time = as.numeric(time)) %>% 
  ggplot() +
  geom_histogram(aes(x = time)) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
  

# isochrone calculation ---------------------------------------------------

time_threshold <- 600
dist_buffer <- 50

calc_basic_isochrone <- function(origin_point, network, time_threshold, dist_buffer) {
  network %>%
    activate("nodes") %>% 
    filter(node_distance_from(st_nearest_feature(origin_point, net), weights = time) <= time_threshold) %>% 
    st_as_sf("edges") %>% 
    select(geometry) %>% 
    st_combine() %>% 
    st_buffer(dist = dist_buffer)
}

# for some reason, slice_sample() provides a result that doesn't match nicely to nodes, unlike the below:
p <- net %>%
  activate("nodes") %>% 
  st_as_sf() %>%
  slice(sample(1:nrow(.), 1))

isochrone_sf <- calc_basic_isochrone(p, net, time_threshold, dist_buffer)

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), colour = "grey80") +
  geom_sf(data = isochrone_sf, colour = "blue", fill = "steelblue", alpha = 0.3) +
  geom_sf(data = p, colour = "firebrick", alpha = 0.8, size = 2) +
  theme_void()

origins <- net %>% 
  activate("nodes") %>% 
  slice(20:40) %>% 
  st_as_sf() %>% 
  mutate(sa1_id = 7000 + row_number())

# use of split() based on https://stackoverflow.com/questions/43694187/row-wise-operations-on-sf-objects
# to enforce row-wise operations
iso_sf <- origins %>% 
  mutate(new_geom = split(., row_number())) %>% 
  mutate(service_area = map(new_geom, calc_basic_isochrone, network = net,
                            time_threshold = time_threshold, dist_buffer = dist_buffer)) %>% 
  unnest(service_area) %>%
  st_set_geometry("service_area") %>% 
  select(sa1_id, service_area)

mock_sa_sf <- net %>% 
  st_as_sf("nodes") %>% 
  st_make_grid(n = c(20, 20), square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(jobs = runif(row_number()) * 1000)

iso_sf %>% 
  ggplot() +
  geom_sf(data = mock_sa_sf, aes(fill = jobs), alpha = 0.8) +
  geom_sf(colour = "red", fill = "red", alpha = 0.2) +
  theme_void()

iso_sf %>% 
  st_intersection(mock_sa_sf)

iso_sf %>% 
  slice(1) %>% 
  st_join(mock_sa_sf, join = st_intersects) %>% 
  plot()

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), colour = "grey80") +
  geom_sf(data = st_join(mock_sa_sf, iso_sf[1, ], join = st_overlaps, left = FALSE),
          fill = "firebrick", alpha = 0.5) +
  geom_sf(data = st_join(mock_sa_sf, iso_sf[1, ], join = st_within, left = FALSE),
          fill = "steelblue", alpha = 0.5) +
  geom_sf(data = iso_sf[1, ], colour = "yellow", fill = NA) +
  theme_void()
