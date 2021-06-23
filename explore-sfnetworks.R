# Get familiar with sfnetworks

# based on https://luukvdmeer.github.io/sfnetworks/articles/structure.html

library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(igraph)

# Construction ------------------------------------------------------------

p1 <- st_point(c(7, 51))
p2 <- st_point(c(7, 52))
p3 <- st_point(c(8, 52))
p4 <- st_point(c(8, 51.5))

l1 <- st_sfc(st_linestring(c(p1, p2)))
l2 <- st_sfc(st_linestring(c(p1, p4, p3)))
l3 <- st_sfc(st_linestring(c(p3, p2)))

edges <- st_as_sf(c(l1, l2, l3), crs <- 4326)
nodes <- st_as_sf(c(st_sfc(p1), st_sfc(p2), st_sfc(p3)), crs <- 4326)

edges$from <- c(1, 1, 3)
edges$to <- c(2, 3, 2)

net <- sfnetwork(nodes, edges)
net

nodes$name <- c("city", "village", "farm")
edges$from <- c("city", "city", "farm")
edges$to <- c("village", "farm", "village")

edges

net <- sfnetwork(nodes, edges, node_key = "name")
net

# weights

edges$from <- c(1, 1, 3)
edges$to <- c(2, 3, 2)

net <- sfnetwork(nodes, edges, length_as_weight = TRUE)
net

st_geometry(edges) <- NULL

other_net <- sfnetwork(nodes, edges, edges_as_lines = TRUE)

plot(net, cex = 2, lwd = 2, main = "Original geometries")
plot(other_net, cex = 2, lwd = 2, main = "Straight lines")


# from sf objects ---------------------------------------------------------

roxel
net <- as_sfnetwork(roxel)
plot(net)


# Vis --------------------------------------------------------------

net %>% 
  autoplot() +
  ggtitle("Here's Roxel!")

net <- net %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness())

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(net, "nodes"), aes(col = bc, size = bc)) +
  ggtitle("Betweenness centrality in Münster Roxel")


# geometry access and manipulation ---------------------------------------------------

net %>%
  activate("edges") %>%
  st_set_geometry(NULL) %>%
  plot(draw_lines = FALSE, main = "Edges without geometries")

net %>%
  activate("nodes") %>%
  st_set_geometry(NULL) %>%
  plot(vertex.color = "black", main = "Nodes without geometries")
# a very ugly graph vis, by default!

as_sfnetwork(roxel, directed = TRUE) %>%
  activate("edges") %>%
  st_reverse()

node_coords <- net %>%
  activate("nodes") %>%
  st_coordinates()

node_coords[1:4, ]

# Currently there are neither Z nor M coordinates.
st_z_range(net)
st_m_range(net)

# Add Z coordinates with value 0 to all features.
# This will affect both nodes and edges, no matter which element is active.
st_zm(net, drop = FALSE, what = "Z")

net %>%
  st_zm(drop = FALSE, what = "Z") %>%
  mutate(X = node_X(), Y = node_Y(), Z = node_Z(), M = node_M())

net %>%
  activate("nodes") %>%
  st_bbox()
net %>%
  activate("edges") %>%
  st_bbox()


# joining, filtering & cropping -----------------------------------------------------

# from https://luukvdmeer.github.io/sfnetworks/articles/join_filter.html

net <- as_sfnetwork(roxel) %>%
  st_transform(3035)

p1 <- st_point(c(4151358, 3208045))
p2 <- st_point(c(4151340, 3207520))
p3 <- st_point(c(4151756, 3207506))
p4 <- st_point(c(4151774, 3208031))

poly <- st_multipoint(c(p1, p2, p3, p4)) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 3035)

net %>%
  activate("edges") %>%
  filter(edge_crosses(.E()))
# findsedges that cross others: none in this example

v <- 4152000
l <- st_linestring(rbind(c(v, st_bbox(net)["ymin"]), c(v, st_bbox(net)["ymax"])))

filtered_by_coords <- net %>%
  activate("nodes") %>%
  filter(node_X() > v)

plot(net, col = "grey")
plot(l, col = "red", lty = 4, lwd = 4, add = TRUE)
plot(net, col = "grey")
plot(filtered_by_coords, col = "red", add = TRUE)

cropped <- net %>%
  activate("edges") %>%
  st_crop(poly) %>%
  activate("nodes") %>%
  filter(!node_is_isolated())

plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(cropped, add = TRUE)


# routing -----------------------------------------------------------------

# based on https://luukvdmeer.github.io/sfnetworks/articles/routing.html

library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
# library(TSP)

net <- as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

net

paths <- st_network_paths(net, from = 495, to = c(458, 121))
paths

paths %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

paths %>%
  slice(1) %>%
  pull(edge_paths) %>%
  unlist()

plot_path <- function(node_path) {
  net %>%
    activate("nodes") %>%
    slice(node_path) %>%
    plot(cex = 1.5, lwd = 1.5, add = TRUE)
}

colors <- sf.colors(3, categorical = TRUE)

plot(net, col = "grey")
paths %>%
  pull(node_paths) %>%
  walk(plot_path)
net %>%
  activate("nodes") %>%
  st_as_sf() %>%
  slice(c(495, 121, 458)) %>%
  plot(col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)

p1 <- st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50))) # add an offset
st_crs(p1) <- st_crs(net)
p2 <- st_geometry(net, "nodes")[458]
p3 <- st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
st_crs(p3) <- st_crs(net)

paths <- st_network_paths(net, from = p1, to = c(p2, p3))

plot(net, col = "grey")
paths %>%
  pull(node_paths) %>%
  walk(plot_path)
plot(c(p1, p2, p3), col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)

# Our network consists of several unconnected components.
with_graph(net, graph_component_count())

connected_net <- net %>%
  activate("nodes") %>%
  filter(group_components() == 1)

plot(net, col = colors[2])
plot(connected_net, cex = 1.1, lwd = 1.1, add = TRUE)

st_network_cost(net, from = c(p1, p2, p3), to = c(p1, p2, p3))

# Our network has 701 nodes.
with_graph(net, graph_order())

cost_matrix <- st_network_cost(net)
dim(cost_matrix)


# closest facility analysis -----------------------------------------------

# Select a random set of sites and facilities.
# We select random locations within the bounding box of the nodes.
set.seed(128)
hull <- net %>%
  activate("nodes") %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()

sites <- st_sample(hull, 50, type = "random")
facilities <- st_sample(hull, 5, type = "random")

# Blend the sites and facilities into the network to get better results.
# Also select only the largest connected component.
new_net <- net %>%
  activate("nodes") %>%
  filter(group_components() == 1) %>%
  st_network_blend(c(sites, facilities))

# Calculate the cost matrix.
# By default the weight column is used for edge weights.
# In our case this column contains the geographic lengths of the edges.
cost_matrix <- st_network_cost(new_net, from = sites, to = facilities)

# Find for each site which facility is closest.
closest <- facilities[apply(cost_matrix, 1, function(x) which(x == min(x))[1])]

# Create a line between each site and its closest facility, for visualization.
draw_lines <- function(sources, targets) {
  lines = mapply(
    function(a, b) st_sfc(st_cast(c(a, b), "LINESTRING"), crs = st_crs(net)),
    sources,
    targets,
    SIMPLIFY = FALSE
  )
  do.call("c", lines)
}

connections <- draw_lines(sites, closest)

plot(new_net, col = "grey")
plot(connections, col = colors[2], lwd = 2, add = TRUE)
plot(facilities, pch = 8, cex = 2, lwd = 2, col = colors[3], add = TRUE)
plot(sites, pch = 20, cex = 2, col = colors[2], add = TRUE)


# isochrones --------------------------------------------------------------

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

net <- activate(net, "nodes")

p <- net %>%
  st_geometry() %>%
  st_combine() %>%
  st_centroid()

iso <- net %>%
  filter(node_distance_from(st_nearest_feature(p, net), weights = time) <= 600)

iso_poly <- iso %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()

plot(net, col = "grey")
plot(iso_poly, col = NA, border = "black", lwd = 3, add = TRUE)
plot(iso, col = colors[2], add = TRUE)
plot(p, col = colors[1], pch = 8, cex = 2, lwd = 2, add = TRUE)

# not a very good isochrone!

# try improved isochrones -------------------------------------------------

isochrone_sf <- iso %>% 
  st_as_sf("edges") %>% 
  select(geometry) %>% 
  st_combine() %>% 
  st_buffer(dist = 50)

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), colour = "grey80") +
  geom_sf(data = isochrone_sf, colour = "blue", fill = "steelblue", alpha = 0.3) +
  geom_sf(data = st_as_sf(iso, "nodes"), colour = "firebrick", size = 0.5) +
  geom_sf(data = p, colour = "magenta", alpha = 0.8, size = 2) +
  theme_void()

# much better, but would be improved if it interpolated along outermost edges rather than
# contracting to outermost points


