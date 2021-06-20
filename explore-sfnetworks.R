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
