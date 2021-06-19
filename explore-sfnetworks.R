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


