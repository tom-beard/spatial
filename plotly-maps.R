# Interactive plotly maps with OSM basemaps

# based on https://plotly.com/r/mapbox-layers/

library(plotly)

us_cities <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/us-cities-top-1k.csv")

base_fig <- us_cities %>%
  plot_ly(
    lat = ~lat,
    lon = ~lon,
    marker = list(color = "fuchsia"),
    type = 'scattermapbox',
    hovertext = us_cities[, "City"])

base_fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom = 2.5,
      center = list(lon = -88, lat = 34))) 
# In RStudio viewer window, plotly menu appears, but with blank background
# However, it works fine in Chrome

base_fig %>%
  layout(mapbox = list(
    style = "white-bg",
    zoom = 3,
    center = list(lon = -93 ,lat= 41),
    layers = list(list(
      below = 'traces',
      sourcetype = "raster",
      source = list("https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}")))))

base_fig %>%
  layout(
    mapbox = list(
      style = 'stamen-toner',
      zoom = 2.5,
      center = list(lon = -88, lat = 34))) 

