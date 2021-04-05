# Interactive plotly maps with OSM basemaps

# based on https://plotly.com/r/mapbox-layers/

library(plotly)

us_cities <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/us-cities-top-1k.csv")

fig <- us_cities 
fig <- fig %>%
  plot_ly(
    lat = ~lat,
    lon = ~lon,
    marker = list(color = "fuchsia"),
    type = 'scattermapbox',
    hovertext = us_cities[, "City"]) 
fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -88, lat = 34))) 

fig
# no result!
