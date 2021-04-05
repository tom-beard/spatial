# Interactive plotly maps with OSM basemaps

# basemaps ---------------------------------------------------------------

# based on https://plotly.com/r/mapbox-layers/
# these examples work without mapbox tokens

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
    center = list(lon = -93, lat= 41),
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


# choropleths on basemaps -------------------------------------------------

# based on https://plotly.com/r/mapbox-county-choropleth/
# start new session

library(plotly)
library(rjson)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file = url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses = c(fips = "character"))
fig <- plot_ly() %>%
  add_trace(
    type = "choroplethmapbox",
    geojson = counties,
    locations = df$fips,
    z = df$unemp,
    colorscale = "Viridis",
    zmin = 0,
    zmax = 12,
    marker = list(line = list(
      width = 0),
      opacity = 0.5
    )
  )

fig %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      zoom = 2,
      center = list(lon = -95.71, lat = 37.09))
  )


# add interactivity? -------------------------------------------------------

fig_map <- fig %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      zoom = 2,
      center = list(lon = -95.71, lat = 37.09))
  )

# these aren't going to work as-is: need to delve deeper into
# https://plotly-r.com/client-side-linking.html#graphical-queries

hist <- add_histogram(
  fig,
  x = ~median, 
  histnorm = "probability density"
)
hist


subplot(time_series, hist, nrows = 2) %>%
  layout(barmode = "overlay", showlegend = FALSE) %>%
  highlight(
    dynamic = TRUE, 
    selectize = TRUE, 
    selected = attrs_selected(opacity = 0.3)
  )
