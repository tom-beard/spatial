# deckgl experiments

# quickstart --------------------------------------------------------------

# based on https://crazycapivara.github.io/deckgl/articles/deckgl.html

library(deckgl)

deckgl() # starts a viewer pane

deckgl() %>% 
  add_basemap() # no basemap visible in viewer pane: need to click "Show in new window" to open in browser
# this might be an RStudio issue, solved in 1.3.1054: https://github.com/rstudio/rstudio/issues/7152

# Grid layer example
data("sf_bike_parking")

props <- list(
  extruded = TRUE,
  cellSize = 200,
  elevationScale = 4,
  getPosition = ~lng + lat,
  tooltip = "Count: {{count}}"
)

deckgl(zoom = 11, pitch = 45) %>%
  add_basemap() %>%
  add_grid_layer(
    data = sf_bike_parking,
    properties = props
  )

# custom deckgl() call
deckgl(zoom = 11, pitch = 45, latitude = -41.3, longitude = 174.77) %>% # Wellington
  add_basemap()


# layer example -----------------------------------------------------------

# from https://crazycapivara.github.io/deckgl/reference/add_arc_layer.html

data("bart_segments")

properties <- list(
  getWidth = 12,
  getSourcePosition = ~from_lng + from_lat,
  getTargetPosition = ~to_lng + to_lat,
  getSourceColor = JS("d => [Math.sqrt(d.inbound), 140, 0]"),
  getTargetColor = JS("d => [Math.sqrt(d.outbound), 140, 0]"),
  tooltip = use_tooltip(
    html = "{{from_name}} to {{to_name}}",
    style = "background: steelBlue; border-radius: 5px;"
  )
)

deck <- deckgl(zoom = 10, pitch = 35) %>%
  add_arc_layer(data = bart_segments, properties = properties) %>%
  add_control("Arc Layer", "top-left") %>%
  add_basemap()
deck


# basemap choice ----------------------------------------------------------

deckgl(zoom = 11, pitch = 45, latitude = -41.3, longitude = 174.77) %>% # Wellington
  add_basemap(style = use_carto_style("positron"))
