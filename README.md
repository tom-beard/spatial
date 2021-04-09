# spatial
Utilities for geospatial processing and visualisation

Further ideas and packages to pursue:

* `stplanr::SpatialLinesNetwork()` and `igraph` to combine graph theory and geography (https://bookdown.org/robinlovelace/geocompr/transport.html#route-networks)
* Interactive client-side dashboards with `flexdashboard` and `crosstalk` (https://nicar.r-journalism.com/docs/crosstalk-flexdashboard-leaflet-datatable/, or see example at https://matt-dray.github.io/earl18-crosstalk/04_leaflet-flexdash-dt-crosstalk.html)
* Plotly for interactive leaflet-like maps (https://plotly.com/r/mapbox-layers/). Can it do brushing etc within Plotly? Including polygons?
* Processing OSM layers to break up polygon roads into polylines, get sea/ocean geometry etc. Try https://cran.r-project.org/web/packages/osmplotr/? (Tried this: osmplotr::osmlines2poly() doesn't work)
* WebGL-based vis of large spatial data with deck.gl (https://github.com/crazycapivara/deckgl)
* crop network to SA1 and find graph-theoretic or OTP routed "centre" of that.
