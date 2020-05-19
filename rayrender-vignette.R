# Basic vignette from https://www.rayrender.net/
# Not exactly spatial, but useful for rayshader


library(rayrender)

# initial scene -----------------------------------------------------------

scene <- generate_ground() %>%
  add_object(sphere(material = diffuse(color = "#ff5555")))
render_scene(scene, parallel = TRUE, width = 800, height = 800, samples = 1000)
