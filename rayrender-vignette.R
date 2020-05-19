# Basic vignette from https://www.rayrender.net/
# Not exactly spatial, but useful for rayshader


library(rayrender)

# initial scene -----------------------------------------------------------

scene <- generate_ground() %>%
  add_object(sphere(material = diffuse(color = "#ff5555")))
render_scene(scene, parallel = TRUE, width = 800, height = 800, samples = 1000)


# lighting ----------------------------------------------------------------

scene <- generate_ground() %>%
  add_object(sphere(material = diffuse(color = "#ff5555", sigma = 100))) %>%
  add_object(sphere(y = 5, z = 5, x = 5, radius = 3,
                    material = light(intensity = 10)))
render_scene(scene, parallel = TRUE, width = 800, height = 800, samples = 1000)


# object model ------------------------------------------------------------

scene <- generate_ground() %>%
  add_object(sphere(material = diffuse(color = "#ff5555"))) %>%
  add_object(obj_model(r_obj(), y = -0.4, z = 0.9, scale_obj = 0.6)) %>%
  add_object(sphere(y = 5, z = 5, x = 5, radius = 3,
                    material = light(intensity = 5)))
render_scene(scene, parallel = TRUE, width = 800, height = 800, samples = 1000)
