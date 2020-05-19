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


# multiple viewpoints -----------------------------------------------------

par(mfrow = c(2,2))
render_scene(scene, parallel = TRUE, width = 400, height = 400, lookfrom = c(7,1,7), samples = 1000)
render_scene(scene, parallel = TRUE, width = 400, height = 400, lookfrom = c(0,7,7), samples = 1000)
render_scene(scene, parallel = TRUE, width = 400, height = 400, lookfrom = c(-7,0,-7), samples = 1000)
render_scene(scene, parallel = TRUE, width = 400, height = 400, lookfrom = c(-7,7,7), samples = 1000)



# Cornell box -------------------------------------------------------------

par(mfrow = c(1, 1))

scene <- generate_cornell()
render_scene(scene,
             lookfrom = c(278,278,-800),
             lookat = c(278,278,0),
             aperture = 0, fov = 40, samples = 1000,
             ambient_light = FALSE, parallel = TRUE, width = 800, height = 800,
             clamp_value = 5)

tempfileplot <- tempfile()
png(filename = tempfileplot, height=1600, width=1600)
plot(iris$Petal.Length, iris$Sepal.Width, col = iris$Species, pch = 18, cex = 12)
dev.off()

image_array <- png::readPNG(tempfileplot)

generate_cornell() %>%
  add_object(ellipsoid(x = 555 / 2, y = 100, z = 555 / 2 ,
                       a = 50, b = 100, c = 50,
                       material = metal(color = "lightblue"))) %>%
  add_object(cube(x = 100, y = 130 / 2, z = 200,
                  xwidth = 130, ywidth = 130, zwidth = 130,
                  material = diffuse(checkercolor = "purple", checkerperiod = 30),
                  angle = c(0, 10, 0))) %>%
  add_object(pig(x = 100, y = 190, z = 200,
                 scale = 40, angle = c(0, 30, 0))) %>%
  add_object(sphere(x = 420, y= 555 / 8, z = 100,
                    radius = 555 / 8,
                    material = dielectric(color = "orange"))) %>%
  add_object(yz_rect(x = 0.01, y = 300, z = 555 / 2,
                     zwidth = 400, ywidth = 400,
                     material = diffuse(image = image_array))) %>%
  add_object(yz_rect(x = 555 / 2, y = 300, z = 555 - 0.01,
                     zwidth = 400, ywidth = 400,
                     material = diffuse(image = image_array),
                     angle = c(0, 90, 0))) %>%
  add_object(yz_rect(x = 555 - 0.01, y = 300, z = 555 / 2,
                     zwidth = 400, ywidth = 400,
                     material = diffuse(image = image_array),
                     angle = c(0, 180, 0))) %>%
  render_scene(lookfrom = c(278, 278, -800),
               lookat = c(278, 278, 0),
               aperture = 0, fov = 40, samples = 1000,
               ambient_light = FALSE, parallel = TRUE, width = 800, height = 800,
               clamp_value = 5)


# Venice with pig ---------------------------------------------------------

# this seems to crash Windows every time!
# try ewithout environment_light

tempfilehdr <- tempfile(fileext = ".hdr")
download.file("https://www.tylermw.com/data/venice_sunset_2k.hdr", tempfilehdr)

# Create star polygon
angles <- seq(0, 360, by = 36)
xx <- rev(c(rep(c(1, 0.5), 5), 1) * sinpi(angles / 180))
yy <- rev(c(rep(c(1, 0.5), 5), 1) * cospi(angles / 180))
star_polygon <- tibble::tibble(x = xx, y = yy)
hollow_star <- rbind(star_polygon, 0.8 * star_polygon)

generate_ground(material = diffuse(color = "grey20", checkercolor = "grey50", sigma = 90)) %>%
  add_object(sphere(material = metal())) %>%
  add_object(obj_model(y = -1, x = -1.8,
                       r_obj(),
                       angle = c(0, 135, 0),
                       material = diffuse(sigma = 90))) %>%
  add_object(pig(x = 1.8, y = -1.2,
                 scale = 0.5, angle = c(0, 90, 0),
                 diffuse_sigma = 90)) %>%
  add_object(extruded_polygon(hollow_star,
                              top = -0.5, bottom = -1, z = -2,
                              holes = nrow(star_polygon),
                              material = diffuse(color = "red", sigma = 90))) %>%
  # render_scene(parallel = TRUE, environment_light = tempfilehdr,
  render_scene(parallel = TRUE,
               width = 800, height = 800,
               fov = 70, clamp_value = 10, samples = 1000, aperture = 0.1,
               lookfrom = c(-0.9, 1.2, -4.5),
               lookat = c(0, -1, 0))


# simple purrr::map example --------------------------------------------------

library(tidyverse)

base_scene <- generate_ground() %>%
  add_object(sphere(y = 5, z = 5, x = 5, radius = 3,
                    material = light(intensity = 10)))

default_mat <- diffuse(color = "#ff0000", sigma = 100)
sphere_at <- function(y_pos) sphere(x = 0, y = y_pos, z = 0, radius = 0.2, material = default_mat)

balls <- seq(from = -1, to = 1, by = 0.5) %>%
  map_dfr(sphere_at)

scene <- base_scene %>% 
  add_object(balls)
  
render_scene(scene, parallel = TRUE, width = 800, height = 800, samples = 1000)
