 library(ambient)
 library(tidyverse)
 
# examples ----------------------------------------------------------------

# from https://github.com/thomasp85/ambient

set.seed(1234)
simplex <- noise_simplex(c(500, 500),
                         pertubation = 'normal',
                         pertubation_amplitude = 40)
plot(as.raster(normalise(simplex)))

combo <- long_grid(x = seq(0, 10, length.out = 1000), 
          y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_simplex(x, y) / 2, 
    y1 = y + gen_simplex(x, y) / 2,
    worley = gen_worley(x, y, value = 'distance', seed = 5),
    worley_frac = fracture(gen_worley, ridged, octaves = 8, x = x, y = y, 
                           value = 'distance', seed = 5),
    full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1))
  )

combo %>% plot(full)
combo %>% plot(worley_frac)


# from help examples ------------------------------------------------------

grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
grid$circles <- gen_spheres(grid$x, grid$y)
grid$cylinders <- gen_spheres(grid$x)

plot(grid, circles)
plot(grid, cylinders)
