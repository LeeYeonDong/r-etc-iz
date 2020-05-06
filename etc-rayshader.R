
#### raster tif파일

dem <- raster("D:/GIS/dem_01.tif")

extent(dem)
plot(dem)


dem_mat <- matrix(extract(dem, extent(dem), buffer = 1000), nrow = ncol(dem), ncol = nrow(dem))
dem_ambmat <- dem_mat %>% ambient_shade()


dem_plot <- dem_mat %>% 
  sphere_shade(sunangle = 45, texture = "imhof1") %>% 
  add_water(detect_water(dem_mat), color = "imhof1") %>%
  add_shadow(ray_shade(dem_mat, zscale = 3, maxsearch = 300), 0.5) %>%
  add_shadow(dem_ambmat, 0.5) %>%
  plot_3d(dem_mat, zscale = 10, fov = 0, theta = 135, zoom = 1, phi = 45, windowsize = c(1000, 800)) 

render_snapshot()

render_movie(filename = "dem_plot", 
             type = "orbit",
             phi = 45, 
             theta = 60)


####

ggdiamonds <- ggplot(diamonds) +
  stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)), 
                  geom = "polygon", 
                  n = 100,
                  bins = 10, 
                  contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

plot_gg(ggdiamonds, width = 5, 
        height = 5, 
        raytrace = FALSE, 
        preview = TRUE)

plot_gg(ggdiamonds, width = 5, 
        height = 5, 
        multicore = TRUE, 
        scale = 250, 
        zoom = 0.7, 
        theta = 10, 
        phi = 30, windowsize = c(1200, 1200))


render_movie(filename = "ggdiamonds", 
             type = "orbit",
             phi = 45, 
             theta = 60)

render_movie(filename = "ggdiamonds_oscillate", 
             type = "oscillate",
             phi = 45, 
             theta = 60)





