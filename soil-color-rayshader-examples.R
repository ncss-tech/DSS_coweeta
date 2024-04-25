library(terra)
library(rayshader)

## TODO: use WCS: https://ncss-tech.github.io/AQP/soilDB/WCS-demonstration-01.html#Soil_Color

# temp location until WCS is in place
# CONUS soil color grid
# EPSG 5070
x <- rast('e:/working_copies/soil-color/conus-soil-colors/results/025cm-gNATSGO-highres.tif')

# code / color LUT
soilcolor.lut <- read.csv('e:/working_copies/soil-color/conus-soil-colors/results/unique-moist-color-LUT.csv')
soilcolor.lut$col <- rgb(soilcolor.lut$r, soilcolor.lut$g, soilcolor.lut$b, maxColorValue = 255)

# set color table for grid
coltab(x) <- soilcolor.lut[, c('id', 'r', 'g', 'b')]

# local DEM to crop
# UTM
e <- rast('grids/elev_pcs.tif')

# watershed
w <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# other thematic data for overlays
o <- rast('grids/SWI.tif')

# bbox
b <- as.polygons(ext(e))
crs(b) <- crs(e)

# transform BBOX -> 5070
b <- project(b, 'epsg:5070')

# crop soil color
x <- crop(x, b)

# check: OK
plot(x)
lines(b)

# transform back to UTM and resample to same extent / res as DEM (10m)
soilcolor <- project(x, e, method = 'near')

# check that color table has survived
has.colors(soilcolor)

# check: OK
plot(soilcolor, legend = FALSE, axes = FALSE, mar = c(1, 1, 1, 1))
lines(w, col = 'white')


# save as overlay image
ragg::agg_png(filename = 'overlay-soilcolor.png', width = ncol(soilcolor), height = nrow(soilcolor))
plot(soilcolor, legend = FALSE, axes = FALSE, maxcell = ncell(soilcolor), mar = c(0, 0, 0, 0))
lines(w, col = 'white')
dev.off()

ragg::agg_png(filename = 'overlay-SWI.png', width = ncol(o), height = nrow(o))
plot(o, col = hcl.colors(25, palette = 'mako'), legend = FALSE, axes = FALSE, maxcell = ncell(o), mar = c(0, 0, 0, 0))
lines(w, col = 'white')
dev.off()



# prepare DEM for rayshader
elmat <- raster_to_matrix(e)


# thematic map, as PNG, exact same dimensions
ov <- png::readPNG('overlay-SWI.png')


# compute shadows
raymat <- ray_shade(elmat, multicore = TRUE, progbar = TRUE, zscale = 0.1)
ambmat <- ambient_shade(elmat, multicore = TRUE, progbar = TRUE, zscale = 0.1)


## testing
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  # add_overlay(ov, alphalayer = 0.9) %>%
  plot_map()




# theta (z-axis rotation)
# phi (azimuth)

# camera parameters
.theta <- 160
.phi <- 40
.zoom <- 0.7
.fov <- 48

## output size
px.width <- 1200
px.height <- 800


elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  add_overlay(ov, alphalayer = 0.9) %>%
  plot_3d(elmat, zscale = 10, windowsize = c(px.width, px.height),
          baseshape = 'rectangle', lineantialias = TRUE,
          theta = .theta, phi = .phi, zoom = .zoom, fov = .fov)



## adjust until right
# render_camera(theta = 160, phi = 40, zoom = 0.7, fov = 48)

render_snapshot(
  filename = 'SWI-block-diagram.png', clear = TRUE, 
  instant_capture = TRUE
)



## do this after a session, to clear rgl device
rgl::close3d()


