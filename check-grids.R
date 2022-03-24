library(raster)
library(sf)
library(rasterVis)
library(viridisLite)


x <- raster('grids/beam_rad_sum_mj.tif')

# Coweeta watersheds
b <- read_sf('vect/Coweeta_Hydrologic_Laboratory.shp')
b <- st_transform(b, crs(x))


levelplot(
  x, 
  scales = list(draw = FALSE), 
  col.regions = inferno,
  margin = FALSE, 
  main = 'Annual Beam Radiance (MJ/m^2)\nGRASS GIS r.sun',
  panel = function(...) {
    panel.levelplot(...)
    sp.polygons(as(b, 'Spatial'), col = 'black', lwd = 1)
  }
) 






