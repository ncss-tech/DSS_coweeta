library(raster)
library(sp)
library(sf)


x <- raster('Raster soil survey/Coweeta_Final_raster.tif')
bb <- st_bbox(x)
bb <- st_as_sfc(bb)

bb <- st_transform(bb, 4326)

# st_centroid(bb)
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=35.04758,-83.45026


st_write(bb, dsn = 'vect/bbox.shp', overwrite = TRUE)
