library(terra)

# UTM z17
e <- rast('grids/elev_pcs.tif')
w <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# WGS84
a <- vect('c:/Users/Dylan.Beaudette/Downloads/at_latest.shp')
a <- project(a, crs(e))
a <- crop(a, e)

# check
plot(e, axes = FALSE, legend = FALSE, col = viridis::mako(25))
lines(w, col = 'white')
lines(a, lwd = 2)

# save cropped version
writeVector(a, filename = 'vect/AT.shp', overwrite = TRUE)
