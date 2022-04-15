library(raster)

## https://github.com/stevenpawley/Rsagacmd
library(Rsagacmd)

# initiate a saga object
# takes a while to "find" the binaries
saga <- saga_gis()

# DEM in projected coordinate system
e <- raster('grids/elev_pcs.tif')

## diurnal anisotropic heating index
dah <- saga$ta_morphometry$diurnal_anisotropic_heating(dem = e)

# quick check: OK
plot(dah)


## saga wetness index
# result is a list
swi <- saga$ta_hydrology$saga_wetness_index(dem = e, .verbose = TRUE)

# quick check: OK
plot(swi$twi)


## topographic ruggedness index
# radius in n. grid cells
tri <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem = e, radius = 5)

## vector ruggedness measure
# radius in n. grid cells
vri <- saga$ta_morphometry$vector_ruggedness_measure_vrm(dem = e, radius = 5)

## topographic position index
# search radius in meters
tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = e, radius_min = 0, radius_max = 300)

## morphometric protection index
# search radius in meters 
pi <- saga$ta_morphometry$morphometric_protection_index(dem = e, radius = 1000)

# extent is larger than DEM

## multi-scale TPI
multiscale.tpi <- saga$ta_morphometry$multi_scale_topographic_position_index_tpi(dem = e)


# check: OK
plot(tri)
plot(vri)
plot(tpi)
plot(pi)
plot(multiscale.tpi)



## export
writeRaster(dah, filename = 'grids/DAH.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(swi$twi, filename = 'grids/SWI.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)

writeRaster(tri, filename = 'grids/TRI.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(vri, filename = 'grids/VRI.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(tpi, filename = 'grids/TPI.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(pi, filename = 'grids/PI.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(multiscale.tpi, filename = 'grids/MTPI.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)



