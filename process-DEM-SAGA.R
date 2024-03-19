## 2024-03-19: latest Rsagacmd, slight changes in syntax, now uses terra objects

library(terra)

## https://github.com/stevenpawley/Rsagacmd
library(Rsagacmd)

# initiate a saga object
# had to manually specify path, YMMV
saga <- saga_gis(saga_bin = 'C:/Program Files/SAGA/saga_cmd.exe')

# DEM in projected coordinate system
e <- rast('grids/elev_pcs.tif')

## diurnal anisotropic heating index
dah <- saga$ta_morphometry$diurnal_anisotropic_heat(dem = e)

# quick check: OK
plot(dah)


## saga wetness index
# result is a list
swi <- saga$ta_hydrology$saga_wetness_index(dem = e, .verbose = TRUE)

# quick check: OK
plot(swi$twi)
plot(swi$area_mod)

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
plot(tri, col = hcl.colors(100, palette = 'mako'))
plot(vri, col = hcl.colors(100, palette = 'mako'))
plot(tpi, col = hcl.colors(100, palette = 'mako'))
plot(pi, col = hcl.colors(100, palette = 'mako'))
plot(multiscale.tpi, col = hcl.colors(100, palette = 'mako'))


## MRVBF
mrvbf <- saga$ta_morphometry$multiresolution_index_of_valley_bottom_flatness_mrvbf(dem = e)

plot(mrvbf$mrvbf, col = hcl.colors(100, palette = 'mako'))
plot(mrvbf$mrrtf, col = hcl.colors(100, palette = 'mako'))


## export
writeRaster(dah, filename = 'grids/DAH.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(swi$twi, filename = 'grids/SWI.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)

writeRaster(tri, filename = 'grids/TRI.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(vri, filename = 'grids/VRI.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(tpi, filename = 'grids/TPI.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(pi, filename = 'grids/PI.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(multiscale.tpi, filename = 'grids/MTPI.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)

writeRaster(mrvbf$mrvbf, filename = 'grids/MRVBF.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)
writeRaster(mrvbf$mrrtf, filename = 'grids/MRRTF.tif', gdal = c('COMPRESS=LZW'), overwrite = TRUE)

