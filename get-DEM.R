## Prepare a DEM for all subsequent use and derivatives from USGS 10m NED. No warping or resampling is performed at this time.
## 2023-10-05
## D.E. Beaudette

library(sf)
library(elevatr)

# using this vs. terra, as get_elev_raster() returns raster objects
library(raster)

# use watershed polygons for BBOX to request elevation data
x <- read_sf('vect/Coweeta_Hydrologic_Laboratory.shp')

# buffer to ensure that there are no truncated watersheds / accumulation
# as created by r.watershed
x.buff <- st_as_sf(st_buffer(st_as_sfc(st_bbox(x)), dist = 1000))

# convert to GCS WGS84 -> no transformation / warp will be applied to DEM
x.gcs <- st_transform(x, 4326)
x.buff.gcs <- st_transform(x.buff, 4326)

# requires sf collection (geometry + attributes)
# get DEM in GCS WGS84 (no warp/resample)
# use z = 14 for best available data
# result is a RasterLayer (not terra::SpatRaster)
e <- get_elev_raster(locations = x.buff.gcs, z = 12, clip = 'bbox')

# check: OK
plot(e)
plot(st_geometry(x.buff.gcs), add = TRUE, col = NA, lwd = 2)
plot(st_geometry(x.gcs), add = TRUE, col = NA)

## save for later

# GCS
writeRaster(e, file = 'grids/elev.tif', overwrite = TRUE, options=c("COMPRESS=LZW"))

# original PCS
write_sf(x.buff, 'vect/coweeta_boundary_buff.shp', delete_layer = TRUE)






