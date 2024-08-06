##
##
##


library(terra)

# original RSS, 10m resolution
# note that extent is not aligned to integer coords
rss <- rast('grids/rss_utm.tif')

# DEM, native CRS and extent, from USGS holdings
# roughly 10m resolution
e <- rast('grids/elev.tif')

# Coweeta outline + watersheds
# do not use this for alignment, just overview
o <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# warp DEM to exact grid origin + resolution as RSS
ee <- project(e, rss, method = 'cubicspline')

# crop + MAS DEM to RSS
# this is important because the RSS doesn't fully cover some watersheds
# truncating the DEM is the only solution
ee <- crop(ee, rss, mask = TRUE)

# check, note missing pixels in WS7
plot(ee, col = hcl.colors(50))
lines(o)

# save for later, mostly for use by RHESSys
writeRaster(ee, filename = 'grids/elev_pcs_tight_crop.tif', overwrite = TRUE)

