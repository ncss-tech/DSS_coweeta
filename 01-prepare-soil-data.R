library(terra)
library(soilDB)
library(rasterVis)
library(viridisLite)
library(sf)


## Notes:
# * "Coweeta_Final_raster.tif" is a non-standard packaging of the RSS
# * grid cells are non-standard keys -> RAT -> [musym] -> mapunit -> [mukey]
# --> use FY23 RSS instead: 10m grid via WCS
# --> tabular RSS data via local download 

## Output Data (UTM z17):
# * RSS 10m grid
# * SSURGO 10m grid
# * SSURGO original polygons


# * DEM and derivatives will work on a larger area, for complete basin characterization




## start with Coweeta Laboratory Watersheds (outline)
# UTM z17 NAD83
o <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# extend by 200m
# ensure a tight crop without losing anything
b <- buffer(o, width = 200)


## get latest RSS via WCS
# cell values are map unit keys
rss <- mukey.wcs(b, db = 'rss', res = 10)

# warp to UTM z17
rss <- project(rss, crs(o), method = 'near')

# crop to watershed boundaries + 200m
rss <- crop(rss, b)

# check: ok
plot(rss)
lines(o)

# save for later, this includes .xml file with RAT
writeRaster(rss, filename = 'grids/rss_utm.tif', overwrite = TRUE)


## get latest SSURGO via SDA
# using BBOX of slightly expanded watershed outline
# nearby: https://casoilresource.lawr.ucdavis.edu/gmap/?loc=35.02427,-83.47449,z16
# NC113, 1:12k
# WGS84
s <- SDA_spatialQuery(b, what = 'mupolygon', geomIntersection = TRUE)

# transform to UTM z17
s <- project(s, crs(o))

# save polygons for later
writeVector(s, filename = 'vect/SSURGO-MU.shp', overwrite = TRUE)

# check: ok
plot(rss)
lines(s)

# rasterize using RSS grid system
# cell values are mukey
s.rast <- rasterize(s, rss, field = 'mukey')

# crop/mask to RSS data
s.rast <- mask(crop(s.rast, rss), rss)

# init RAT
s.rast <- as.factor(s.rast)

# check: ok
plot(s.rast)
lines(s)

# save for later, this includes .xml file with RAT
writeRaster(s.rast, filename = 'grids/ssurgo_utm.tif', overwrite = TRUE)



## RSS spatial data
# cell values are map unit keys
# read as grid + RAT
r <- rast('Raster soil survey/Coweeta_Final_raster.tif')

# RAT: raster attribute table
rat <- read.dbf('Raster soil survey/Coweeta_Final_raster.tif.vat.dbf', as.is = TRUE)
names(rat) <- tolower(names(rat))

