## Generate derivatives from a warped/resampled DEM. The NAD83 / UTM zone 17N (EPSG:26917) projected coordinate reference system is used.
## 2023-10-05
## D.E. Beaudette


##
## Instructions for a happy R--GRASS experience
## 

# 0. create a GRASS location/mapset in GRASS, do this by starting GRASS manually
#    I typically setup dummy location/mapset in GCS WGS84
# 
#    E:\GRASS\gcs_wgs84\PERMANENT

# 1. open OSGeo4W-shell

# 2. start GRASS GIS, find database / location / mapset
#
# adjust paths as needed
# "c:\Program Files\QGIS 3.22.5\bin\grass78.bat" e:\GRASS\gcs_wgs84\PERMANENT

# 3. start RStudio from GRASS console
# "c:\Program Files\RStudio\bin\rstudio.exe"

# 4. interact with GRASS in GRASS GUI / console or within R using rgrass6 package

## reset WD, as it will be in "program files..."
setwd('e:/working_copies/DSS_coweeta/')


# https://cran.r-project.org/web/packages/rgrass/vignettes/use.html
library(rgrass)

## import elevation into previously created GCS WGS84 location/mapset
# https://grass.osgeo.org/grass79/manuals/r.in.gdal.html
execGRASS('r.in.gdal', flags = c('overwrite'), parameters = list(input = 'grids/elev.tif', output = 'coweeta_elev'))


## import expanded watershed boundary into new PCS location/mapset
## CRS and extent are based on this vector layer
# https://grass.osgeo.org/grass79/manuals/v.in.ogr.html
execGRASS('v.in.ogr', flags = c('overwrite'), parameters = list(input = 'vect', layer = 'coweeta_boundary_buff', output = 'boundary', type = 'boundary', location = 'coweeta'))

## exit, and restart into this new location/mapset

# "c:\Program Files\QGIS 3.22.5\bin\grass78.bat" e:\GRASS\coweeta\PERMANENT

# "c:\Program Files\RStudio\bin\rstudio.exe"

## reset WD, as it will be in "program files..."
setwd('e:/working_copies/DSS_coweeta/')

library(rgrass)

library(sf)
library(terra)

library(rasterVis)
library(viridisLite)
library(RColorBrewer)



# check: boundary should be in there
execGRASS('g.list', parameters = list(type = 'vect'))

## setup region / resolution
# https://grass.osgeo.org/grass79/manuals/g.region.html
execGRASS('g.region', flags = c('a', 'p'), parameters = list(vector = 'boundary', res = '10'))

## warp DEM to local PCS
# https://grass.osgeo.org/grass79/manuals/r.proj.html
# extent / resolution is set by the local location/mapset
execGRASS('r.proj', flags = c('overwrite'), parameters = list(location = 'gcs_wgs84', input = 'coweeta_elev', output = 'elev', method = 'bicubic'))

# check
execGRASS('g.list', parameters = list(type = 'rast'))


## contours for EDA


## watershed modeling
# https://grass.osgeo.org/grass79/manuals/r.watershed.html
# threshold is in number of cells
# note that accumulation is negative for cells affected by out-of-extent cells
execGRASS(
  cmd = 'r.watershed', 
  flags = c('overwrite', 'b'), 
  parameters = list(
    elevation = 'elev',
    threshold = 1000,
    basin = 'basins',
    accumulation = 'acc',
    drainage = 'drain_dir'
    
  )
)

# files are visible now
execGRASS('g.list', parameters = list(type = 'rast'))

## extract streams using accumulation map
# https://grass.osgeo.org/grass78/manuals/r.stream.extract.html
# threshold is in number of cells
# accumulation map from r.watershed used here
execGRASS(
  cmd = 'r.stream.extract', 
  flags = c('overwrite'), 
  parameters = list(
    elevation = 'elev',
    accumulation = 'acc',
    threshold = 1000,
    stream_raster = 'streams',
    stream_vector = 'streams'
  )
)


## alternative flow algorithm
# https://grass.osgeo.org/grass78/manuals/r.flow.html
execGRASS(
  cmd = 'r.flow', 
  flags = c('overwrite', '3'), 
  parameters = list(
    elevation = 'elev',
    flowaccumulation = 'flowacc',
    flowline = 'flowline'
  )
)


# list output
execGRASS('g.list', parameters = list(type = 'vect'))
execGRASS('g.list', parameters = list(type = 'rast'))



## check results in R, more convenient

# load raster / vector data into sp / raster objects
# GRASS raster --> terra::spatRaster
# GRASS vector --> terra::spatVector

b <- read_RAST('basins')
s <- read_VECT('streams')
# note special syntax, consider adding CATS above
fl <- read_VECT('flowline', flags = 'c')
d <- read_RAST('drain_dir')
a <- read_RAST('acc')
fa <- read_RAST('flowacc')

# remove 0-values
fa[fa <= 0] <- NA


# # note that some values are 0
# table(values(d))
# 
# # remove those
# d[d < 0] <- NA

# convert to factor
.uvals <- unlist(unique(d))
levels(d) <- data.frame(ID = .uvals, name  = .uvals)




# load original watershed boundaries
x <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# watershed areas + stream network
plot(b)
lines(s)

plot(b)
lines(fl)

# original watershed boundaries
plot(b)
lines(x)

# flow direction + stream network
plot(d)
lines(s)

# note: negative accumulation values -> unreliable flow from off-grid
a.neg <- a < 0

plot(a.neg, main = 'Negative Flow Accumulation (r.watershed)')
lines(x)

# plot log10-transformed values, negative values are discarded
levelplot(
  a, 
  scales = list(draw = FALSE), 
  margin = FALSE, 
  zscaleLog = 10, 
  main = 'Flow Accumulation (r.watershed)',
  panel = function(...) {
    panel.levelplot(...)
    sp::sp.polygons(as(x, 'Spatial'), col = 'white', lwd = 1)
  }
) 


# plot flow direction
levelplot(
  d, 
  # att = 'ID',
  scales = list(draw = FALSE), 
  col.regions = brewer.pal(8, 'Spectral'),
  margin = FALSE, 
  main = 'Flow Direction (r.watershed)',
  panel = function(...) {
    panel.levelplot(...)
    sp::sp.polygons(as(x, 'Spatial'), col = 'black', lwd = 1)
  }
) 


# plot log10-transformed values, 0's have been set to NA
levelplot(
  fa,
  scales = list(draw = FALSE),
  margin = FALSE,
  zscaleLog = 10,
  main = 'Flow Accumulation (r.watershed)',
  panel = function(...) {
    panel.levelplot(...)
    sp::sp.polygons(as(x, 'Spatial'), col = 'white', lwd = 1)
  }
)



## export from GRASS directly to GeoTiff
# https://grass.osgeo.org/grass79/manuals/r.out.gdal.html
# paths are relative to the working directory in R
execGRASS(
  cmd = 'r.out.gdal', 
  flags = c('overwrite', 'c', 'm'), 
  parameters = list(
    input = 'elev',
    output = 'grids/elev_pcs.tif',
    format = 'GTiff',
    createopt = 'COMPRESS=LZW'
  )
)

# check: looks right
gdal_utils(util = 'info', source = 'grids/elev_pcs.tif')

## export from R, in this case grids we have modified
## the same kind of modifications can be done in GRASS
## and are typically much more efficient
writeRaster(d, file = 'grids/drain_dir.tif', overwrite = TRUE)
writeRaster(fa, file = 'grids/flowacc.tif', overwrite = TRUE)

write_sf(st_as_sf(fl), dsn = 'vect/flowlines.shp', overwrite = TRUE)



# double-check on data type: CELL --> signed integer, Int16
execGRASS('r.info', parameters = list(map = 'drain_dir'))

execGRASS(
  cmd = 'r.out.gdal',
  flags = c('overwrite', 'c', 'm'),
  parameters = list(
    input = 'drain_dir',
    output = 'grids/drain_dir.tif',
    format = 'GTiff',
    createopt = 'COMPRESS=LZW'
  )
)

# ok
gdal_utils(util = 'info', source = 'grids/drain_dir.tif')





