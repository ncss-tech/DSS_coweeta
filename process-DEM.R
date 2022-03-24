library(sf)
library(raster)

library(link2GI)
library(rgrass7)

# for now, force rgrass7 to use the sp-interface
library(sp)
use_sp()

library(rasterVis)
library(viridisLite)
library(RColorBrewer)

## https://github.com/stevenpawley/Rsagacmd
library(Rsagacmd)

# initiate a saga object
saga <- saga_gis()

# DEM in projected coordinate system
e <- raster('grids/elev_pcs.tif')

dah <- saga$ta_morphometry$diurnal_anisotropic_heating(dem = e)

# result is a list
swi <- saga$ta_hydrology$saga_wetness_index(dem = e, .verbose = TRUE)


## TODO:
# TRI
# VRI



## export
writeRaster(dah, filename = 'grids/DAH.tif', options = c('COMPRESS=LZW'))
writeRaster(swi$twi, filename = 'grids/SWI.tif', options = c('COMPRESS=LZW'))


## GRASS, internal connection (start RStudio from GRASS shell)
# https://github.com/rsbivand/rgrass7


## GRASS, external connection

# ideas from: https://geocompr.robinlovelace.net/gis.html#rgrass

# kind of slow
gr <- findGRASS() 

# find a GRASS 7 installation, and use the first one
ind <- grep("7", gr$version)[1]

# next line of code only necessary if we want to use GRASS as installed by 
# OSGeo4W. Among others, this adds some paths to PATH, which are also needed
# for running GRASS.
link2GI::paramGRASSw(gr[ind, ])

grass_path <- ifelse(test = !is.null(gr$installation_type) && 
                       gr$installation_type[ind] == "osgeo4W",
                     yes = file.path(gr$instDir[ind], "apps/grass", gr$version[ind]),
                     no = gr$instDir)



## temporary location for HOME and GRASS "database"
td <- tempdir()

# spurious error in print.gmeta() doesn't affect subsequent commands
# https://github.com/rsbivand/rgrass7/issues/31

# throw-away location/mapset used to bootstrap the process
initGRASS(gisBase = grass_path,
          home = td,
          gisDbase = td, location = "garbage",
          mapset = "PERMANENT", override = TRUE)


## import elevation into a new GCS WGS84 location/mapset
# https://grass.osgeo.org/grass79/manuals/r.in.gdal.html
execGRASS('r.in.gdal', flags = c('overwrite'), parameters = list(input = 'grids/elev.tif', output = 'elev', location = 'gcs'))


## import expanded watershed boundary into new PCS location/mapset
# https://grass.osgeo.org/grass79/manuals/v.in.ogr.html
execGRASS('v.in.ogr', flags = c('overwrite'), parameters = list(input = 'vect', layer = 'coweeta_boundary_buff', output = 'boundary', type = 'boundary', location = 'coweeta'))



## switch to PCS location/mapset
initGRASS(gisBase = grass_path,
          home = td,
          gisDbase = td, location = "coweeta",
          mapset = "PERMANENT", override = TRUE)

# check: boundary should be in there
execGRASS('g.list', parameters = list(type = 'vect'))

## setup region / resolution
# https://grass.osgeo.org/grass79/manuals/g.region.html
execGRASS('g.region', flags = c('a', 'p'), parameters = list(vector = 'boundary', res = '10'))

## warp DEM to local PCS
# https://grass.osgeo.org/grass79/manuals/r.proj.html
# extent / resolution is set by the local location/mapset
execGRASS('r.proj', flags = c('overwrite'), parameters = list(location = 'gcs', input = 'elev', output = 'elev', method = 'bicubic'))

# check
execGRASS('g.list', parameters = list(type = 'rast'))


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
b <- raster(readRAST('basins'))
s <- readVECT('streams')
d <- raster(readRAST('drain_dir'))
a <- raster(readRAST('acc'))
fa <- raster(readRAST('flowacc'))

# remove 0-values
fa[fa <= 0] <- NA


# note that some values are 0
table(d[])

# remove those
d[d < 0] <- NA

# convert to factor
d <- ratify(d)




# load original watershed boundaries
x <- read_sf('Coweeta and Hubbard Brook Shapefiles/Coweeta_Hydrologic_Laboratory.shp')
x <- as(x, 'Spatial')

# watershed areas + stream network
plot(b)
plot(s, add = TRUE)

# original watershed boundaries
plot(b)
plot(x, add = TRUE)


# flow direction + stream network
plot(d)
plot(s, add = TRUE)



# note: negative accumulation values -> unreliable flow from off-grid
a.neg <- a < 0
a.neg <- ratify(a.neg)

levelplot(
  a.neg, 
  att = 'ID', 
  scales = list(draw = FALSE), 
  margin = FALSE, 
  main = 'Negative Flow Accumulation (r.watershed)', 
  col.regions = c('white', 'royalblue'),
  panel = function(...) {
    panel.levelplot(...)
    sp.polygons(x, col = 'black', lwd = 1)
  }
)

# plot log10-transformed values, negative values are discarded
levelplot(
  a, 
  scales = list(draw = FALSE), 
  margin = FALSE, 
  zscaleLog = 10, 
  main = 'Flow Accumulation (r.watershed)',
  panel = function(...) {
    panel.levelplot(...)
    sp.polygons(x, col = 'white', lwd = 1)
  }
) 

# plot flow direction
levelplot(
  d, 
  att = 'ID',
  scales = list(draw = FALSE), 
  col.regions = brewer.pal(8, 'Spectral'),
  margin = FALSE, 
  main = 'Flow Direction (r.watershed)',
  panel = function(...) {
    panel.levelplot(...)
    sp.polygons(x, col = 'black', lwd = 1)
  }
) 


# # plot log10-transformed values, 0's have been set to NA
# levelplot(
#   fa, 
#   scales = list(draw = FALSE), 
#   margin = FALSE, 
#   zscaleLog = 10,
#   main = 'Flow Accumulation (r.watershed)',
#   panel = function(...) {
#     panel.levelplot(...)
#     sp.polygons(x, col = 'white', lwd = 1)
#   }
# ) 



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
gdalUtils::gdalinfo('grids/elev_pcs.tif')


## export from R, in this case grids we have modified
## the same kind of modifications can be done in GRASS
## and are typically much more efficient
writeRaster(d, file = 'grids/drain_dir.tif', options = 'COMPRESS=LZW', overwrite = TRUE)





# # double-check on data type: CELL --> signed integer, Int16
# execGRASS('r.info', parameters = list(map = 'drain_dir'))
# 
# execGRASS(
#   cmd = 'r.out.gdal', 
#   flags = c('overwrite', 'c', 'm'), 
#   parameters = list(
#     input = 'drain_dir',
#     output = 'grids/drain_dir.tif',
#     format = 'GTiff',
#     createopt = 'COMPRESS=LZW'
#   )
# )
# 
# # ok
# gdalUtils::gdalinfo('grids/drain_dir.tif')
# 
