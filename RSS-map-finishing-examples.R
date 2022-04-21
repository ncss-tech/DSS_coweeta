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



execGRASS('r.in.gdal', flags = c('overwrite'), parameters = list(input = 'grids/rss_utm.tif', output = 'rss', location = 'utm'))

## switch to PCS location/mapset
initGRASS(gisBase = grass_path,
          home = td,
          gisDbase = td, location = "utm",
          mapset = "PERMANENT", override = TRUE)


execGRASS('g.region', flags = c('a', 'p'), parameters = list(raster = 'rss'))
execGRASS('r.info', parameters = list(map = 'rss'))

# https://grass.osgeo.org/grass80/manuals/r.reclass.area.html
execGRASS('r.reclass.area', flags = c('d', 'overwrite'), parameters = list(input = 'rss', output = 'rss_c', mode = 'lesser', method = 'rmarea', value = 0.5))

x <- raster(readRAST('rss_c'))
x <- ratify(x)

levelplot(
  x, 
  att = 'ID',
  scales = list(draw = FALSE), 
  col.regions = viridis,
  margin = FALSE, 
  main = 'Map Finishing (1ha)'
) 

writeRaster(x, file = 'grids/rss_gen.tif', datatype = 'INT1U', options = 'COMPRESS=LZW', overwrite = TRUE)









