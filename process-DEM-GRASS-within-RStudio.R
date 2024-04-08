
# https://rsbivand.github.io/rgrass/articles/use.html





## open from stand-alone OSGeo4W shell


# "c:\Program Files\RStudio\rstudio.exe"


## only feasible solution on Windows:
# https://github.com/rsbivand/rgrass/issues/87

# * start OSGeo shell
# * start grass
#    - grass83 e:\GRASS\coweeta\PERMANENT
# start R from command line (RStudio won't work)
#    - R
# proceed from there 

## reset WD, as it will be in "program files..."
setwd('e:/working_copies/DSS_coweeta/')


library(terra)
library(rgrass)

# works!

(e <- rast('grids/elev_pcs.tif'))

.gb <- 'c:/OSGeo4W/apps/grass/grass83'
loc <- initGRASS(gisBase = .gb, home = tempdir(), SG  = e, override = TRUE)

execGRASS('g.region', flags = 'p')




