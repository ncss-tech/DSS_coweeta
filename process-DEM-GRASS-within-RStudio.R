
# https://rsbivand.github.io/rgrass/articles/use.html



## hangs with white screen
# "c:\Program Files\RStudio\bin\rstudio.exe"

## works as expected
# "c:\Program Files\r\R-4.3.1\bin\R.exe"


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

# check: boundary should be in there
execGRASS('g.list', parameters = list(type = 'vect'))


## the GRASS from within R isn't going to work

(e <- rast('grids/elev_pcs.tif'))

.gb <- 'c:/OSGeo4W/apps/grass/grass83'
loc <- initGRASS(gisBase = .gb, home = tempdir(), SG  = e, override = TRUE)

