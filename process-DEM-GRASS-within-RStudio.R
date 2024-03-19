
# https://rsbivand.github.io/rgrass/articles/use.html


library(terra)
library(rgrass)

(e <- rast('grids/elev_pcs.tif'))

# .gb <- 'c:/Program Files/QGIS 3.32.3/apps/grass/grass83'
.gb <- 'c:/PROGRA~1/QGIS33~1.3/apps/grass/grass83'
loc <- initGRASS(gisBase = .gb, home = 'e:/temp/GRASS', SG  = r, override = TRUE)

