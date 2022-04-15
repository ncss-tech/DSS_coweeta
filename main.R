##
##
##


# get 10m DEM
source('get-DEM.R')

# GRASS GIS: warp DEM -> projected CRS
# GRASS GIS: DEM processing / derivatives
source('process-DEM-GRASS.R')

# SAGA GIS: DEM derivatives
source('process-DEM-SAGA.R')





