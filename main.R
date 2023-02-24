##
##
##

# estimate GCS BBOX for project
source('get-project-BBOX.R')

## prepare soil data: spatial + tabular
source('01-prepare-soil-data.R')

## aggregate soil data


## soil maps from aggregate data




# get 10m DEM
source('get-DEM.R')

# GRASS GIS: warp DEM -> projected CRS
# GRASS GIS: DEM processing / derivatives
source('process-DEM-GRASS.R')

# SAGA GIS: DEM derivatives
source('process-DEM-SAGA.R')

# prepare raster soil survey and derived grids
source('prepare-RSS-data.R')





