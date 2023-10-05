## 
##
##


## TODO: convert to targets workflow if isn't too complex 

# estimate GCS BBOX for project
source('get-project-BBOX.R')

## prepare soil data: spatial + tabular
source('01-prepare-soil-data.R')

## aggregate soil data
source('02-aggregate-soil-data.R')

## soil maps from aggregate data
source('03-prepare-soil-maps.R')


## acquire DEM based on project BBOX, and generate derivatives

# get 10m DEM from USGS
source('get-DEM.R')

# GRASS GIS: warp/resampling of DEM -> projected CRS
# GRASS GIS: DEM processing / derivatives
source('process-DEM-GRASS.R')

# SAGA GIS: DEM derivatives
source('process-DEM-SAGA.R')


####### Need to update the following scripts to use the new soil data #########






# prepare raster soil survey and derived grids
source('prepare-RSS-data.R')





