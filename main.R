## Sequential steps used to prepare soil data and DEM / derivatives from official sources.
## 2023-10-05
## D.E. Beaudette


## TODO: 
# * convert to targets workflow if isn't too complex 
# * use FY24 soils data



# estimate GCS BBOX for project
source('get-project-BBOX.R')

## prepare soil data: spatial + tabular
source('01-prepare-soil-data.R')

## aggregate soil data


# old version, still a lot of useful ideas here
# source('02-aggregate-soil-data.R')

# custom soil definition files
source('02a-prepare-soil-def-files.R')


## Note: this one needs some more work
## soil maps from aggregate data
# source('03-prepare-soil-maps.R')


## various thematic maps from aggregate data
source('04-prepare-thematic-maps.R')



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





