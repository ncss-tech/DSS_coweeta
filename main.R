## Sequential steps used to prepare soil data and DEM / derivatives from official sources.
## 2023-10-05
## D.E. Beaudette


## TODO: 
# * convert to targets workflow if isn't too complex 
# * use FY24 RSS



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


## TODO: adapt to use soil definition summaries

## various thematic maps from aggregate data
source('04-prepare-thematic-maps.R')



## acquire DEM based on project BBOX, and generate derivatives

# get 10m DEM from USGS
source('get-DEM.R')

# warp, resample, crop, mask DEM for RHESSys 
# this ensures that the DEM, RSS, SSURGO grids are exactly the same
# NA cells and all
# use 'elev_pcs_tight_crop.tif' for model
source('DEM-tweaks.R')







# ----------------- none of this output is used by RHESSys -----------------



# GRASS GIS: warp/resampling of DEM -> projected CRS
# GRASS GIS: DEM processing / derivatives
source('process-DEM-GRASS.R')

# SAGA GIS: DEM derivatives
source('process-DEM-SAGA.R')




## no longer used, since RSS is available via WCS

# prepare raster soil survey and derived grids
# source('prepare-RSS-data.R')





