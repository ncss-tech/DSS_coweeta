library(aqp)
library(soilDB)
library(raster)
library(rasterVis)
library(viridisLite)
library(sf)

# phasing these out eventually
library(sp)
library(rgdal)

# boundary via watersheds
x <- read_sf('vect/Coweeta_Hydrologic_Laboratory.shp')

# reduce to single polygon
x <- st_union(x)

# get 30m gSSURGO mukey grid here
mu <- mukey.wcs(aoi = x, db = 'gssurgo')


# get SSURGO polygons
# nearby: https://casoilresource.lawr.ucdavis.edu/gmap/?loc=35.02427,-83.47449,z16
# NC113, 1:12k
# WGS84
mu.poly <- SDA_spatialQuery(x, what = 'mupolygon', geomIntersection = TRUE)

# save a copy
st_write(
  st_transform(mu.poly, st_crs(x)),
  dsn = 'vect/SSURGO-MU.shp', append = FALSE
)



# unique map unit keys
ll <- levels(mu)[[1]]

# map unit keys
levelplot(
  mu, 
  att = 'ID', 
  margin = FALSE, 
  colorkey = FALSE, 
  col.regions = viridis, 
  scales = list(draw = FALSE)
)


## Note: some of these map units are dominated by non-soil components
# Dominant Component (Numeric) -> NODATA
# Weighted Average -> use any available data

# get thematic data from SDA
# dominant component
# depth-weighted average
# sand, silt, clay, AWC (RV)
p <-  get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r", "awc_r"),
                       method = "WEIGHTED AVERAGE", 
                       mukeys = ll$ID,
                       top_depth = 0,
                       bottom_depth = 25)

head(p)

# re-create raster attribute table with aggregate soil properties
rat <- merge(ll, p, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# re-pack RAT
levels(mu) <- rat

# convert raster + RAT --> stack of values
s <- deratify(mu, att = c("sandtotal_r", "silttotal_r", "claytotal_r", "awc_r"))

# graphical check
levelplot(
  s[[1:3]], 
  main = 'Sand, Silt, Clay (RV) 0-25cm\nWeighted Average',
  margin = FALSE, 
  scales = list(draw = FALSE), 
  col.regions = viridis
)

levelplot(
  s[[4]], 
  main = 'AWC (RV) 0-25cm\nWeighted Average',
  margin = FALSE, 
  scales = list(draw = FALSE), 
  col.regions = viridis
)



# convert to a representative soil texture class
txt.lut <- read.csv('http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/texture_2550.csv')

# make a copy
texture_025 <- s[[1]]

# note: soil textures that aren't present are dropped from factor levels
texture_025[] <- ssc_to_texcl(sand = s$sandtotal_r[], clay = s$claytotal_r[])

# extract RAT
rat <- levels(texture_025)[[1]]

# add colors
rat <- merge(rat, txt.lut, by.x = 'VALUE', by.y = 'class', all.x = TRUE, sort = FALSE)

# fix column order
rat <- rat[, c('ID', 'VALUE', 'hex', 'names')]

# re-pack
levels(texture_025) <- rat

# check: ok
cols <- levels(texture_025)[[1]]$hex
levelplot(
  texture_025, 
  att = 'names', col.regions = cols,
  main = 'Soil Texture 0-25cm\nWeighted Average',
  margin = FALSE, 
  scales = list(draw = FALSE),
  panel = function(...) {
    panel.levelplot(...)
    sp.lines(as(b, 'Spatial'), col = 'black', lwd = 2)
    # sp.lines(as(x, 'Spatial'), col = 'white', lwd = 1, lty = 3)
  }
)




