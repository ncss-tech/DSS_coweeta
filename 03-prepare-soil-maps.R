library(terra)
library(aqp)
library(viridisLite)

stop('need to finish converting / updating this code')



## TODO: make new maps
## * soil depth
## * depth to top of first restriction
## * modified soil depth
## * soil parameter file ID
##



## 10m mukey grids (UTM z17), same grid topology
r <- rast('grids/rss_utm.tif')
s <- rast('grids/ssurgo_utm.tif')

## combined LUT with depth/texture classes --> soil type class
lut <- readRDS('data/soil-depth-texture-classe-lut.rds')

## checking on codes used by source

# check classes
str(lut$soil.type)
levels(lut$soil.type)

table(source = lut$source, soil.type = lut$soil.type)

table(source = lut$source, compname = lut$compname)



## ensure that factor levels / ordering are not lost in subsequent steps
# use integer codes
lut$soil.type.numeric <- as.numeric(lut$soil.type)

## extract RATs
r.rat <- cats(r)[[1]]
s.rat <- cats(s)[[1]]

## merge with combined LUT
r.rat <- merge(r.rat, lut, by = 'mukey', all.x = TRUE, sort = FALSE)
s.rat <- merge(s.rat, lut, by = 'mukey', all.x = TRUE, sort = FALSE)

## re-pack updated RATs
levels(r) <- r.rat
levels(s) <- s.rat

## convert RATE columns -> raster layers
rr <- catalyze(r)
ss <- catalyze(s)

plot(rr['compname'])

## check
# combine grids, soil type integer code
z <- c(rr['soil.type.numeric'], ss['soil.type.numeric'])

# colors don't match
plot(z)

## save
writeRaster(rr['soil.type.numeric'], filename = 'grids/rss-soiltype-class.tif', overwrite = TRUE)
writeRaster(ss['soil.type.numeric'], filename = 'grids/ssurgo-soiltype-class.tif', overwrite = TRUE)

# check: ok
# plot(rast('grids/rss-soiltype-class.tif'))
# plot(rast('grids/ssurgo-soiltype-class.tif'))


## doesn't really work
# ## testing: convert soil type integers -> factors
# z <- as.factor(z)
# ll <- cats(z)
# 
# .st <- levels(lut$soil.type)
# ll[[1]]$soiltype <- .st[ll[[1]]$label]
# ll[[2]]$soiltype <- .st[ll[[2]]$label]
# 
# levels(z) <- ll
# 
# activeCat(z[[1]]) <- 'soiltype'
# activeCat(z[[2]]) <- 'soiltype'
# plot(z)

