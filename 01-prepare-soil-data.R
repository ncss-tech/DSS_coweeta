library(terra)
library(soilDB)
library(rasterVis)
library(viridisLite)
library(sf)
library(aqp)

## Input Data:
# * Coweeta watershed outlines (UTM z17)
# * FY23 RSS grid (NC, EPSG:5070)
# * FY24 SSURGO polygons (SDA, WGS84)
# * FY24 SSURGO tabular data (SDA)

## Output Data (UTM z17):
# * RSS 10m grid
# * SSURGO 10m grid
# * SSURGO original polygons
# * RSS + SSURGO tabular data as SPC


## Notes:
# * "Coweeta_Final_raster.tif" is a non-standard packaging of the RSS
# * grid cells are non-standard keys -> RAT -> [musym] -> mapunit -> [mukey]
# --> use FY23 RSS instead: 10m grid via WCS
# --> tabular RSS data via local download 
# * DEM and derivatives will work on a larger area, for complete basin characterization


## 2023-10-05: FY24 gSSURGO, RSS, and gNATSGO not yet available
# stop('Do not replace existing (FY23) data until FY24 data are available!')


## start with Coweeta Laboratory Watersheds (outline)
# UTM z17 NAD83
o <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# extend by 200m
# ensure a tight crop without losing anything
b <- buffer(o, width = 200)


## get latest RSS via WCS
# cell values are map unit keys
rss <- mukey.wcs(b, db = 'rss', res = 10)

# warp to UTM z17
rss <- project(rss, crs(o), method = 'near', res = 10)

# crop to watershed boundaries + 200m
rss <- crop(rss, b)

# check: ok
plot(rss)
lines(o)

# save for later, this includes .xml file with RAT
writeRaster(rss, filename = 'grids/rss_utm.tif', overwrite = TRUE)


## get latest SSURGO via SDA
# using BBOX of slightly expanded watershed outline
# nearby: https://casoilresource.lawr.ucdavis.edu/gmap/?loc=35.02427,-83.47449,z16
# NC113, 1:12k
# WGS84
s <- SDA_spatialQuery(b, what = 'mupolygon', geomIntersection = TRUE)

# SDA errors
if(inherits(s, 'try-error')) {
  stop('SDA returned an error', call. = FALSE)
}

# transform to UTM z17
s <- project(s, crs(o))

# save polygons for later
writeVector(s, filename = 'vect/SSURGO-MU.shp', overwrite = TRUE)

# check: ok
plot(rss)
lines(s)

# rasterize using RSS grid system
# cell values are mukey
s.rast <- rasterize(s, rss, field = 'mukey')

# crop/mask to RSS data
s.rast <- mask(crop(s.rast, rss), rss)

# init RAT
s.rast <- as.factor(s.rast)

## TODO: this doesn't change / remove 'label' column
# fix names in RAT
# set.names(s.rast, 'mukey')
# rat <- cats(s.rast)[[1]]
# rat$mukey <- rat$label
# levels(s.rast) <- rat

# check: ok
plot(s.rast, axes = FALSE)
lines(s)

# save for later, this includes .xml file with RAT
writeRaster(s.rast, filename = 'grids/ssurgo_utm.tif', overwrite = TRUE)


## FY23 RSS tabular data
# local files
# https://nrcs.app.box.com/v/soils/folder/176851236810

.rss_path <- 'e:/gis_data/RSS/RSS_NC.gdb'
st_layers(.rss_path)

# load relevant tables
rss.mu <- st_read(.rss_path, layer = 'mapunit')
rss.co <- st_read(.rss_path, layer = 'component')
rss.hz <- st_read(.rss_path, layer = 'chorizon')
rss.corestrictions <- st_read(.rss_path, layer = 'corestrictions')

# extract RSS RAT
# use these mukey to subset RSS tabular data
rat <- cats(rss)[[1]]

# check for missing symbols
# none missing from RAT
setdiff(rat$mukey, rss.mu$mukey)

# map unit table contains some extra
setdiff(rss.mu$mukey, rat$mukey)

# subset RSS tables
rss.mu <- rss.mu[rss.mu$mukey %in% rat$mukey, ]
rss.co <- rss.co[rss.co$mukey %in% rss.mu$mukey, ]
rss.hz <- rss.hz[rss.hz$cokey %in% rss.co$cokey, ]
rss.corestrictions <- rss.corestrictions[rss.corestrictions$cokey %in% rss.co$cokey, ]

# save just in case we need these
save(rss.mu, rss.co, rss.hz, rss.corestrictions, file = 'data/rss-tab-data-raw.rda')


## FY23 SSURGO tabular data
# get from SDA
# use rasterized SSURGO mukey
rat <- cats(s.rast)[[1]]

.mukeys <- as.numeric(rat$mukey)
ssurgo.mu <- SDA_query(sprintf("SELECT * FROM mapunit WHERE mukey IN %s", format_SQL_in_statement(.mukeys)))
ssurgo.co <- SDA_query(sprintf("SELECT * FROM component WHERE mukey IN %s", format_SQL_in_statement(.mukeys)))

.cokeys <- unique(ssurgo.co$cokey)
ssurgo.hz <- SDA_query(sprintf("SELECT * FROM chorizon WHERE cokey IN %s", format_SQL_in_statement(.cokeys)))
ssurgo.corestrictions <- SDA_query(sprintf("SELECT * FROM corestrictions WHERE cokey IN %s", format_SQL_in_statement(.cokeys)))


# save just in case we need these
save(ssurgo.mu, ssurgo.co, ssurgo.hz, ssurgo.corestrictions, file = 'data/ssurgo-tab-data-raw.rda')


## upgrade tabular data -> SPC
depths(rss.hz) <- cokey ~ hzdept_r + hzdepb_r
site(rss.hz) <- rss.co
site(rss.hz) <- rss.mu

depths(ssurgo.hz) <- cokey ~ hzdept_r + hzdepb_r
site(ssurgo.hz) <- ssurgo.co
site(ssurgo.hz) <- ssurgo.mu

# note different component key style
plotSPC(rss.hz[1:10, ])
plotSPC(ssurgo.hz[1:10, ])

## combine
site(rss.hz)$source <- 'RSS'
site(ssurgo.hz)$source <- 'SSURGO'
spc <- c(rss.hz, ssurgo.hz)

# set horizon name
hzdesgnname(spc) <- 'hzname'

# check: 216 profiles
length(spc)

# checkHzDepthLogic(spc)

## compute depth class
sdc <- getSoilDepthClass(spc)
site(spc) <- sdc

table(spc$depth.class)
table(spc$source, spc$depth.class)

## keep track of all restrictive features, using diagnostic horizons
r <- make.groups(ssurgo = ssurgo.corestrictions, rss = rss.corestrictions)
diagnostic_hz(spc) <- r

(.tab <- xtabs( ~ reskind + which, data = r))
round(prop.table(.tab, margin = 2), 2)

## compute depth to top restrictive feature
rr <- split(r, r$cokey)

# depth to first restrictive feature (RV), by cokey
rr <- lapply(rr, function(i) {
  
  # remove NA RV depths just in case
  i <- i[which(!is.na(i$resdept_r)), ]
  
  # safely account for all NA
  if(nrow(i) < 1) {
    .depth <- NA
    .kind <- NA
  } else {
    .idx <- order(i$resdept_r, decreasing = TRUE)
    .depth <- i$resdept_r[.idx[1]]
    .kind <- i$reskind[.idx[1]]
  }
  
  # compile results
  .d <- data.frame(
    cokey = i$cokey[1],
    reskind = .kind,
    resdepth = .depth
  )
  
  return(.d)
})

rr <- do.call('rbind', rr)
row.names(rr) <- NULL

# merge into site data
site(spc) <- rr

## new soil depth calculation, with restrictive features
cor(spc$depth, spc$resdepth, use = 'complete.obs')

spc$depth.to.restriction <- ifelse(spc$resdepth < spc$depth & !is.na(spc$resdepth), spc$resdepth, spc$depth)

hist(spc$depth.to.restriction, breaks = 20, las = 1)

table(spc$depth.to.restriction, useNA = 'always')


## classify <2mm soil texture class
spc$texture <- ssc_to_texcl(sand = spc$sandtotal_r, clay = spc$claytotal_r, simplify = TRUE) 



## save
saveRDS(spc, file = 'data/combined-tab-data-SPC.rds')

# done
