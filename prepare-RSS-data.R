library(aqp)
library(raster)
library(sf)
library(foreign)
library(stringi)

source('local-functions.R')


## TODO: which CRS are we using for most of the work... ?

## TODO: convert to terra

# use the elevation model as target grid
# note that it has a larger extent
e <- raster('grids/elev_pcs.tif')


# RSS: cell values are map unit keys
r <- raster('Raster soil survey/Coweeta_Final_raster.tif')

## for now, convert to UTM z17
r <- projectRaster(r, e, method = 'ngb')

# convert to raster + RAT
r <- ratify(r)

# RAT: raster attribute table
rat <- read.dbf('Raster soil survey/Coweeta_Final_raster.tif.vat.dbf', as.is = TRUE)
names(rat) <- tolower(names(rat))


## FGDB
# st_layers('Raster soil survey/rss_nc/rss_nc.gdb')

# musym is the national mu symbol
rss.mu <- st_read('Raster soil survey/rss_nc/rss_nc.gdb', layer = 'mapunit')
rss.co <- st_read('Raster soil survey/rss_nc/rss_nc.gdb', layer = 'component')
rss.hz <- st_read('Raster soil survey/rss_nc/rss_nc.gdb', layer = 'chorizon')


## check for missing symbols
# none missing from RAT
setdiff(rat$MUSYM, rss.mu$musym)
# map unit table contains some extra
setdiff(rss.mu$musym, rat$MUSYM)

## subset child tables
rss.mu <- rss.mu[rss.mu$musym %in% rat$musym, ]
rss.co <- rss.co[rss.co$mukey %in% rss.mu$mukey, ]
rss.hz <- rss.hz[rss.hz$cokey %in% rss.co$cokey, ]




getDominantCondition <- function(x, v) {
  
  res <- lapply(
    split(x, x$mukey), 
    dominantCondition, v = v
  )
  
  res <- do.call('rbind', res)
  
  return(res)
}



getDominantValue <- function(x, v) {
  
  res <- lapply(
    split(x, x$mukey), 
    dominantValue, v = v
  )
  
  res <- do.call('rbind', res)
  
  return(res)
}

co.taxpartsize <- getDominantCondition(rss.co, v = 'taxpartsize')

co.compnane <- getDominantCondition(rss.co, v = 'compname')
co.cokey <- getDominantCondition(rss.co, v = 'cokey')

## init SPC

co.spc <- rss.hz

depths(co.spc) <- cokey ~ hzdept_r + hzdepb_r
hzdesgnname(co.spc) <- 'hzname'
site(co.spc) <- rss.co

sdc <- getSoilDepthClass(co.spc)
site(co.spc) <- sdc

par(mar = c(0, 0, 3, 0))
plotSPC(co.spc[1:10, ], label = 'compname', color = 'claytotal_r')


x <- subset(co.spc, cokey %in% co.cokey$cokey)
plotSPC(x, label = 'compname', color = 'claytotal_r', name.style = 'center-center')


# at dZ = 1cm, use awc_r directly
co.spc$aws <- co.spc$awc_r * 1

a <- slab(co.spc, cokey ~ aws, slab.structure = c(0, 50), slab.fun = sum)

head(a)

# # ok just checking
# co.spc <- trunc(co.spc, 0, 50)
# a.test <- profileApply(co.spc, function(i) {
#   sum((i$hzdepb_r - i$hzdept_r) * i$awc_r, na.rm = TRUE)
# })
# all(a$value == a.test)





rss.co.aws050 <- merge(rss.co, a, by = 'cokey', sort = FALSE)[, c('mukey', 'cokey', 'compkind', 'comppct_r', 'value')]

# no NA allowed in wt. mean / etc.
rss.co.aws050 <- rss.co.aws050[!is.na(rss.co.aws050$value), ]

co.aws050 <- getDominantValue(rss.co.aws050, v = 'value')

names(co.aws050) <- c('mukey', 'aws050')




mu.subset <- rss.mu[, c('mukey', 'musym', 'mukind')]

agg <- merge(mu.subset, co.taxpartsize, by = 'mukey', sort = FALSE)

agg <- merge(agg, co.aws050, by = 'mukey', sort = FALSE)

rat <- merge(rat, agg, by = 'musym', sort = FALSE)


head(rat)



# attempt to split out component / series names
rat$co.names <- stri_split_fixed(str = rat$muname,  pattern = ',', n = 2, simplify = TRUE)[, 1]

# this only works because all component names are single-word names
rat$co.names <- stri_split_fixed(str = rat$co.names,  pattern = ' ', n = 2, simplify = TRUE)[, 1]

# # series list
# s.list <- unique(
#   c(
#     rat$co.names[grep('-', rat$co.names, invert = TRUE)],
#     stri_split_fixed(str = rat$co.names[grep('-', rat$co.names)],  pattern = '-', n = 2, simplify = TRUE)[, 1]
#   )
# )

# RAT management
ll.original <- levels(r)[[1]]

# merge and pack updated RAT
ll <- merge(ll.original, rat, by.x = 'ID', by.y = 'value', all.x = TRUE, sort = FALSE)
levels(r) <- ll

# convert select attributes to new raster objects via RAT + codes
r.id <- deratify(r, att = 'ID')
r.mukind <- deratify(r, att = 'mukind')
r.coname <- deratify(r, att = 'co.names')
r.taxpartsize <- deratify(r, att = 'taxpartsize')
r.aws050 <- deratify(r, att = 'aws050')
r.mukey <- ratify(deratify(r, att = 'mukey'))


plot(r.aws050)


## export to single grid files
writeRaster(r.id, filename = 'grids/rss_utm.tif', options = c('COMPRESS=LZW'), datatype = 'INT1U', overwrite = TRUE)

writeRaster(r.mukind, filename = 'grids/rss_mukind.tif', options = c('COMPRESS=LZW'), datatype = 'INT1U', overwrite = TRUE)

writeRaster(r.coname, filename = 'grids/rss_coname.tif', options = c('COMPRESS=LZW'), datatype = 'INT1U', overwrite = TRUE)

writeRaster(r.taxpartsize, filename = 'grids/rss_taxpartsize.tif', options = c('COMPRESS=LZW'), datatype = 'INT1U', overwrite = TRUE)

writeRaster(r.aws050, filename = 'grids/rss_aws050.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)

# UTM version of RSS + RAT
writeRaster(r.mukey, filename = 'grids/rss_mukey.tif', options = c('COMPRESS=LZW'), overwrite = TRUE)


## NOTE / BUG
# re-loading these grids will add a 0 to the RAT




